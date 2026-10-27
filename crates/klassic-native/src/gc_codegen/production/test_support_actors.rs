//! Explicit owners sharing the real production runtime/heap. This fixture
//! covers Idle and nonmoving Old marking after native-return adoption.
//! This does not implement Running-peer or moving N-mutator phase admission.
use super::super::*;
use crate::gc_abi::{RootRegistration, RootRegistry};
#[derive(Clone, Copy)]
pub(crate) struct TestActor {
    pub(super) pointer: crate::DataLabel,
}
fn select(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
}
fn restore(g: &mut crate::NativeCodeGenerator) {
    g.pop_reg(R::OwnerContext);
}
pub(crate) fn create_actor(
    g: &mut crate::NativeCodeGenerator,
    cells: u64,
    entries: u64,
) -> TestActor {
    let actor = prepare_actor(g, cells, entries);
    actor_reattach(g, actor);
    actor
}
/// Allocate native owner storage without publishing any participant or roots.
pub(crate) fn prepare_actor(
    g: &mut crate::NativeCodeGenerator,
    cells: u64,
    entries: u64,
) -> TestActor {
    let runtime = g.gc_gen.take().unwrap();
    let roots_bytes = size_of::<RootStorage>() as u64 + (cells + entries) * 32;
    let bytes = offset_of!(OwnerState, roots) as u64 + roots_bytes;
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let pointer = g.asm.data_label_with_i64s(&[0]);
    g.mov_imm64(R::V7, bytes);
    g.call_label(runtime.labels.metadata_map);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, runtime.labels.native_oom);
    g.mov_data_addr(R::V1, pointer);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.mov_reg_reg(R::V1, R::V0);
    for (field, value) in [
        (offset_of!(RootStorageSpec, cells), cells),
        (offset_of!(RootStorageSpec, entries), entries),
        (offset_of!(RootStorageSpec, bytes), roots_bytes),
    ] {
        g.mov_imm64(R::V0, value);
        g.store_ptr_disp32(R::V1, d(offset_of!(OwnerState, spec) + field), R::V0);
    }
    g.gc_gen = Some(runtime);
    TestActor { pointer }
}
pub(crate) fn actor_alloc(g: &mut crate::NativeCodeGenerator, actor: TestActor, bytes: u64) {
    let label = g.gc_gen.as_ref().unwrap().labels.alloc_actor;
    select(g, actor);
    g.mov_imm64(R::V7, bytes);
    g.mov_imm64(R::V6, 1);
    g.call_label(label);
    restore(g);
}
pub(crate) fn actor_publish(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let label = g.gc_gen.as_ref().unwrap().labels.publish_pending;
    select(g, actor);
    g.call_label(label);
    restore(g);
}
#[derive(Clone, Copy)]
pub(crate) enum ActorMetric {
    Identity,
    Scope,
    Pending,
    Heap,
    Arena,
    ArenaRecord,
    Bytes,
    FreshSize,
    Lock,
    Phase,
    MaintenanceTick,
    RootDepth,
    CopyGuard,
    CopyClaim,
    CopyWins,
    CopyLosses,
    CheckpointAcks,
    JournalBeforeAck,
    JournalAfterAck,
    NativeAdoptions,
    NativeEntries,
    NativeExits,
    AttachWaits,
}
pub(crate) fn actor_metric(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    metric: ActorMetric,
) {
    select(g, actor);
    match metric {
        ActorMetric::AttachWaits => {
            lea_rt(
                g,
                R::V1,
                owner_state::offset(offset_of!(OwnerState, attach_waits)),
            );
            g.atomic_load_acquire(R::V0, R::V1);
        }
        ActorMetric::NativeAdoptions => load_rt(g, R::V0, native_io::ADOPTIONS),
        ActorMetric::NativeEntries => load_rt(g, R::V0, native_io::ENTRIES),
        ActorMetric::NativeExits => load_rt(g, R::V0, native_io::EXITS),
        ActorMetric::Identity => load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id)),
        ActorMetric::Scope => load_rt(
            g,
            R::V0,
            registered_roots::registration(g) + offset_of!(RootRegistration, scope),
        ),
        ActorMetric::Pending => {
            load_rt(g, R::V0, RT_PENDING_REF);
            decode(g, R::V0, R::V1);
        }
        ActorMetric::Heap => {
            load_rt(g, R::V0, RT_REQUEST + offset_of!(AllocationRequest, page));
            let empty = g.create_text_label();
            let done = g.create_text_label();
            g.test_reg_reg(R::V0, R::V0);
            g.jcc_label(C::Equal, empty);
            g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(AllocationPage, heap_base)));
            g.jmp_label(done);
            g.bind_text_label(empty);
            // A peer can finish before a checkpoint, close its TLAB there,
            // and perform no later allocation. Follow its immutable heap link.
            load_rt(
                g,
                R::V0,
                owner_state::offset(offset_of!(OwnerState, shared_runtime)),
            );
            g.load_ptr_disp32(R::V0, R::V0, d(RT_HEAP_BASE));
            g.bind_text_label(done);
        }
        ActorMetric::Arena => load_rt(g, R::V0, RT_REQUEST + offset_of!(AllocationRequest, marks)),
        ActorMetric::ArenaRecord => load_rt(
            g,
            R::V0,
            allocation_records::OWNER + offset_of!(MarkArena, records),
        ),
        ActorMetric::Bytes => load_rt(
            g,
            R::V0,
            RT_CTX + offset_of!(MutatorContext, allocated_bytes),
        ),
        ActorMetric::FreshSize => load_rt(g, R::V0, RT_FRESH_SIZE),
        ActorMetric::Lock => load_rt(g, R::V0, root_journal::OWNER_LOCKED),
        ActorMetric::Phase => load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase)),
        ActorMetric::MaintenanceTick => load_rt(g, R::V0, commit::OWNER_TICK),
        ActorMetric::RootDepth => load_rt(g, R::V0, RT_CELL_SP),
        ActorMetric::CopyGuard => load_rt(g, R::V0, RT_COLLECTING),
        ActorMetric::CopyClaim => load_rt(g, R::V0, copy_claims::SOURCE),
        ActorMetric::CopyWins => load_rt(g, R::V0, RT_MUTATOR_COPIES),
        ActorMetric::CopyLosses => load_rt(g, R::V0, RT_MUTATOR_LOST),
        ActorMetric::CheckpointAcks => load_rt(g, R::V0, quiescence::OWN_ACKS),
        ActorMetric::JournalBeforeAck => load_rt(
            g,
            R::V0,
            owner_state::offset(offset_of!(OwnerState, checkpoint_journal_before)),
        ),
        ActorMetric::JournalAfterAck => load_rt(
            g,
            R::V0,
            owner_state::offset(offset_of!(OwnerState, checkpoint_journal_after)),
        ),
    }
    restore(g);
}

pub(crate) fn actor_native_boundary(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    entering: bool,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = if entering {
        l.native_enter_actor
    } else {
        l.native_exit_actor
    };
    select(g, actor);
    g.call_label(entry);
    restore(g);
}

pub(crate) fn actor_load_root(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    slot: crate::DataLabel,
) {
    let entry = g.gc_gen.as_ref().unwrap().labels.root_load_actor;
    select(g, actor);
    g.mov_data_addr(R::V0, slot);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    g.call_label(entry);
    restore(g);
}
pub(crate) fn actor_shared_heap(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, RT_HEAP_BASE);
}
pub(crate) fn actor_shared_arena(g: &mut crate::NativeCodeGenerator) {
    lea_rt(g, R::V0, RT_MARKS);
}
pub(crate) fn actor_shared_records(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, RT_MARKS + offset_of!(MarkArena, records));
}
pub(crate) fn actor_active_regions(g: &mut crate::NativeCodeGenerator) {
    load_rt(
        g,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, active),
    );
}
pub(crate) fn actor_header_state(g: &mut crate::NativeCodeGenerator) {
    g.load_ptr_disp32(
        R::V0,
        R::V0,
        d(offset_of!(ObjectHeader, state)) - size_of::<ObjectHeader>() as i32,
    );
}

pub(crate) fn actor_reattach(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let label = g.gc_gen.as_ref().unwrap().labels.attach_actor;
    g.mov_data_addr(R::V7, actor.pointer);
    g.load_ptr_disp32(R::V7, R::V7, 0);
    g.call_label(label);
}
pub(crate) fn primary_allocated_diagnostic(g: &mut crate::NativeCodeGenerator) {
    load_diagnostic(g, R::V0, R::V3, stat_offsets().allocated_bytes);
}

/// The owner entry derives its heap even with an unrelated caller V3, and
/// must restore that caller word on return. Returned V0 is the actual payload.
pub(crate) fn actor_alloc_without_ambient_runtime(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    bytes: u64,
) {
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.push_reg(R::V3);
    g.mov_imm64(R::V3, 123);
    actor_alloc(g, actor, bytes);
    g.cmp_reg_imm32(R::V3, 123);
    g.jcc_label(C::NotEqual, fail);
    g.pop_reg(R::V3);
}

#[path = "test_support_copy_claims.rs"]
mod copying;
pub(crate) use copying::*;

#[path = "test_support_actor_threads.rs"]
mod threads_support;
pub(crate) use threads_support::*;

#[path = "test_support_attach.rs"]
mod attach_support;
pub(crate) use attach_support::*;

pub(crate) fn prepare_primary_record(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().unwrap();
    g.call_label(runtime.labels.publish_pending);
    allocation_records::prepare(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
pub(crate) fn primary_record_state(g: &mut crate::NativeCodeGenerator, which: u64) {
    let field = match which {
        0 => allocation_records::OWNER + offset_of!(MarkArena, used),
        1 => allocation_records::OWNER_REVISION,
        2 => allocation_records::REVISION,
        _ => allocation_records::OWNER + offset_of!(MarkArena, records),
    };
    load_rt(g, R::V0, field);
}
pub(crate) fn header_record(g: &mut crate::NativeCodeGenerator) {
    g.load_ptr_disp32(
        R::V0,
        R::V0,
        offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
    );
}

pub(crate) fn allocation_counter_edge(g: &mut crate::NativeCodeGenerator, mode: u64) {
    match mode {
        0 => store_rt_imm(g, RT_NEXT_OBJECT_ID, u64::MAX),
        1 => store_rt_imm(g, allocation_records::REVISION, u64::MAX),
        _ => {
            store_rt_imm(g, old_policy::ALLOCATED, u64::MAX - 24);
            store_rt_imm(g, RT_PUBLISHED, u64::MAX);
        }
    }
}
pub(crate) fn publish_primary(g: &mut crate::NativeCodeGenerator) {
    let label = g.gc_gen.as_ref().unwrap().labels.publish_pending;
    g.call_label(label);
}

pub(crate) fn primary_root_depth(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, RT_CELL_SP);
}

pub(crate) fn adopt_actor_value(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
) -> crate::DataLabel {
    let label = g.gc_gen.as_ref().unwrap().labels.root_adopt_actor;
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let slot = g.asm.data_label_with_i64s(&[0]);
    g.mov_data_addr(R::V1, slot);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    select(g, actor);
    g.mov_data_addr(R::V0, slot);
    g.call_label(label);
    restore(g);
    slot
}
fn clone_actor(g: &mut crate::NativeCodeGenerator, actor: TestActor) -> TestActor {
    let map = g.gc_gen.as_ref().unwrap().labels.metadata_map;
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let pointer = g.asm.data_label_with_i64s(&[0]);
    g.mov_imm64(R::V7, size_of::<OwnerState>() as u64);
    g.call_label(map);
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, fail);
    g.mov_data_addr(R::V1, pointer);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.mov_reg_reg(R::V7, R::V0);
    g.mov_data_addr(R::V6, actor.pointer);
    g.load_ptr_disp32(R::V6, R::V6, 0);
    g.mov_imm64(R::V1, size_of::<OwnerState>() as u64);
    g.rep_movsb();
    TestActor { pointer }
}
/// Deliberately invalid caller metadata; the boundary must reject before use.
pub(crate) fn actor_boundary_fault(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    own: crate::DataLabel,
    foreign: crate::DataLabel,
    mode: u64,
) {
    let actor = if mode == 1 {
        clone_actor(g, actor)
    } else {
        actor
    };
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (read, field) = (l.root_load_actor, l.field_load_actor);
    select(g, actor);
    match mode {
        2 => store_rt_imm(
            g,
            RT_CTX + offset_of!(MutatorContext, adopted_phase),
            0x10000,
        ),
        3 => {
            lea_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, state));
            g.mov_imm64(R::V0, 2);
            g.atomic_store_release_u32(R::V1, R::V0);
        }
        4 => store_rt_imm(
            g,
            participants::OWNER + offset_of!(crate::gc_abi::ThreadRegistration, registered),
            0,
        ),
        5 => store_rt_imm(
            g,
            participants::OWNER + offset_of!(crate::gc_abi::ThreadRegistration, registry),
            0,
        ),
        _ => {}
    }
    if mode == 0 {
        g.mov_data_addr(R::V0, foreign);
        g.load_ptr_disp32(R::V0, R::V0, 0);
        g.call_label(read);
    } else {
        g.mov_data_addr(R::V10, own);
        g.load_ptr_disp32(R::V10, R::V10, 0);
        g.atomic_load_acquire(R::V0, R::V10);
        g.call_label(field);
    }
    restore(g);
}

pub(crate) fn invalid_checkpoint(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let label = g.gc_gen.as_ref().unwrap().labels.checkpoint_actor;
    match mode {
        0 => store_rt_imm(g, RT_CTX + offset_of!(MutatorContext, private_producers), 1),
        1 => store_rt_imm(g, RT_CTX + offset_of!(MutatorContext, access_epoch), 1),
        2 => store_rt_imm(g, RT_COLLECTING, 1),
        _ => {
            store_rt_imm(g, RT_CTX + offset_of!(MutatorContext, adopted_phase), 1);
            store_rt_imm(
                g,
                registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
                1,
            );
        }
    }
    g.call_label(label);
}

#[path = "test_support_native_mark.rs"]
mod native_mark;
pub(crate) use native_mark::*;

#[path = "test_support_worker_view.rs"]
mod worker_view_support;
pub(crate) use worker_view_support::*;
