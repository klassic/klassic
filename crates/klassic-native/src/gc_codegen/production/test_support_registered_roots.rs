//! Observe the actual production root registry and its independent leases.
use super::super::*;
use crate::gc_abi::{RootLease, RootRegistration, RootRegistry, RootSegment};
use crate::gc_codegen::production::{OwnerState, lea_rt, load_rt, owner_state, store_rt};

#[derive(Clone, Copy)]
pub(crate) enum RootRegistryMetric {
    Active,
    Leases,
    Registered,
    Scope,
    Phase,
    Old,
    Young,
    Relocation,
    LastLease,
}
fn field<E: PortableAsm>(a: &E, metric: RootRegistryMetric) -> usize {
    use RootRegistryMetric::*;
    match metric {
        Active => registered_roots::REGISTRY + offset_of!(RootRegistry, active),
        Leases => registered_roots::REGISTRY + offset_of!(RootRegistry, leases),
        Registered => registered_roots::registration(a) + offset_of!(RootRegistration, registered),
        Scope => registered_roots::registration(a) + offset_of!(RootRegistration, scope),
        Phase => registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
        Old => registered_roots::OLD + offset_of!(RootLease, kind),
        Young => registered_roots::YOUNG + offset_of!(RootLease, kind),
        Relocation => registered_roots::RELOCATION + offset_of!(RootLease, kind),
        LastLease => registered_roots::REGISTRY + offset_of!(RootRegistry, last_lease),
    }
}
pub(crate) fn root_registry_metric<E: PortableAsm>(a: &mut E, metric: RootRegistryMetric) {
    load_rt(a, R::V0, field(a, metric));
}
/// Invalid metadata followed exclusively by a rejection probe.
pub(crate) fn root_registry_set<E: PortableAsm>(a: &mut E, metric: RootRegistryMetric, value: u64) {
    store_rt_imm(a, field(a, metric), value);
}
pub(crate) fn root_registry_links<E: PortableAsm>(a: &mut E) {
    let bad = a.create_text_label();
    let done = a.create_text_label();
    lea_rt(a, R::V1, registered_roots::registration(a));
    for f in [
        RT_CTX + offset_of!(MutatorContext, root_segments),
        registered_roots::REGISTRY + offset_of!(RootRegistry, inventory),
    ] {
        load_rt(a, R::V0, f);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, bad);
    }
    load_rt(a, R::V2, RT_SEGMENT);
    a.load_ptr_disp32(R::V0, R::V2, d(offset_of!(RootSegment, registration)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, bad);
    a.load_ptr_disp32(R::V0, R::V2, d(offset_of!(RootSegment, owner)));
    load_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, bad);
    a.load_ptr_disp32(R::V0, R::V2, d(offset_of!(RootSegment, scope)));
    load_rt(a, R::V1, field(a, RootRegistryMetric::Scope));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, bad);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(bad);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(done);
}

/// Register real additional stable storage owned by the existing participant.
/// V0 is a frame RootCell pointer (or zero for an empty region). The copied
/// value is published before any cycle starts; no concurrent attach is claimed.
pub(crate) fn root_registry_add_region(
    g: &mut crate::NativeCodeGenerator,
    empty: bool,
) -> crate::DataLabel {
    let runtime = g.gc_gen.take().expect("generational runtime");
    use crate::gc_abi::{RootCell, RootLifeRequest};
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let cell = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<RootCell>() / 8]);
    let segment = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<RootSegment>() / 8]);
    let registration = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<RootRegistration>() / 8]);
    let request = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<RootLifeRequest>() / 8]);
    // Keep the frame's actual encoded reference while the finite registration
    // operation validates and links this new empty prepared segment.
    if !empty {
        g.load_ptr_disp32(R::V0, R::V0, 0);
    }
    g.push_reg(R::V0);
    g.push_reg(R::V0);
    worker::lock(g, &runtime.labels);
    g.mov_data_addr(R::V6, request);
    g.mov_data_addr(R::V1, registration);
    g.store_ptr_disp32(
        R::V6,
        offset_of!(RootLifeRequest, registration) as i32,
        R::V1,
    );
    lea_rt(g, R::V0, RT_CTX);
    g.store_ptr_disp32(R::V6, offset_of!(RootLifeRequest, context) as i32, R::V0);
    g.mov_data_addr(R::V2, segment);
    g.store_ptr_disp32(R::V1, offset_of!(RootRegistration, segment) as i32, R::V2);
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.store_ptr_disp32(R::V2, offset_of!(RootSegment, owner) as i32, R::V0);
    g.mov_data_addr(R::V0, cell);
    g.store_ptr_disp32(R::V2, offset_of!(RootSegment, cells) as i32, R::V0);
    g.mov_imm64(R::V0, 1);
    g.store_ptr_disp32(R::V2, offset_of!(RootSegment, capacity) as i32, R::V0);
    lea_rt(g, R::V7, registered_roots::REGISTRY);
    g.mov_imm64(R::V2, 0);
    g.call_label(runtime.labels.root_life_step);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, runtime.labels.fail);
    worker::unlock(g, &runtime.labels);
    g.pop_reg(R::V0);
    g.pop_reg(R::V0);
    if !empty {
        g.mov_data_addr(R::V1, cell);
        g.atomic_store_release(R::V1, R::V0);
        g.mov_imm64(R::V0, 1);
        g.store_ptr_disp32(R::V1, offset_of!(RootCell, reuse_generation) as i32, R::V0);
        g.store_ptr_disp32(R::V1, offset_of!(RootCell, active) as i32, R::V0);
        g.mov_data_addr(R::V1, segment);
        g.mov_imm64(R::V0, 1);
        g.store_ptr_disp32(R::V1, offset_of!(RootSegment, used) as i32, R::V0);
    }
    g.gc_gen = Some(runtime);
    cell
}

/// Exercise the real journal CLAIM against the production pressure drain.
/// The current producer protects references eagerly; this probe does not
/// pretend to hand the copied references to a deferred mark consumer.
pub(crate) fn root_journal_drain_probe(g: &mut crate::NativeCodeGenerator, mode: u64) {
    use crate::gc_abi::{RootChange, RootJournalRequest, RootLease};
    let runtime = g.gc_gen.take().expect("generational runtime");
    if matches!(mode, 0 | 3 | 4) {
        g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
        let lease = g
            .asm
            .data_label_with_i64s(&vec![0; size_of::<RootLease>() / 8]);
        let request = g
            .asm
            .data_label_with_i64s(&vec![0; size_of::<RootJournalRequest>() / 8]);
        let output = g
            .asm
            .data_label_with_i64s(&vec![0; size_of::<RootChange>() / 8]);
        let claim = g.create_text_label();
        let continuation = g.create_text_label();
        g.jmp_label(continuation);
        g.bind_text_label(claim);
        super::super::super::root_lifetime::root_journal::emit_root_journal(g);
        g.ret();
        g.bind_text_label(continuation);
        worker::lock(g, &runtime.labels);
        g.mov_data_addr(R::V6, request);
        lea_rt(g, R::V0, registered_roots::registration(g));
        g.store_ptr_disp32(
            R::V6,
            offset_of!(RootJournalRequest, registration) as i32,
            R::V0,
        );
        g.load_ptr_disp32(R::V1, R::V0, offset_of!(RootRegistration, scope) as i32);
        g.store_ptr_disp32(R::V6, offset_of!(RootJournalRequest, scope) as i32, R::V1);
        g.mov_data_addr(R::V0, lease);
        g.store_ptr_disp32(R::V6, offset_of!(RootJournalRequest, lease) as i32, R::V0);
        g.mov_imm64(R::V0, 1);
        g.store_ptr_disp32(
            R::V6,
            offset_of!(RootJournalRequest, capacity) as i32,
            R::V0,
        );
        lea_rt(g, R::V7, registered_roots::REGISTRY);
        g.mov_imm64(R::V2, 0);
        g.call_label(claim);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, runtime.labels.fail);
        if mode >= 3 {
            g.mov_data_addr(R::V6, request);
            g.mov_data_addr(R::V0, output);
            g.store_ptr_disp32(R::V6, offset_of!(RootJournalRequest, output) as i32, R::V0);
            lea_rt(g, R::V7, registered_roots::REGISTRY);
            g.mov_imm64(R::V2, 1);
            g.call_label(claim);
            g.test_reg_reg(R::V0, R::V0);
            g.jcc_label(C::NotEqual, runtime.labels.fail);
            if mode == 3 {
                // Both generations are Idle in this probe: the copied entry
                // creates no marking obligation. Complete the real claim.
                g.mov_data_addr(R::V6, request);
                lea_rt(g, R::V7, registered_roots::REGISTRY);
                g.mov_imm64(R::V2, 2);
                g.call_label(claim);
                g.test_reg_reg(R::V0, R::V0);
                g.jcc_label(C::NotEqual, runtime.labels.fail);
            }
        }
        worker::unlock(g, &runtime.labels);
    } else if mode == 1 {
        // A nonzero end without a serial is also forbidden, rather than
        // silently making this malformed claim's ring slots reusable.
        store_rt_imm(
            g,
            registered_roots::registration(g) + offset_of!(RootRegistration, journal_end),
            1,
        );
    }
    g.call_label(runtime.labels.drain);
    g.gc_gen = Some(runtime);
}

pub(crate) fn root_journal_entries<E: PortableAsm>(a: &mut E) {
    load_rt(
        a,
        R::V0,
        owner_state::offset(offset_of!(OwnerState, root_journal))
            + offset_of!(root_journal::State, entries),
    );
}
pub(crate) fn root_journal_yields<E: PortableAsm>(a: &mut E) {
    load_rt(
        a,
        R::V0,
        owner_state::offset(offset_of!(OwnerState, root_journal))
            + offset_of!(root_journal::State, yields),
    );
}
/// Real registered producer writes without the eager production pre-barriers.
/// V0/V1 are initialized, nonmoving-until-collection payloads. Reserve beyond
/// both snapshots so only journal handoff can discover the overwritten value.
pub(crate) fn root_journal_publish_pair(g: &mut crate::NativeCodeGenerator, count: usize) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    encode(g, R::V0, R::V2);
    g.push_reg(R::V0);
    g.mov_reg_reg(R::V0, R::V1);
    encode(g, R::V0, R::V2);
    g.push_reg(R::V0);
    for index in 0..=count {
        if index != 0 {
            g.load_ptr_disp32(R::V0, R::V4, if index % 2 == 1 { 8 } else { 0 });
            store_rt(g, RT_FRAME_ROOT + REQUEST_VALUE, R::V0);
        }
        lea_rt(g, R::V7, RT_CTX);
        lea_rt(g, R::V6, RT_FRAME_ROOT);
        g.mov_imm64(R::V2, if index == 0 { ROOT_RESERVE } else { ROOT_STORE });
        g.call_label(runtime.labels.step);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, runtime.labels.fail);
    }
    g.pop_reg(R::V0);
    g.pop_reg(R::V0);
    load_rt(g, R::V0, RT_FRAME_ROOT + HANDLE_CELL);
    g.gc_gen = Some(runtime);
}
/// An ordinary phase publication calls the real boundary consumer first.
pub(crate) fn root_journal_advance_young(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    phase::young_marking(g, &runtime.labels, crate::gc_abi::Phase::MarkEndCheck);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

/// Deliberately corrupt an already published record for a rejection probe.
pub(crate) fn root_journal_corrupt_entry(g: &mut crate::NativeCodeGenerator, phase: bool) {
    use crate::gc_abi::{BufferCursor, RootChange};
    load_rt(g, R::V1, RT_JOURNAL);
    g.load_ptr_disp32(R::V0, R::V1, d(offset_of!(BufferCursor, consumed)));
    g.load_ptr_disp32(R::V6, R::V1, d(offset_of!(BufferCursor, capacity)));
    g.load_ptr_disp32(R::V10, R::V1, d(offset_of!(BufferCursor, entries)));
    g.div_unsigned(R::V6);
    g.shl_reg_imm8(R::V2, 5);
    g.add_reg_reg(R::V10, R::V2);
    let field = if phase {
        offset_of!(RootChange, phase)
    } else {
        offset_of!(RootChange, scope)
    };
    g.load_ptr_disp32(R::V0, R::V10, d(field));
    g.add_reg_imm32(R::V0, if phase { 0x10000 } else { 1 });
    g.store_ptr_disp32(R::V10, d(field), R::V0);
}

/// Publish the real mark-loop result as the worker does, then offer already
/// marked values through a new journal entry at an ordinary Old phase edge.
pub(crate) fn root_journal_complete_old(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    super::emit_unbounded_budget(g);
    g.call_label(runtime.labels.worker.as_ref().unwrap().mark_loop);
    store_rt(g, RT_MARK_DONE, R::V0);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
pub(crate) fn root_journal_advance_old(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    phase::marking(g, &runtime.labels, crate::gc_abi::Phase::ConcurrentMark);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

/// Wait without owner graph assistance. Reject a failed spawn rather than
/// allowing the cooperative fallback to stand in for a real worker test.
pub(crate) fn root_journal_wait_for_worker(g: &mut crate::NativeCodeGenerator, budget_slot: i32) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    let w = runtime.labels.worker.as_ref().expect("worker runtime");
    let again = g.create_text_label();
    let busy = g.create_text_label();
    let done = g.create_text_label();
    g.mov_imm64(R::V0, 1_000_000);
    g.store_rbp_slot(budget_slot, R::V0);
    g.bind_text_label(again);
    worker::lock(g, &runtime.labels);
    load_rt(g, R::V0, RT_WORKER_ALIVE);
    g.cmp_reg_imm8(R::V0, 1);
    g.jcc_label(C::NotEqual, runtime.labels.fail);
    for field in [RT_MARK_DONE, young_mark::DONE] {
        load_rt(g, R::V0, field);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::Equal, busy);
    }
    worker::unlock(g, &runtime.labels);
    g.jmp_label(done);
    g.bind_text_label(busy);
    worker::request_work(g);
    worker::unlock(g, &runtime.labels);
    worker::wake_if_pending(g, &runtime.labels);
    g.call_label(w.hooks.relax);
    g.load_rbp_slot(R::V0, budget_slot);
    g.sub_reg_imm8(R::V0, 1);
    g.store_rbp_slot(budget_slot, R::V0);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, runtime.labels.fail);
    g.jmp_label(again);
    g.bind_text_label(done);
    g.gc_gen = Some(runtime);
}
