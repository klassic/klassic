//! Observe real publication/ACK ledgers; faults never synthesize positive work.
use super::super::*;
use super::{JournalRegion, TestActor};
pub(crate) fn root_obligation_counter(g: &mut crate::NativeCodeGenerator, kind: usize) {
    use crate::gc_abi::{HandshakeState, RootRegistry};
    let fields = [
        root_work::PENDING,
        root_work::PUBLISHED,
        root_work::ACKNOWLEDGED,
        owner_state::offset(
            offset_of!(OwnerState, root_journal) + offset_of!(root_journal::State, fast_entries),
        ),
        registered_roots::REGISTRY + offset_of!(RootRegistry, leases),
        registered_roots::REGISTRY + offset_of!(RootRegistry, walkers),
        participants::HANDSHAKE + offset_of!(HandshakeState, native),
        participants::HANDSHAKE + offset_of!(HandshakeState, expected),
        participants::HANDSHAKE + offset_of!(HandshakeState, acknowledged),
    ];
    lea_rt(g, R::V1, fields[kind]);
    g.atomic_load_acquire(R::V0, R::V1);
}
pub(crate) fn root_obligation_natives_at_close(g: &mut crate::NativeCodeGenerator) {
    g.load_rbp_slot(R::V0, 1504);
}
pub(crate) fn require_no_root_obligations(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    root_work::require_empty(g, &runtime.labels);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
pub(crate) fn root_obligation_fault(g: &mut crate::NativeCodeGenerator, kind: u64) {
    if kind == 4 {
        store_rt_imm(g, root_work::PENDING, 1);
        return;
    }
    let fields = [
        root_work::PENDING,
        root_work::PUBLISHED,
        root_work::ACKNOWLEDGED,
        root_work::PENDING,
    ];
    store_rt_imm(
        g,
        fields[kind as usize],
        if kind == 3 { 0 } else { u64::MAX },
    );
}

pub(crate) fn root_obligation_invalidate_closed_mark(
    g: &mut crate::NativeCodeGenerator,
    active: bool,
) {
    store_rt_imm(
        g,
        if active {
            RT_MARK_ACTIVE
        } else {
            RT_MARK_VALID
        },
        u64::from(active),
    );
}
pub(crate) fn root_obligation_actor_value(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    cell: crate::DataLabel,
) {
    let runtime = g.gc_gen.take().unwrap();
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V1, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V1, 0);
    g.mov_data_addr(R::V1, cell);
    g.load_ptr_disp32(R::V0, R::V1, 0);
    g.call_label(runtime.labels.root_load_actor);
    g.pop_reg(R::OwnerContext);
    g.gc_gen = Some(runtime);
}
pub(crate) fn root_obligation_batch_ack(
    g: &mut crate::NativeCodeGenerator,
    region: JournalRegion,
    count: u64,
) {
    use crate::gc_abi::{RootJournalRequest, RootLease, RootRegistration};
    let runtime = g.gc_gen.take().unwrap();
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let request = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<RootJournalRequest>() / 8]);
    let lease = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<RootLease>() / 8]);
    let output = g.asm.data_label_with_i64s(&vec![0; count as usize * 4]);
    g.mov_data_addr(R::V6, request);
    g.mov_data_addr(R::V0, region.storage);
    g.store_ptr_disp32(
        R::V6,
        offset_of!(RootJournalRequest, registration) as i32,
        R::V0,
    );
    g.load_ptr_disp32(R::V0, R::V0, offset_of!(RootRegistration, scope) as i32);
    g.store_ptr_disp32(R::V6, offset_of!(RootJournalRequest, scope) as i32, R::V0);
    for (field, data) in [
        (offset_of!(RootJournalRequest, lease), lease),
        (offset_of!(RootJournalRequest, output), output),
    ] {
        g.mov_data_addr(R::V0, data);
        g.store_ptr_disp32(R::V6, field as i32, R::V0);
    }
    g.mov_imm64(R::V0, count);
    g.store_ptr_disp32(
        R::V6,
        offset_of!(RootJournalRequest, capacity) as i32,
        R::V0,
    );
    worker::lock(g, &runtime.labels);
    // These records contain only null. The actual claim, copy and ACK suffice;
    // there is no managed target to offer to a generation's graph queue.
    for op in 0..3 {
        lea_rt(g, R::V7, registered_roots::REGISTRY);
        g.mov_data_addr(R::V6, request);
        g.mov_imm64(R::V2, op);
        g.call_label(runtime.labels.root_journal_step);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, runtime.labels.fail);
    }
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

pub(crate) fn root_legacy_scope(g: &mut crate::NativeCodeGenerator, enter: bool, explicit: bool) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let label = match (enter, explicit) {
        (true, true) => l.native_enter_actor,
        (false, true) => l.native_exit_actor,
        (true, false) => l.native_enter,
        (false, false) => l.native_exit,
    };
    g.call_label(label);
}
pub(crate) fn root_native_borrows(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, native_io::BORROWS);
}
pub(crate) fn root_legacy_registry_fault(g: &mut crate::NativeCodeGenerator) {
    use crate::gc_abi::RootRegistry;
    load_rt(
        g,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    g.add_reg_imm32(R::V0, 0x10000);
    store_rt(
        g,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
        R::V0,
    );
}
pub(crate) fn root_mark_other_owner(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let runtime = g.gc_gen.take().unwrap();
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    // Fresh owner: pending/scratch/TLAB are actually empty. Prepare/start the
    // same real snapshot body, with this owner as the Running coordinator.
    mark_prepare::ensure(g, &runtime.labels);
    worker::lock(g, &runtime.labels);
    mark::start_ready(g, &runtime.labels);
    worker::unlock(g, &runtime.labels);
    g.pop_reg(R::OwnerContext);
    g.gc_gen = Some(runtime);
}

/// Exercise the actual arithmetic emitters without transport or phase claims.
pub(crate) fn root_arithmetic_owner_bytes() -> usize {
    size_of::<OwnerState>()
}
pub(crate) fn root_arithmetic_runtime_bytes() -> usize {
    size_of::<Runtime>()
}
pub(crate) fn emit_root_arithmetic<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, mode: u64) {
    match mode {
        0 => {
            root_work::publish(a, l);
            root_work::publish(a, l);
            a.mov_imm64(R::V0, 2);
            root_work::ack(a, l, Some(R::V0));
            root_work::require_empty(a, l);
            for field in [root_work::PUBLISHED, root_work::ACKNOWLEDGED] {
                load_rt(a, R::V0, field);
                a.cmp_reg_imm32(R::V0, 2);
                a.jcc_label(C::NotEqual, l.fail);
            }
        }
        1 | 2 => {
            store_rt_imm(
                a,
                if mode == 1 {
                    root_work::PENDING
                } else {
                    root_work::PUBLISHED
                },
                u64::MAX,
            );
            root_work::publish(a, l);
        }
        3 => {
            store_rt_imm(a, root_work::PENDING, 1);
            store_rt_imm(a, root_work::ACKNOWLEDGED, u64::MAX);
            root_work::ack(a, l, None);
        }
        4 => root_work::ack(a, l, None),
        5 => {
            // Deliberately stale Z=1 before a successful increment.
            a.cmp_reg_reg(R::V0, R::V0);
            native_io::change_borrow(a, l, true);
            load_rt(a, R::V0, native_io::BORROWS);
            a.cmp_reg_imm32(R::V0, 1);
            a.jcc_label(C::NotEqual, l.fail);
            native_io::change_borrow(a, l, false);
            native_io::require_phase_clear(a, l);
        }
        6 => {
            store_rt_imm(a, native_io::BORROWS, u64::MAX);
            // Stale Z=0 must not allow a wrapped zero borrow count.
            a.mov_imm64(R::V0, 1);
            a.cmp_reg_imm32(R::V0, 0);
            native_io::change_borrow(a, l, true);
        }
        7 => native_io::change_borrow(a, l, false),
        8..=14 => nonmoving_adoption_fixture(a, l, mode),
        _ => unreachable!(),
    }
}

/// AArch64 validation-only fixture, not a synthetic mutator/worker execution.
fn nonmoving_adoption_fixture<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, mode: u64) {
    let select = mode >= 12;
    let mut target = if select {
        (8 << 16) | 0x40
    } else {
        (2 << 16) | 0x220
    };
    target ^= match mode {
        9 => 0x100,
        10 => 0x400,
        11 => 0x800,
        _ => 0,
    };
    store_rt_imm(a, RT_CTX, u64::from(ABI_VERSION) | (1 << 32));
    store_rt_imm(a, RT_CTX + offset_of!(MutatorContext, adopted_phase), 0);
    store_rt_imm(a, phase_round::PHASE, 0);
    store_rt_imm(a, phase_round::BANK, 1);
    store_rt_imm(a, allocation_records::REVISION, 1);
    store_rt_imm(a, allocation_records::OWNER_REVISION, 1);
    store_rt_imm(a, RT_MARK_ACTIVE, if select && mode != 14 { 0 } else { 1 });
    store_rt_imm(a, RT_MARK_VALID, if select && mode != 13 { 1 } else { 0 });
    store_rt_imm(a, RT_MARK_CYCLES, if select { 2 } else { 1 });
    store_rt_imm(
        a,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
        target,
    );
    native_io::adoption::adopt_snapshot(
        a,
        l,
        phase_round::PHASE,
        phase_round::BANK,
        phase_round::ADOPTIONS,
    );
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.mov_imm64(R::V1, target);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, phase_round::ADOPTIONS);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
}
