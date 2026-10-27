//! Real OS owners deleting distinct pre-snapshot edges before a controlled seal.
use super::*;
use crate::gc_codegen::production::test_support as support;
const ARRIVED: i32 = 0;
const GO: i32 = 8;
const FINISHED: i32 = 16;
const RELEASED: i32 = 24;
const OLD_BEFORE: i32 = 32;
const YOUNG_BEFORE: i32 = 40;
const LEASES_BEFORE: i32 = 48;
const OLD_AFTER: i32 = 56;
const YOUNG_AFTER: i32 = 64;
const LEASES_AFTER: i32 = 72;
const OLD_ACTIVE: i32 = 80;
const YOUNG_ACTIVE: i32 = 88;
const RETRIES: i32 = 96;
const BASE_LEASES: i32 = 104;
const BASE_RETRIES: i32 = 112;
const OWNERS: i32 = 128;
const STRIDE: i32 = 64;
const CHILD: i32 = 0;
const BEFORE_COUNT: i32 = 8;
const BEFORE_OLD: i32 = 16;
const BEFORE_YOUNG: i32 = 24;
const AFTER_COUNT: i32 = 32;
const ACKS: i32 = 40;
const AFTER_STATE: i32 = 48;

fn read(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel, off: i32) {
    g.mov_data_addr(R::V1, state);
    g.add_reg_imm32(R::V1, off);
    g.atomic_load_acquire(R::V0, R::V1);
}
fn put(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel, off: i32) {
    g.mov_data_addr(R::V1, state);
    g.add_reg_imm32(R::V1, off);
    g.atomic_store_release(R::V1, R::V0);
}
fn write(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel, off: i32, value: u64) {
    g.mov_imm64(R::V0, value);
    put(g, state, off);
}
fn own(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel, off: i32) {
    load_rt(g, R::V2, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.sub_reg_imm8(R::V2, 2);
    g.shl_reg_imm8(R::V2, 6);
    g.mov_data_addr(R::V1, state);
    g.add_reg_reg(R::V1, R::V2);
    g.add_reg_imm32(R::V1, OWNERS + off);
}
fn put_own(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel, off: i32) {
    own(g, state, off);
    g.store_ptr_disp32(R::V1, 0, R::V0);
}
fn child_address(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel) {
    own(g, state, CHILD);
    g.load_ptr_disp32(R::V0, R::V1, 0);
}
fn increment(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel, off: i32) {
    g.mov_data_addr(R::V7, state);
    g.add_reg_imm32(R::V7, off);
    let again = g.create_text_label();
    g.bind_text_label(again);
    g.atomic_load_acquire(R::V0, R::V7);
    g.mov_reg_reg(R::V8, R::V0);
    g.mov_reg_reg(R::V6, R::V0);
    g.add_reg_imm32(R::V6, 1);
    g.atomic_compare_exchange(R::V7, R::V6);
    g.cmp_reg_reg(R::V0, R::V8);
    g.jcc_label(C::NotEqual, again);
}
fn deadline(g: &mut crate::NativeCodeGenerator) {
    let clock = g
        .gc_gen
        .as_ref()
        .unwrap()
        .labels
        .worker
        .as_ref()
        .unwrap()
        .hooks
        .clock;
    g.call_label(clock);
    g.mov_imm64(R::V1, 20_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1984, R::V0);
}
fn tick(g: &mut crate::NativeCodeGenerator) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1984);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
}
fn wait(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel, off: i32, value: u64) {
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    read(g, state, off);
    g.cmp_reg_imm32(R::V0, value as i32);
    g.jcc_label(C::AboveOrEqual, done);
    tick(g);
    g.jmp_label(again);
    g.bind_text_label(done);
}
fn not_leader(g: &mut crate::NativeCodeGenerator, peer: crate::TextLabel) {
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, peer);
}

pub(super) fn child(
    g: &mut crate::NativeCodeGenerator,
    owners: u64,
    state: crate::DataLabel,
    gate: crate::DataLabel,
    seal: u64,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (exit, enter, load, store, checkpoint, end, detach, step, fail) = (
        l.native_exit_actor,
        l.native_enter_actor,
        l.root_load_actor,
        l.field_store_actor,
        l.owner_checkpoint_poll,
        l.old_end_actor,
        l.detach_begin_actor,
        l.detach_step_actor,
        l.fail,
    );
    graph::call_owner(g, exit);
    deadline(g);
    // Read the owned holder root, never load the child through a GC barrier.
    g.load_rbp_slot(R::V0, 8);
    g.load_ptr_disp32(R::V0, R::V0, HUB_ROOT);
    graph::call_owner(g, load);
    g.mov_reg_reg(R::V7, R::V0);
    load_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.sub_reg_imm8(R::V1, 2);
    g.shl_reg_imm8(R::V1, 3);
    g.add_reg_reg(R::V7, R::V1);
    g.mov_imm64(R::V6, 0);
    graph::call_owner(g, store);
    load_rt(g, R::V0, satb_buffer::COUNT);
    put_own(g, state, BEFORE_COUNT);
    child_address(g, state);
    support::emit_marked(g);
    put_own(g, state, BEFORE_OLD);
    child_address(g, state);
    support::emit_young_marked(g);
    put_own(g, state, BEFORE_YOUNG);
    load_rt(g, R::V0, quiescence::OWN_ACKS);
    g.store_rbp_slot(120, R::V0);
    increment(g, state, ARRIVED);
    // No poll here: all private entries must coexist before any handoff.
    wait(g, state, ARRIVED, owners);
    let peer = g.create_text_label();
    not_leader(g, peer);
    for (field, offset) in [
        (satb_buffer::pending_count(false), OLD_BEFORE),
        (satb_buffer::pending_count(true), YOUNG_BEFORE),
    ] {
        load_rt(g, R::V0, field);
        put(g, state, offset);
    }
    support::lease_metric(g, 0);
    put(g, state, LEASES_BEFORE);
    write(g, state, GO, 1);
    g.bind_text_label(peer);
    wait(g, state, GO, 1);
    if seal == 0 {
        let peer = g.create_text_label();
        let sealed = g.create_text_label();
        not_leader(g, peer);
        graph::call_owner(g, end);
        g.jmp_label(sealed);
        g.bind_text_label(peer);
        let requested = g.create_text_label();
        let wait_request = g.create_text_label();
        g.bind_text_label(wait_request);
        lea_rt(g, R::V1, quiescence::REQUEST);
        g.atomic_load_acquire(R::V0, R::V1);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, requested);
        tick(g);
        g.jmp_label(wait_request);
        g.bind_text_label(requested);
        // Internal checkpoint entry expects the already-bound runtime. Only
        // public actor entries may use call_owner's invalid-runtime sentinel.
        g.call_label(checkpoint);
        g.bind_text_label(sealed);
    } else if seal == 1 {
        graph::call_owner(g, enter);
    } else {
        graph::call_owner(g, detach);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
    }
    load_rt(g, R::V0, satb_buffer::COUNT);
    put_own(g, state, AFTER_COUNT);
    load_rt(g, R::V0, quiescence::OWN_ACKS);
    g.load_rbp_slot(R::V1, 120);
    g.sub_reg_reg(R::V0, R::V1);
    put_own(g, state, ACKS);
    if seal == 0 {
        graph::call_owner(g, enter);
    }
    lea_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, state));
    g.atomic_load_acquire_u32(R::V0, R::V1);
    put_own(g, state, AFTER_STATE);
    increment(g, state, FINISHED);
    wait(g, state, FINISHED, owners);
    let peer = g.create_text_label();
    not_leader(g, peer);
    // All producers are now Native/Detaching, and both workers remain gated.
    // Only native counters are observed here, never a managed payload.
    for (field, offset) in [
        (satb_buffer::pending_count(false), OLD_AFTER),
        (satb_buffer::pending_count(true), YOUNG_AFTER),
        (RT_MARK_ACTIVE, OLD_ACTIVE),
        (young_mark::ACTIVE, YOUNG_ACTIVE),
        (phase_round::END_RETRIES, RETRIES),
    ] {
        load_rt(g, R::V0, field);
        put(g, state, offset);
    }
    support::lease_metric(g, 0);
    put(g, state, LEASES_AFTER);
    support::worker_journal_open_gate(g, gate);
    write(g, state, RELEASED, 1);
    g.bind_text_label(peer);
    wait(g, state, RELEASED, 1);
    if seal == 2 {
        let again = g.create_text_label();
        let done = g.create_text_label();
        g.bind_text_label(again);
        graph::call_owner(g, step);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::Equal, done);
        g.cmp_reg_imm32(R::V0, 2);
        g.jcc_label(C::NotEqual, fail);
        tick(g);
        g.jmp_label(again);
        g.bind_text_label(done);
    }
}

/// Inputs are existing actual roots/objects. Diagnostic child addresses are
/// not root publications and never enter a load barrier before deletion.
pub(crate) fn run_parallel_satb_deletions(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    hub_slot: i32,
    children: crate::DataLabel,
    seal: u64,
) -> crate::DataLabel {
    assert!(seal <= 2);
    let state = g.asm.data_label_with_i64s(&vec![0; 16 + actors.len() * 8]);
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (read_root, adopt, enter) = (l.root_load_actor, l.root_adopt_actor, l.native_enter_actor);
    let mut roots = Vec::new();
    for (i, actor) in actors.iter().enumerate() {
        g.mov_data_addr(R::V1, children);
        g.load_ptr_disp32(R::V0, R::V1, (i * 8) as i32);
        put(g, state, OWNERS + i as i32 * STRIDE + CHILD);
        let cell = g.asm.data_label_with_i64s(&[0]);
        g.load_rbp_slot(R::V0, hub_slot);
        graph::call_owner(g, read_root);
        g.mov_data_addr(R::V1, cell);
        g.store_ptr_disp32(R::V1, 0, R::V0);
        select(g, *actor);
        g.mov_data_addr(R::V0, cell);
        graph::call_owner(g, adopt);
        graph::call_owner(g, enter);
        restore(g);
        roots.push(cell);
    }
    support::dual_mark_enable(g);
    let gate = support::satb_gates(g);
    support::dual_mark_entry(g, false, true);
    support::dual_mark_entry(g, true, true);
    support::lease_metric(g, 0);
    put(g, state, BASE_LEASES);
    load_rt(g, R::V0, phase_round::END_RETRIES);
    put(g, state, BASE_RETRIES);
    run_parallel_impl(
        g,
        actors,
        1,
        Some(&roots),
        GraphMode::Satb(state, gate, seal),
    );
    // The explicit fixture keeps Young automatic end disabled until all
    // observations above are captured; now ordinary completion may proceed.
    support::set_young_auto(g, true);
    support::satb_poll_until_closed(g);
    if seal == 2 {
        for actor in actors {
            support::actor_detach_dispose_wait(g, *actor);
        }
    }
    state
}

pub(crate) fn satb_parallel_metric(
    g: &mut crate::NativeCodeGenerator,
    state: crate::DataLabel,
    which: usize,
) {
    read(
        g,
        state,
        [
            OLD_BEFORE,
            YOUNG_BEFORE,
            LEASES_BEFORE,
            OLD_AFTER,
            YOUNG_AFTER,
            LEASES_AFTER,
            OLD_ACTIVE,
            YOUNG_ACTIVE,
            RETRIES,
            BASE_LEASES,
            BASE_RETRIES,
        ][which],
    );
}
pub(crate) fn satb_parallel_owner_metric(
    g: &mut crate::NativeCodeGenerator,
    state: crate::DataLabel,
    index: usize,
    which: usize,
) {
    read(
        g,
        state,
        OWNERS
            + index as i32 * STRIDE
            + [
                BEFORE_COUNT,
                BEFORE_OLD,
                BEFORE_YOUNG,
                AFTER_COUNT,
                ACKS,
                AFTER_STATE,
            ][which],
    );
}
