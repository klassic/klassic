//! Real excluded owners race weak publication with actual phase-round closure.
//! Latches defer a weak quantum or hold a completed publication; they never
//! write collector epochs, queues, phase flags, roots or completion receipts.
use super::*;
const ALLOW_WORK: i32 = 8;
const PUBLISHED: i32 = 16;
const END_ENTERED: i32 = 24;
const ATTEMPTED: i32 = 32;
const RETRY: i32 = 40;
const FINISHED: i32 = 48;
const GATE_OBSERVED: i32 = 56;
const DEBT_AT_END: i32 = 64;
const DEFERRED: i32 = 72;
const RECORD: i32 = 80;
const WHITE: i32 = 88;
fn read(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel, off: i32) {
    g.mov_data_addr(R::V1, state);
    g.add_reg_imm32(R::V1, off);
    g.atomic_load_acquire(R::V0, R::V1);
}
fn write(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel, off: i32, value: u64) {
    g.mov_data_addr(R::V1, state);
    g.add_reg_imm32(R::V1, off);
    g.mov_imm64(R::V0, value);
    g.atomic_store_release(R::V1, R::V0);
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
    g.mov_imm64(R::V10, 10_000_000_000);
    g.add_reg_reg(R::V10, R::V0);
}
fn tick(g: &mut crate::NativeCodeGenerator) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.cmp_reg_reg(R::V0, R::V10);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
}
fn wait(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel, off: i32, value: u64) {
    let top = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(top);
    read(g, state, off);
    g.cmp_reg_imm32(R::V0, value as i32);
    g.jcc_label(C::AboveOrEqual, done);
    tick(g);
    g.jmp_label(top);
    g.bind_text_label(done);
}
/// V0 is the actual source RootCell; preserve it for the fixture.
pub(crate) fn prepare_weak_race(
    g: &mut crate::NativeCodeGenerator,
    handle: crate::DataLabel,
    gate_first: bool,
) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let state = g.asm.data_label_with_i64s(&[0; 12]);
    g.push_reg(R::V0);
    let load = g.gc_gen.as_ref().unwrap().labels.root_load_actor;
    graph::call_owner(g, load);
    g.load_ptr_disp32(R::V0, R::V0, copy::MARK_RECORD);
    g.mov_data_addr(R::V1, state);
    g.store_ptr_disp32(R::V1, RECORD, R::V0);
    g.mov_data_addr(R::V0, handle);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    let skip = g.create_text_label();
    let work = g.create_text_label();
    let published = g.create_text_label();
    let end = g.create_text_label();
    g.jmp_label(skip);
    let saved = [
        R::OwnerContext,
        R::V1,
        R::V2,
        R::V3,
        R::V6,
        R::V7,
        R::V8,
        R::V9,
        R::V10,
        R::V11,
    ];
    g.bind_text_label(work);
    for r in saved {
        g.push_reg(r);
    }
    let allowed = g.create_text_label();
    let work_done = g.create_text_label();
    read(g, state, ALLOW_WORK);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, allowed);
    increment(g, state, DEFERRED);
    g.mov_imm64(R::V0, 1);
    g.jmp_label(work_done);
    g.bind_text_label(allowed);
    g.mov_imm64(R::V0, 0);
    g.bind_text_label(work_done);
    for r in saved.into_iter().rev() {
        g.pop_reg(r);
    }
    g.ret();
    g.bind_text_label(published);
    quiescence::save(g);
    increment(g, state, PUBLISHED);
    if !gate_first {
        // The root is published and its counted journal debt is still live:
        // the ticket remains held until another thread enters the end API.
        deadline(g);
        wait(g, state, END_ENTERED, 1);
    }
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(end);
    quiescence::save(g);
    let no_publication = g.create_text_label();
    read(g, state, PUBLISHED);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, no_publication);
    read(g, state, END_ENTERED);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, no_publication);
    lea_rt(g, R::V1, root_work::PENDING);
    g.atomic_load_acquire(R::V0, R::V1);
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, fail);
    g.mov_data_addr(R::V1, state);
    g.add_reg_imm32(R::V1, DEBT_AT_END);
    g.atomic_store_release(R::V1, R::V0);
    write(g, state, END_ENTERED, 1);
    g.bind_text_label(no_publication);
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(skip);
    let l = &mut g.gc_gen.as_mut().unwrap().labels;
    l.weak_work_hook = Some(work);
    l.weak_publish_hook = Some(published);
    l.weak_end_enter_hook = Some(end);
    g.pop_reg(R::V0);
    state
}
pub(crate) fn run_parallel_weak_race(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    outputs: &[crate::DataLabel],
    state: crate::DataLabel,
    young: bool,
    gate_first: bool,
) -> ParallelActors {
    // Retire the real allocation scratch and its pre-snapshot journal while
    // no lane is active. Clearing only the source frame cell leaves that
    // private scratch eligible for conservative start-round admission.
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (enter, exit, enable) = (
        l.native_enter_actor,
        l.native_exit_actor,
        l.moving_enable_actor,
    );
    graph::call_owner(g, enter);
    graph::call_owner(g, exit);
    graph::call_owner(g, enable);
    super::dual_mark_entry(g, young, true);
    run_parallel_impl(
        g,
        actors,
        1,
        Some(outputs),
        GraphMode::WeakRace(state, young, gate_first),
    )
}
fn load(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel) {
    let entry = g.gc_gen.as_ref().unwrap().labels.weak_load_actor;
    read(g, state, 0);
    g.mov_reg_reg(R::V7, R::V0);
    g.load_rbp_slot(R::V1, 8);
    g.load_ptr_disp32(R::V6, R::V1, HUB_ROOT);
    graph::call_owner(g, entry);
}
pub(super) fn child(
    g: &mut crate::NativeCodeGenerator,
    state: crate::DataLabel,
    _young: bool,
    gate_first: bool,
    ready: crate::DataLabel,
    finish_gate: crate::DataLabel,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (exit, enter, fail) = (l.native_exit_actor, l.native_enter_actor, l.fail);
    graph::call_owner(g, exit);
    load(g, state);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, GRAPH_READS, R::V0);
    if gate_first {
        g.cmp_reg_imm8(R::V0, 2);
        g.jcc_label(C::NotEqual, fail);
    } else {
        let admitted = g.create_text_label();
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::Equal, admitted);
        g.cmp_reg_imm8(R::V0, 2);
        g.jcc_label(C::NotEqual, fail);
        g.bind_text_label(admitted);
    }
    graph::call_owner(g, enter);
    increment(g, state, ATTEMPTED);
    deadline(g);
    wait(g, state, RETRY, 1);
    graph::call_owner(g, exit);
    g.load_rbp_slot(R::V1, 8);
    g.load_ptr_disp32(R::V0, R::V1, GRAPH_READS);
    let loaded = g.create_text_label();
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, loaded);
    load(g, state);
    g.bind_text_label(loaded);
    g.cmp_reg_imm8(R::V0, u8::from(gate_first) as i8);
    g.jcc_label(C::NotEqual, fail);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, DELAYED_ACKS, R::V0);
    graph::call_owner(g, enter);
    increment(g, state, FINISHED);
    scheduling::schedule_arrive(g, ready);
    deadline(g);
    wait(g, finish_gate, 0, 1);
}
fn end(g: &mut crate::NativeCodeGenerator, young: bool) {
    super::dual_mark_entry(g, young, false);
}
pub(super) fn coordinator(
    g: &mut crate::NativeCodeGenerator,
    owners: u64,
    state: crate::DataLabel,
    young: bool,
    gate_first: bool,
    start: crate::DataLabel,
    finish_gate: crate::DataLabel,
) {
    // The genuine source metadata is protected by the weak entry until clear.
    // Check the actual fresh snapshot, before any weak load can admit it.
    read(g, state, RECORD);
    g.load_ptr_disp32(
        R::V0,
        R::V0,
        d(if young {
            offset_of!(MarkRecord, young_epoch)
        } else {
            offset_of!(MarkRecord, old_epoch)
        }),
    );
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    load_rt(
        g,
        R::V2,
        if young {
            young_mark::EPOCH
        } else {
            RT_MARK_CYCLES
        },
    );
    g.cmp_reg_reg(R::V0, R::V2);
    g.jcc_label(C::Equal, fail);
    write(g, state, WHITE, 1);
    if gate_first {
        deadline(g);
        let again = g.create_text_label();
        let closed = g.create_text_label();
        g.bind_text_label(again);
        end(g, young);
        load_rt(g, R::V0, weak::CLOSED);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, closed);
        tick(g);
        g.jmp_label(again);
        g.bind_text_label(closed);
        write(g, state, GATE_OBSERVED, 1);
    }
    write(g, start, 0, 1);
    if !gate_first {
        deadline(g);
        wait(g, state, PUBLISHED, 1);
        end(g, young);
    }
    deadline(g);
    wait(g, state, ATTEMPTED, owners);
    write(g, state, ALLOW_WORK, 1);
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    worker::request_work(g);
    worker::unlock(g, &rt.labels);
    worker::wake_if_pending(g, &rt.labels);
    g.gc_gen = Some(rt);
    deadline(g);
    let again = g.create_text_label();
    let closed = g.create_text_label();
    g.bind_text_label(again);
    end(g, young);
    load_rt(
        g,
        R::V0,
        if young {
            young_mark::ACTIVE
        } else {
            RT_MARK_ACTIVE
        },
    );
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, closed);
    tick(g);
    g.jmp_label(again);
    g.bind_text_label(closed);
    load_rt(g, R::V0, weak::CLOSED);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    write(g, state, RETRY, 1);
    deadline(g);
    wait(g, state, FINISHED, owners);
    // Output cells, not raw returned words, retain the graph across movement.
    store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
    let move_start = g.gc_gen.as_ref().unwrap().labels.moving_start_actor;
    graph::call_owner(g, move_start);
    // End is one bounded attempt. Service its actual continuations through
    // the public poll/end APIs until relocation, retirement and resumes finish.
    next_mark::finish_move(g);
    let enter = g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
    graph::call_owner(g, enter);
    write(g, finish_gate, 0, 1);
}
pub(crate) fn weak_race_metric(
    g: &mut crate::NativeCodeGenerator,
    state: crate::DataLabel,
    field: usize,
) {
    read(
        g,
        state,
        [
            PUBLISHED,
            END_ENTERED,
            ATTEMPTED,
            FINISHED,
            GATE_OBSERVED,
            DEBT_AT_END,
            DEFERRED,
            WHITE,
        ][field],
    );
}
