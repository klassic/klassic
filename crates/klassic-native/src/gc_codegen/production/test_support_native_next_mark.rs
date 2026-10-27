//! Native peers cross completed physical banks and subsequent real logical
//! snapshots. No fabricated phase/epoch or completion hint releases peers.
use super::*;
pub(crate) fn run_parallel_native_next_mark(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    roots: &[crate::DataLabel],
    cycles: u64,
    full: bool,
    marks: u64,
) -> ParallelActors {
    let enable = g.gc_gen.as_ref().unwrap().labels.moving_enable_actor;
    graph::call_owner(g, enable);
    run_parallel_impl(
        g,
        actors,
        1,
        Some(roots),
        GraphMode::NativeMarks(cycles, full, marks),
    )
}
pub(super) fn collect_and_mark(
    g: &mut crate::NativeCodeGenerator,
    cycles: u64,
    full: bool,
    marks: u64,
) {
    for _ in 0..cycles {
        if full {
            store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
        }
        let start = g.gc_gen.as_ref().unwrap().labels.moving_start_actor;
        graph::call_owner(g, start);
        finish_move(g);
    }
    if marks & 1 != 0 {
        let old = g.gc_gen.as_ref().unwrap().labels.old_start_actor;
        graph::call_owner(g, old);
    }
    let young = g.gc_gen.as_ref().unwrap().labels.young_start_actor;
    graph::call_owner(g, young);
    if marks & 2 != 0 {
        finish_mark(g, true);
        if marks & 1 != 0 {
            finish_mark(g, false);
        }
    }
}
pub(crate) fn finish_move(g: &mut crate::NativeCodeGenerator) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (end, poll, clock, relax, fail) = (
        l.moving_end_actor,
        l.poll_actor,
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    deadline(g, clock);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    graph::call_owner(g, end);
    graph::call_owner(g, poll);
    load_rt(g, R::V0, RT_PHASE_OPEN);
    load_rt(g, R::V1, RT_RETIRE_STAGE);
    g.or_reg_reg(R::V0, R::V1);
    load_rt(g, R::V1, phase_round::RESUMING);
    g.or_reg_reg(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    wait(g, clock, relax, fail);
    g.jmp_label(again);
    g.bind_text_label(done);
}
fn finish_mark(g: &mut crate::NativeCodeGenerator, young: bool) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (end, poll, clock, relax, fail) = (
        if young {
            l.young_end_actor
        } else {
            l.old_end_actor
        },
        l.poll_actor,
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    deadline(g, clock);
    let again = g.create_text_label();
    let service = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    graph::call_owner(g, poll);
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
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
    let closed = g.create_text_label();
    g.jcc_label(C::Equal, closed);
    load_rt(
        g,
        R::V0,
        if young {
            young_mark::DONE
        } else {
            RT_MARK_DONE
        },
    );
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, service);
    load_rt(g, R::V0, root_work::PENDING);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, service);
    worker::unlock(g, &rt.labels);
    g.call_label(end);
    wait(g, clock, relax, fail);
    g.jmp_label(again);
    g.bind_text_label(service);
    worker::request_work(g);
    worker::unlock(g, &rt.labels);
    worker::wake_if_pending(g, &rt.labels);
    wait(g, clock, relax, fail);
    g.jmp_label(again);
    g.bind_text_label(closed);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
    g.bind_text_label(done);
}
fn deadline(g: &mut crate::NativeCodeGenerator, clock: crate::TextLabel) {
    g.call_label(clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1504, R::V0);
}
fn wait(
    g: &mut crate::NativeCodeGenerator,
    clock: crate::TextLabel,
    relax: crate::TextLabel,
    fail: crate::TextLabel,
) {
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1504);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
}
pub(crate) fn native_next_metric(g: &mut crate::NativeCodeGenerator, which: usize) {
    load_rt(
        g,
        R::V0,
        [
            young_mark::ACTIVE,
            young_mark::VALID,
            young_mark::EPOCH,
            RT_MARK_ACTIVE,
            RT_MARK_VALID,
            RT_MARK_CYCLES,
            registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
        ][which],
    );
}

pub(crate) fn native_next_fault(g: &mut crate::NativeCodeGenerator, actor: TestActor, mode: u64) {
    let enable = g.gc_gen.as_ref().unwrap().labels.moving_enable_actor;
    graph::call_owner(g, enable);
    let old = mode >= 5;
    let selected = matches!(mode, 3 | 4 | 8);
    collect_and_mark(g, 1, false, u64::from(old) | (u64::from(selected) << 1));
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    let (active, epoch, valid) = if old {
        (RT_MARK_ACTIVE, RT_MARK_CYCLES, RT_MARK_VALID)
    } else {
        (young_mark::ACTIVE, young_mark::EPOCH, young_mark::VALID)
    };
    match mode {
        0 | 5 => store_rt_imm(g, active, 0),
        1 | 6 => store_rt_imm(g, epoch, 0),
        2 | 7 => {
            load_rt(g, R::V0, epoch);
            g.add_reg_imm32(R::V0, 1);
            store_rt(g, epoch, R::V0);
        }
        3 | 8 => store_rt_imm(g, valid, 0),
        4 => store_rt_imm(g, active, 1),
        _ => unreachable!(),
    }
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
    select(g, actor);
    let exit = g.gc_gen.as_ref().unwrap().labels.native_exit_actor;
    graph::call_owner(g, exit);
    // Acceptance exits successfully immediately; no later guard can mask it.
    g.asm.mov_imm64(X::Rax, 231);
    g.asm.mov_imm64(X::Rdi, 0);
    g.asm.syscall();
}
