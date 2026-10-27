//! Observe actual pool threads and bound the work allowed to each test worker.
//! The owner never calls the trace entry or manufactures a completion count.
use super::super::*;

pub(crate) fn pool_slot_address(g: &mut crate::NativeCodeGenerator, index: usize) {
    lea_rt(g, R::V0, worker_pool::slot(index));
}

pub(crate) fn gc_worker_pool_metric(g: &mut crate::NativeCodeGenerator, field: usize) {
    load_rt(
        g,
        R::V0,
        [
            worker_pool::LIVE,
            worker_pool::LAUNCHED,
            worker_pool::STOPPED_COUNT,
            worker_pool::ACTIVE,
            worker_pool::ENTERED,
            worker_pool::COMPLETED,
        ][field],
    );
}

pub(crate) fn pool_worker_metric(g: &mut crate::NativeCodeGenerator, index: usize, field: usize) {
    let fields = [
        worker_pool::STATUS,
        worker_pool::OLD_UNITS,
        worker_pool::YOUNG_UNITS,
        worker_pool::RELOCATE_UNITS,
        worker_view::ACTIVE,
        worker_view::ENTERED,
        worker_view::COMPLETED,
    ];
    load_rt(g, R::V0, worker_pool::field(index, fields[field]));
}

pub(crate) fn pool_linux_metric(g: &mut crate::NativeCodeGenerator, index: usize, field: usize) {
    use super::super::super::linux_production::State;
    load_rt(
        g,
        R::V0,
        worker_pool::slot(index)
            + offset_of!(worker_pool::Slot, linux)
            + [
                offset_of!(State, tid),
                offset_of!(State, stack),
                offset_of!(State, observed_tls),
                offset_of!(State, joins),
                offset_of!(State, releases),
                offset_of!(State, bootstrap),
            ][field],
    );
}

pub(crate) fn pool_stop(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::shutdown(g, &rt.labels, rt.data.runtime);
    g.gc_gen = Some(rt);
}

// Permission is bounded separately for Young/Old, per actual worker identity.
// Word4 selects a worker blocked before its ticket; word5 is its receipt.
pub(crate) fn pool_gates(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    let mut rt = g.gc_gen.take().unwrap();
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let data = g.asm.data_label_with_i64s(&[0, 0, 0, 0, 2, 0]);
    let after = g.create_text_label();
    g.jmp_label(after);
    for young in [false, true] {
        let hook = g.create_text_label();
        let blocked = g.create_text_label();
        let end = g.create_text_label();
        if young {
            rt.labels.young_mark_gate = Some(hook);
        } else {
            rt.labels.phase_mark_gate = Some(hook);
        }
        g.bind_text_label(hook);
        for reg in [R::V1, R::V2] {
            g.push_reg(reg);
        }
        load_rt(g, R::V1, worker_pool::IDENTITY);
        g.shl_reg_imm8(R::V1, 3);
        g.mov_data_addr(R::V2, data);
        g.add_reg_reg(R::V2, R::V1);
        if young {
            g.add_reg_imm32(R::V2, 16);
        }
        g.atomic_load_acquire(R::V1, R::V2);
        load_rt(
            g,
            R::V0,
            if young {
                worker_pool::YOUNG_UNITS
            } else {
                worker_pool::OLD_UNITS
            },
        );
        g.cmp_reg_reg(R::V0, R::V1);
        g.jcc_label(C::AboveOrEqual, blocked);
        g.mov_imm64(R::V0, 1);
        g.jmp_label(end);
        g.bind_text_label(blocked);
        g.mov_imm64(R::V0, 0);
        g.bind_text_label(end);
        for reg in [R::V2, R::V1] {
            g.pop_reg(reg);
        }
        g.ret();
    }
    let hook = g.create_text_label();
    rt.labels.worker_before_lock_hook = Some(hook);
    g.bind_text_label(hook);
    quiescence::save(g);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    g.mov_data_addr(R::V1, data);
    g.add_reg_imm32(R::V1, 32);
    g.atomic_load_acquire(R::V0, R::V1);
    load_rt(g, R::V2, worker_pool::IDENTITY);
    g.cmp_reg_reg(R::V0, R::V2);
    g.jcc_label(C::NotEqual, done);
    load_rt(g, R::V0, worker_view::ACTIVE);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, rt.labels.fail);
    g.add_reg_imm32(R::V1, 8);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
    g.call_label(rt.labels.worker.as_ref().unwrap().hooks.relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(after);
    g.gc_gen = Some(rt);
    data
}

pub(crate) fn pool_set_gate(
    g: &mut crate::NativeCodeGenerator,
    data: crate::DataLabel,
    field: usize,
    value: u64,
) {
    g.mov_data_addr(R::V1, data);
    g.add_reg_imm32(R::V1, field as i32 * 8);
    g.mov_imm64(R::V0, value);
    g.atomic_store_release(R::V1, R::V0);
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    worker::request_work(g);
    worker::unlock(g, &rt.labels);
    worker::wake_if_pending(g, &rt.labels);
    g.gc_gen = Some(rt);
}

/// A bounded observation loop: this caller only takes/relinquishes its ticket.
/// All marking progress must be supplied by the real pool workers.
pub(crate) fn pool_wait_work(
    g: &mut crate::NativeCodeGenerator,
    index: usize,
    field: usize,
    minimum: u64,
) {
    wait(g, |g| pool_worker_metric(g, index, field), minimum);
}

pub(crate) fn pool_wait_parked(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel) {
    wait(
        g,
        |g| {
            g.mov_data_addr(R::V1, data);
            g.add_reg_imm32(R::V1, 40);
            g.atomic_load_acquire(R::V0, R::V1);
        },
        1,
    );
}

fn wait(
    g: &mut crate::NativeCodeGenerator,
    metric: impl FnOnce(&mut crate::NativeCodeGenerator),
    minimum: u64,
) {
    let rt = g.gc_gen.take().unwrap();
    let hooks = &rt.labels.worker.as_ref().unwrap().hooks;
    g.call_label(hooks.clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1984, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    worker::lock(g, &rt.labels);
    metric(g);
    worker::unlock(g, &rt.labels);
    g.mov_imm64(R::V1, minimum);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, done);
    g.call_label(hooks.clock);
    g.load_rbp_slot(R::V1, 1984);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, rt.labels.fail);
    g.call_label(hooks.relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    g.gc_gen = Some(rt);
}
