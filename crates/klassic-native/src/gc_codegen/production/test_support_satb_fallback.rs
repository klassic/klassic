//! Park the real worker outside its ticket during a real relocation interval.
use super::super::*;
use super::call_owner;

fn flag(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel, field: i32, value: u64) {
    g.mov_data_addr(R::V1, data);
    g.add_reg_imm32(R::V1, field);
    g.mov_imm64(R::V0, value);
    g.atomic_store_release(R::V1, R::V0);
}
fn wait_flag(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel, field: i32, value: i32) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let w = l.worker.as_ref().unwrap();
    let (clock, relax, fail) = (w.hooks.clock, w.hooks.relax, l.fail);
    g.call_label(clock);
    g.mov_imm64(R::V10, 20_000_000_000);
    g.add_reg_reg(R::V10, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    g.mov_data_addr(R::V1, data);
    g.add_reg_imm32(R::V1, field);
    g.atomic_load_acquire(R::V0, R::V1);
    g.cmp_reg_imm32(R::V0, value);
    g.jcc_label(C::Equal, done);
    g.call_label(clock);
    g.cmp_reg_reg(R::V0, R::V10);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
pub(crate) fn satb_fallback_prepare(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    let data = g.asm.data_label_with_i64s(&[0, 0]);
    let hook = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.worker_before_lock_hook = Some(hook);
    let body = g.create_text_label();
    let done = g.create_text_label();
    g.jmp_label(body);
    g.bind_text_label(hook);
    quiescence::save(g);
    g.mov_data_addr(R::V1, data);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    load_rt(g, R::V0, RT_PHASE_OPEN);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    flag(g, data, 8, 1);
    wait_flag(g, data, 0, 0);
    flag(g, data, 8, 0);
    g.bind_text_label(done);
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(body);
    data
}
pub(crate) fn satb_fallback_open(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel) {
    flag(g, data, 0, 1);
    let start = g.gc_gen.as_ref().unwrap().labels.moving_start_actor;
    call_owner(g, start);
    wait_flag(g, data, 8, 1);
}
pub(crate) fn satb_fallback_finish(g: &mut crate::NativeCodeGenerator, data: crate::DataLabel) {
    flag(g, data, 0, 0);
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (end, poll, relax) = (
        l.moving_end_actor,
        l.poll_actor,
        l.worker.as_ref().unwrap().hooks.relax,
    );
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    call_owner(g, end);
    call_owner(g, poll);
    load_rt(g, R::V0, RT_PHASE_OPEN);
    load_rt(g, R::V1, RT_RETIRE_STAGE);
    g.or_reg_reg(R::V0, R::V1);
    load_rt(g, R::V1, phase_round::RESUMING);
    g.or_reg_reg(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
