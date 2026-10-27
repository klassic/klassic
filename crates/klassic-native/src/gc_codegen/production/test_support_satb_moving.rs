//! Observe the real relocated Old frontier without introducing an early root.
use super::super::*;
use super::call_owner;

/// Search at most32 actual queue entries for a pre-recorded object identity.
/// No forwarding/load barrier repairs a stale queue word during this probe.
/// V0 returns the canonical destination payload address, or zero if absent.
pub(crate) fn satb_moving_frontier(g: &mut crate::NativeCodeGenerator, identity: crate::DataLabel) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    g.mov_data_addr(R::V1, identity);
    g.load_ptr_disp32(R::V7, R::V1, 0);
    load_rt(g, R::V9, RT_MSTACK_LEN);
    g.cmp_reg_imm32(R::V9, 32);
    g.jcc_label(C::Above, rt.labels.fail);
    load_rt(g, R::V6, mark_queue::entries(false));
    g.mov_imm64(R::V8, 0);
    let again = g.create_text_label();
    let candidate = g.create_text_label();
    let next = g.create_text_label();
    let missing = g.create_text_label();
    let found = g.create_text_label();
    load_rt(g, R::V0, RT_M_OBJ);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, candidate);
    g.bind_text_label(again);
    g.cmp_reg_reg(R::V8, R::V9);
    g.jcc_label(C::AboveOrEqual, missing);
    g.mov_reg_reg(R::V1, R::V8);
    g.shl_reg_imm8(R::V1, 3);
    g.add_reg_reg(R::V1, R::V6);
    g.load_ptr_disp32(R::V0, R::V1, 0);
    g.add_reg_imm32(R::V8, 1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, next);
    g.bind_text_label(candidate);
    g.shr_reg_imm8(R::V0, 16);
    load_rt(g, R::V1, RT_HEAP_BASE);
    g.add_reg_reg(R::V0, R::V1);
    g.load_ptr_disp32(
        R::V1,
        R::V0,
        offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
    );
    g.test_reg_reg(R::V1, R::V1);
    g.jcc_label(C::Equal, rt.labels.fail);
    g.load_ptr_disp32(R::V2, R::V1, offset_of!(MarkRecord, object_id) as i32);
    g.cmp_reg_reg(R::V2, R::V7);
    g.jcc_label(C::Equal, found);
    g.bind_text_label(next);
    g.jmp_label(again);
    g.bind_text_label(missing);
    g.mov_imm64(R::V0, 0);
    g.bind_text_label(found);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}

/// This fixture explicitly enabled movement, but not automatic move scheduling.
/// Close Old by its actual phase entry instead of relying on disabled policies.
pub(crate) fn satb_moving_close_old(g: &mut crate::NativeCodeGenerator) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (end, clock, relax, fail) = (
        l.old_end_actor,
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 20_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1984, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    call_owner(g, end);
    load_rt(g, R::V0, RT_MARK_ACTIVE);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1984);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
