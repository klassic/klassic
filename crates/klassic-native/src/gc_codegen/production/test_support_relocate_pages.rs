//! Exercise the actual Cheney scanner on real independently owned TLAB pages.
//! No moving phase, bank switch or completion certificate is manufactured.
use super::*;

pub(crate) fn relocate_page_test_fail(g: &crate::NativeCodeGenerator) -> crate::TextLabel {
    g.gc_gen.as_ref().unwrap().labels.fail
}

pub(crate) fn relocate_page_exhaust_epoch(g: &mut crate::NativeCodeGenerator) {
    store_rt_imm(g, relocate_pages::EPOCH, u64::MAX);
    relocate_page_begin(g);
}

pub(crate) fn relocate_page_begin(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    load_rt(g, R::V0, RT_OLD_TOP);
    store_rt(g, RT_OLD_SCAN, R::V0);
    load_rt(g, R::V0, old_runs::ALLOC);
    store_rt(g, old_runs::SCAN, R::V0);
    scan::emit_cheney_init(g, rt.labels.fail);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}

pub(crate) fn relocate_page_quantum(g: &mut crate::NativeCodeGenerator, budget: u64) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    super::super::test_support::emit_budget(g, budget);
    super::super::test_support::set_collecting(g, true);
    g.call_label(rt.labels.worker.as_ref().unwrap().wcheney);
    super::super::test_support::set_collecting(g, false);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}

pub(crate) fn relocate_page_completed(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, RT_CS_COMPLETED);
}

pub(crate) fn relocate_page_one_unit_finish(
    g: &mut crate::NativeCodeGenerator,
) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let stats = g.asm.data_label_with_i64s(&[0, 0]);
    let again = g.create_text_label();
    let done = g.create_text_label();
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.bind_text_label(again);
    relocate_page_quantum(g, 1);
    g.mov_reg_reg(R::V2, R::V0);
    g.mov_data_addr(R::V1, stats);
    load_rt(g, R::V0, RT_CS_COMPLETED);
    g.load_ptr_disp32(R::V6, R::V1, 0);
    g.add_reg_reg(R::V0, R::V6);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.load_ptr_disp32(R::V0, R::V1, 8);
    g.add_reg_imm32(R::V0, 1);
    g.store_ptr_disp32(R::V1, 8, R::V0);
    g.test_reg_reg(R::V2, R::V2);
    g.jcc_label(C::NotEqual, done);
    g.cmp_reg_imm32(R::V0, 64);
    g.jcc_label(C::AboveOrEqual, fail);
    g.jmp_label(again);
    g.bind_text_label(done);
    stats
}

pub(crate) fn relocate_page_test_stat(
    g: &mut crate::NativeCodeGenerator,
    stats: crate::DataLabel,
    index: i32,
) {
    g.mov_data_addr(R::V0, stats);
    g.load_ptr_disp32(R::V0, R::V0, index * 8);
}

pub(crate) fn relocate_page_pending(g: &mut crate::NativeCodeGenerator) {
    let yes = g.create_text_label();
    let done = g.create_text_label();
    relocate_pages::pending(g, yes);
    g.mov_imm64(R::V0, 0);
    g.jmp_label(done);
    g.bind_text_label(yes);
    g.mov_imm64(R::V0, 1);
    g.bind_text_label(done);
}

pub(crate) fn relocate_page_cursor(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    select(g, actor);
    load_rt(g, R::V1, RT_REQUEST + offset_of!(AllocationRequest, page));
    load_rt(g, R::V0, RT_PAGES);
    g.sub_reg_reg(R::V1, R::V0);
    g.mov_reg_reg(R::V0, R::V1);
    g.mov_imm64(R::V1, size_of::<AllocationPage>() as u64);
    g.div_unsigned(R::V1);
    g.shl_reg_imm8(R::V0, 4);
    lea_rt(g, R::V1, layout(g).relocate_pages as usize);
    g.add_reg_reg(R::V0, R::V1);
    g.load_ptr_disp32(R::V0, R::V0, 8);
    restore(g);
}
