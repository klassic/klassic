//! Actual collection between output chunks; no synthetic phase or root state.
use super::super::*;

pub(crate) fn native_buffer_borrow_offset() -> usize {
    native_io::BORROWS
}
pub(crate) fn native_buffer_call(g: &mut crate::NativeCodeGenerator) {
    let entry = g.gc_gen.as_ref().unwrap().labels.write_native_buffer;
    g.call_label(entry);
}

pub(crate) fn rooted_write_depth<E: PortableAsm>(g: &mut E) {
    load_rt(g, R::V0, RT_CELL_SP);
}

pub(crate) fn rooted_write_call(g: &mut crate::NativeCodeGenerator) {
    let entry = g.gc_gen.as_ref().unwrap().labels.write_rooted_string;
    g.call_label(entry);
}

/// Install a native callback run after each chunk in actual Running state.
/// The writer's temporary cell is the object's sole root. Every invocation
/// forces real Full movement, verifies identity and absence of heap borrows,
/// and records one completion. The following chunk must reacquire its address.
pub(crate) fn rooted_write_collect_between_chunks(
    g: &mut crate::NativeCodeGenerator,
) -> crate::DataLabel {
    let runtime = g.gc_gen.take().unwrap();
    let count = g.asm.data_label_with_i64s(&[0]);
    rooted_write_emit_collection_hook(g, &runtime.labels, count);
    g.gc_gen = Some(runtime);
    count
}
pub(crate) fn rooted_write_emit_collection_hook<E: PortableAsm>(
    g: &mut E,
    l: &Labels<E::TextLabel>,
    count: E::DataLabel,
) {
    let hook = g.create_text_label();
    let body = g.create_text_label();
    g.mov_text_addr(R::V0, hook);
    store_rt(
        g,
        owner_state::offset(offset_of!(OwnerState, rooted_write_hook)),
        R::V0,
    );
    g.jmp_label(body);
    g.bind_text_label(hook);
    g.native_entry_begin(48);
    g.store_rbp_slot(8, R::V7);
    owner_state::bind_running(g, l);
    load_rt(g, R::V0, native_io::BORROWS);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, l.fail);
    g.load_rbp_slot(R::V0, 8);
    g.call_label(l.root_load_actor);
    g.store_rbp_slot(16, R::V0);
    g.load_ptr_disp32(
        R::V0,
        R::V0,
        d(offset_of!(ObjectHeader, mark_record)) - copy::HEADER,
    );
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(MarkRecord, object_id)));
    g.store_rbp_slot(24, R::V0);
    load_rt(g, R::V0, RT_FULL_COLLECTIONS);
    g.store_rbp_slot(32, R::V0);
    store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
    g.call_label(l.collect);
    worker::lock(g, l);
    g.call_label(l.finish);
    worker::unlock(g, l);
    load_rt(g, R::V0, RT_FULL_COLLECTIONS);
    g.load_rbp_slot(R::V1, 32);
    g.add_reg_imm32(R::V1, 1);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::Below, l.fail);
    g.load_rbp_slot(R::V0, 8);
    g.call_label(l.root_load_actor);
    g.load_rbp_slot(R::V1, 16);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::Equal, l.fail);
    g.load_ptr_disp32(
        R::V0,
        R::V0,
        d(offset_of!(ObjectHeader, mark_record)) - copy::HEADER,
    );
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(MarkRecord, object_id)));
    g.load_rbp_slot(R::V1, 24);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, l.fail);
    g.mov_data_addr(R::V1, count);
    g.load_ptr_disp32(R::V0, R::V1, 0);
    g.add_reg_imm32(R::V0, 1);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.native_entry_end();
    g.ret();
    g.bind_text_label(body);
}
