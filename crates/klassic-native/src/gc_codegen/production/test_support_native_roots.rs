use super::super::*;
pub(crate) fn native_root_call(
    g: &mut crate::NativeCodeGenerator,
    handle: crate::DataLabel,
    op: u64,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = match op {
        0 => l.native_root_new_actor,
        1 => l.native_root_load_actor,
        2 => l.native_root_release_actor,
        _ => unreachable!(),
    };
    g.mov_data_addr(R::V7, handle);
    super::call_owner(g, entry);
}
pub(crate) fn native_root_active(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, native_roots::ACTIVE);
}

pub(crate) fn native_root_reader(
    g: &mut crate::NativeCodeGenerator,
    handle: crate::DataLabel,
    request: Option<crate::DataLabel>,
) -> crate::DataLabel {
    native_roots::reader_probe(g, handle, request)
}
pub(crate) fn native_root_unmap_fail_next(g: &mut crate::NativeCodeGenerator) {
    metadata::fail_release_next(g);
}
pub(crate) fn native_root_actor_prepare(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    source: crate::DataLabel,
) -> (crate::DataLabel, crate::DataLabel) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    let handle = super::lease_request(g);
    g.mov_data_addr(R::V0, source);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    native_root_call(g, handle, 0);
    super::lease_require_ok(g);
    let store = g.gc_gen.as_ref().unwrap().labels.root_store_actor;
    g.mov_data_addr(R::V1, source);
    g.mov_imm64(R::V0, 0);
    super::call_owner(g, store);
    let output = g.asm.data_label_with_i64s(&[0]);
    let adopt = g.gc_gen.as_ref().unwrap().labels.root_adopt_actor;
    g.mov_data_addr(R::V0, output);
    super::call_owner(g, adopt);
    g.pop_reg(R::OwnerContext);
    (handle, output)
}
pub(crate) fn native_root_actor_load_id(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    handle: crate::DataLabel,
    output: crate::DataLabel,
) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    g.mov_data_addr(R::V6, output);
    g.load_ptr_disp32(R::V6, R::V6, 0);
    native_root_call(g, handle, 1);
    super::lease_require_ok(g);
    let load = g.gc_gen.as_ref().unwrap().labels.root_load_actor;
    g.mov_data_addr(R::V0, output);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    super::call_owner(g, load);
    g.load_ptr_disp32(R::V0, R::V0, copy::MARK_RECORD);
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(MarkRecord, object_id)));
    g.push_reg(R::V0);
    g.push_reg(R::V0);
    let store = g.gc_gen.as_ref().unwrap().labels.root_store_actor;
    g.mov_data_addr(R::V1, output);
    g.mov_imm64(R::V0, 0);
    super::call_owner(g, store);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    native_root_call(g, handle, 2);
    g.cmp_reg_imm8(R::V0, 2);
    g.jcc_label(C::NotEqual, done);
    let poll = g.gc_gen.as_ref().unwrap().labels.poll_actor;
    super::call_owner(g, poll);
    g.jmp_label(again);
    g.bind_text_label(done);
    super::lease_require_ok(g);
    g.pop_reg(R::V0);
    g.pop_reg(R::V0);
    g.pop_reg(R::OwnerContext);
}
