use super::super::*;
pub(crate) fn scoped_pin_token(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    g.asm
        .data_label_with_i64s(&vec![0; scoped_pin::token_size() / 8])
}
pub(crate) fn scoped_pin_call(
    g: &mut crate::NativeCodeGenerator,
    token: crate::DataLabel,
    op: u64,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = match op {
        0 => l.scoped_pin_new_actor,
        1 => l.scoped_pin_step_actor,
        2 => l.scoped_pin_release_actor,
        3 => l.scoped_pin_native_enter_actor,
        4 => l.scoped_pin_native_exit_actor,
        5 => l.scoped_pin_callback_enter_actor,
        6 => l.scoped_pin_callback_return_actor,
        _ => unreachable!(),
    };
    g.mov_data_addr(R::V7, token);
    super::call_owner(g, entry);
}
pub(crate) fn scoped_pin_field(
    g: &mut crate::NativeCodeGenerator,
    token: crate::DataLabel,
    field: usize,
) {
    g.mov_data_addr(R::V1, token);
    g.load_ptr_disp32(R::V0, R::V1, d(scoped_pin::token_field(field)));
}
pub(crate) fn scoped_pin_copy(
    g: &mut crate::NativeCodeGenerator,
    from: crate::DataLabel,
    to: crate::DataLabel,
) {
    g.mov_data_addr(R::V8, from);
    g.mov_data_addr(R::V9, to);
    for off in (0..scoped_pin::token_size()).step_by(8) {
        g.load_ptr_disp32(R::V0, R::V8, d(off));
        g.store_ptr_disp32(R::V9, d(off), R::V0);
    }
}

pub(crate) fn scoped_pin_actor_call(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    token: crate::DataLabel,
    root: Option<crate::DataLabel>,
    op: u64,
) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V1, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V1, 0);
    if let Some(root) = root {
        g.mov_data_addr(R::V0, root);
        g.load_ptr_disp32(R::V0, R::V0, 0);
    }
    scoped_pin_call(g, token, op);
    g.pop_reg(R::OwnerContext);
}
pub(crate) fn scoped_pin_clear_actor_root(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    root: crate::DataLabel,
) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V1, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V1, 0);
    g.mov_data_addr(R::V1, root);
    g.mov_imm64(R::V0, 0);
    let store = g.gc_gen.as_ref().unwrap().labels.root_store_actor;
    super::call_owner(g, store);
    g.pop_reg(R::OwnerContext);
}

pub(crate) fn scoped_pin_wait(
    g: &mut crate::NativeCodeGenerator,
    token: crate::DataLabel,
    release: bool,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1856, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    scoped_pin_call(g, token, if release { 2 } else { 1 });
    g.cmp_reg_imm8(R::V0, 2);
    g.jcc_label(C::NotEqual, done);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1856);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    super::lease_require_ok(g);
}

pub(crate) fn scoped_pin_actor_release_wait(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    token: crate::DataLabel,
) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V1, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V1, 0);
    scoped_pin_wait(g, token, true);
    g.pop_reg(R::OwnerContext);
}

/// Read-only view of the token's real native root, used only to retain/drop
/// an actual RootLife reader. The view never creates/releases a native handle.
pub(crate) fn scoped_pin_reader(
    g: &mut crate::NativeCodeGenerator,
    token: crate::DataLabel,
    request: Option<crate::DataLabel>,
) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let view = g.asm.data_label_with_i64s(&[0; 3]);
    if request.is_none() {
        g.mov_data_addr(R::V8, token);
        g.mov_data_addr(R::V9, view);
        for off in (0..24).step_by(8) {
            g.load_ptr_disp32(
                R::V0,
                R::V8,
                d(offset_of!(crate::gc_abi::ScopedPinToken, root) + off),
            );
            g.store_ptr_disp32(R::V9, d(off), R::V0);
        }
    }
    super::native_root_reader(g, view, request)
}
