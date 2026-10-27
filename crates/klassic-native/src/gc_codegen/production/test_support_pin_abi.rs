use super::super::*;
use crate::gc_abi::{NativeApiBinding, ScopedPinApi, ScopedPinCall, ScopedPinScope};

pub(crate) const PIN_C_CALLBACK: u64 = 0xf170_cab1_f170_cab1;
pub(crate) const PIN_C_MODE: u64 = 0xf170_cab2_f170_cab2;

pub(crate) fn pin_abi_fixture(
    g: &mut crate::NativeCodeGenerator,
    source_slot: i32,
) -> (crate::DataLabel, crate::DataLabel) {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let scope = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<ScopedPinScope>() / 8]);
    let user = g
        .asm
        .data_label_with_i64s(&[0, 0, 0, PIN_C_MODE as i64, 0, 0, 0, 0]);
    let external = g.asm.data_label_with_i64s(&[PIN_C_CALLBACK as i64]);
    let bootstrap = g.gc_gen.as_ref().unwrap().labels.native_api_bootstrap;
    g.mov_reg_reg(R::V7, R::OwnerContext);
    g.mov_text_addr(R::V11, bootstrap);
    g.call_native_unary(R::V11);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, g.gc_gen.as_ref().unwrap().labels.fail);
    g.mov_reg_reg(R::V8, R::V0);
    g.mov_data_addr(R::V9, user);
    g.store_ptr_disp32(R::V9, 56, R::V8);
    g.mov_text_addr(R::V0, bootstrap);
    g.store_ptr_disp32(R::V9, 48, R::V0);
    g.mov_data_addr(R::V9, scope);
    g.load_ptr_disp32(R::V0, R::V8, d(offset_of!(NativeApiBinding, context)));
    g.store_ptr_disp32(R::V9, d(offset_of!(ScopedPinCall, context)), R::V0);
    g.load_ptr_disp32(R::V0, R::V8, d(offset_of!(NativeApiBinding, pins)));
    g.store_ptr_disp32(R::V9, d(offset_of!(ScopedPinScope, api)), R::V0);
    g.load_rbp_slot(R::V0, source_slot);
    g.store_ptr_disp32(R::V9, d(offset_of!(ScopedPinCall, source)), R::V0);
    g.mov_data_addr(R::V0, user);
    g.store_ptr_disp32(R::V9, d(offset_of!(ScopedPinScope, user)), R::V0);
    g.mov_data_addr(R::V0, external);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    g.store_ptr_disp32(R::V9, d(offset_of!(ScopedPinScope, callback)), R::V0);
    let managed = g.create_text_label();
    let fail_unmap = g.create_text_label();
    g.mov_data_addr(R::V9, user);
    g.mov_text_addr(R::V0, managed);
    g.store_ptr_disp32(R::V9, 0, R::V0);
    g.mov_text_addr(R::V0, fail_unmap);
    g.store_ptr_disp32(R::V9, 8, R::V0);
    let body = g.create_text_label();
    g.jmp_label(body);
    let rt = g.gc_gen.take().unwrap();
    pin_abi_emit_bridges(g, &rt.labels, managed, fail_unmap);
    g.gc_gen = Some(rt);
    g.bind_text_label(body);
    (scope, user)
}
pub(crate) fn pin_abi_scope_call(g: &mut crate::NativeCodeGenerator, scope: crate::DataLabel) {
    g.mov_data_addr(R::V0, scope);
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(ScopedPinScope, api)));
    g.load_ptr_disp32(R::V11, R::V0, d(offset_of!(ScopedPinApi, scope)));
    g.mov_data_addr(R::V7, scope);
    g.call_native_unary(R::V11);
}
pub(crate) fn pin_abi_embed_callback(
    g: &mut crate::NativeCodeGenerator,
    scope: crate::DataLabel,
    user: crate::DataLabel,
    code: &[u8],
    mode: u64,
) {
    let callback = g.create_text_label();
    let after = g.create_text_label();
    g.jmp_label(after);
    g.bind_text_label(callback);
    g.asm.bytes(code);
    g.bind_text_label(after);
    g.mov_data_addr(R::V9, scope);
    g.mov_text_addr(R::V0, callback);
    g.store_ptr_disp32(R::V9, d(offset_of!(ScopedPinScope, callback)), R::V0);
    g.mov_data_addr(R::V9, user);
    g.mov_imm64(R::V0, mode);
    g.store_ptr_disp32(R::V9, 24, R::V0);
}
pub(crate) fn pin_abi_scope_word(
    g: &mut crate::NativeCodeGenerator,
    scope: crate::DataLabel,
    index: usize,
) {
    let off = [
        offset_of!(ScopedPinScope, result),
        offset_of!(ScopedPinScope, stage),
        offset_of!(ScopedPinScope, last_status),
        offset_of!(ScopedPinScope, call) + offset_of!(ScopedPinCall, address),
        offset_of!(ScopedPinScope, error),
    ][index];
    g.mov_data_addr(R::V1, scope);
    g.load_ptr_disp32(R::V0, R::V1, d(off));
}
pub(crate) fn pin_abi_scope_unused_body(
    g: &mut crate::NativeCodeGenerator,
    scope: crate::DataLabel,
) {
    let callback = g.create_text_label();
    let after = g.create_text_label();
    g.mov_data_addr(R::V9, scope);
    g.mov_text_addr(R::V0, callback);
    g.store_ptr_disp32(R::V9, d(offset_of!(ScopedPinScope, callback)), R::V0);
    g.jmp_label(after);
    g.bind_text_label(callback);
    g.native_entry_begin(16);
    g.load_ptr_disp32(R::V1, R::V7, d(offset_of!(ScopedPinScope, user)));
    g.load_ptr_disp32(R::V0, R::V1, 16);
    g.add_reg_imm32(R::V0, 1);
    g.store_ptr_disp32(R::V1, 16, R::V0);
    g.mov_imm64(R::V0, 77);
    g.native_entry_end();
    g.ret();
    g.bind_text_label(after);
}
pub(crate) fn pin_abi_scope_wait(g: &mut crate::NativeCodeGenerator, scope: crate::DataLabel) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1880, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    pin_abi_scope_call(g, scope);
    g.cmp_reg_imm8(R::V0, 2);
    let retry = g.create_text_label();
    let pending = g.create_text_label();
    g.jcc_label(C::Equal, pending);
    g.cmp_reg_imm8(R::V0, 6);
    g.jcc_label(C::NotEqual, done);
    g.mov_data_addr(R::V1, scope);
    g.load_ptr_disp32(R::V1, R::V1, d(offset_of!(ScopedPinScope, user)));
    g.load_ptr_disp32(R::V0, R::V1, 32);
    g.add_reg_imm32(R::V0, 1);
    g.store_ptr_disp32(R::V1, 32, R::V0);
    g.jmp_label(retry);
    g.bind_text_label(pending);
    g.mov_data_addr(R::V1, scope);
    g.load_ptr_disp32(R::V0, R::V1, d(offset_of!(ScopedPinScope, stage)));
    g.cmp_reg_imm8(R::V0, 4);
    g.jcc_label(C::NotEqual, retry);
    g.load_ptr_disp32(
        R::V0,
        R::V1,
        d(offset_of!(ScopedPinScope, call) + offset_of!(ScopedPinCall, address)),
    );
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.load_ptr_disp32(
        R::V0,
        R::V1,
        d(offset_of!(ScopedPinScope, token) + offset_of!(crate::gc_abi::ScopedPinToken, uses)),
    );
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, retry);
    g.cmp_reg_imm8(R::V0, 1);
    g.jcc_label(C::NotEqual, fail);
    g.load_ptr_disp32(R::V1, R::V1, d(offset_of!(ScopedPinScope, user)));
    g.load_ptr_disp32(R::V0, R::V1, 40);
    g.add_reg_imm32(R::V0, 1);
    g.store_ptr_disp32(R::V1, 40, R::V0);
    // Actual unmap failure keeps the unique scope at stage4, and never
    // reexecutes its body. The following scope step performs the real retry.
    g.bind_text_label(retry);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1880);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    super::lease_require_ok(g);
}

/// Native bridges shared by complete x86-64 and AArch64 fixtures. Success
/// requires an actual additional Full flip and its actual retirement closure.
pub(crate) fn pin_abi_emit_bridges<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    managed: E::TextLabel,
    fail_unmap: E::TextLabel,
) {
    a.bind_text_label(managed);
    a.native_entry_begin(32);
    a.load_ptr_disp32(
        R::OwnerContext,
        R::V7,
        d(offset_of!(ScopedPinCall, context)),
    );
    owner_state::bind_running(a, l);
    a.mov_imm64(R::V7, 32);
    a.mov_imm64(R::V6, 1);
    a.call_label(l.alloc_actor);
    load_rt(a, R::V0, RT_FULL_COLLECTIONS);
    a.store_rbp_slot(8, R::V0);
    let w = &l.worker.as_ref().unwrap().hooks;
    a.call_label(w.clock);
    a.mov_imm64(R::V1, 10_000_000_000);
    a.add_reg_reg(R::V0, R::V1);
    a.store_rbp_slot(16, R::V0);
    store_rt_imm(a, old_cycle::REQUEST, old_cycle::CAPACITY);
    let again = a.create_text_label();
    let open = a.create_text_label();
    let start = a.create_text_label();
    let poll = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(again);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    load_rt(a, R::V1, RT_RETIRE_STAGE);
    a.or_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, phase_round::RESUMING);
    a.or_reg_reg(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, open);
    load_rt(a, R::V0, RT_FULL_COLLECTIONS);
    a.load_rbp_slot(R::V1, 8);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, done);
    a.jmp_label(start);
    a.bind_text_label(open);
    a.call_label(l.moving_end_actor);
    a.jmp_label(poll);
    a.bind_text_label(start);
    a.call_label(l.moving_start_actor);
    a.bind_text_label(poll);
    a.call_label(l.poll_actor);
    a.call_label(w.clock);
    a.load_rbp_slot(R::V1, 16);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, l.fail);
    a.call_label(w.relax);
    a.jmp_label(again);
    a.bind_text_label(done);
    a.mov_imm64(R::V0, 0);
    a.native_entry_end();
    a.ret();
    a.bind_text_label(fail_unmap);
    a.native_entry_begin(16);
    a.load_ptr_disp32(
        R::OwnerContext,
        R::V7,
        d(offset_of!(ScopedPinCall, context)),
    );
    owner_state::bind_running(a, l);
    metadata::fail_release_next(a);
    a.mov_imm64(R::V0, 0);
    a.native_entry_end();
    a.ret();
}
