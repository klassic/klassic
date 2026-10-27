//! Native ABI probe shared by direct Win64 and AArch64 emitted-code fixtures.
use super::super::*;
use crate::gc_abi::{NativeApiCall, NativeThreadApi, NativeThreadCall, NativeThreadToken};
const CALL: i32 = 80;
const TOKEN: i32 = 128;
const GETTER: i32 = 16;
const API_PTR: i32 = 152;
fn ptr<E: PortableAsm>(a: &mut E, r: R, slot: i32) {
    a.mov_reg_reg(r, R::V5);
    a.sub_reg_imm32(r, slot);
}
fn invoke<E: PortableAsm>(a: &mut E, op: u64) {
    a.mov_imm64(R::V0, op);
    a.store_rbp_slot(CALL - offset_of!(NativeThreadCall, operation) as i32, R::V0);
    a.load_rbp_slot(R::V8, API_PTR);
    a.load_ptr_disp32(R::V11, R::V8, d(offset_of!(NativeThreadApi, call)));
    ptr(a, R::V7, CALL);
    a.call_native_unary(R::V11);
}
fn ok<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}
fn boundary<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    a.load_rbp_slot(R::V0, CALL - offset_of!(NativeThreadCall, binding) as i32);
    a.store_rbp_slot(GETTER - offset_of!(NativeApiCall, binding) as i32, R::V0);
    a.mov_imm64(R::V0, op);
    a.store_rbp_slot(GETTER - offset_of!(NativeApiCall, operation) as i32, R::V0);
    ptr(a, R::V7, GETTER);
    a.mov_text_addr(R::V11, l.native_api_call);
    a.call_native_unary(R::V11);
    ok(a, l);
}
pub(crate) fn emit_public_thread_api_probe<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm32(R::V4, 256);
    a.push_reg(R::OwnerContext); // original VM owner; also native call alignment
    for slot in (8..=192).step_by(8) {
        a.mov_imm64(R::V0, 0);
        a.store_rbp_slot(slot, R::V0);
    }
    a.call_label(l.worker.as_ref().unwrap().mark_start);
    a.mov_reg_reg(R::V7, R::OwnerContext);
    a.mov_text_addr(R::V11, l.native_api_bootstrap);
    a.call_native_unary(R::V11);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.store_rbp_slot(GETTER, R::V0);
    a.mov_imm64(R::V0, 4);
    a.store_rbp_slot(GETTER - 8, R::V0);
    ptr(a, R::V7, GETTER);
    a.mov_text_addr(R::V11, l.native_api_call);
    a.call_native_unary(R::V11);
    a.cmp_reg_imm32(R::V0, 7);
    a.jcc_label(C::Below, l.fail);
    a.store_rbp_slot(API_PTR, R::V0);
    a.store_rbp_slot(CALL - offset_of!(NativeThreadCall, api) as i32, R::V0);
    ptr(a, R::V0, TOKEN);
    a.store_rbp_slot(CALL - offset_of!(NativeThreadCall, token) as i32, R::V0);
    for off in [
        offset_of!(NativeThreadCall, cells),
        offset_of!(NativeThreadCall, entries),
    ] {
        a.mov_imm64(R::V0, 16);
        a.store_rbp_slot(CALL - off as i32, R::V0);
    }
    invoke(a, 0);
    ok(a, l);
    a.load_rbp_slot(
        R::OwnerContext,
        CALL - offset_of!(NativeThreadCall, context) as i32,
    );
    a.store_rbp_slot(184, R::OwnerContext);
    owner_state::bind_running(a, l);
    a.mov_imm64(R::V7, 24);
    a.mov_imm64(R::V6, 1);
    a.call_label(l.alloc_actor);
    a.mov_imm64(R::V1, 84);
    a.store_ptr_disp32(R::V0, 0, R::V1);
    a.store_rbp_slot(144, R::V0);
    ptr(a, R::V0, 144);
    a.call_label(l.root_adopt_actor);
    a.call_label(l.publish_pending);
    boundary(a, l, 0);
    // Only the original VM owner drives these real mark/moving transitions.
    // The API-created context remains root-only Native with its actual frame.
    a.mov_reg_reg(R::V1, R::V4);
    a.load_ptr_disp32(R::OwnerContext, R::V1, 0);
    worker::lock(a, l);
    super::emit_unbounded_budget(a);
    a.call_label(l.worker.as_ref().unwrap().mark_loop);
    a.call_label(l.mark_end);
    worker::unlock(a, l);
    super::request_full_collection(a);
    a.call_label(l.collect);
    worker::lock(a, l);
    a.call_label(l.finish);
    worker::unlock(a, l);
    load_rt(a, R::V0, RT_FULL_COLLECTIONS);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.load_rbp_slot(R::OwnerContext, 184);
    boundary(a, l, 1);
    a.load_rbp_slot(R::V0, 144);
    a.call_label(l.root_load_actor);
    a.load_ptr_disp32(R::V0, R::V0, 0);
    a.cmp_reg_imm32(R::V0, 84);
    a.jcc_label(C::NotEqual, l.fail);
    invoke(a, 3);
    ok(a, l);
    a.load_rbp_slot(R::V0, CALL - offset_of!(NativeThreadCall, context) as i32);
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, l.fail);
    // Restore the real caller binding before the API releases the other owner.
    a.mov_reg_reg(R::V1, R::V4);
    a.load_ptr_disp32(R::OwnerContext, R::V1, 0);
    a.call_label(l.worker.as_ref().unwrap().hooks.clock);
    a.mov_imm64(R::V1, 20_000_000_000);
    a.add_reg_reg(R::V0, R::V1);
    a.store_rbp_slot(176, R::V0);
    let again = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(again);
    invoke(a, 2);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::NotEqual, l.fail);
    a.call_label(l.worker.as_ref().unwrap().hooks.clock);
    a.load_rbp_slot(R::V1, 176);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, l.fail);
    a.call_label(l.worker.as_ref().unwrap().hooks.relax);
    a.jmp_label(again);
    a.bind_text_label(done);
    for off in (0..size_of::<NativeThreadToken>()).step_by(8) {
        a.load_rbp_slot(R::V0, TOKEN - off as i32);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    invoke(a, 4);
    ok(a, l);
    invoke(a, 2); // empty cleanup must not recreate an OS key
    ok(a, l);
    load_rt(
        a,
        R::V0,
        offset_of!(Runtime, thread_tls) + offset_of!(thread_tls::State, stage),
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.pop_reg(R::OwnerContext);
    a.leave();
}
