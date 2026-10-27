//! Public native thread calls over real runtime-allocated contexts.
use super::super::*;
use super::TestActor;
use crate::gc_abi::{NativeApiCall, NativeThreadApi, NativeThreadCall, NativeThreadToken};
#[derive(Clone, Copy)]
pub(crate) struct ThreadApiFixture {
    pub(crate) call: crate::DataLabel,
    pub(crate) token: crate::DataLabel,
}
pub(crate) fn thread_api_fixture(g: &mut crate::NativeCodeGenerator) -> ThreadApiFixture {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let token = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<NativeThreadToken>() / 8]);
    let call = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<NativeThreadCall>() / 8]);
    let getter = g.asm.data_label_with_i64s(&[0, 4]);
    let (bootstrap, dispatch, fail) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (l.native_api_bootstrap, l.native_api_call, l.fail)
    };
    g.mov_reg_reg(R::V7, R::OwnerContext);
    g.mov_text_addr(R::V11, bootstrap);
    g.call_native_unary(R::V11);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, fail);
    g.mov_data_addr(R::V7, getter);
    g.store_ptr_disp32(R::V7, d(offset_of!(NativeApiCall, binding)), R::V0);
    g.mov_text_addr(R::V11, dispatch);
    g.call_native_unary(R::V11);
    g.cmp_reg_imm32(R::V0, 7);
    g.jcc_label(C::Below, fail);
    g.mov_data_addr(R::V9, call);
    g.store_ptr_disp32(R::V9, d(offset_of!(NativeThreadCall, api)), R::V0);
    g.mov_data_addr(R::V0, token);
    g.store_ptr_disp32(R::V9, d(offset_of!(NativeThreadCall, token)), R::V0);
    for off in [
        offset_of!(NativeThreadCall, cells),
        offset_of!(NativeThreadCall, entries),
    ] {
        g.mov_imm64(R::V0, 16);
        g.store_ptr_disp32(R::V9, d(off), R::V0);
    }
    ThreadApiFixture { call, token }
}
pub(crate) fn thread_api_call(g: &mut crate::NativeCodeGenerator, f: ThreadApiFixture, op: u64) {
    g.mov_data_addr(R::V7, f.call);
    g.mov_imm64(R::V0, op);
    g.store_ptr_disp32(R::V7, d(offset_of!(NativeThreadCall, operation)), R::V0);
    g.load_ptr_disp32(R::V0, R::V7, d(offset_of!(NativeThreadCall, api)));
    g.load_ptr_disp32(R::V11, R::V0, d(offset_of!(NativeThreadApi, call)));
    g.call_native_unary(R::V11);
}
pub(crate) fn thread_api_actor(
    g: &mut crate::NativeCodeGenerator,
    f: ThreadApiFixture,
) -> TestActor {
    let pointer = g.asm.data_label_with_i64s(&[0]);
    g.mov_data_addr(R::V1, f.call);
    g.load_ptr_disp32(R::V0, R::V1, d(offset_of!(NativeThreadCall, context)));
    g.mov_data_addr(R::V1, pointer);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    TestActor { pointer }
}
pub(crate) fn thread_api_same_current(
    g: &mut crate::NativeCodeGenerator,
    f: ThreadApiFixture,
    actor: TestActor,
) {
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    thread_api_call(g, f, 3);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.mov_data_addr(R::V1, f.call);
    g.load_ptr_disp32(R::V0, R::V1, d(offset_of!(NativeThreadCall, context)));
    g.mov_data_addr(R::V1, actor.pointer);
    g.load_ptr_disp32(R::V1, R::V1, 0);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, fail);
    g.mov_imm64(R::V0, 1);
}
pub(crate) fn thread_api_token_empty(g: &mut crate::NativeCodeGenerator, f: ThreadApiFixture) {
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.mov_data_addr(R::V9, f.token);
    for off in (0..size_of::<NativeThreadToken>()).step_by(8) {
        g.load_ptr_disp32(R::V0, R::V9, d(off));
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
    }
    g.mov_imm64(R::V0, 1);
}
pub(crate) fn thread_api_detach_wait(g: &mut crate::NativeCodeGenerator, f: ThreadApiFixture) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 20_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1976, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    thread_api_call(g, f, 2);
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, done);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1976);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
pub(crate) fn thread_api_cells(
    g: &mut crate::NativeCodeGenerator,
    f: ThreadApiFixture,
    cells: u64,
) {
    g.mov_data_addr(R::V9, f.call);
    g.mov_imm64(R::V0, cells);
    g.store_ptr_disp32(R::V9, d(offset_of!(NativeThreadCall, cells)), R::V0);
}
pub(crate) fn thread_api_fail_map(g: &mut crate::NativeCodeGenerator) {
    metadata::fail_next(g);
}
pub(crate) fn thread_api_copied(
    g: &mut crate::NativeCodeGenerator,
    f: ThreadApiFixture,
) -> ThreadApiFixture {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let token = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<NativeThreadToken>() / 8]);
    let call = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<NativeThreadCall>() / 8]);
    for (from, to, bytes) in [
        (f.token, token, size_of::<NativeThreadToken>()),
        (f.call, call, size_of::<NativeThreadCall>()),
    ] {
        g.mov_data_addr(R::V8, from);
        g.mov_data_addr(R::V9, to);
        for off in (0..bytes).step_by(8) {
            g.load_ptr_disp32(R::V0, R::V8, d(off));
            g.store_ptr_disp32(R::V9, d(off), R::V0);
        }
    }
    g.mov_data_addr(R::V9, call);
    g.mov_data_addr(R::V0, token);
    g.store_ptr_disp32(R::V9, d(offset_of!(NativeThreadCall, token)), R::V0);
    ThreadApiFixture { call, token }
}
pub(crate) fn thread_api_outputs_zero(g: &mut crate::NativeCodeGenerator, f: ThreadApiFixture) {
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.mov_data_addr(R::V9, f.call);
    for off in [
        offset_of!(NativeThreadCall, context),
        offset_of!(NativeThreadCall, binding),
    ] {
        g.load_ptr_disp32(R::V0, R::V9, d(off));
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
    }
    g.mov_imm64(R::V0, 1);
}
pub(crate) fn thread_api_boundary(
    g: &mut crate::NativeCodeGenerator,
    f: ThreadApiFixture,
    op: u64,
) {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let request = g.asm.data_label_with_i64s(&[0, op as i64]);
    g.mov_data_addr(R::V9, f.call);
    g.load_ptr_disp32(R::V8, R::V9, d(offset_of!(NativeThreadCall, binding)));
    g.mov_data_addr(R::V7, request);
    g.store_ptr_disp32(R::V7, 0, R::V8);
    g.load_ptr_disp32(
        R::V11,
        R::V8,
        d(offset_of!(crate::gc_abi::NativeApiBinding, call)),
    );
    g.call_native_unary(R::V11);
}
