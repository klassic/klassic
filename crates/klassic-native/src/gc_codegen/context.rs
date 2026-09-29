//! Shared finite context-state publication. Full lifecycle callers must own the
//! context and hold their coordinator registry lock. This leaf neither grants
//! a heap-access guard nor implements phase/buffer/participant coordination.
use crate::gc_abi::{ABI_VERSION, MutatorContext, ThreadState};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

/// `(V7=ctx, V6=expected_u64, V2=desired_u64) -> V0=status`.
/// Clobbers V0/V1/V8/V9 and flags. All non-state context bytes remain untouched.
/// Status values match gc_context.h: OK/ABI/state/access/edge/private-work = 0/1/2/3/4/5.
/// Self-edges for Running and AtSafepoint support an empty poll and consecutive
/// handshake rounds. Detached storage is initialized before registry publication,
/// never revived through a borrowed context's state word.
#[allow(dead_code)]
pub(crate) fn emit_transition_locked<E: PortableAsm>(a: &mut E) {
    let bad_abi = a.create_text_label();
    let bad_state = a.create_text_label();
    let active_access = a.create_text_label();
    let private_work = a.create_text_label();
    let bad_edge = a.create_text_label();
    let running = a.create_text_label();
    let parked = a.create_text_label();
    let native = a.create_text_label();
    let publish = a.create_text_label();
    let done = a.create_text_label();

    a.atomic_load_acquire_u32(R::V0, R::V7); // immutable abi_version at offset 0
    a.cmp_reg_imm32(R::V0, ABI_VERSION as i32);
    a.jcc_label(C::NotEqual, bad_abi);
    a.mov_reg_reg(R::V8, R::V7);
    a.add_reg_imm32(R::V8, offset_of!(MutatorContext, state) as i32);
    a.atomic_load_acquire_u32(R::V0, R::V8);
    a.cmp_reg_reg(R::V0, R::V6);
    a.jcc_label(C::NotEqual, bad_state);
    a.mov_reg_reg(R::V9, R::V7);
    a.add_reg_imm32(R::V9, offset_of!(MutatorContext, access_epoch) as i32);
    a.atomic_load_acquire(R::V0, R::V9);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, active_access);
    a.mov_reg_reg(R::V9, R::V7);
    a.add_reg_imm32(R::V9, offset_of!(MutatorContext, private_producers) as i32);
    a.atomic_load_acquire(R::V0, R::V9);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, private_work);
    for (state, label) in [
        (ThreadState::Running, running),
        (ThreadState::AtSafepoint, parked),
        (ThreadState::BlockedNative, native),
    ] {
        a.cmp_reg_imm32(R::V6, state as i32);
        a.jcc_label(C::Equal, label);
    }
    a.cmp_reg_imm32(R::V6, ThreadState::Detaching as i32);
    a.jcc_label(C::NotEqual, bad_edge);
    a.cmp_reg_imm32(R::V2, ThreadState::Detached as i32);
    a.jcc_label(C::Equal, publish);
    a.jmp_label(bad_edge);

    a.bind_text_label(running);
    a.cmp_reg_imm32(R::V2, ThreadState::Detaching as i32 + 1);
    a.jcc_label(C::Below, publish);
    a.jmp_label(bad_edge);
    a.bind_text_label(parked);
    a.cmp_reg_imm32(R::V2, ThreadState::AtSafepoint as i32 + 1);
    a.jcc_label(C::Below, publish);
    a.jmp_label(bad_edge);
    a.bind_text_label(native);
    a.cmp_reg_imm32(R::V2, ThreadState::Running as i32);
    a.jcc_label(C::NotEqual, bad_edge);
    a.bind_text_label(publish);
    a.atomic_store_release_u32(R::V8, R::V2);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    for (label, status) in [
        (bad_abi, 1),
        (bad_state, 2),
        (active_access, 3),
        (bad_edge, 4),
        (private_work, 5),
    ] {
        a.bind_text_label(label);
        a.mov_imm64(R::V0, status);
        a.jmp_label(done);
    }
    a.bind_text_label(done);
}
