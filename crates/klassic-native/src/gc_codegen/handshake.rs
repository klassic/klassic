//! Shared finite, registry-lock protected handshake bookkeeping. See
//! runtime/gc/gc_handshake.h for the matching C implementation and prerequisites.
//! Callers own storage, OS locking/notification, TLS, roots and phase adoption.
use crate::gc_abi::{
    ABI_VERSION, BufferConsumer, BufferCursor, HandshakeState as H, MutatorContext as M,
    ThreadRegistration as T,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

const GENERATION: i32 = offset_of!(H, generation) as i32;
const MEMBERS: i32 = offset_of!(H, members) as i32;
const NATIVE: i32 = offset_of!(H, native) as i32;
const EXPECTED: i32 = offset_of!(H, expected) as i32;
const ACKNOWLEDGED: i32 = offset_of!(H, acknowledged) as i32;
const ACTIVE: i32 = offset_of!(H, active) as i32;
const LAST_ID: i32 = offset_of!(H, last_identity) as i32;
const HEAD: i32 = offset_of!(H, head) as i32;
const CONTEXT: i32 = offset_of!(T, context) as i32;
const IDENTITY: i32 = offset_of!(T, identity) as i32;
const ACK_GEN: i32 = offset_of!(T, acknowledged_generation) as i32;
const REGISTERED: i32 = offset_of!(T, registered) as i32;
const REGISTRY: i32 = offset_of!(T, registry) as i32;
const PREVIOUS: i32 = offset_of!(T, previous) as i32;
const NEXT: i32 = offset_of!(T, next) as i32;

fn store_constant<E: PortableAsm>(a: &mut E, base: R, offset: i32, value: u64) {
    a.mov_imm64(R::V0, value);
    a.store_ptr_disp32(base, offset, R::V0);
}
fn publish_state<E: PortableAsm>(a: &mut E, state: u64) {
    a.mov_reg_reg(R::V11, R::V8);
    a.add_reg_imm32(R::V11, offset_of!(M, state) as i32);
    a.mov_imm64(R::V0, state);
    a.atomic_store_release_u32(R::V11, R::V0);
}

/// `(V7=h, V6=registration_or_null, V2=operation) -> V0=status`.
/// Operations: register/ack/resume/native-enter/native-leave/detach/begin/end
/// = 0..7. Status: OK/wait/invalid/exhausted/duplicate/incomplete = 0..5.
/// Clobbers V0/V1/V8/V9/V10/V11, flags. No calls, allocation, scans or waits.
#[allow(dead_code)]
pub(crate) fn emit_step_locked<E: PortableAsm>(a: &mut E) {
    let wait = a.create_text_label();
    let invalid = a.create_text_label();
    let exhausted = a.create_text_label();
    let duplicate = a.create_text_label();
    let incomplete = a.create_text_label();
    let success = a.create_text_label();
    let done = a.create_text_label();
    let inactive = a.create_text_label();
    let validated = a.create_text_label();
    let ops: [_; 8] = std::array::from_fn(|_| a.create_text_label());
    a.cmp_reg_imm32(R::V2, 7);
    a.jcc_label(C::Above, invalid);
    a.load_ptr_disp32(R::V0, R::V7, NATIVE);
    a.load_ptr_disp32(R::V10, R::V7, MEMBERS);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::Above, invalid);
    let empty = a.create_text_label();
    let head_checked = a.create_text_label();
    a.load_ptr_disp32(R::V1, R::V7, HEAD);
    a.test_reg_reg(R::V10, R::V10);
    a.jcc_label(C::Equal, empty);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, invalid);
    a.jmp_label(head_checked);
    a.bind_text_label(empty);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(head_checked);
    a.load_ptr_disp32(R::V1, R::V7, ACTIVE);
    a.cmp_reg_imm32(R::V1, 1);
    a.jcc_label(C::Above, invalid);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, inactive);
    a.load_ptr_disp32(R::V1, R::V7, GENERATION);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, invalid);
    a.sub_reg_reg(R::V10, R::V0);
    a.load_ptr_disp32(R::V0, R::V7, EXPECTED);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V7, ACKNOWLEDGED);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Above, invalid);
    a.jmp_label(validated);
    a.bind_text_label(inactive);
    a.load_ptr_disp32(R::V0, R::V7, EXPECTED);
    a.load_ptr_disp32(R::V1, R::V7, ACKNOWLEDGED);
    a.or_reg_reg(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(validated);
    for (operation, label) in [(6, ops[6]), (7, ops[7])] {
        a.cmp_reg_imm32(R::V2, operation);
        a.jcc_label(C::Equal, label);
    }
    let need_request = a.create_text_label();
    let participant = a.create_text_label();
    a.load_ptr_disp32(R::V0, R::V7, ACTIVE);
    a.cmp_reg_imm32(R::V2, 1);
    a.jcc_label(C::Equal, need_request);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, wait);
    a.jmp_label(participant);
    a.bind_text_label(need_request);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, wait);
    a.bind_text_label(participant);
    a.test_reg_reg(R::V6, R::V6);
    a.jcc_label(C::Equal, invalid);
    let have_registration = a.create_text_label();
    a.cmp_reg_imm32(R::V2, 0);
    a.jcc_label(C::Equal, have_registration);
    a.load_ptr_disp32(R::V0, R::V6, REGISTERED);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(have_registration);
    a.load_ptr_disp32(R::V8, R::V6, CONTEXT);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, invalid);
    a.atomic_load_acquire_u32(R::V0, R::V8);
    a.cmp_reg_imm32(R::V0, ABI_VERSION as i32);
    a.jcc_label(C::NotEqual, invalid);
    a.mov_reg_reg(R::V11, R::V8);
    a.add_reg_imm32(R::V11, offset_of!(M, access_epoch) as i32);
    a.atomic_load_acquire(R::V0, R::V11);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    let no_private = a.create_text_label();
    a.mov_reg_reg(R::V11, R::V8);
    a.add_reg_imm32(R::V11, offset_of!(M, private_producers) as i32);
    a.atomic_load_acquire(R::V0, R::V11);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, no_private);
    a.cmp_reg_imm32(R::V2, 0);
    a.jcc_label(C::Equal, invalid);
    a.jmp_label(wait);
    a.bind_text_label(no_private);
    a.mov_reg_reg(R::V11, R::V8);
    a.add_reg_imm32(R::V11, offset_of!(M, state) as i32);
    a.atomic_load_acquire_u32(R::V9, R::V11);
    a.cmp_reg_imm32(R::V2, 0);
    a.jcc_label(C::Equal, ops[0]);
    a.load_ptr_disp32(R::V0, R::V6, REGISTRY);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V6, IDENTITY);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V1, R::V8, offset_of!(M, thread_id) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V10, R::V7, GENERATION);
    a.load_ptr_disp32(R::V0, R::V6, ACK_GEN);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::Above, invalid);
    for (operation, label) in ops.iter().enumerate().take(5).skip(1) {
        a.cmp_reg_imm32(R::V2, operation as i32);
        a.jcc_label(C::Equal, *label);
    }
    // Detach: caller has already entered Detaching and retired owned resources.
    a.bind_text_label(ops[5]);
    a.cmp_reg_imm32(R::V9, 3);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V7, MEMBERS);
    a.load_ptr_disp32(R::V1, R::V7, NATIVE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, invalid);
    for offset in [
        offset_of!(M, root_segments),
        offset_of!(M, retiring_roots),
        offset_of!(M, tlab_start),
        offset_of!(M, tlab_top),
        offset_of!(M, tlab_end),
        offset_of!(M, satb_consumer) + offset_of!(BufferConsumer, serial),
        offset_of!(M, remembered_consumer) + offset_of!(BufferConsumer, serial),
    ] {
        a.load_ptr_disp32(R::V1, R::V8, offset as i32);
        a.test_reg_reg(R::V1, R::V1);
        a.jcc_label(C::NotEqual, wait);
    }
    for base in [offset_of!(M, satb), offset_of!(M, remembered)] {
        a.mov_reg_reg(R::V11, R::V8);
        a.add_reg_imm32(R::V11, (base + offset_of!(BufferCursor, published)) as i32);
        a.atomic_load_acquire(R::V9, R::V11);
        a.mov_reg_reg(R::V11, R::V8);
        a.add_reg_imm32(R::V11, (base + offset_of!(BufferCursor, consumed)) as i32);
        a.atomic_load_acquire(R::V10, R::V11);
        a.load_ptr_disp32(R::V1, R::V8, (base + offset_of!(BufferCursor, top)) as i32);
        a.cmp_reg_reg(R::V9, R::V1);
        a.jcc_label(C::Above, invalid);
        a.cmp_reg_reg(R::V10, R::V9);
        a.jcc_label(C::Above, invalid);
        a.cmp_reg_reg(R::V9, R::V1);
        a.jcc_label(C::NotEqual, wait);
        a.cmp_reg_reg(R::V10, R::V9);
        a.jcc_label(C::NotEqual, wait);
    }
    a.load_ptr_disp32(R::V9, R::V6, PREVIOUS);
    a.load_ptr_disp32(R::V10, R::V6, NEXT);
    let single = a.create_text_label();
    let count_ok = a.create_text_label();
    a.load_ptr_disp32(R::V0, R::V7, MEMBERS);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, single);
    a.mov_reg_reg(R::V1, R::V9);
    a.or_reg_reg(R::V1, R::V10);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, invalid);
    a.jmp_label(count_ok);
    a.bind_text_label(single);
    a.mov_reg_reg(R::V1, R::V9);
    a.or_reg_reg(R::V1, R::V10);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(count_ok);
    let is_head = a.create_text_label();
    let prev_ok = a.create_text_label();
    let links_ok = a.create_text_label();
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, is_head);
    a.mov_reg_reg(R::V0, R::V9);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V9, REGISTRY);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V9, REGISTERED);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V9, NEXT);
    a.cmp_reg_reg(R::V0, R::V6);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V9, IDENTITY);
    a.load_ptr_disp32(R::V1, R::V6, IDENTITY);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid);
    a.jcc_label(C::Equal, invalid);
    a.jmp_label(prev_ok);
    a.bind_text_label(is_head);
    a.load_ptr_disp32(R::V0, R::V7, HEAD);
    a.cmp_reg_reg(R::V0, R::V6);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(prev_ok);
    a.test_reg_reg(R::V10, R::V10);
    a.jcc_label(C::Equal, links_ok);
    a.mov_reg_reg(R::V0, R::V10);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V10, REGISTRY);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V10, REGISTERED);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V10, PREVIOUS);
    a.cmp_reg_reg(R::V0, R::V6);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V10, IDENTITY);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V1, R::V6, IDENTITY);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.bind_text_label(links_ok);
    let unlink_head = a.create_text_label();
    let unlink_next = a.create_text_label();
    let unlinked = a.create_text_label();
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, unlink_head);
    a.store_ptr_disp32(R::V9, NEXT, R::V10);
    a.jmp_label(unlink_next);
    a.bind_text_label(unlink_head);
    a.store_ptr_disp32(R::V7, HEAD, R::V10);
    a.bind_text_label(unlink_next);
    a.test_reg_reg(R::V10, R::V10);
    a.jcc_label(C::Equal, unlinked);
    a.store_ptr_disp32(R::V10, PREVIOUS, R::V9);
    a.bind_text_label(unlinked);
    a.load_ptr_disp32(R::V0, R::V7, MEMBERS);
    a.sub_reg_imm8(R::V0, 1);
    a.store_ptr_disp32(R::V7, MEMBERS, R::V0);
    publish_state(a, 4);
    for offset in [
        CONTEXT, IDENTITY, ACK_GEN, REGISTERED, REGISTRY, PREVIOUS, NEXT,
    ] {
        store_constant(a, R::V6, offset, 0);
    }
    a.jmp_label(success);

    a.bind_text_label(ops[0]); // register fresh unpublished context
    for base in [
        offset_of!(M, satb_consumer),
        offset_of!(M, remembered_consumer),
    ] {
        for off in [
            offset_of!(BufferConsumer, serial),
            offset_of!(BufferConsumer, end),
        ] {
            a.load_ptr_disp32(R::V0, R::V8, (base + off) as i32);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, invalid);
        }
    }
    a.load_ptr_disp32(R::V0, R::V6, REGISTERED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    for off in [REGISTRY, PREVIOUS, NEXT] {
        a.load_ptr_disp32(R::V0, R::V6, off);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(M, thread_id) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V7, MEMBERS);
    a.cmp_reg_imm32(R::V1, -1);
    a.jcc_label(C::Equal, exhausted);
    a.load_ptr_disp32(R::V0, R::V7, LAST_ID);
    a.cmp_reg_imm32(R::V0, -1);
    a.jcc_label(C::Equal, exhausted);
    a.load_ptr_disp32(R::V10, R::V7, HEAD);
    let register_head_ok = a.create_text_label();
    a.test_reg_reg(R::V10, R::V10);
    a.jcc_label(C::Equal, register_head_ok);
    a.mov_reg_reg(R::V0, R::V10);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.cmp_reg_reg(R::V10, R::V6);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V0, R::V10, REGISTRY);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V10, REGISTERED);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V10, PREVIOUS);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V10, IDENTITY);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V1, R::V7, LAST_ID);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, invalid);
    a.bind_text_label(register_head_ok);
    a.load_ptr_disp32(R::V0, R::V7, LAST_ID);
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V7, LAST_ID, R::V0);
    a.store_ptr_disp32(R::V8, offset_of!(M, thread_id) as i32, R::V0);
    a.store_ptr_disp32(R::V6, IDENTITY, R::V0);
    a.load_ptr_disp32(R::V0, R::V7, GENERATION);
    a.store_ptr_disp32(R::V6, ACK_GEN, R::V0);
    store_constant(a, R::V6, REGISTERED, 1);
    a.store_ptr_disp32(R::V6, REGISTRY, R::V7);
    store_constant(a, R::V6, PREVIOUS, 0);
    a.store_ptr_disp32(R::V6, NEXT, R::V10);
    let linked = a.create_text_label();
    a.test_reg_reg(R::V10, R::V10);
    a.jcc_label(C::Equal, linked);
    a.store_ptr_disp32(R::V10, PREVIOUS, R::V6);
    a.bind_text_label(linked);
    a.store_ptr_disp32(R::V7, HEAD, R::V6);
    a.load_ptr_disp32(R::V1, R::V7, MEMBERS);
    a.add_reg_imm32(R::V1, 1);
    a.store_ptr_disp32(R::V7, MEMBERS, R::V1);
    a.jmp_label(success);

    a.bind_text_label(ops[1]); // acknowledge at most once per registration/round
    let ack_state_ok = a.create_text_label();
    a.cmp_reg_imm32(R::V9, 3);
    a.jcc_label(C::Equal, ack_state_ok);
    a.cmp_reg_imm32(R::V9, 2);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.bind_text_label(ack_state_ok);
    let first_ack = a.create_text_label();
    a.load_ptr_disp32(R::V0, R::V6, ACK_GEN);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, first_ack);
    a.cmp_reg_imm32(R::V9, 1);
    a.jcc_label(C::Equal, duplicate);
    a.cmp_reg_imm32(R::V9, 3);
    a.jcc_label(C::Equal, duplicate);
    a.jmp_label(invalid);
    a.bind_text_label(first_ack);
    a.load_ptr_disp32(R::V1, R::V7, ACKNOWLEDGED);
    a.load_ptr_disp32(R::V0, R::V7, EXPECTED);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::AboveOrEqual, invalid);
    let keep_detaching = a.create_text_label();
    a.cmp_reg_imm32(R::V9, 3);
    a.jcc_label(C::Equal, keep_detaching);
    publish_state(a, 1);
    a.bind_text_label(keep_detaching);
    a.store_ptr_disp32(R::V6, ACK_GEN, R::V10);
    a.add_reg_imm32(R::V1, 1);
    a.store_ptr_disp32(R::V7, ACKNOWLEDGED, R::V1);
    a.jmp_label(success);

    a.bind_text_label(ops[2]); // resume / empty poll
    let resume_state_ok = a.create_text_label();
    a.cmp_reg_imm32(R::V9, 3);
    a.jcc_label(C::Equal, resume_state_ok);
    a.cmp_reg_imm32(R::V9, 2);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.bind_text_label(resume_state_ok);
    a.load_ptr_disp32(R::V0, R::V6, ACK_GEN);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, invalid);
    a.cmp_reg_imm32(R::V9, 3);
    a.jcc_label(C::Equal, success);
    publish_state(a, 0);
    a.jmp_label(success);

    a.bind_text_label(ops[3]); // enter native
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V6, ACK_GEN);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V7, NATIVE);
    a.load_ptr_disp32(R::V0, R::V7, MEMBERS);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Equal, invalid);
    publish_state(a, 2);
    a.add_reg_imm32(R::V1, 1);
    a.store_ptr_disp32(R::V7, NATIVE, R::V1);
    a.jmp_label(success);

    a.bind_text_label(ops[4]); // leave native
    a.cmp_reg_imm32(R::V9, 2);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V7, NATIVE);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, invalid);
    publish_state(a, 0);
    a.store_ptr_disp32(R::V6, ACK_GEN, R::V10);
    a.sub_reg_imm8(R::V1, 1);
    a.store_ptr_disp32(R::V7, NATIVE, R::V1);
    a.jmp_label(success);

    a.bind_text_label(ops[6]); // begin
    a.load_ptr_disp32(R::V0, R::V7, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, wait);
    a.load_ptr_disp32(R::V0, R::V7, GENERATION);
    a.cmp_reg_imm32(R::V0, -1);
    a.jcc_label(C::Equal, exhausted);
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V7, GENERATION, R::V0);
    a.load_ptr_disp32(R::V0, R::V7, MEMBERS);
    a.load_ptr_disp32(R::V1, R::V7, NATIVE);
    a.sub_reg_reg(R::V0, R::V1);
    a.store_ptr_disp32(R::V7, EXPECTED, R::V0);
    store_constant(a, R::V7, ACTIVE, 1);
    a.jmp_label(success);
    a.bind_text_label(ops[7]); // end only after every counted owner acknowledged
    a.load_ptr_disp32(R::V0, R::V7, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V0, R::V7, ACKNOWLEDGED);
    a.load_ptr_disp32(R::V1, R::V7, EXPECTED);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, incomplete);
    for offset in [ACTIVE, EXPECTED, ACKNOWLEDGED] {
        store_constant(a, R::V7, offset, 0);
    }
    a.bind_text_label(success);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    for (label, status) in [
        (wait, 1),
        (invalid, 2),
        (exhausted, 3),
        (duplicate, 4),
        (incomplete, 5),
    ] {
        a.bind_text_label(label);
        a.mov_imm64(R::V0, status);
        a.jmp_label(done);
    }
    a.bind_text_label(done);
}
