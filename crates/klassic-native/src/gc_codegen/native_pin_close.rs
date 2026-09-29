//! One shutdown action with explicit worker-boundary requests and native locks.
use super::*;
use crate::gc_abi::{
    MetadataCharge, NativePin as Pin, NativePinRuntime as Runtime, PinRuntime, RootPin,
};
fn owner(off: usize) -> E {
    field(Q, off, 0)
}
fn runtime(off: usize) -> E {
    field(G, off, 0)
}
pub(super) fn callback<A: PortableAsm>(b: &mut Emit<'_, A>, heap: bool, unlock: bool) {
    let (base, off, arg) = if heap {
        (
            REG,
            if unlock {
                offset_of!(PinRuntime, unlock)
            } else {
                offset_of!(PinRuntime, lock)
            },
            offset_of!(PinRuntime, heap_lock),
        )
    } else {
        (
            G,
            if unlock {
                offset_of!(Runtime, unlock)
            } else {
                offset_of!(Runtime, lock)
            },
            offset_of!(Runtime, registry_lock),
        )
    };
    b.value(field(base, off, 0));
    b.a.mov_reg_reg(R::V10, R::V0);
    b.value(field(base, arg, 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.call_native_unary(R::V10);
}
pub(super) fn validate_runtime<A: PortableAsm>(b: &mut Emit<'_, A>, invalid: A::TextLabel) {
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.branch(
        owner(offset_of!(Pin, self_)),
        local(Q),
        C::NotEqual,
        invalid,
    );
    b.branch(owner(offset_of!(Pin, stage)), n(1), C::Below, invalid);
    b.branch(owner(offset_of!(Pin, stage)), n(6), C::Above, invalid);
    b.branch(
        owner(offset_of!(Pin, registry)),
        runtime(offset_of!(Runtime, registry)),
        C::NotEqual,
        invalid,
    );
    for off in [
        offset_of!(Runtime, pins),
        offset_of!(Runtime, registry),
        offset_of!(Runtime, heap),
        offset_of!(Runtime, native),
    ] {
        b.pointer(runtime(off), invalid);
    }
    for off in [
        offset_of!(Runtime, registry_lock),
        offset_of!(Runtime, lock),
        offset_of!(Runtime, unlock),
    ] {
        b.branch(runtime(off), n(0), C::Equal, invalid);
    }
    b.set(local(REG), runtime(offset_of!(Runtime, pins)));
    for off in [
        offset_of!(PinRuntime, heap_lock),
        offset_of!(PinRuntime, lock),
        offset_of!(PinRuntime, unlock),
    ] {
        b.branch(field(REG, off, 0), n(0), C::Equal, invalid);
    }
}
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let cancel = a.create_text_label();
    let retire = a.create_text_label();
    let dispose = a.create_text_label();
    let dynamic = a.create_text_label();
    let more = a.create_text_label();
    let completed = a.create_text_label();
    let resume = a.create_text_label();
    let native = a.create_text_label();
    let entries: [_; 4] = std::array::from_fn(|_| a.create_text_label());
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    validate_runtime(&mut b, invalid);
    b.branch(owner(offset_of!(Pin, stage)), n(6), C::Equal, completed);
    b.pointer(owner(offset_of!(Pin, charge)), invalid);
    b.set(local(NEXT), owner(offset_of!(Pin, charge)));
    b.branch(
        field(NEXT, offset_of!(MetadataCharge, owner), 0),
        runtime(offset_of!(Runtime, heap)),
        C::NotEqual,
        invalid,
    );
    let context = b.a.create_text_label();
    b.branch(owner(offset_of!(Pin, stage)), n(3), C::NotEqual, context);
    b.branch(
        owner(offset_of!(Pin, pin) + offset_of!(RootPin, stage)),
        n(6),
        C::NotEqual,
        cancel,
    );
    b.a.bind_text_label(context);
    b.pointer(owner(offset_of!(Pin, context)), invalid);
    b.set(local(CTX), owner(offset_of!(Pin, context)));
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.set(
        local(USED),
        field(CTX, offset_of!(MutatorContext, state), 4),
    );
    let valid = b.a.create_text_label();
    b.branch(local(USED), n(0), C::Equal, valid);
    b.branch(local(USED), n(2), C::NotEqual, invalid);
    b.a.bind_text_label(valid);
    b.branch(owner(offset_of!(Pin, stage)), n(4), C::Below, retire);
    b.branch(local(USED), n(0), C::Equal, native);
    b.branch(owner(offset_of!(Pin, stage)), n(4), C::Equal, dispose);
    callback(&mut b, true, false);
    b.value(runtime(offset_of!(Runtime, heap)));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.load_rbp_slot(R::V7, Q);
    b.a.call_label(entries[3]);
    b.a.store_rbp_slot(STATE, R::V0);
    callback(&mut b, true, true);
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    b.a.bind_text_label(retire);
    b.branch(local(USED), n(2), C::Equal, resume);
    callback(&mut b, false, false);
    b.a.load_rbp_slot(R::V7, Q);
    b.a.call_label(entries[1]);
    b.a.store_rbp_slot(STATE, R::V0);
    callback(&mut b, false, true);
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    b.a.bind_text_label(cancel);
    b.value(add(local(Q), n(offset_of!(Pin, pin) as u64)));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.load_rbp_slot(R::V7, REG);
    b.a.mov_imm64(R::V2, 1);
    b.a.call_label(entries[0]);
    b.a.jmp_label(dynamic);
    b.a.bind_text_label(dispose);
    b.value(runtime(offset_of!(Runtime, native)));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.load_rbp_slot(R::V7, Q);
    b.a.call_label(entries[2]);
    b.a.bind_text_label(dynamic);
    b.a.cmp_reg_imm32(R::V0, 8);
    b.a.jcc_label(C::Equal, more);
    b.a.jmp_label(done);
    for (label, status) in [
        (invalid, 4),
        (more, 0),
        (completed, 8),
        (resume, 11),
        (native, 12),
    ] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(entries[0]);
    super::pin_dispatch::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(entries[1]);
    super::native_pin::emit_retire(b.a);
    b.a.ret();
    b.a.bind_text_label(entries[2]);
    super::native_pin::emit_dispose(b.a);
    b.a.ret();
    b.a.bind_text_label(entries[3]);
    super::native_pin::emit_release(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
