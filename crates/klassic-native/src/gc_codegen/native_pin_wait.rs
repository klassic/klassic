//! Registry-locked root completion recheck and single-owner sticky event arm.
use super::*;
use crate::gc_abi::{MetadataCharge, NativePin as Pin, NativePinRuntime as Runtime};
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let more = a.create_text_label();
    let check = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    super::native_pin_close::validate_runtime(&mut b, invalid);
    super::native_pin::validate(&mut b, invalid);
    b.branch(
        field(Q, offset_of!(Pin, stage), 0),
        n(4),
        C::NotEqual,
        invalid,
    );
    b.branch(local(OP), n(0), C::Equal, invalid);
    b.branch(and(local(OP), n(3)), n(0), C::NotEqual, invalid);
    b.branch(
        field(NEXT, offset_of!(MetadataCharge, owner), 0),
        field(G, offset_of!(Runtime, heap), 0),
        C::NotEqual,
        invalid,
    );
    b.set(local(CTX), field(Q, offset_of!(Pin, context), 0));
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
        field(CTX, offset_of!(MutatorContext, state), 4),
        n(2),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.value(field(Q, offset_of!(Pin, storage), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.call_label(check);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::Equal, more);
    b.a.load_rbp_slot(R::V1, OP);
    b.a.mov_imm64(R::V0, 0);
    b.a.atomic_store_release_u32(R::V1, R::V0);
    b.a.mov_imm64(R::V0, 2);
    b.a.jmp_label(done);
    b.a.bind_text_label(more);
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 4);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(check);
    crate::gc_codegen::root_storage::emit_can_dispose(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
