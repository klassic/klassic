//! Guarded header-to-pending-mark-job ownership transfer; gc_mark_prepare.h.
use super::*;
use crate::gc_abi::{
    GuardedMarkPrepare as T, MarkRecord, MarkRequest, MetadataLeaseRequest, ObjectHeader,
    SourceAccessRequest,
};
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(T, $f), 0)
    };
}
#[allow(dead_code)]
pub(crate) fn emit_mark_prepare<A: PortableAsm>(a: &mut A) {
    const {
        assert!(std::mem::size_of::<T>() == 112);
        assert!(std::mem::size_of::<MarkRequest>() == 56);
        assert!(std::mem::size_of::<MetadataLeaseRequest>() == 24);
    }
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    validate(&mut b, invalid);
    transfer(&mut b);
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
}

// Q points to GuardedMarkPrepare; NEXT holds its validated stable MarkRecord.
// Validation uses G/REG/SEG/LEASE/STATE/USED/NEXT/KIND; OP remains available.
pub(super) fn validate<A: PortableAsm>(b: &mut Emit<'_, A>, invalid: A::TextLabel) {
    validate_state(b, invalid, false);
}
pub(super) fn validate_state<A: PortableAsm>(
    b: &mut Emit<'_, A>,
    invalid: A::TextLabel,
    allow_initializing: bool,
) {
    for off in (0..56).step_by(8) {
        b.branch(
            field(Q, offset_of!(T, output) + off, 0),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.branch(req!(reference), n(0), C::Equal, invalid);
    super::barrier::reference(b, req!(reference), invalid);
    b.branch(
        field(Q, offset_of!(T, lease), 0),
        req!(page),
        C::NotEqual,
        invalid,
    );
    let token = field(
        Q,
        offset_of!(T, lease) + offset_of!(MetadataLeaseRequest, token),
        0,
    );
    b.branch(
        and(token.clone(), n(u32::MAX as u64)),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(shr(token, 32), n(0), C::Equal, invalid);
    super::barrier::guarded_object_state(
        b,
        req!(scope),
        req!(page),
        req!(phase),
        invalid,
        allow_initializing,
    );
    b.branch(
        shr(req!(reference), 16),
        field(G, offset_of!(SourceAccessRequest, source_offset), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(Q, offset_of!(T, lease) + 8, 0),
        field(G, offset_of!(SourceAccessRequest, incarnation), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(REG, offset_of!(ObjectHeader, descriptor_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(REG, offset_of!(ObjectHeader, descriptor_id), 0),
        n(u32::MAX as u64),
        C::Above,
        invalid,
    );
    b.set(
        local(NEXT),
        field(REG, offset_of!(ObjectHeader, mark_record), 0),
    );
    b.pointer(local(NEXT), invalid);
    b.branch(
        field(NEXT, offset_of!(MarkRecord, object_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
}

pub(super) fn transfer<A: PortableAsm>(b: &mut Emit<'_, A>) {
    b.set(
        field(
            Q,
            offset_of!(T, output) + offset_of!(MarkRequest, record),
            0,
        ),
        local(NEXT),
    );
    b.set(
        field(
            Q,
            offset_of!(T, output) + offset_of!(MarkRequest, reference),
            0,
        ),
        req!(reference),
    );
    b.set(
        field(
            Q,
            offset_of!(T, output) + offset_of!(MarkRequest, incarnation),
            0,
        ),
        field(Q, offset_of!(T, lease) + 8, 0),
    );
    b.set(
        field(
            Q,
            offset_of!(T, output) + offset_of!(MarkRequest, metadata_lease),
            0,
        ),
        field(Q, offset_of!(T, lease) + 16, 0),
    );
    for off in [0, 8, 16] {
        b.set(field(Q, offset_of!(T, lease) + off, 0), n(0));
    }
}
