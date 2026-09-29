//! Canonical guard acquisition after counted pending follow.
use super::*;
use crate::gc_abi::{ABI_VERSION, ResolveRequest, SourceAccessRequest};
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let status: [_; 9] = std::array::from_fn(|_| a.create_text_label());
    let metadata = a.create_text_label();
    let resolve = a.create_text_label();
    let nonnull = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 32);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(REG, R::V2);
    a.mov_reg_reg(R::V0, R::V5);
    a.sub_reg_imm32(R::V0, 144);
    a.store_rbp_slot(LEASE, R::V0);
    let mut b = Emit { a };
    for slot in [G, Q, REG] {
        b.pointer(local(slot), status[1]);
    }
    const S: usize = offset_of!(ResolveRequest, scope);
    for off in [
        offset_of!(ResolveRequest, reference),
        offset_of!(ResolveRequest, destination),
        S + offset_of!(SourceAccessRequest, source_offset),
        S + offset_of!(SourceAccessRequest, incarnation),
        S + offset_of!(SourceAccessRequest, held),
        S + offset_of!(SourceAccessRequest, ticket),
    ] {
        b.branch(field(REG, off, 0), n(0), C::NotEqual, status[1]);
    }
    b.set(
        local(CTX),
        field(REG, S + offset_of!(SourceAccessRequest, context), 0),
    );
    b.pointer(local(CTX), status[1]);
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(ABI_VERSION as u64),
        C::NotEqual,
        status[1],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        status[1],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        status[1],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        status[1],
    );
    b.branch(field(Q, 0, 0), n(0), C::NotEqual, nonnull);
    for off in [8, 16, 24] {
        b.branch(field(Q, off, 0), n(0), C::NotEqual, status[1]);
    }
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(nonnull);
    for off in [0, 8, 16] {
        b.set(field(LEASE, off, 0), field(Q, off + 8, 0));
    }
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, LEASE);
    b.a.mov_imm64(R::V2, 2);
    b.a.call_label(metadata);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, status[1]);
    for off in [0, 8] {
        b.branch(
            field(LEASE, off, 0),
            field(Q, off + 8, 0),
            C::NotEqual,
            status[1],
        );
    }
    b.set(
        field(REG, offset_of!(ResolveRequest, reference), 0),
        field(Q, 0, 0),
    );
    b.value(field(LEASE, 0, 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, REG);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(resolve);
    b.a.store_rbp_slot(PREV, R::V0);
    b.branch(local(PREV), n(0), C::Equal, status[8]);
    for off in (0..std::mem::size_of::<ResolveRequest>()).step_by(8) {
        b.set(field(REG, off, 0), n(0));
    }
    b.set(
        field(REG, S + offset_of!(SourceAccessRequest, context), 0),
        local(CTX),
    );
    for (source, result) in [(3, 3), (2, 2), (4, 6)] {
        b.branch(local(PREV), n(source), C::Equal, status[result]);
    }
    b.a.jmp_label(status[1]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
    b.a.bind_text_label(resolve);
    super::resolve::emit_resolve_access(b.a);
    b.a.ret();
}
