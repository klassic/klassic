//! Exact root color repair after required mark admission; gc_rooted_mark_publish.h.
use super::*;
use crate::gc_abi::{
    MetadataLeaseRequest as L, ResolveRequest, RootAccess, RootCell, RootFollow, RootHandle,
    RootRequest, RootedMarkSet as T, SourceAccessRequest as S,
};
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let bad = a.create_text_label();
    let changed = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(OP, R::V6);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(T, $f), 0)
        };
    }
    macro_rules! root {
        ($f:ident) => {
            field(Q, offset_of!(T, root) + offset_of!(RootRequest, $f), 0)
        };
    }
    let root_base = offset_of!(T, root);
    let request_base = offset_of!(RootFollow, root);
    let scope_base = offset_of!(RootAccess, access) + offset_of!(ResolveRequest, scope);
    b.pointer(local(Q), bad);
    b.pointer(local(OP), bad);
    b.branch(q!(state), n(2), C::NotEqual, bad);
    b.branch(
        q!(context),
        field(OP, offset_of!(RootFollow, context), 0),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(Q, root_base + offset_of!(RootRequest, value), 0),
        n(0),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(OP, request_base + offset_of!(RootRequest, value), 0),
        n(0),
        C::NotEqual,
        bad,
    );
    for off in (0..offset_of!(RootRequest, value)).step_by(8) {
        b.branch(
            field(Q, root_base + off, 0),
            field(OP, request_base + off, 0),
            C::NotEqual,
            bad,
        );
    }
    b.set(local(STATE), n(0));
    for g in 0..2 {
        let skip = b.a.create_text_label();
        b.set(local(USED), and(shr(q!(phase), g * 4), n(15)));
        b.branch(local(USED), n(1), C::Below, skip);
        b.branch(local(USED), n(3), C::Above, skip);
        b.set(local(STATE), add(local(STATE), n(1 << g)));
        b.a.bind_text_label(skip);
    }
    b.branch(q!(required), local(STATE), C::NotEqual, bad);
    b.branch(q!(completed), local(STATE), C::NotEqual, bad);
    let cleanup = offset_of!(RootFollow, cleanup);
    b.branch(
        field(OP, cleanup + offset_of!(L, incarnation), 0),
        field(OP, scope_base + offset_of!(S, incarnation), 0),
        C::NotEqual,
        bad,
    );
    let token = field(OP, cleanup + offset_of!(L, token), 0);
    b.branch(and(token.clone(), n(u32::MAX as u64)), n(0), C::Equal, bad);
    b.branch(shr(token, 32), n(0), C::Equal, bad);
    super::barrier::guarded_object(
        &mut b,
        add(local(OP), n(scope_base as u64)),
        field(OP, cleanup, 0),
        q!(phase),
        bad,
    );
    b.branch(local(LEASE), q!(context), C::NotEqual, bad);
    b.pointer(root!(segment), bad);
    b.set(local(SEG), root!(segment));
    macro_rules! seg {
        ($f:ident) => {
            field(SEG, offset_of!(RootSegment, $f), 0)
        };
    }
    let handle = root_base + offset_of!(RootRequest, handle);
    b.branch(
        seg!(owner),
        field(LEASE, offset_of!(MutatorContext, thread_id), 0),
        C::NotEqual,
        bad,
    );
    b.branch(seg!(scope), n(0), C::Equal, bad);
    b.pointer(seg!(cells), bad);
    b.branch(seg!(capacity), n(0), C::Equal, bad);
    b.branch(seg!(capacity), n(u32::MAX as u64), C::Above, bad);
    b.branch(
        seg!(cells),
        sub(n(u64::MAX), shl(seg!(capacity), 5)),
        C::Above,
        bad,
    );
    b.branch(
        field(SEG, offset_of!(RootSegment, state), 8),
        n(1),
        C::NotEqual,
        bad,
    );
    b.branch(
        seg!(scope),
        field(Q, handle + offset_of!(RootHandle, scope), 0),
        C::NotEqual,
        bad,
    );
    b.set(local(USED), field(SEG, offset_of!(RootSegment, used), 8));
    b.branch(local(USED), seg!(capacity), C::Above, bad);
    b.set(
        local(REG),
        field(Q, handle + offset_of!(RootHandle, cell), 0),
    );
    b.branch(local(REG), seg!(cells), C::Below, bad);
    b.set(local(STATE), sub(local(REG), seg!(cells)));
    b.branch(local(STATE), shl(local(USED), 5), C::AboveOrEqual, bad);
    b.branch(and(local(STATE), n(31)), n(0), C::NotEqual, bad);
    let reuse = field(Q, handle + offset_of!(RootHandle, reuse_generation), 0);
    b.branch(reuse.clone(), n(0), C::Equal, bad);
    b.branch(
        field(REG, offset_of!(RootCell, active), 0),
        n(0),
        C::Equal,
        bad,
    );
    b.branch(
        field(REG, offset_of!(RootCell, reuse_generation), 0),
        reuse,
        C::NotEqual,
        bad,
    );
    b.set(local(PREV), field(REG, offset_of!(RootCell, value), 8));
    b.branch(local(PREV), n(0), C::Equal, bad);
    super::barrier::reference(&mut b, local(PREV), bad);
    b.branch(
        shr(local(PREV), 16),
        field(OP, scope_base + offset_of!(S, source_offset), 0),
        C::NotEqual,
        bad,
    );
    b.set(local(STATE), and(local(PREV), n(!0xffffu64 | 0x30)));
    for (shift, mask, bias) in [(8, 1, 8), (9, 1, 10), (10, 3, 12)] {
        b.value(add(and(shr(q!(phase), shift), n(mask)), n(bias)));
        b.a.mov_reg_reg(R::V1, R::V0);
        b.a.mov_imm64(R::V2, 1);
        b.a.shl_reg_count(R::V2);
        b.a.store_rbp_slot(KIND, R::V2);
        b.set(local(STATE), add(local(STATE), local(KIND)));
    }
    b.value(local(STATE));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.value(local(PREV));
    b.a.load_rbp_slot(R::V11, REG);
    b.a.atomic_compare_exchange(R::V11, R::V2);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), local(PREV), C::NotEqual, changed);
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(changed);
    b.a.mov_imm64(R::V0, 2);
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
}
