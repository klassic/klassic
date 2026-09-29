//! Prepared-root guarded store commit; gc_rooted_store_commit.h.
use super::*;
use crate::gc_abi::{GuardedStore as Store, RootCell, RootHandle, RootRequest, RootedMarkSet as T};
const COMMIT: i32 = 272;

#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let bad = a.create_text_label();
    let reprepare = a.create_text_label();
    let store = a.create_text_label();
    let null = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 48);
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
    b.pointer(local(Q), bad);
    b.pointer(local(OP), bad);
    b.branch(q!(state), n(2), C::NotEqual, bad);
    b.branch(root!(value), n(0), C::NotEqual, bad);
    b.branch(
        field(OP, offset_of!(Store, desired), 0),
        n(0),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(OP, offset_of!(Store, transferred), 0),
        n(0),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(OP, offset_of!(Store, phase), 0),
        q!(phase),
        C::NotEqual,
        bad,
    );
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
    super::barrier::guarded_object(
        &mut b,
        field(OP, offset_of!(Store, scope), 0),
        field(OP, offset_of!(Store, page), 0),
        q!(phase),
        bad,
    );
    b.branch(
        field(LEASE, offset_of!(MutatorContext, private_producers), 8),
        n(0),
        C::Equal,
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
    super::barrier::reference(&mut b, local(PREV), bad);
    b.set(local(STATE), n(0));
    for (shift, mask, bias) in [(8, 1, 8), (9, 1, 10), (10, 3, 12)] {
        b.value(add(and(shr(q!(phase), shift), n(mask)), n(bias)));
        b.a.mov_reg_reg(R::V1, R::V0);
        b.a.mov_imm64(R::V2, 1);
        b.a.shl_reg_count(R::V2);
        b.a.store_rbp_slot(KIND, R::V2);
        b.set(local(STATE), add(local(STATE), local(KIND)));
    }
    b.branch(local(PREV), n(0), C::Equal, null);
    b.branch(
        and(local(PREV), n(0xffcf)),
        local(STATE),
        C::NotEqual,
        reprepare,
    );
    // Add current remembered parity only for the actual store attempt.
    b.value(add(and(shr(q!(phase), 12), n(1)), n(4)));
    b.a.mov_reg_reg(R::V1, R::V0);
    b.a.mov_imm64(R::V2, 1);
    b.a.shl_reg_count(R::V2);
    b.a.store_rbp_slot(KIND, R::V2);
    b.set(
        local(PREV),
        add(
            add(and(local(PREV), n(!0xffffu64)), local(STATE)),
            local(KIND),
        ),
    );
    b.a.bind_text_label(null);
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, COMMIT);
    b.a.store_rbp_slot(CTX, R::V0);
    for off in (0..size_of::<Store>()).step_by(8) {
        b.set(field(CTX, off, 0), field(OP, off, 0));
    }
    b.set(field(CTX, offset_of!(Store, desired), 0), local(PREV));
    b.a.mov_reg_reg(R::V7, R::V5);
    b.a.sub_reg_imm32(R::V7, COMMIT);
    b.a.call_label(store);
    b.a.store_rbp_slot(STATE, R::V0);
    b.set(
        field(OP, offset_of!(Store, transferred), 0),
        field(CTX, offset_of!(Store, transferred), 0),
    );
    b.value(local(STATE));
    b.a.jmp_label(done);
    b.a.bind_text_label(reprepare);
    b.a.mov_imm64(R::V0, 6);
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(store);
    super::barrier::emit_guarded_store(b.a);
    b.a.ret();
}
