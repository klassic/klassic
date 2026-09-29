//! Committed private root package -> registered owner scope, under registry lock.
use super::*;
use crate::gc_abi::{
    BufferCursor, MetadataCharge as M, RootCell, RootChange, RootLifeRequest, RootStorage as S,
};
const REQUEST: i32 = 192;
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let check = a.create_text_label();
    let attach = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 80);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(CTX, R::V6);
    a.store_rbp_slot(Q, R::V2);
    let mut b = Emit { a };
    for slot in [G, CTX, Q] {
        b.pointer(local(slot), invalid);
    }
    b.branch(
        field(Q, offset_of!(M, stage), 0),
        n(2),
        C::NotEqual,
        invalid,
    );
    b.pointer(field(Q, offset_of!(M, owner), 0), invalid);
    b.set(local(SEG), field(Q, offset_of!(M, allocation), 0));
    b.pointer(local(SEG), invalid);
    b.set(local(USED), field(Q, offset_of!(M, bytes), 0));
    b.branch(local(USED), n(size_of::<S>() as u64), C::Below, invalid);
    b.branch(add(local(SEG), local(USED)), local(SEG), C::Below, invalid);
    b.branch(
        field(
            SEG,
            offset_of!(S, registration) + offset_of!(RootRegistration, segment),
            0,
        ),
        add(local(SEG), n(offset_of!(S, segment) as u64)),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(
            SEG,
            offset_of!(S, segment) + offset_of!(RootSegment, cells),
            0,
        ),
        add(local(SEG), n(size_of::<S>() as u64)),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(
            SEG,
            offset_of!(S, segment) + offset_of!(RootSegment, journal),
            0,
        ),
        add(local(SEG), n(offset_of!(S, journal) as u64)),
        C::NotEqual,
        invalid,
    );
    b.set(
        local(NEXT),
        field(
            SEG,
            offset_of!(S, segment) + offset_of!(RootSegment, capacity),
            0,
        ),
    );
    b.set(
        local(PREV),
        field(
            SEG,
            offset_of!(S, journal) + offset_of!(BufferCursor, capacity),
            0,
        ),
    );
    for slot in [NEXT, PREV] {
        b.branch(local(slot), n(0), C::Equal, invalid);
        b.branch(local(slot), n(u32::MAX as u64), C::Above, invalid);
    }
    const {
        assert!(size_of::<RootCell>() == 32 && size_of::<RootChange>() == 32);
    }
    b.branch(
        add(
            n(size_of::<S>() as u64),
            shl(add(local(NEXT), local(PREV)), 5),
        ),
        local(USED),
        C::Above,
        invalid,
    );
    b.branch(
        field(
            SEG,
            offset_of!(S, journal) + offset_of!(BufferCursor, entries),
            0,
        ),
        add(
            add(local(SEG), n(size_of::<S>() as u64)),
            shl(local(NEXT), 5),
        ),
        C::NotEqual,
        invalid,
    );
    for (offset, width) in [
        (offset_of!(BufferCursor, top), 0),
        (offset_of!(BufferCursor, published), 8),
        (offset_of!(BufferCursor, consumed), 8),
    ] {
        b.branch(
            field(SEG, offset_of!(S, journal) + offset, width),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.a.load_rbp_slot(R::V7, SEG);
    b.a.call_label(check);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, invalid);
    for i in 0..size_of::<RootLifeRequest>() / 8 {
        b.set(local(REQUEST - i as i32 * 8), n(0));
    }
    b.set(
        local(REQUEST - offset_of!(RootLifeRequest, registration) as i32),
        add(local(SEG), n(offset_of!(S, registration) as u64)),
    );
    b.set(
        local(REQUEST - offset_of!(RootLifeRequest, context) as i32),
        local(CTX),
    );
    b.a.load_rbp_slot(R::V7, G);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, REQUEST);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(attach);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(check);
    crate::gc_codegen::root_storage::emit_can_dispose(b.a);
    b.a.ret();
    b.a.bind_text_label(attach);
    super::emit_life_locked(b.a);
    b.a.ret();
}
