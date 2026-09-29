//! Bounded cleanup of an exclusively owned, uninstalled relocation copy.
use super::*;
use crate::gc_abi::{
    AllocationPage as P, ForwardCell as F, ObjectHeader as H, RelocationCopy, RelocationDiscard,
};
macro_rules! page {
    ($f:ident,$a:expr) => {
        field(G, offset_of!(P, $f), $a)
    };
}
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(RelocationDiscard, $f), 0)
    };
}
macro_rules! src {
    ($f:ident,$a:expr) => {
        field(REG, offset_of!(F, $f), $a)
    };
}
macro_rules! copy {
    ($f:ident) => {
        field(SEG, offset_of!(RelocationCopy, $f), 0)
    };
}
macro_rules! header {
    ($f:ident,$a:expr) => {
        field(CTX, offset_of!(H, $f), $a)
    };
}
#[allow(dead_code)]
pub(crate) fn emit_relocate_discard<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let more = a.create_text_label();
    let finish = a.create_text_label();
    let fresh = a.create_text_label();
    let controls = a.create_text_label();
    let page_ok = a.create_text_label();
    let unprepared = a.create_text_label();
    let geometry = a.create_text_label();
    let untouched = a.create_text_label();
    let rounded = a.create_text_label();
    let disjoint = a.create_text_label();
    let partial = a.create_text_label();
    let size = a.create_text_label();
    let size_ok = a.create_text_label();
    let count = a.create_text_label();
    let cleared = a.create_text_label();
    let clear = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.pointer(req!(source), invalid);
    b.pointer(req!(copy), invalid);
    b.branch(req!(owner), n(0), C::Equal, fresh);
    b.branch(req!(owner), local(Q), C::NotEqual, invalid);
    b.a.jmp_label(controls);
    b.a.bind_text_label(fresh);
    b.branch(req!(cursor), n(0), C::NotEqual, invalid);
    b.branch(req!(bytes), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(controls);
    b.set(local(REG), req!(source));
    b.set(local(SEG), req!(copy));
    b.branch(src!(source_offset, 0), n(48), C::Below, invalid);
    b.branch(
        and(src!(source_offset, 0), n(15)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        src!(source_offset, 0),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    b.branch(copy!(ticket), n(0), C::Equal, invalid);
    b.branch(copy!(ticket), n(u64::MAX >> 2), C::Above, invalid);
    b.branch(src!(incarnation, 0), n(0), C::Equal, invalid);
    b.branch(
        copy!(source_incarnation),
        src!(incarnation, 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        src!(word, 8),
        add(shl(copy!(ticket), 2), n(1)),
        C::NotEqual,
        invalid,
    );
    b.branch(src!(access, 8), n(1u64 << 63), C::NotEqual, invalid);
    b.branch(copy!(stage), n(2), C::Above, invalid);
    b.set(local(STATE), page!(state, 8));
    b.set(local(USED), page!(allocated_top, 8));
    b.branch(local(STATE), n(3), C::Equal, page_ok);
    b.branch(local(STATE), n(5), C::NotEqual, invalid);
    b.a.bind_text_label(page_ok);
    b.branch(page!(owner, 0), n(0), C::NotEqual, invalid);
    b.branch(page!(incarnation, 0), n(0), C::Equal, invalid);
    b.branch(page!(start, 0), n(0), C::Equal, invalid);
    b.branch(page!(start, 0), page!(limit, 0), C::AboveOrEqual, invalid);
    b.branch(
        page!(limit, 0),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    for value in [
        page!(heap_base, 0),
        page!(start, 0),
        page!(limit, 0),
        local(USED),
    ] {
        b.branch(and(value, n(15)), n(0), C::NotEqual, invalid);
    }
    b.branch(local(USED), page!(start, 0), C::Below, invalid);
    b.branch(local(USED), page!(limit, 0), C::Above, invalid);
    let closed = b.a.create_text_label();
    b.branch(local(STATE), n(3), C::NotEqual, closed);
    b.branch(local(USED), page!(start, 0), C::NotEqual, invalid);
    b.a.bind_text_label(closed);
    b.branch(
        page!(heap_base, 0),
        sub(n(u64::MAX), page!(limit, 0)),
        C::Above,
        invalid,
    );
    b.branch(copy!(heap_base), page!(heap_base, 0), C::NotEqual, invalid);
    b.branch(copy!(destination), n(0), C::Equal, unprepared);
    b.branch(
        copy!(destination),
        add(local(USED), n(32)),
        C::NotEqual,
        invalid,
    );
    b.branch(
        copy!(destination_limit),
        page!(limit, 0),
        C::NotEqual,
        invalid,
    );
    b.branch(sub(page!(limit, 0), local(USED)), n(48), C::Below, invalid);
    b.a.jmp_label(geometry);
    b.a.bind_text_label(unprepared);
    b.branch(copy!(destination_limit), n(0), C::NotEqual, invalid);
    b.branch(copy!(stage), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(geometry);
    b.set(local(LEASE), n(0));
    b.branch(copy!(stage), n(0), C::Equal, untouched);
    b.branch(src!(source_offset, 0), n(48), C::Below, invalid);
    b.branch(
        and(src!(source_offset, 0), n(15)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        copy!(source_limit),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    b.branch(
        src!(source_offset, 0),
        copy!(source_limit),
        C::AboveOrEqual,
        invalid,
    );
    b.branch(
        copy!(heap_base),
        sub(n(u64::MAX), copy!(source_limit)),
        C::Above,
        invalid,
    );
    b.set(
        local(CTX),
        sub(add(copy!(heap_base), src!(source_offset, 0)), n(32)),
    );
    b.branch(header!(state, 8), n(2), C::NotEqual, invalid);
    b.branch(
        header!(payload_bytes, 0),
        sub(copy!(source_limit), src!(source_offset, 0)),
        C::Above,
        invalid,
    );
    b.branch(header!(descriptor_id, 0), n(0), C::Equal, invalid);
    b.branch(
        header!(descriptor_id, 0),
        n(u32::MAX as u64),
        C::Above,
        invalid,
    );
    b.pointer(header!(mark_record, 0), invalid);
    b.set(
        local(LEASE),
        and(add(header!(payload_bytes, 0), n(15)), n(!15u64)),
    );
    b.branch(local(LEASE), n(0), C::NotEqual, rounded);
    b.set(local(LEASE), n(16));
    b.a.bind_text_label(rounded);
    b.branch(
        local(LEASE),
        sub(copy!(source_limit), src!(source_offset, 0)),
        C::Above,
        invalid,
    );
    b.branch(
        local(LEASE),
        sub(page!(limit, 0), copy!(destination)),
        C::Above,
        invalid,
    );
    b.branch(
        local(USED),
        add(src!(source_offset, 0), local(LEASE)),
        C::AboveOrEqual,
        disjoint,
    );
    b.branch(
        add(copy!(destination), local(LEASE)),
        sub(src!(source_offset, 0), n(32)),
        C::Above,
        invalid,
    );
    b.a.bind_text_label(disjoint);
    b.branch(copy!(stage), n(1), C::Equal, partial);
    b.branch(
        copy!(cursor),
        header!(payload_bytes, 0),
        C::NotEqual,
        invalid,
    );
    b.a.jmp_label(size);
    b.a.bind_text_label(partial);
    b.branch(copy!(cursor), n(0), C::Equal, invalid);
    b.branch(
        copy!(cursor),
        header!(payload_bytes, 0),
        C::AboveOrEqual,
        invalid,
    );
    b.a.bind_text_label(size);
    b.set(local(LEASE), add(local(LEASE), n(32)));
    b.a.jmp_label(size_ok);
    b.a.bind_text_label(untouched);
    b.branch(copy!(cursor), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(size_ok);
    let original = b.a.create_text_label();
    b.branch(req!(owner), n(0), C::Equal, original);
    b.branch(req!(bytes), local(LEASE), C::NotEqual, invalid);
    b.a.bind_text_label(original);
    b.branch(req!(cursor), local(LEASE), C::Above, invalid);
    b.branch(and(req!(cursor), n(15)), n(0), C::NotEqual, invalid);
    b.set(local(PREV), sub(local(LEASE), req!(cursor)));
    b.branch(local(PREV), n(256), C::Below, count);
    b.set(local(PREV), n(256));
    b.a.bind_text_label(count);
    b.set(local(NEXT), add(req!(cursor), local(PREV)));
    b.branch(local(PREV), n(0), C::Equal, cleared);
    b.value(add(add(page!(heap_base, 0), local(USED)), req!(cursor)));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V1, PREV);
    b.a.mov_imm64(R::V2, 0);
    b.a.bind_text_label(clear);
    b.a.store_ptr_disp32(R::V7, 0, R::V2);
    b.a.add_reg_imm32(R::V7, 8);
    b.a.sub_reg_imm8(R::V1, 8);
    b.a.test_reg_reg(R::V1, R::V1);
    b.a.jcc_label(C::NotEqual, clear);
    b.a.bind_text_label(cleared);
    b.set(req!(owner), local(Q));
    b.set(req!(bytes), local(LEASE));
    b.set(req!(cursor), local(NEXT));
    b.branch(local(NEXT), local(LEASE), C::NotEqual, more);
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(finish);
    b.a.bind_text_label(more);
    b.a.mov_imm64(R::V0, 2);
    b.a.jmp_label(finish);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(finish);
    b.a.jmp_label(done);
    b.a.bind_text_label(done);
    b.a.leave();
}
