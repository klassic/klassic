//! One weak-slot forwarding edge with metadata ownership transfer.
use super::*;
use crate::gc_abi::{
    WeakEntry as Entry, WeakFollowRequest as Request, WeakLoadGate as Gate, WeakSlot as Slot,
    WeakTable as Table,
};
use std::mem::size_of;

#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let cleanup = a.create_text_label();
    let empty = a.create_text_label();
    let held = a.create_text_label();
    let done = a.create_text_label();
    let follow = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    for _ in 0..2 {
        a.sub_reg_imm8(R::V4, 80);
    }
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    a.mov_reg_reg(R::V0, R::V5);
    a.sub_reg_imm32(R::V0, 144);
    a.store_rbp_slot(NEXT, R::V0);
    let mut b = Emit { a };
    b.pointer(local(Q), invalid);
    b.branch(local(OP), n(0), C::NotEqual, invalid);
    for off in [16, 24, 32] {
        b.branch(field(Q, off, 0), n(0), C::NotEqual, cleanup);
    }
    validate_table(&mut b, invalid);
    validate_entry(
        &mut b,
        field(Q, offset_of!(Request, index), 0),
        field(Q, offset_of!(Request, serial), 0),
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(Slot, reference), 0),
        n(0),
        C::NotEqual,
        held,
    );
    for off in [24, 32, 40] {
        b.branch(field(CTX, off, 0), n(0), C::NotEqual, invalid);
    }
    b.a.jmp_label(empty);
    b.a.bind_text_label(held);
    for off in [24, 32, 40] {
        b.branch(field(CTX, off, 0), n(0), C::Equal, invalid);
    }
    for off in [0, 8, 16, 24] {
        b.set(
            field(NEXT, off, 0),
            field(CTX, offset_of!(Slot, reference) + off, 0),
        );
    }
    b.value(field(REG, offset_of!(Gate, pool), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, NEXT);
    b.value(add(local(Q), n(offset_of!(Request, retired) as u64)));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(follow);
    b.a.store_rbp_slot(STATE, R::V0);
    for off in [0, 8, 16, 24] {
        b.set(
            field(CTX, offset_of!(Slot, reference) + off, 0),
            field(NEXT, off, 0),
        );
    }
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    for (label, result) in [(empty, 0), (invalid, 1), (cleanup, 7)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, result);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(follow);
    super::pending_follow::emit(b.a);
    b.a.ret();
}

// Validates G and leaves its gate in REG. Other persistent locals are unchanged.
pub(super) fn validate_table<A: PortableAsm>(b: &mut Emit<'_, A>, invalid: A::TextLabel) {
    let room = b.a.create_text_label();
    let geometry = b.a.create_text_label();
    b.pointer(local(G), invalid);
    b.set(local(REG), field(G, offset_of!(Table, gate), 0));
    b.pointer(local(REG), invalid);
    b.pointer(field(G, offset_of!(Table, entries), 0), invalid);
    let capacity = field(G, offset_of!(Table, capacity), 0);
    b.branch(capacity.clone(), n(0), C::Equal, invalid);
    b.branch(capacity.clone(), n(u32::MAX as u64), C::Above, invalid);
    // WeakEntry is seven qwords; neither multiplication nor address may wrap.
    assert_eq!(size_of::<Entry>(), 56);
    let bytes = sub(shl(capacity.clone(), 6), shl(capacity.clone(), 3));
    b.branch(
        field(G, offset_of!(Table, entries), 0),
        sub(n(u64::MAX), bytes),
        C::Above,
        invalid,
    );
    let active = field(G, offset_of!(Table, active), 0);
    b.branch(active.clone(), capacity.clone(), C::Above, invalid);
    b.branch(
        active.clone(),
        field(G, offset_of!(Table, last_serial), 0),
        C::Above,
        invalid,
    );
    b.branch(
        field(G, offset_of!(Table, free_head), 0),
        capacity.clone(),
        C::Above,
        invalid,
    );
    b.branch(active, capacity.clone(), C::Below, room);
    b.branch(
        field(G, offset_of!(Table, free_head), 0),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.a.jmp_label(geometry);
    b.a.bind_text_label(room);
    b.branch(
        field(G, offset_of!(Table, free_head), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.a.bind_text_label(geometry);
    b.branch(
        field(REG, offset_of!(Gate, closed), 0),
        n(1),
        C::Above,
        invalid,
    );
    b.pointer(field(REG, offset_of!(Gate, pool), 0), invalid);
}

// validate_table has established G/REG. Leaves the serial-bound entry in CTX.
pub(super) fn validate_entry<A: PortableAsm>(
    b: &mut Emit<'_, A>,
    index: E,
    serial: E,
    invalid: A::TextLabel,
) {
    let capacity = field(G, offset_of!(Table, capacity), 0);
    b.branch(index.clone(), n(0), C::Equal, invalid);
    b.branch(index.clone(), capacity, C::Above, invalid);
    b.branch(serial.clone(), n(0), C::Equal, invalid);
    b.branch(
        serial.clone(),
        field(G, offset_of!(Table, last_serial), 0),
        C::Above,
        invalid,
    );
    let position = sub(index, n(1));
    b.set(
        local(CTX),
        add(
            field(G, offset_of!(Table, entries), 0),
            sub(shl(position.clone(), 6), shl(position, 3)),
        ),
    );
    b.branch(
        field(CTX, offset_of!(Slot, gate), 0),
        local(REG),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(Slot, serial), 0),
        serial.clone(),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(Entry, next_free), 0),
        n(0),
        C::NotEqual,
        invalid,
    );
}
