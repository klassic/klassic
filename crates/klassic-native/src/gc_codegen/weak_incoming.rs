//! Bounded weak-table source-range audit; gc_weak_incoming_step_locked.
use super::*;
use crate::gc_abi::{
    AllocationPage as Page, MAX_OFFSET, WeakEntry as Entry, WeakIncoming as Scan,
    WeakLoadGate as Gate, WeakSlot as Slot, WeakTable as Table,
};
use std::mem::size_of;

fn q(off: usize) -> E {
    field(Q, off, 0)
}
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let ret: [_; 5] = std::array::from_fn(|_| a.create_text_label());
    let begin = a.create_text_label();
    let next = a.create_text_label();
    let complete = a.create_text_label();
    let captured_done = a.create_text_label();
    let occupied = a.create_text_label();
    let live = a.create_text_label();
    let advance = a.create_text_label();
    let clear = a.create_text_label();
    let found = a.create_text_label();
    let done = a.create_text_label();
    let metadata = a.create_text_label();
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
    b.pointer(local(Q), ret[3]);
    b.branch(local(OP), n(1), C::Above, ret[3]);
    b.branch(local(OP), n(0), C::Equal, begin);
    b.branch(q(offset_of!(Scan, table)), local(G), C::NotEqual, ret[3]);
    b.branch(q(offset_of!(Scan, stage)), n(1), C::Below, ret[3]);
    b.branch(q(offset_of!(Scan, stage)), n(2), C::Above, ret[3]);
    b.a.jmp_label(next);
    b.a.bind_text_label(begin);
    for off in (0..size_of::<Scan>()).step_by(8) {
        if off != offset_of!(Scan, target_start) && off != offset_of!(Scan, target_limit) {
            b.branch(q(off), n(0), C::NotEqual, ret[3]);
        }
    }
    b.a.bind_text_label(next);
    super::weak_follow::validate_table(&mut b, ret[3]);
    b.branch(q(offset_of!(Scan, target_start)), n(0), C::Equal, ret[3]);
    b.branch(
        q(offset_of!(Scan, target_start)),
        q(offset_of!(Scan, target_limit)),
        C::AboveOrEqual,
        ret[3],
    );
    b.branch(
        q(offset_of!(Scan, target_limit)),
        n(MAX_OFFSET),
        C::Above,
        ret[3],
    );
    for off in [
        offset_of!(Scan, target_start),
        offset_of!(Scan, target_limit),
    ] {
        b.branch(and(q(off), n(15)), n(0), C::NotEqual, ret[3]);
    }
    let capture = b.a.create_text_label();
    b.branch(local(OP), n(0), C::Equal, capture);
    b.branch(
        q(offset_of!(Scan, cursor)),
        q(offset_of!(Scan, capacity)),
        C::Above,
        ret[3],
    );
    for (table_off, scan_off) in [
        (offset_of!(Table, entries), offset_of!(Scan, entries)),
        (offset_of!(Table, gate), offset_of!(Scan, gate)),
        (offset_of!(Table, capacity), offset_of!(Scan, capacity)),
        (
            offset_of!(Table, last_serial),
            offset_of!(Scan, last_serial),
        ),
    ] {
        b.branch(field(G, table_off, 0), q(scan_off), C::NotEqual, ret[4]);
    }
    b.branch(
        field(REG, offset_of!(Gate, closed), 0),
        n(0),
        C::Equal,
        ret[4],
    );
    b.branch(
        field(REG, offset_of!(Gate, pool), 0),
        q(offset_of!(Scan, pool)),
        C::NotEqual,
        ret[4],
    );
    b.branch(q(offset_of!(Scan, stage)), n(2), C::Equal, captured_done);
    b.branch(
        q(offset_of!(Scan, cursor)),
        q(offset_of!(Scan, capacity)),
        C::Equal,
        complete,
    );
    let cursor = q(offset_of!(Scan, cursor));
    b.set(
        local(CTX),
        add(
            q(offset_of!(Scan, entries)),
            sub(shl(cursor.clone(), 6), shl(cursor, 3)),
        ),
    );
    b.branch(
        field(CTX, offset_of!(Slot, serial), 0),
        n(0),
        C::NotEqual,
        occupied,
    );
    for off in (0..size_of::<Slot>()).step_by(8) {
        b.branch(field(CTX, off, 0), n(0), C::NotEqual, ret[3]);
    }
    b.branch(
        field(CTX, offset_of!(Entry, next_free), 0),
        q(offset_of!(Scan, capacity)),
        C::Above,
        ret[3],
    );
    b.a.jmp_label(advance);
    b.a.bind_text_label(occupied);
    b.branch(
        field(CTX, offset_of!(Slot, serial), 0),
        field(G, offset_of!(Table, last_serial), 0),
        C::Above,
        ret[3],
    );
    b.branch(
        field(CTX, offset_of!(Slot, gate), 0),
        local(REG),
        C::NotEqual,
        ret[3],
    );
    b.branch(
        field(CTX, offset_of!(Entry, next_free), 0),
        n(0),
        C::NotEqual,
        ret[3],
    );
    b.branch(
        field(CTX, offset_of!(Slot, reference), 0),
        n(0),
        C::NotEqual,
        live,
    );
    for off in [24, 32, 40] {
        b.branch(field(CTX, off, 0), n(0), C::NotEqual, ret[3]);
    }
    b.a.jmp_label(advance);
    b.a.bind_text_label(live);
    super::barrier::reference(&mut b, field(CTX, offset_of!(Slot, reference), 0), ret[3]);
    for off in [0, 8, 16] {
        b.set(
            field(NEXT, off, 0),
            field(CTX, offset_of!(Slot, lease) + off, 0),
        );
    }
    b.value(q(offset_of!(Scan, pool)));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, NEXT);
    b.a.mov_imm64(R::V2, 2);
    b.a.call_label(metadata);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, ret[3]);
    for off in [0, 8] {
        b.branch(
            field(NEXT, off, 0),
            field(CTX, offset_of!(Slot, lease) + off, 0),
            C::NotEqual,
            ret[3],
        );
    }
    b.set(local(SEG), field(NEXT, 0, 0));
    b.set(local(USED), field(SEG, offset_of!(Page, allocated_top), 8));
    b.set(
        local(LEASE),
        shr(field(CTX, offset_of!(Slot, reference), 0), 16),
    );
    for (left, right, condition) in [
        (
            field(SEG, offset_of!(Page, start), 0),
            field(SEG, offset_of!(Page, limit), 0),
            C::AboveOrEqual,
        ),
        (
            field(SEG, offset_of!(Page, limit), 0),
            n(MAX_OFFSET),
            C::Above,
        ),
        (
            local(USED),
            field(SEG, offset_of!(Page, start), 0),
            C::Below,
        ),
        (
            local(USED),
            field(SEG, offset_of!(Page, limit), 0),
            C::Above,
        ),
        (
            local(LEASE),
            add(field(SEG, offset_of!(Page, start), 0), n(32)),
            C::Below,
        ),
        (local(LEASE), local(USED), C::AboveOrEqual),
    ] {
        b.branch(left, right, condition, ret[3]);
    }
    b.branch(
        local(LEASE),
        q(offset_of!(Scan, target_start)),
        C::Below,
        advance,
    );
    b.branch(
        local(LEASE),
        q(offset_of!(Scan, target_limit)),
        C::Below,
        found,
    );
    b.a.jmp_label(advance);
    b.a.bind_text_label(found);
    b.set(q(offset_of!(Scan, handle)), local(G));
    b.set(
        q(offset_of!(Scan, handle) + 8),
        add(q(offset_of!(Scan, cursor)), n(1)),
    );
    b.set(
        q(offset_of!(Scan, handle) + 16),
        field(CTX, offset_of!(Slot, serial), 0),
    );
    b.set(
        q(offset_of!(Scan, observed)),
        field(CTX, offset_of!(Slot, reference), 0),
    );
    b.a.jmp_label(ret[2]);
    b.a.bind_text_label(captured_done);
    b.branch(
        q(offset_of!(Scan, cursor)),
        q(offset_of!(Scan, capacity)),
        C::NotEqual,
        ret[3],
    );
    b.a.jmp_label(ret[0]);
    b.a.bind_text_label(complete);
    b.set(q(offset_of!(Scan, stage)), n(2));
    b.set(local(STATE), n(0));
    b.a.jmp_label(clear);
    b.a.bind_text_label(advance);
    b.set(
        q(offset_of!(Scan, cursor)),
        add(q(offset_of!(Scan, cursor)), n(1)),
    );
    b.set(local(STATE), n(1));
    b.a.bind_text_label(clear);
    for off in [80, 88, 96, 104] {
        b.set(q(off), n(0));
    }
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    b.a.bind_text_label(capture);
    b.branch(
        field(REG, offset_of!(Gate, closed), 0),
        n(0),
        C::Equal,
        ret[4],
    );
    b.set(q(offset_of!(Scan, table)), local(G));
    b.set(q(offset_of!(Scan, gate)), local(REG));
    b.set(
        q(offset_of!(Scan, pool)),
        field(REG, offset_of!(Gate, pool), 0),
    );
    for (table_off, scan_off) in [
        (offset_of!(Table, entries), offset_of!(Scan, entries)),
        (offset_of!(Table, capacity), offset_of!(Scan, capacity)),
        (
            offset_of!(Table, last_serial),
            offset_of!(Scan, last_serial),
        ),
    ] {
        b.set(q(scan_off), field(G, table_off, 0));
    }
    b.set(q(offset_of!(Scan, stage)), n(1));
    b.a.jmp_label(ret[1]);
    for (value, label) in ret.into_iter().enumerate() {
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
}
