//! Registered strong root -> metadata-owning weak handle; gc_weak_create_step_locked.
use super::*;
use crate::gc_abi::{
    RootRequest as Root, WeakCreateRequest as Request, WeakEntry as Entry, WeakHandle as Handle,
    WeakLoadGate as Gate, WeakSlot as Slot, WeakTable as Table,
};
use std::mem::size_of;
const TABLE: i32 = 104;
const SOURCE: i32 = 112;
const OUT: i32 = 120;
const ROOT_COPY: i32 = 128;
const HELD: i32 = 136;
const INDEX: i32 = 144;

#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let ret: [_; 7] = std::array::from_fn(|_| a.create_text_label());
    let room = a.create_text_label();
    let prepared = a.create_text_label();
    let commit = a.create_text_label();
    let roots = a.create_text_label();
    let acquire = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    for bytes in [112, 112, 48] {
        a.sub_reg_imm8(R::V4, bytes);
    }
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(TABLE, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    for (slot, offset) in [(ROOT_COPY, 208), (HELD, 256)] {
        a.mov_reg_reg(R::V0, R::V5);
        a.sub_reg_imm32(R::V0, offset);
        a.store_rbp_slot(slot, R::V0);
    }
    let mut b = Emit { a };
    b.pointer(local(Q), ret[3]);
    b.branch(local(OP), n(0), C::NotEqual, ret[3]);
    super::weak_follow::validate_table(&mut b, ret[3]);
    for off in [
        offset_of!(Request, admission),
        offset_of!(Request, context),
        offset_of!(Request, source_root),
        offset_of!(Request, output_handle),
    ] {
        b.pointer(field(Q, off, 0), ret[3]);
    }
    b.set(local(SOURCE), field(Q, offset_of!(Request, source_root), 0));
    b.set(local(OUT), field(Q, offset_of!(Request, output_handle), 0));
    for off in (0..size_of::<Handle>()).step_by(8) {
        b.branch(field(OUT, off, 0), n(0), C::NotEqual, ret[3]);
    }
    b.set(local(G), local(REG));
    b.branch(
        field(G, offset_of!(Gate, closed), 0),
        n(0),
        C::NotEqual,
        ret[2],
    );
    super::weak_load::validate_admission(
        &mut b,
        field(Q, offset_of!(Request, admission), 0),
        field(Q, offset_of!(Request, context), 0),
        ret[3],
        ret[2],
    );
    b.set(local(SEG), field(SOURCE, offset_of!(Root, segment), 0));
    b.pointer(local(SEG), ret[3]);
    b.set(
        local(REG),
        field(SEG, offset_of!(RootSegment, registration), 0),
    );
    b.pointer(local(REG), ret[3]);
    for (left, right) in [
        (
            field(REG, offset_of!(RootRegistration, segment), 0),
            local(SEG),
        ),
        (
            field(REG, offset_of!(RootRegistration, scope), 0),
            field(SEG, offset_of!(RootSegment, scope), 0),
        ),
        (
            field(REG, offset_of!(RootRegistration, context), 0),
            local(CTX),
        ),
        (
            field(REG, offset_of!(RootRegistration, registered), 0),
            n(1),
        ),
    ] {
        b.branch(left, right, C::NotEqual, ret[3]);
    }
    for off in (0..size_of::<Root>()).step_by(8) {
        b.set(field(ROOT_COPY, off, 0), field(SOURCE, off, 0));
    }
    b.a.load_rbp_slot(R::V7, CTX);
    b.a.load_rbp_slot(R::V6, ROOT_COPY);
    b.a.mov_imm64(R::V2, 3);
    b.a.call_label(roots);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, ret[3]);
    b.branch(
        field(TABLE, offset_of!(Table, last_serial), 0),
        n(u64::MAX),
        C::Equal,
        ret[5],
    );
    b.set(local(INDEX), field(TABLE, offset_of!(Table, free_head), 0));
    b.branch(local(INDEX), n(0), C::Equal, ret[6]);
    let position = sub(local(INDEX), n(1));
    b.set(
        local(PREV),
        add(
            field(TABLE, offset_of!(Table, entries), 0),
            sub(shl(position.clone(), 6), shl(position, 3)),
        ),
    );
    for off in (0..size_of::<Slot>()).step_by(8) {
        b.branch(field(PREV, off, 0), n(0), C::NotEqual, ret[3]);
    }
    let link = field(PREV, offset_of!(Entry, next_free), 0);
    b.branch(
        link.clone(),
        field(TABLE, offset_of!(Table, capacity), 0),
        C::Above,
        ret[3],
    );
    b.branch(link.clone(), local(INDEX), C::Equal, ret[3]);
    b.branch(
        add(field(TABLE, offset_of!(Table, active), 0), n(1)),
        field(TABLE, offset_of!(Table, capacity), 0),
        C::Below,
        room,
    );
    b.branch(link.clone(), n(0), C::NotEqual, ret[3]);
    b.a.jmp_label(prepared);
    b.a.bind_text_label(room);
    b.branch(link, n(0), C::Equal, ret[3]);
    b.a.bind_text_label(prepared);
    b.set(
        field(HELD, 0, 0),
        field(ROOT_COPY, offset_of!(Root, value), 0),
    );
    for off in [8, 16, 24] {
        b.set(field(HELD, off, 0), n(0));
    }
    b.branch(field(HELD, 0, 0), n(0), C::Equal, commit);
    b.value(field(G, offset_of!(Gate, pool), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, HELD);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(acquire);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(2), C::Equal, ret[6]);
    b.branch(local(STATE), n(3), C::Equal, ret[5]);
    b.branch(local(STATE), n(0), C::NotEqual, ret[3]);
    b.a.bind_text_label(commit);
    b.set(
        local(STATE),
        add(field(TABLE, offset_of!(Table, last_serial), 0), n(1)),
    );
    b.set(
        field(TABLE, offset_of!(Table, free_head), 0),
        field(PREV, offset_of!(Entry, next_free), 0),
    );
    b.set(
        field(TABLE, offset_of!(Table, active), 0),
        add(field(TABLE, offset_of!(Table, active), 0), n(1)),
    );
    b.set(
        field(TABLE, offset_of!(Table, last_serial), 0),
        local(STATE),
    );
    b.set(field(PREV, offset_of!(Slot, gate), 0), local(G));
    b.set(field(PREV, offset_of!(Slot, serial), 0), local(STATE));
    for off in [0, 8, 16, 24] {
        b.set(
            field(PREV, offset_of!(Slot, reference) + off, 0),
            field(HELD, off, 0),
        );
    }
    b.set(field(PREV, offset_of!(Entry, next_free), 0), n(0));
    b.set(field(OUT, offset_of!(Handle, table), 0), local(TABLE));
    b.set(field(OUT, offset_of!(Handle, index), 0), local(INDEX));
    b.set(field(OUT, offset_of!(Handle, serial), 0), local(STATE));
    b.a.jmp_label(ret[0]);
    for (value, label) in ret.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(roots);
    crate::gc_codegen::roots::emit_step(b.a);
    b.a.ret();
    b.a.bind_text_label(acquire);
    super::reference_lease::emit_reference_lease(b.a);
    b.a.ret();
}
