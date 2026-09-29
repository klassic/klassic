//! Constant-time relocation admission and publication-quiescence gate.
use super::*;
use crate::gc_abi::{RelocationGate, RelocationGateRequest, RelocationPermit};
macro_rules! gate {
    ($f:ident) => {
        field(G, offset_of!(RelocationGate, $f), 0)
    };
}
macro_rules! request {
    ($f:ident) => {
        field(Q, offset_of!(RelocationGateRequest, $f), 0)
    };
}
macro_rules! permit {
    ($base:expr,$f:ident) => {
        field($base, offset_of!(RelocationPermit, $f), 0)
    };
}
#[allow(dead_code)]
pub(crate) fn emit_relocation_gate<A: PortableAsm>(a: &mut A) {
    emit_gate(a, false);
}

#[allow(dead_code)]
pub(crate) fn emit_pin_page<A: PortableAsm>(a: &mut A) {
    emit_gate(a, true);
}

fn emit_gate<A: PortableAsm>(a: &mut A, pin: bool) {
    let invalid = a.create_text_label();
    let wait = a.create_text_label();
    let exhausted = a.create_text_label();
    let ok = a.create_text_label();
    let done = a.create_text_label();
    let close = a.create_text_label();
    let check = a.create_text_label();
    let reopen = a.create_text_label();
    let release = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(if pin { 3 } else { 5 }), C::Above, invalid);
    if pin {
        b.pointer(local(G), invalid);
        b.pointer(local(Q), invalid);
    }
    b.branch(gate!(epoch), n(0), C::Equal, invalid);
    b.branch(gate!(closed), n(1), C::Above, invalid);
    b.branch(gate!(active), gate!(last_serial), C::Above, invalid);
    b.branch(and(gate!(head), n(7)), n(0), C::NotEqual, invalid);
    let empty = b.a.create_text_label();
    let valid = b.a.create_text_label();
    b.branch(gate!(active), n(0), C::Equal, empty);
    b.branch(gate!(head), n(0), C::Equal, invalid);
    b.a.jmp_label(valid);
    b.a.bind_text_label(empty);
    b.branch(gate!(head), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(valid);
    if pin {
        let permit_op = b.a.create_text_label();
        b.branch(local(OP), n(2), C::Below, permit_op);
        b.branch(request!(permit), n(0), C::NotEqual, invalid);
        b.branch(request!(serial), n(0), C::NotEqual, invalid);
        let selection = b.a.create_text_label();
        b.branch(local(OP), n(2), C::Equal, selection);
        b.branch(request!(epoch), n(0), C::Equal, invalid);
        b.branch(request!(epoch), gate!(epoch), C::NotEqual, invalid);
        b.a.jmp_label(reopen);
        b.a.bind_text_label(selection);
        b.branch(request!(epoch), n(0), C::NotEqual, invalid);
        b.branch(gate!(active), n(0), C::NotEqual, wait);
        b.branch(gate!(closed), n(0), C::NotEqual, wait);
        b.set(gate!(closed), n(1));
        b.set(request!(epoch), gate!(epoch));
        b.a.jmp_label(ok);
        b.a.bind_text_label(permit_op);
    }
    b.branch(local(OP), n(2), C::Equal, close);
    b.branch(local(OP), n(3), C::Equal, check);
    b.branch(local(OP), n(4), C::Equal, reopen);
    b.pointer(local(Q), invalid);
    b.pointer(request!(permit), invalid);
    b.set(local(SEG), request!(permit));
    b.branch(local(OP), n(0), C::NotEqual, release);
    for value in [
        request!(serial),
        request!(epoch),
        permit!(SEG, gate),
        permit!(SEG, serial),
        permit!(SEG, epoch),
        permit!(SEG, previous),
        permit!(SEG, next),
        permit!(SEG, job),
    ] {
        b.branch(value, n(0), C::NotEqual, invalid);
    }
    b.branch(gate!(closed), n(0), C::NotEqual, wait);
    b.branch(gate!(last_serial), n(u64::MAX), C::Equal, exhausted);
    b.set(local(NEXT), gate!(head));
    let acquire = b.a.create_text_label();
    b.branch(local(NEXT), n(0), C::Equal, acquire);
    b.branch(permit!(NEXT, gate), local(G), C::NotEqual, invalid);
    b.branch(permit!(NEXT, epoch), gate!(epoch), C::NotEqual, invalid);
    b.branch(permit!(NEXT, previous), n(0), C::NotEqual, invalid);
    b.branch(permit!(NEXT, serial), n(0), C::Equal, invalid);
    b.branch(permit!(NEXT, serial), gate!(last_serial), C::Above, invalid);
    b.a.bind_text_label(acquire);
    b.set(gate!(last_serial), add(gate!(last_serial), n(1)));
    b.set(permit!(SEG, gate), local(G));
    b.set(permit!(SEG, serial), gate!(last_serial));
    b.set(permit!(SEG, epoch), gate!(epoch));
    b.set(permit!(SEG, previous), n(0));
    b.set(permit!(SEG, next), local(NEXT));
    let no_head = b.a.create_text_label();
    b.branch(local(NEXT), n(0), C::Equal, no_head);
    b.set(permit!(NEXT, previous), local(SEG));
    b.a.bind_text_label(no_head);
    b.set(gate!(head), local(SEG));
    b.set(gate!(active), add(gate!(active), n(1)));
    b.set(request!(serial), permit!(SEG, serial));
    b.set(request!(epoch), permit!(SEG, epoch));
    b.a.jmp_label(ok);
    b.a.bind_text_label(release);
    b.branch(permit!(SEG, job), n(0), C::NotEqual, invalid);
    b.branch(gate!(active), n(0), C::Equal, invalid);
    b.branch(permit!(SEG, gate), local(G), C::NotEqual, invalid);
    b.branch(permit!(SEG, serial), n(0), C::Equal, invalid);
    b.branch(request!(serial), permit!(SEG, serial), C::NotEqual, invalid);
    b.branch(permit!(SEG, serial), gate!(last_serial), C::Above, invalid);
    b.branch(permit!(SEG, epoch), gate!(epoch), C::NotEqual, invalid);
    b.branch(request!(epoch), permit!(SEG, epoch), C::NotEqual, invalid);
    b.set(local(PREV), permit!(SEG, previous));
    b.set(local(NEXT), permit!(SEG, next));
    for value in [local(PREV), local(NEXT)] {
        b.branch(and(value, n(7)), n(0), C::NotEqual, invalid);
    }
    let many = b.a.create_text_label();
    let links = b.a.create_text_label();
    let has_link = b.a.create_text_label();
    b.branch(gate!(active), n(1), C::NotEqual, many);
    b.branch(local(PREV), n(0), C::NotEqual, invalid);
    b.branch(local(NEXT), n(0), C::NotEqual, invalid);
    b.a.jmp_label(links);
    b.a.bind_text_label(many);
    b.branch(local(PREV), n(0), C::NotEqual, has_link);
    b.branch(local(NEXT), n(0), C::Equal, invalid);
    b.a.bind_text_label(has_link);
    b.a.bind_text_label(links);
    let head = b.a.create_text_label();
    let validate_next = b.a.create_text_label();
    let commit = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, head);
    b.branch(permit!(PREV, gate), local(G), C::NotEqual, invalid);
    b.branch(permit!(PREV, epoch), gate!(epoch), C::NotEqual, invalid);
    b.branch(
        permit!(SEG, serial),
        permit!(PREV, serial),
        C::AboveOrEqual,
        invalid,
    );
    b.branch(permit!(PREV, serial), gate!(last_serial), C::Above, invalid);
    b.branch(permit!(PREV, next), local(SEG), C::NotEqual, invalid);
    b.a.jmp_label(validate_next);
    b.a.bind_text_label(head);
    b.branch(gate!(head), local(SEG), C::NotEqual, invalid);
    b.a.bind_text_label(validate_next);
    b.branch(local(NEXT), n(0), C::Equal, commit);
    b.branch(permit!(NEXT, gate), local(G), C::NotEqual, invalid);
    b.branch(permit!(NEXT, epoch), gate!(epoch), C::NotEqual, invalid);
    b.branch(permit!(NEXT, serial), n(0), C::Equal, invalid);
    b.branch(
        permit!(NEXT, serial),
        permit!(SEG, serial),
        C::AboveOrEqual,
        invalid,
    );
    b.branch(permit!(NEXT, previous), local(SEG), C::NotEqual, invalid);
    b.a.bind_text_label(commit);
    b.branch(local(OP), n(5), C::Equal, ok);
    let change_head = b.a.create_text_label();
    let change_next = b.a.create_text_label();
    let clear = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, change_head);
    b.set(permit!(PREV, next), local(NEXT));
    b.a.jmp_label(change_next);
    b.a.bind_text_label(change_head);
    b.set(gate!(head), local(NEXT));
    b.a.bind_text_label(change_next);
    b.branch(local(NEXT), n(0), C::Equal, clear);
    b.set(permit!(NEXT, previous), local(PREV));
    b.a.bind_text_label(clear);
    b.set(gate!(active), sub(gate!(active), n(1)));
    for value in [
        permit!(SEG, gate),
        permit!(SEG, serial),
        permit!(SEG, epoch),
        permit!(SEG, previous),
        permit!(SEG, next),
        permit!(SEG, job),
    ] {
        b.set(value, n(0));
    }
    b.a.jmp_label(ok);
    b.a.bind_text_label(close);
    b.set(gate!(closed), n(1));
    b.branch(gate!(active), n(0), C::NotEqual, wait);
    b.a.jmp_label(ok);
    b.a.bind_text_label(check);
    b.branch(gate!(closed), n(0), C::Equal, wait);
    b.branch(gate!(active), n(0), C::NotEqual, wait);
    b.a.jmp_label(ok);
    b.a.bind_text_label(reopen);
    b.branch(gate!(closed), n(0), C::Equal, invalid);
    b.branch(gate!(active), n(0), C::NotEqual, wait);
    b.branch(gate!(epoch), n(u64::MAX), C::Equal, exhausted);
    b.set(gate!(epoch), add(gate!(epoch), n(1)));
    b.set(gate!(closed), n(0));
    if pin {
        b.set(request!(epoch), n(0));
    }
    b.a.jmp_label(ok);
    for (label, status) in [(ok, 0), (invalid, 1), (wait, 2), (exhausted, 3)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
