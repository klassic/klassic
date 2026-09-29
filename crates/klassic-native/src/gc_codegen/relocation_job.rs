//! Stable installed relocation jobs and exact publication receipts.
use super::*;
use crate::gc_abi::{ForwardCell, RelocationGate, RelocationJob, RelocationPermit};
macro_rules! gate {
    ($f:ident) => {
        field(G, offset_of!(RelocationGate, $f), 0)
    };
}
macro_rules! job {
    ($f:ident) => {
        field(Q, offset_of!(RelocationJob, $f), 0)
    };
}
macro_rules! permit {
    ($base:expr,$f:ident) => {
        field($base, offset_of!(RelocationPermit, $f), 0)
    };
}
macro_rules! source {
    ($f:ident,$a:expr) => {
        field(REG, offset_of!(ForwardCell, $f), $a)
    };
}
#[allow(dead_code)]
pub(crate) fn emit_relocation_job<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let wait = a.create_text_label();
    let ok = a.create_text_label();
    let done = a.create_text_label();
    let check = a.create_text_label();
    let published = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(2), C::Above, invalid);
    b.branch(gate!(epoch), n(0), C::Equal, invalid);
    b.branch(gate!(closed), n(1), C::Above, invalid);
    b.branch(gate!(active), gate!(last_serial), C::Above, invalid);
    b.pointer(gate!(head), invalid);
    b.pointer(job!(permit), invalid);
    b.pointer(job!(source), invalid);
    b.branch(job!(ticket), n(0), C::Equal, invalid);
    b.branch(job!(ticket), n(u64::MAX >> 2), C::Above, invalid);
    b.set(local(SEG), job!(permit));
    b.branch(gate!(active), n(0), C::Equal, invalid);
    b.branch(permit!(SEG, gate), local(G), C::NotEqual, invalid);
    b.branch(permit!(SEG, serial), n(0), C::Equal, invalid);
    b.branch(permit!(SEG, serial), job!(ticket), C::NotEqual, invalid);
    b.branch(permit!(SEG, serial), gate!(last_serial), C::Above, invalid);
    b.branch(permit!(SEG, epoch), gate!(epoch), C::NotEqual, invalid);
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

    b.set(local(REG), job!(source));
    b.branch(source!(source_offset, 0), n(0), C::Equal, invalid);
    b.branch(
        and(source!(source_offset, 0), n(15)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        source!(source_offset, 0),
        n((1u64 << 48) - 1),
        C::Above,
        invalid,
    );
    b.branch(source!(incarnation, 0), n(0), C::Equal, invalid);
    b.branch(job!(destination), n(0), C::Equal, invalid);
    super::barrier::reference(&mut b, job!(destination), invalid);
    b.branch(
        shr(job!(destination), 16),
        source!(source_offset, 0),
        C::Equal,
        invalid,
    );
    b.branch(source!(access, 8), n(1u64 << 63), C::NotEqual, invalid);
    b.set(local(STATE), source!(word, 8));
    b.set(local(USED), add(shl(job!(ticket), 2), n(1)));
    b.branch(local(OP), n(0), C::NotEqual, check);
    for value in [
        permit!(SEG, job),
        job!(owner),
        job!(gate),
        job!(serial),
        job!(epoch),
        job!(source_offset),
        job!(incarnation),
        job!(state),
    ] {
        b.branch(value, n(0), C::NotEqual, invalid);
    }
    b.branch(local(STATE), local(USED), C::NotEqual, invalid);
    b.set(job!(owner), local(Q));
    b.set(job!(gate), local(G));
    b.set(job!(serial), permit!(SEG, serial));
    b.set(job!(epoch), permit!(SEG, epoch));
    b.set(job!(source_offset), source!(source_offset, 0));
    b.set(job!(incarnation), source!(incarnation, 0));
    b.set(job!(state), n(1));
    b.set(permit!(SEG, job), local(Q));
    b.a.jmp_label(ok);
    b.a.bind_text_label(check);
    b.branch(permit!(SEG, job), local(Q), C::NotEqual, invalid);
    for (left, right) in [
        (job!(owner), local(Q)),
        (job!(gate), local(G)),
        (job!(serial), permit!(SEG, serial)),
        (job!(epoch), permit!(SEG, epoch)),
        (job!(source_offset), source!(source_offset, 0)),
        (job!(incarnation), source!(incarnation, 0)),
    ] {
        b.branch(left, right, C::NotEqual, invalid);
    }
    b.branch(job!(state), n(0), C::Equal, invalid);
    b.branch(job!(state), n(2), C::Above, invalid);
    let inspect = b.a.create_text_label();
    b.branch(local(OP), n(2), C::NotEqual, inspect);
    b.branch(job!(state), n(2), C::NotEqual, invalid);
    b.branch(
        local(STATE),
        add(job!(destination), n(2)),
        C::NotEqual,
        invalid,
    );
    b.set(job!(state), n(3));
    b.set(permit!(SEG, job), n(0));
    b.a.jmp_label(ok);
    b.a.bind_text_label(inspect);
    b.branch(local(STATE), local(USED), C::NotEqual, published);
    b.branch(job!(state), n(1), C::NotEqual, invalid);
    b.a.jmp_label(wait);
    b.a.bind_text_label(published);
    b.branch(
        local(STATE),
        add(job!(destination), n(2)),
        C::NotEqual,
        invalid,
    );
    b.set(job!(state), n(2));
    b.a.jmp_label(ok);
    for (label, status) in [(ok, 0), (invalid, 1), (wait, 2)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
