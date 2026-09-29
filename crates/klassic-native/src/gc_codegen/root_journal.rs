//! Registered journal claims retain ring slots and a counted storage lease
//! through out-of-lock copying and mark handoff. Matches gc_root_journal.h.
use super::*;
use crate::gc_abi::{
    BufferCursor, RootJournalRequest, RootLease, RootRegistration, RootRegistry, RootSegment,
};
#[allow(dead_code)]
pub(crate) fn emit_root_journal<A: PortableAsm>(a: &mut A) {
    let status: [_; 5] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let claim = a.create_text_label();
    let copy = a.create_text_label();
    let capped = a.create_text_label();
    let loop_top = a.create_text_label();
    let copied = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(RootJournalRequest, $f), 0)
        };
    }
    macro_rules! r {
        ($f:ident) => {
            field(REG, offset_of!(RootRegistration, $f), 0)
        };
    }
    macro_rules! l {
        ($f:ident) => {
            field(LEASE, offset_of!(RootLease, $f), 0)
        };
    }
    macro_rules! g {
        ($f:ident) => {
            field(G, offset_of!(RootRegistry, $f), 0)
        };
    }
    macro_rules! s {
        ($f:ident) => {
            field(SEG, offset_of!(RootSegment, $f), 0)
        };
    }
    macro_rules! buf {
        ($f:ident) => {
            field(CTX, offset_of!(BufferCursor, $f), 0)
        };
    }
    b.branch(local(OP), n(2), C::Above, invalid);
    b.set(local(REG), q!(registration));
    b.pointer(local(REG), invalid);
    b.set(local(LEASE), q!(lease));
    b.pointer(local(LEASE), invalid);
    b.branch(r!(registered), n(1), C::NotEqual, invalid);
    b.branch(r!(scope), n(0), C::Equal, invalid);
    b.branch(q!(scope), r!(scope), C::NotEqual, invalid);
    b.set(local(SEG), r!(segment));
    b.pointer(local(SEG), invalid);
    b.set(local(STATE), field(SEG, offset_of!(RootSegment, state), 8));
    b.branch(s!(registration), local(REG), C::NotEqual, invalid);
    b.branch(s!(scope), r!(scope), C::NotEqual, invalid);
    b.branch(local(STATE), n(1), C::Below, invalid);
    b.branch(local(STATE), n(2), C::Above, invalid);
    b.set(local(CTX), s!(journal));
    b.pointer(local(CTX), invalid);
    b.set(local(KIND), buf!(entries));
    b.pointer(local(KIND), invalid);
    b.branch(buf!(capacity), n(0), C::Equal, invalid);
    b.branch(buf!(capacity), n(u32::MAX as u64), C::Above, invalid);
    b.branch(
        add(local(KIND), shl(buf!(capacity), 5)),
        local(KIND),
        C::Below,
        invalid,
    );
    b.set(
        local(PREV),
        field(CTX, offset_of!(BufferCursor, consumed), 8),
    );
    b.set(
        local(NEXT),
        field(CTX, offset_of!(BufferCursor, published), 8),
    );
    b.branch(local(PREV), local(NEXT), C::Above, invalid);
    b.branch(
        sub(local(NEXT), local(PREV)),
        buf!(capacity),
        C::Above,
        invalid,
    );
    b.branch(local(OP), n(0), C::Equal, claim);
    b.branch(l!(kind), n(4), C::NotEqual, invalid);
    b.branch(q!(serial), n(0), C::Equal, invalid);
    b.branch(q!(serial), l!(serial), C::NotEqual, invalid);
    b.branch(l!(registration), local(REG), C::NotEqual, invalid);
    b.branch(l!(scope), r!(scope), C::NotEqual, invalid);
    b.branch(r!(journal_serial), l!(serial), C::NotEqual, invalid);
    b.branch(q!(begin), local(PREV), C::NotEqual, invalid);
    b.branch(q!(end), r!(journal_end), C::NotEqual, invalid);
    b.branch(q!(end), local(PREV), C::Below, invalid);
    b.branch(q!(end), local(PREV), C::Equal, invalid);
    b.branch(q!(end), local(NEXT), C::Above, invalid);
    b.set(local(USED), sub(q!(end), local(PREV)));
    b.branch(local(USED), n(256), C::Above, invalid);
    b.branch(local(OP), n(1), C::Equal, copy);
    b.branch(r!(leases), n(0), C::Equal, invalid);
    b.branch(r!(leases), g!(leases), C::Above, invalid);
    b.branch(l!(cursor), q!(end), C::NotEqual, status[3]);
    b.branch(q!(written), local(USED), C::NotEqual, status[3]);
    b.set(field(CTX, offset_of!(BufferCursor, consumed), 8), q!(end));
    b.set(r!(journal_serial), n(0));
    b.set(r!(journal_end), n(0));
    b.set(r!(leases), sub(r!(leases), n(1)));
    b.set(g!(leases), sub(g!(leases), n(1)));
    b.set(l!(kind), n(0));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(claim);
    b.branch(l!(kind), n(0), C::NotEqual, invalid);
    b.branch(r!(leases), g!(leases), C::Above, invalid);
    let unclaimed = b.a.create_text_label();
    b.branch(r!(journal_serial), n(0), C::Equal, unclaimed);
    b.branch(r!(journal_end), n(0), C::Equal, invalid);
    b.a.jmp_label(status[3]);
    b.a.bind_text_label(unclaimed);
    b.branch(r!(journal_end), n(0), C::NotEqual, invalid);
    b.branch(q!(capacity), n(0), C::Equal, invalid);
    b.branch(q!(capacity), n(256), C::Above, invalid);
    b.branch(local(PREV), local(NEXT), C::Equal, status[2]);
    for value in [g!(last_lease), g!(leases), r!(leases)] {
        b.branch(value, n(u64::MAX), C::Equal, status[4]);
    }
    b.set(local(USED), sub(local(NEXT), local(PREV)));
    b.branch(local(USED), q!(capacity), C::Below, capped);
    b.set(local(USED), q!(capacity));
    b.a.bind_text_label(capped);
    b.set(g!(last_lease), add(g!(last_lease), n(1)));
    b.set(l!(registration), local(REG));
    b.set(l!(scope), r!(scope));
    b.set(l!(serial), g!(last_lease));
    b.set(l!(kind), n(4));
    b.set(l!(cursor), local(PREV));
    b.set(r!(leases), add(r!(leases), n(1)));
    b.set(g!(leases), add(g!(leases), n(1)));
    b.set(r!(journal_serial), l!(serial));
    b.set(r!(journal_end), add(local(PREV), local(USED)));
    b.set(q!(serial), l!(serial));
    b.set(q!(begin), local(PREV));
    b.set(q!(end), r!(journal_end));
    b.set(q!(written), n(0));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(copy);
    b.branch(l!(cursor), local(PREV), C::NotEqual, invalid);
    b.branch(q!(written), n(0), C::NotEqual, invalid);
    b.pointer(q!(output), invalid);
    b.branch(q!(capacity), local(USED), C::Below, invalid);
    b.branch(q!(capacity), n(256), C::Above, invalid);
    b.branch(
        add(q!(output), shl(q!(capacity), 5)),
        q!(output),
        C::Below,
        invalid,
    );
    b.set(local(STATE), n(0));
    b.a.bind_text_label(loop_top);
    b.branch(local(STATE), local(USED), C::Equal, copied);
    b.value(add(local(PREV), local(STATE)));
    b.a.load_rbp_slot(R::V11, CTX);
    b.a.load_ptr_disp32(R::V1, R::V11, offset_of!(BufferCursor, capacity) as i32);
    b.a.div_unsigned(R::V1);
    b.a.store_rbp_slot(SEG, R::V2);
    b.set(local(SEG), add(local(KIND), shl(local(SEG), 5)));
    b.set(local(NEXT), add(q!(output), shl(local(STATE), 5)));
    for off in [0, 8, 16, 24] {
        b.set(field(NEXT, off, 0), field(SEG, off, 0));
    }
    b.set(local(STATE), add(local(STATE), n(1)));
    b.a.jmp_label(loop_top);
    b.a.bind_text_label(copied);
    b.set(q!(written), local(USED));
    b.set(l!(cursor), q!(end));
    b.a.jmp_label(status[0]);
    for (code, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, code as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
