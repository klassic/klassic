//! Registry-serialized precise slot maps; matches gc_remembered.h.
use super::*;
use crate::gc_abi::{RememberedJob as J, RememberedSet as S};
pub(super) fn phase<A: PortableAsm>(b: &mut Emit<'_, A>, v: E, bad: A::TextLabel) {
    b.branch(and(v.clone(), n(0xe000)), n(0), C::NotEqual, bad);
    b.branch(and(v.clone(), n(15)), n(7), C::Above, bad);
    b.branch(and(shr(v, 4), n(15)), n(7), C::Above, bad);
}
// Bit masks use at most 63 fixed shifts; this is a buffer-consumer operation,
// not the mutator store fast path. No ISA-dependent variable shift is hidden.
fn mask<A: PortableAsm>(b: &mut Emit<'_, A>) {
    let top = b.a.create_text_label();
    let done = b.a.create_text_label();
    b.set(local(LEASE), n(1));
    b.set(local(USED), and(local(STATE), n(63)));
    b.a.bind_text_label(top);
    b.branch(local(USED), n(0), C::Equal, done);
    b.set(local(LEASE), shl(local(LEASE), 1));
    b.set(local(USED), sub(local(USED), n(1)));
    b.a.jmp_label(top);
    b.a.bind_text_label(done);
}
#[allow(dead_code)]
pub(crate) fn emit_remembered<A: PortableAsm>(a: &mut A) {
    let status: [_; 7] = std::array::from_fn(|_| a.create_text_label());
    let bad = status[1];
    let flip = a.create_text_label();
    let record = a.create_text_label();
    let take = a.create_text_label();
    let check = a.create_text_label();
    let finish = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! s {
        ($f:ident) => {
            field(G, offset_of!(S, $f), 0)
        };
    }
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(J, $f), 0)
        };
    }
    b.branch(local(OP), n(4), C::Above, bad);
    b.pointer(local(G), bad);
    b.pointer(local(Q), bad);
    b.branch(s!(start), n(0), C::Equal, bad);
    b.branch(s!(start), s!(limit), C::Above, bad);
    b.branch(s!(start), s!(limit), C::Equal, bad);
    b.branch(and(s!(start), n(7)), n(0), C::NotEqual, bad);
    b.branch(and(s!(limit), n(7)), n(0), C::NotEqual, bad);
    b.branch(s!(limit), n(crate::gc_abi::MAX_OFFSET), C::Above, bad);
    b.branch(s!(incarnation), n(0), C::Equal, bad);
    b.branch(s!(current), n(1), C::Above, bad);
    b.set(local(STATE), shr(sub(s!(limit), s!(start)), 3));
    b.branch(s!(words), n(0), C::Equal, bad);
    b.branch(
        s!(words),
        shr(add(local(STATE), n(63)), 6),
        C::NotEqual,
        bad,
    );
    let bank0 = field(G, offset_of!(S, bits), 0);
    let bank1 = field(G, offset_of!(S, bits) + 8, 0);
    b.pointer(bank0.clone(), bad);
    b.pointer(bank1.clone(), bad);
    b.branch(bank0.clone(), bank1.clone(), C::Equal, bad);
    for bank in [bank0.clone(), bank1.clone()] {
        b.branch(add(bank.clone(), shl(s!(words), 3)), bank, C::Below, bad);
    }
    phase(&mut b, s!(epoch), bad);
    phase(&mut b, s!(last_phase), bad);
    b.branch(shr(s!(epoch), 16), n(0), C::Equal, bad);
    b.branch(s!(last_phase), s!(epoch), C::Below, bad);
    b.branch(s!(current_count), local(STATE), C::Above, bad);
    b.branch(s!(pending), local(STATE), C::Above, bad);
    b.branch(s!(cursor), s!(words), C::Above, bad);
    b.branch(s!(cursor), s!(words), C::Equal, bad);
    b.branch(s!(active_serial), s!(last_serial), C::Above, bad);
    let inactive = b.a.create_text_label();
    let active_ok = b.a.create_text_label();
    b.branch(s!(active_serial), n(0), C::Equal, inactive);
    b.branch(s!(active_offset), s!(start), C::Below, bad);
    b.branch(s!(active_offset), s!(limit), C::Above, bad);
    b.branch(s!(active_offset), s!(limit), C::Equal, bad);
    b.branch(and(s!(active_offset), n(7)), n(0), C::NotEqual, bad);
    b.a.jmp_label(active_ok);
    b.a.bind_text_label(inactive);
    b.branch(s!(active_offset), n(0), C::NotEqual, bad);
    b.a.bind_text_label(active_ok);
    let no_previous = b.a.create_text_label();
    let previous_ok = b.a.create_text_label();
    b.branch(s!(previous_epoch), n(0), C::Equal, no_previous);
    phase(&mut b, s!(previous_epoch), bad);
    b.branch(shr(s!(previous_epoch), 16), n(0), C::Equal, bad);
    b.branch(
        shr(s!(previous_epoch), 16),
        shr(s!(epoch), 16),
        C::Above,
        bad,
    );
    b.branch(
        shr(s!(previous_epoch), 16),
        shr(s!(epoch), 16),
        C::Equal,
        bad,
    );
    b.branch(
        and(s!(previous_epoch), n(0x1000)),
        and(s!(epoch), n(0x1000)),
        C::Equal,
        bad,
    );
    b.a.jmp_label(previous_ok);
    b.a.bind_text_label(no_previous);
    b.branch(s!(pending), n(0), C::NotEqual, bad);
    b.branch(s!(active_serial), n(0), C::NotEqual, bad);
    b.a.bind_text_label(previous_ok);
    b.set(local(REG), bank0.clone());
    b.set(local(SEG), bank1.clone());
    let banks = b.a.create_text_label();
    b.branch(s!(current), n(0), C::Equal, banks);
    b.set(local(REG), bank1);
    b.set(local(SEG), bank0);
    b.a.bind_text_label(banks);
    b.branch(local(OP), n(4), C::Equal, check);
    b.branch(local(OP), n(1), C::Equal, flip);
    b.branch(local(OP), n(2), C::Equal, take);
    b.branch(q!(incarnation), s!(incarnation), C::NotEqual, bad);
    b.branch(q!(offset), s!(start), C::Below, bad);
    b.branch(q!(offset), s!(limit), C::Above, bad);
    b.branch(q!(offset), s!(limit), C::Equal, bad);
    b.branch(and(q!(offset), n(7)), n(0), C::NotEqual, bad);
    b.set(local(STATE), shr(sub(q!(offset), s!(start)), 3));
    mask(&mut b);
    b.set(local(REG), add(local(REG), shl(shr(local(STATE), 6), 3)));
    b.set(local(SEG), add(local(SEG), shl(shr(local(STATE), 6), 3)));
    b.set(local(KIND), n(0));
    b.branch(local(OP), n(0), C::Equal, record);
    b.branch(q!(serial), n(0), C::Equal, bad);
    b.branch(q!(serial), s!(active_serial), C::NotEqual, bad);
    b.branch(q!(offset), s!(active_offset), C::NotEqual, bad);
    b.branch(q!(phase), s!(epoch), C::NotEqual, bad);
    b.branch(q!(keep), n(1), C::Above, bad);
    let set_current = b.a.create_text_label();
    let after_current = b.a.create_text_label();
    let late = b.a.create_text_label();
    let phase_ok = b.a.create_text_label();
    b.branch(q!(keep), n(0), C::Equal, after_current);
    b.a.jmp_label(set_current);
    b.a.bind_text_label(record);
    b.branch(q!(serial), n(0), C::NotEqual, bad);
    phase(&mut b, q!(phase), bad);
    b.branch(shr(q!(phase), 16), n(0), C::Equal, bad);
    b.branch(q!(phase), s!(epoch), C::Below, late);
    b.branch(
        and(q!(phase), n(0x1000)),
        and(s!(epoch), n(0x1000)),
        C::NotEqual,
        bad,
    );
    b.a.jmp_label(phase_ok);
    b.a.bind_text_label(late);
    b.branch(s!(previous_epoch), n(0), C::Equal, status[6]);
    b.branch(q!(phase), s!(previous_epoch), C::Below, status[6]);
    b.branch(
        and(q!(phase), n(0x1000)),
        and(s!(previous_epoch), n(0x1000)),
        C::NotEqual,
        bad,
    );
    b.set(local(KIND), n(1));
    b.a.bind_text_label(phase_ok);
    b.a.bind_text_label(set_current);
    b.branch(
        and(field(REG, 0, 0), local(LEASE)),
        n(0),
        C::NotEqual,
        after_current,
    );
    b.set(field(REG, 0, 0), add(field(REG, 0, 0), local(LEASE)));
    b.set(s!(current_count), add(s!(current_count), n(1)));
    b.a.bind_text_label(after_current);
    let after_previous = b.a.create_text_label();
    b.branch(local(KIND), n(0), C::Equal, after_previous);
    b.branch(
        and(field(SEG, 0, 0), local(LEASE)),
        n(0),
        C::NotEqual,
        after_previous,
    );
    b.set(field(SEG, 0, 0), add(field(SEG, 0, 0), local(LEASE)));
    b.set(s!(pending), add(s!(pending), n(1)));
    b.a.bind_text_label(after_previous);
    let ack = b.a.create_text_label();
    b.branch(local(OP), n(3), C::Equal, ack);
    b.branch(q!(phase), s!(last_phase), C::Below, status[0]);
    b.set(s!(last_phase), q!(phase));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(ack);
    b.set(s!(active_serial), n(0));
    b.set(s!(active_offset), n(0));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(check);
    b.branch(s!(pending), n(0), C::NotEqual, status[2]);
    b.branch(s!(active_serial), n(0), C::NotEqual, status[2]);
    b.a.jmp_label(status[3]);
    b.a.bind_text_label(flip);
    b.branch(q!(serial), n(0), C::NotEqual, bad);
    b.branch(q!(incarnation), s!(incarnation), C::NotEqual, bad);
    phase(&mut b, q!(phase), bad);
    b.branch(and(q!(phase), n(15)), n(1), C::NotEqual, bad);
    b.branch(shr(q!(phase), 16), shr(s!(last_phase), 16), C::Below, bad);
    b.branch(shr(q!(phase), 16), shr(s!(last_phase), 16), C::Equal, bad);
    b.branch(
        and(q!(phase), n(0x1000)),
        and(s!(epoch), n(0x1000)),
        C::Equal,
        bad,
    );
    b.branch(s!(pending), n(0), C::NotEqual, status[2]);
    b.branch(s!(active_serial), n(0), C::NotEqual, status[2]);
    b.set(s!(previous_epoch), s!(epoch));
    b.set(s!(epoch), q!(phase));
    b.set(s!(last_phase), q!(phase));
    b.set(s!(current), sub(n(1), s!(current)));
    b.set(s!(pending), s!(current_count));
    b.set(s!(current_count), n(0));
    b.set(s!(cursor), n(0));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(take);
    b.branch(q!(serial), n(0), C::NotEqual, bad);
    b.branch(q!(budget), n(0), C::Equal, bad);
    b.branch(q!(budget), n(256), C::Above, bad);
    b.branch(s!(active_serial), n(0), C::NotEqual, status[2]);
    b.branch(s!(pending), n(0), C::Equal, status[3]);
    b.branch(s!(last_serial), n(u64::MAX), C::Equal, status[5]);
    b.set(local(PREV), s!(cursor));
    b.set(local(CTX), n(0));
    let scan = b.a.create_text_label();
    let found = b.a.create_text_label();
    let nowrap = b.a.create_text_label();
    b.a.bind_text_label(scan);
    b.set(local(REG), add(local(SEG), shl(local(PREV), 3)));
    b.set(local(USED), field(REG, 0, 0));
    b.branch(local(USED), n(0), C::NotEqual, found);
    b.set(local(PREV), add(local(PREV), n(1)));
    b.branch(local(PREV), s!(words), C::NotEqual, nowrap);
    b.set(local(PREV), n(0));
    b.a.bind_text_label(nowrap);
    b.branch(local(PREV), s!(cursor), C::Equal, bad);
    b.set(local(CTX), add(local(CTX), n(1)));
    b.branch(local(CTX), q!(budget), C::Below, scan);
    b.set(s!(cursor), local(PREV));
    b.a.jmp_label(status[4]);
    b.a.bind_text_label(found);
    b.set(local(LEASE), n(1));
    b.set(local(STATE), n(0));
    let bits = b.a.create_text_label();
    let bit_found = b.a.create_text_label();
    b.a.bind_text_label(bits);
    b.branch(and(local(USED), local(LEASE)), n(0), C::NotEqual, bit_found);
    b.set(local(LEASE), shl(local(LEASE), 1));
    b.set(local(STATE), add(local(STATE), n(1)));
    b.a.jmp_label(bits);
    b.a.bind_text_label(bit_found);
    b.set(
        local(NEXT),
        add(s!(start), shl(add(shl(local(PREV), 6), local(STATE)), 3)),
    );
    b.branch(local(NEXT), s!(limit), C::Above, bad);
    b.branch(local(NEXT), s!(limit), C::Equal, bad);
    b.set(field(REG, 0, 0), sub(local(USED), local(LEASE)));
    b.set(s!(pending), sub(s!(pending), n(1)));
    b.set(s!(last_serial), add(s!(last_serial), n(1)));
    b.set(s!(active_serial), s!(last_serial));
    b.set(s!(active_offset), local(NEXT));
    b.set(s!(cursor), local(PREV));
    b.set(q!(offset), local(NEXT));
    b.set(q!(incarnation), s!(incarnation));
    b.set(q!(phase), s!(epoch));
    b.set(q!(serial), s!(active_serial));
    b.set(q!(keep), n(0));
    b.a.jmp_label(status[0]);
    for (code, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, code as u64);
        b.a.jmp_label(finish);
    }
    b.a.bind_text_label(finish);
    b.a.leave();
}
