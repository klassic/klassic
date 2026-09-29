//! Aggregate remembered-page work with constant-time termination observation.
use super::*;
use crate::gc_abi::{
    AllocationPage as P, RememberedInventory as I, RememberedInventoryRequest as Qr,
    RememberedJob as J, RememberedRegistration as T, RememberedSet as S,
};
pub(super) fn valid<A: PortableAsm>(b: &mut Emit<'_, A>, slot: i32, bad: A::TextLabel) {
    macro_rules! g {
        ($f:ident) => {
            field(slot, offset_of!(I, $f), 0)
        };
    }
    b.pointer(local(slot), bad);
    let empty = b.a.create_text_label();
    let head_ok = b.a.create_text_label();
    b.branch(g!(members), n(0), C::Equal, empty);
    b.branch(g!(head), n(0), C::Equal, bad);
    b.a.jmp_label(head_ok);
    b.a.bind_text_label(empty);
    b.branch(g!(head), n(0), C::NotEqual, bad);
    b.a.bind_text_label(head_ok);
    b.branch(g!(members), g!(last_identity), C::Above, bad);
    for off in [
        offset_of!(I, pending),
        offset_of!(I, claims),
        offset_of!(I, unflipped),
    ] {
        b.branch(field(slot, off, 0), g!(members), C::Above, bad);
    }
    super::remembered::phase(b, g!(epoch), bad);
    b.branch(shr(g!(epoch), 16), n(0), C::Equal, bad);
    super::remembered::phase(b, g!(last_phase), bad);
    b.branch(g!(last_phase), g!(epoch), C::Below, bad);
    let done = b.a.create_text_label();
    b.branch(g!(previous_epoch), n(0), C::Equal, done);
    super::remembered::phase(b, g!(previous_epoch), bad);
    b.branch(shr(g!(previous_epoch), 16), n(0), C::Equal, bad);
    b.branch(
        shr(g!(previous_epoch), 16),
        shr(g!(epoch), 16),
        C::Above,
        bad,
    );
    b.branch(
        shr(g!(previous_epoch), 16),
        shr(g!(epoch), 16),
        C::Equal,
        bad,
    );
    b.branch(
        and(g!(previous_epoch), n(0x1000)),
        and(g!(epoch), n(0x1000)),
        C::Equal,
        bad,
    );
    b.a.bind_text_label(done);
}
fn boolean<A: PortableAsm>(b: &mut Emit<'_, A>, dst: i32, value: E) {
    let done = b.a.create_text_label();
    b.set(local(dst), n(0));
    b.branch(value, n(0), C::Equal, done);
    b.set(local(dst), n(1));
    b.a.bind_text_label(done);
}
#[allow(dead_code)]
pub(crate) fn emit_inventory<A: PortableAsm>(a: &mut A) {
    let status: [_; 7] = std::array::from_fn(|_| a.create_text_label());
    let bad = status[1];
    let check = a.create_text_label();
    let begin = a.create_text_label();
    let attach = a.create_text_label();
    let detach = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let core = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! g {
        ($f:ident) => {
            field(G, offset_of!(I, $f), 0)
        };
    }
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(Qr, $f), 0)
        };
    }
    macro_rules! r {
        ($f:ident) => {
            field(REG, offset_of!(T, $f), 0)
        };
    }
    macro_rules! s {
        ($f:ident) => {
            field(SEG, offset_of!(S, $f), 0)
        };
    }
    macro_rules! job {
        ($f:ident) => {
            field(LEASE, offset_of!(J, $f), 0)
        };
    }
    b.branch(local(OP), n(9), C::Above, bad);
    valid(&mut b, G, bad);
    b.pointer(local(Q), bad);
    b.branch(local(OP), n(7), C::Equal, check);
    b.branch(local(OP), n(2), C::Equal, begin);
    b.set(local(REG), q!(registration));
    b.pointer(local(REG), bad);
    b.set(local(LEASE), q!(job));
    b.pointer(local(LEASE), bad);
    b.set(local(SEG), r!(set));
    b.pointer(local(SEG), bad);
    b.a.load_rbp_slot(R::V7, SEG);
    b.a.load_rbp_slot(R::V6, LEASE);
    b.a.mov_imm64(R::V2, 4);
    b.a.call_label(core);
    b.a.store_rbp_slot(KIND, R::V0);
    let set_valid = b.a.create_text_label();
    b.branch(local(KIND), n(3), C::Equal, set_valid);
    b.branch(local(KIND), n(2), C::NotEqual, done);
    b.a.bind_text_label(set_valid);
    b.branch(local(OP), n(0), C::Equal, attach);
    b.branch(r!(registry), local(G), C::NotEqual, bad);
    b.branch(r!(identity), n(0), C::Equal, bad);
    b.branch(r!(identity), g!(last_identity), C::Above, bad);
    b.branch(g!(members), n(0), C::Equal, bad);
    b.branch(r!(epoch), s!(epoch), C::NotEqual, bad);
    let epoch_ok = b.a.create_text_label();
    b.branch(r!(epoch), g!(epoch), C::Equal, epoch_ok);
    b.branch(g!(previous_epoch), n(0), C::Equal, bad);
    b.branch(r!(epoch), g!(previous_epoch), C::NotEqual, bad);
    b.branch(g!(unflipped), n(0), C::Equal, bad);
    b.a.bind_text_label(epoch_ok);
    let flip_count_ok = b.a.create_text_label();
    b.branch(r!(epoch), g!(epoch), C::NotEqual, flip_count_ok);
    b.branch(g!(unflipped), g!(members), C::Equal, bad);
    b.a.bind_text_label(flip_count_ok);
    b.branch(and(r!(previous), n(7)), n(0), C::NotEqual, bad);
    b.branch(and(r!(next), n(7)), n(0), C::NotEqual, bad);
    b.set(local(CTX), r!(previous));
    b.set(local(PREV), r!(next));
    let head = b.a.create_text_label();
    let prev_ok = b.a.create_text_label();
    let next_ok = b.a.create_text_label();
    b.branch(local(CTX), n(0), C::Equal, head);
    b.branch(
        field(CTX, offset_of!(T, registry), 0),
        local(G),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(CTX, offset_of!(T, next), 0),
        local(REG),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(CTX, offset_of!(T, identity), 0),
        r!(identity),
        C::Below,
        bad,
    );
    b.branch(
        field(CTX, offset_of!(T, identity), 0),
        r!(identity),
        C::Equal,
        bad,
    );
    b.branch(
        field(CTX, offset_of!(T, identity), 0),
        g!(last_identity),
        C::Above,
        bad,
    );
    b.a.jmp_label(prev_ok);
    b.a.bind_text_label(head);
    b.branch(g!(head), local(REG), C::NotEqual, bad);
    b.a.bind_text_label(prev_ok);
    b.branch(local(PREV), n(0), C::Equal, next_ok);
    b.branch(
        field(PREV, offset_of!(T, registry), 0),
        local(G),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(PREV, offset_of!(T, previous), 0),
        local(REG),
        C::NotEqual,
        bad,
    );
    b.branch(field(PREV, offset_of!(T, identity), 0), n(0), C::Equal, bad);
    b.branch(
        field(PREV, offset_of!(T, identity), 0),
        r!(identity),
        C::Above,
        bad,
    );
    b.branch(
        field(PREV, offset_of!(T, identity), 0),
        r!(identity),
        C::Equal,
        bad,
    );
    b.a.bind_text_label(next_ok);
    boolean(&mut b, STATE, s!(pending));
    boolean(&mut b, USED, s!(active_serial));
    b.branch(g!(pending), local(STATE), C::Below, bad);
    b.branch(g!(claims), local(USED), C::Below, bad);
    let pending_ok = b.a.create_text_label();
    let claims_ok = b.a.create_text_label();
    b.branch(local(STATE), n(0), C::NotEqual, pending_ok);
    b.branch(g!(pending), g!(members), C::Equal, bad);
    b.a.bind_text_label(pending_ok);
    b.branch(local(USED), n(0), C::NotEqual, claims_ok);
    b.branch(g!(claims), g!(members), C::Equal, bad);
    b.a.bind_text_label(claims_ok);
    b.branch(local(OP), n(8), C::Equal, status[0]);
    b.branch(local(OP), n(1), C::Equal, detach);
    b.branch(local(OP), n(9), C::Equal, detach);
    let not_flip = b.a.create_text_label();
    let operate = b.a.create_text_label();
    b.branch(local(OP), n(4), C::NotEqual, not_flip);
    b.branch(r!(epoch), g!(epoch), C::Equal, bad);
    b.branch(job!(phase), g!(epoch), C::NotEqual, bad);
    b.a.jmp_label(operate);
    b.a.bind_text_label(not_flip);
    b.branch(r!(epoch), g!(epoch), C::Equal, operate);
    b.branch(local(OP), n(3), C::NotEqual, status[2]);
    b.branch(job!(phase), g!(epoch), C::Above, status[2]);
    b.branch(job!(phase), g!(epoch), C::Equal, status[2]);
    b.a.bind_text_label(operate);
    b.a.load_rbp_slot(R::V7, SEG);
    b.a.load_rbp_slot(R::V6, LEASE);
    b.value(sub(local(OP), n(3)));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(core);
    b.a.store_rbp_slot(KIND, R::V0);
    let accounting = b.a.create_text_label();
    for code in [0, 2, 3, 4] {
        b.branch(local(KIND), n(code), C::Equal, accounting);
    }
    b.a.jmp_label(done);
    b.a.bind_text_label(accounting);
    b.set(g!(pending), sub(g!(pending), local(STATE)));
    b.set(g!(claims), sub(g!(claims), local(USED)));
    boolean(&mut b, STATE, s!(pending));
    boolean(&mut b, USED, s!(active_serial));
    b.set(g!(pending), add(g!(pending), local(STATE)));
    b.set(g!(claims), add(g!(claims), local(USED)));
    let record_done = b.a.create_text_label();
    b.branch(local(OP), n(3), C::NotEqual, record_done);
    b.branch(local(KIND), n(0), C::NotEqual, record_done);
    b.branch(job!(phase), g!(last_phase), C::Below, record_done);
    b.set(g!(last_phase), job!(phase));
    b.a.bind_text_label(record_done);
    b.branch(local(OP), n(4), C::NotEqual, done);
    b.branch(local(KIND), n(0), C::NotEqual, done);
    b.set(r!(epoch), g!(epoch));
    b.set(g!(unflipped), sub(g!(unflipped), n(1)));
    b.a.jmp_label(done);
    b.a.bind_text_label(detach);
    let detached_page_checked = b.a.create_text_label();
    b.branch(local(OP), n(9), C::NotEqual, detached_page_checked);
    // Job storage is no longer needed on the unlink path; reuse its local
    // for the explicit page binding, leaving prev/next links untouched.
    b.set(local(LEASE), q!(phase));
    b.pointer(local(LEASE), bad);
    for (off, expected) in [
        (offset_of!(P, start), s!(start)),
        (offset_of!(P, limit), s!(limit)),
        (offset_of!(P, incarnation), s!(incarnation)),
        (offset_of!(P, generation), n(1)),
    ] {
        b.branch(field(LEASE, off, 0), expected, C::NotEqual, bad);
    }
    b.set(local(KIND), field(LEASE, offset_of!(P, state), 8));
    b.branch(local(KIND), n(1), C::Equal, status[2]);
    let closed_page = b.a.create_text_label();
    b.branch(local(KIND), n(5), C::Equal, closed_page);
    b.branch(local(KIND), n(7), C::Equal, closed_page);
    b.branch(local(KIND), n(8), C::NotEqual, bad);
    b.a.bind_text_label(closed_page);
    b.branch(
        field(LEASE, offset_of!(P, owner), 0),
        n(0),
        C::NotEqual,
        status[2],
    );
    b.branch(
        field(LEASE, offset_of!(P, metadata_leases), 0),
        n(0),
        C::NotEqual,
        status[2],
    );
    b.a.bind_text_label(detached_page_checked);
    b.branch(g!(walkers), n(0), C::NotEqual, status[2]);
    b.branch(r!(epoch), g!(epoch), C::NotEqual, status[2]);
    b.branch(s!(current_count), n(0), C::NotEqual, status[2]);
    b.branch(local(STATE), n(0), C::NotEqual, status[2]);
    b.branch(local(USED), n(0), C::NotEqual, status[2]);
    let unlink_head = b.a.create_text_label();
    let unlink_next = b.a.create_text_label();
    let unlink_done = b.a.create_text_label();
    b.branch(local(CTX), n(0), C::Equal, unlink_head);
    b.set(field(CTX, offset_of!(T, next), 0), r!(next));
    b.a.jmp_label(unlink_next);
    b.a.bind_text_label(unlink_head);
    b.set(g!(head), r!(next));
    b.a.bind_text_label(unlink_next);
    b.branch(local(PREV), n(0), C::Equal, unlink_done);
    b.set(field(PREV, offset_of!(T, previous), 0), r!(previous));
    b.a.bind_text_label(unlink_done);
    b.set(g!(members), sub(g!(members), n(1)));
    for off in [
        offset_of!(T, registry),
        offset_of!(T, identity),
        offset_of!(T, previous),
        offset_of!(T, next),
        offset_of!(T, epoch),
    ] {
        b.set(field(REG, off, 0), n(0));
    }
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(attach);
    for off in [
        offset_of!(T, registry),
        offset_of!(T, identity),
        offset_of!(T, previous),
        offset_of!(T, next),
        offset_of!(T, epoch),
    ] {
        b.branch(field(REG, off, 0), n(0), C::NotEqual, bad);
    }
    b.branch(s!(epoch), g!(epoch), C::NotEqual, bad);
    b.branch(s!(previous_epoch), g!(previous_epoch), C::NotEqual, bad);
    b.branch(s!(last_phase), g!(epoch), C::NotEqual, bad);
    for off in [
        offset_of!(S, current_count),
        offset_of!(S, pending),
        offset_of!(S, active_serial),
    ] {
        b.branch(field(SEG, off, 0), n(0), C::NotEqual, bad);
    }
    b.branch(g!(last_identity), n(u64::MAX), C::Equal, status[5]);
    b.set(local(CTX), g!(head));
    let attach_ok = b.a.create_text_label();
    b.branch(local(CTX), n(0), C::Equal, attach_ok);
    b.pointer(local(CTX), bad);
    b.branch(
        field(CTX, offset_of!(T, registry), 0),
        local(G),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(CTX, offset_of!(T, previous), 0),
        n(0),
        C::NotEqual,
        bad,
    );
    b.branch(field(CTX, offset_of!(T, identity), 0), n(0), C::Equal, bad);
    b.branch(
        field(CTX, offset_of!(T, identity), 0),
        g!(last_identity),
        C::Above,
        bad,
    );
    b.a.bind_text_label(attach_ok);
    b.set(r!(registry), local(G));
    b.set(g!(last_identity), add(g!(last_identity), n(1)));
    b.set(r!(identity), g!(last_identity));
    b.set(r!(epoch), g!(epoch));
    b.set(r!(next), g!(head));
    let attach_head = b.a.create_text_label();
    b.branch(local(CTX), n(0), C::Equal, attach_head);
    b.set(field(CTX, offset_of!(T, previous), 0), local(REG));
    b.a.bind_text_label(attach_head);
    b.set(g!(head), local(REG));
    b.set(g!(members), add(g!(members), n(1)));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(begin);
    super::remembered::phase(&mut b, q!(phase), bad);
    b.branch(and(q!(phase), n(15)), n(1), C::NotEqual, bad);
    b.branch(shr(q!(phase), 16), shr(g!(last_phase), 16), C::Below, bad);
    b.branch(shr(q!(phase), 16), shr(g!(last_phase), 16), C::Equal, bad);
    b.branch(
        and(q!(phase), n(0x1000)),
        and(g!(epoch), n(0x1000)),
        C::Equal,
        bad,
    );
    for off in [
        offset_of!(I, unflipped),
        offset_of!(I, pending),
        offset_of!(I, claims),
        offset_of!(I, walkers),
    ] {
        b.branch(field(G, off, 0), n(0), C::NotEqual, status[2]);
    }
    b.set(g!(previous_epoch), g!(epoch));
    b.set(g!(epoch), q!(phase));
    b.set(g!(last_phase), q!(phase));
    b.set(g!(unflipped), g!(members));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(check);
    for off in [
        offset_of!(I, unflipped),
        offset_of!(I, pending),
        offset_of!(I, claims),
        offset_of!(I, walkers),
    ] {
        b.branch(field(G, off, 0), n(0), C::NotEqual, status[2]);
    }
    b.a.jmp_label(status[3]);
    for (code, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.set(local(KIND), n(code as u64));
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.load_rbp_slot(R::V0, KIND);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(core);
    super::remembered::emit_remembered(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
