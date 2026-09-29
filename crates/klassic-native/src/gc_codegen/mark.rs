//! Generation-local mark stamps and finite queue ownership transactions.
//! Matching native-storage/locking contract: runtime/gc/gc_mark.h.
use super::*;
use crate::gc_abi::{MarkQueue, MarkRecord, MarkRequest, MarkTask};
macro_rules! queue {
    ($f:ident) => {
        field(G, offset_of!(MarkQueue, $f), 0)
    };
}
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(MarkRequest, $f), 0)
    };
}
macro_rules! task {
    ($f:ident) => {
        field(SEG, offset_of!(MarkTask, $f), 0)
    };
}
macro_rules! tail {
    ($f:ident) => {
        field(PREV, offset_of!(MarkTask, $f), 0)
    };
}
fn occupied() -> E {
    add(queue!(queued), queue!(in_flight))
}
fn mark_slot<A: PortableAsm>(b: &mut Emit<'_, A>) {
    b.set(
        local(SEG),
        add(queue!(tasks), shl(sub(local(STATE), n(1)), 6)),
    );
}
fn clear_request<A: PortableAsm>(b: &mut Emit<'_, A>) {
    for off in (0..56).step_by(8) {
        b.set(field(Q, off, 0), n(0));
    }
}
fn append<A: PortableAsm>(b: &mut Emit<'_, A>) {
    let empty = b.a.create_text_label();
    let done = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, empty);
    b.set(tail!(next), local(STATE));
    b.a.jmp_label(done);
    b.a.bind_text_label(empty);
    b.set(queue!(head), local(STATE));
    b.a.bind_text_label(done);
    b.set(queue!(tail), local(STATE));
    b.set(queue!(queued), add(queue!(queued), n(1)));
}
/// Checks bool(value) iff lhs < rhs, without changing any control metadata.
fn presence<A: PortableAsm>(b: &mut Emit<'_, A>, value: E, lhs: E, rhs: E, invalid: A::TextLabel) {
    let yes = b.a.create_text_label();
    let done = b.a.create_text_label();
    b.branch(lhs, rhs, C::Below, yes);
    b.branch(value.clone(), n(0), C::NotEqual, invalid);
    b.a.jmp_label(done);
    b.a.bind_text_label(yes);
    b.branch(value, n(0), C::Equal, invalid);
    b.a.bind_text_label(done);
}
/// V7=queue,V6=request,V2=operation; externally serialized, no allocation/wait.
#[allow(dead_code)]
pub(crate) fn emit_mark_step<A: PortableAsm>(a: &mut A) {
    let status: [_; 7] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let done = a.create_text_label();
    let take = a.create_text_label();
    let held = a.create_text_label();
    let finish = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(4), C::Above, invalid);
    b.pointer(queue!(tasks), invalid);
    b.branch(queue!(capacity), n(0), C::Equal, invalid);
    b.branch(queue!(capacity), n(u32::MAX as u64), C::Above, invalid);
    b.branch(
        add(queue!(tasks), shl(queue!(capacity), 6)),
        queue!(tasks),
        C::Below,
        invalid,
    );
    b.branch(queue!(epoch), n(0), C::Equal, invalid);
    b.branch(queue!(generation), n(1), C::Above, invalid);
    b.branch(queue!(queued), queue!(capacity), C::Above, invalid);
    b.branch(
        queue!(in_flight),
        sub(queue!(capacity), queue!(queued)),
        C::Above,
        invalid,
    );
    for v in [queue!(free_head), queue!(head), queue!(tail)] {
        b.branch(v, queue!(capacity), C::Above, invalid);
    }
    presence(
        &mut b,
        queue!(free_head),
        occupied(),
        queue!(capacity),
        invalid,
    );
    presence(&mut b, queue!(head), n(0), queue!(queued), invalid);
    presence(&mut b, queue!(tail), n(0), queue!(queued), invalid);
    for v in [queue!(overflow_head), queue!(overflow_tail)] {
        b.branch(and(v.clone(), n(7)), n(0), C::NotEqual, invalid);
        presence(&mut b, v, n(0), queue!(overflow_count), invalid);
    }
    b.branch(
        queue!(overflow_count),
        sub(n(u64::MAX), occupied()),
        C::Above,
        invalid,
    );
    let work = b.a.create_text_label();
    b.branch(local(OP), n(4), C::NotEqual, work);
    b.branch(
        add(occupied(), queue!(overflow_count)),
        n(0),
        C::Equal,
        status[4],
    );
    b.a.jmp_label(status[5]);
    b.a.bind_text_label(work);
    b.pointer(local(Q), invalid);
    b.set(local(PREV), n(0));
    let tail_done = b.a.create_text_label();
    let check_tail = b.a.create_text_label();
    b.branch(local(OP), n(0), C::Equal, check_tail);
    b.branch(local(OP), n(3), C::NotEqual, tail_done);
    b.a.bind_text_label(check_tail);
    b.branch(queue!(tail), n(0), C::Equal, tail_done);
    b.set(
        local(PREV),
        add(queue!(tasks), shl(sub(queue!(tail), n(1)), 6)),
    );
    b.branch(tail!(state), n(1), C::NotEqual, invalid);
    b.branch(tail!(next), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(tail_done);
    b.branch(local(OP), n(1), C::Equal, take);
    b.branch(local(OP), n(0), C::NotEqual, held);
    for v in [req!(index), req!(serial), req!(cursor)] {
        b.branch(v, n(0), C::NotEqual, invalid);
    }
    b.pointer(req!(record), invalid);
    b.branch(req!(incarnation), n(0), C::Equal, invalid);
    b.branch(req!(metadata_lease), n(0), C::Equal, invalid);
    b.branch(shr(req!(reference), 16), n(0), C::Equal, invalid);
    b.branch(
        and(req!(reference), n(0xf0000 | 0xcf)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(and(req!(reference), n(0x30)), n(0), C::Equal, invalid);
    for mask in [0x300, 0xc00, 0xf000] {
        b.set(local(USED), and(req!(reference), n(mask)));
        b.branch(local(USED), n(0), C::Equal, invalid);
        b.branch(
            and(local(USED), sub(local(USED), n(1))),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.set(local(REG), req!(record));
    b.branch(
        field(REG, offset_of!(MarkRecord, object_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.set(
        local(LEASE),
        add(local(REG), add(n(8), shl(queue!(generation), 3))),
    );
    b.branch(field(LEASE, 0, 0), queue!(epoch), C::Above, invalid);
    b.branch(field(LEASE, 0, 0), queue!(epoch), C::Equal, status[2]);
    b.branch(queue!(free_head), n(0), C::Equal, status[3]);
    b.branch(queue!(overflow_count), n(0), C::NotEqual, status[3]);
    b.branch(queue!(last_serial), n(u64::MAX), C::Equal, status[6]);
    b.set(local(STATE), queue!(free_head));
    mark_slot(&mut b);
    for v in [
        task!(state),
        task!(record),
        task!(reference),
        task!(incarnation),
        task!(metadata_lease),
        task!(cursor),
    ] {
        b.branch(v, n(0), C::NotEqual, invalid);
    }
    b.branch(task!(serial), queue!(last_serial), C::Above, invalid);
    b.branch(task!(next), queue!(capacity), C::Above, invalid);
    b.branch(task!(next), local(STATE), C::Equal, invalid);
    presence(
        &mut b,
        task!(next),
        add(occupied(), n(1)),
        queue!(capacity),
        invalid,
    );
    b.set(queue!(free_head), task!(next));
    b.set(task!(next), n(0));
    b.set(task!(state), n(1));
    b.set(queue!(last_serial), add(queue!(last_serial), n(1)));
    b.set(task!(serial), queue!(last_serial));
    b.set(task!(record), req!(record));
    b.set(task!(reference), req!(reference));
    b.set(task!(incarnation), req!(incarnation));
    b.set(task!(metadata_lease), req!(metadata_lease));
    b.set(task!(cursor), n(0));
    append(&mut b);
    b.set(field(LEASE, 0, 0), queue!(epoch));
    clear_request(&mut b);
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(take);
    for off in (0..56).step_by(8) {
        b.branch(field(Q, off, 0), n(0), C::NotEqual, invalid);
    }
    b.branch(queue!(head), n(0), C::Equal, status[4]);
    b.set(local(STATE), queue!(head));
    mark_slot(&mut b);
    b.branch(task!(state), n(1), C::NotEqual, invalid);
    for v in [
        task!(serial),
        task!(record),
        task!(reference),
        task!(incarnation),
        task!(metadata_lease),
    ] {
        b.branch(v, n(0), C::Equal, invalid);
    }
    b.branch(task!(serial), queue!(last_serial), C::Above, invalid);
    b.branch(task!(next), queue!(capacity), C::Above, invalid);
    b.branch(task!(next), local(STATE), C::Equal, invalid);
    presence(&mut b, task!(next), n(1), queue!(queued), invalid);
    let many = b.a.create_text_label();
    let head_ok = b.a.create_text_label();
    b.branch(queue!(queued), n(1), C::NotEqual, many);
    b.branch(local(STATE), queue!(tail), C::NotEqual, invalid);
    b.a.jmp_label(head_ok);
    b.a.bind_text_label(many);
    b.branch(local(STATE), queue!(tail), C::Equal, invalid);
    b.a.bind_text_label(head_ok);
    b.set(queue!(head), task!(next));
    let still_queued = b.a.create_text_label();
    b.branch(queue!(head), n(0), C::NotEqual, still_queued);
    b.set(queue!(tail), n(0));
    b.a.bind_text_label(still_queued);
    b.set(queue!(queued), sub(queue!(queued), n(1)));
    b.set(queue!(in_flight), add(queue!(in_flight), n(1)));
    b.set(task!(next), n(0));
    b.set(task!(state), n(2));
    b.set(req!(record), task!(record));
    b.set(req!(reference), task!(reference));
    b.set(req!(incarnation), task!(incarnation));
    b.set(req!(metadata_lease), task!(metadata_lease));
    b.set(req!(index), local(STATE));
    b.set(req!(serial), task!(serial));
    b.set(req!(cursor), task!(cursor));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(held);
    b.set(local(STATE), req!(index));
    b.branch(local(STATE), n(0), C::Equal, invalid);
    b.branch(local(STATE), queue!(capacity), C::Above, invalid);
    b.branch(req!(serial), n(0), C::Equal, invalid);
    b.branch(queue!(in_flight), n(0), C::Equal, invalid);
    mark_slot(&mut b);
    b.branch(task!(state), n(2), C::NotEqual, invalid);
    b.branch(task!(next), n(0), C::NotEqual, invalid);
    b.branch(task!(serial), req!(serial), C::NotEqual, invalid);
    b.branch(req!(serial), queue!(last_serial), C::Above, invalid);
    b.branch(task!(record), req!(record), C::NotEqual, invalid);
    b.branch(task!(reference), req!(reference), C::NotEqual, invalid);
    b.branch(task!(incarnation), req!(incarnation), C::NotEqual, invalid);
    b.branch(
        task!(metadata_lease),
        req!(metadata_lease),
        C::NotEqual,
        invalid,
    );
    b.branch(req!(cursor), task!(cursor), C::Below, invalid);
    b.branch(local(OP), n(2), C::Equal, finish);
    b.branch(and(req!(cursor), n(7)), n(0), C::NotEqual, invalid);
    b.branch(queue!(last_serial), n(u64::MAX), C::Equal, status[6]);
    b.set(task!(cursor), req!(cursor));
    b.set(task!(state), n(1));
    b.set(queue!(last_serial), add(queue!(last_serial), n(1)));
    b.set(task!(serial), queue!(last_serial));
    append(&mut b);
    b.set(queue!(in_flight), sub(queue!(in_flight), n(1)));
    clear_request(&mut b);
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(finish);
    b.set(queue!(in_flight), sub(queue!(in_flight), n(1)));
    b.set(task!(state), n(0));
    b.set(task!(next), queue!(free_head));
    b.set(queue!(free_head), local(STATE));
    for off in [24, 32, 40, 48, 56] {
        b.set(field(SEG, off, 0), n(0));
    }
    b.set(req!(index), n(0));
    b.set(req!(serial), n(0));
    b.a.jmp_label(status[0]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
