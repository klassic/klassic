//! Constant-work overflow admission/refill using prepared logical mark records.
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
macro_rules! spill {
    ($f:ident) => {
        field(REG, offset_of!(MarkTask, $f), 0)
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
fn cell(record: E) -> E {
    add(
        record,
        add(
            n(offset_of!(MarkRecord, young_spill) as u64),
            shl(queue!(generation), 6),
        ),
    )
}
const _: () = assert!(
    std::mem::size_of::<MarkTask>() == 64
        && offset_of!(MarkRecord, old_spill) == offset_of!(MarkRecord, young_spill) + 64
);
/// V7=queue,V6=request,V2=OFFER_OR_SPILL(0)/REFILL(1). Same queue lock as core.
#[allow(dead_code)]
pub(crate) fn emit_mark_spill<A: PortableAsm>(a: &mut A) {
    let status: [_; 7] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let core = a.create_text_label();
    let refill = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(1), C::Above, invalid);
    b.branch(local(OP), n(1), C::Equal, refill);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(core);
    b.a.cmp_reg_imm32(R::V0, 3);
    b.a.jcc_label(C::NotEqual, done);
    b.branch(queue!(last_serial), n(u64::MAX), C::Equal, status[6]);
    b.branch(
        queue!(overflow_count),
        sub(sub(n(u64::MAX), queue!(queued)), queue!(in_flight)),
        C::Equal,
        status[6],
    );
    b.set(local(REG), cell(req!(record)));
    for off in (0..64).step_by(8) {
        b.branch(field(REG, off, 0), n(0), C::NotEqual, invalid);
    }
    b.set(local(PREV), n(0));
    let tail_ok = b.a.create_text_label();
    b.branch(queue!(overflow_tail), n(0), C::Equal, tail_ok);
    b.set(local(PREV), cell(queue!(overflow_tail)));
    b.branch(tail!(state), n(1), C::NotEqual, invalid);
    b.branch(tail!(next), n(0), C::NotEqual, invalid);
    b.branch(tail!(record), queue!(overflow_tail), C::NotEqual, invalid);
    b.a.bind_text_label(tail_ok);
    b.set(queue!(last_serial), add(queue!(last_serial), n(1)));
    b.set(spill!(serial), queue!(last_serial));
    b.set(spill!(state), n(1));
    b.set(spill!(record), req!(record));
    b.set(spill!(reference), req!(reference));
    b.set(spill!(incarnation), req!(incarnation));
    b.set(spill!(metadata_lease), req!(metadata_lease));
    let empty = b.a.create_text_label();
    let appended = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, empty);
    b.set(tail!(next), req!(record));
    b.a.jmp_label(appended);
    b.a.bind_text_label(empty);
    b.set(queue!(overflow_head), req!(record));
    b.a.bind_text_label(appended);
    b.set(queue!(overflow_tail), req!(record));
    b.set(queue!(overflow_count), add(queue!(overflow_count), n(1)));
    b.set(
        local(LEASE),
        add(
            req!(record),
            add(
                n(offset_of!(MarkRecord, young_epoch) as u64),
                shl(queue!(generation), 3),
            ),
        ),
    );
    b.set(field(LEASE, 0, 0), queue!(epoch));
    for off in (0..56).step_by(8) {
        b.set(field(Q, off, 0), n(0));
    }
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(refill);
    b.a.mov_imm64(R::V2, 4);
    b.a.call_label(core);
    b.a.cmp_reg_imm32(R::V0, 1);
    b.a.jcc_label(C::Equal, done);
    b.branch(queue!(overflow_count), n(0), C::Equal, status[4]);
    b.branch(queue!(free_head), n(0), C::Equal, status[3]);
    b.set(local(STATE), queue!(free_head));
    b.set(
        local(SEG),
        add(queue!(tasks), shl(sub(local(STATE), n(1)), 6)),
    );
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
    let more = b.a.create_text_label();
    let free_ok = b.a.create_text_label();
    b.branch(
        add(add(queue!(queued), queue!(in_flight)), n(1)),
        queue!(capacity),
        C::Below,
        more,
    );
    b.branch(task!(next), n(0), C::NotEqual, invalid);
    b.a.jmp_label(free_ok);
    b.a.bind_text_label(more);
    b.branch(task!(next), n(0), C::Equal, invalid);
    b.a.bind_text_label(free_ok);
    b.set(local(PREV), n(0));
    let normal_tail_ok = b.a.create_text_label();
    b.branch(queue!(tail), n(0), C::Equal, normal_tail_ok);
    b.set(
        local(PREV),
        add(queue!(tasks), shl(sub(queue!(tail), n(1)), 6)),
    );
    b.branch(tail!(state), n(1), C::NotEqual, invalid);
    b.branch(tail!(next), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(normal_tail_ok);
    b.set(local(REG), cell(queue!(overflow_head)));
    b.set(
        local(LEASE),
        add(
            queue!(overflow_head),
            add(
                n(offset_of!(MarkRecord, young_epoch) as u64),
                shl(queue!(generation), 3),
            ),
        ),
    );
    b.branch(spill!(state), n(1), C::NotEqual, invalid);
    b.branch(spill!(serial), n(0), C::Equal, invalid);
    b.branch(spill!(serial), queue!(last_serial), C::Above, invalid);
    b.branch(spill!(record), queue!(overflow_head), C::NotEqual, invalid);
    for v in [
        spill!(reference),
        spill!(incarnation),
        spill!(metadata_lease),
    ] {
        b.branch(v, n(0), C::Equal, invalid);
    }
    b.branch(spill!(cursor), n(0), C::NotEqual, invalid);
    b.branch(and(spill!(next), n(7)), n(0), C::NotEqual, invalid);
    b.branch(spill!(next), queue!(overflow_head), C::Equal, invalid);
    let many = b.a.create_text_label();
    let head_ok = b.a.create_text_label();
    b.branch(queue!(overflow_count), n(1), C::NotEqual, many);
    b.branch(spill!(next), n(0), C::NotEqual, invalid);
    b.branch(
        queue!(overflow_head),
        queue!(overflow_tail),
        C::NotEqual,
        invalid,
    );
    b.a.jmp_label(head_ok);
    b.a.bind_text_label(many);
    b.branch(spill!(next), n(0), C::Equal, invalid);
    b.branch(
        queue!(overflow_head),
        queue!(overflow_tail),
        C::Equal,
        invalid,
    );
    b.a.bind_text_label(head_ok);
    b.branch(field(LEASE, 0, 0), queue!(epoch), C::NotEqual, invalid);
    b.set(queue!(free_head), task!(next));
    b.set(queue!(overflow_head), spill!(next));
    let remain = b.a.create_text_label();
    b.branch(queue!(overflow_head), n(0), C::NotEqual, remain);
    b.set(queue!(overflow_tail), n(0));
    b.a.bind_text_label(remain);
    b.set(queue!(overflow_count), sub(queue!(overflow_count), n(1)));
    for off in (0..64).step_by(8) {
        b.set(field(SEG, off, 0), field(REG, off, 0));
        b.set(field(REG, off, 0), n(0));
    }
    b.set(task!(next), n(0));
    let normal_empty = b.a.create_text_label();
    let normal_done = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, normal_empty);
    b.set(tail!(next), local(STATE));
    b.a.jmp_label(normal_done);
    b.a.bind_text_label(normal_empty);
    b.set(queue!(head), local(STATE));
    b.a.bind_text_label(normal_done);
    b.set(queue!(tail), local(STATE));
    b.set(queue!(queued), add(queue!(queued), n(1)));
    b.a.jmp_label(status[0]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(core);
    super::mark::emit_mark_step(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
