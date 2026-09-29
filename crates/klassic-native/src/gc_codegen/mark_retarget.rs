//! Transfer a claimed mark job to a prepared physical receiver.
use super::*;
use crate::gc_abi::{MarkQueue, MarkRecord, MarkRequest, MarkRetarget, MarkTask};
macro_rules! job {
    ($f:ident) => {
        field(LEASE, offset_of!(MarkRequest, $f), 0)
    };
}
macro_rules! dest {
    ($f:ident) => {
        field(
            Q,
            offset_of!(MarkRetarget, destination) + offset_of!(MarkRequest, $f),
            0,
        )
    };
}
macro_rules! queue {
    ($f:ident) => {
        field(G, offset_of!(MarkQueue, $f), 0)
    };
}
macro_rules! task {
    ($f:ident) => {
        field(SEG, offset_of!(MarkTask, $f), 0)
    };
}
#[allow(dead_code)]
pub(crate) fn emit_mark_retarget<A: PortableAsm>(a: &mut A) {
    const {
        assert!(std::mem::size_of::<MarkRetarget>() == 120);
        assert!(std::mem::size_of::<MarkRequest>() == 56);
        assert!(std::mem::size_of::<MarkTask>() == 64);
    }
    let invalid = a.create_text_label();
    let exhausted = a.create_text_label();
    let done = a.create_text_label();
    let core = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.mov_imm64(R::V6, 0);
    a.mov_imm64(R::V2, 4);
    a.call_label(core);
    a.cmp_reg_imm32(R::V0, 5);
    a.jcc_label(C::NotEqual, invalid);
    let mut b = Emit { a };
    b.set(local(LEASE), field(Q, offset_of!(MarkRetarget, job), 0));
    b.pointer(local(LEASE), invalid);
    for off in (0..56).step_by(8) {
        b.branch(
            field(Q, offset_of!(MarkRetarget, retired) + off, 0),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.branch(job!(index), n(0), C::Equal, invalid);
    b.branch(job!(index), queue!(capacity), C::Above, invalid);
    for v in [
        job!(serial),
        queue!(in_flight),
        job!(incarnation),
        job!(metadata_lease),
        dest!(incarnation),
        dest!(metadata_lease),
    ] {
        b.branch(v, n(0), C::Equal, invalid);
    }
    b.pointer(job!(record), invalid);
    b.branch(and(job!(cursor), n(7)), n(0), C::NotEqual, invalid);
    b.branch(dest!(record), job!(record), C::NotEqual, invalid);
    b.branch(
        dest!(metadata_lease),
        job!(metadata_lease),
        C::Equal,
        invalid,
    );
    for v in [dest!(index), dest!(serial), dest!(cursor)] {
        b.branch(v, n(0), C::NotEqual, invalid);
    }
    for v in [job!(reference), dest!(reference)] {
        b.branch(v.clone(), n(0), C::Equal, invalid);
        super::barrier::reference(&mut b, v, invalid);
    }
    b.set(
        local(SEG),
        add(queue!(tasks), shl(sub(job!(index), n(1)), 6)),
    );
    b.branch(task!(state), n(2), C::NotEqual, invalid);
    b.branch(task!(next), n(0), C::NotEqual, invalid);
    b.branch(task!(serial), job!(serial), C::NotEqual, invalid);
    b.branch(job!(serial), queue!(last_serial), C::Above, invalid);
    for off in [
        offset_of!(MarkRequest, record),
        offset_of!(MarkRequest, reference),
        offset_of!(MarkRequest, incarnation),
        offset_of!(MarkRequest, metadata_lease),
    ] {
        b.branch(
            field(SEG, 24 + off, 0),
            field(LEASE, off, 0),
            C::NotEqual,
            invalid,
        );
    }
    b.branch(job!(cursor), task!(cursor), C::Below, invalid);
    b.set(local(REG), job!(record));
    b.branch(
        field(REG, offset_of!(MarkRecord, object_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.set(
        local(NEXT),
        add(local(REG), add(n(8), shl(queue!(generation), 3))),
    );
    b.branch(field(NEXT, 0, 0), queue!(epoch), C::NotEqual, invalid);
    b.branch(queue!(last_serial), n(u64::MAX), C::Equal, exhausted);
    for off in [0, 8, 16, 24] {
        b.set(
            field(Q, offset_of!(MarkRetarget, retired) + off, 0),
            field(LEASE, off, 0),
        );
    }
    for off in [8, 16, 24] {
        b.set(
            field(LEASE, off, 0),
            field(Q, offset_of!(MarkRetarget, destination) + off, 0),
        );
        b.set(field(SEG, 24 + off, 0), field(LEASE, off, 0));
    }
    b.set(task!(cursor), job!(cursor));
    b.set(queue!(last_serial), add(queue!(last_serial), n(1)));
    b.set(task!(serial), queue!(last_serial));
    b.set(job!(serial), queue!(last_serial));
    for off in (0..56).step_by(8) {
        b.set(
            field(Q, offset_of!(MarkRetarget, destination) + off, 0),
            n(0),
        );
    }
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.jmp_label(done);
    b.a.bind_text_label(exhausted);
    b.a.mov_imm64(R::V0, 6);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(core);
    super::mark::emit_mark_step(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
