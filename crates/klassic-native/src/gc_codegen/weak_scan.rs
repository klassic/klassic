//! Bounded weak-table owner BEGIN/NEXT/FINISH/ABORT; gc_weak_scan_step_locked.
use super::*;
use crate::gc_abi::{
    MarkPhaseControl as Mark, MetadataLeaseRequest, ReferenceLease, WeakEntry as Entry, WeakHandle,
    WeakLoadGate as Gate, WeakMarkControl as Owner, WeakProcess as Process,
    WeakProcessStart as Capture, WeakScan as Scan, WeakScanStart as Start, WeakSlot,
    WeakTable as Table,
};
use std::mem::size_of;
const SCAN: i32 = 104;
const START: i32 = 112;
const OWNER: i32 = 120;
const CONTEXT: i32 = 128;
const MARK: i32 = 136;
const WORK: i32 = 144;
const CAPTURE: i32 = 152;
fn scan(off: usize) -> E {
    field(SCAN, off, 0)
}
fn work(off: usize) -> E {
    field(WORK, off, 0)
}
fn context<A: PortableAsm>(b: &mut Emit<'_, A>, invalid: A::TextLabel) {
    b.pointer(local(CONTEXT), invalid);
    b.branch(
        field(CONTEXT, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CONTEXT, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(CONTEXT, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CONTEXT, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
}
fn clear_work<A: PortableAsm>(b: &mut Emit<'_, A>) {
    for off in (0..size_of::<Process>()).step_by(8) {
        b.set(work(off), n(0));
    }
}
// SCAN/WORK are established. Retains G=table, REG=gate, OWNER/CONTEXT/MARK.
pub(super) fn validate<A: PortableAsm>(b: &mut Emit<'_, A>, invalid: A::TextLabel) {
    b.set(local(G), scan(offset_of!(Scan, table)));
    super::weak_follow::validate_table(b, invalid);
    b.set(local(CONTEXT), scan(offset_of!(Scan, context)));
    context(b, invalid);
    b.set(local(OWNER), scan(offset_of!(Scan, owner)));
    super::weak_capture::validate_owner(
        b,
        OWNER,
        REG,
        MARK,
        scan(offset_of!(Scan, phase)),
        invalid,
    );
    for (left, right) in [
        (field(OWNER, offset_of!(Owner, scan), 0), local(WORK)),
        (
            field(G, offset_of!(Table, entries), 0),
            scan(offset_of!(Scan, entries)),
        ),
        (
            field(G, offset_of!(Table, capacity), 0),
            scan(offset_of!(Scan, capacity)),
        ),
        (
            field(G, offset_of!(Table, last_serial), 0),
            scan(offset_of!(Scan, last_serial)),
        ),
        (
            field(MARK, offset_of!(Mark, epoch), 0),
            scan(offset_of!(Scan, epoch)),
        ),
        (
            field(MARK, offset_of!(Mark, generation), 0),
            scan(offset_of!(Scan, generation)),
        ),
        (
            field(MARK, offset_of!(Mark, rendezvous), 0),
            scan(offset_of!(Scan, rendezvous)),
        ),
        (
            field(CONTEXT, offset_of!(MutatorContext, adopted_phase), 8),
            scan(offset_of!(Scan, phase)),
        ),
    ] {
        b.branch(left, right, C::NotEqual, invalid);
    }
    b.branch(
        scan(offset_of!(Scan, cursor)),
        scan(offset_of!(Scan, capacity)),
        C::Above,
        invalid,
    );
}

#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let wait = a.create_text_label();
    let done = a.create_text_label();
    let begin = a.create_text_label();
    let finish = a.create_text_label();
    let abort = a.create_text_label();
    let fresh = a.create_text_label();
    let captured = a.create_text_label();
    let retry_slot = a.create_text_label();
    let reset_advance = a.create_text_label();
    let reset_retry = a.create_text_label();
    let advance = a.create_text_label();
    let covered = a.create_text_label();
    let active_entry = a.create_text_label();
    let aborted = a.create_text_label();
    let capture = a.create_text_label();
    let cancel = a.create_text_label();
    let results: [_; 4] = std::array::from_fn(|_| a.create_text_label());
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(SCAN, R::V7);
    a.store_rbp_slot(START, R::V6);
    a.store_rbp_slot(OP, R::V2);
    a.mov_reg_reg(R::V0, R::V5);
    a.sub_reg_imm32(R::V0, 224);
    a.store_rbp_slot(CAPTURE, R::V0);
    let mut b = Emit { a };
    b.pointer(local(SCAN), invalid);
    b.branch(local(OP), n(3), C::Above, invalid);
    b.set(
        local(WORK),
        add(local(SCAN), n(offset_of!(Scan, work) as u64)),
    );
    b.branch(local(OP), n(0), C::Equal, begin);
    b.branch(local(OP), n(3), C::Equal, abort);
    validate(&mut b, invalid);
    b.branch(local(OP), n(2), C::Equal, finish);
    b.branch(scan(offset_of!(Scan, stage)), n(1), C::NotEqual, invalid);
    b.branch(work(offset_of!(Process, stage)), n(0), C::Equal, fresh);
    b.branch(work(offset_of!(Process, stage)), n(6), C::NotEqual, wait);
    b.branch(
        field(OWNER, offset_of!(Owner, processor), 0),
        n(0),
        C::NotEqual,
        wait,
    );
    for (left, right) in [
        (work(offset_of!(Process, table)), local(G)),
        (work(offset_of!(Process, owner)), local(OWNER)),
        (
            work(offset_of!(Process, handle) + offset_of!(WeakHandle, index)),
            add(scan(offset_of!(Scan, cursor)), n(1)),
        ),
        (
            work(
                offset_of!(Process, current)
                    + offset_of!(ReferenceLease, lease)
                    + offset_of!(MetadataLeaseRequest, token),
            ),
            n(0),
        ),
        (
            work(offset_of!(Process, retired) + offset_of!(MetadataLeaseRequest, token)),
            n(0),
        ),
    ] {
        b.branch(left, right, C::NotEqual, invalid);
    }
    b.branch(
        work(offset_of!(Process, result)),
        n(8),
        C::Equal,
        retry_slot,
    );
    b.branch(
        work(offset_of!(Process, result)),
        n(6),
        C::Equal,
        reset_advance,
    );
    b.branch(
        work(offset_of!(Process, result)),
        n(7),
        C::Equal,
        reset_advance,
    );
    b.a.jmp_label(invalid);
    b.a.bind_text_label(retry_slot);
    b.branch(
        work(offset_of!(Process, handle) + offset_of!(WeakHandle, table)),
        local(G),
        C::NotEqual,
        reset_advance,
    );
    super::weak_follow::validate_entry(
        &mut b,
        work(offset_of!(Process, handle) + offset_of!(WeakHandle, index)),
        work(offset_of!(Process, handle) + offset_of!(WeakHandle, serial)),
        reset_advance,
    );
    b.branch(
        field(CTX, offset_of!(WeakSlot, reference), 0),
        n(0),
        C::NotEqual,
        reset_retry,
    );
    for off in (0..size_of::<MetadataLeaseRequest>()).step_by(8) {
        b.branch(
            field(CTX, offset_of!(WeakSlot, lease) + off, 0),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.a.bind_text_label(reset_advance);
    clear_work(&mut b);
    b.a.jmp_label(advance);
    b.a.bind_text_label(reset_retry);
    clear_work(&mut b);
    b.a.jmp_label(results[1]);

    b.a.bind_text_label(fresh);
    b.branch(
        field(OWNER, offset_of!(Owner, processor), 0),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        scan(offset_of!(Scan, cursor)),
        scan(offset_of!(Scan, capacity)),
        C::Equal,
        covered,
    );
    let cursor = scan(offset_of!(Scan, cursor));
    b.set(
        local(CTX),
        add(
            scan(offset_of!(Scan, entries)),
            sub(shl(cursor.clone(), 6), shl(cursor, 3)),
        ),
    );
    b.branch(
        field(CTX, offset_of!(WeakSlot, serial), 0),
        n(0),
        C::NotEqual,
        active_entry,
    );
    for off in [
        offset_of!(WeakSlot, gate),
        offset_of!(WeakSlot, reference),
        offset_of!(WeakSlot, lease),
        offset_of!(WeakSlot, lease) + 8,
        offset_of!(WeakSlot, lease) + 16,
    ] {
        b.branch(field(CTX, off, 0), n(0), C::NotEqual, invalid);
    }
    b.branch(
        field(CTX, offset_of!(Entry, next_free), 0),
        scan(offset_of!(Scan, capacity)),
        C::Above,
        invalid,
    );
    b.a.jmp_label(advance);
    b.a.bind_text_label(active_entry);
    for (off, value) in [
        (offset_of!(Capture, table), local(G)),
        (offset_of!(Capture, owner), local(OWNER)),
        (offset_of!(Capture, context), local(CONTEXT)),
        (
            offset_of!(Capture, handle) + offset_of!(WeakHandle, table),
            local(G),
        ),
        (
            offset_of!(Capture, handle) + offset_of!(WeakHandle, index),
            add(scan(offset_of!(Scan, cursor)), n(1)),
        ),
        (
            offset_of!(Capture, handle) + offset_of!(WeakHandle, serial),
            field(CTX, offset_of!(WeakSlot, serial), 0),
        ),
    ] {
        b.set(field(CAPTURE, off, 0), value);
    }
    b.a.load_rbp_slot(R::V7, WORK);
    b.a.load_rbp_slot(R::V6, CAPTURE);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(capture);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(9), C::Equal, advance);
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    b.a.bind_text_label(advance);
    b.set(
        scan(offset_of!(Scan, cursor)),
        add(scan(offset_of!(Scan, cursor)), n(1)),
    );
    b.a.jmp_label(results[1]);
    b.a.bind_text_label(covered);
    b.set(scan(offset_of!(Scan, stage)), n(2));
    b.a.jmp_label(results[2]);

    b.a.bind_text_label(begin);
    b.pointer(local(START), invalid);
    for off in (0..size_of::<Scan>()).step_by(8) {
        b.branch(scan(off), n(0), C::NotEqual, invalid);
    }
    b.set(local(G), field(START, offset_of!(Start, table), 0));
    super::weak_follow::validate_table(&mut b, invalid);
    b.set(local(CONTEXT), field(START, offset_of!(Start, context), 0));
    context(&mut b, invalid);
    b.set(local(OWNER), field(START, offset_of!(Start, owner), 0));
    super::weak_capture::validate_owner(
        &mut b,
        OWNER,
        REG,
        MARK,
        field(CONTEXT, offset_of!(MutatorContext, adopted_phase), 8),
        invalid,
    );
    b.branch(
        field(OWNER, offset_of!(Owner, processor), 0),
        n(0),
        C::NotEqual,
        wait,
    );
    b.branch(
        field(OWNER, offset_of!(Owner, scan), 0),
        n(0),
        C::NotEqual,
        wait,
    );
    for (off, value) in [
        (offset_of!(Scan, table), local(G)),
        (offset_of!(Scan, owner), local(OWNER)),
        (offset_of!(Scan, context), local(CONTEXT)),
        (
            offset_of!(Scan, entries),
            field(G, offset_of!(Table, entries), 0),
        ),
        (
            offset_of!(Scan, capacity),
            field(G, offset_of!(Table, capacity), 0),
        ),
        (
            offset_of!(Scan, last_serial),
            field(G, offset_of!(Table, last_serial), 0),
        ),
        (
            offset_of!(Scan, phase),
            field(MARK, offset_of!(Mark, phase), 0),
        ),
        (
            offset_of!(Scan, epoch),
            field(MARK, offset_of!(Mark, epoch), 0),
        ),
        (
            offset_of!(Scan, generation),
            field(MARK, offset_of!(Mark, generation), 0),
        ),
        (
            offset_of!(Scan, rendezvous),
            field(MARK, offset_of!(Mark, rendezvous), 0),
        ),
        (offset_of!(Scan, stage), n(1)),
    ] {
        b.set(scan(off), value);
    }
    b.set(field(OWNER, offset_of!(Owner, scan), 0), local(WORK));
    b.a.jmp_label(results[0]);

    b.a.bind_text_label(finish);
    b.branch(scan(offset_of!(Scan, stage)), n(2), C::NotEqual, invalid);
    b.branch(
        scan(offset_of!(Scan, cursor)),
        scan(offset_of!(Scan, capacity)),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(OWNER, offset_of!(Owner, processor), 0),
        n(0),
        C::NotEqual,
        invalid,
    );
    for off in (0..size_of::<Process>()).step_by(8) {
        b.branch(work(off), n(0), C::NotEqual, invalid);
    }
    b.set(field(OWNER, offset_of!(Owner, scan), 0), n(0));
    b.set(field(OWNER, offset_of!(Owner, stage), 0), n(0));
    b.set(field(REG, offset_of!(Gate, closed), 0), n(0));
    b.set(scan(offset_of!(Scan, stage)), n(3));
    b.a.jmp_label(results[2]);

    b.a.bind_text_label(abort);
    b.branch(scan(offset_of!(Scan, stage)), n(1), C::Equal, captured);
    b.branch(scan(offset_of!(Scan, stage)), n(2), C::NotEqual, invalid);
    b.a.bind_text_label(captured);
    b.set(local(OWNER), scan(offset_of!(Scan, owner)));
    b.pointer(local(OWNER), invalid);
    b.branch(
        field(OWNER, offset_of!(Owner, scan), 0),
        local(WORK),
        C::NotEqual,
        invalid,
    );
    b.set(local(CONTEXT), scan(offset_of!(Scan, context)));
    context(&mut b, invalid);
    b.branch(work(offset_of!(Process, stage)), n(0), C::Equal, aborted);
    b.branch(work(offset_of!(Process, stage)), n(6), C::Equal, aborted);
    b.a.load_rbp_slot(R::V7, WORK);
    b.a.mov_imm64(R::V6, 1);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(cancel);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(work(offset_of!(Process, stage)), n(6), C::Equal, aborted);
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    b.a.bind_text_label(aborted);
    b.branch(
        field(OWNER, offset_of!(Owner, processor), 0),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.set(field(OWNER, offset_of!(Owner, scan), 0), n(0));
    b.set(scan(offset_of!(Scan, stage)), n(4));
    b.a.jmp_label(results[3]);
    for (label, status) in [(invalid, 2), (wait, 1)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    for (label, status) in results.into_iter().zip([0, 5, 11, 12]) {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(capture);
    super::weak_capture::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(cancel);
    super::weak_finish::emit(b.a);
    b.a.ret();
}
