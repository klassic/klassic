//! Serial-bound weak processing capture; gc_weak_process_capture_locked.
use super::*;
use crate::gc_abi::{
    HandshakeState, MarkPhaseControl as Mark, MarkQueue, MetadataLeaseRequest, ReferenceLease,
    RootAudit, WeakHandle, WeakLoadGate as Gate, WeakMarkControl as Owner, WeakProcess as Process,
    WeakProcessStart as Start, WeakSlot,
};
use std::mem::size_of;
const PROCESS: i32 = 104;
const START: i32 = 112;
const OWNER: i32 = 120;
const CONTEXT: i32 = 128;
const MARK: i32 = 136;
const CHECKED: i32 = 144;
const CURRENT: i32 = 152;
const POOL: i32 = 160;

#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let stale = a.create_text_label();
    let empty = a.create_text_label();
    let full = a.create_text_label();
    let exhausted = a.create_text_label();
    let claim = a.create_text_label();
    let nonnull = a.create_text_label();
    let done = a.create_text_label();
    let metadata = a.create_text_label();
    let acquire = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(PROCESS, R::V7);
    a.store_rbp_slot(START, R::V6);
    a.store_rbp_slot(OP, R::V2);
    for (slot, offset) in [(CHECKED, 192), (CURRENT, 224)] {
        a.mov_reg_reg(R::V0, R::V5);
        a.sub_reg_imm32(R::V0, offset);
        a.store_rbp_slot(slot, R::V0);
    }
    let mut b = Emit { a };
    b.pointer(local(START), invalid);
    b.branch(local(OP), n(0), C::NotEqual, invalid);
    b.pointer(local(PROCESS), invalid);
    for off in (0..size_of::<Process>()).step_by(8) {
        b.branch(field(PROCESS, off, 0), n(0), C::NotEqual, invalid);
    }
    b.set(local(CONTEXT), field(START, offset_of!(Start, context), 0));
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
    b.set(local(G), field(START, offset_of!(Start, table), 0));
    super::weak_follow::validate_table(&mut b, invalid);
    b.set(local(OWNER), field(START, offset_of!(Start, owner), 0));
    validate_owner(
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
        invalid,
    );
    b.branch(
        field(OWNER, offset_of!(Owner, scan), 0),
        n(0),
        C::Equal,
        claim,
    );
    b.branch(
        field(OWNER, offset_of!(Owner, scan), 0),
        local(PROCESS),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(claim);
    let handle = offset_of!(Start, handle);
    b.branch(
        field(START, handle + offset_of!(WeakHandle, table), 0),
        local(G),
        C::NotEqual,
        stale,
    );
    super::weak_follow::validate_entry(
        &mut b,
        field(START, handle + offset_of!(WeakHandle, index), 0),
        field(START, handle + offset_of!(WeakHandle, serial), 0),
        stale,
    );
    b.branch(
        field(CTX, offset_of!(WeakSlot, reference), 0),
        n(0),
        C::NotEqual,
        nonnull,
    );
    for off in (0..size_of::<MetadataLeaseRequest>()).step_by(8) {
        b.branch(
            field(CTX, offset_of!(WeakSlot, lease) + off, 0),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.a.jmp_label(empty);
    b.a.bind_text_label(nonnull);
    b.set(local(POOL), field(REG, offset_of!(Gate, pool), 0));
    for off in (0..size_of::<MetadataLeaseRequest>()).step_by(8) {
        b.set(
            field(CHECKED, off, 0),
            field(CTX, offset_of!(WeakSlot, lease) + off, 0),
        );
    }
    b.a.load_rbp_slot(R::V7, POOL);
    b.a.load_rbp_slot(R::V6, CHECKED);
    b.a.mov_imm64(R::V2, 2);
    b.a.call_label(metadata);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    for off in [0, 8] {
        b.branch(
            field(CHECKED, off, 0),
            field(CTX, offset_of!(WeakSlot, lease) + off, 0),
            C::NotEqual,
            invalid,
        );
    }
    for off in (0..size_of::<ReferenceLease>()).step_by(8) {
        b.set(field(CURRENT, off, 0), n(0));
    }
    b.set(
        field(CURRENT, offset_of!(ReferenceLease, reference), 0),
        field(CTX, offset_of!(WeakSlot, reference), 0),
    );
    b.a.load_rbp_slot(R::V7, POOL);
    b.a.load_rbp_slot(R::V6, CURRENT);
    b.a.call_label(acquire);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(2), C::Equal, full);
    b.branch(local(STATE), n(3), C::Equal, exhausted);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    // Nothing below can fail: publish the captured ownership only after acquire.
    for (off, value) in [
        (offset_of!(Process, table), local(G)),
        (offset_of!(Process, owner), local(OWNER)),
        (offset_of!(Process, pool), local(POOL)),
        (offset_of!(Process, context), local(CONTEXT)),
        (
            offset_of!(Process, phase),
            field(MARK, offset_of!(Mark, phase), 0),
        ),
        (
            offset_of!(Process, epoch),
            field(MARK, offset_of!(Mark, epoch), 0),
        ),
        (
            offset_of!(Process, generation),
            field(MARK, offset_of!(Mark, generation), 0),
        ),
        (
            offset_of!(Process, rendezvous),
            field(MARK, offset_of!(Mark, rendezvous), 0),
        ),
        (offset_of!(Process, stage), n(1)),
    ] {
        b.set(field(PROCESS, off, 0), value);
    }
    for (dest, source, start, bytes) in [
        (
            offset_of!(Process, handle),
            START,
            handle,
            size_of::<WeakHandle>(),
        ),
        (offset_of!(Process, source), CTX, 0, size_of::<WeakSlot>()),
        (
            offset_of!(Process, current),
            CURRENT,
            0,
            size_of::<ReferenceLease>(),
        ),
    ] {
        for off in (0..bytes).step_by(8) {
            b.set(field(PROCESS, dest + off, 0), field(source, start + off, 0));
        }
    }
    b.set(
        field(OWNER, offset_of!(Owner, processor), 0),
        local(PROCESS),
    );
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    for (label, status) in [
        (invalid, 2),
        (stale, 8),
        (empty, 9),
        (full, 3),
        (exhausted, 4),
    ] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
    b.a.bind_text_label(acquire);
    super::reference_lease::emit_reference_lease(b.a);
    b.a.ret();
}

// Completed mark owner validation shared by capture and guarded processing.
// The caller supplies the expected gate; SEG and LEASE are scratch locals.
pub(super) fn validate_owner<A: PortableAsm>(
    b: &mut Emit<'_, A>,
    owner: i32,
    gate: i32,
    mark: i32,
    phase: E,
    invalid: A::TextLabel,
) {
    b.pointer(local(gate), invalid);
    b.pointer(local(owner), invalid);
    b.branch(
        field(owner, offset_of!(Owner, stage), 0),
        n(3),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(owner, offset_of!(Owner, gate), 0),
        local(gate),
        C::NotEqual,
        invalid,
    );
    b.set(local(mark), field(owner, offset_of!(Owner, mark), 0));
    b.pointer(local(mark), invalid);
    b.branch(
        field(gate, offset_of!(Gate, closed), 0),
        n(1),
        C::NotEqual,
        invalid,
    );
    b.set(local(SEG), field(gate, offset_of!(Gate, registry), 0));
    b.set(local(LEASE), field(gate, offset_of!(Gate, handshake), 0));
    b.pointer(local(SEG), invalid);
    b.pointer(local(LEASE), invalid);
    for (left, right) in [
        (field(mark, offset_of!(Mark, handshake), 0), local(LEASE)),
        (field(mark, offset_of!(Mark, state), 0), n(3)),
        (field(mark, offset_of!(Mark, outcome), 0), n(4)),
        (field(mark, offset_of!(Mark, phase), 0), phase),
        (
            field(SEG, offset_of!(RootRegistry, phase), 0),
            field(mark, offset_of!(Mark, phase), 0),
        ),
        (field(LEASE, offset_of!(HandshakeState, active), 0), n(0)),
        (
            field(LEASE, offset_of!(HandshakeState, generation), 0),
            field(mark, offset_of!(Mark, rendezvous), 0),
        ),
    ] {
        b.branch(left, right, C::NotEqual, invalid);
    }
    b.branch(
        field(mark, offset_of!(Mark, generation), 0),
        n(1),
        C::Above,
        invalid,
    );
    for off in [offset_of!(Mark, epoch), offset_of!(Mark, rendezvous)] {
        b.branch(field(mark, off, 0), n(0), C::Equal, invalid);
    }
    b.set(local(SEG), field(mark, offset_of!(Mark, audit), 0));
    b.pointer(local(SEG), invalid);
    b.set(local(SEG), field(SEG, offset_of!(RootAudit, queue), 0));
    b.pointer(local(SEG), invalid);
    b.branch(
        field(SEG, offset_of!(MarkQueue, epoch), 0),
        field(mark, offset_of!(Mark, epoch), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(SEG, offset_of!(MarkQueue, generation), 0),
        field(mark, offset_of!(Mark, generation), 0),
        C::NotEqual,
        invalid,
    );
}
