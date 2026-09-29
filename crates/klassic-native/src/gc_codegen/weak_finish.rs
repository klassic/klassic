//! Weak COMMIT/CANCEL and retryable metadata cleanup; gc_weak_process_finish_locked.
use super::*;
use crate::gc_abi::{
    MetadataLeaseRequest, ReferenceLease, WeakHandle, WeakMarkControl as Owner,
    WeakProcess as Process, WeakSlot,
};
use std::mem::size_of;
const PROCESS: i32 = 104;
const CONTEXT: i32 = 112;
const OWNER: i32 = 120;
const MARK: i32 = 128;
const POOL: i32 = 136;
const CHECKED: i32 = 144;
const CURRENT: i32 = 152;
const RETIRED: i32 = 160;
const SLOT_LEASE: i32 = 168;
fn process(off: usize) -> E {
    field(PROCESS, off, 0)
}
fn clear_current<A: PortableAsm>(b: &mut Emit<'_, A>) {
    for off in (0..size_of::<ReferenceLease>()).step_by(8) {
        b.set(process(offset_of!(Process, current) + off), n(0));
    }
}
fn metadata_call<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel, request: i32, op: u64) {
    b.a.load_rbp_slot(R::V7, POOL);
    b.a.load_rbp_slot(R::V6, request);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
}
fn authenticate<A: PortableAsm>(
    b: &mut Emit<'_, A>,
    label: A::TextLabel,
    source: i32,
    invalid: A::TextLabel,
) {
    for off in (0..size_of::<MetadataLeaseRequest>()).step_by(8) {
        b.set(field(CHECKED, off, 0), field(source, off, 0));
    }
    metadata_call(b, label, CHECKED, 2);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    for off in [0, 8] {
        b.branch(
            field(CHECKED, off, 0),
            field(source, off, 0),
            C::NotEqual,
            invalid,
        );
    }
}

#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let retry = a.create_text_label();
    let stage = a.create_text_label();
    let cancel = a.create_text_label();
    let stale = a.create_text_label();
    let result_ok = a.create_text_label();
    let dead = a.create_text_label();
    let committed = a.create_text_label();
    let cleanup = a.create_text_label();
    let clean_current = a.create_text_label();
    let complete = a.create_text_label();
    let done = a.create_text_label();
    let metadata = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 96);
    a.store_rbp_slot(PROCESS, R::V7);
    a.store_rbp_slot(OP, R::V6);
    a.store_rbp_slot(STATE, R::V2);
    a.mov_reg_reg(R::V0, R::V5);
    a.sub_reg_imm32(R::V0, 200);
    a.store_rbp_slot(CHECKED, R::V0);
    let mut b = Emit { a };
    b.pointer(local(PROCESS), invalid);
    b.branch(local(OP), n(1), C::Above, invalid);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.branch(process(offset_of!(Process, stage)), n(4), C::Equal, stage);
    b.branch(process(offset_of!(Process, stage)), n(5), C::Equal, stage);
    b.branch(local(OP), n(1), C::NotEqual, invalid);
    b.branch(
        process(offset_of!(Process, stage)),
        n(1),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(stage);
    b.set(local(CONTEXT), process(offset_of!(Process, context)));
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
    b.set(local(POOL), process(offset_of!(Process, pool)));
    b.set(local(OWNER), process(offset_of!(Process, owner)));
    b.pointer(local(OWNER), invalid);
    b.set(
        local(CURRENT),
        add(
            local(PROCESS),
            n((offset_of!(Process, current) + offset_of!(ReferenceLease, lease)) as u64),
        ),
    );
    b.set(
        local(RETIRED),
        add(local(PROCESS), n(offset_of!(Process, retired) as u64)),
    );
    b.branch(process(offset_of!(Process, stage)), n(5), C::Equal, cleanup);
    b.branch(local(OP), n(1), C::Equal, cancel);
    super::weak_access::validate_process_owner(&mut b, PROCESS, CONTEXT, OWNER, MARK, invalid);
    b.branch(
        process(offset_of!(Process, result)),
        n(6),
        C::Equal,
        result_ok,
    );
    b.branch(
        process(offset_of!(Process, result)),
        n(7),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(result_ok);
    authenticate(&mut b, metadata, CURRENT, invalid);
    // A released/reused slot is stale work, not authority to touch its replacement.
    b.set(local(G), process(offset_of!(Process, table)));
    super::weak_follow::validate_table(&mut b, stale);
    let handle = offset_of!(Process, handle);
    b.branch(
        process(handle + offset_of!(WeakHandle, table)),
        local(G),
        C::NotEqual,
        stale,
    );
    super::weak_follow::validate_entry(
        &mut b,
        process(handle + offset_of!(WeakHandle, index)),
        process(handle + offset_of!(WeakHandle, serial)),
        stale,
    );
    for off in (0..size_of::<WeakSlot>()).step_by(8) {
        b.branch(
            field(CTX, off, 0),
            process(offset_of!(Process, source) + off),
            C::NotEqual,
            stale,
        );
    }
    b.set(
        local(SLOT_LEASE),
        add(local(CTX), n(offset_of!(WeakSlot, lease) as u64)),
    );
    authenticate(&mut b, metadata, SLOT_LEASE, invalid);
    metadata_call(&mut b, metadata, SLOT_LEASE, 1);
    b.branch(local(STATE), n(0), C::NotEqual, retry);
    b.branch(process(offset_of!(Process, result)), n(6), C::Equal, dead);
    for off in (0..size_of::<ReferenceLease>()).step_by(8) {
        b.set(
            field(CTX, offset_of!(WeakSlot, reference) + off, 0),
            process(offset_of!(Process, current) + off),
        );
    }
    clear_current(&mut b);
    b.a.jmp_label(committed);
    b.a.bind_text_label(dead);
    b.set(field(CTX, offset_of!(WeakSlot, reference), 0), n(0));
    b.a.bind_text_label(committed);
    b.set(process(offset_of!(Process, stage)), n(5));
    b.a.jmp_label(cleanup);
    b.a.bind_text_label(stale);
    b.a.bind_text_label(cancel);
    b.set(process(offset_of!(Process, result)), n(8));
    b.set(process(offset_of!(Process, stage)), n(5));
    // This path deliberately ignores stale epoch/table state. Only the unique
    // processor claim and private lease obligations govern cleanup completion.
    b.a.bind_text_label(cleanup);
    b.branch(
        field(OWNER, offset_of!(Owner, processor), 0),
        local(PROCESS),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(RETIRED, offset_of!(MetadataLeaseRequest, token), 0),
        n(0),
        C::Equal,
        clean_current,
    );
    metadata_call(&mut b, metadata, RETIRED, 1);
    b.branch(local(STATE), n(0), C::NotEqual, retry);
    b.a.bind_text_label(clean_current);
    b.branch(
        field(CURRENT, offset_of!(MetadataLeaseRequest, token), 0),
        n(0),
        C::Equal,
        complete,
    );
    metadata_call(&mut b, metadata, CURRENT, 1);
    b.branch(local(STATE), n(0), C::NotEqual, retry);
    b.a.bind_text_label(complete);
    clear_current(&mut b);
    b.set(process(offset_of!(Process, stage)), n(6));
    b.set(field(OWNER, offset_of!(Owner, processor), 0), n(0));
    b.value(process(offset_of!(Process, result)));
    b.a.jmp_label(done);
    for (label, status) in [(invalid, 2), (retry, 10)] {
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
}
