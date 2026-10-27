//! Submitted native control nodes retain region/context storage until the real
//! deletion scan, journal consumption, reader release and exclusive clearing end.
//! One step visits/protects or clears at most one cell. Other lifetime users
//! (notably native strong handles) keep their own existing retirement protocol.
use super::*;
use crate::gc_abi::{
    BufferCursor, RootLease, RootLifeRequest, RootRegistration, RootScan, RootScanJob, RootSegment,
};

#[repr(C)]
pub(super) struct Node {
    pub(super) next: u64,
    pub(super) stage: u64,
    pub(super) completed: std::sync::atomic::AtomicU64,
    pub(super) life: RootLifeRequest,
    pub(super) lease: RootLease,
    pub(super) scan: RootScanJob,
    value: u64,
    ready: u64,
    journal_remaining: u64,
}
#[repr(C)]
pub(super) struct State {
    head: u64,
    tail: u64,
    current: u64,
    queued: u64,
    pub(super) debt: u64,
    pub(super) transport: u64,
    pass_remaining: u64,
    pass_progress: u64,
    pub(super) submitted: u64,
    pub(super) processed: u64,
    pub(super) cleared: u64,
    pub(super) finished: u64,
    pub(super) blocked: u64,
}
const BASE: usize = offset_of!(Runtime, root_retirement);
const HEAD: usize = BASE + offset_of!(State, head);
const TAIL: usize = BASE + offset_of!(State, tail);
const CURRENT: usize = BASE + offset_of!(State, current);
const QUEUED: usize = BASE + offset_of!(State, queued);
const DEBT: usize = BASE + offset_of!(State, debt);
const TRANSPORT: usize = BASE + offset_of!(State, transport);
const LEFT: usize = BASE + offset_of!(State, pass_remaining);
const PROGRESS: usize = BASE + offset_of!(State, pass_progress);
const LIFE: usize = offset_of!(Node, life);
const LEASE: usize = offset_of!(Node, lease);
const SCAN: usize = offset_of!(Node, scan);
fn get<E: PortableAsm>(a: &mut E, dst: R, field: usize) {
    load_rt(a, R::V11, CURRENT);
    a.load_ptr_disp32(dst, R::V11, d(field));
}
fn put<E: PortableAsm>(a: &mut E, field: usize, src: R) {
    assert!(src != R::V11);
    load_rt(a, R::V11, CURRENT);
    a.store_ptr_disp32(R::V11, d(field), src);
}
fn address<E: PortableAsm>(a: &mut E, dst: R, field: usize) {
    load_rt(a, dst, CURRENT);
    a.add_reg_imm32(dst, d(field));
}
fn life<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    address(a, R::V6, LIFE);
    lea_rt(a, R::V7, registered_roots::REGISTRY);
    a.mov_imm64(R::V2, op);
    a.call_label(l.root_life_step);
}
fn ok<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}
fn stage<E: PortableAsm>(a: &mut E, value: u64) {
    a.mov_imm64(R::V0, value);
    put(a, offset_of!(Node, stage), R::V0);
}
fn progress<E: PortableAsm>(a: &mut E) {
    a.push_reg(R::V0);
    store_rt_imm(a, PROGRESS, 1);
    a.pop_reg(R::V0);
}

/// Existing sole-owner phase handoff holds the ticket. Drain actual deletion
/// obligations in finite slices before changing the snapshot, releasing the
/// ticket between slices exactly as journal transport does. Cooperative rounds
/// normally let the independent worker clear this debt before their end check.
pub(super) fn drain_deletions<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let again = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(again);
    load_rt(a, R::V0, DEBT);
    load_rt(a, R::V1, TRANSPORT);
    a.or_reg_reg(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.call_label(l.root_retirement_step);
    worker::request_work(a);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    if let Some(w) = &l.worker {
        a.call_label(w.hooks.relax);
    }
    worker::lock(a, l);
    a.jmp_label(again);
    a.bind_text_label(done);
}

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    producer(a, l);
    consumer(a, l);
}

/// V7 is fresh native Node storage, with only life.registration/scope filled.
/// The owner/context and Node must remain allocated through completed acquire;
/// a context additionally remains live through participant detachment.
fn producer<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.root_retire_actor);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    owner_state::bind_running(a, l);
    a.push_reg(R::V7);
    a.call_label(l.publish_pending);
    quiescence::poll(a, l);
    a.pop_reg(R::V7);
    let retry = a.create_text_label();
    let busy = a.create_text_label();
    let ready = a.create_text_label();
    a.bind_text_label(retry);
    worker::lock(a, l);
    for f in [quiescence::REQUEST, phase_round::RESUMING] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, busy);
    }
    a.jmp_label(ready);
    a.bind_text_label(busy);
    worker::unlock(a, l);
    a.call_label(l.owner_checkpoint_poll);
    a.jmp_label(retry);
    a.bind_text_label(ready);
    submit_locked(a, l);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.mov_imm64(R::V0, 0);
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();
}

/// V7 is fresh Node storage; actual Running/Detaching owner and ticket held.
/// No poll/unlock or cell traversal is introduced at this publication boundary.
pub(super) fn submit_locked<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, l.fail);
    a.mov_reg_reg(R::V0, R::V7);
    a.and_reg_imm32(R::V0, 7);
    a.jcc_label(C::NotEqual, l.fail);
    // CURRENT is exclusively borrowed only inside this ticket hold. A worker
    // never retains it across unlock; its durable state lives in queued Nodes.
    store_rt(a, CURRENT, R::V7);
    for field in (0..size_of::<Node>()).step_by(8) {
        if field != LIFE + offset_of!(RootLifeRequest, registration)
            && field != LIFE + offset_of!(RootLifeRequest, scope)
        {
            get(a, R::V0, field);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, l.fail);
        }
    }
    lea_rt(a, R::V0, RT_CTX);
    put(a, LIFE + offset_of!(RootLifeRequest, context), R::V0);
    address(a, R::V0, LEASE);
    put(a, LIFE + offset_of!(RootLifeRequest, lease), R::V0);
    life(a, l, 3);
    ok(a, l);
    journal_remaining(a, l);
    put(a, offset_of!(Node, journal_remaining), R::V0);
    load_rt(a, R::V1, TRANSPORT);
    a.add_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    store_rt(a, TRANSPORT, R::V0);
    get(a, R::V0, LIFE + offset_of!(RootLifeRequest, registration));
    a.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootRegistration, limit)));
    let empty = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    root_work::publish_retired(a, l, R::V0);
    load_rt(a, R::V1, DEBT);
    a.add_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    store_rt(a, DEBT, R::V0);
    a.bind_text_label(empty);
    stage(a, 1);
    load_rt(a, R::V0, QUEUED);
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    store_rt(a, QUEUED, R::V0);
    load_rt(a, R::V1, TAIL);
    load_rt(a, R::V0, CURRENT);
    let first = a.create_text_label();
    let linked = a.create_text_label();
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, first);
    a.store_ptr_disp32(R::V1, d(offset_of!(Node, next)), R::V0);
    a.jmp_label(linked);
    a.bind_text_label(first);
    store_rt(a, HEAD, R::V0);
    a.bind_text_label(linked);
    store_rt(a, TAIL, R::V0);
    store_rt_imm(a, CURRENT, 0);
    copy::bump(a, BASE + offset_of!(State, submitted), R::V0);
    worker::request_work(a);
}

fn consumer<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.root_retirement_step);
    let saved = [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11];
    for r in saved {
        a.push_reg(r);
    }
    let done = a.create_text_label();
    let scan = a.create_text_label();
    let cleanup = a.create_text_label();
    let clear = a.create_text_label();
    let rotate = a.create_text_label();
    let blocked = a.create_text_label();
    let finish_pass = a.create_text_label();
    load_rt(a, R::V0, HEAD);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    store_rt(a, CURRENT, R::V0);
    let pass = a.create_text_label();
    load_rt(a, R::V0, LEFT);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pass);
    load_rt(a, R::V0, QUEUED);
    store_rt(a, LEFT, R::V0);
    store_rt_imm(a, PROGRESS, 0);
    a.bind_text_label(pass);
    reconcile_journal(a, l);
    let journal_empty = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, journal_empty);
    get(a, R::V0, LIFE + offset_of!(RootLifeRequest, registration));
    a.call_label(l.worker_journal_consume_region);
    reconcile_journal(a, l);
    a.bind_text_label(journal_empty);
    get(a, R::V0, offset_of!(Node, stage));
    for (value, label) in [(2, scan), (3, cleanup), (4, clear)] {
        a.cmp_reg_imm32(R::V0, value);
        a.jcc_label(C::Equal, label);
    }
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    get(a, R::V8, LIFE + offset_of!(RootLifeRequest, registration));
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(RootRegistration, cursor)));
    a.load_ptr_disp32(R::V1, R::V8, d(offset_of!(RootRegistration, limit)));
    a.cmp_reg_reg(R::V0, R::V1);
    let claim = a.create_text_label();
    a.jcc_label(C::Below, claim);
    a.jcc_label(C::NotEqual, l.fail);
    stage(a, 3);
    progress(a);
    a.jmp_label(rotate);
    a.bind_text_label(claim);
    life(a, l, 4);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::Equal, blocked);
    ok(a, l);
    get(a, R::V8, LIFE + offset_of!(RootLifeRequest, registration));
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(RootRegistration, segment)));
    put(a, SCAN + offset_of!(RootScan, segment), R::V0);
    get(a, R::V0, LIFE + offset_of!(RootLifeRequest, begin));
    put(a, SCAN + offset_of!(RootScan, cursor), R::V0);
    address(a, R::V0, offset_of!(Node, value));
    put(a, SCAN + offset_of!(RootScan, output), R::V0);
    a.mov_imm64(R::V0, 1);
    put(a, SCAN + offset_of!(RootScan, capacity), R::V0);
    address(a, R::V0, LEASE);
    put(a, SCAN + offset_of!(RootScanJob, lease), R::V0);
    get(a, R::V0, LIFE + offset_of!(RootLifeRequest, serial));
    put(a, SCAN + offset_of!(RootScanJob, serial), R::V0);
    stage(a, 2);
    progress(a);
    a.jmp_label(rotate);

    a.bind_text_label(scan);
    let protect = a.create_text_label();
    get(a, R::V0, offset_of!(Node, ready));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, protect);
    address(a, R::V7, SCAN);
    a.call_label(l.root_scan_leased);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    get(a, R::V0, SCAN + offset_of!(RootScan, written));
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    put(a, offset_of!(Node, ready), R::V0);
    progress(a);
    a.bind_text_label(protect);
    get(a, R::V0, LIFE + offset_of!(RootLifeRequest, phase));
    load_rt(
        a,
        R::V1,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
    );
    journal_snapshot::validate(a, l);
    // A captured generation cannot close before its actual deletion debt was
    // admitted. Do not turn a completed/new mark into an old-epoch certificate.
    for (bit, active) in [(1, young_mark::ACTIVE), (2, RT_MARK_ACTIVE)] {
        let optional = a.create_text_label();
        get(a, R::V0, LIFE + offset_of!(RootLifeRequest, mask));
        a.and_reg_imm32(R::V0, bit);
        a.jcc_label(C::Equal, optional);
        load_rt(a, R::V0, active);
        a.cmp_reg_imm32(R::V0, 1);
        a.jcc_label(C::NotEqual, l.fail);
        a.bind_text_label(optional);
    }
    let admitted = a.create_text_label();
    get(a, R::V0, offset_of!(Node, value));
    fields::offset(a, R::V0, R::V2, admitted);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.add_reg_reg(R::V1, R::V2);
    a.add_reg_imm32(
        R::V1,
        d(offset_of!(ObjectHeader, state)) - size_of::<ObjectHeader>() as i32,
    );
    a.atomic_load_acquire(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, crate::gc_abi::OBJECT_PUBLISHED as i32);
    a.jcc_label(C::NotEqual, blocked);
    if let Some(w) = &l.worker {
        let young = a.create_text_label();
        load_rt(a, R::V0, RT_MARK_ACTIVE);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, young);
        a.call_label(w.mark_ref);
        store_rt_imm(a, RT_MARK_DONE, 0);
        a.bind_text_label(young);
        a.call_label(w.young.reference);
    }
    a.bind_text_label(admitted);
    root_work::ack(a, l, None);
    load_rt(a, R::V0, DEBT);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, DEBT, R::V0);
    a.mov_imm64(R::V0, 0);
    put(a, offset_of!(Node, ready), R::V0);
    copy::bump(a, BASE + offset_of!(State, processed), R::V0);
    progress(a);
    get(a, R::V0, SCAN + offset_of!(RootScan, cursor));
    get(a, R::V1, LIFE + offset_of!(RootLifeRequest, end));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, rotate);
    a.jcc_label(C::NotEqual, l.fail);
    life(a, l, 5);
    ok(a, l);
    stage(a, 1);
    a.jmp_label(rotate);

    a.bind_text_label(cleanup);
    get(a, R::V8, LIFE + offset_of!(RootLifeRequest, registration));
    // Keep the region discoverable until its real journal consumer has ACKed.
    a.load_ptr_disp32(R::V9, R::V8, d(offset_of!(RootRegistration, segment)));
    a.load_ptr_disp32(R::V9, R::V9, d(offset_of!(RootSegment, journal)));
    let no_journal = a.create_text_label();
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, no_journal);
    a.load_ptr_disp32(R::V0, R::V9, d(offset_of!(BufferCursor, consumed)));
    a.add_reg_imm32(R::V9, d(offset_of!(BufferCursor, published)));
    a.atomic_load_acquire(R::V1, R::V9);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, blocked);
    a.bind_text_label(no_journal);
    life(a, l, 6);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::Equal, blocked);
    ok(a, l);
    stage(a, 4);
    progress(a);
    a.jmp_label(rotate);
    a.bind_text_label(clear);
    let finish = a.create_text_label();
    get(a, R::V8, LIFE + offset_of!(RootLifeRequest, registration));
    a.load_ptr_disp32(
        R::V0,
        R::V8,
        d(offset_of!(RootRegistration, cleanup_cursor)),
    );
    a.load_ptr_disp32(R::V1, R::V8, d(offset_of!(RootRegistration, limit)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, finish);
    a.jcc_label(C::Above, l.fail);
    address(a, R::V7, LIFE);
    a.call_label(l.root_retired_clear_one);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    copy::bump(a, BASE + offset_of!(State, cleared), R::V1);
    progress(a);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, rotate);
    a.bind_text_label(finish);
    life(a, l, 7);
    ok(a, l);
    load_rt(a, R::V8, CURRENT);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Node, next)));
    store_rt(a, HEAD, R::V0);
    let keep_tail = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, keep_tail);
    store_rt_imm(a, TAIL, 0);
    a.bind_text_label(keep_tail);
    load_rt(a, R::V0, QUEUED);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, QUEUED, R::V0);
    copy::bump(a, BASE + offset_of!(State, finished), R::V0);
    a.mov_imm64(R::V0, 5);
    a.store_ptr_disp32(R::V8, d(offset_of!(Node, stage)), R::V0);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V8, d(offset_of!(Node, next)), R::V0);
    store_rt_imm(a, CURRENT, 0);
    // Last Node access: completion acquire permits its owner to dispose it.
    a.add_reg_imm32(R::V8, d(offset_of!(Node, completed)));
    a.mov_imm64(R::V0, 1);
    a.atomic_store_release(R::V8, R::V0);
    a.jmp_label(finish_pass);

    a.bind_text_label(blocked);
    copy::bump(a, BASE + offset_of!(State, blocked), R::V0);
    a.bind_text_label(rotate);
    load_rt(a, R::V8, CURRENT);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Node, next)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, finish_pass);
    store_rt(a, HEAD, R::V0);
    load_rt(a, R::V1, TAIL);
    a.store_ptr_disp32(R::V1, d(offset_of!(Node, next)), R::V8);
    store_rt(a, TAIL, R::V8);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V8, d(offset_of!(Node, next)), R::V0);
    a.bind_text_label(finish_pass);
    store_rt_imm(a, CURRENT, 0);
    load_rt(a, R::V0, LEFT);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, LEFT, R::V0);
    load_rt(a, R::V1, PROGRESS);
    a.or_reg_reg(R::V0, R::V1);
    let parked = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, parked);
    store_rt_imm(a, worker_journal::RETRY, 1);
    a.bind_text_label(parked);
    a.bind_text_label(done);
    for r in saved.into_iter().rev() {
        a.pop_reg(r);
    }
    a.ret();
}

/// Actual unread ring records. RETIRE excludes future producer writes; the
/// registration/segment remain retained by this Node until real FINISH.
fn journal_remaining<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let empty = a.create_text_label();
    let done = a.create_text_label();
    get(a, R::V8, LIFE + offset_of!(RootLifeRequest, registration));
    a.load_ptr_disp32(R::V9, R::V8, d(offset_of!(RootRegistration, segment)));
    a.load_ptr_disp32(R::V9, R::V9, d(offset_of!(RootSegment, journal)));
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, empty);
    a.mov_reg_reg(R::V1, R::V9);
    a.add_reg_imm32(R::V1, d(offset_of!(BufferCursor, published)));
    a.atomic_load_acquire(R::V0, R::V1);
    a.add_reg_imm32(R::V9, d(offset_of!(BufferCursor, consumed)));
    a.atomic_load_acquire(R::V1, R::V9);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    a.sub_reg_reg(R::V0, R::V1);
    a.load_ptr_disp32(
        R::V1,
        R::V9,
        d(offset_of!(BufferCursor, capacity)) - d(offset_of!(BufferCursor, consumed)),
    );
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    a.jmp_label(done);
    a.bind_text_label(empty);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(done);
}

/// Observe only real ACK movement, including ACKs by the ordinary inventory
/// consumer. Its RootWork debit already happened; never credit/debit it twice.
fn reconcile_journal<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    journal_remaining(a, l);
    get(a, R::V1, offset_of!(Node, journal_remaining));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    let unchanged = a.create_text_label();
    a.jcc_label(C::Equal, unchanged);
    a.sub_reg_reg(R::V1, R::V0);
    load_rt(a, R::V2, TRANSPORT);
    a.cmp_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Below, l.fail);
    a.sub_reg_reg(R::V2, R::V1);
    store_rt(a, TRANSPORT, R::V2);
    put(a, offset_of!(Node, journal_remaining), R::V0);
    progress(a);
    a.bind_text_label(unchanged);
}
