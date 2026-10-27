//! Actual copier requests over precommitted private pin destinations. This
//! inventory identifies logical objects; it never substitutes a mark frontier
//! or a source-completion certificate. Each initializer owns one real CAS state.
use super::*;
const CAPACITY: usize = 128;
#[repr(C)]
struct Job {
    identity: u64,
    token: u64,
    page: u64,
    incarnation: u64,
    stage: u64, // 1 armed, 2 initializing, 3 forwarding winner, 4 loser
}
#[repr(C)]
pub(super) struct State {
    active: u64,
    owner_wins: u64,
    worker_wins: u64,
    losses: u64,
    jobs: [Job; CAPACITY],
}
const BASE: usize = offset_of!(Runtime, pin_copies);
const ACTIVE: usize = BASE + offset_of!(State, active);
pub(super) const OWNER_WINS: usize = BASE + offset_of!(State, owner_wins);
pub(super) const WORKER_WINS: usize = BASE + offset_of!(State, worker_wins);
pub(super) const LOSSES: usize = BASE + offset_of!(State, losses);
const JOBS: usize = BASE + offset_of!(State, jobs);
const TOKEN: i32 = offset_of!(Job, token) as i32;
const PAGE: i32 = offset_of!(Job, page) as i32;
const INC: i32 = offset_of!(Job, incarnation) as i32;
const STAGE: i32 = offset_of!(Job, stage) as i32;

/// Preserve caller registers. The job slot is nonzero only after this copier
/// exclusively initializes an actual precommitted destination. Competitors
/// proceed through their ordinary private candidates rather than waiting while
/// holding a ticket needed by this initializer.
pub(super) fn prepare<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    source: usize,
    destination: usize,
    descriptor: usize,
    job: usize,
) {
    quiescence::save(a);
    store_rt_imm(a, job, 0);
    let done = a.create_text_label();
    let next = a.create_text_label();
    let scan = a.create_text_label();
    let changed = a.create_text_label();
    load_rt(a, R::V0, RT_FULL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    lea_rt(a, R::V1, ACTIVE);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, source);
    a.load_ptr_disp32(R::V0, R::V0, copy::MARK_RECORD);
    a.load_ptr_disp32(R::V10, R::V0, d(offset_of!(MarkRecord, object_id)));
    lea_rt(a, R::V8, JOBS);
    a.mov_imm64(R::V11, CAPACITY as u64);
    a.bind_text_label(scan);
    a.atomic_load_acquire(R::V0, R::V8);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, next);
    a.mov_reg_reg(R::V7, R::V8);
    a.add_reg_imm32(R::V7, STAGE);
    a.mov_imm64(R::V0, 1);
    a.mov_imm64(R::V1, 2);
    a.atomic_compare_exchange(R::V7, R::V1);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, next);
    // A cancelled slot may have been reused between our first ID read and
    // the CAS. Revalidate its published identity before borrowing its token.
    a.atomic_load_acquire(R::V0, R::V8);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, changed);
    store_rt(a, job, R::V8);
    a.load_ptr_disp32(R::V0, R::V8, PAGE);
    a.sub_reg_imm8(R::V0, 1);
    scan::page_ptr(a, R::V9, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V9, d(offset_of!(AllocationPage, incarnation)));
    a.load_ptr_disp32(R::V1, R::V8, INC);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, R::V8, PAGE);
    large_object::record(a, R::V0);
    a.load_ptr_disp32(R::V0, R::V8, pin_destination::RESERVATION);
    load_rt(a, R::V9, job);
    a.load_ptr_disp32(R::V1, R::V9, TOKEN);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V8, R::V9, PAGE);
    a.shl_reg_imm8(R::V8, 17);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.add_reg_reg(R::V8, R::V1); // actual private destination header
    load_rt(a, R::V6, source);
    a.load_ptr_disp32(R::V1, R::V6, copy::PAYLOAD);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, payload_bytes)), R::V1);
    a.mov_imm64(R::V2, crate::gc_abi::OBJECT_INITIALIZING);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, state)), R::V2);
    a.mov_reg_reg(R::V7, R::V8);
    a.add_reg_imm32(R::V7, copy::HEADER);
    a.mov_reg_reg(R::V0, R::V7);
    encode(a, R::V0, R::V2);
    store_rt(a, destination, R::V0);
    a.rep_movsb();
    load_rt(a, R::V8, destination);
    decode(a, R::V8, R::V1);
    a.sub_reg_imm32(R::V8, copy::HEADER);
    identity::copy_prepared_record(a, l, source);
    load_rt(a, R::V2, descriptor);
    a.mov_reg_reg(R::V9, R::V8);
    a.add_reg_imm32(R::V9, d(offset_of!(ObjectHeader, descriptor_id)));
    a.atomic_store_release(R::V9, R::V2);
    a.mov_reg_reg(R::V9, R::V8);
    a.add_reg_imm32(R::V9, d(offset_of!(ObjectHeader, state)));
    a.mov_imm64(R::V2, crate::gc_abi::OBJECT_PUBLISHED);
    a.atomic_store_release(R::V9, R::V2);
    slot_owner::publish(a);
    a.jmp_label(done);
    a.bind_text_label(changed);
    a.mov_imm64(R::V0, 1);
    a.atomic_store_release(R::V7, R::V0);
    a.bind_text_label(next);
    a.add_reg_imm32(R::V8, d(size_of::<Job>()));
    a.sub_reg_imm8(R::V11, 1);
    a.jcc_label(C::NotEqual, scan);
    a.bind_text_label(done);
    quiescence::restore(a);
}

/// Called only after the real source CAS. A winner enters the actual Large
/// retain/rescan frontier before releasing its copy obligation. Lost private
/// candidates remain reservation-owned and may be reinitialized after closure.
pub(super) fn outcome<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    job: usize,
    destination: usize,
    worker: bool,
    won: bool,
) {
    quiescence::save(a);
    let done = a.create_text_label();
    load_rt(a, R::V8, job);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, done);
    if won {
        let locked = a.create_text_label();
        if !worker {
            load_rt(a, R::V0, root_journal::OWNER_LOCKED);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, locked);
            worker::lock(a, l);
            a.mov_imm64(R::V0, 1);
            a.push_reg(R::V0);
            a.push_reg(R::V0);
        } else {
            a.jmp_label(locked);
        }
        let retained = a.create_text_label();
        if !worker {
            a.jmp_label(retained);
        }
        a.bind_text_label(locked);
        a.mov_imm64(R::V0, 0);
        a.push_reg(R::V0);
        a.push_reg(R::V0);
        a.bind_text_label(retained);
        load_rt(a, R::V0, destination);
        a.mov_reg_reg(R::V1, R::V0);
        a.shr_reg_imm8(R::V1, 16);
        large_trace::retain(a, l);
        copy::bump(a, if worker { WORKER_WINS } else { OWNER_WINS }, R::V0);
        load_rt(a, R::V8, job);
        a.add_reg_imm32(R::V8, STAGE);
        a.mov_imm64(R::V0, 3);
        a.atomic_store_release(R::V8, R::V0);
        a.pop_reg(R::V0);
        a.pop_reg(R::V0);
        a.test_reg_reg(R::V0, R::V0);
        let no_unlock = a.create_text_label();
        a.jcc_label(C::Equal, no_unlock);
        worker::unlock(a, l);
        worker::wake_if_pending(a, l);
        a.bind_text_label(no_unlock);
    } else {
        a.mov_imm64(R::V0, 1);
        copy::add_bytes(a, LOSSES, R::V0);
        a.add_reg_imm32(R::V8, STAGE);
        a.mov_imm64(R::V0, 4);
        a.atomic_store_release(R::V8, R::V0);
    }
    a.bind_text_label(done);
    store_rt_imm(a, job, 0);
    quiescence::restore(a);
}

/// Actual resource cancellation or transfer of a winning candidate to managed
/// Large ownership. Returns the winner flag in the actor's private scratch.
pub(super) fn before_cancel<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    pending: E::TextLabel,
) {
    quiescence::save(a);
    store_rt_imm(a, pin_destination::COPIED, 0);
    let scan = a.create_text_label();
    let next = a.create_text_label();
    let blocked = a.create_text_label();
    let done = a.create_text_label();
    let won = a.create_text_label();
    let release = a.create_text_label();
    lea_rt(a, R::V8, JOBS);
    load_rt(a, R::V10, pin_destination::CALLER);
    a.mov_imm64(R::V11, CAPACITY as u64);
    a.bind_text_label(scan);
    a.atomic_load_acquire(R::V0, R::V8);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, next);
    a.load_ptr_disp32(R::V0, R::V8, TOKEN);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, next);
    a.mov_reg_reg(R::V7, R::V8);
    a.add_reg_imm32(R::V7, STAGE);
    a.atomic_load_acquire(R::V0, R::V7);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, blocked);
    a.cmp_reg_imm8(R::V0, 3);
    a.jcc_label(C::Equal, won);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::Equal, release);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::NotEqual, l.fail);
    a.jmp_label(release);
    a.bind_text_label(won);
    store_rt_imm(a, pin_destination::COPIED, 1);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(release);
    a.mov_reg_reg(R::V9, R::V0);
    a.mov_imm64(R::V1, 0);
    a.atomic_compare_exchange(R::V7, R::V1);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, blocked);
    a.mov_imm64(R::V0, 0);
    a.atomic_store_release(R::V8, R::V0);
    for field in [TOKEN, PAGE, INC] {
        a.store_ptr_disp32(R::V8, field, R::V0);
    }
    lea_rt(a, R::V1, ACTIVE);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    a.atomic_store_release(R::V1, R::V0);
    a.jmp_label(done);
    a.bind_text_label(next);
    a.add_reg_imm32(R::V8, d(size_of::<Job>()));
    a.sub_reg_imm8(R::V11, 1);
    a.jcc_label(C::NotEqual, scan);
    a.bind_text_label(done);
    quiescence::restore(a);
    let returned = a.create_text_label();
    a.jmp_label(returned);
    a.bind_text_label(blocked);
    quiescence::restore(a);
    a.jmp_label(pending);
    a.bind_text_label(returned);
}

pub(super) fn emit_watch<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    use super::pin_destination as p;
    p::begin(a, l, l.pin_destination_watch_actor); // actual source RootCell, owned token
    a.call_label(l.root_load_actor);
    let resolved = a.create_text_label();
    a.jmp_label(resolved);
    p::begin_resolved(a, l, l.pin_destination_watch_resolved);
    a.bind_text_label(resolved);
    let invalid_unlocked = a.create_text_label();
    let invalid = a.create_text_label();
    let busy = a.create_text_label();
    let resource = a.create_text_label();
    let already = a.create_text_label();
    let success = a.create_text_label();
    let finish = a.create_text_label();
    let unlocked_finish = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid_unlocked);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.mov_reg_reg(R::V2, R::V0);
    a.sub_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Below, invalid_unlocked);
    a.cmp_reg_imm32(R::V2, geometry(a).heap_bytes as i32);
    a.jcc_label(C::AboveOrEqual, invalid_unlocked);
    a.load_ptr_disp32(R::V1, R::V0, copy::MARK_RECORD);
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(MarkRecord, object_id)));
    store_rt(a, p::SOURCE_ID, R::V1);
    a.load_ptr_disp32(R::V1, R::V0, copy::PAYLOAD);
    a.add_reg_imm32(R::V1, 15);
    a.and_reg_imm32(R::V1, -16);
    let sized = a.create_text_label();
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V1, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V1, copy::HEADER);
    store_rt(a, p::PAGE_COUNT, R::V1); // required bytes, not a published page count
    store_rt_imm(a, p::COPIED, 0);
    generation::test_role(a, R::V2, generation::LARGE);
    let movable = a.create_text_label();
    a.jcc_label(C::NotEqual, movable);
    store_rt_imm(a, p::COPIED, 1);
    a.bind_text_label(movable);
    worker::lock(a, l);
    p::authenticate(a, l, invalid);
    load_rt(a, R::V0, p::COPIED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, already);
    a.shl_reg_imm8(R::V1, 17);
    load_rt(a, R::V0, p::PAGE_COUNT);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, invalid);
    store_rt_imm(a, p::JOB, 0);
    lea_rt(a, R::V8, JOBS);
    a.mov_imm64(R::V11, CAPACITY as u64);
    let scan = a.create_text_label();
    let next = a.create_text_label();
    let occupied = a.create_text_label();
    let available = a.create_text_label();
    let rearm = a.create_text_label();
    a.bind_text_label(scan);
    a.atomic_load_acquire(R::V0, R::V8);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, occupied);
    load_rt(a, R::V0, p::JOB);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, next);
    store_rt(a, p::JOB, R::V8);
    a.jmp_label(next);
    a.bind_text_label(occupied);
    load_rt(a, R::V1, p::SOURCE_ID);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, next);
    a.load_ptr_disp32(R::V0, R::V8, TOKEN);
    load_rt(a, R::V1, p::CALLER);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, busy);
    a.load_ptr_disp32(R::V0, R::V8, STAGE);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::Equal, success);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::Equal, rearm);
    a.jmp_label(busy);
    a.bind_text_label(next);
    a.add_reg_imm32(R::V8, d(size_of::<Job>()));
    a.sub_reg_imm8(R::V11, 1);
    a.jcc_label(C::NotEqual, scan);
    load_rt(a, R::V8, p::JOB);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, resource);
    a.jmp_label(available);
    a.bind_text_label(rearm);
    a.add_reg_imm32(R::V8, STAGE);
    a.mov_imm64(R::V0, 1);
    a.atomic_store_release(R::V8, R::V0);
    a.jmp_label(success);
    a.bind_text_label(available);
    load_rt(a, R::V7, p::CALLER);
    a.store_ptr_disp32(R::V8, TOKEN, R::V7);
    load_rt(a, R::V0, p::PAGE);
    a.store_ptr_disp32(R::V8, PAGE, R::V0);
    a.load_ptr_disp32(R::V0, R::V7, 16);
    a.store_ptr_disp32(R::V8, INC, R::V0);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V8, STAGE, R::V0);
    load_rt(a, R::V0, p::SOURCE_ID);
    a.atomic_store_release(R::V8, R::V0);
    lea_rt(a, R::V1, ACTIVE);
    a.atomic_load_acquire(R::V0, R::V1);
    a.add_reg_imm32(R::V0, 1);
    a.atomic_store_release(R::V1, R::V0);
    a.bind_text_label(success);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(finish);
    a.bind_text_label(already);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(finish);
    a.bind_text_label(busy);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(finish);
    a.bind_text_label(resource);
    a.mov_imm64(R::V0, 6);
    a.jmp_label(finish);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(finish);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.jmp_label(unlocked_finish);
    a.bind_text_label(invalid_unlocked);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(unlocked_finish);
    p::finish(a);
}

pub(super) fn slot(worker: bool) -> usize {
    if worker {
        offset_of!(Runtime, w_pin_job)
    } else {
        owner_state::offset(offset_of!(OwnerState, pin_copy_job))
    }
}
/// Fast inactive branch keeps ordinary copies out of the full register save.
pub(super) fn before_copy<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    source: usize,
    destination: usize,
    descriptor: usize,
    worker: bool,
    prepared: E::TextLabel,
) {
    let done = a.create_text_label();
    let job = slot(worker);
    a.mov_imm64(R::V2, 0);
    store_rt(a, job, R::V2);
    lea_rt(a, R::V2, ACTIVE);
    a.atomic_load_acquire(R::V2, R::V2);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, done);
    prepare(a, l, source, destination, descriptor, job);
    load_rt(a, R::V2, job);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, prepared);
    a.bind_text_label(done);
}
