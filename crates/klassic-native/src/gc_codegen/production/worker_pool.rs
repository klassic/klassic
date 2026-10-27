//! Two real GC workers with stable identities and independently owned OS
//! resources. Tracing cursors and copy scratch remain shared under the fair
//! collector ticket: this stage does not introduce parallel tracing/stealing.
use super::*;

pub(super) const CAPACITY: usize = 2;
pub(super) const STARTING: u64 = 1;
pub(super) const RUNNING: u64 = 2;
pub(super) const STOPPED: u64 = 3;
pub(super) const JOINED: u64 = 4;

#[repr(C)]
pub(super) struct Slot {
    // OwnerContext addresses this prefix during every worker operation. It is
    // never registered as a mutator and owns no language roots or TLAB.
    pub(super) view: OwnerState,
    pub(super) epoch: worker_view::Epoch,
    pub(super) linux: super::super::linux_production::State,
    pub(super) windows_event: u64,
    pub(super) windows_thread: u64,
    pub(super) macos: [u64; 17],
    pub(super) identity: u64,
    state: u64,
    old_units: u64,
    young_units: u64,
    relocate_units: u64,
}

#[repr(C)]
pub(super) struct State {
    live: u64,
    launched: u64,
    stopped: u64,
    stop: u64,
    active: u64,
    entered: u64,
    completed: u64,
    slots: [Slot; CAPACITY],
}
const BASE: usize = offset_of!(Runtime, worker_pool);
pub(super) const LIVE: usize = BASE + offset_of!(State, live);
pub(super) const LAUNCHED: usize = BASE + offset_of!(State, launched);
pub(super) const STOPPED_COUNT: usize = BASE + offset_of!(State, stopped);
pub(super) const STOP: usize = BASE + offset_of!(State, stop);
pub(super) const ACTIVE: usize = BASE + offset_of!(State, active);
pub(super) const ENTERED: usize = BASE + offset_of!(State, entered);
pub(super) const COMPLETED: usize = BASE + offset_of!(State, completed);
pub(super) const STATUS: usize = owner_state::offset(offset_of!(Slot, state));
pub(super) const IDENTITY: usize = owner_state::offset(offset_of!(Slot, identity));
pub(super) const OLD_UNITS: usize = owner_state::offset(offset_of!(Slot, old_units));
pub(super) const YOUNG_UNITS: usize = owner_state::offset(offset_of!(Slot, young_units));
pub(super) const RELOCATE_UNITS: usize = owner_state::offset(offset_of!(Slot, relocate_units));

pub(super) const fn slot(index: usize) -> usize {
    assert!(index < CAPACITY);
    BASE + offset_of!(State, slots) + index * size_of::<Slot>()
}
#[cfg(test)]
pub(super) const fn field(index: usize, private: usize) -> usize {
    slot(index) + owner_state::raw(private)
}

pub(super) fn increment<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, field: usize) {
    load_rt(a, R::V0, field);
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    store_rt(a, field, R::V0);
}

/// All lifecycle publications are visible to independently running callbacks.
fn set_status<E: PortableAsm>(a: &mut E, status: u64) {
    lea_rt(a, R::V1, STATUS);
    a.mov_imm64(R::V0, status);
    a.atomic_store_release(R::V1, R::V0);
}

/// Initialization runs before mutators start. A child may run before spawn
/// returns, so its stable identity and runtime link precede the OS launch.
pub(super) fn spawn<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else { return };
    quiescence::save(a);
    for index in 0..CAPACITY {
        lea_rt(a, R::OwnerContext, slot(index));
        store_rt(
            a,
            owner_state::offset(offset_of!(OwnerState, shared_runtime)),
            R::V3,
        );
        store_rt_imm(a, IDENTITY, index as u64);
        set_status(a, STARTING);
        a.call_label(w.hooks.spawn);
        let failed = a.create_text_label();
        let next = a.create_text_label();
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, failed);
        a.cmp_reg_imm8(R::V0, 1);
        a.jcc_label(C::NotEqual, l.fail);
        set_status(a, RUNNING);
        increment(a, l, LIVE);
        increment(a, l, LAUNCHED);
        // Existing scheduling predicates ask whether any worker exists.
        store_rt_imm(a, RT_WORKER_ALIVE, 1);
        a.jmp_label(next);
        a.bind_text_label(failed);
        // The OS adapter has already rolled back this slot's failed launch;
        // successfully launched peers remain alive and keep their resources.
        set_status(a, 0);
        a.bind_text_label(next);
    }
    worker::request_work(a);
    quiescence::restore(a);
    worker::wake_if_pending(a, l);
}

/// Constant-size broadcast. A per-worker auto-reset event must never be
/// mistaken for a broadcast event; each successfully launched slot is woken.
pub(super) fn wake<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else { return };
    quiescence::save(a);
    for index in 0..CAPACITY {
        let next = a.create_text_label();
        lea_rt(a, R::OwnerContext, slot(index));
        lea_rt(a, R::V1, STATUS);
        a.atomic_load_acquire(R::V0, R::V1);
        a.cmp_reg_imm8(R::V0, RUNNING as i8);
        a.jcc_label(C::NotEqual, next);
        lea_rt(a, R::V7, RT_WORK_SEQ);
        a.call_label(w.hooks.unpark);
        a.bind_text_label(next);
    }
    quiescence::restore(a);
}

/// Called with the current worker's ticket after shared journal leases close.
pub(super) fn exiting<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    lea_rt(a, R::V1, STATUS);
    a.atomic_load_acquire(R::V0, R::V1);
    a.cmp_reg_imm8(R::V0, RUNNING as i8);
    a.jcc_label(C::NotEqual, l.fail);
    set_status(a, STOPPED);
    increment(a, l, STOPPED_COUNT);
}

pub(super) fn shutdown<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    let Some(w) = &l.worker else { return };
    let Some(join) = w.hooks.join else { return };
    quiescence::save(a);
    load_runtime(a, runtime);
    worker::lock(a, l);
    let absent = a.create_text_label();
    load_rt(a, R::V0, LIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, absent);
    store_rt_imm(a, STOP, 1);
    worker::request_work(a);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    // No OS join or resource free may hold the ticket the child needs to exit.
    for index in 0..CAPACITY {
        let next = a.create_text_label();
        lea_rt(a, R::OwnerContext, slot(index));
        lea_rt(a, R::V1, STATUS);
        a.atomic_load_acquire(R::V0, R::V1);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, next);
        a.call_label(join);
        lea_rt(a, R::V1, STATUS);
        a.atomic_load_acquire(R::V0, R::V1);
        a.cmp_reg_imm8(R::V0, STOPPED as i8);
        a.jcc_label(C::NotEqual, l.fail);
        set_status(a, JOINED);
        a.bind_text_label(next);
    }
    load_runtime(a, runtime);
    worker::lock(a, l);
    worker_view::require_quiescent(a, l);
    load_rt(a, R::V0, LAUNCHED);
    load_rt(a, R::V1, STOPPED_COUNT);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt_imm(a, LIVE, 0);
    store_rt_imm(a, RT_WORKER_ALIVE, 0);
    a.bind_text_label(absent);
    worker::unlock(a, l);
    quiescence::restore(a);
}

/// Preserve the quantum's completion flag in V0 while attributing actual
/// charged graph work to its OS worker, not merely counting lock acquisitions.
pub(super) fn record_units<E: PortableAsm>(a: &mut E, field: usize, budget: u64) {
    a.push_reg(R::V0);
    a.mov_imm64(R::V1, budget);
    load_rt(a, R::V2, RT_BUDGET);
    a.sub_reg_reg(R::V1, R::V2);
    load_rt(a, R::V0, field);
    a.add_reg_reg(R::V0, R::V1);
    store_rt(a, field, R::V0);
    a.pop_reg(R::V0);
}
