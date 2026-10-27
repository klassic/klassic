//! M-G6 steps 4-5: the GC worker thread for `--gc=generational`.
//!
//! The worker marks concurrently (from the root snapshot a short pause
//! takes) and, after a relocation pause, runs the resumable Cheney scan
//! (`wcheney`, copying through `wforward` into worker-private to-space
//! pages) while the mutator keeps running. Copies are claimed by a CAS on
//! the source header, so the mutator's load barrier and the worker never
//! need a common lock to copy. One fair ticket lock remains: the worker holds
//! it for one quantum at a time, and the mutator takes it only to stop the
//! worker (relocation pause, mark start, phase finish, `--gc-log` verify)
//! and on the SATB slow path. A worker heal of a heap field is a CAS against
//! the word it read, so a racing mutator store always wins. After the owner
//! adopts Retire at a quiescent safepoint, the worker cleans the detached
//! from-space in bounded quanta. Only the owner adopts Idle. Without a worker,
//! normal allocation quanta and explicit resource waits service cleanup.
//! Mark membership uses object-owned epochs, so readiness requires no worker
//! clearing pass; only the owner activates the snapshot at an allocation safepoint.
use super::{
    Labels, RT_LOCK_NEXT, RT_LOCK_SERVING, RT_MARK_ACTIVE, RT_MARK_DONE, RT_PHASE_OPEN,
    RT_WAKE_PENDING, RT_WORK_SEQ, RT_WORKER_ALIVE, RT_WORKER_DONE, RT_WORKER_SCANNED, lea_rt,
    load_rt, load_runtime, store_rt,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

/// Work budget per lock hold: mark roots, objects and fields each cost a
/// unit. Relocation likewise charges root, object admission and field work.
const WORKER_QUANTUM: u64 = super::QUANTUM_OBJECTS;

/// Backend leaves the worker needs (platform threads and futexes).
pub(crate) struct WorkerHooks<T> {
    /// `() -> V0 = 1` when a thread now runs `entry` on its own stack,
    /// else 0. Preserves V3.
    pub spawn: T,
    /// `(V7 = u32 address, V6 = expected)`: sleep while `*V7 == V6`.
    /// Clobbers the caller-saved registers.
    pub park: T,
    /// `(V7 = u32 address)`: wake a parked thread. Clobbers the
    /// caller-saved registers.
    pub unpark: T,
    /// `()`: give up the CPU (a lock waiter whose holder was preempted).
    /// Preserves every register.
    pub relax: T,
    /// Monotonic ns in V0, zero on failure; preserves all other registers.
    pub clock: T,
    /// Join a stopped worker and release its OS notification/thread resources.
    /// Available only on backends whose callback can return to its OS caller.
    pub join: Option<T>,
}

/// Normal process completion only: stop between bounded worker quanta, wake
/// an idle worker, then join outside the collector lock. Fatal exits retain
/// their process-wide abort path. No collector storage is freed before join.
pub(crate) fn shutdown<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    let Some(w) = &l.worker else { return };
    let Some(join) = w.hooks.join else { return };
    let done = a.create_text_label();
    load_runtime(a, runtime);
    lock(a, l);
    load_rt(a, R::V0, RT_WORKER_ALIVE);
    a.test_reg_reg(R::V0, R::V0);
    let absent = a.create_text_label();
    a.jcc_label(C::Equal, absent);
    // Startup can schedule the child before publishing alive=1. Only the
    // distinct stop value 2 may make the child return from its entry.
    a.mov_imm64(R::V0, 2);
    store_rt(a, RT_WORKER_ALIVE, R::V0);
    request_work(a);
    unlock(a, l);
    wake_if_pending(a, l);
    a.call_label(join);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_WORKER_ALIVE, R::V0);
    a.jmp_label(done);
    a.bind_text_label(absent);
    unlock(a, l);
    a.bind_text_label(done);
}

pub(super) struct WorkerLabels<T> {
    pub(super) hooks: WorkerHooks<T>,
    pub(super) entry: T,
    pub(super) lock: T,
    pub(super) unlock: T,
    /// Worker-side copy (private to-space pages) and scan.
    pub(super) wforward: T,
    pub(super) wcheney: T,
    /// Marking: `mark_ref` (V2 = payload offset), budgeted `mark_loop`,
    /// and the mark-start pause.
    pub(super) mark_ref: T,
    pub(super) mark_loop: T,
    pub(super) mark_start: T,
    pub(super) young: super::young_mark::Labels<T>,
}

impl<T: Copy> WorkerLabels<T> {
    pub(super) fn new(hooks: WorkerHooks<T>, mut create: impl FnMut() -> T) -> Self {
        Self {
            hooks,
            entry: create(),
            lock: create(),
            unlock: create(),
            wforward: create(),
            wcheney: create(),
            mark_ref: create(),
            mark_loop: create(),
            mark_start: create(),
            young: super::young_mark::Labels::new(&mut create),
        }
    }
}

/// Take the collector lock (V3 = runtime), stopping the worker between
/// two quanta. No-op without a worker. Preserves every register.
pub(super) fn lock<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.call_label(l.owner_lock);
}

pub(super) fn unlock<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.call_label(l.owner_unlock);
}

/// Shared release jobs can run on either actor. They never perform root
/// producer operations, and must not change the sole owner's ownership flag.
pub(super) fn lock_raw<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if let Some(w) = &l.worker {
        a.call_label(w.lock);
    }
}

/// Drop only the ticket, preserving every register. Completion of a release
/// job must not recursively claim a newly queued job in this same call stack.
pub(super) fn unlock_raw<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if l.worker.is_some() {
        a.push_reg(R::V0);
        a.push_reg(R::V1);
        super::worker_view::release(a, l);
        load_rt(a, R::V0, RT_LOCK_SERVING);
        a.add_reg_imm32(R::V0, 1);
        lea_rt(a, R::V1, RT_LOCK_SERVING);
        a.atomic_store_release(R::V1, R::V0);
        a.pop_reg(R::V1);
        a.pop_reg(R::V0);
    }
}

/// Ask the worker to look for work again (it is woken after `alloc`).
pub(super) fn request_work<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, RT_WORK_SEQ);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, RT_WORK_SEQ, R::V0);
    a.mov_imm64(R::V0, 1);
    store_rt(a, RT_WAKE_PENDING, R::V0);
}

/// After `alloc` released the lock: wake the worker if a pause opened a
/// phase. Preserves V0 (the allocation result) and V3.
pub(super) fn wake_if_pending<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    let none = a.create_text_label();
    load_rt(a, R::V1, RT_WAKE_PENDING);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, none);
    a.mov_imm64(R::V1, 0);
    store_rt(a, RT_WAKE_PENDING, R::V1);
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    lea_rt(a, R::V7, RT_WORK_SEQ);
    a.call_label(w.hooks.unpark);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.bind_text_label(none);
}

/// In `collect`, once the pause opened a phase: hand it to the worker.
pub(super) fn open_phase<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if l.worker.is_none() {
        return;
    }
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_WORKER_DONE, R::V0);
    request_work(a);
}

/// In `quantum`: with a live worker the mutator does no scanning; it only
/// checks a phase the worker reported scanned, rearming stale completion
/// when metadata still records work. Without a worker, falls through to the mutator
/// quantum.
pub(super) fn quantum_handoff<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if l.worker.is_none() {
        return;
    }
    let mutator = a.create_text_label();
    let not_done = a.create_text_label();
    load_rt(a, R::V0, RT_WORKER_ALIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, mutator);
    load_rt(a, R::V0, RT_WORKER_DONE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, not_done);
    lock(a, l);
    a.call_label(l.try_finish_relocation);
    unlock(a, l);
    a.bind_text_label(not_done);
    // Refill may be waiting for this phase and cannot reach allocation's
    // return wake. Deliver a rearmed request before that wait polls again.
    wake_if_pending(a, l);
    a.ret();
    a.bind_text_label(mutator);
}

/// Keep the field word just read (V0) for `heal`'s CAS (worker builds).
pub(super) fn remember_read<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if l.worker_entry().is_some() {
        store_rt(a, super::RT_CS_OLD, R::V0);
    }
}

/// `[V2] = V0`. With a worker, a CAS against the word read: an unlocked
/// mutator store that landed in between wins, and the scan moves on to
/// `lost` (that store's own barrier already ran). Preserves V1 and V2.
pub(super) fn heal<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, lost: E::TextLabel) {
    super::fields::forget_nonnull(a, R::V0, R::V6);
    if l.worker_entry().is_none() {
        a.store_ptr_disp32(R::V2, 0, R::V0);
        return;
    }
    a.mov_reg_reg(R::V6, R::V0);
    load_rt(a, R::V0, super::RT_CS_OLD);
    a.atomic_compare_exchange(R::V2, R::V6);
    load_rt(a, R::V7, super::RT_CS_OLD);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::NotEqual, lost);
}

/// At the end of `startup` (V3 = runtime): start the worker.
pub(super) fn emit_spawn_call<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    a.call_label(w.hooks.spawn);
    store_rt(a, RT_WORKER_ALIVE, R::V0);
}

pub(super) fn emit_worker<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
    emit_owner_locks(a, l);
    let Some(w) = &l.worker else {
        return;
    };
    // lock: fair ticket lock. V3 = runtime; preserves every register.
    let retry = a.create_text_label();
    let wait = a.create_text_label();
    let held = a.create_text_label();
    a.bind_text_label(w.lock);
    for reg in [R::V0, R::V1, R::V2, R::V8, R::V9] {
        a.push_reg(reg);
    }
    lea_rt(a, R::V1, RT_LOCK_NEXT);
    a.atomic_load_acquire(R::V0, R::V1);
    a.bind_text_label(retry);
    a.mov_reg_reg(R::V8, R::V0);
    a.mov_reg_reg(R::V2, R::V0);
    a.add_reg_imm32(R::V2, 1);
    a.atomic_compare_exchange(R::V1, R::V2);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, retry);
    // V8 = my ticket.
    lea_rt(a, R::V1, RT_LOCK_SERVING);
    a.mov_imm64(R::V2, 0);
    let spin = a.create_text_label();
    a.bind_text_label(wait);
    a.atomic_load_acquire(R::V9, R::V1);
    a.cmp_reg_reg(R::V9, R::V8);
    a.jcc_label(C::Equal, held);
    // Yield every 128 failed polls: on one CPU the holder needs it.
    a.add_reg_imm32(R::V2, 1);
    a.mov_reg_reg(R::V9, R::V2);
    a.and_reg_imm32(R::V9, 127);
    a.jcc_label(C::NotEqual, spin);
    a.call_label(w.hooks.relax);
    a.bind_text_label(spin);
    a.jmp_label(wait);
    a.bind_text_label(held);
    super::worker_view::refresh(a, l);
    for reg in [R::V9, R::V8, R::V2, R::V1, R::V0] {
        a.pop_reg(reg);
    }
    a.ret();

    // unlock: only the holder writes `serving`. Preserves every register.
    a.bind_text_label(w.unlock);
    unlock_raw(a, l);
    super::metadata::after_unlock(a, l);
    super::weak::after_unlock(a, l);
    super::metadata_leases::after_unlock(a, l);
    super::mark_queue::after_unlock(a, l);
    a.ret();

    // Returning backends enter through an OS ABI wrapper; other backends
    // retain their process-owned worker until process-wide termination.
    let top = a.create_text_label();
    let idle = a.create_text_label();
    let scanned = a.create_text_label();
    let relocate = a.create_text_label();
    let release = a.create_text_label();
    a.bind_text_label(w.entry);
    load_runtime(a, runtime);
    super::worker_view::bind(a);
    a.bind_text_label(top);
    #[cfg(test)]
    if let Some(hook) = l.worker_before_lock_hook {
        a.call_label(hook);
    }
    a.call_label(w.lock);
    // Lease slots have no raw pointer borrowers between tickets. Service
    // growth even when retained source leases delay phase retirement.
    super::metadata_leases::work(a, l);
    super::mark_queue::work(a, l);
    super::store_rt_imm(a, super::worker_journal::RETRY, 0);
    if w.hooks.join.is_some() {
        let running = a.create_text_label();
        load_rt(a, R::V0, RT_WORKER_ALIVE);
        a.cmp_reg_imm8(R::V0, 2);
        a.jcc_label(C::NotEqual, running);
        super::worker_journal::shutdown(a, l, release);
        a.call_label(w.unlock);
        a.ret();
        a.bind_text_label(running);
    }
    let normal = a.create_text_label();
    load_rt(a, R::V0, super::RT_RETIRE_STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, normal);
    a.cmp_reg_imm8(R::V0, super::retire::DONE as i8);
    a.jcc_label(C::Equal, idle);
    a.mov_imm64(R::V0, WORKER_QUANTUM);
    store_rt(a, super::RT_BUDGET, R::V0);
    a.call_label(l.retire_step);
    a.call_label(w.unlock);
    a.jmp_label(top);
    a.bind_text_label(normal);
    // Independent journal admission: one region and at most one record per
    // lock hold, followed by ordinary graph work so a busy producer cannot
    // starve tracing. The lane owns shared scratch, never a mutator's scratch.
    a.call_label(l.worker_journal_step);
    // One retired cell (or one constant metadata step) follows journal work.
    // Keep its retry request after the journal lane resets its own pass flag.
    // Submitted Nodes keep their real storage leases until actual completion.
    a.call_label(l.root_retirement_step);
    // Relocate retained receivers before resuming the Old mark.
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, relocate);
    a.mov_imm64(R::V0, WORKER_QUANTUM);
    store_rt(a, super::RT_BUDGET, R::V0);
    super::weak::work(a, l);
    let old_mark = a.create_text_label();
    let young_mark = a.create_text_label();
    load_rt(a, R::V0, super::young_mark::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, old_mark);
    load_rt(a, R::V0, super::young_mark::DONE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, old_mark);
    load_rt(a, R::V0, RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, young_mark);
    load_rt(a, R::V0, RT_MARK_DONE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, young_mark);
    load_rt(a, R::V0, super::young_mark::TURN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, young_mark);
    a.bind_text_label(old_mark);
    load_rt(a, R::V0, RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, relocate);
    load_rt(a, R::V0, RT_MARK_DONE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, relocate);
    a.mov_imm64(R::V0, WORKER_QUANTUM);
    store_rt(a, super::RT_BUDGET, R::V0);
    a.call_label(w.mark_loop);
    super::drain::record_worker_mark(a, l);
    a.mov_imm64(R::V1, 0);
    store_rt(a, super::young_mark::TURN, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, release);
    store_rt(a, RT_MARK_DONE, R::V0);
    a.jmp_label(release);
    a.bind_text_label(young_mark);
    a.mov_imm64(R::V0, WORKER_QUANTUM);
    store_rt(a, super::RT_BUDGET, R::V0);
    a.call_label(w.young.trace);
    a.mov_imm64(R::V1, WORKER_QUANTUM);
    load_rt(a, R::V2, super::RT_BUDGET);
    a.sub_reg_reg(R::V1, R::V2);
    load_rt(a, R::V2, super::young_mark::WORKER_UNITS);
    a.add_reg_reg(R::V1, R::V2);
    store_rt(a, super::young_mark::WORKER_UNITS, R::V1);
    a.mov_imm64(R::V1, 1);
    store_rt(a, super::young_mark::TURN, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, release);
    store_rt(a, super::young_mark::DONE, R::V0);
    a.bind_text_label(release);
    a.call_label(w.unlock);
    a.jmp_label(top);
    a.bind_text_label(relocate);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, idle);
    load_rt(a, R::V0, RT_WORKER_DONE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, idle);
    a.mov_imm64(R::V0, WORKER_QUANTUM);
    store_rt(a, super::RT_BUDGET, R::V0);
    a.call_label(w.wcheney);
    super::drain::record_worker_relocation(a, l);
    // Count completed objects once, independent of root/field work or resumes.
    load_rt(a, R::V1, RT_WORKER_SCANNED);
    load_rt(a, R::V2, super::RT_CS_COMPLETED);
    a.add_reg_reg(R::V1, R::V2);
    store_rt(a, RT_WORKER_SCANNED, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, scanned);
    a.mov_imm64(R::V0, 1);
    store_rt(a, RT_WORKER_DONE, R::V0);
    a.bind_text_label(scanned);
    a.call_label(w.unlock);
    a.jmp_label(top);
    // Nothing to do: run one bounded idle scan, then sample the wake sequence
    // under the lock. Park returns on a sequence change or its idle timeout.
    a.bind_text_label(idle);
    super::weak::retry(a, release);
    super::metadata_leases::retry(a, release);
    super::mark_queue::retry(a, release);
    load_rt(a, R::V0, super::worker_journal::RETRY);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, release);
    a.call_label(w.hooks.clock);
    a.call_label(l.uncommit_poll);
    load_rt(a, R::V6, RT_WORK_SEQ);
    a.call_label(w.unlock);
    lea_rt(a, R::V7, RT_WORK_SEQ);
    a.call_label(w.hooks.park);
    load_runtime(a, runtime);
    super::worker_view::bind(a);
    a.jmp_label(top);
}

/// Only the owner calls these wrappers. Publish ownership after the real
/// acquire, clear it before release. Worker and shared reclaim paths stay raw.
fn emit_owner_locks<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    use super::root_journal::OWNER_LOCKED;
    a.bind_text_label(l.owner_lock);
    a.push_reg(R::V0);
    load_rt(a, R::V0, OWNER_LOCKED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.pop_reg(R::V0);
    lock_raw(a, l);
    a.push_reg(R::V0);
    super::store_rt_imm(a, OWNER_LOCKED, 1);
    a.pop_reg(R::V0);
    a.ret();
    for (label, raw) in [(l.owner_unlock, false), (l.owner_unlock_raw, true)] {
        a.bind_text_label(label);
        a.push_reg(R::V0);
        load_rt(a, R::V0, OWNER_LOCKED);
        a.cmp_reg_imm8(R::V0, 1);
        a.jcc_label(C::NotEqual, l.fail);
        super::store_rt_imm(a, OWNER_LOCKED, 0);
        a.pop_reg(R::V0);
        if raw {
            unlock_raw(a, l);
        } else if let Some(w) = &l.worker {
            a.call_label(w.unlock);
        } else {
            super::metadata::after_unlock(a, l);
        }
        a.ret();
    }
}
