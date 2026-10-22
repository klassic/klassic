//! M-G6 step 5: the GC worker thread for `--gc=generational`.
//!
//! After a pause opens a relocation phase, the worker runs the resumable
//! Cheney scan (`cheney`, which copies through `forward`) in bounded quanta
//! while the mutator keeps running its own code. Collector state (TLAB,
//! scratch root, root journal, forwarding, remembered set) is shared, so
//! every collector operation runs under one fair ticket lock: the worker
//! holds it for one quantum at a time; the mutator takes it in `alloc`, in
//! the load-barrier slow path, and -- only while a phase is open -- in its
//! root, write-barrier and equality routines. A worker heal of a heap field
//! is a CAS against the word it read, so a racing unlocked mutator store
//! always wins. The worker never recycles from-space: it sets `worker_done`
//! and the mutator finishes the phase at its next allocation (a safepoint).
//! Without a worker (spawn failed, or a backend without hooks) the
//! allocation quanta of steps 1-3 run as before.
use super::{
    Labels, RT_COLLECTING, RT_LOCK_NEXT, RT_LOCK_SERVING, RT_PHASE_OPEN, RT_WAKE_PENDING,
    RT_WORK_SEQ, RT_WORKER_ALIVE, RT_WORKER_DONE, RT_WORKER_SCANNED, d, lea_rt, load_runtime,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

/// Objects the worker scans per lock hold.
const WORKER_QUANTUM: u64 = 64;

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
}

pub(super) struct WorkerLabels<T> {
    pub(super) hooks: WorkerHooks<T>,
    pub(super) entry: T,
    pub(super) lock: T,
    pub(super) unlock: T,
}

impl<T: Copy> WorkerLabels<T> {
    pub(super) fn new(hooks: WorkerHooks<T>, mut create: impl FnMut() -> T) -> Self {
        Self {
            hooks,
            entry: create(),
            lock: create(),
            unlock: create(),
        }
    }
}

/// Take the collector lock (V3 = runtime). With `only_open`, skip it while
/// no relocation phase is open: the mutator alone writes `phase_open`, and
/// it closes only under the lock, so a closed phase means the worker is
/// idle. The caller must pair it with `unlock_if` on the same condition and
/// must not open or close a phase in between.
pub(super) fn lock_if<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, only_open: bool) {
    let Some(w) = &l.worker else {
        return;
    };
    let skip = a.create_text_label();
    if only_open {
        a.push_reg(R::V0);
        a.load_ptr_disp32(R::V0, R::V3, d(RT_PHASE_OPEN));
        a.test_reg_reg(R::V0, R::V0);
        a.pop_reg(R::V0);
        a.jcc_label(C::Equal, skip);
    }
    a.call_label(w.lock);
    a.bind_text_label(skip);
}

pub(super) fn unlock_if<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, only_open: bool) {
    let Some(w) = &l.worker else {
        return;
    };
    let skip = a.create_text_label();
    if only_open {
        a.push_reg(R::V0);
        a.load_ptr_disp32(R::V0, R::V3, d(RT_PHASE_OPEN));
        a.test_reg_reg(R::V0, R::V0);
        a.pop_reg(R::V0);
        a.jcc_label(C::Equal, skip);
    }
    a.call_label(w.unlock);
    a.bind_text_label(skip);
}

/// After `alloc` released the lock: wake the worker if a pause opened a
/// phase. Preserves V0 (the allocation result) and V3.
pub(super) fn wake_if_pending<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    let none = a.create_text_label();
    a.load_ptr_disp32(R::V1, R::V3, d(RT_WAKE_PENDING));
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, none);
    a.mov_imm64(R::V1, 0);
    a.store_ptr_disp32(R::V3, d(RT_WAKE_PENDING), R::V1);
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
    a.store_ptr_disp32(R::V3, d(RT_WORKER_DONE), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_WORK_SEQ));
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_WORK_SEQ), R::V0);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_WAKE_PENDING), R::V0);
}

/// In `quantum`: with a live worker the mutator does no scanning; it only
/// finishes a phase the worker reported scanned (jumps to `finish`), else
/// returns. Without a worker, falls through to the mutator quantum.
pub(super) fn quantum_handoff<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if l.worker.is_none() {
        return;
    }
    let mutator = a.create_text_label();
    let not_done = a.create_text_label();
    a.load_ptr_disp32(R::V0, R::V3, d(RT_WORKER_ALIVE));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, mutator);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_WORKER_DONE));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, not_done);
    a.jmp_label(l.finish);
    a.bind_text_label(not_done);
    a.ret();
    a.bind_text_label(mutator);
}

/// Keep the field word just read (V0) for `heal`'s CAS (worker builds).
pub(super) fn remember_read<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if l.worker_entry().is_some() {
        a.store_ptr_disp32(R::V3, d(super::RT_CS_OLD), R::V0);
    }
}

/// `[V2] = V0`. With a worker, a CAS against the word read: an unlocked
/// mutator store that landed in between wins, and the scan moves on to
/// `lost` (that store's own barrier already ran). Preserves V1 and V2.
pub(super) fn heal<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, lost: E::TextLabel) {
    if l.worker_entry().is_none() {
        a.store_ptr_disp32(R::V2, 0, R::V0);
        return;
    }
    a.mov_reg_reg(R::V6, R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(super::RT_CS_OLD));
    a.atomic_compare_exchange(R::V2, R::V6);
    a.load_ptr_disp32(R::V7, R::V3, d(super::RT_CS_OLD));
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::NotEqual, lost);
}

/// At the end of `startup` (V3 = runtime): start the worker.
pub(super) fn emit_spawn_call<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    a.call_label(w.hooks.spawn);
    a.store_ptr_disp32(R::V3, d(RT_WORKER_ALIVE), R::V0);
}

pub(super) fn emit_worker<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
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
    for reg in [R::V9, R::V8, R::V2, R::V1, R::V0] {
        a.pop_reg(reg);
    }
    a.ret();

    // unlock: only the holder writes `serving`. Preserves every register.
    a.bind_text_label(w.unlock);
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_LOCK_SERVING));
    a.add_reg_imm32(R::V0, 1);
    lea_rt(a, R::V1, RT_LOCK_SERVING);
    a.atomic_store_release(R::V1, R::V0);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
    a.ret();

    // entry: the worker thread's loop. It never returns; the process ends
    // with an exit_group from the mutator (or from a fatal report).
    let top = a.create_text_label();
    let idle = a.create_text_label();
    let scanned = a.create_text_label();
    a.bind_text_label(w.entry);
    load_runtime(a, runtime);
    a.bind_text_label(top);
    a.call_label(w.lock);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_PHASE_OPEN));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, idle);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_WORKER_DONE));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, idle);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_COLLECTING), R::V0);
    a.mov_imm64(R::V0, WORKER_QUANTUM);
    a.store_ptr_disp32(R::V3, d(super::RT_BUDGET), R::V0);
    a.call_label(l.cheney);
    a.mov_imm64(R::V1, 0);
    a.store_ptr_disp32(R::V3, d(RT_COLLECTING), R::V1);
    // worker_scanned += quantum - unspent budget.
    a.load_ptr_disp32(R::V1, R::V3, d(RT_WORKER_SCANNED));
    a.add_reg_imm32(R::V1, WORKER_QUANTUM as i32);
    a.load_ptr_disp32(R::V2, R::V3, d(super::RT_BUDGET));
    a.sub_reg_reg(R::V1, R::V2);
    a.store_ptr_disp32(R::V3, d(RT_WORKER_SCANNED), R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, scanned);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_WORKER_DONE), R::V0);
    a.bind_text_label(scanned);
    a.call_label(w.unlock);
    a.jmp_label(top);
    // Nothing to do: read the wake sequence under the lock, then sleep
    // until a pause bumps it (a bump in between makes park return).
    a.bind_text_label(idle);
    a.load_ptr_disp32(R::V6, R::V3, d(RT_WORK_SEQ));
    a.call_label(w.unlock);
    lea_rt(a, R::V7, RT_WORK_SEQ);
    a.call_label(w.hooks.park);
    load_runtime(a, runtime);
    a.jmp_label(top);
}
