//! Space flips, relocation handoff and explicit finish for the production GC.
//!
//! The owner flips spaces, captures roots and opens relocation. Normal
//! quanta use barriers/CAS-claimed copies. A quiescent owner finish closes
//! moving ranges before handing dead-space cleanup to budgeted retirement.
//! The next flip waits for that cleanup; explicit finish releases the lock
//! between relocation/cleanup quanta. Byte copies, mapping and start resets
//! still have unbounded work; this is not the final low-pause coordinator.
use super::{
    Labels, REQUEST_VALUE, ROOT_STORE, RT_ALLOC_ROOT, RT_ALLOC_SIZE, RT_ALLOC_TAG, RT_COLLECTING,
    RT_COLLECTIONS, RT_CTX, RT_FROM_BASE, RT_FROM_END, RT_FROM_MARKS, RT_MARKS, RT_NEXT_PAGE,
    RT_REQUEST, RT_SAVED_DESCRIPTOR, RT_SAVED_ELEMENTS, RT_SAVED_SIZE, RT_SAVED_TAG, RT_SPACE_BASE,
    RT_SPACE_END, d, imm, lea_rt, load_rt, root_step, store_rt,
};
use crate::gc_abi::{AllocationRequest, MarkArena, MarkRecord};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

/// `dst = mark arena bank, independent of physical page placement`.
pub(super) fn arena<E: PortableAsm>(a: &mut E, dst: R, base: R) {
    let first = a.create_text_label();
    lea_rt(a, dst, super::layout(a).marks as usize);
    a.test_reg_reg(base, base);
    a.jcc_label(C::Equal, first);
    a.add_reg_imm32(
        dst,
        imm(super::geometry::mark_bank_bytes(a.gc_heap_capacity())),
    );
    a.bind_text_label(first);
}

pub(super) fn emit_collect<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    emit_phase(a, l);
    super::retire::emit(a, l);
    let elements = RT_REQUEST + offset_of!(AllocationRequest, elements);
    let descriptor = RT_REQUEST + offset_of!(AllocationRequest, descriptor);
    let page_field = RT_REQUEST + offset_of!(AllocationRequest, page);

    // collect: V3 = runtime. Clobbers the caller-saved registers; the
    // interrupted allocation's size and element count are restored.
    a.bind_text_label(l.collect);
    super::remset_buffer::flush(a, l, false);
    super::satb_buffer::flush(a, l, false);
    let legacy = a.create_text_label();
    load_rt(a, R::V0, super::moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, legacy);
    a.call_label(l.moving_start_actor);
    a.ret();
    a.bind_text_label(legacy);
    super::cooperative_old::branch_enabled(a, l.fail);
    // Phase history and the shared monotonic-clock watermark are also
    // written by the worker. Serialize the optional timer with those writes.
    if a.gc_timing() {
        super::worker::lock(a, l);
        super::mutator_time::begin(a, l);
        super::worker::unlock(a, l);
    }
    super::young_mark::wait(a, l);
    // Stop the worker for the whole pause.
    super::worker::lock(a, l);
    a.bind_text_label(l.collect_moving);
    // The rendezvous must have sealed every owner before either a full
    // mark drain or any physical work can wait on private obligations.
    super::remset_buffer::require_empty(a, l);
    super::satb_buffer::require_empty(a, l);
    load_rt(a, R::V0, RT_ALLOC_SIZE);
    store_rt(a, RT_SAVED_SIZE, R::V0);
    load_rt(a, R::V0, elements);
    store_rt(a, RT_SAVED_ELEMENTS, R::V0);
    // `forward` copies through alloc_raw, which overwrites the shape tag and
    // the request descriptor; the interrupted allocation needs its own back.
    load_rt(a, R::V0, RT_ALLOC_TAG);
    store_rt(a, RT_SAVED_TAG, R::V0);
    load_rt(a, R::V0, descriptor);
    store_rt(a, RT_SAVED_DESCRIPTOR, R::V0);
    // A relocation phase still open from the previous collection must
    // complete before its spaces can flip again.
    let ready = a.create_text_label();
    load_rt(a, R::V0, super::phase_round::KIND);
    a.cmp_reg_imm32(R::V0, 5);
    a.jcc_label(C::Equal, ready);
    a.call_label(l.finish);
    super::young_mark::ensure_for_collection(a, l);
    a.bind_text_label(ready);
    super::old_cycle::recheck(a);
    super::mark_prepare::cancel(a, l);
    // Young-only moves preserve the active Old snapshot; full Old moves
    // must close it before changing Old physical addresses.
    super::mark_frontier::before_flip(a, l);
    // The same collector ticket excludes admissions while the rendezvous
    // keeps acknowledged peers parked. Check before any physical bank or
    // source-run mutation, not merely before the remembered bitmap flip.
    super::remset_buffer::require_empty(a, l);
    super::satb_buffer::require_empty(a, l);
    clear_scratch(a, l);
    // Close the allocating page: it belongs to the space being evacuated.
    let closed = a.create_text_label();
    load_rt(a, R::V6, page_field);
    a.test_reg_reg(R::V6, R::V6);
    a.jcc_label(C::Equal, closed);
    lea_rt(a, R::V7, RT_CTX);
    a.mov_imm64(R::V2, 1);
    a.call_label(l.tlab_page);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_imm64(R::V0, 0);
    store_rt(a, page_field, R::V0);
    a.bind_text_label(closed);

    // Capture the source run, then switch the independent mark metadata bank. Pages
    // [space_base, next_page) were bound by the TLAB, [copy_low,
    // space_end) by the worker.
    load_rt(a, R::V0, RT_SPACE_BASE);
    store_rt(a, RT_FROM_BASE, R::V0);
    load_rt(a, R::V1, RT_SPACE_END);
    store_rt(a, super::capacity::SOURCE_END, R::V1);
    load_rt(a, R::V1, RT_NEXT_PAGE);
    store_rt(a, RT_FROM_END, R::V1);
    load_rt(a, R::V1, super::RT_COPY_LOW);
    store_rt(a, super::RT_FROM_COPY_LOW, R::V1);
    load_rt(a, R::V1, RT_MARKS + offset_of!(MarkArena, used));
    store_rt(a, RT_FROM_MARKS, R::V1);
    load_rt(a, R::V0, super::pool::BANK);
    store_rt(a, super::pool::FROM_BANK, R::V0);
    a.mov_imm64(R::V1, 1);
    a.xor_reg_reg(R::V0, R::V1);
    store_rt(a, super::pool::BANK, R::V0);
    arena(a, R::V2, R::V0);
    store_rt(a, RT_MARKS + offset_of!(MarkArena, records), R::V2);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_MARKS + offset_of!(MarkArena, used), R::V0);
    super::allocation_records::advance(a, l);
    // Full collection when the actual Old run cannot absorb the Young reserve.
    // Its destination is selected from the same pool after choosing Young.
    let young_only = a.create_text_label();
    let decided = a.create_text_label();
    let full = a.create_text_label();
    load_rt(a, R::V1, super::RT_OLD_BASE);
    load_rt(a, R::V0, super::old_cycle::REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, full);
    super::pool::must_full(a, l, full);
    a.jmp_label(young_only);
    a.bind_text_label(full);
    super::old_runs::prepare_capacity(a, l);
    load_rt(a, R::V1, super::RT_OLD_BASE);
    super::old_cycle::consume(a);
    a.mov_imm64(R::V0, 1);
    store_rt(a, super::RT_FULL, R::V0);
    store_rt(a, super::RT_OLD_FROM, R::V1);
    load_rt(a, R::V0, super::RT_OLD_TOP);
    store_rt(a, super::RT_OLD_FROM_END, R::V0);
    super::old_runs::detach(a, l);
    // Everything is promoted, so no Young survivor needs a remembered field:
    // both sets are discarded. Destinations require clean free pages; the
    // detached source bitmap is cleared during retirement. New
    // stores into moved Old objects can register while cleanup runs.
    super::remset_buffer::require_empty(a, l);
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_REMSET_LEN, R::V0);
    store_rt(a, super::RT_REMSET_PREV_LEN, R::V0);
    load_rt(a, R::V0, super::RT_FULL_COLLECTIONS);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, super::RT_FULL_COLLECTIONS, R::V0);
    super::large_trace::begin(a, l);
    a.jmp_label(decided);
    a.bind_text_label(young_only);
    // G07: the current remembered set becomes the previous one.
    super::remset::emit_flip(a, l);
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_FULL, R::V0);
    a.bind_text_label(decided);
    super::capacity::select(a, l);
    let old_kept = a.create_text_label();
    load_rt(a, R::V0, super::RT_FULL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, old_kept);
    super::old_runs::claim(a, l);
    a.bind_text_label(old_kept);
    let deferred_page = a.create_text_label();
    load_rt(a, R::V0, super::phase_round::KIND);
    a.cmp_reg_imm32(R::V0, 5);
    a.jcc_label(C::Equal, deferred_page);
    super::alloc::reserve_initial_page(a, l);
    a.bind_text_label(deferred_page);
    super::tenuring::begin(a, l);
    super::phase::relocating(a, l, crate::gc_abi::Phase::RelocateStart);
    // From-space ranges: exactly the objects this collection moves.
    let full_range = a.create_text_label();
    let ranges = a.create_text_label();
    load_rt(a, R::V0, RT_FROM_BASE);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    store_rt(a, super::RT_RY_LO, R::V0);
    load_rt(a, R::V0, super::capacity::SOURCE_END);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    store_rt(a, super::RT_RY_HI, R::V0);
    a.mov_imm64(R::V0, 0);
    a.mov_imm64(R::V1, 0);
    load_rt(a, R::V2, super::RT_FULL);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, ranges);
    a.bind_text_label(full_range);
    load_rt(a, R::V0, super::RT_OLD_FROM);
    load_rt(a, R::V1, super::RT_OLD_FROM_END);
    a.bind_text_label(ranges);
    store_rt(a, super::RT_RO_LO, R::V0);
    store_rt(a, super::RT_RO_HI, R::V1);
    // Objects promoted from here on are scanned by the Cheney pass.
    load_rt(a, R::V0, super::RT_OLD_TOP);
    store_rt(a, super::RT_OLD_SCAN, R::V0);
    load_rt(a, R::V0, super::old_runs::ALLOC);
    store_rt(a, super::old_runs::SCAN, R::V0);
    a.mov_imm64(R::V0, 1);
    store_rt(a, RT_COLLECTING, R::V0);

    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_M_RELOCATE_INDEX, R::V0);
    // Capture the stable segment's bound; discovery/forwarding belongs to
    // normal relocation quanta, not this pause.
    super::relocate_roots::start(a, l);
    // Previous remembered entries are discovered by normal relocation quanta.
    // AArch64 raw shadow roots still need synchronous rewriting. The x86
    // hook is empty: permanent literal RootCells use the captured bound.
    a.call_label(l.literals);
    // The pause ends here: to-space and promoted objects are scanned by the
    // open relocation phase (allocation quanta, load barriers), and
    // from-space is recycled when that phase finishes.
    super::scan::emit_cheney_init(a, l.fail);
    clear_scratch(a, l);
    a.mov_imm64(R::V0, 1);
    store_rt(a, super::RT_PHASE_OPEN, R::V0);
    super::phase::relocating(a, l, crate::gc_abi::Phase::ConcurrentRelocate);
    super::worker::open_phase(a, l);

    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_COLLECTING, R::V0);
    load_rt(a, R::V0, RT_COLLECTIONS);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, RT_COLLECTIONS, R::V0);
    load_rt(a, R::V0, RT_SAVED_SIZE);
    store_rt(a, RT_ALLOC_SIZE, R::V0);
    load_rt(a, R::V0, RT_SAVED_ELEMENTS);
    store_rt(a, elements, R::V0);
    load_rt(a, R::V0, RT_SAVED_TAG);
    store_rt(a, RT_ALLOC_TAG, R::V0);
    load_rt(a, R::V0, RT_SAVED_DESCRIPTOR);
    store_rt(a, descriptor, R::V0);
    super::worker::unlock(a, l);
    // The coordinated moving path enters at collect_moving, after the
    // sole-owner collection timer's BEGIN. Its rendezvous has its own timing
    // records. Check this owner's timer rather than the moving-mode flag,
    // which another owner can enable after the ticket is released.
    if a.gc_timing() {
        super::worker::lock(a, l);
        let no_sole_owner_timer = a.create_text_label();
        load_rt(a, R::V0, super::mutator_time::ACTIVE);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, no_sole_owner_timer);
        super::mutator_time::end(a, l);
        a.bind_text_label(no_sole_owner_timer);
        super::worker::unlock(a, l);
    }
    a.ret();
}

/// `finish`: explicitly drain relocation and wait for cleanup (lock held).
/// Normal allocation quanta may return with detached cleanup still pending.
/// V3 = runtime; private fresh-allocation arguments live outside this scratch.
fn emit_phase<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let closed = a.create_text_label();
    let retire = a.create_text_label();
    a.bind_text_label(l.quantum);
    super::retire::poll(a, l);
    load_rt(a, R::V0, super::RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, closed);
    super::worker::quantum_handoff(a, l);
    // Failed-spawn fallback also owns the ticket: phase-end journal handoff
    // may release it between bounded slices, even with no OS worker alive.
    super::worker::lock(a, l);
    let quantum_done = a.create_text_label();
    a.mov_imm64(R::V0, 1);
    store_rt(a, RT_COLLECTING, R::V0);
    a.mov_imm64(R::V0, super::QUANTUM_OBJECTS);
    store_rt(a, super::RT_BUDGET, R::V0);
    a.call_label(l.cheney);
    a.mov_imm64(R::V1, 0);
    store_rt(a, RT_COLLECTING, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, quantum_done);
    a.call_label(l.try_finish_relocation);
    a.bind_text_label(quantum_done);
    super::worker::unlock(a, l);
    a.ret();

    a.bind_text_label(l.finish);
    a.call_label(l.finish_relocation);
    super::retire::wait(a, l);
    super::uncommit::owner_poll_locked(a, l);
    a.ret();

    a.bind_text_label(l.finish_relocation);
    load_rt(a, R::V0, super::RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, closed);
    a.mov_imm64(R::V0, 1);
    store_rt(a, RT_COLLECTING, R::V0);
    a.call_label(l.publish_pending);
    super::drain::relocate(a, l);
    a.jmp_label(retire);

    // Allocation's end check never traces pending work. A worker completion
    // flag is only a hint: late barriers/publications can extend the frontier.
    a.bind_text_label(l.try_finish_relocation);
    load_rt(a, R::V0, super::moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, closed);
    load_rt(a, R::V0, super::RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, closed);
    super::copy::bump(a, super::RT_END_CHECKS, R::V0);
    let pending = a.create_text_label();
    relocation_pending(a, pending);
    a.jmp_label(retire);
    a.bind_text_label(pending);
    super::copy::bump(a, super::RT_END_RETRIES, R::V0);
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_WORKER_DONE, R::V0);
    super::worker::request_work(a);
    a.ret();

    a.bind_text_label(retire);
    super::registered_roots::finish(a, l, super::registered_roots::RELOCATION);
    clear_scratch(a, l);
    super::old_policy::observe(a, l);
    super::tenuring::observe(a);
    super::capacity::observe(a);
    super::start_policy::observe(a);
    super::phase::relocating(a, l, crate::gc_abi::Phase::Retire);
    super::retire::begin(a, l);
    a.mov_imm64(R::V0, 0);
    for field in [
        super::RT_PHASE_OPEN,
        super::RT_RY_LO,
        super::RT_RY_HI,
        super::RT_RO_LO,
        super::RT_RO_HI,
        RT_COLLECTING,
    ] {
        store_rt(a, field, R::V0);
    }
    if l.worker_entry().is_some() {
        store_rt(a, super::RT_WORKER_DONE, R::V0);
        store_rt(a, super::RT_MARK_VALID, R::V0);
        store_rt(a, super::young_mark::VALID, R::V0);
        // Fold the worker's private counters (it is stopped).
        for (total, part) in [
            (super::RT_PROMOTED, super::RT_W_PROMOTED),
            (super::RT_UNMARKED, super::RT_W_UNMARKED),
            (
                super::young_mark::UNMARKED,
                super::young_mark::WORKER_UNMARKED,
            ),
        ] {
            load_rt(a, R::V1, total);
            load_rt(a, R::V2, part);
            a.add_reg_reg(R::V1, R::V2);
            store_rt(a, total, R::V1);
            store_rt(a, part, R::V0);
        }
    }

    a.bind_text_label(closed);
    a.ret();
}

/// Publish the pending object, then empty the scratch root (cell 0).
pub(super) fn clear_scratch<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let running = a.create_text_label();
    let done = a.create_text_label();
    lea_rt(
        a,
        R::V1,
        RT_CTX + offset_of!(crate::gc_abi::MutatorContext, state),
    );
    a.atomic_load_acquire_u32(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, running);
    super::moving_round::require_coordinator(a, l);
    load_rt(a, R::V0, RT_ALLOC_ROOT + super::HANDLE_CELL);
    a.atomic_load_acquire(R::V0, R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.jmp_label(done);
    a.bind_text_label(running);
    a.call_label(l.publish_pending);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_ALLOC_ROOT + REQUEST_VALUE, R::V0);
    root_step(a, l, RT_ALLOC_ROOT, ROOT_STORE);
    a.bind_text_label(done);
}

const _: () = assert!(size_of::<MarkRecord>() == 168);

/// Constant-count metadata check under the sole owner's collector lock (or
/// the no-worker path). No object, root, remembered entry or page-chain walk.
/// A false completion hint leaves the phase/ranges open for normal quanta.
pub(super) fn relocation_pending<E: PortableAsm>(a: &mut E, pending: E::TextLabel) {
    super::large_trace::pending(a, pending);
    super::mark_frontier::pending(a, pending);
    super::old_reserve::pending(a, pending);
    load_rt(a, R::V0, super::RT_CS_KIND);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    super::copy_claims::pending(a, pending);
    super::registered_roots::pending(a, super::registered_roots::RELOCATION, pending);
    load_rt(a, R::V0, super::RT_RS_INDEX);
    load_rt(a, R::V1, super::RT_REMSET_PREV_LEN);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, pending);
    super::relocate_pages::pending(a, pending);
    let b_done = a.create_text_label();
    load_rt(a, R::V0, super::RT_CB_PAGE);
    load_rt(a, R::V1, super::RT_COPY_LOW);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, b_done);
    a.jcc_label(C::Above, pending);
    page_pending(a, super::RT_CB_OFF, pending);
    a.bind_text_label(b_done);
}

/// V0 = page index. Compare its scan offset with its allocated-top metadata.
fn page_pending<E: PortableAsm>(a: &mut E, cursor: usize, pending: E::TextLabel) {
    super::scan::page_ptr(a, R::V8, R::V0, R::V1);
    load_rt(a, R::V0, cursor);
    a.load_ptr_disp32(
        R::V1,
        R::V8,
        d(offset_of!(crate::gc_abi::AllocationPage, allocated_top)),
    );
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, pending);
}
