//! Source-run cleanup after the sole owner's quiescent Retire adoption.
//!
//! The owner closes all moving ranges before resuming. Cleanup never walks
//! live roots or changes the owner's phase; it zeros one word or advances one
//! constant-size stage per budget unit. A new flip waits for cleanup. This is
//! not general N-owner lease/segment retirement or an uncommit protocol.
use super::{Labels, d, load_rt, store_rt};
use crate::gc_abi::{AllocationPage, ForwardTable, PAGE_READY};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

// 1/2 select mutator/worker source page; 3/4 payload; 5/6 start bitmap;
// 7/8 forwarding cells; 9 mark arena; 10 Old heap; 11/12 source Old
// remembered-slot bitmaps; 13 Old starts; 14 Old owner hints; 15 Young roles;
// 16 allocated Old roles; 17 complete.
pub(super) const DONE: u64 = 17;
// Run starts own whole metadata words. Round only within the final source
// page, so cleanup cannot erase a neighboring destination page.
const _: () = assert!(super::HEAP_PAGE_BYTES.is_multiple_of(1024));

fn stage<E: PortableAsm>(a: &mut E, value: u64) {
    a.mov_imm64(R::V0, value);
    store_rt(a, super::RT_RETIRE_STAGE, R::V0);
}

/// V7 = start address, V1 = number of qwords. Retain a half-open zero range.
fn range<E: PortableAsm>(a: &mut E, next: u64) {
    store_rt(a, super::RT_RETIRE_CURSOR, R::V7);
    a.shl_reg_imm8(R::V1, 3);
    a.add_reg_reg(R::V1, R::V7);
    store_rt(a, super::RT_RETIRE_END, R::V1);
    stage(a, next);
}

fn page<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, super::RT_SCAN_INDEX);
    super::scan::page_ptr(a, R::V8, R::V0, R::V1);
}

pub(super) fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    super::copy_claims::require_empty(a, l);
    super::metadata_leases::begin_retire(a, l);
    super::store_rt_imm(a, super::old_runs::YOUNG_RETURNED, 0);
    load_rt(a, R::V0, super::RT_RETIRE_STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, super::RT_FROM_BASE);
    store_rt(a, super::RT_SCAN_INDEX, R::V0);
    stage(a, 1);
    if l.worker.is_some() {
        super::worker::request_work(a);
    }
}

/// Owner only, at an allocation safepoint and under the collector lock.
pub(super) fn complete<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, super::RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    super::collect::clear_scratch(a, l);
    super::alloc::release_empty_tail(a, l);
    super::phase::idle(a, l);
    stage(a, 0);
    super::native_completion::publish(a, l);
    let done = a.create_text_label();
    load_rt(a, R::V0, super::RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    // A worker may already be parked at cleanup DONE. Explicit finish can
    // return without another allocation, so deliver the resume wake here.
    super::worker::request_work(a);
    super::worker::wake_if_pending(a, l);
    a.bind_text_label(done);
}

/// A normal allocation services one fallback quantum or adopts completed
/// worker cleanup. It may return while cleanup is pending.
pub(super) fn poll<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    let adopt = a.create_text_label();
    let release = a.create_text_label();
    load_rt(a, R::V0, super::RT_RETIRE_STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    super::worker::lock(a, l);
    load_rt(a, R::V0, super::RT_RETIRE_STAGE);
    a.cmp_reg_imm8(R::V0, DONE as i8);
    a.jcc_label(C::Equal, adopt);
    if l.worker.is_some() {
        load_rt(a, R::V0, super::RT_WORKER_ALIVE);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, release);
    }
    a.mov_imm64(R::V0, super::QUANTUM_OBJECTS);
    store_rt(a, super::RT_BUDGET, R::V0);
    a.call_label(l.retire_step);
    load_rt(a, R::V0, super::RT_RETIRE_STAGE);
    a.cmp_reg_imm8(R::V0, DONE as i8);
    a.jcc_label(C::NotEqual, release);
    a.bind_text_label(adopt);
    super::moving_round::branch_enabled(a, release);
    complete(a, l);
    a.bind_text_label(release);
    super::worker::unlock(a, l);
    a.bind_text_label(done);
}

/// Explicit finish / next flip: caller holds the lock. Retain that contract
/// on return, but release between bounded cleanup quanta so the worker runs.
pub(super) fn wait<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let top = a.create_text_label();
    let done = a.create_text_label();
    let adopt = a.create_text_label();
    a.bind_text_label(top);
    load_rt(a, R::V0, super::RT_RETIRE_STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm8(R::V0, DONE as i8);
    a.jcc_label(C::Equal, adopt);
    a.mov_imm64(R::V0, super::QUANTUM_OBJECTS);
    store_rt(a, super::RT_BUDGET, R::V0);
    a.call_label(l.retire_step);
    super::worker::unlock(a, l);
    super::worker::wake_if_pending(a, l);
    if let Some(w) = &l.worker {
        a.call_label(w.hooks.relax);
    }
    super::worker::lock(a, l);
    a.jmp_label(top);
    a.bind_text_label(adopt);
    complete(a, l);
    a.bind_text_label(done);
}

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let top = a.create_text_label();
    let done = a.create_text_label();
    let paused = a.create_text_label();
    let zero = a.create_text_label();
    let zero_done = a.create_text_label();
    let selectors = [a.create_text_label(), a.create_text_label()];
    let transitions: Vec<_> = (3..DONE).map(|_| a.create_text_label()).collect();
    let large_sweep = a.create_text_label();
    a.bind_text_label(l.retire_step);
    super::metadata_leases::before_retire(a, paused);
    a.bind_text_label(top);
    load_rt(a, R::V0, super::large_object::SWEEP_RUNNING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, large_sweep);
    load_rt(a, R::V0, super::RT_RETIRE_STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm8(R::V0, DONE as i8);
    a.jcc_label(C::Equal, done);
    a.jcc_label(C::Above, l.fail);
    super::scan::spend(a, paused);
    super::copy::bump(a, super::RT_RETIRE_UNITS, R::V9);
    for (i, label) in selectors.iter().enumerate() {
        a.cmp_reg_imm8(R::V0, (i + 1) as i8);
        a.jcc_label(C::Equal, *label);
    }
    a.jmp_label(zero);
    for (i, label) in selectors.iter().enumerate() {
        let have = a.create_text_label();
        a.bind_text_label(*label);
        load_rt(a, R::V0, super::RT_SCAN_INDEX);
        load_rt(
            a,
            R::V1,
            if i == 0 {
                super::RT_FROM_END
            } else {
                super::capacity::SOURCE_END
            },
        );
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::Below, have);
        if i == 0 {
            load_rt(a, R::V0, super::RT_FROM_COPY_LOW);
            store_rt(a, super::RT_SCAN_INDEX, R::V0);
            stage(a, 2);
        } else {
            load_rt(a, R::V0, super::pool::FROM_BANK);
            super::collect::arena(a, R::V7, R::V0);
            // MarkRecord is 21 qwords; only the captured used prefix is dead.
            load_rt(a, R::V1, super::RT_FROM_MARKS);
            a.mov_reg_reg(R::V0, R::V1);
            a.shl_reg_imm8(R::V0, 4);
            a.mov_reg_reg(R::V2, R::V1);
            a.shl_reg_imm8(R::V2, 2);
            a.add_reg_reg(R::V1, R::V0);
            a.add_reg_reg(R::V1, R::V2);
            range(a, 9);
        }
        a.jmp_label(top);
        a.bind_text_label(have);
        page(a);
        a.load_ptr_disp32(R::V7, R::V8, d(offset_of!(AllocationPage, start)));
        a.load_ptr_disp32(R::V1, R::V8, d(offset_of!(AllocationPage, allocated_top)));
        a.sub_reg_reg(R::V1, R::V7);
        a.shr_reg_imm8(R::V1, 3);
        load_rt(a, R::V0, super::RT_HEAP_BASE);
        a.add_reg_reg(R::V7, R::V0);
        range(a, 3 + i as u64);
        a.jmp_label(top);
    }
    a.bind_text_label(zero);
    load_rt(a, R::V1, super::RT_RETIRE_CURSOR);
    load_rt(a, R::V2, super::RT_RETIRE_END);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::AboveOrEqual, zero_done);
    let ordinary_word = a.create_text_label();
    let cleared = a.create_text_label();
    a.cmp_reg_imm8(R::V0, 15);
    a.jcc_label(C::Below, ordinary_word);
    super::metadata::retire_page(a, l, paused);
    super::pool::release(a, l);
    a.mov_imm64(R::V2, 0);
    a.atomic_store_release(R::V1, R::V2);
    a.jmp_label(cleared);
    a.bind_text_label(ordinary_word);
    a.mov_imm64(R::V2, 0);
    a.store_ptr_disp32(R::V1, 0, R::V2);
    a.bind_text_label(cleared);
    a.add_reg_imm32(R::V1, 8);
    store_rt(a, super::RT_RETIRE_CURSOR, R::V1);
    a.jmp_label(top);
    a.bind_text_label(zero_done);
    for (i, label) in transitions.iter().enumerate() {
        a.cmp_reg_imm8(R::V0, (i + 3) as i8);
        a.jcc_label(C::Equal, *label);
    }
    a.jmp_label(l.fail);
    for i in 0..2 {
        a.bind_text_label(transitions[i]); // payload -> start bitmap
        page(a);
        a.load_ptr_disp32(R::V7, R::V8, d(offset_of!(AllocationPage, starts)));
        a.mov_imm64(R::V1, super::STARTS_WORDS);
        range(a, 5 + i as u64);
        a.jmp_label(top);
        a.bind_text_label(transitions[i + 2]); // bitmap -> forwarding cells
        page(a);
        a.load_ptr_disp32(R::V9, R::V8, d(offset_of!(AllocationPage, forward_table)));
        a.load_ptr_disp32(R::V7, R::V9, d(offset_of!(ForwardTable, cells)));
        a.load_ptr_disp32(R::V1, R::V9, d(offset_of!(ForwardTable, used)));
        a.shl_reg_imm8(R::V1, 2);
        range(a, 7 + i as u64);
        a.jmp_label(top);
        a.bind_text_label(transitions[i + 4]); // fully clean -> READY
        page(a);
        a.load_ptr_disp32(R::V9, R::V8, d(offset_of!(AllocationPage, forward_table)));
        a.mov_imm64(R::V0, 0);
        a.store_ptr_disp32(R::V9, d(offset_of!(ForwardTable, used)), R::V0);
        a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, owner)), R::V0);
        a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, start)));
        a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, allocated_top)), R::V0);
        a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
        a.add_reg_imm32(R::V0, super::geometry(a).pages as i32);
        a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, incarnation)), R::V0);
        a.mov_imm64(R::V0, PAGE_READY);
        a.mov_reg_reg(R::V1, R::V8);
        a.add_reg_imm32(R::V1, d(offset_of!(AllocationPage, state)));
        a.atomic_store_release(R::V1, R::V0);
        super::copy::bump(a, super::RT_SCAN_INDEX, R::V0);
        stage(a, 1 + i as u64);
        a.jmp_label(top);
    }
    a.bind_text_label(transitions[6]); // arena -> first source run, or done
    let all_done = a.create_text_label();
    load_rt(a, R::V0, super::RT_FULL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, all_done);
    let old_heap = a.create_text_label();
    a.bind_text_label(old_heap);
    load_rt(a, R::V7, super::RT_OLD_FROM);
    load_rt(a, R::V1, super::RT_OLD_FROM_END);
    a.sub_reg_reg(R::V1, R::V7);
    a.shr_reg_imm8(R::V1, 3);
    load_rt(a, R::V0, super::RT_HEAP_BASE);
    a.add_reg_reg(R::V7, R::V0);
    range(a, 10);
    a.jmp_label(top);
    // Old source addresses are now detached. Clear their membership in both
    // sets, never the live destination run where resumed stores can register.
    // An unused source reservation tail may already belong to a destination;
    // only the actual allocated prefix is retired.
    for i in 0..2 {
        a.bind_text_label(transitions[7 + i]);
        load_rt(a, R::V7, super::RT_OLD_FROM);
        a.shr_reg_imm8(R::V7, 9); // 512 heap bytes per bitmap qword.
        a.shl_reg_imm8(R::V7, 3);
        a.add_reg_reg(R::V7, R::V3);
        a.add_reg_imm32(
            R::V7,
            (super::layout(a).rbits + i as u64 * super::geometry(a).rbits_words * 8) as i32,
        );
        load_rt(a, R::V1, super::RT_OLD_FROM_END);
        load_rt(a, R::V0, super::RT_OLD_FROM);
        a.sub_reg_reg(R::V1, R::V0);
        a.add_reg_imm32(R::V1, 511);
        a.shr_reg_imm8(R::V1, 9);
        range(a, 11 + i as u64);
        a.jmp_label(top);
    }
    // The captured Old source span owns complete 1-KiB metadata cards.
    for (index, base) in [
        (9, super::layout(a).starts),
        (10, super::layout(a).owner_cards),
    ] {
        a.bind_text_label(transitions[index]);
        load_rt(a, R::V7, super::RT_OLD_FROM);
        load_rt(a, R::V1, super::RT_OLD_FROM_END);
        a.sub_reg_reg(R::V1, R::V7);
        a.add_reg_imm32(R::V1, 1023);
        a.shr_reg_imm8(R::V1, 10);
        if index == 9 {
            a.sub_reg_imm32(R::V7, super::HEAP_PAGE_BYTES as i32);
        }
        a.shr_reg_imm8(R::V7, 10);
        a.shl_reg_imm8(R::V7, 3);
        a.add_reg_reg(R::V7, R::V3);
        a.add_reg_imm32(R::V7, base as i32);
        range(a, index as u64 + 4);
        a.jmp_label(top);
    }
    a.bind_text_label(transitions[11]);
    let old_roles = a.create_text_label();
    load_rt(a, R::V0, super::old_runs::YOUNG_RETURNED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, old_roles);
    a.bind_text_label(all_done);
    // Keep the complete source Young run occupied until its used pages and
    // mark bank are clean. Unused gaps are already Ready and contain no object.
    load_rt(a, R::V7, super::RT_FROM_BASE);
    load_rt(a, R::V1, super::capacity::SOURCE_END);
    a.sub_reg_reg(R::V1, R::V7);
    a.shl_reg_imm8(R::V7, 3);
    a.add_reg_reg(R::V7, R::V3);
    a.add_reg_imm32(R::V7, (super::layout(a).generations + 8) as i32);
    range(a, 15);
    a.jmp_label(top);
    a.bind_text_label(transitions[12]);
    super::store_rt_imm(a, super::old_runs::YOUNG_RETURNED, 1);
    a.bind_text_label(old_roles);
    let truly_done = a.create_text_label();
    load_rt(a, R::V0, super::RT_FULL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, truly_done);
    load_rt(a, R::V7, super::RT_OLD_FROM);
    a.shr_reg_imm8(R::V7, 17);
    a.shl_reg_imm8(R::V7, 3);
    a.add_reg_reg(R::V7, R::V3);
    a.add_reg_imm32(R::V7, super::layout(a).generations as i32);
    load_rt(a, R::V1, super::RT_OLD_FROM_END);
    load_rt(a, R::V0, super::RT_OLD_FROM);
    a.sub_reg_reg(R::V1, R::V0);
    a.add_reg_imm32(R::V1, (super::HEAP_PAGE_BYTES - 1) as i32);
    a.shr_reg_imm8(R::V1, 17);
    range(a, 16);
    a.jmp_label(top);
    a.bind_text_label(transitions[13]);
    super::copy::bump(a, super::old_runs::RETIRE, R::V0);
    load_rt(a, R::V1, super::old_runs::FROM_COUNT);
    let exhausted = a.create_text_label();
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, exhausted);
    super::old_runs::source_span(a);
    a.jmp_label(old_heap);
    a.bind_text_label(exhausted);
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_FULL, R::V0);
    a.bind_text_label(truly_done);
    super::store_rt_imm(a, super::large_object::SWEEP_RUNNING, 1);
    a.bind_text_label(large_sweep);
    super::large_sweep::step(a, l, paused);
    super::store_rt_imm(a, super::large_object::SWEEP_RUNNING, 0);
    stage(a, DONE);
    a.bind_text_label(done);
    a.mov_imm64(R::V0, 1);
    a.ret();
    a.bind_text_label(paused);
    a.mov_imm64(R::V0, 0);
    a.ret();
}
