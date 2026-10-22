//! M-G6 step 4: concurrent marking (worker builds only).
//!
//! A short mark-start pause (the worker stopped) clears the mark bitmap and
//! marks the referents of every root cell and collection-literal cell: the
//! snapshot. The worker then traces the heap in budgeted quanta while the
//! mutator runs. Snapshot-at-the-beginning: objects allocated while
//! marking are marked at allocation (`alloc_black`), and `field_store`
//! marks the reference it overwrites (SATB pre-barrier, under the
//! collector lock). The relocation pause finishes whatever the worker left
//! (counted), and the relocation that follows uses the mark: remembered
//! fields whose referent is unmarked are dropped instead of evacuated, and
//! copying an unmarked object is counted as an invariant violation. Marking
//! and relocation never overlap: marking starts only with no phase open,
//! and the relocation pause ends it.
use super::{
    L, Labels, MARK_TRIGGER_PAGES, MBITS_WORDS, MSTACK_ENTRIES, RT_BUDGET, RT_HEAP_BASE,
    RT_LIT_MARK, RT_M_COUNT, RT_M_FIELD, RT_M_OBJ, RT_MARK_ACTIVE, RT_MARK_CYCLES, RT_MARK_DONE,
    RT_MARK_DRAINS, RT_MARK_VALID, RT_MARKED, RT_MSTACK_LEN, RT_NEXT_PAGE, RT_PHASE_OPEN,
    RT_SCAN_INDEX, RT_SEGMENT, RT_SPACE_BASE, RT_WORKER_ALIVE,
    copy::{DESCRIPTOR, PAYLOAD, bump},
    d,
};
use crate::gc_abi::RootSegment;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

/// `word` = the bitmap word of payload offset `off`, `bit` = its mask;
/// then `test word, bit` (NotEqual: marked). Clobbers V1.
pub(super) fn test_mark<E: PortableAsm>(a: &mut E, off: R, bit: R, word: R) {
    a.mov_reg_reg(R::V1, off);
    a.shr_reg_imm8(R::V1, 4);
    a.and_reg_imm32(R::V1, 63);
    a.mov_imm64(bit, 1);
    a.shl_reg_count(bit);
    a.mov_reg_reg(word, off);
    a.shr_reg_imm8(word, 10);
    a.shl_reg_imm8(word, 3);
    a.add_reg_reg(word, R::V3);
    a.load_ptr_disp32(word, word, L.mbits as i32);
    a.test_reg_reg(word, bit);
}

/// Atomically set the mark bit of payload offset `off` (preserved); V0 =
/// the previous bitmap word, `bit` = the mask. Clobbers V1, V8, V9, `addr`.
fn set_mark<E: PortableAsm>(a: &mut E, off: R, addr: R, bit: R) {
    a.mov_reg_reg(R::V1, off);
    a.shr_reg_imm8(R::V1, 4);
    a.and_reg_imm32(R::V1, 63);
    a.mov_imm64(bit, 1);
    a.shl_reg_count(bit);
    a.mov_reg_reg(addr, off);
    a.shr_reg_imm8(addr, 10);
    a.shl_reg_imm8(addr, 3);
    a.add_reg_reg(addr, R::V3);
    a.add_reg_imm32(addr, L.mbits as i32);
    a.atomic_fetch_or(addr, bit);
}

/// In `alloc_raw`, V0 = the new object's bits (preserved): while marking,
/// a new object is born marked. Clobbers V1, V2, V6..V9.
pub(super) fn alloc_black<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if l.worker.is_none() {
        return;
    }
    let skip = a.create_text_label();
    a.load_ptr_disp32(R::V2, R::V3, d(RT_MARK_ACTIVE));
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, skip);
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    set_mark(a, R::V2, R::V6, R::V7);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.bind_text_label(skip);
}

/// In `alloc` (V3 = runtime): start a mark cycle once enough of the
/// semispace is in use and no phase or cycle is open.
pub(super) fn trigger<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    let skip = a.create_text_label();
    a.load_ptr_disp32(R::V0, R::V3, d(RT_WORKER_ALIVE));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, skip);
    for flag in [RT_PHASE_OPEN, RT_MARK_ACTIVE] {
        a.load_ptr_disp32(R::V0, R::V3, d(flag));
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, skip);
    }
    a.load_ptr_disp32(R::V0, R::V3, d(RT_NEXT_PAGE));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_SPACE_BASE));
    a.sub_reg_reg(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, MARK_TRIGGER_PAGES as i32);
    a.jcc_label(C::Below, skip);
    a.call_label(w.mark_start);
    a.bind_text_label(skip);
}

/// In the relocation pause (lock held, before the flip): finish an open
/// mark cycle on the mutator if the worker left work, then validate it.
pub(super) fn drain<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    let none = a.create_text_label();
    let drained = a.create_text_label();
    a.load_ptr_disp32(R::V0, R::V3, d(RT_MARK_ACTIVE));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, none);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_MSTACK_LEN));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, drained);
    bump(a, RT_MARK_DRAINS, R::V0);
    a.mov_imm64(R::V0, u64::MAX);
    a.store_ptr_disp32(R::V3, d(RT_BUDGET), R::V0);
    a.call_label(w.mark_loop);
    a.bind_text_label(drained);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_MARK_ACTIVE), R::V0);
    a.store_ptr_disp32(R::V3, d(RT_MARK_DONE), R::V0);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_MARK_VALID), R::V0);
    a.bind_text_label(none);
}

pub(super) fn emit_mark<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    // mark_ref: V2 = payload offset (preserved). Mark it; if it was not
    // marked, push it. Caller holds the lock. Clobbers V0, V1, V6..V9.
    let known = a.create_text_label();
    a.bind_text_label(w.mark_ref);
    set_mark(a, R::V2, R::V6, R::V7);
    a.test_reg_reg(R::V0, R::V7);
    a.jcc_label(C::NotEqual, known);
    bump(a, RT_MARKED, R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_MSTACK_LEN));
    a.cmp_reg_imm32(R::V0, MSTACK_ENTRIES as i32);
    a.jcc_label(C::AboveOrEqual, l.fail);
    a.mov_reg_reg(R::V1, R::V0);
    a.shl_reg_imm8(R::V1, 3);
    a.add_reg_reg(R::V1, R::V3);
    a.store_ptr_disp32(R::V1, L.mstack as i32, R::V2);
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_MSTACK_LEN), R::V0);
    a.bind_text_label(known);
    a.ret();

    emit_mark_loop(a, l, w.mark_loop, w.mark_ref);
    emit_mark_start(a, l, w.mark_start, w.mark_ref);
}

/// mark_loop: pop and trace objects until the stack is empty (V0 = 1) or
/// the budget runs out (V0 = 0). Caller holds the lock.
fn emit_mark_loop<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    entry: E::TextLabel,
    mark_ref: E::TextLabel,
) {
    let _ = l;
    let top = a.create_text_label();
    let fields = a.create_text_label();
    let field_loop = a.create_text_label();
    let next_field = a.create_text_label();
    let empty = a.create_text_label();
    let paused = a.create_text_label();
    a.bind_text_label(entry);
    a.bind_text_label(top);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_MSTACK_LEN));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    super::scan::spend(a, paused);
    a.sub_reg_imm8(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_MSTACK_LEN), R::V0);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.load_ptr_disp32(R::V2, R::V0, L.mstack as i32);
    a.store_ptr_disp32(R::V3, d(RT_M_OBJ), R::V2);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V1, R::V2);
    a.load_ptr_disp32(R::V0, R::V1, DESCRIPTOR);
    a.load_ptr_disp32(R::V6, R::V1, PAYLOAD);
    a.shr_reg_imm8(R::V6, 3);
    a.store_ptr_disp32(R::V3, d(RT_M_COUNT), R::V6);
    a.mov_imm64(R::V7, 0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, fields);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::NotEqual, top);
    a.mov_imm64(R::V7, 1);
    a.bind_text_label(fields);
    a.store_ptr_disp32(R::V3, d(RT_M_FIELD), R::V7);
    a.bind_text_label(field_loop);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_M_FIELD));
    a.load_ptr_disp32(R::V6, R::V3, d(RT_M_COUNT));
    a.cmp_reg_reg(R::V7, R::V6);
    a.jcc_label(C::AboveOrEqual, top);
    a.shl_reg_imm8(R::V7, 3);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V7, R::V1);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_M_OBJ));
    a.add_reg_reg(R::V7, R::V1);
    a.load_ptr_disp32(R::V0, R::V7, 0);
    super::enums::heap_offset(a, R::V0, R::V2, next_field);
    a.call_label(mark_ref);
    a.bind_text_label(next_field);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_M_FIELD));
    a.add_reg_imm32(R::V7, 1);
    a.store_ptr_disp32(R::V3, d(RT_M_FIELD), R::V7);
    a.jmp_label(field_loop);
    a.bind_text_label(empty);
    a.mov_imm64(R::V0, 1);
    a.ret();
    a.bind_text_label(paused);
    a.mov_imm64(R::V0, 0);
    a.ret();
}

/// mark_start (mutator, from `alloc`): the snapshot pause.
fn emit_mark_start<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    entry: E::TextLabel,
    mark_ref: E::TextLabel,
) {
    let cells = a.create_text_label();
    let next = a.create_text_label();
    let roots_done = a.create_text_label();
    a.bind_text_label(entry);
    super::worker::lock(a, l);
    a.mov_reg_reg(R::V7, R::V3);
    a.add_reg_imm32(R::V7, L.mbits as i32);
    a.mov_imm64(R::V1, MBITS_WORDS);
    a.call_label(l.zero);
    a.mov_imm64(R::V0, 0);
    for field in [RT_MSTACK_LEN, RT_MARK_DONE, RT_MARK_VALID] {
        a.store_ptr_disp32(R::V3, d(field), R::V0);
    }
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_MARK_ACTIVE), R::V0);
    a.store_ptr_disp32(R::V3, d(RT_SCAN_INDEX), R::V0);
    bump(a, RT_MARK_CYCLES, R::V0);
    // Root cells [1, used): reference bits, or an external tagged pointer.
    a.bind_text_label(cells);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SCAN_INDEX));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_SEGMENT));
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(RootSegment, used)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, roots_done);
    super::collect::cell_ptr(a, R::V1, R::V0);
    a.load_ptr_disp32(R::V0, R::V1, 0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, next);
    a.bt_reg_imm8(R::V0, 0);
    a.jcc_label(C::Below, next);
    a.shr_reg_imm8(R::V0, 16);
    a.mov_reg_reg(R::V2, R::V0);
    a.call_label(mark_ref);
    a.bind_text_label(next);
    bump(a, RT_SCAN_INDEX, R::V0);
    a.jmp_label(cells);
    a.bind_text_label(roots_done);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_LIT_MARK), R::V0);
    a.call_label(l.literals);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_LIT_MARK), R::V0);
    super::worker::request_work(a);
    super::worker::unlock(a, l);
    a.ret();
}
