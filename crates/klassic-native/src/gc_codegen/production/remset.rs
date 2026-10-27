//! G07: double-buffered remembered sets for `--gc=generational`.
//!
//! Each set owns a separate slot bitmap. A store registers in the current
//! set before publication even when that slot remains in the sealed previous
//! set. Normal relocation quanta consume previous entries with the receiving
//! actor's copier; exact-word CAS never overwrites a newer mutator store.
//! Clearing a previous bit cannot erase current registration. The next flip
//! happens only after consumption and relocation finish. This is the sole
//! owner's fixed-storage protocol, not N-owner buffer sealing or retirement.
use super::{
    Labels, REMSET_ENTRIES, RT_HEAP_BASE, RT_MARK_VALID, RT_REMSET_BASE, RT_REMSET_DROPPED,
    RT_REMSET_FLIPPED, RT_REMSET_FLIPS, RT_REMSET_KEPT, RT_REMSET_LEN, RT_REMSET_PREV_LEN,
    RT_RS_INDEX, copy::bump, d, fields::young_offset, load_rt, store_rt,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

/// Address of the slot-bit word of `slot` in `word` and its mask in `mask`
/// (V3 = runtime). Clobbers V0 and V1 (the shift count).
fn slot_bit<E: PortableAsm>(a: &mut E, slot: R, word: R, mask: R, previous: bool) {
    a.mov_reg_reg(word, slot);
    load_rt(a, R::V0, RT_HEAP_BASE);
    a.sub_reg_reg(word, R::V0);
    a.mov_reg_reg(R::V1, word);
    a.shr_reg_imm8(R::V1, 3);
    a.and_reg_imm32(R::V1, 63);
    a.mov_imm64(mask, 1);
    a.shl_reg_count(mask);
    a.shr_reg_imm8(word, 9);
    a.shl_reg_imm8(word, 3);
    a.add_reg_reg(word, R::V3);
    a.add_reg_imm32(word, super::layout(a).rbits as i32);
    let first = a.create_text_label();
    load_rt(a, R::V0, RT_REMSET_BASE);
    a.mov_imm64(R::V1, super::layout(a).remset);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(if previous { C::NotEqual } else { C::Equal }, first);
    a.add_reg_imm32(word, (super::geometry(a).rbits_words * 8) as i32);
    a.bind_text_label(first);
}

/// Forget that `slot` (preserved) is in a set: clear its slot bit. Only the
/// previous-set consumer calls this under the collector lock. Clobbers V0, V1, V8, V9.
fn forget<E: PortableAsm>(a: &mut E, slot: R) {
    slot_bit(a, slot, R::V8, R::V9, true);
    a.mov_imm64(R::V0, u64::MAX);
    a.xor_reg_reg(R::V9, R::V0);
    a.load_ptr_disp32(R::V0, R::V8, 0);
    a.and_reg_reg(R::V0, R::V9);
    a.store_ptr_disp32(R::V8, 0, R::V0);
}

/// Append the field address in `field` to the current set (V3 = runtime),
/// unless the slot is already in the current set: a slot bit (set atomically by
/// whichever thread gets there first) makes every slot appear at most once,
/// so a loop that keeps storing Young values into Old slots cannot grow the
/// set. `scratch` must be V1; clobbers V0 and `scratch`/`index`; preserves
/// the rest. With a worker both threads append, so the entry is reserved by
/// a CAS on the length.
pub(super) fn remember<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    field: R,
    scratch: R,
    index: R,
) {
    assert_eq!(scratch, R::V1, "the slot bit shifts by CL");
    let skip = a.create_text_label();
    for reg in [R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(reg);
    }
    slot_bit(a, field, R::V11, R::V10, false);
    a.atomic_fetch_or(R::V11, R::V10);
    a.test_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, skip);
    if l.worker.is_none() {
        load_rt(a, index, RT_REMSET_LEN);
        a.cmp_reg_imm32(index, REMSET_ENTRIES as i32);
        a.jcc_label(C::AboveOrEqual, l.old_full);
        a.mov_reg_reg(scratch, index);
        a.shl_reg_imm8(scratch, 3);
        a.add_reg_reg(scratch, R::V3);
        load_rt(a, R::V0, RT_REMSET_BASE);
        a.add_reg_reg(scratch, R::V0);
        a.store_ptr_disp32(scratch, 0, field);
        a.add_reg_imm32(index, 1);
        store_rt(a, RT_REMSET_LEN, index);
    } else {
        let retry = a.create_text_label();
        a.mov_reg_reg(scratch, R::V3);
        a.add_reg_imm32(scratch, d(RT_REMSET_LEN));
        a.bind_text_label(retry);
        a.load_ptr_disp32(R::V0, scratch, 0);
        a.cmp_reg_imm32(R::V0, REMSET_ENTRIES as i32);
        a.jcc_label(C::AboveOrEqual, l.old_full);
        a.mov_reg_reg(index, R::V0);
        a.add_reg_imm32(index, 1);
        a.atomic_compare_exchange(scratch, index);
        a.sub_reg_imm8(index, 1);
        a.cmp_reg_reg(R::V0, index);
        a.jcc_label(C::NotEqual, retry);
        a.shl_reg_imm8(index, 3);
        a.add_reg_reg(index, R::V3);
        load_rt(a, R::V0, RT_REMSET_BASE);
        a.add_reg_reg(index, R::V0);
        a.store_ptr_disp32(index, 0, field);
    }
    a.bind_text_label(skip);
    for reg in [R::V11, R::V10, R::V9, R::V8] {
        a.pop_reg(reg);
    }
}

/// In `collect`, for a Young-only collection (V3 = runtime; clobbers V0,
/// V1): previous := current, current := empty.
pub(super) fn emit_flip<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, RT_REMSET_LEN);
    store_rt(a, RT_REMSET_PREV_LEN, R::V0);
    load_rt(a, R::V1, RT_REMSET_FLIPPED);
    a.add_reg_reg(R::V1, R::V0);
    store_rt(a, RT_REMSET_FLIPPED, R::V1);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_REMSET_LEN, R::V0);
    store_rt(a, RT_RS_INDEX, R::V0);
    load_rt(a, R::V0, RT_REMSET_BASE);
    a.mov_imm64(R::V1, super::layout(a).remset + super::layout(a).remset2);
    a.sub_reg_reg(R::V1, R::V0);
    store_rt(a, RT_REMSET_BASE, R::V1);
    bump(a, RT_REMSET_FLIPS, R::V0);
}

/// Previous-set entries are roots of a Young collection: forward each
/// referent and re-register the entry in the current set only while its slot
/// still points into Young. Empties the previous set.
pub(super) fn scan<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    forward: E::TextLabel,
    paused: E::TextLabel,
) {
    let next = a.create_text_label();
    let keep = a.create_text_label();
    let done = a.create_text_label();
    let lp = a.create_text_label();
    a.bind_text_label(lp);
    load_rt(a, R::V0, RT_RS_INDEX);
    load_rt(a, R::V1, RT_REMSET_PREV_LEN);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, done);
    super::scan::spend(a, paused);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    load_rt(a, R::V1, RT_REMSET_BASE);
    a.mov_imm64(R::V7, super::layout(a).remset + super::layout(a).remset2);
    a.sub_reg_reg(R::V7, R::V1);
    a.add_reg_reg(R::V0, R::V7);
    a.load_ptr_disp32(R::V7, R::V0, 0);
    a.mov_reg_reg(R::V0, R::V7);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    a.cmp_reg_imm32(R::V0, super::HEAP_PAGE_BYTES as i32);
    a.jcc_label(C::Below, l.fail);
    a.cmp_reg_imm32(R::V0, super::geometry(a).heap_bytes as i32);
    a.jcc_label(C::AboveOrEqual, l.fail);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    // Resolve/validate the holder before reading a potentially stale slot.
    a.call_label(l.remembered_owner);
    a.mov_reg_reg(R::V6, R::V0);
    // The entry leaves the set; a kept one is appended afresh below.
    forget(a, R::V7);
    let owner_valid = a.create_text_label();
    a.test_reg_reg(R::V6, R::V6);
    a.jcc_label(C::NotEqual, owner_valid);
    bump(a, RT_REMSET_DROPPED, R::V2);
    a.jmp_label(next);
    a.bind_text_label(owner_valid);
    a.atomic_load_acquire(R::V0, R::V7);
    young_offset(a, R::V0, R::V1, next);
    if l.worker.is_some() {
        // After a complete mark, an unmarked referent is garbage and so is
        // every Old holder of it: drop the entry instead of copying it.
        let live = a.create_text_label();
        load_rt(a, R::V2, RT_MARK_VALID);
        a.test_reg_reg(R::V2, R::V2);
        a.jcc_label(C::Equal, live);
        // A holder born after this snapshot cannot be declared dead by it.
        a.mov_reg_reg(R::V2, R::V6);
        super::slot_owner::newer(a, R::V2);
        a.jcc_label(C::Above, live);
        // The sealed Old mark describes from-space. Loads/stores may have
        // already resolved this slot into to-space, which can also hold fresh
        // allocations made after that snapshot was sealed.
        a.mov_reg_reg(R::V2, R::V1);
        load_rt(a, R::V6, super::RT_RY_LO);
        a.sub_reg_reg(R::V2, R::V6);
        load_rt(a, R::V6, super::RT_RY_HI);
        a.push_reg(R::V1);
        load_rt(a, R::V1, super::RT_RY_LO);
        a.sub_reg_reg(R::V6, R::V1);
        a.pop_reg(R::V1);
        a.cmp_reg_reg(R::V2, R::V6);
        a.jcc_label(C::AboveOrEqual, live);
        a.mov_reg_reg(R::V2, R::V1);
        super::slot_owner::newer(a, R::V2);
        a.jcc_label(C::Above, live);
        super::mark::test_mark(a, R::V2, R::V6, R::V8);
        a.mov_reg_reg(R::V1, R::V2);
        a.jcc_label(C::NotEqual, live);
        bump(a, RT_REMSET_DROPPED, R::V2);
        a.jmp_label(next);
        a.bind_text_label(live);
    }
    // Retain exactly the observed field word across forwarding. V7 is the
    // stable Old slot (Old moves only in a full collection, which has no set).
    a.push_reg(R::V7);
    a.push_reg(R::V0);
    // V1 = Young offset -> reference bits.
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 16);
    super::colours::load_current(a, R::V1);
    a.or_reg_reg(R::V0, R::V1);
    a.call_label(forward);
    a.mov_reg_reg(R::V6, R::V0);
    a.pop_reg(R::V8); // observed word
    a.pop_reg(R::V7); // stable slot
    a.mov_reg_reg(R::V1, R::V0);
    a.shr_reg_imm8(R::V1, 16);
    super::generation::test(a, R::V1, super::generation::YOUNG);
    a.jcc_label(C::Equal, keep);
    heal(a, false);
    a.jmp_label(next);
    a.bind_text_label(keep);
    bump(a, RT_REMSET_KEPT, R::V0);
    remember(a, l, R::V7, R::V1, R::V2);
    // Certify the remembered bit only after current-set registration. A
    // failed heal leaves a newer publication and its own barrier intact.
    heal(a, true);
    a.bind_text_label(next);
    load_rt(a, R::V0, RT_RS_INDEX);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, RT_RS_INDEX, R::V0);
    a.jmp_label(lp);
    a.bind_text_label(done);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_REMSET_PREV_LEN, R::V0);
}

/// V6 = resolved reference, V8 = observed word, V7 = stable Old slot.
/// Kept references may certify the current remembered epoch after registration.
pub(super) fn heal<E: PortableAsm>(a: &mut E, kept: bool) {
    a.mov_reg_reg(R::V1, R::V6);
    if kept {
        a.and_reg_imm32(R::V1, !i32::from(crate::gc_abi::REMEMBERED_MASK));
        super::colours::load_current(a, R::V0);
        a.and_reg_imm32(R::V0, i32::from(crate::gc_abi::REMEMBERED_MASK));
        a.or_reg_reg(R::V1, R::V0);
    } else {
        super::fields::forget_nonnull(a, R::V1, R::V0);
    }
    a.mov_reg_reg(R::V0, R::V8);
    a.atomic_compare_exchange(R::V7, R::V1);
}
