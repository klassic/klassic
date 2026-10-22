//! G07: double-buffered remembered sets for `--gc=generational`.
//!
//! An Old field that holds a Young payload address is recorded as the
//! field's address. Two arrays of `REMSET_ENTRIES` words exist; `remset_base`
//! names the *current* one. Mutators (`field_store`) and the relocation
//! scan (promoted objects whose children stay Young) append to the current
//! set. A Young collection *flips* at its start: the current set becomes the
//! previous one and an empty set becomes current. The pause consumes the
//! previous set as roots -- each referent is forwarded, and an entry whose
//! slot still holds a Young address is re-registered in the new current set,
//! so everything stores did during the cycle lands in a set the pause is not
//! reading, and a live Young referent is kept for the next cycle. After the
//! pause the previous set is empty. A full collection promotes every
//! survivor and moves every Old object, so both sets are discarded instead
//! (their slot addresses are in the half being evacuated).
//!
//! A slot bitmap (`rbits`, one bit per 8-byte heap word) records which slots
//! are in either set, so a slot is appended at most once however often it is
//! stored to: a set is bounded by the number of distinct Old slots, and
//! consuming an entry clears its bit before a kept one is re-registered.
use super::{
    L, Labels, REMSET_ENTRIES, RT_FORWARD_CELL, RT_HEAP_BASE, RT_MARK_VALID, RT_REMSET_BASE,
    RT_REMSET_DROPPED, RT_REMSET_FLIPPED, RT_REMSET_FLIPS, RT_REMSET_KEPT, RT_REMSET_LEN,
    RT_REMSET_PREV_LEN, RT_RS_INDEX, YOUNG_END, copy::bump, d, enums::young_offset,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

/// Sum of the two arrays' offsets: `other = SUM - current`.
const SUM: u64 = L.remset + L.remset2;

/// Address of the slot-bit word of `slot` in `word` and its mask in `mask`
/// (V3 = runtime). Clobbers V0 and V1 (the shift count).
fn slot_bit<E: PortableAsm>(a: &mut E, slot: R, word: R, mask: R) {
    a.mov_reg_reg(word, slot);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(word, R::V0);
    a.mov_reg_reg(R::V1, word);
    a.shr_reg_imm8(R::V1, 3);
    a.and_reg_imm32(R::V1, 63);
    a.mov_imm64(mask, 1);
    a.shl_reg_count(mask);
    a.shr_reg_imm8(word, 9);
    a.shl_reg_imm8(word, 3);
    a.add_reg_reg(word, R::V3);
    a.add_reg_imm32(word, L.rbits as i32);
}

/// Forget that `slot` (preserved) is in a set: clear its slot bit. Only the
/// pause calls this, with the worker stopped. Clobbers V0, V1, V8, V9.
fn forget<E: PortableAsm>(a: &mut E, slot: R) {
    slot_bit(a, slot, R::V8, R::V9);
    a.mov_imm64(R::V0, u64::MAX);
    a.xor_reg_reg(R::V9, R::V0);
    a.load_ptr_disp32(R::V0, R::V8, 0);
    a.and_reg_reg(R::V0, R::V9);
    a.store_ptr_disp32(R::V8, 0, R::V0);
}

/// Append the field address in `field` to the current set (V3 = runtime),
/// unless the slot is already in a set: a slot bit (set atomically by
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
    slot_bit(a, field, R::V11, R::V10);
    a.atomic_fetch_or(R::V11, R::V10);
    a.test_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, skip);
    if l.worker.is_none() {
        a.load_ptr_disp32(index, R::V3, d(RT_REMSET_LEN));
        a.cmp_reg_imm32(index, REMSET_ENTRIES as i32);
        a.jcc_label(C::AboveOrEqual, l.old_full);
        a.mov_reg_reg(scratch, index);
        a.shl_reg_imm8(scratch, 3);
        a.add_reg_reg(scratch, R::V3);
        a.load_ptr_disp32(R::V0, R::V3, d(RT_REMSET_BASE));
        a.add_reg_reg(scratch, R::V0);
        a.store_ptr_disp32(scratch, 0, field);
        a.add_reg_imm32(index, 1);
        a.store_ptr_disp32(R::V3, d(RT_REMSET_LEN), index);
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
        a.load_ptr_disp32(R::V0, R::V3, d(RT_REMSET_BASE));
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
    a.load_ptr_disp32(R::V0, R::V3, d(RT_REMSET_LEN));
    a.store_ptr_disp32(R::V3, d(RT_REMSET_PREV_LEN), R::V0);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_REMSET_FLIPPED));
    a.add_reg_reg(R::V1, R::V0);
    a.store_ptr_disp32(R::V3, d(RT_REMSET_FLIPPED), R::V1);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_REMSET_LEN), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_REMSET_BASE));
    a.mov_imm64(R::V1, SUM);
    a.sub_reg_reg(R::V1, R::V0);
    a.store_ptr_disp32(R::V3, d(RT_REMSET_BASE), R::V1);
    bump(a, RT_REMSET_FLIPS, R::V0);
}

/// Previous-set entries are roots of a Young collection: forward each
/// referent and re-register the entry in the current set only while its slot
/// still points into Young. Empties the previous set.
pub(super) fn emit_remset_roots<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let next = a.create_text_label();
    let keep = a.create_text_label();
    let done = a.create_text_label();
    let lp = a.create_text_label();
    a.bind_text_label(l.remset_roots);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_RS_INDEX), R::V0);
    a.bind_text_label(lp);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_RS_INDEX));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_REMSET_PREV_LEN));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, done);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_REMSET_BASE));
    a.mov_imm64(R::V7, SUM);
    a.sub_reg_reg(R::V7, R::V1);
    a.add_reg_reg(R::V0, R::V7);
    a.load_ptr_disp32(R::V7, R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_FORWARD_CELL), R::V7);
    // The entry leaves the set; a kept one is appended afresh below.
    forget(a, R::V7);
    a.load_ptr_disp32(R::V0, R::V7, 0);
    young_offset(a, R::V0, R::V1, next);
    if l.worker.is_some() {
        // After a complete mark, an unmarked referent is garbage and so is
        // every Old holder of it: drop the entry instead of copying it.
        let live = a.create_text_label();
        a.load_ptr_disp32(R::V2, R::V3, d(RT_MARK_VALID));
        a.test_reg_reg(R::V2, R::V2);
        a.jcc_label(C::Equal, live);
        a.mov_reg_reg(R::V2, R::V1);
        super::mark::test_mark(a, R::V2, R::V6, R::V8);
        a.mov_reg_reg(R::V1, R::V2);
        a.jcc_label(C::NotEqual, live);
        bump(a, RT_REMSET_DROPPED, R::V2);
        a.jmp_label(next);
        a.bind_text_label(live);
    }
    // V1 = Young offset -> reference bits.
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 16);
    a.mov_imm64(R::V1, super::color());
    a.or_reg_reg(R::V0, R::V1);
    a.call_label(l.forward);
    a.mov_reg_reg(R::V1, R::V0);
    a.shr_reg_imm8(R::V0, 16);
    a.load_ptr_disp32(R::V2, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V2);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_FORWARD_CELL));
    a.store_ptr_disp32(R::V7, 0, R::V0);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_imm32(R::V1, YOUNG_END as i32);
    a.jcc_label(C::Below, keep);
    a.jmp_label(next);
    a.bind_text_label(keep);
    bump(a, RT_REMSET_KEPT, R::V0);
    remember(a, l, R::V7, R::V1, R::V2);
    a.bind_text_label(next);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_RS_INDEX));
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_RS_INDEX), R::V0);
    a.jmp_label(lp);
    a.bind_text_label(done);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_REMSET_PREV_LEN), R::V0);
    a.ret();
}
