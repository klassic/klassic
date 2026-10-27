//! Heap-field load/store barriers and payload range checks for the direct
//! generational runtime. Heap fields retain the existing payload-address ABI.
//! RootCells, copying, remembered-set registration and mark/phase rules remain
//! the caller's existing contracts; this module adds no new owner protocol.
use super::{
    HEAP_BYTES, Labels, RT_HEAP_BASE, RT_MARK_ACTIVE, RT_MARK_DONE, YOUNG_END, copy::HEADER, d,
    load_runtime,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

const HEAP_PAGE: u64 = crate::gc_abi::HEAP_PAGE_BYTES;
/// Lowest heap offset of a Young payload (first page + one header).
const YOUNG_LOW: i32 = HEAP_PAGE as i32 + HEADER;

/// Jump to `outside` unless `value - heap_base` (left in `scratch`) is a
/// Young payload offset.
pub(super) fn young_offset<E: PortableAsm>(a: &mut E, value: R, scratch: R, outside: E::TextLabel) {
    a.mov_reg_reg(scratch, value);
    a.load_ptr_disp32(value, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(scratch, value);
    a.jcc_label(C::Below, outside);
    a.cmp_reg_imm32(scratch, YOUNG_LOW);
    a.jcc_label(C::Below, outside);
    a.cmp_reg_imm32(scratch, YOUNG_END as i32);
    a.jcc_label(C::AboveOrEqual, outside);
}

/// Like `young_offset`, for any heap payload offset (Young or Old).
pub(super) fn heap_offset<E: PortableAsm>(a: &mut E, value: R, scratch: R, outside: E::TextLabel) {
    a.mov_reg_reg(scratch, value);
    a.load_ptr_disp32(value, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(scratch, value);
    a.jcc_label(C::Below, outside);
    a.cmp_reg_imm32(scratch, YOUNG_LOW);
    a.jcc_label(C::Below, outside);
    a.mov_imm64(value, HEAP_BYTES);
    a.cmp_reg_reg(scratch, value);
    a.jcc_label(C::AboveOrEqual, outside);
}

/// field_load: the load barrier. V0 = loaded word, V10 = field address.
/// While a relocation phase is open, a word that addresses from-space is
/// relocated (copied once, or the winner's copy taken) and the field
/// healed by a CAS against the stale word; preserves the rest.
pub(super) fn emit_field_load<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
    let pass = a.create_text_label();
    let hit = a.create_text_label();
    a.bind_text_label(l.field_load);
    a.push_reg(R::V3);
    a.push_reg(R::V1);
    a.push_reg(R::V2);
    load_runtime(a, runtime);
    a.load_ptr_disp32(R::V1, R::V3, d(super::RT_PHASE_OPEN));
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, pass);
    a.mov_reg_reg(R::V1, R::V0);
    a.load_ptr_disp32(R::V2, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Below, pass);
    // V1 = heap offset; keep the loaded word on the stack while testing.
    a.push_reg(R::V0);
    for (lo, hi) in [
        (super::RT_RY_LO, super::RT_RY_HI),
        (super::RT_RO_LO, super::RT_RO_HI),
    ] {
        a.mov_reg_reg(R::V2, R::V1);
        a.load_ptr_disp32(R::V0, R::V3, d(lo));
        a.sub_reg_reg(R::V2, R::V0);
        a.load_ptr_disp32(R::V0, R::V3, d(hi));
        a.push_reg(R::V1);
        a.load_ptr_disp32(R::V1, R::V3, d(lo));
        a.sub_reg_reg(R::V0, R::V1);
        a.pop_reg(R::V1);
        a.cmp_reg_reg(R::V2, R::V0);
        a.jcc_label(C::Below, hit);
    }
    a.pop_reg(R::V0);
    a.jmp_label(pass);
    // The stale word stays on the stack for the healing CAS.
    a.bind_text_label(hit);
    for reg in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(reg);
    }
    a.mov_imm64(R::V2, 1);
    a.store_ptr_disp32(R::V3, d(super::RT_COLLECTING), R::V2);
    a.shl_reg_imm8(R::V1, 16);
    a.mov_imm64(R::V2, super::color());
    a.or_reg_reg(R::V1, R::V2);
    a.mov_reg_reg(R::V0, R::V1);
    a.call_label(l.forward);
    a.shr_reg_imm8(R::V0, 16);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V1);
    a.mov_imm64(R::V1, 0);
    a.store_ptr_disp32(R::V3, d(super::RT_COLLECTING), R::V1);
    a.load_ptr_disp32(R::V1, R::V3, d(super::RT_HEALED));
    a.add_reg_imm32(R::V1, 1);
    a.store_ptr_disp32(R::V3, d(super::RT_HEALED), R::V1);
    for reg in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(reg);
    }
    a.mov_reg_reg(R::V1, R::V0);
    a.pop_reg(R::V0);
    if l.worker.is_some() {
        // Only the worker's own heal of the same field can intervene; it
        // stores the same copy, so a failed CAS changes nothing.
        a.atomic_compare_exchange(R::V10, R::V1);
    } else {
        a.store_ptr_disp32(R::V10, 0, R::V1);
    }
    a.mov_reg_reg(R::V0, R::V1);
    a.bind_text_label(pass);
    a.pop_reg(R::V2);
    a.pop_reg(R::V1);
    a.pop_reg(R::V3);
    a.ret();
}

/// field_store: the SATB pre-barrier (while marking, the overwritten
/// reference is marked), the store, then the generational write barrier:
/// an Old field that now holds a Young payload address joins the remset.
pub(super) fn emit_field_store<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
    let done = a.create_text_label();
    a.bind_text_label(l.field_store);
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    a.push_reg(R::V2);
    a.push_reg(R::V3);
    load_runtime(a, runtime);
    if let Some(w) = &l.worker {
        let satb_done = a.create_text_label();
        a.load_ptr_disp32(R::V0, R::V3, d(RT_MARK_ACTIVE));
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, satb_done);
        a.load_ptr_disp32(R::V0, R::V7, 0);
        heap_offset(a, R::V0, R::V2, satb_done);
        for reg in [R::V6, R::V7, R::V8, R::V9] {
            a.push_reg(reg);
        }
        a.call_label(w.lock);
        a.call_label(w.mark_ref);
        a.mov_imm64(R::V0, 0);
        a.store_ptr_disp32(R::V3, d(RT_MARK_DONE), R::V0);
        super::worker::request_work(a);
        a.call_label(w.unlock);
        for reg in [R::V9, R::V8, R::V7, R::V6] {
            a.pop_reg(reg);
        }
        a.bind_text_label(satb_done);
    }
    a.store_ptr_disp32(R::V7, 0, R::V6);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.mov_reg_reg(R::V0, R::V7);
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, done);
    a.cmp_reg_imm32(R::V0, YOUNG_END as i32);
    a.jcc_label(C::Below, done);
    a.mov_imm64(R::V1, HEAP_BYTES);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, done);
    young_offset(a, R::V6, R::V0, done);
    super::remset::remember(a, l, R::V7, R::V1, R::V2);
    a.bind_text_label(done);
    a.load_ptr_disp32(R::V6, R::V7, 0);
    a.pop_reg(R::V3);
    a.pop_reg(R::V2);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
    a.ret();
}
