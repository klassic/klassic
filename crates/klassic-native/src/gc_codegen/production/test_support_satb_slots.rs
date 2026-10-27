//! Directed probes of real published/copy destination ownership metadata.
use super::super::*;

/// V7=field address; the real bounded SATB lookup returns payload offset V0.
pub(crate) fn satb_slot_owner_query(g: &mut crate::NativeCodeGenerator) {
    let entry = g.gc_gen.as_ref().unwrap().labels.satb_slot_owner;
    g.call_label(entry);
}

/// V0=payload address -> its managed offset, for exact owner comparisons.
pub(crate) fn satb_slot_owner_offset<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
}

/// Replace only V7's card hint with another real object's header offset V6.
/// Starts remain untouched: the lookup must reject a different Young page.
pub(crate) fn satb_slot_cross_page_hint<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V7, R::V1);
    a.sub_reg_reg(R::V6, R::V1);
    a.sub_reg_imm32(R::V6, size_of::<ObjectHeader>() as i32);
    a.shr_reg_imm8(R::V7, 10);
    a.shl_reg_imm8(R::V7, 3);
    a.add_reg_reg(R::V7, R::V3);
    a.add_reg_imm32(R::V7, layout(a).owner_cards as i32);
    a.atomic_store_release(R::V7, R::V6);
}
