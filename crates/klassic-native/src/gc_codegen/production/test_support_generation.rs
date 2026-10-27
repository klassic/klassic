//! Directed layout probes; setters are not a production ownership-transfer API.
//! Tests have no OS worker and set a layout before tracing, or isolate a full
//! move. They do not establish pool claims, normal mixed-layout evacuation,
//! concurrent retagging, leases or safe reuse.
use super::super::{generation as g, load_rt, *};
use crate::gc_codegen::production::lea_rt;
pub(crate) const GENERATION_YOUNG: u64 = g::YOUNG;
pub(crate) const GENERATION_OLD: u64 = g::OLD;
pub(crate) const GENERATION_LARGE: u64 = g::LARGE;
pub(crate) fn generation_flags<E: PortableAsm>(a: &mut E, off: R, kind: u64) {
    g::test(a, off, kind);
}
pub(crate) fn generation_role_flags<E: PortableAsm>(a: &mut E, off: R, kind: u64) {
    g::test_role(a, off, kind);
}
pub(crate) fn generation_matches<E: PortableAsm>(a: &mut E, off: u64, kind: u64) {
    a.mov_imm64(R::V0, off);
    g::test(a, R::V0, kind);
    let yes = a.create_text_label();
    let done = a.create_text_label();
    a.jcc_label(C::Equal, yes);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(yes);
    a.mov_imm64(R::V0, 1);
    a.bind_text_label(done);
}
pub(crate) fn generation_entry<E: PortableAsm>(a: &mut E, off: u64, kind: u64) {
    assert!(off < super::super::geometry(a).heap_bytes);
    lea_rt(
        a,
        R::V1,
        (super::super::layout(a).generations + (off >> 17) * 8) as usize,
    );
    a.mov_imm64(R::V2, kind);
    a.atomic_store_release(R::V1, R::V2);
}
/// V0 = actual payload address; only compute its page-directory slot.
pub(crate) fn generation_override<E: PortableAsm>(a: &mut E, kind: u64) {
    a.mov_reg_reg(R::V1, R::V0);
    load_rt(a, R::V2, RT_HEAP_BASE);
    a.sub_reg_reg(R::V1, R::V2);
    a.shr_reg_imm8(R::V1, 17);
    a.shl_reg_imm8(R::V1, 3);
    a.add_reg_reg(R::V1, R::V3);
    a.add_reg_imm32(R::V1, super::super::layout(a).generations as i32);
    a.mov_imm64(R::V2, kind);
    a.atomic_store_release(R::V1, R::V2);
}
/// V0 = canonical payload -> actual adopted load mask in V0.
pub(crate) fn generation_load_mask<E: PortableAsm>(a: &mut E) {
    encode(a, R::V0, R::V1);
    colours::load_access(a);
    a.mov_reg_reg(R::V0, R::V2);
}
pub(crate) fn generation_heap_bytes() -> u64 {
    HEAP_BYTES
}
pub(crate) fn pool_pages() -> u64 {
    pool::PAGES
}
/// Poison a real destination, independent of pool placement.
pub(crate) fn generation_destination<E: PortableAsm>(a: &mut E, old_delta: Option<u64>, kind: u64) {
    if let Some(delta) = old_delta {
        load_rt(a, R::V0, RT_OLD_BASE);
        a.add_reg_imm32(R::V0, delta as i32);
    } else {
        load_rt(a, R::V0, RT_SPACE_END);
        a.shl_reg_imm8(R::V0, 17); // First byte of the highest Young page.
    }
    a.shr_reg_imm8(R::V0, 17);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, super::super::layout(a).generations as i32);
    a.mov_imm64(R::V1, kind);
    a.atomic_store_release(R::V0, R::V1);
}

/// Directed retention pass with no moving phase. This exercises the actual
/// previous/current sets and the target-generation decision, not placement.
pub(crate) fn generation_remembered_retention(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    remset::emit_flip(g);
    super::emit_unbounded_budget(g);
    let paused = g.create_text_label();
    remset::scan(g, &runtime.labels, runtime.labels.forward, paused);
    g.bind_text_label(paused);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
