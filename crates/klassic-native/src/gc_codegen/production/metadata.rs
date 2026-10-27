//! Separate budget for writable collector mappings, including pending maps.
//! Copy backing retires at the sole-owner source handoff. General metadata
//! leases and multi-owner record retirement remain separate obligations.
use super::{
    Labels, Runtime, d, geometry, layout, lea_rt, load_rt, round_up, store_rt, store_rt_imm,
};
use crate::gc_abi::{HEAP_PAGE_BYTES, MarkRecord};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

mod native_release;
mod reclaim;

#[repr(C)]
pub(super) struct State {
    limit: u64,
    charged: u64,
    committed: u64,
    peak: u64,
    maps: u64,
    failures: u64,
    initial: u64,
    fail_after: u64,
    unmaps: u64,
    unmap_failures: u64,
    release_state: u64,
    release_entry: u64,
    release_address: u64,
    release_fail_after: u64,
    report: [u64; 11],
}
const LIMIT: usize = offset_of!(Runtime, metadata) + offset_of!(State, limit);
const CHARGED: usize = offset_of!(Runtime, metadata) + offset_of!(State, charged);
const COMMITTED: usize = offset_of!(Runtime, metadata) + offset_of!(State, committed);
const PEAK: usize = offset_of!(Runtime, metadata) + offset_of!(State, peak);
const MAPS: usize = offset_of!(Runtime, metadata) + offset_of!(State, maps);
const FAILURES: usize = offset_of!(Runtime, metadata) + offset_of!(State, failures);
const INITIAL: usize = offset_of!(Runtime, metadata) + offset_of!(State, initial);
const REPORT: usize = offset_of!(Runtime, metadata) + offset_of!(State, report);
#[cfg(test)]
const FAIL_AFTER: usize = offset_of!(Runtime, metadata) + offset_of!(State, fail_after);

pub(crate) fn initial_bytes<E: PortableAsm>(a: &E) -> u64 {
    round_up(layout(a).total, a.gc_metadata_alignment())
}
pub(crate) fn copy_bytes<E: PortableAsm>(a: &E) -> u64 {
    round_up(
        HEAP_PAGE_BYTES.div_ceil(48) * size_of::<MarkRecord>() as u64,
        a.gc_metadata_alignment(),
    )
}
pub(crate) fn limit<E: PortableAsm>(a: &E) -> u64 {
    a.gc_metadata_limit().unwrap_or_else(|| {
        initial_bytes(a)
            + geometry(a).pages * copy_bytes(a)
            + round_up(a.gc_external_metadata_bytes(), a.gc_metadata_alignment())
    })
}
pub(super) fn startup<E: PortableAsm>(a: &mut E) {
    store_rt_imm(a, LIMIT, limit(a));
    for field in [CHARGED, COMMITTED, PEAK, INITIAL] {
        store_rt_imm(a, field, initial_bytes(a));
    }
    store_rt_imm(a, MAPS, 1);
}

/// V8 = signed delta, V1 = resulting value. Clobbers V0/V2/V9.
fn adjust<E: PortableAsm>(a: &mut E, field: usize) {
    lea_rt(a, R::V9, field);
    let again = a.create_text_label();
    a.bind_text_label(again);
    a.atomic_load_acquire(R::V0, R::V9);
    a.mov_reg_reg(R::V2, R::V0);
    a.mov_reg_reg(R::V1, R::V0);
    a.add_reg_reg(R::V1, R::V8);
    a.atomic_compare_exchange(R::V9, R::V1);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::NotEqual, again);
}

/// V7 = byte request -> V0 = mapping or NULL on OS failure; preserves V3.
/// Budget exhaustion terminates before calling the OS. A reserved charge is
/// included in CHARGED until the OS either commits it or returns failure.
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    reclaim::emit(a, l);
    native_release::emit(a, l);
    a.bind_text_label(l.metadata_map);
    let exhausted = a.create_text_label();
    let retry = a.create_text_label();
    let failed = a.create_text_label();
    let peak = a.create_text_label();
    let counted = a.create_text_label();
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, l.fail);
    a.mov_reg_reg(R::V8, R::V7);
    a.add_reg_imm32(R::V8, (a.gc_metadata_alignment() - 1) as i32);
    a.jcc_label(C::Below, exhausted);
    a.and_reg_imm32(R::V8, -(a.gc_metadata_alignment() as i32));
    lea_rt(a, R::V9, CHARGED);
    a.bind_text_label(retry);
    a.atomic_load_acquire(R::V0, R::V9);
    a.mov_reg_reg(R::V6, R::V0);
    a.mov_reg_reg(R::V1, R::V0);
    a.add_reg_reg(R::V1, R::V8);
    a.jcc_label(C::Below, exhausted);
    load_rt(a, R::V2, LIMIT);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Above, exhausted);
    a.atomic_compare_exchange(R::V9, R::V1);
    a.cmp_reg_reg(R::V0, R::V6);
    a.jcc_label(C::NotEqual, retry);
    a.mov_reg_reg(R::V7, R::V8);
    #[cfg(test)]
    {
        let normal = a.create_text_label();
        load_rt(a, R::V0, FAIL_AFTER);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, normal);
        a.sub_reg_imm32(R::V0, 1);
        store_rt(a, FAIL_AFTER, R::V0);
        a.jcc_label(C::NotEqual, normal);
        // Aligned but impossible OS size: exercise the actual mapping failure.
        a.mov_imm64(R::V7, !4095_u64);
        a.bind_text_label(normal);
    }
    a.push_reg(R::V8);
    a.push_reg(R::V8);
    a.call_label(l.map);
    a.pop_reg(R::V8);
    a.pop_reg(R::V8);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, failed);
    a.mov_reg_reg(R::V7, R::V0);
    adjust(a, COMMITTED);
    a.mov_reg_reg(R::V6, R::V1);
    lea_rt(a, R::V9, PEAK);
    a.bind_text_label(peak);
    a.atomic_load_acquire(R::V0, R::V9);
    a.cmp_reg_reg(R::V0, R::V6);
    a.jcc_label(C::AboveOrEqual, counted);
    a.mov_reg_reg(R::V2, R::V0);
    a.atomic_compare_exchange(R::V9, R::V6);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::NotEqual, peak);
    a.bind_text_label(counted);
    a.mov_imm64(R::V8, 1);
    adjust(a, MAPS);
    a.mov_reg_reg(R::V0, R::V7);
    a.ret();
    a.bind_text_label(failed);
    a.mov_imm64(R::V1, 0);
    a.sub_reg_reg(R::V1, R::V8);
    a.mov_reg_reg(R::V8, R::V1);
    adjust(a, CHARGED);
    a.mov_imm64(R::V8, 1);
    adjust(a, FAILURES);
    a.mov_imm64(R::V0, 0);
    a.ret();
    a.bind_text_label(exhausted);
    a.mov_imm64(R::V8, 1);
    adjust(a, FAILURES);
    a.jmp_label(l.metadata_budget_oom);
}

pub(crate) fn seal_initial<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, COMMITTED);
    store_rt(a, INITIAL, R::V0);
}
pub(super) fn snapshot<E: PortableAsm>(a: &mut E) {
    for (i, field) in [
        LIMIT,
        INITIAL,
        CHARGED,
        COMMITTED,
        CHARGED,
        PEAK,
        MAPS,
        FAILURES,
        reclaim::UNMAPS,
        reclaim::FAILURES,
        reclaim::STATE,
    ]
    .into_iter()
    .enumerate()
    {
        load_rt(a, R::V0, field);
        if i == 4 {
            load_rt(a, R::V1, COMMITTED);
            a.sub_reg_reg(R::V0, R::V1);
        }
        if i == 10 {
            let empty = a.create_text_label();
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::Equal, empty);
            a.mov_imm64(R::V0, 1);
            a.bind_text_label(empty);
        }
        store_rt(a, REPORT + i * 8, R::V0);
    }
}
pub(crate) fn report_offsets() -> [i32; 11] {
    std::array::from_fn(|i| d(REPORT + i * 8))
}
#[cfg(test)]
pub(crate) fn live_offsets() -> [i32; 6] {
    [LIMIT, INITIAL, CHARGED, COMMITTED, MAPS, FAILURES].map(d)
}
#[cfg(test)]
pub(crate) fn fail_next<E: PortableAsm>(a: &mut E) {
    store_rt_imm(a, FAIL_AFTER, 1);
}

#[cfg(test)]
pub(crate) fn copy_backings<E: PortableAsm>(a: &mut E) {
    lea_rt(a, R::V7, layout(a).copy_marks as usize + 8);
    a.mov_imm64(R::V6, geometry(a).pages);
    a.mov_imm64(R::V1, 0);
    let next = a.create_text_label();
    let empty = a.create_text_label();
    a.bind_text_label(next);
    a.load_ptr_disp32(R::V0, R::V7, 0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Below, empty);
    a.add_reg_imm32(R::V1, 1);
    a.bind_text_label(empty);
    a.add_reg_imm32(R::V7, 8);
    a.sub_reg_imm32(R::V6, 1);
    a.jcc_label(C::NotEqual, next);
    a.mov_reg_reg(R::V0, R::V1);
}

pub(super) fn retire_page<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    paused: E::TextLabel,
) {
    reclaim::retire_page(a, l, paused);
}
pub(super) fn after_unlock<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    reclaim::after_unlock(a, l);
}
#[cfg(test)]
pub(crate) use reclaim::fail_release_next;
#[cfg(all(test, target_os = "linux", target_arch = "x86_64"))]
pub(crate) use reclaim::{release_offsets, retry_release};

#[cfg(test)]
pub(crate) fn backing_for<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V1, super::RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    a.shr_reg_imm8(R::V0, 17);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.load_ptr_disp32(R::V0, R::V0, layout(a).copy_marks as i32);
}
#[cfg(test)]
pub(crate) fn unlock_ticket_only(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    g.call_label(runtime.labels.owner_unlock_raw);
    g.gc_gen = Some(runtime);
}
