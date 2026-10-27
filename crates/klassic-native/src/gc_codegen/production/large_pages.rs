//! OS backing observations, independent of 128-KiB collector page ownership.
use super::*;
use crate::gc_codegen::production::{load_rt, store_rt};
#[repr(C)]
pub(super) struct State {
    mode: u64,
    reservation_base: u64,
    reservation_bytes: u64,
    advised_bytes: u64,
    advice_failures: u64,
    collapsed_bytes: u64,
    collapse_failures: u64,
    report: [u64; 7],
}
macro_rules! f { ($($n:ident:$v:ident),* $(,)?) => { $(
    pub(crate) const $n: usize = offset_of!(Runtime, large_pages.$v);
)* }; }
f! { MODE:mode, RESERVATION_BASE:reservation_base, RESERVATION_BYTES:reservation_bytes,
ADVISED_BYTES:advised_bytes, ADVICE_FAILURES:advice_failures, COLLAPSED_BYTES:collapsed_bytes,
COLLAPSE_FAILURES:collapse_failures }
pub(super) fn startup<E: PortableAsm>(a: &mut E) {
    store_rt_imm(a, MODE, a.gc_large_pages() as u64);
}
pub(super) fn reservation<E: PortableAsm>(a: &mut E) {
    if a.gc_large_pages() == crate::GcLargePages::Off {
        store_rt(a, RESERVATION_BASE, R::V0);
        store_rt_imm(a, RESERVATION_BYTES, geometry(a).heap_bytes);
    }
}
pub(super) fn snapshot<E: PortableAsm>(a: &mut E) {
    for (i, field) in [
        MODE,
        RESERVATION_BASE,
        RESERVATION_BYTES,
        ADVISED_BYTES,
        ADVICE_FAILURES,
        COLLAPSED_BYTES,
        COLLAPSE_FAILURES,
    ]
    .into_iter()
    .enumerate()
    {
        load_rt(a, R::V0, field);
        store_rt(a, offset_of!(Runtime, large_pages.report) + i * 8, R::V0);
    }
}
pub(crate) fn report_offsets() -> [i32; 7] {
    std::array::from_fn(|i| d(offset_of!(Runtime, large_pages.report) + i * 8))
}
#[cfg(test)]
pub(crate) fn live_offsets() -> [i32; 7] {
    [
        MODE,
        RESERVATION_BASE,
        RESERVATION_BYTES,
        ADVISED_BYTES,
        ADVICE_FAILURES,
        COLLAPSED_BYTES,
        COLLAPSE_FAILURES,
    ]
    .map(d)
}
