//! Directed pool inputs and observations for actual emitted runtime tests.
use super::super::*;
use crate::gc_codegen::production::{lea_rt, load_rt, store_rt};
/// Quiescent admission probe. Independent reservation only, no object is
/// fabricated. The caller must not collect a successful descriptor-less span.
pub(crate) fn large_reserve_probe(a: &mut crate::NativeCodeGenerator, pages: u64) {
    let runtime = a.gc_gen.take().expect("generational runtime");
    worker::lock(a, &runtime.labels);
    a.mov_imm64(R::V7, pages);
    let failed = a.create_text_label();
    let done = a.create_text_label();
    large_reserve::claim_or(a, &runtime.labels, failed);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(failed);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(done);
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    worker::unlock(a, &runtime.labels);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.gc_gen = Some(runtime);
}
pub(crate) fn old_unused_pages(a: &mut crate::NativeCodeGenerator) {
    let runtime = a.gc_gen.take().expect("generational runtime");
    worker::lock(a, &runtime.labels);
    old_runs::unused_pages(a, &runtime.labels, R::V0);
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    worker::unlock(a, &runtime.labels);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.gc_gen = Some(runtime);
}
/// Reserve an independently selected empty page to shape the free pool.
pub(crate) fn pool_claim_at(a: &mut crate::NativeCodeGenerator, page: u64) {
    let runtime = a.gc_gen.take().expect("generational runtime");
    worker::lock(a, &runtime.labels);
    lea_rt(a, R::V0, (layout(a).generations + (page + 1) * 8) as usize);
    a.atomic_load_acquire(R::V0, R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, runtime.labels.fail);
    a.mov_imm64(R::V1, page);
    a.mov_imm64(R::V2, 1);
    a.mov_imm64(R::V6, generation::YOUNG);
    pool::publish(a, &runtime.labels);
    worker::unlock(a, &runtime.labels);
    a.gc_gen = Some(runtime);
}
/// Independent empty reservations split all free runs at a fixed stride.
/// Populated sources stay intact; no object or Large descriptor is fabricated.
pub(crate) fn fragment_free_runs(a: &mut crate::NativeCodeGenerator, stride: usize) {
    fragment_free_runs_impl(a, stride, true);
}
/// Keep the live Old reservation while shaping otherwise Free runs. Actual
/// worker copy placement may leave a larger contiguous run than a fixture
/// anticipated, so an escalation probe must establish its spatial premise.
pub(crate) fn fragment_free_runs_retaining_old(a: &mut crate::NativeCodeGenerator, stride: usize) {
    fragment_free_runs_impl(a, stride, false);
}
fn fragment_free_runs_impl(a: &mut crate::NativeCodeGenerator, stride: usize, shrink_old: bool) {
    let runtime = a.gc_gen.take().expect("generational runtime");
    worker::lock(a, &runtime.labels);
    if shrink_old {
        old_runs::shrink_unused(a, &runtime.labels);
    }
    for page in ((stride - 1)..geometry(a).pages as usize).step_by(stride) {
        let occupied = a.create_text_label();
        lea_rt(
            a,
            R::V0,
            (layout(a).generations + (page as u64 + 1) * 8) as usize,
        );
        a.atomic_load_acquire(R::V0, R::V0);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, occupied);
        a.mov_imm64(R::V1, page as u64);
        a.mov_imm64(R::V2, 1);
        a.mov_imm64(R::V6, generation::YOUNG);
        pool::publish(a, &runtime.labels);
        a.bind_text_label(occupied);
    }
    worker::unlock(a, &runtime.labels);
    a.gc_gen = Some(runtime);
}
/// Read the actual Free-role map with worker exclusion; no role is changed.
pub(crate) fn largest_free_run(a: &mut crate::NativeCodeGenerator) {
    let runtime = a.gc_gen.take().expect("generational runtime");
    worker::lock(a, &runtime.labels);
    a.mov_imm64(R::V0, 0);
    a.mov_imm64(R::V1, 0);
    for page in 0..geometry(a).pages {
        let occupied = a.create_text_label();
        let next = a.create_text_label();
        a.load_ptr_disp32(
            R::V2,
            R::V3,
            (layout(a).generations + (page + 1) * 8) as i32,
        );
        a.test_reg_reg(R::V2, R::V2);
        a.jcc_label(C::NotEqual, occupied);
        a.add_reg_imm32(R::V1, 1);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::AboveOrEqual, next);
        a.mov_reg_reg(R::V0, R::V1);
        a.jmp_label(next);
        a.bind_text_label(occupied);
        a.mov_imm64(R::V1, 0);
        a.bind_text_label(next);
    }
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    worker::unlock(a, &runtime.labels);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.gc_gen = Some(runtime);
}
#[derive(Clone, Copy)]
pub(crate) enum PoolMetric {
    OldCapacity,
    ReducedOldRuns,
    Claims,
    Released,
    Transfers,
    YoungBase,
    YoungEnd,
    OldBase,
}
pub(crate) fn pool_metric<E: PortableAsm>(a: &mut E, metric: PoolMetric) {
    let offset = match metric {
        PoolMetric::OldCapacity => pool::OLD_PAGES,
        PoolMetric::ReducedOldRuns => pool::REDUCED_OLD_RUNS,
        PoolMetric::Claims => pool::CLAIMS,
        PoolMetric::Released => pool::RELEASED,
        PoolMetric::Transfers => pool::TRANSFERS,
        PoolMetric::YoungBase => RT_SPACE_BASE,
        PoolMetric::YoungEnd => RT_SPACE_END,
        PoolMetric::OldBase => RT_OLD_BASE,
    };
    load_rt(a, R::V0, offset);
    if matches!(metric, PoolMetric::OldBase) {
        a.shr_reg_imm8(R::V0, 17);
        a.sub_reg_imm32(R::V0, 1);
    }
}
pub(crate) fn pool_claim(g: &mut crate::NativeCodeGenerator, count: u64, kind: u64) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    g.mov_imm64(R::V7, count);
    g.mov_imm64(R::V6, kind);
    pool::claim(g, &runtime.labels);
    g.push_reg(R::V0);
    g.push_reg(R::V0);
    worker::unlock(g, &runtime.labels);
    g.pop_reg(R::V0);
    g.pop_reg(R::V0);
    g.gc_gen = Some(runtime);
}
/// Independent empty Old reservations using Full's first/subsequent search.
/// No descriptor or object is fabricated; do not collect after this probe.
pub(crate) fn old_claim_probe(g: &mut crate::NativeCodeGenerator, first: bool, limit: u64) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    g.mov_imm64(R::V7, limit);
    if first {
        store_rt_imm(g, old_runs::WALK, 0);
        pool::largest_old_limit(g, &runtime.labels, true);
    } else {
        pool::next_old_limit(g, &runtime.labels);
    }
    // Preserve the public probe result across unlock and harness reporting.
    g.store_rbp_slot(8 * 46, R::V0);
    g.store_rbp_slot(8 * 47, R::V7);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
pub(crate) fn old_claim_cursor<E: PortableAsm>(g: &mut E) {
    load_rt(g, R::V0, old_runs::WALK);
}
/// Run Full's complete claimant with an explicit budget while startup Old is
/// empty. Its prior reservation remains occupied; no populated source is lost.
/// These probes stop before any subsequent collection.
pub(crate) fn old_claim_budget(g: &mut crate::NativeCodeGenerator, pages: u64) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    store_rt_imm(g, old_runs::REMAINING, pages);
    old_runs::claim(g, &runtime.labels);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
pub(crate) fn old_claim_run<E: PortableAsm>(g: &mut E, index: u64, pages: bool) {
    g.mov_imm64(R::V0, index);
    old_runs::ptr(g, old_runs::BANK);
    g.load_ptr_disp32(R::V0, R::V8, old_runs::BASE);
    if pages {
        g.load_ptr_disp32(R::V1, R::V8, old_runs::END);
        g.sub_reg_reg(R::V1, R::V0);
        g.mov_reg_reg(R::V0, R::V1);
        g.shr_reg_imm8(R::V0, 17);
    } else {
        g.shr_reg_imm8(R::V0, 17);
        g.sub_reg_imm8(R::V0, 1);
    }
}
pub(crate) fn old_claim_remaining<E: PortableAsm>(g: &mut E) {
    load_rt(g, R::V0, old_runs::REMAINING);
}
pub(crate) fn pool_role<E: PortableAsm>(a: &mut E, index: u64) {
    lea_rt(
        a,
        R::V0,
        (super::super::layout(a).generations + (index + 1) * 8) as usize,
    );
    a.atomic_load_acquire(R::V0, R::V0);
}
/// V0 = canonical heap address -> current directory role (fixture addresses).
pub(crate) fn pool_address_role<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    a.shr_reg_imm8(R::V0, 17);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, super::super::layout(a).generations as i32);
    a.atomic_load_acquire(R::V0, R::V0);
}
pub(crate) fn pool_page_state<E: PortableAsm>(a: &mut E, index: u64, value: u64) {
    a.mov_imm64(R::V0, index);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.mov_imm64(R::V0, value);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, state)), R::V0);
}

/// R0 = first/last remembered bitmap word of the actual allocated Old span.
/// The unallocated reservation tail can return to the pool before relocation.
pub(crate) fn old_bitmap_word<E: PortableAsm>(a: &mut E, source: bool, set: u64, last: bool) {
    assert!(set < 2);
    load_rt(a, R::V0, if source { RT_OLD_FROM } else { RT_OLD_BASE });
    a.shr_reg_imm8(R::V0, 9);
    if last {
        load_rt(a, R::V0, if source { RT_OLD_FROM_END } else { RT_OLD_TOP });
        a.add_reg_imm32(R::V0, 511);
        a.shr_reg_imm8(R::V0, 9);
        a.sub_reg_imm32(R::V0, 1);
    }
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(
        R::V0,
        (super::super::layout(a).rbits + set * super::super::geometry(a).rbits_words * 8) as i32,
    );
}
pub(crate) fn pool_payload_page<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    a.shr_reg_imm8(R::V0, 17);
    a.sub_reg_imm32(R::V0, 1);
}

/// Probe largest-run selection without replacing the active Old allocator.
/// Return base in V0 and actual count in V7, preserving both over unlock.
pub(crate) fn pool_largest_old(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    pool::largest_old(g, &runtime.labels);
    g.push_reg(R::V0);
    g.push_reg(R::V7);
    worker::unlock(g, &runtime.labels);
    g.pop_reg(R::V7);
    g.pop_reg(R::V0);
    g.gc_gen = Some(runtime);
}

/// Fault only: lower the active copy bound after relocation has opened.
pub(crate) fn pool_old_copy_limit<E: PortableAsm>(a: &mut E, pages: u64) {
    store_rt_imm(a, pool::OLD_PAGES, pages);
}

pub(crate) fn pool_page_incarnation<E: PortableAsm>(a: &mut E) {
    pool_payload_page(a);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
}
/// Predicate-only controller inputs. No allocation/collection follows them.
pub(crate) fn pool_preflight(
    a: &mut crate::NativeCodeGenerator,
    pages: u64,
    used: u64,
    protected: Option<u64>,
) {
    let runtime = a.gc_gen.take().expect("generational runtime");
    store_rt_imm(a, pool::OLD_PAGES, pages);
    load_rt(a, R::V0, RT_OLD_BASE);
    a.add_reg_imm32(R::V0, used as i32);
    store_rt(a, RT_OLD_TOP, R::V0);
    store_rt_imm(a, young_mark::VALID, u64::from(protected.is_some()));
    store_rt_imm(a, capacity::SEALED, protected.unwrap_or(0));
    // Numerical probe treats its entire supplied volume as promotion candidates.
    store_rt_imm(a, capacity::MATURE_SEALED, protected.unwrap_or(0));
    let full = a.create_text_label();
    let done = a.create_text_label();
    pool::must_full(a, &runtime.labels, full);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(full);
    a.mov_imm64(R::V0, 1);
    a.bind_text_label(done);
    a.gc_gen = Some(runtime);
}

pub(crate) fn pool_dirty_owner_or_forward<E: PortableAsm>(a: &mut E, index: u64, forward: bool) {
    a.mov_imm64(R::V0, index);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    if forward {
        a.load_ptr_disp32(R::V8, R::V8, d(offset_of!(AllocationPage, forward_table)));
        a.add_reg_imm32(R::V8, d(offset_of!(ForwardTable, used)));
    } else {
        a.add_reg_imm32(R::V8, d(offset_of!(AllocationPage, owner)));
    }
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V8, 0, R::V0);
}
pub(crate) fn pool_exhaust_incarnation<E: PortableAsm>(a: &mut E) {
    pool_payload_page(a);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.mov_imm64(R::V0, u64::MAX);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, incarnation)), R::V0);
}
