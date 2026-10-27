//! Directed inputs/observations for the production capacity controller.
use super::super::{capacity as c, load_rt, *};
pub(crate) fn old_protected_bytes<E: PortableAsm>(a: &mut E) {
    old_footprint::protected(a);
}
/// Observe the same quiescent capacity predicate used by Full and admission.
pub(crate) fn full_destination_pages(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    old_runs::destination_pages(g, &runtime.labels);
    g.push_reg(R::V0);
    g.push_reg(R::V0);
    worker::unlock(g, &runtime.labels);
    g.pop_reg(R::V0);
    g.pop_reg(R::V0);
    g.gc_gen = Some(runtime);
}
pub(crate) fn old_protection_counter<E: PortableAsm>(a: &mut E, black: bool) {
    load_rt(
        a,
        R::V0,
        if black {
            old_footprint::BLACK
        } else {
            old_footprint::TRACE
        },
    );
}
/// Numerical overflow probe only; does not fabricate live objects or marks.
pub(crate) fn seed_old_protection<E: PortableAsm>(a: &mut E, trace: u64, black: u64) {
    store_rt_imm(a, old_footprint::TRACE, trace);
    store_rt_imm(a, old_footprint::BLACK, black);
}
pub(crate) fn request_young_pages<E: PortableAsm>(a: &mut E, pages: u64) {
    store_rt_imm(a, c::REQUESTED, pages);
}
pub(crate) fn capacity_policy_input<E: PortableAsm>(a: &mut E, pages: u64, live: u64, full: bool) {
    store_rt_imm(a, c::ACTIVE, pages);
    store_rt_imm(a, tenuring::LAST_YOUNG, live);
    store_rt_imm(a, RT_FULL, u64::from(full));
    copy::bump(a, start_policy::MARK_SAMPLES, R::V0);
    c::observe(a);
}
pub(crate) fn capacity_policy_repeat<E: PortableAsm>(a: &mut E) {
    c::observe(a);
}
pub(crate) fn young_window_pages<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, RT_SPACE_END);
    load_rt(a, R::V1, RT_SPACE_BASE);
    a.sub_reg_reg(R::V0, R::V1);
}
pub(crate) fn young_trace_complete<E: PortableAsm>(
    a: &mut E,
    l: &super::HarnessLabels<E::TextLabel>,
) {
    a.call_label(l.young_start);
    a.call_label(l.lock);
    super::emit_unbounded_budget(a);
    a.call_label(l.young_loop);
    a.call_label(l.unlock);
}

/// Directed call of the same admission emitter used by the allocation entry,
/// after controlled copier progress, without that entry's preceding quantum.
pub(crate) fn capacity_admit(g: &mut crate::NativeCodeGenerator, size: u64) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    store_rt_imm(g, RT_FRESH_SIZE, size);
    admission::wait_for_refill(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

/// Close a completely drained real Young snapshot before a sizing observation.
pub(crate) fn seal_young_trace(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    young_mark::wait(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

/// Logical Young candidate footprint before or after snapshot sealing.
pub(crate) fn young_mature_bytes<E: PortableAsm>(a: &mut E, sealed: bool) {
    load_rt(a, R::V0, if sealed { c::MATURE_SEALED } else { c::MATURE });
}

/// Numerical bound probe only; no allocation/collection may follow these inputs.
pub(crate) fn seed_young_source_pages<E: PortableAsm>(a: &mut E, bounds: [u64; 5]) {
    store_rt_imm(
        a,
        tenuring::LAST_YOUNG,
        (bounds[1].saturating_sub(bounds[0]) + bounds[3].saturating_sub(bounds[2]))
            * HEAP_PAGE_BYTES,
    );
    for (field, value) in [
        RT_SPACE_BASE,
        RT_NEXT_PAGE,
        RT_COPY_LOW,
        RT_SPACE_END,
        c::ACTIVE,
    ]
    .into_iter()
    .zip(bounds)
    {
        store_rt_imm(a, field, value);
    }
}
pub(crate) fn young_allocated_pages(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    c::allocated_source_pages(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

/// Observe each real source stream separately after relocation is complete.
pub(crate) fn young_source_stream_pages<E: PortableAsm>(a: &mut E, worker: bool) {
    let (high, low) = if worker {
        (RT_SPACE_END, RT_COPY_LOW)
    } else {
        (RT_NEXT_PAGE, RT_SPACE_BASE)
    };
    load_rt(a, R::V0, high);
    load_rt(a, R::V1, low);
    a.sub_reg_reg(R::V0, R::V1);
}

/// Predicate-only previous-copy census fault/boundary input.
pub(crate) fn seed_previous_young_copies<E: PortableAsm>(a: &mut E, bytes: u64) {
    store_rt_imm(a, tenuring::LAST_YOUNG, bytes);
}

/// Actual request ownership, independently of page-count predictions.
pub(crate) fn current_tlab_bound<E: PortableAsm>(a: &mut E) {
    load_rt(
        a,
        R::V0,
        RT_REQUEST + offset_of!(crate::gc_abi::AllocationRequest, page),
    );
}

/// Pure predicate inputs; no allocation or collection follows these probes.
pub(crate) fn legacy_overflow_probe(
    g: &mut crate::NativeCodeGenerator,
    protected: u64,
    mature: u64,
    runs: (u64, u64),
    available: Option<(u64, u64)>,
) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    store_rt_imm(g, young_mark::VALID, 1);
    store_rt_imm(g, c::SEALED, protected);
    store_rt_imm(g, c::MATURE_SEALED, mature);
    store_rt_imm(g, old_runs::COUNT, runs.0);
    store_rt_imm(g, old_runs::ALLOC, runs.1);
    if let Some((pages, used)) = available {
        store_rt_imm(g, pool::OLD_PAGES, pages);
        load_rt(g, R::V0, RT_OLD_BASE);
        g.add_reg_imm32(R::V0, used as i32);
        store_rt(g, RT_OLD_TOP, R::V0);
        let full = g.create_text_label();
        let done = g.create_text_label();
        pool::must_full(g, &runtime.labels, full);
        g.mov_imm64(R::V0, 0);
        g.jmp_label(done);
        g.bind_text_label(full);
        g.mov_imm64(R::V0, 1);
        g.bind_text_label(done);
    } else {
        c::normal_promotion_pages(g, &runtime.labels);
    }
    g.gc_gen = Some(runtime);
}
