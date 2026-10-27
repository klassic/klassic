//! Probes around the actual physical accessibility state and OS callbacks.
use super::super::*;
use crate::gc_codegen::production::load_rt;
pub(crate) enum CommitMetric {
    Minimum,
    Current,
    Peak,
    Commits,
    Recommits,
    Uncommits,
    Pretouched,
}
pub(crate) fn commit_metric<E: PortableAsm>(a: &mut E, metric: CommitMetric) {
    let f = match metric {
        CommitMetric::Minimum => commit::MINIMUM,
        CommitMetric::Current => commit::COMMITTED,
        CommitMetric::Peak => commit::PEAK,
        CommitMetric::Commits => commit::COMMITS,
        CommitMetric::Recommits => commit::RECOMMITS,
        CommitMetric::Uncommits => commit::UNCOMMITS,
        CommitMetric::Pretouched => commit::PRETOUCHED,
    };
    load_rt(a, R::V0, f);
}
pub(crate) fn commit_page(g: &mut crate::NativeCodeGenerator, page: u64) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    g.mov_imm64(R::V0, page * HEAP_PAGE_BYTES);
    commit::ensure(g, &runtime.labels, R::V0);
    g.gc_gen = Some(runtime);
}
pub(crate) fn uncommit_page(g: &mut crate::NativeCodeGenerator, page: u64) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    g.mov_imm64(R::V0, page);
    g.call_label(runtime.labels.uncommit_heap_page);
    g.push_reg(R::V0);
    g.push_reg(R::V0);
    worker::unlock(g, &runtime.labels);
    g.pop_reg(R::V0);
    g.pop_reg(R::V0);
    g.gc_gen = Some(runtime);
}
pub(crate) fn fail_commit_after<E: PortableAsm>(a: &mut E, calls: u64) {
    store_rt_imm(a, commit::FAIL_AFTER, calls);
}
pub(crate) fn heap_page_address<E: PortableAsm>(a: &mut E, page: u64) {
    load_rt(a, R::V0, RT_HEAP_BASE);
    a.add_reg_imm32(R::V0, (page * HEAP_PAGE_BYTES) as i32);
}

pub(crate) fn invalid_heap_os_operation(g: &mut crate::NativeCodeGenerator, operation: u64) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    let entry = match operation {
        0 => {
            g.mov_imm64(R::V7, 0);
            runtime.labels.memory.reserve
        }
        1 => {
            g.mov_imm64(R::V7, 1);
            runtime.labels.memory.commit
        }
        2 => {
            g.mov_imm64(R::V7, 1);
            runtime.labels.memory.decommit
        }
        _ => unreachable!(),
    };
    g.mov_imm64(R::V6, HEAP_PAGE_BYTES);
    g.call_label(entry);
    g.gc_gen = Some(runtime);
}

/// Drive the production bounded scan through one full 64 MiB fixture pool.
pub(crate) fn poll_uncommit(g: &mut crate::NativeCodeGenerator, now: u64) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    for _ in 0..g.gc_max_heap.div_ceil(HEAP_PAGE_BYTES * 64) {
        g.mov_imm64(R::V0, now);
        g.call_label(runtime.labels.uncommit_poll);
    }
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
pub(crate) fn free_since<E: PortableAsm>(a: &mut E, page: u64) {
    a.load_ptr_disp32(R::V0, R::V3, (layout(a).free_since + page * 8) as i32);
}

pub(crate) fn idle_observation(g: &mut crate::NativeCodeGenerator, page: u64) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    free_since(g, page);
    g.push_reg(R::V0);
    g.push_reg(R::V0);
    worker::unlock(g, &runtime.labels);
    g.pop_reg(R::V0);
    g.pop_reg(R::V0);
    g.gc_gen = Some(runtime);
}

pub(crate) fn take_commit_report(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    g.call_label(runtime.labels.verify);
    g.gc_gen = Some(runtime);
}
pub(crate) fn reported_commit_metric<E: PortableAsm>(a: &mut E, metric: CommitMetric) {
    let s = stat_offsets();
    let offset = match metric {
        CommitMetric::Minimum => s.heap_minimum,
        CommitMetric::Current => s.heap_committed,
        CommitMetric::Peak => s.heap_peak,
        CommitMetric::Commits => s.heap_commits,
        CommitMetric::Recommits => s.heap_recommits,
        CommitMetric::Uncommits => s.heap_uncommits,
        CommitMetric::Pretouched => s.heap_pretouched,
    };
    load_diagnostic(a, R::V0, R::V3, offset);
}

/// Set one physical page's observed Free time to test group-wide delay checks.
pub(crate) fn set_free_since<E: PortableAsm>(a: &mut E, page: u64, now: u64) {
    a.mov_imm64(R::V0, now);
    a.store_ptr_disp32(R::V3, (layout(a).free_since + page * 8) as i32, R::V0);
}
