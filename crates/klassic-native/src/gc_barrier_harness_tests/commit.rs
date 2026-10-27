//! Real PROT_NONE reservation, minimal startup, and zeroed page reuse.
use super::{H, Src, run, run_failure};
use crate::gc_codegen::production::test_support::{
    CommitMetric as M, GENERATION_YOUNG, commit_metric, commit_page, fail_commit_after,
    heap_page_address, pool_claim, uncommit_page,
};
use crate::portable_asm::{PortableAsm, Reg as R};
use std::os::unix::process::ExitStatusExt;
const PAGE: u64 = 128 << 10;
const MIN: i64 = 4 << 20;
fn configure(g: &mut crate::NativeCodeGenerator) {
    g.gc_max_heap = 64 << 20;
    g.gc_min_heap = MIN as u64;
}
fn protected(g: &mut crate::NativeCodeGenerator, page: u64) {
    configure(g);
    let mut h = H::begin(g);
    heap_page_address(h.g, page);
    h.g.load_ptr_disp32(R::V0, R::V0, 0);
    h.keep(20);
    h.report("unexpected", |h| h.value(Src::Raw(20)));
}
#[test]
fn production_commit_keeps_the_guard_and_uncommitted_payload_inaccessible() {
    for page in [0, 33] {
        let o = run_failure(protected, page);
        assert_eq!(o.status.signal(), Some(11), "page {page}: {o:?}");
    }
}
fn grow(g: &mut crate::NativeCodeGenerator, _arg: u64) {
    configure(g);
    let mut h = H::begin(g);
    h.report("minimum", |h| commit_metric(h.g, M::Minimum));
    h.report("initial", |h| commit_metric(h.g, M::Current));
    commit_page(h.g, 33);
    h.report("once", |h| commit_metric(h.g, M::Current));
    commit_page(h.g, 33);
    h.report("twice", |h| commit_metric(h.g, M::Current));
    heap_page_address(h.g, 33);
    h.g.mov_imm64(R::V1, 42);
    h.g.store_ptr_disp32(R::V0, 4096, R::V1);
    h.g.load_ptr_disp32(R::V1, R::V0, 4096);
    h.g.mov_imm64(R::V2, 0);
    h.g.store_ptr_disp32(R::V0, 4096, R::V2);
    h.g.mov_reg_reg(R::V0, R::V1);
    h.keep(20);
    h.report("accessible", |h| h.value(Src::Raw(20)));
    h.report("peak", |h| commit_metric(h.g, M::Peak));
    h.report("commits", |h| commit_metric(h.g, M::Commits));
}
#[test]
fn production_commit_grows_once_before_first_access_and_counts_duplicate_ensures_once() {
    let o = run(grow, 0, false);
    for name in ["minimum", "initial"] {
        assert_eq!(o.get(name), MIN);
    }
    for name in ["once", "twice", "peak"] {
        assert_eq!(o.get(name), MIN + PAGE as i64);
    }
    assert_eq!(o.get("commits"), 33);
    assert_eq!(o.get("accessible"), 42);
}
fn reuse(g: &mut crate::NativeCodeGenerator, mode: u64) {
    configure(g);
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.value(Src::Root(0));
    h.keep(30);
    h.collect();
    h.finish_collection();
    h.report("before", |h| commit_metric(h.g, M::Current));
    uncommit_page(h.g, 1);
    h.keep(20);
    h.report("released", |h| h.value(Src::Raw(20)));
    h.report("after", |h| commit_metric(h.g, M::Current));
    uncommit_page(h.g, 2);
    h.keep(20);
    h.report("floor", |h| h.value(Src::Raw(20)));
    if mode == 1 {
        h.load(Src::Raw(30), 0);
    } // Deliberately probe the retired PROT_NONE page.
    pool_claim(h.g, 1, GENERATION_YOUNG);
    if mode == 2 {
        fail_commit_after(h.g, 1);
    }
    commit_page(h.g, 1);
    h.report("zero", |h| h.load(Src::Raw(30), 0));
    h.report("recommits", |h| commit_metric(h.g, M::Recommits));
    commit_page(h.g, 86); // An unused but owned Old page, beyond the initial minimum.
    uncommit_page(h.g, 86);
    h.keep(20);
    h.report("owned", |h| h.value(Src::Raw(20)));
    h.report("uncommits", |h| commit_metric(h.g, M::Uncommits));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_commit_uncommits_only_free_clean_pages_above_minimum_and_recommits_zeroed_storage() {
    let o = run(reuse, 0, false);
    assert_eq!(o.get("before"), MIN + PAGE as i64, "{}", o.text());
    assert_eq!(o.get("released"), 1);
    assert_eq!(o.get("after"), MIN);
    for name in ["floor", "zero", "owned"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
    assert_eq!(o.get("recommits"), 1);
    assert_eq!(o.get("uncommits"), 1);
    assert_eq!(o.get("value"), 84);
    let o = run_failure(reuse, 1);
    assert_eq!(o.status.signal(), Some(11));
}
#[test]
fn production_commit_recommit_failure_is_diagnosed_before_payload_access() {
    let o = run_failure(reuse, 2);
    assert_eq!(o.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&o.stderr);
    assert!(stderr.contains("heap commit failed"), "{stderr}");
}

fn invalid_os(g: &mut crate::NativeCodeGenerator, operation: u64) {
    configure(g);
    let mut h = H::begin(g);
    crate::gc_codegen::production::test_support::invalid_heap_os_operation(h.g, operation);
    h.keep(20);
    h.report("result", |h| h.value(Src::Raw(20)));
    h.report("committed", |h| commit_metric(h.g, M::Current));
}
#[test]
fn production_commit_os_leaves_report_actual_invalid_operation_failures() {
    for operation in 0..3 {
        let o = run(invalid_os, operation, false);
        assert_eq!(o.get("result"), 0, "{}", o.text());
        assert_eq!(o.get("committed"), MIN);
    }
}

fn delayed(g: &mut crate::NativeCodeGenerator, mode: u64) {
    use crate::gc_codegen::production::test_support::{free_since, poll_uncommit};
    const SECOND: u64 = 1_000_000_000;
    configure(g);
    if mode == 3 {
        g.gc_min_heap = g.gc_max_heap;
    }
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.collect();
    h.finish_collection();
    poll_uncommit(h.g, SECOND);
    h.report("observed", |h| free_since(h.g, 1));
    poll_uncommit(h.g, 0); // Failed clock cannot authorize reclaim.
    poll_uncommit(h.g, 301 * SECOND - 1);
    h.report("early", |h| commit_metric(h.g, M::Uncommits));
    if mode == 1 {
        poll_uncommit(h.g, SECOND / 2); // Backwards clock restarts the wait.
        poll_uncommit(h.g, 300 * SECOND);
        h.report("reset", |h| commit_metric(h.g, M::Uncommits));
        poll_uncommit(h.g, 301 * SECOND);
    } else if mode == 2 {
        pool_claim(h.g, 1, GENERATION_YOUNG);
        h.report("claim_age", |h| free_since(h.g, 1));
        commit_page(h.g, 1); // Includes the already accessible fast path.
        h.report("reuse_age", |h| free_since(h.g, 1));
        poll_uncommit(h.g, 301 * SECOND);
        h.report("owned_age", |h| free_since(h.g, 1));
    } else {
        poll_uncommit(h.g, 301 * SECOND);
    }
    h.report("released", |h| commit_metric(h.g, M::Uncommits));
    h.report("remaining", |h| commit_metric(h.g, M::Current));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_commit_idle_delay_requires_elapsed_monotonic_time_and_preserves_minimum() {
    for mode in [0, 1] {
        let o = run(delayed, mode, false);
        assert_eq!(o.get("observed"), 1_000_000_000, "{}", o.text());
        assert_eq!(o.get("early"), 0, "{}", o.text());
        if mode == 1 {
            assert_eq!(o.get("reset"), 0, "{}", o.text());
        }
        assert_eq!(o.get("released"), 1, "{}", o.text());
        assert_eq!(o.get("remaining"), MIN, "{}", o.text());
        assert_eq!(o.get("value"), 84, "{}", o.text());
    }
}
#[test]
fn production_commit_rebinding_accessible_pages_cancels_the_idle_age() {
    let o = run(delayed, 2, false);
    assert_eq!(o.get("claim_age"), 0, "{}", o.text());
    assert_eq!(o.get("reuse_age"), 0, "{}", o.text());
    assert_eq!(o.get("owned_age"), 0, "{}", o.text());
    assert_eq!(o.get("value"), 84, "{}", o.text());
}

fn idle_worker(g: &mut crate::NativeCodeGenerator, _arg: u64) {
    use crate::gc_codegen::production::test_support::idle_observation;
    configure(g);
    let mut h = H::begin(g);
    let done = h.g.create_text_label();
    let repeat = h.g.create_text_label();
    h.g.mov_imm64(R::V0, 50);
    h.keep(20);
    h.g.bind_text_label(repeat);
    idle_observation(h.g, 256); // Free, outside the first 64-page scan.
    h.g.test_reg_reg(R::V0, R::V0);
    h.g.jcc_label(super::C::NotEqual, done);
    // Leave the owner entirely idle for 100ms, without a wake notification.
    use crate::Reg;
    h.g.asm.sub_reg_imm32(Reg::Rsp, 16);
    h.g.asm.mov_imm64(Reg::Rax, 0);
    h.g.asm.store_ptr_disp32(Reg::Rsp, 0, Reg::Rax);
    h.g.asm.mov_imm64(Reg::Rax, 100_000_000);
    h.g.asm.store_ptr_disp32(Reg::Rsp, 8, Reg::Rax);
    h.g.asm.mov_reg_reg(Reg::Rdi, Reg::Rsp);
    h.g.asm.mov_imm64(Reg::Rsi, 0);
    h.g.asm.mov_imm64(Reg::Rax, 35); // nanosleep
    h.g.asm.syscall();
    h.g.asm.add_reg_imm32(Reg::Rsp, 16);
    h.value(Src::Raw(20));
    h.g.sub_reg_imm8(R::V0, 1);
    h.keep(20);
    h.g.jcc_label(super::C::NotEqual, repeat);
    h.g.mov_imm64(R::V0, 0);
    h.g.bind_text_label(done);
    h.keep(21);
    h.report("observed", |h| h.value(Src::Raw(21)));
}
#[test]
fn production_commit_idle_worker_wakes_without_owner_notification() {
    let o = run(idle_worker, 0, true);
    assert!(o.get("observed") > 0, "{}", o.text());
}

#[test]
fn production_commit_idle_uncommit_respects_minimum_equal_to_maximum() {
    let o = run(delayed, 3, false);
    assert_eq!(o.get("released"), 0, "{}", o.text());
    assert_eq!(o.get("remaining"), 64 << 20, "{}", o.text());
    assert_eq!(o.get("value"), 84, "{}", o.text());
}

fn report_snapshot(g: &mut crate::NativeCodeGenerator, _arg: u64) {
    use crate::gc_codegen::production::test_support::{reported_commit_metric, take_commit_report};
    configure(g);
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.collect();
    h.finish_collection();
    take_commit_report(h.g);
    uncommit_page(h.g, 1);
    h.report("current", |h| commit_metric(h.g, M::Current));
    h.report("reported", |h| reported_commit_metric(h.g, M::Current));
    h.report("reported_uncommits", |h| {
        reported_commit_metric(h.g, M::Uncommits)
    });
    take_commit_report(h.g);
    h.report("refreshed", |h| reported_commit_metric(h.g, M::Current));
    h.report("refreshed_uncommits", |h| {
        reported_commit_metric(h.g, M::Uncommits)
    });
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_commit_report_stays_coherent_while_idle_maintenance_changes_live_counters() {
    let o = run(report_snapshot, 0, false);
    assert_eq!(o.get("current"), MIN, "{}", o.text());
    assert_eq!(o.get("reported"), MIN + PAGE as i64, "{}", o.text());
    assert_eq!(o.get("reported_uncommits"), 0, "{}", o.text());
    assert_eq!(o.get("refreshed"), MIN, "{}", o.text());
    assert_eq!(o.get("refreshed_uncommits"), 1, "{}", o.text());
    assert_eq!(o.get("value"), 84, "{}", o.text());
}

fn owner_fallback(g: &mut crate::NativeCodeGenerator, _arg: u64) {
    configure(g);
    g.gc_uncommit_delay_ns = 0;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.collect();
    h.finish_collection();
    h.report("before", |h| commit_metric(h.g, M::Uncommits));
    for _ in 0..1024 {
        h.alloc(1, 16, 10);
    }
    h.report("released", |h| commit_metric(h.g, M::Uncommits));
    h.report("committed", |h| commit_metric(h.g, M::Current));
    h.report("commits", |h| commit_metric(h.g, M::Commits));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_commit_owner_fallback_uncommits_without_a_worker_through_real_allocations() {
    let o = run(owner_fallback, 0, false);
    assert_eq!(o.get("before"), 0, "{}", o.text());
    assert!(o.get("released") > 0, "{}", o.text());
    assert!(o.get("committed") >= MIN, "{}", o.text());
    assert_eq!(
        o.get("committed"),
        (o.get("commits") - o.get("released")) * PAGE as i64,
        "{}",
        o.text()
    );
    assert_eq!(o.get("value"), 84, "{}", o.text());
}

fn configured_delay(g: &mut crate::NativeCodeGenerator, zero: u64) {
    use crate::gc_codegen::production::test_support::poll_uncommit;
    configure(g);
    g.gc_uncommit_delay_ns = if zero == 0 { 7_000_000 } else { 0 };
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.collect();
    h.finish_collection();
    poll_uncommit(h.g, 1_000_000_000);
    h.report("first", |h| commit_metric(h.g, M::Uncommits));
    poll_uncommit(h.g, 1_006_999_999);
    h.report("early", |h| commit_metric(h.g, M::Uncommits));
    poll_uncommit(h.g, 1_007_000_000);
    h.report("boundary", |h| commit_metric(h.g, M::Uncommits));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_commit_configured_delay_uses_exact_nanoseconds_and_zero_has_no_second_scan_wait() {
    let o = run(configured_delay, 0, false);
    assert_eq!(o.get("first"), 0, "{}", o.text());
    assert_eq!(o.get("early"), 0, "{}", o.text());
    assert_eq!(o.get("boundary"), 1, "{}", o.text());
    assert_eq!(o.get("value"), 84);
    let o = run(configured_delay, 1, false);
    assert_eq!(o.get("first"), 1, "{}", o.text());
    assert_eq!(o.get("value"), 84);
}
