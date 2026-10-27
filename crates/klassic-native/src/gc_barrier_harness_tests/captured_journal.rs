//! Deferred real journals survive actual Old start and failed end-check rounds.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

fn backlog(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let native = arg & 256 != 0;
    let n = arg & 255;
    let mut h = H::begin(g);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 91);
    h.alloc(2, 8, 0);
    h.store(Src::Raw(0), 0, Src::Raw(2), true);
    h.alloc(1, 16, 1);
    publish_primary(h.g);
    let regions: Vec<_> = (0..n).map(|_| journal_region_new(h.g, 65)).collect();
    cooperative_enable(h.g);
    let gate = captured_journal_gate(h.g);
    for region in regions {
        h.g.mov_imm64(R::V0, 32);
        h.keep(40);
        let again = h.g.create_text_label();
        h.g.bind_text_label(again);
        h.value(Src::Raw(0));
        journal_region_store(h.g, region, false);
        h.value(Src::Raw(1));
        journal_region_store(h.g, region, false);
        h.g.load_rbp_slot(R::V0, slot_offset(40));
        h.g.sub_reg_imm8(R::V0, 1);
        h.keep(40);
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(C::NotEqual, again);
    }
    h.report("owner_before", |h| root_journal_entries(h.g));
    observed_old_entry(h.g, true, 0);
    h.report("owner_after_start", |h| root_journal_entries(h.g));
    h.report("pending_after_start", |h| root_obligation_counter(h.g, 0));
    h.report("worker_before_release", |h| worker_journal_counter(h.g, 0));
    if native {
        root_journal_native_boundary(h.g);
        h.report("owner_after_native", |h| root_journal_entries(h.g));
        h.report("pending_after_native", |h| root_obligation_counter(h.g, 0));
    } else {
        worker_journal_wait(h.g, 4, 1); // real graph done, actual journal debt remains
        let poll = h.g.gc_gen.as_ref().unwrap().labels.poll_actor;
        h.g.call_label(poll);
        h.report("automatic_end_checks", |h| phase_round_metric(h.g, 7));
        observed_old_entry(h.g, false, 1);
        h.report("owner_after_retry", |h| root_journal_entries(h.g));
        h.report("pending_after_retry", |h| root_obligation_counter(h.g, 0));
        for (i, name) in [
            (2, "starts"),
            (3, "ends_before_release"),
            (7, "end_checks"),
            (8, "end_retries"),
        ] {
            h.report(name, |h| phase_round_metric(h.g, i));
        }
        captured_journal_release(h.g, gate);
        worker_journal_wait(h.g, 0, n * 64 + 1);
    }
    worker_journal_wait(h.g, 4, 1);
    observed_old_entry(h.g, false, 1);
    h.report("ends", |h| phase_round_metric(h.g, 3));
    h.report("owner_final", |h| root_journal_entries(h.g));
    h.report("worker_final", |h| worker_journal_counter(h.g, 0));
    h.report("pending_final", |h| root_obligation_counter(h.g, 0));
    h.report("published", |h| root_obligation_counter(h.g, 1));
    h.report("acknowledged", |h| root_obligation_counter(h.g, 2));
    h.report_counter("marked", Counter::Marked);
    h.report_counter("valid", Counter::MarkValid);
    h.report("bank", |h| scheduled_old_metric(h.g, 10));
    h.report("value", |h| h.load(Src::Raw(2), 0));
}
#[test]
fn production_old_start_and_reopen_retain_large_journals_without_owner_handoff_then_worker_closes()
{
    for n in [1, 32] {
        let o = run(backlog, n, true);
        assert_eq!(
            o.get("owner_after_start"),
            o.get("owner_before"),
            "{}",
            o.text()
        );
        assert_eq!(o.get("owner_after_retry"), o.get("owner_before"));
        assert_eq!(o.get("owner_final"), o.get("owner_before"));
        assert_eq!(o.get("pending_after_start"), n as i64 * 64 + 1);
        assert_eq!(o.get("pending_after_retry"), n as i64 * 64 + 1);
        assert_eq!(o.get("worker_before_release"), 0);
        assert_eq!(o.get("starts"), 1);
        assert_eq!(o.get("ends_before_release"), 0);
        assert_eq!(o.get("end_checks"), 1);
        assert_eq!(o.get("automatic_end_checks"), 0);
        assert_eq!(o.get("end_retries"), 1);
        assert_eq!(o.get("worker_final"), n as i64 * 64 + 1);
        assert_eq!(o.get("pending_final"), 0);
        assert_eq!(o.get("published"), o.get("acknowledged"));
        assert_eq!(o.get("ends"), 1);
        assert_eq!(o.get("marked"), 3);
        assert_eq!(o.get("valid"), 1);
        assert_eq!(o.get("bank"), 1);
        assert_eq!(o.get("value"), 91);
    }
}
#[test]
fn production_native_owner_consumes_captured_prestart_journals_with_the_same_real_lifetime() {
    for n in [1, 32] {
        let o = run(backlog, n | 256, true);
        assert_eq!(
            o.get("owner_after_start"),
            o.get("owner_before"),
            "{}",
            o.text()
        );
        assert_eq!(
            o.get("owner_after_native") - o.get("owner_before"),
            n as i64 * 64 + 1
        );
        assert_eq!(o.get("pending_after_native"), 0);
        assert_eq!(o.get("worker_final"), 0);
        assert_eq!(o.get("marked"), 3);
        assert_eq!(o.get("ends"), 1);
        assert_eq!(o.get("value"), 91);
    }
}
fn invalid(g: &mut crate::NativeCodeGenerator, kind: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    publish_primary(h.g);
    cooperative_enable(h.g);
    let gate = captured_journal_gate(h.g);
    observed_old_entry(h.g, true, 0);
    captured_journal_fault(h.g, kind);
    captured_journal_release(h.g, gate);
    worker_journal_wait(h.g, 0, 1);
}
#[test]
fn production_retained_journals_reject_incompatible_epochs_stages_sequences_and_bank_reuse() {
    for kind in 0..10 {
        let output = super::execute(invalid, kind, true, false);
        assert_eq!(output.status.code(), Some(1), "kind {kind}: {output:?}");
        assert!(String::from_utf8_lossy(&output.stderr).contains("runtime invariant violated"));
    }
}

fn invalid_round(g: &mut crate::NativeCodeGenerator, kind: u64) {
    let h = H::begin(g);
    cooperative_enable(h.g);
    captured_journal_fault(h.g, kind & 15);
    if kind & 16 != 0 {
        captured_journal_check_lifetime(h.g);
        return;
    }
    observed_old_entry(h.g, true, 0);
}
#[test]
fn production_old_round_rejects_enrollment_bank_or_mapping_changes_before_publication() {
    for kind in [8, 9, 24, 25] {
        let output = super::execute(invalid_round, kind, true, false);
        assert_eq!(output.status.code(), Some(1), "kind {kind}: {output:?}");
        assert!(String::from_utf8_lossy(&output.stderr).contains("runtime invariant violated"));
    }
}
fn reuse_tlab(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    cooperative_enable(h.g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 61);
    h.root(0);
    publish_primary(h.g);
    for (i, name) in [
        (0, "top_before"),
        (1, "page_before"),
        (4, "next_page_before"),
    ] {
        h.report(name, |h| captured_tlab_metric(h.g, i));
    }
    for _ in 0..20 {
        one_explicit_old_cycle(h.g);
    }
    for (i, name) in [
        (0, "top_after"),
        (1, "page_after"),
        (2, "suspends"),
        (3, "restores"),
        (4, "next_page_after"),
    ] {
        h.report(name, |h| captured_tlab_metric(h.g, i));
    }
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("cycles", |h| scheduled_old_metric(h.g, 7));
}
#[test]
fn production_twenty_real_old_cycles_restore_the_same_tlab_without_spending_unused_page_tails() {
    let o = run(reuse_tlab, 0, true);
    assert!(o.get("top_before") > 0, "{}", o.text());
    assert_eq!(o.get("top_after"), o.get("top_before"));
    assert_eq!(o.get("page_after"), o.get("page_before"));
    assert_eq!(o.get("next_page_after"), o.get("next_page_before"));
    assert_eq!(o.get("cycles"), 20);
    assert!(o.get("suspends") >= 40);
    assert_eq!(o.get("suspends"), o.get("restores"));
    assert_eq!(o.get("value"), 61);
}
fn invalid_tlab(g: &mut crate::NativeCodeGenerator, kind: u64) {
    let mut h = H::begin(g);
    cooperative_enable(h.g);
    h.alloc(1, 16, 0);
    publish_primary(h.g);
    captured_tlab_fault(h.g, kind);
}
#[test]
fn production_tlab_restore_rejects_changed_owner_incarnation_frontier_state_or_request() {
    for kind in 0..5 {
        let output = super::execute(invalid_tlab, kind, true, false);
        assert_eq!(output.status.code(), Some(1), "kind {kind}: {output:?}");
        assert!(String::from_utf8_lossy(&output.stderr).contains("runtime invariant violated"));
    }
}
