//! Real ordinary allocation/safepoint scheduling, without explicit cycle calls.
use super::{owner_boundaries::scheduled_old_graph, run};
#[test]
fn production_normal_allocations_schedule_multiple_old_cycles_on_one_two_and_eight_owners() {
    for n in [1, 2, 8] {
        let o = run(scheduled_old_graph, n, true);
        assert_eq!(o.get("enabled"), 1, "{}", o.text());
        assert!(o.get("worker_journal_entries") > 0, "{}", o.text());
        assert!(o.get("cycles") >= 2, "{}", o.text());
        assert_eq!(o.get("starts"), o.get("cycles"));
        assert_eq!(o.get("ends"), o.get("cycles"));
        assert_eq!(o.get("completed"), o.get("starts") + o.get("end_checks"));
        assert_eq!(o.get("end_checks"), o.get("ends") + o.get("end_retries"));
        assert!(o.get("auto_starts") >= o.get("starts"));
        assert!(o.get("auto_ends") >= o.get("end_checks"));
        assert!(o.get("scheduler_polls") > 0);
        assert_eq!(o.get("bank"), 1);
        assert_eq!(o.get("collections"), 0);
        assert_eq!(o.get("coordinator_slices"), 0);
        assert_eq!(o.get("mark_active"), 0);
        assert_eq!(o.get("valid"), 1);
        assert_eq!(o.get("resuming"), 0);
        assert_eq!(o.get("closed_pub"), o.get("closed_ack"));
        assert!(o.get("closed_worker") > 0);
        assert_eq!(
            o.get("final_pending"),
            o.get("final_pub") - o.get("final_ack")
        );
        let mut tids = std::collections::HashSet::new();
        let mut coordinated = 0;
        for i in 0..n {
            assert!(o.get(&format!("tid{i}")) > 0);
            assert!(tids.insert(o.get(&format!("tid{i}"))));
            assert_eq!(o.get(&format!("acks{i}")), o.get("completed"));
            coordinated += o.get(&format!("coordinations{i}"));
            assert_eq!(
                o.get(&format!("adoptions{i}")) + o.get(&format!("coordinations{i}")),
                o.get("completed")
            );
            assert_eq!(o.get(&format!("phase{i}")), o.get("phase"));
            assert_eq!(o.get(&format!("bytes{i}")), (32768 / n * 96) as i64);
            assert_eq!(
                o.get(&format!("leaf{i}")),
                (((i + 2) << 32) | (32768 / n - 1)) as i64
            );
            assert_eq!(o.get(&format!("back{i}")), o.get("hub"));
        }
        assert_eq!(coordinated, o.get("completed"));
    }
}

#[test]
fn production_delayed_normal_start_request_is_discarded_after_another_owner_finishes_its_cycle() {
    for n in [2, 8] {
        let o = run(
            super::owner_boundaries::delayed_scheduled_old_graph,
            n,
            true,
        );
        assert!(o.get("delayed_observed_cycle") >= 1, "{}", o.text());
        assert_eq!(o.get("delayed_requested_cycle"), 0);
        assert!(o.get("stale_starts") >= 1);
        assert!(o.get("cycles") >= 2);
        assert_eq!(o.get("starts"), o.get("ends"));
        assert_eq!(o.get("completed"), o.get("starts") + o.get("end_checks"));
        assert_eq!(o.get("mark_active"), 0);
        assert_eq!(o.get("valid"), 1);
        assert_eq!(o.get("bank"), 1);
        assert_eq!(o.get("collections"), 0);
        for i in 0..n {
            assert_eq!(o.get(&format!("acks{i}")), o.get("completed"));
        }
    }
}
fn stale(g: &mut crate::NativeCodeGenerator, _: u64) {
    use crate::gc_codegen::production::test_support::*;
    let mut h = super::H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    publish_primary(h.g);
    cooperative_enable(h.g);
    h.alloc(1, 16, 1);
    publish_primary(h.g);
    one_explicit_old_cycle(h.g);
    observed_old_entry(h.g, true, 0);
    observed_old_entry(h.g, false, 0);
    for (kind, name) in [
        (5, "stale_starts"),
        (6, "stale_ends"),
        (7, "cycles"),
        (1, "base"),
        (8, "allocated"),
    ] {
        h.report(name, |h| scheduled_old_metric(h.g, kind));
    }
}
#[test]
fn production_observed_old_start_and_end_never_replay_a_completed_cycle() {
    let o = run(stale, 0, true);
    assert_eq!(o.get("cycles"), 1);
    assert_eq!(o.get("stale_starts"), 1);
    assert_eq!(o.get("stale_ends"), 1);
    assert_eq!(o.get("base"), o.get("allocated"));
}

fn invalid(g: &mut crate::NativeCodeGenerator, mode: u64) {
    use crate::gc_codegen::production::test_support::*;
    let mut h = super::H::begin(g);
    match mode {
        0 => observed_old_entry(h.g, true, 1),
        1 => observed_old_entry(h.g, false, 1),
        2 => {
            let _ = create_actor(h.g, 16, 8);
            cooperative_enable(h.g);
        }
        3 => {
            h.mark_start();
            cooperative_enable(h.g);
        }
        4 => cooperative_private_fault(h.g),
        5 => {
            cooperative_enable(h.g);
            cooperative_enable(h.g);
        }
        6 => {
            cooperative_enable(h.g);
            observe_legacy_collect_failure(h.g);
            legacy_collect_entry(h.g);
        }
        7 => {
            h.alloc(1, 16, 0);
            h.root(0);
            publish_primary(h.g);
            let a = create_actor(h.g, 16, 8);
            native_actor(h.g, a, true);
            root_legacy_scope(h.g, true, false);
            native_actor(h.g, a, false);
            enable_cooperative_other(h.g, a);
        }
        _ => unreachable!(),
    }
}
#[test]
fn production_cooperative_join_future_requests_and_unadmitted_legacy_move_reject() {
    for mode in 0..8 {
        let o = super::execute(invalid, mode, true, false);
        assert!(!o.status.success(), "mode {mode} accepted");
        if mode == 6 {
            assert!(
                String::from_utf8_lossy(&o.stdout).contains("collect_active=0\n"),
                "legacy collect started before rejection: {o:?}"
            );
        }
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant"),
            "mode {mode}: {o:?}"
        );
    }
}
