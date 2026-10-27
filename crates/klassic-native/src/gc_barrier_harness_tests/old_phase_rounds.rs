//! Real Running-owner Old start/end rounds, self handoff and self adoption.
use super::{owner_boundaries::running_old_graph, run};
#[test]
fn production_running_owners_start_and_close_old_mark_on_one_two_and_eight_real_threads() {
    for n in [1, 2, 8] {
        let o = run(running_old_graph, n, true);
        assert_eq!(o.get("starts"), 1, "{}", o.text());
        assert_eq!(o.get("ends"), 1);
        assert_eq!(o.get("completed"), 1 + o.get("end_checks"));
        assert_eq!(o.get("end_checks"), 1 + o.get("end_retries"));
        assert_eq!(o.get("expected"), n as i64);
        assert_eq!(o.get("acks"), n as i64);
        for k in [
            "round_kind",
            "resuming",
            "active",
            "native",
            "primary_acks",
            "mark_active",
            "collections",
        ] {
            assert_eq!(o.get(k), 0, "{k}: {}", o.text());
        }
        assert_eq!(o.get("valid"), 1);
        assert_eq!(o.get("closed_pub"), o.get("closed_ack"));
        assert!(o.get("closed_pub") > 0);
        assert!(o.get("closed_worker") > 0);
        assert_eq!(o.get("coordinator_slices"), 0);
        assert_eq!(o.get("bank"), 1);
        assert!(o.get("final_pub") > o.get("closed_pub"));
        assert_eq!(
            o.get("final_pending"),
            o.get("final_pub") - o.get("final_ack")
        );
        assert!(o.get("final_pending") > 0);
        assert_eq!(o.get("native_entries"), 1);
        assert_eq!(o.get("native_exits"), 1);
        let mut tids = std::collections::HashSet::new();
        for i in 0..n {
            assert!(tids.insert(o.get(&format!("tid{i}"))));
            assert!(o.get(&format!("tid{i}")) > 0);
            assert_eq!(o.get(&format!("acks{i}")), o.get("completed"));
            assert_eq!(
                o.get(&format!("adoptions{i}")),
                if i == 0 { 0 } else { o.get("completed") }
            );
            assert_eq!(o.get(&format!("phase{i}")), o.get("phase"));
            assert_eq!(o.get(&format!("depth{i}")), 1);
            assert_eq!(o.get(&format!("pending{i}")), 0);
            assert_eq!(o.get(&format!("guard{i}")), 0);
            assert!(o.get(&format!("worker{i}")) > 0);
            assert_eq!(o.get(&format!("bytes{i}")), 98304);
            assert_eq!(o.get(&format!("leaf{i}")), (((i + 2) << 32) | 1023) as i64);
            assert_eq!(o.get(&format!("back{i}")), o.get("hub"));
            assert_eq!(
                o.get(&format!("parent_state{i}")),
                crate::gc_abi::OBJECT_PUBLISHED as i64
            );
            assert_eq!(
                o.get(&format!("leaf_state{i}")),
                crate::gc_abi::OBJECT_PUBLISHED as i64
            );
        }
    }
}

#[test]
fn production_next_old_round_waits_for_the_last_actual_parked_owner_to_adopt_and_resume() {
    for n in [2, 8] {
        let o = run(super::owner_boundaries::delayed_running_old_graph, n, true);
        assert_eq!(o.get("delayed_phase"), 0, "{}", o.text());
        assert!(o.get("resume_waits") > 0, "{}", o.text());
        assert_eq!(o.get("starts"), 1);
        assert_eq!(o.get("ends"), 1);
        assert_eq!(o.get("completed"), 1 + o.get("end_checks"));
        assert_eq!(o.get("end_checks"), 1 + o.get("end_retries"));
        assert_eq!(o.get("resuming"), 0);
        assert_eq!(o.get("closed_pub"), o.get("closed_ack"));
        for i in 0..n {
            assert_eq!(o.get(&format!("acks{i}")), o.get("completed"));
            assert_eq!(
                o.get(&format!("adoptions{i}")),
                if i == 0 { 0 } else { o.get("completed") }
            );
            assert_eq!(o.get(&format!("phase{i}")), o.get("phase"));
        }
    }
}

fn delayed_running_old_graph_logged(g: &mut crate::NativeCodeGenerator, n: u64) {
    g.gc_log = true;
    g.gc_log_format = crate::GcLogFormat::Json;
    g.gc_trace_capacity = 256;
    super::owner_boundaries::delayed_running_old_graph(g, n);
}

#[test]
fn production_park_timing_survives_delayed_old_phase_adoption() {
    let output = super::execute(delayed_running_old_graph_logged, 2, true, false);
    assert!(output.status.success(), "{output:?}");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let waits: u64 = stdout
        .lines()
        .find_map(|line| line.strip_prefix("resume_waits="))
        .unwrap()
        .parse()
        .unwrap();
    assert!(waits > 0, "{stdout}");
    let stderr = String::from_utf8(output.stderr).unwrap();
    let parked = stderr
        .lines()
        .filter(|line| {
            line.contains("\"event\":\"mutator_park\"") && line.contains("\"clock_status\":3")
        })
        .count();
    assert!(parked >= 2, "{stderr}");
}

fn invalid_start(g: &mut crate::NativeCodeGenerator, mode: u64) {
    use crate::gc_codegen::production::test_support::*;
    let mut h = super::H::begin(g);
    if mode < 4 {
        old_phase_entry_fault(h.g, mode);
        return;
    }
    h.alloc(1, 16, 0);
    h.root(0);
    publish_primary(h.g);
    let actor = create_actor(h.g, 16, 8);
    native_actor(h.g, actor, true);
    root_legacy_scope(h.g, true, false);
    native_actor(h.g, actor, false);
    old_phase_start_other(h.g, actor);
}
#[test]
fn production_old_phase_round_rejects_live_private_access_copying_young_cycle_and_real_legacy_borrow()
 {
    for mode in 0..5 {
        let o = super::run_failure(invalid_start, mode);
        assert!(!o.status.success(), "mode {mode} unexpectedly accepted");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant"),
            "mode {mode}: {o:?}"
        );
    }
}

#[test]
fn production_incomplete_old_end_check_reopens_and_resumes_real_owners_before_worker_completion() {
    for n in [1, 2, 8] {
        let o = run(super::owner_boundaries::retry_running_old_graph, n, true);
        assert_eq!(o.get("starts"), 1, "{}", o.text());
        assert_eq!(o.get("ends"), 1);
        assert!(o.get("end_retries") > 0, "{}", o.text());
        assert_eq!(o.get("end_checks"), 1 + o.get("end_retries"));
        assert_eq!(o.get("completed"), 1 + o.get("end_checks"));
        assert_eq!(o.get("resuming"), 0);
        assert_eq!(o.get("mark_active"), 0);
        assert_eq!(o.get("valid"), 1);
        assert!(o.get("closed_worker") > 0);
        assert_eq!(o.get("coordinator_slices"), 0);
        assert_eq!(o.get("bank"), 1);
        assert_eq!(o.get("closed_pub"), o.get("closed_ack"));
        for i in 0..n {
            assert_eq!(o.get(&format!("acks{i}")), o.get("completed"));
            assert_eq!(
                o.get(&format!("adoptions{i}")),
                if i == 0 { 0 } else { o.get("completed") }
            );
            assert_eq!(o.get(&format!("phase{i}")), o.get("phase"));
        }
    }
}
