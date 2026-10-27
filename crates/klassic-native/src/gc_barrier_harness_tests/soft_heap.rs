//! An advisory target changes policy without rejecting a larger reachable graph.
use super::{H, Src, execute_with_soft_limit};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::{MAX_PAYLOAD, test_support::*};

fn graph(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.report("initial_pages", |h| young_window_pages(h.g));
    h.report_counter("initial_target", Counter::PolicyTarget);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    // 18.75 MiB retained payload exceeds the 16-MiB soft target but fits the
    // unchanged 192-MiB hard pool. Keep every allocation in a distinct RootCell.
    h.repeat(150, |h| {
        h.alloc(1, MAX_PAYLOAD, 9);
        h.root(9);
    });
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.mark_start();
    h.drain_marks();
    young_trace_complete(h.g, &h.l);
    completed_mark_hint(h.g);
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report_counter("clamps", Counter::CapacityClamps);
    h.report("used", |h| old_used(h.g));
    h.report_counter("full_before", Counter::PolicyFull);
    h.report_counter("target", Counter::PolicyTarget);
    // A completed snapshot of the same live set must not cause a full move
    // at every allocation merely because its retained bytes exceed soft max.
    h.mark_start();
    h.drain_marks();
    young_trace_complete(h.g, &h.l);
    completed_mark_hint(h.g);
    set_worker_alive(h.g, true);
    h.alloc(1, 16, 5);
    set_worker_alive(h.g, false);
    h.finish_collection();
    h.report_counter("full_after", Counter::PolicyFull);
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
}
#[test]
fn production_soft_heap_changes_policy_and_retains_live_data_above_target_without_repeat_full() {
    for soft in [None, Some(16 << 20)] {
        let output = execute_with_soft_limit(graph, 0, false, false, soft);
        let text = String::from_utf8_lossy(&output.stdout);
        assert!(output.status.success(), "{output:?}");
        let get = |name: &str| -> u64 {
            text.lines()
                .filter_map(|l| l.split_once('='))
                .find(|(key, _)| *key == name)
                .unwrap()
                .1
                .parse()
                .unwrap()
        };
        assert_eq!(get("initial_pages"), if soft.is_some() { 21 } else { 256 });
        assert_eq!(
            get("initial_target"),
            if soft.is_some() {
                (16 << 20) / 3 / (128 << 10) * (128 << 10) / 4
            } else {
                16 << 20
            }
        );
        assert!(get("used") > 16 << 20);
        if soft.is_some() {
            assert!(get("clamps") > 0, "{text}");
            assert!(get("target") <= (16 << 20) / 6, "{text}");
        }
        assert_eq!(get("full_after"), get("full_before"), "{text}");
        assert_eq!(get("value"), 84);
        assert_eq!(get("id"), 1);
    }
}

fn early_old(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.repeat(50, |h| {
        h.alloc(1, MAX_PAYLOAD, 9);
        h.root(9);
    });
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.report("used", |h| old_used(h.g));
    h.mark_start();
    h.drain_marks();
    young_trace_complete(h.g, &h.l);
    completed_mark_hint(h.g);
    h.report_counter("plans_before", Counter::OldPlannedCycles);
    set_worker_alive(h.g, true);
    h.alloc(1, 16, 5);
    set_worker_alive(h.g, false);
    h.finish_collection();
    h.report_counter("plans_after", Counter::OldPlannedCycles);
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_soft_heap_requests_an_old_move_before_the_hard_geometry_threshold() {
    for soft in [None, Some(16 << 20)] {
        let output = execute_with_soft_limit(early_old, 0, false, false, soft);
        let text = String::from_utf8_lossy(&output.stdout);
        assert!(output.status.success(), "{output:?}");
        let get = |name: &str| -> u64 {
            text.lines()
                .filter_map(|l| l.split_once('='))
                .find(|(key, _)| *key == name)
                .unwrap()
                .1
                .parse()
                .unwrap()
        };
        assert!(get("used") > 5 << 20 && get("used") < 16 << 20, "{text}");
        assert_eq!(
            get("plans_after") - get("plans_before"),
            u64::from(soft.is_some()),
            "{text}"
        );
        assert_eq!(get("value"), 84);
    }
}
