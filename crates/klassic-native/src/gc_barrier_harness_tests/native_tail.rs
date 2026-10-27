//! Repeated real Native boundaries must not spend evacuation's page reserve.
use super::{H, run};
use crate::gc_codegen::production::test_support::*;

fn churn(g: &mut crate::NativeCodeGenerator, arg: u64) {
    if arg & 512 != 0 {
        // Keep the entire real window within the remaining 384-entry peer
        // workload. The original 256-MiB churn fixture remains unchanged.
        g.gc_max_heap = 64 << 20;
    }
    let mut h = H::begin(g);
    observe_unexpected_young_oom(h.g);
    publish_primary(h.g);
    let n = arg & 255;
    let full = arg & 256 != 0;
    let refill = (arg & 512 != 0).then(|| pause_refill_return_for_peer(h.g));
    let mut actors = Vec::new();
    let mut roots = Vec::new();
    for i in 0..n {
        let actor = create_actor(h.g, 16, 8);
        let root = native_completed_graph(h.g, actor, 42 + i);
        h.report(&format!("id_before{i}"), |h| {
            native_completed_root(h.g, actor, root, true)
        });
        copy_actor_native_enter(h.g, actor);
        actors.push(actor);
        roots.push(root);
    }
    let p = run_parallel_native_churn(h.g, &actors, &roots, 384, full);
    h.report_counter("collections", Counter::Collections);
    h.report_counter("full", Counter::FullCollections);
    h.report("tail_bytes", |h| native_tail_observe(h.g, 3));
    if let Some(observed) = refill {
        h.report("return_full", |h| refill_return_observed(h.g, observed, 1));
        h.report("return_bank", |h| refill_return_observed(h.g, observed, 2));
    }
    for (i, actor) in actors.iter().enumerate() {
        native_completed_resume(h.g, *actor);
        h.report(&format!("id{i}"), |h| {
            native_completed_root(h.g, *actor, roots[i], true)
        });
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        h.report(&format!("value{i}"), |h| {
            native_completed_metric(h.g, &p, i, 64)
        });
        h.report(&format!("fresh{i}"), |h| {
            native_completed_metric(h.g, &p, i, 72)
        });
        h.report(&format!("entries{i}"), |h| {
            actor_metric(h.g, *actor, ActorMetric::NativeEntries)
        });
        h.report(&format!("exits{i}"), |h| {
            actor_metric(h.g, *actor, ActorMetric::NativeExits)
        });
    }
}

#[test]
fn production_refill_return_services_a_real_peer_round_after_its_window_fills() {
    for n in [2, 8] {
        let o = run(churn, n | 512, true);
        assert_eq!(o.get("return_full"), 1, "{}", o.text());
        assert!(o.get("return_bank") > 0, "{}", o.text());
        assert_eq!(o.get("full"), 0, "{}", o.text());
        for i in 0..n {
            assert_eq!(
                o.get(&format!("id{i}")),
                o.get(&format!("id_before{i}")),
                "{}",
                o.text()
            );
            assert_eq!(o.get(&format!("value{i}")), 42 + i as i64, "{}", o.text());
            assert_eq!(o.get(&format!("fresh{i}")), 99, "{}", o.text());
            assert_eq!(o.get(&format!("entries{i}")), 386, "{}", o.text());
            assert_eq!(o.get(&format!("exits{i}")), 386, "{}", o.text());
        }
    }
}

#[test]
fn production_repeated_native_entries_preserve_copy_reserve_and_complete_young_and_full_moves() {
    for full in [false, true] {
        for n in [1, 2, 8] {
            let o = run(churn, n | if full { 256 } else { 0 }, true);
            assert!(o.get("collections") >= 1);
            assert_eq!(o.get("full"), i64::from(full));
            assert!(o.get("tail_bytes") > 0);
            let mut tids = std::collections::HashSet::new();
            for i in 0..n {
                assert!(tids.insert(o.get(&format!("tid{i}"))));
                assert_eq!(o.get(&format!("id{i}")), o.get(&format!("id_before{i}")));
                assert_eq!(o.get(&format!("value{i}")), 42 + i as i64);
                assert_eq!(o.get(&format!("fresh{i}")), 99);
                assert_eq!(o.get(&format!("entries{i}")), 386);
                assert_eq!(o.get(&format!("exits{i}")), 386);
            }
        }
    }
}

fn debit(g: &mut crate::NativeCodeGenerator, batches: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    publish_primary(h.g);
    for _ in 0..2 {
        let actor = create_actor(h.g, 16, 8);
        copy_actor_native_enter(h.g, actor);
    }
    native_tail_start(h.g);
    for _ in 0..batches {
        h.alloc(1, 50_000, 1);
    }
    h.report("budget_before", |h| native_tail_observe(h.g, 0));
    h.report("tail", |h| native_tail_observe(h.g, 1));
    h.report("open_before", |h| native_tail_observe(h.g, 2));
    let l = &h.g.gc_gen.as_ref().unwrap().labels;
    let (enter, exit) = (l.native_enter_actor, l.native_exit_actor);
    call_owner(h.g, enter);
    call_owner(h.g, exit);
    h.report("budget_after", |h| native_tail_observe(h.g, 0));
    h.report("open_after", |h| native_tail_observe(h.g, 2));
    h.report("tail_bytes", |h| native_tail_observe(h.g, 3));
    h.report("tail_waits", |h| native_tail_observe(h.g, 4));
}

#[test]
fn production_native_entry_funds_the_actual_discarded_tlab_tail() {
    let o = run(debit, 1, true);
    assert_eq!(o.get("open_before"), 1);
    assert_eq!(o.get("open_after"), 1);
    assert!(o.get("tail") > 0);
    assert_eq!(o.get("tail_waits"), 0);
    assert_eq!(o.get("tail_bytes"), o.get("tail"));
    assert_eq!(
        o.get("budget_after"),
        o.get("budget_before") - o.get("tail"),
        "{}",
        o.text()
    );
}

#[test]
fn production_native_entry_finishes_the_move_before_discarding_an_unfunded_tail() {
    let o = run(debit, 7, true);
    assert_eq!(o.get("open_before"), 1);
    assert!(o.get("tail") > o.get("budget_before"), "{}", o.text());
    assert_eq!(o.get("open_after"), 0, "{}", o.text());
    assert!(o.get("tail_waits") > 0);
    assert_eq!(o.get("tail_bytes"), 0);
}
