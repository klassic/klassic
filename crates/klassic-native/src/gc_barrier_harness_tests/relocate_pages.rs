//! Real owner TLAB growth, publication, and budgeted Cheney resumption.
use super::{H, Src, run};
use crate::gc_codegen::production::test_support::*;

fn growing(g: &mut crate::NativeCodeGenerator, n: u64) {
    let mut h = H::begin(g);
    let actors: Vec<_> = (0..n).map(|_| create_actor(h.g, 8, 64)).collect();
    let parallel = run_parallel_actors(h.g, &actors, 1);
    verify_parallel_actors(h.g, &parallel);
    for i in 0..n {
        h.report(&format!("tid{i}"), |h| {
            parallel_actor_tid(h.g, &parallel, i as usize)
        });
    }
    relocate_page_begin(h.g);
    h.report("first", |h| relocate_page_quantum(h.g, 128));
    h.report("first_count", |h| relocate_page_completed(h.g));
    h.report("first_pending", |h| relocate_page_pending(h.g));
    for (i, &a) in actors.iter().enumerate() {
        h.report(&format!("cursor_before{i}"), |h| {
            relocate_page_cursor(h.g, a)
        });
        actor_alloc(h.g, a, 16); // Earlier pages remain independently open.
        actor_publish(h.g, a);
    }
    h.report("late_pending", |h| relocate_page_pending(h.g));
    h.report("second", |h| relocate_page_quantum(h.g, 128));
    h.report("second_count", |h| relocate_page_completed(h.g));
    h.report("second_pending", |h| relocate_page_pending(h.g));
    for (i, &a) in actors.iter().enumerate() {
        h.report(&format!("cursor_after{i}"), |h| {
            relocate_page_cursor(h.g, a)
        });
    }
    relocate_page_begin(h.g); // A fresh scan epoch must revisit the same pages.
    h.report("next_epoch", |h| relocate_page_quantum(h.g, 128));
    h.report("next_epoch_count", |h| relocate_page_completed(h.g));
}

#[test]
fn production_cheney_revisits_independently_growing_tlabs_and_invalidates_its_actual_publication_certificate()
 {
    for n in [1, 2, 8] {
        let o = run(growing, n, false);
        assert_eq!(o.get("first"), 1, "{}", o.text());
        assert_eq!(o.get("first_count"), n as i64);
        assert_eq!(o.get("first_pending"), 0);
        assert_eq!(o.get("late_pending"), 1);
        assert_eq!(o.get("second"), 1);
        assert_eq!(o.get("second_count"), n as i64);
        assert_eq!(o.get("second_pending"), 0);
        let mut tids = std::collections::HashSet::new();
        for i in 0..n {
            assert!(tids.insert(o.get(&format!("tid{i}"))));
            assert_eq!(
                o.get(&format!("cursor_after{i}")) - o.get(&format!("cursor_before{i}")),
                48
            );
        }
        assert_eq!(o.get("next_epoch"), 1);
        assert_eq!(o.get("next_epoch_count"), 2 * n as i64);
    }
}

fn one_unit(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let actors: Vec<_> = (0..8).map(|_| create_actor(h.g, 8, 64)).collect();
    let parallel = run_parallel_actors(h.g, &actors, 1);
    verify_parallel_actors(h.g, &parallel);
    relocate_page_begin(h.g);
    let stats = relocate_page_one_unit_finish(h.g);
    h.report("objects", |h| relocate_page_test_stat(h.g, stats, 0));
    h.report("slices", |h| relocate_page_test_stat(h.g, stats, 1));
    h.report("pending", |h| relocate_page_pending(h.g));
}

#[test]
fn production_cheney_one_unit_slices_cross_eight_owner_pages_without_retracing_or_starvation() {
    let o = run(one_unit, 0, false);
    assert_eq!(o.get("objects"), 8, "{}", o.text());
    assert!(o.get("slices") > 8 && o.get("slices") <= 64);
    assert_eq!(o.get("pending"), 0);
}

fn initializing(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let a = create_actor(h.g, 8, 64);
    let b = create_actor(h.g, 8, 64);
    actor_alloc(h.g, a, 16); // Real INITIALIZING header, on an earlier page.
    actor_alloc(h.g, b, 16);
    actor_publish(h.g, b);
    relocate_page_begin(h.g);
    h.report("blocked", |h| relocate_page_quantum(h.g, 16));
    h.report("blocked_count", |h| relocate_page_completed(h.g));
    h.report("blocked_pending", |h| relocate_page_pending(h.g));
    actor_publish(h.g, a);
    h.report("ready", |h| relocate_page_quantum(h.g, 16));
    h.report("ready_count", |h| relocate_page_completed(h.g));
    h.report("ready_pending", |h| relocate_page_pending(h.g));
}

#[test]
fn production_cheney_does_not_parse_an_initializing_object_on_an_earlier_owner_page() {
    let o = run(initializing, 0, false);
    assert_eq!(o.get("blocked"), 0, "{}", o.text());
    assert_eq!(o.get("blocked_count"), 0);
    assert_eq!(o.get("blocked_pending"), 1);
    assert_eq!(o.get("ready"), 1);
    assert_eq!(o.get("ready_count"), 2);
    assert_eq!(o.get("ready_pending"), 0);
}

fn copy_ready(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    h.collect();
    h.report("ready", |h| {
        h.value(Src::Root(0)); // Actual load-barrier copy; no owner poll follows.
        actor_header_state(h.g);
    });
    h.report("copies", |h| h.counter(Counter::MutatorCopies));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.finish_collection();
}

#[test]
fn production_load_barrier_copy_finishes_owned_initialization_before_returning() {
    let o = run(copy_ready, 0, false);
    assert_eq!(
        o.get("ready"),
        crate::gc_abi::OBJECT_PUBLISHED as i64,
        "{}",
        o.text()
    );
    assert_eq!(o.get("copies"), 1);
    assert_eq!(o.get("value"), 42);
}

fn exhausted(g: &mut crate::NativeCodeGenerator, _: u64) {
    let h = H::begin(g);
    relocate_page_exhaust_epoch(h.g);
}

#[test]
fn production_cheney_rejects_scan_epoch_exhaustion_before_side_table_reuse() {
    let o = super::execute(exhausted, 0, false, false);
    assert_eq!(o.status.code(), Some(1), "{o:?}");
    assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
}
