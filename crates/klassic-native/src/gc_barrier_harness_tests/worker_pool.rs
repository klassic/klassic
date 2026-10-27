//! Real two-worker lifetime, independent progress and partial launch failure.
use super::satb_fixture::run;
use super::{H, Src};
use crate::gc_codegen::linux_production::Fault;
use crate::gc_codegen::production::test_support::*;

fn lifecycle(g: &mut crate::NativeCodeGenerator, fault: u64) {
    g.linux_worker_fault = match fault {
        0 => None,
        1 => Some(Fault::SecondClone),
        2 => Some(Fault::SecondBootstrap),
        _ => unreachable!(),
    };
    let mut h = H::begin(g);
    h.report("live", |h| gc_worker_pool_metric(h.g, 0));
    h.report("launched", |h| gc_worker_pool_metric(h.g, 1));
    for index in 0..2 {
        h.report(&format!("slot{index}"), |h| pool_slot_address(h.g, index));
        for (field, name) in ["tid", "stack", "tls"].into_iter().enumerate() {
            h.report(&format!("{name}{index}"), |h| {
                pool_linux_metric(h.g, index, field)
            });
        }
    }
    // A failed second launch must leave the first worker capable of actual
    // heap service, without releasing its TLS or stack out from under it.
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77123);
    h.root(0);
    h.collect();
    if fault != 0 {
        pool_wait_work(h.g, 0, 3, 1);
        h.report("surviving_worker_relocation", |h| {
            pool_worker_metric(h.g, 0, 3)
        });
    }
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
    pool_stop(h.g);
    pool_stop(h.g);
    for (field, name) in [
        "live_after",
        "launched_after",
        "stopped",
        "active",
        "entered",
        "completed",
    ]
    .into_iter()
    .enumerate()
    {
        h.report(name, |h| gc_worker_pool_metric(h.g, field));
    }
    for index in 0..2 {
        for (field, name) in [
            "tid_after",
            "stack_after",
            "tls_after",
            "joins",
            "releases",
            "bootstrap",
        ]
        .into_iter()
        .enumerate()
        {
            h.report(&format!("{name}{index}"), |h| {
                pool_linux_metric(h.g, index, field)
            });
        }
    }
}

#[test]
fn production_two_gc_workers_have_distinct_os_resources_and_join_once() {
    let (o, _image) = run(lifecycle, 0, true);
    assert_eq!(o.get("live"), 2, "{}", o.text());
    assert_eq!(o.get("launched"), 2);
    assert_ne!(o.get("tid0"), o.get("tid1"));
    assert_ne!(o.get("stack0"), o.get("stack1"));
    assert_ne!(o.get("slot0"), o.get("slot1"));
    for index in 0..2 {
        assert!(o.get(&format!("tid{index}")) > 0);
        assert!(o.get(&format!("stack{index}")) > 0);
        assert_eq!(
            o.get(&format!("tls{index}")),
            o.get(&format!("slot{index}"))
        );
        assert_eq!(o.get(&format!("joins{index}")), 1);
        assert_eq!(o.get(&format!("releases{index}")), 1);
        assert_eq!(o.get(&format!("tid_after{index}")), 0);
        assert_eq!(o.get(&format!("stack_after{index}")), 0);
    }
    assert_eq!(o.get("value"), 77123);
    assert_eq!(o.get("live_after"), 0);
    assert_eq!(o.get("stopped"), 2);
    assert_eq!(o.get("active"), 0);
    assert!(o.get("entered") > 0);
    assert_eq!(o.get("entered"), o.get("completed"));
}

#[test]
fn production_second_gc_worker_launch_failure_preserves_first_and_rolls_back_only_failed_slot() {
    for fault in [1, 2] {
        let (o, _image) = run(lifecycle, fault, true);
        assert_eq!(o.get("live"), 1, "fault={fault}: {}", o.text());
        assert_eq!(o.get("launched"), 1);
        assert!(o.get("tid0") > 0);
        assert_eq!(o.get("tid1"), 0);
        assert_eq!(o.get("stack1"), 0);
        assert_eq!(o.get("tls0"), o.get("slot0"));
        assert_eq!(o.get("joins0"), 1);
        assert_eq!(o.get("joins1"), i64::from(fault == 2));
        assert_eq!(o.get("releases0"), 1);
        assert_eq!(o.get("releases1"), 1);
        assert_eq!(o.get("value"), 77123);
        assert!(o.get("surviving_worker_relocation") > 0);
        assert_eq!(o.get("live_after"), 0);
        assert_eq!(o.get("stopped"), 1);
        assert_eq!(o.get("active"), 0);
        assert_eq!(o.get("entered"), o.get("completed"));
    }
}

fn progress(g: &mut crate::NativeCodeGenerator, _: u64) {
    let gates = pool_gates(g);
    let mut h = H::begin(g);
    h.alloc(2, 2048 * 8, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 19073);
    for index in 0..2048 {
        h.store(Src::Root(0), index, Src::Raw(1), true);
    }
    publish_primary(h.g);
    dual_mark_enable(h.g);
    dual_mark_entry(h.g, false, true);
    dual_mark_entry(h.g, true, true);
    // Delay worker1 before its collector ticket, not while it owns the lock.
    pool_set_gate(h.g, gates, 4, 1);
    pool_wait_parked(h.g, gates);
    pool_set_gate(h.g, gates, 0, 64);
    pool_set_gate(h.g, gates, 2, 64);
    pool_wait_work(h.g, 0, 1, 64);
    pool_wait_work(h.g, 0, 2, 64);
    h.report("paused_worker_active", |h| pool_worker_metric(h.g, 1, 4));
    h.report("paused_worker_old", |h| pool_worker_metric(h.g, 1, 1));
    h.report("paused_worker_young", |h| pool_worker_metric(h.g, 1, 2));
    // A real mutator allocation/store progresses while the other worker waits.
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 4417);
    h.report("mutator_progress", |h| h.load(Src::Raw(2), 0));
    pool_set_gate(h.g, gates, 1, 64);
    pool_set_gate(h.g, gates, 3, 64);
    pool_set_gate(h.g, gates, 4, 2);
    pool_wait_work(h.g, 1, 1, 64);
    pool_wait_work(h.g, 1, 2, 64);
    for index in 0..2 {
        h.report(&format!("tid{index}"), |h| pool_linux_metric(h.g, index, 0));
        h.report(&format!("old{index}"), |h| {
            pool_worker_metric(h.g, index, 1)
        });
        h.report(&format!("young{index}"), |h| {
            pool_worker_metric(h.g, index, 2)
        });
    }
    for field in 0..4 {
        pool_set_gate(h.g, gates, field, u64::MAX);
    }
    set_young_auto(h.g, true);
    satb_poll_until_closed(h.g);
    h.load_barriered(Src::Root(0), 2047);
    h.keep(3);
    h.report("leaf", |h| h.load(Src::Raw(3), 0));
    pool_stop(h.g);
    h.report("active_after", |h| gc_worker_pool_metric(h.g, 3));
    h.report("entered_after", |h| gc_worker_pool_metric(h.g, 4));
    h.report("completed_after", |h| gc_worker_pool_metric(h.g, 5));
}

#[test]
fn production_both_gc_workers_trace_young_and_old_and_mutator_progresses_past_delayed_peer() {
    let (o, _image) = run(progress, 0, true);
    assert_eq!(o.get("paused_worker_active"), 0, "{}", o.text());
    assert_eq!(o.get("paused_worker_old"), 0);
    assert_eq!(o.get("paused_worker_young"), 0);
    assert_eq!(o.get("mutator_progress"), 4417);
    assert!(o.get("tid0") > 0);
    assert!(o.get("tid1") > 0);
    assert_ne!(o.get("tid0"), o.get("tid1"));
    for index in 0..2 {
        assert_eq!(o.get(&format!("old{index}")), 64);
        assert_eq!(o.get(&format!("young{index}")), 64);
    }
    assert_eq!(o.get("leaf"), 19073);
    assert_eq!(o.get("active_after"), 0);
    assert_eq!(o.get("entered_after"), o.get("completed_after"));
}

fn second_epoch_fault(g: &mut crate::NativeCodeGenerator, fault: u64) {
    worker_epoch_fault_for(g, fault, Some(1));
    let h = H::begin(g);
    pool_wait_work(h.g, 1, 5, 2);
}

#[test]
fn production_second_gc_worker_epoch_blocks_invalid_phase_and_bank_changes() {
    for fault in [0, 4] {
        let o = super::execute(second_epoch_fault, fault, true, false);
        assert_eq!(o.status.code(), Some(1), "fault={fault}: {o:?}");
        let diagnostic = String::from_utf8_lossy(&o.stderr);
        assert!(
            diagnostic.contains("selected GC worker epoch fault reached"),
            "{o:?}"
        );
        assert!(diagnostic.contains("runtime invariant violated"));
    }
}
