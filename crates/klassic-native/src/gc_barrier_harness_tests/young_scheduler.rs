//! Real N-owner allocation-driven Young/Old cycles on one heap and worker.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};

fn resumed_debt(g: &mut crate::NativeCodeGenerator, n: u64) {
    let observed = observe_resume_transport(g);
    graph(g, n);
    let mut h = H::begin(g);
    for (i, name) in [
        "seen",
        "debt",
        "retry",
        "sequence_before",
        "sequence_after",
        "resuming_before",
        "round_kind",
    ]
    .into_iter()
    .enumerate()
    {
        h.report(name, |h| resume_transport_metric(h.g, observed, i as i32));
    }
}

#[test]
fn production_last_actual_peer_resume_retries_and_notifies_retained_journal_debt() {
    for n in [2, 8] {
        let o = run(resumed_debt, n, true);
        assert_eq!(o.get("seen"), 1, "{}", o.text());
        assert!(o.get("debt") > 0);
        assert_eq!(o.get("retry"), 1);
        assert_eq!(o.get("resuming_before"), 1);
        assert!(o.get("round_kind") > 0);
        assert!(o.get("sequence_after") > o.get("sequence_before"));
        assert_eq!(o.get("young_starts"), o.get("young_ends"));
        assert_eq!(o.get("young_active"), 0);
        assert_eq!(o.get("old_active"), 0);
        for i in 0..n {
            assert_eq!(o.get(&format!("acks{i}")), o.get("completed"));
        }
    }
}

fn no_replay(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 8, 0);
    h.root(0);
    publish_primary(h.g);
    set_young_auto(h.g, true);
    young_scheduler_minimum_target(h.g);
    dual_mark_enable(h.g);
    h.repeat(32768, |h| h.alloc(1, 16, 1));
    publish_primary(h.g);
    finish_scheduled_primary(h.g);
    h.report("epoch_before", |h| dual_mark_metric(h.g, 6));
    h.report("old_before", |h| h.counter(Counter::MarkCycles));
    h.report("window_before", |h| young_scheduler_metric(h.g, 0));
    h.report("allocated_before", |h| young_scheduler_metric(h.g, 4));
    let poll = h.g.gc_gen.as_ref().unwrap().labels.poll_actor;
    h.repeat(100, |h| call_owner(h.g, poll));
    h.report("epoch_after", |h| dual_mark_metric(h.g, 6));
    h.report("old_after", |h| h.counter(Counter::MarkCycles));
    h.report("window_after", |h| young_scheduler_metric(h.g, 0));
    h.report("allocated_after", |h| young_scheduler_metric(h.g, 4));
}

#[test]
fn production_idle_ordinary_polls_do_not_replay_young_snapshots_or_spend_fresh_credit() {
    let o = run(no_replay, 0, true);
    assert!(o.get("epoch_before") > 0, "{}", o.text());
    for (a, b) in [
        ("epoch_before", "epoch_after"),
        ("old_before", "old_after"),
        ("window_before", "window_after"),
        ("allocated_before", "allocated_after"),
    ] {
        assert_eq!(o.get(a), o.get(b));
    }
}

fn separate_credit(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 8, 0);
    h.root(0);
    publish_primary(h.g);
    dual_mark_enable(h.g);
    h.report("old_before", |h| scheduled_old_metric(h.g, 1));
    h.report("young_before", |h| young_scheduler_metric(h.g, 0));
    h.alloc(1, 16, 1);
    publish_primary(h.g);
    dual_mark_entry(h.g, true, true);
    h.report("old_after", |h| scheduled_old_metric(h.g, 1));
    h.report("young_after", |h| young_scheduler_metric(h.g, 0));
    h.report("allocated", |h| young_scheduler_metric(h.g, 4));
    set_young_auto(h.g, true);
    finish_scheduled_primary(h.g);
    h.report("ends", |h| dual_mark_metric(h.g, 1));
    h.report("samples", |h| young_scheduler_metric(h.g, 5));
    h.report_counter("invalid_samples", Counter::StartInvalid);
    h.report("sample_open", |h| young_scheduler_metric(h.g, 8));
    h.report("worker_units", |h| dual_mark_metric(h.g, 7));
}

#[test]
fn production_actual_young_start_consumes_only_its_own_allocation_window() {
    let o = run(separate_credit, 0, true);
    assert_eq!(o.get("old_before"), o.get("old_after"), "{}", o.text());
    assert!(o.get("young_after") > o.get("young_before"));
    assert_eq!(o.get("young_after"), o.get("allocated"));
    assert_eq!(o.get("ends"), 1);
    // Owner tracing can finish this tiny snapshot before the worker runs.
    // Only actual worker work supplies a sample; either outcome closes once.
    assert_eq!(
        o.get("samples") + o.get("invalid_samples"),
        1,
        "{}",
        o.text()
    );
    if o.get("worker_units") == 0 {
        assert_eq!(o.get("samples"), 0);
    }
    assert_eq!(o.get("sample_open"), 0);
}

fn debt(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.alloc(1, 16, 1);
    publish_primary(h.g);
    let region = journal_region_new(h.g, 65);
    dual_mark_enable(h.g);
    let gate = captured_journal_gate(h.g);
    h.repeat(32, |h| {
        h.value(Src::Raw(0));
        journal_region_store(h.g, region, false);
        h.value(Src::Null);
        journal_region_store(h.g, region, false);
    });
    dual_mark_entry(h.g, true, true);
    dual_mark_wait(h.g, None); // Actual graph complete; real journal debt remains.
    set_young_auto(h.g, true);
    let poll = h.g.gc_gen.as_ref().unwrap().labels.poll_actor;
    call_owner(h.g, poll);
    h.report("pending", |h| root_obligation_counter(h.g, 0));
    h.report("end_requests_before", |h| young_scheduler_metric(h.g, 2));
    h.report("end_checks_before", |h| dual_mark_metric(h.g, 2));
    captured_journal_release(h.g, gate);
    finish_scheduled_primary(h.g);
    h.report("ends", |h| dual_mark_metric(h.g, 1));
    h.report("end_requests_after", |h| young_scheduler_metric(h.g, 2));
    h.report("pending_after", |h| root_obligation_counter(h.g, 0));
}

#[test]
fn production_normal_young_scheduler_defers_end_requests_for_real_journal_debt() {
    let o = run(debt, 0, true);
    assert!(o.get("pending") >= 64, "{}", o.text());
    assert_eq!(o.get("end_requests_before"), 0);
    assert_eq!(o.get("end_checks_before"), 0);
    assert_eq!(o.get("ends"), 1);
    assert!(o.get("end_requests_after") >= 1);
    assert_eq!(o.get("pending_after"), 0);
}

fn graph(g: &mut crate::NativeCodeGenerator, argument: u64) {
    let n = argument & 255;
    let mut h = H::begin(g);
    h.alloc(2, n * 8, 0);
    h.root(0);
    publish_primary(h.g);
    if argument & 256 != 0 {
        set_young_auto(h.g, true);
        for _ in 0..2 {
            set_tenuring(h.g, 2);
            h.g.call_label(h.l.young_start);
            h.collect();
            h.finish_collection();
        }
        h.report("hub_old", |h| h.is_old(Src::Root(0)));
    }
    h.report("epoch_initial", |h| dual_mark_metric(h.g, 6));
    h.report("bank_initial", |h| dual_mark_metric(h.g, 12));
    h.report("generation_initial", |h| dual_mark_metric(h.g, 15));
    h.report_counter("samples_initial", Counter::StartMarkSamples);
    let actors: Vec<_> = (0..n).map(|_| create_actor(h.g, 16, 8)).collect();
    h.g.asm
        .data
        .resize(h.g.asm.data.len().next_multiple_of(8), 0);
    let master = h.g.asm.data_label_with_i64s(&[0]);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    h.g.mov_data_addr(R::V1, master);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    let iterations = 32768 / n;
    let p = run_parallel_scheduled_dual_graph(h.g, &actors, iterations, master);
    for (i, name) in [
        "young_starts",
        "young_ends",
        "checks",
        "retries",
        "young_active",
        "young_valid",
        "young_epoch",
        "young_worker",
        "young_marked",
        "old_active",
        "old_valid",
        "old_worker",
        "bank",
    ]
    .iter()
    .enumerate()
    {
        h.report(name, |h| dual_mark_metric(h.g, i));
    }
    for (i, name) in [
        "window",
        "start_requests",
        "end_requests",
        "polls",
        "allocated",
        "mark_samples",
        "sample_units",
        "sample_ns",
        "pending_sample",
        "sealed",
        "trace",
        "black",
    ]
    .iter()
    .enumerate()
    {
        h.report(name, |h| young_scheduler_metric(h.g, i));
    }
    h.report("stale_young_starts", |h| dual_mark_metric(h.g, 13));
    h.report_counter("old_cycles", Counter::MarkCycles);
    h.report_counter("phase", Counter::AdoptedPhase);
    h.report("completed", |h| checkpoint_metric(h.g, 0));
    h.report("expected", |h| checkpoint_metric(h.g, 1));
    h.report("hub", |h| h.value(Src::Root(0)));
    for (i, actor) in actors.into_iter().enumerate() {
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        h.report(&format!("phase{i}"), |h| {
            actor_metric(h.g, actor, ActorMetric::Phase)
        });
        h.report(&format!("acks{i}"), |h| {
            actor_metric(h.g, actor, ActorMetric::CheckpointAcks)
        });
        h.report(&format!("reads{i}"), |h| parallel_graph_reads(h.g, &p, i));
        h.load_barriered(Src::Root(0), i as u32);
        h.keep(i as u32 + 1);
        h.report(&format!("leaf{i}"), |h| {
            h.load_barriered(Src::Raw(i as u32 + 1), 0);
            h.g.load_ptr_disp32(R::V0, R::V0, 0);
        });
        h.report(&format!("back{i}"), |h| {
            h.load_barriered(Src::Raw(i as u32 + 1), 1)
        });
    }
}

#[test]
fn production_normal_young_and_old_schedulers_trace_a_real_promoted_hub_with_young_backlinks() {
    for n in [1, 2, 8] {
        let o = run(graph, n | 256, true);
        assert_eq!(o.get("hub_old"), 1, "{}", o.text());
        assert!(o.get("young_starts") >= 2);
        assert_eq!(o.get("young_starts"), o.get("young_ends"));
        assert_eq!(
            o.get("young_epoch") - o.get("epoch_initial"),
            o.get("young_ends")
        );
        assert_eq!(
            o.get("mark_samples") - o.get("samples_initial"),
            o.get("young_ends")
        );
        assert_eq!(o.get("bank"), o.get("bank_initial"));
        assert!(o.get("young_worker") > 0 && o.get("old_worker") > 0);
        assert!(o.get("old_cycles") >= 2);
        assert_eq!(o.get("young_active"), 0);
        assert_eq!(o.get("old_active"), 0);
        assert_eq!(o.get("pending_sample"), 0);
        for i in 0..n {
            assert_eq!(
                o.get(&format!("acks{i}")),
                o.get("completed") - o.get("generation_initial")
            );
            assert_eq!(o.get(&format!("phase{i}")), o.get("phase"));
            assert_eq!(
                o.get(&format!("leaf{i}")),
                (((i + 2) << 32) | (32768 / n - 1)) as i64
            );
            assert_eq!(o.get(&format!("back{i}")), o.get("hub"));
        }
    }
}

#[test]
fn production_normal_allocations_schedule_multiple_young_and_old_cycles_on_one_two_and_eight_owners()
 {
    for n in [1, 2, 8] {
        let o = run(graph, n, true);
        assert!(o.get("young_starts") >= 2, "{}", o.text());
        assert_eq!(o.get("young_starts"), o.get("young_ends"));
        assert_eq!(o.get("young_epoch"), o.get("young_starts"));
        assert_eq!(o.get("mark_samples"), o.get("young_ends"));
        assert!(o.get("start_requests") >= o.get("young_starts"));
        assert!(o.get("end_requests") >= o.get("young_ends"));
        assert_eq!(o.get("young_active"), 0);
        assert_eq!(o.get("old_active"), 0);
        assert_eq!(o.get("young_valid"), 1);
        assert_eq!(o.get("old_valid"), 1);
        assert!(o.get("old_cycles") >= 2);
        assert_eq!(o.get("pending_sample"), 0);
        assert!(o.get("sample_units") > 0 && o.get("sample_ns") > 0);
        assert_eq!(o.get("sealed"), o.get("trace") + o.get("black"));
        assert!(o.get("young_worker") > 0 && o.get("old_worker") > 0);
        assert!(o.get("window") <= o.get("allocated"));
        assert_eq!(o.get("expected"), n as i64);
        assert_eq!(o.get("bank"), 1);
        let mut tids = std::collections::HashSet::new();
        for i in 0..n {
            assert!(tids.insert(o.get(&format!("tid{i}"))));
            assert_eq!(o.get(&format!("phase{i}")), o.get("phase"));
            assert_eq!(o.get(&format!("acks{i}")), o.get("completed"));
            assert_eq!(
                o.get(&format!("leaf{i}")),
                (((i + 2) << 32) | (32768 / n - 1)) as i64
            );
            assert_eq!(o.get(&format!("back{i}")), o.get("hub"));
            assert!(o.get(&format!("reads{i}")) > 0);
        }
    }
}

fn delayed(g: &mut crate::NativeCodeGenerator, n: u64) {
    let request = delay_young_scheduler_request(g);
    graph(g, n);
    let mut h = H::begin(g);
    h.report("delayed_observed", |h| {
        scheduler_delay_metric(h.g, request, true)
    });
    h.report("delayed_requested", |h| {
        scheduler_delay_metric(h.g, request, false)
    });
    h.report("delayed_actual_token", |h| {
        delayed_young_entry_metric(h.g, request, false)
    });
    h.report("delayed_entry_seen", |h| {
        delayed_young_entry_metric(h.g, request, true)
    });
}
#[test]
fn production_delayed_normal_young_request_does_not_replay_or_spend_another_cycle_window() {
    for n in [2, 8] {
        let o = run(delayed, n, true);
        assert_eq!(o.get("delayed_requested"), 0, "{}", o.text());
        assert!(o.get("delayed_observed") >= 1);
        assert_eq!(o.get("delayed_entry_seen"), 1, "{}", o.text());
        assert_eq!(o.get("delayed_actual_token"), 0, "{}", o.text());
        assert!(o.get("stale_young_starts") >= 1);
        assert_eq!(o.get("young_starts"), o.get("young_ends"));
        assert_eq!(o.get("young_epoch"), o.get("young_starts"));
        assert_eq!(o.get("mark_samples"), o.get("young_ends"));
        assert_eq!(o.get("young_active"), 0);
        assert_eq!(o.get("bank"), 1);
        for i in 0..n {
            assert_eq!(o.get(&format!("acks{i}")), o.get("completed"));
            assert_eq!(o.get(&format!("phase{i}")), o.get("phase"));
        }
    }
}

fn epoch_boundaries(g: &mut crate::NativeCodeGenerator, n: u64) {
    graph(g, n);
    let mut h = H::begin(g);
    let epoch = worker_epoch_sample(h.g);
    for (i, name) in [
        "lease_active",
        "lease_entered",
        "lease_completed",
        "lease_phase",
        "lease_bank",
        "lease_cache",
        "lease_current_bank",
    ]
    .into_iter()
    .enumerate()
    {
        h.report(name, |h| worker_epoch_metric(h.g, epoch, i));
    }
}
#[test]
fn production_worker_epoch_closes_each_quantum_across_one_two_and_eight_owner_phase_rounds() {
    for n in [1, 2, 8] {
        let o = run(epoch_boundaries, n, true);
        assert_eq!(o.get("lease_active"), 0, "{}", o.text());
        assert!(o.get("lease_entered") > 0);
        assert_eq!(o.get("lease_entered"), o.get("lease_completed"));
        assert_eq!(o.get("lease_phase"), o.get("lease_cache"));
        assert_eq!(o.get("lease_bank"), o.get("lease_current_bank"));
        assert!(o.get("young_starts") >= 2);
        assert_eq!(o.get("young_starts"), o.get("young_ends"));
        assert_eq!(o.get("young_active"), 0);
        assert_eq!(o.get("pending_sample"), 0);
        assert!(o.get("young_worker") > 0 && o.get("old_worker") > 0);
    }
}
