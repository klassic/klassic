//! Independent worker transfers actual overwritten roots while owners run.
use super::{H, Src, run};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::PortableAsm;

fn graph(g: &mut crate::NativeCodeGenerator, n: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 91);
    h.alloc(2, 8, 0);
    h.store(Src::Raw(0), 0, Src::Raw(2), true);
    h.alloc(1, 16, 1);
    publish_primary(h.g);
    let regions: Vec<_> = (0..n).map(|_| journal_region_new(h.g, 7)).collect();
    for region in &regions {
        h.value(Src::Raw(0));
        journal_region_store(h.g, *region, false);
    }
    // Remove the private B scratch before the snapshot so next-side admission
    // below cannot accidentally be supplied by a retained B->0 scratch entry.
    root_journal_pressure_store(h.g, false);
    cooperative_enable(h.g);
    let gate = worker_journal_gate(h.g);
    observed_old_entry(h.g, true, 0);
    for region in &regions {
        h.value(Src::Raw(1));
        journal_region_store(h.g, *region, false);
    }
    worker_journal_wait(h.g, 0, n);
    h.report("previous_admitted", |h| h.marked(Src::Raw(0)));
    h.report("next_admitted", |h| h.marked(Src::Raw(1)));
    h.report("child_before_trace", |h| h.marked(Src::Raw(2)));
    worker_journal_open_gate(h.g, gate);
    worker_journal_wait(h.g, 4, 1);
    // Closing may drive rendezvous, but journal admission above was entirely
    // the independently executing worker, while this owner remained Running.
    observed_old_entry(h.g, false, 1);
    h.report("worker_entries", |h| worker_journal_counter(h.g, 0));
    h.report("worker_regions", |h| worker_journal_counter(h.g, 1));
    h.report("owner_entries", |h| root_journal_entries(h.g));
    h.report_counter("marked", Counter::Marked);
    h.report_counter("worker_units", Counter::MarkWorkerUnits);
    h.report("pending", |h| root_obligation_counter(h.g, 0));
    h.report("published", |h| root_obligation_counter(h.g, 1));
    h.report("acknowledged", |h| root_obligation_counter(h.g, 2));
    h.report("native_state", |h| native_context_state(h.g));
    h.report("value", |h| h.load(Src::Raw(2), 0));
}

#[test]
fn production_worker_transfers_overwritten_graphs_from_non_power_of_two_journals_without_owner_sealing()
 {
    for n in [1, 32] {
        let o = run(graph, n, true);
        assert_eq!(o.get("worker_entries"), n as i64, "{}", o.text());
        assert_eq!(o.get("previous_admitted"), 1);
        assert_eq!(o.get("next_admitted"), 1);
        assert_eq!(o.get("child_before_trace"), 0);
        assert!(o.get("worker_regions") >= n as i64);
        assert_eq!(o.get("marked"), 3);
        assert!(o.get("worker_units") > 0);
        assert_eq!(o.get("pending"), 0);
        assert_eq!(o.get("published"), o.get("acknowledged"));
        assert_eq!(o.get("native_state"), 0);
        assert_eq!(o.get("value"), 91);
    }
}

fn initialization(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    cooperative_enable(h.g);
    let gate = worker_journal_gate(h.g);
    observed_old_entry(h.g, true, 0);
    worker_journal_before_black(h.g, Some(gate));
    h.alloc(2, 8, 0);
    h.set_scalar(Src::Raw(0), 0, 0);
    h.report("deferred", |h| worker_journal_counter(h.g, 2));
    h.report("initializing", |h| root_journal_pending_state(h.g));
    h.report("worker_before_publish", |h| worker_journal_counter(h.g, 0));
    publish_primary(h.g);
    worker_journal_wait(h.g, 0, 2); // scratch-clear and actual allocation
    h.report("worker_after_publish", |h| worker_journal_counter(h.g, 0));
    h.report("pending", |h| root_obligation_counter(h.g, 0));
}
#[test]
fn production_worker_defers_actual_initializing_journal_before_pending_token_and_black_stamp_exist()
{
    let o = run(initialization, 0, true);
    assert!(o.get("deferred") > 0, "{}", o.text());
    assert_eq!(o.get("initializing"), 1);
    // The empty scratch-clear can already be consumed, the new object cannot.
    assert_eq!(o.get("worker_before_publish"), 1);
    assert_eq!(o.get("worker_after_publish"), 2);
    assert_eq!(o.get("pending"), 0);
}

fn foreign_initialization(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let actor = create_actor(h.g, 8, 8);
    native_actor(h.g, actor, true);
    cooperative_enable(h.g);
    let gate = worker_journal_gate(h.g);
    observed_old_entry(h.g, true, 0);
    // The primary enters Native; a different actual context adopts Mark.
    let enter = h.g.gc_gen.as_ref().unwrap().labels.native_enter;
    h.g.call_label(enter);
    native_actor(h.g, actor, false);
    worker_journal_before_black(h.g, Some(gate));
    actor_alloc(h.g, actor, 16);
    actor_publish(h.g, actor);
    worker_journal_wait(h.g, 0, 2);
    h.report("worker_entries", |h| worker_journal_counter(h.g, 0));
    h.report("deferred", |h| worker_journal_counter(h.g, 2));
    h.report("mark_done", |h| worker_journal_counter(h.g, 4));
}
#[test]
fn production_worker_excludes_each_actual_owner_scratch_from_snapshot_discovery_during_initialization()
 {
    let o = run(foreign_initialization, 0, true);
    assert_eq!(o.get("worker_entries"), 2, "{}", o.text());
    assert!(o.get("deferred") > 0);
    assert_eq!(o.get("mark_done"), 1);
}

fn held_claim(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    publish_primary(h.g);
    let held = journal_region_new(h.g, 3);
    let free = journal_region_new(h.g, 5);
    cooperative_enable(h.g);
    h.report("fast_path", |h| worker_journal_fast_probe(h.g));
    h.g.call_label(h.l.lock);
    h.value(Src::Raw(0));
    journal_region_store(h.g, held, false);
    let request = worker_journal_hold(h.g, held);
    h.value(Src::Raw(0));
    journal_region_store(h.g, free, false);
    h.g.call_label(h.l.unlock);
    worker_journal_wait(h.g, 0, 1);
    worker_journal_wait(h.g, 3, 1);
    h.report("held_consumed", |h| journal_region_cursor(h.g, held, true));
    h.report("free_consumed", |h| journal_region_cursor(h.g, free, true));
    h.report("pending_before_ack", |h| root_obligation_counter(h.g, 0));
    worker_journal_finish_hold(h.g, request);
    h.report("held_after_ack", |h| journal_region_cursor(h.g, held, true));
    h.report("pending_after_ack", |h| root_obligation_counter(h.g, 0));
    h.report("worker_entries", |h| worker_journal_counter(h.g, 0));
}
#[test]
fn production_worker_skips_real_busy_claims_and_owner_fast_ack_is_disabled_after_enrollment() {
    let o = run(held_claim, 0, true);
    assert_eq!(o.get("fast_path"), 0, "{}", o.text());
    assert_eq!(o.get("held_consumed"), 0);
    assert_eq!(o.get("free_consumed"), 1);
    assert_eq!(o.get("pending_before_ack"), 1);
    assert_eq!(o.get("held_after_ack"), 1);
    assert_eq!(o.get("pending_after_ack"), 0);
    assert_eq!(o.get("worker_entries"), 1);
}

fn invalid_record(g: &mut crate::NativeCodeGenerator, mode: u64) {
    use crate::portable_asm::Reg as R;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    publish_primary(h.g);
    cooperative_enable(h.g);
    h.g.call_label(h.l.lock);
    h.value(Src::Raw(0));
    h.g.mov_reg_reg(R::V1, R::V0);
    root_journal_publish_pair(h.g, 1);
    root_journal_corrupt_entry(h.g, mode == 1);
    h.g.call_label(h.l.unlock);
    worker_journal_wait(h.g, 0, 1);
}
#[test]
fn production_worker_rejects_real_published_entries_with_a_stale_phase_or_wrong_scope() {
    for mode in [0, 1] {
        let output = super::execute(invalid_record, mode, true, false);
        assert_eq!(output.status.code(), Some(1));
        assert!(String::from_utf8_lossy(&output.stderr).contains("runtime invariant violated"));
    }
}
