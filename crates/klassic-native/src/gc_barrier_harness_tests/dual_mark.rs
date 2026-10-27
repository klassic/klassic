//! Actual finite Young/Old rounds with independent owners and a real worker.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};

fn untraced(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    publish_primary(h.g);
    dual_mark_enable(h.g);
    let gate = dual_young_gate(h.g);
    dual_mark_entry(h.g, true, true);
    worker_journal_wait(h.g, 0, 1);
    h.report("pending_before", |h| root_obligation_counter(h.g, 0));
    h.report_counter("queue_before", Counter::YoungStackLen);
    dual_mark_entry(h.g, true, false);
    h.report("active_retry", |h| dual_mark_metric(h.g, 4));
    h.report("valid_retry", |h| dual_mark_metric(h.g, 5));
    h.report("retries", |h| dual_mark_metric(h.g, 3));
    worker_journal_open_gate(h.g, gate);
    dual_mark_wait(h.g, None);
    dual_mark_entry(h.g, true, false);
    h.report("ends", |h| dual_mark_metric(h.g, 1));
    h.report("active_closed", |h| dual_mark_metric(h.g, 4));
}

#[test]
fn production_young_end_reopens_for_real_untraced_frontiers_even_after_all_journal_acknowledgments()
{
    let o = run(untraced, 0, true);
    assert_eq!(o.get("pending_before"), 0, "{}", o.text());
    assert!(o.get("queue_before") > 0);
    assert_eq!(o.get("active_retry"), 1);
    assert_eq!(o.get("valid_retry"), 0);
    assert_eq!(o.get("retries"), 1);
    assert_eq!(o.get("ends"), 1);
    assert_eq!(o.get("active_closed"), 0);
}

fn invalid_lifetime(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let h = H::begin(g);
    if mode < 2 {
        cooperative_enable(h.g);
        dual_mark_entry(h.g, true, mode == 0);
    } else {
        dual_mark_enable(h.g);
        dual_mark_entry(h.g, true, true);
        captured_journal_fault(h.g, if mode == 2 { 8 } else { 9 });
        captured_journal_check_lifetime(h.g);
    }
}

#[test]
fn production_young_rounds_require_dual_enrollment_and_fixed_actual_bank_and_remap_lifetime() {
    for mode in 0..4 {
        let o = super::execute(invalid_lifetime, mode, true, false);
        assert_eq!(o.status.code(), Some(1), "mode {mode}: {o:?}");
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
    }
}

fn observed(g: &mut crate::NativeCodeGenerator, future: u64) {
    let mut h = H::begin(g);
    dual_mark_enable(h.g);
    dual_mark_observed(h.g, true, 0);
    dual_mark_observed(h.g, true, 1); // current duplicate, not a new cycle
    dual_mark_wait(h.g, None);
    dual_mark_observed(h.g, false, 1);
    dual_mark_observed(h.g, true, 0); // actual old token
    dual_mark_observed(h.g, false, 0);
    if future != 0 {
        dual_mark_observed(h.g, true, 2);
    }
    h.report("starts", |h| dual_mark_metric(h.g, 0));
    h.report("ends", |h| dual_mark_metric(h.g, 1));
    h.report("stale_starts", |h| dual_mark_metric(h.g, 13));
    h.report("stale_ends", |h| dual_mark_metric(h.g, 14));
}

#[test]
fn production_young_observed_requests_use_the_actual_young_epoch_and_discard_stale_tokens() {
    let o = run(observed, 0, true);
    assert_eq!(o.get("starts"), 1, "{}", o.text());
    assert_eq!(o.get("ends"), 1);
    assert_eq!(o.get("stale_starts"), 1);
    assert_eq!(o.get("stale_ends"), 1);
    let bad = super::execute(observed, 1, true, false);
    assert_eq!(bad.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&bad.stderr).contains("runtime invariant violated"));
}

fn retained(g: &mut crate::NativeCodeGenerator, n: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 91);
    h.alloc(2, 8, 0);
    h.store(Src::Raw(0), 0, Src::Raw(2), true);
    h.alloc(1, 16, 1);
    publish_primary(h.g);
    let regions: Vec<_> = (0..n).map(|_| journal_region_new(h.g, 65)).collect();
    dual_mark_enable(h.g);
    let gate = captured_journal_gate(h.g);
    for cycle in 0..4 {
        dual_gate_close(h.g, gate);
        for region in &regions {
            h.repeat(32, |h| {
                h.value(Src::Raw(0));
                journal_region_store(h.g, *region, false);
                h.value(Src::Null);
                journal_region_store(h.g, *region, false);
            });
        }
        h.report(&format!("owner_before{cycle}"), |h| {
            root_journal_entries(h.g)
        });
        dual_mark_entry(h.g, true, true);
        dual_mark_wait(h.g, None);
        h.report(&format!("epoch_before{cycle}"), |h| {
            h.object_metadata(
                Src::Raw(0),
                std::mem::offset_of!(crate::gc_abi::MarkRecord, young_epoch),
            )
        });
        dual_mark_entry(h.g, true, false);
        h.report(&format!("active_retry{cycle}"), |h| {
            dual_mark_metric(h.g, 4)
        });
        h.report(&format!("owner_retry{cycle}"), |h| {
            root_journal_entries(h.g)
        });
        h.report(&format!("pending_retry{cycle}"), |h| {
            root_obligation_counter(h.g, 0)
        });
        captured_journal_release(h.g, gate);
        dual_mark_wait(h.g, Some(n * 64 * (cycle + 1) + 1));
        dual_mark_wait(h.g, None);
        dual_mark_entry(h.g, true, false);
        h.report(&format!("parent_epoch{cycle}"), |h| {
            h.object_metadata(
                Src::Raw(0),
                std::mem::offset_of!(crate::gc_abi::MarkRecord, young_epoch),
            )
        });
        h.report(&format!("child_epoch{cycle}"), |h| {
            h.object_metadata(
                Src::Raw(2),
                std::mem::offset_of!(crate::gc_abi::MarkRecord, young_epoch),
            )
        });
        h.report(&format!("pending_closed{cycle}"), |h| {
            root_obligation_counter(h.g, 0)
        });
    }
    h.report("starts", |h| dual_mark_metric(h.g, 0));
    h.report("ends", |h| dual_mark_metric(h.g, 1));
    h.report("retries", |h| dual_mark_metric(h.g, 3));
    h.report("bank", |h| dual_mark_metric(h.g, 12));
}

#[test]
fn production_retained_journals_admit_young_graphs_across_real_epoch_wraps_before_ack_and_select() {
    for n in [1, 32] {
        let o = run(retained, n, true);
        for c in 0..4 {
            assert_eq!(o.get(&format!("epoch_before{c}")), c, "{}", o.text());
            assert_eq!(o.get(&format!("active_retry{c}")), 1);
            assert_eq!(
                o.get(&format!("owner_before{c}")),
                o.get(&format!("owner_retry{c}"))
            );
            assert!(o.get(&format!("pending_retry{c}")) >= n as i64 * 64);
            assert_eq!(o.get(&format!("parent_epoch{c}")), c + 1);
            assert_eq!(o.get(&format!("child_epoch{c}")), c + 1);
            assert_eq!(o.get(&format!("pending_closed{c}")), 0);
        }
        assert_eq!(o.get("starts"), 4);
        assert_eq!(o.get("ends"), 4);
        assert_eq!(o.get("retries"), 4);
        assert_eq!(o.get("bank"), 1);
    }
}

fn invalid_native(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let h = H::begin(g);
    let actor = create_actor(h.g, 8, 8);
    native_actor(h.g, actor, true);
    dual_mark_enable(h.g);
    dual_mark_entry(h.g, true, true);
    if mode == 14 {
        dual_mark_wait(h.g, None);
        dual_mark_entry(h.g, true, false);
        dual_mark_entry(h.g, true, true);
    }
    if mode == 10 {
        dual_mark_wait(h.g, None);
        dual_mark_entry(h.g, true, false);
    }
    dual_native_fault(h.g, actor, mode);
}

#[test]
fn production_dual_native_adoption_rejects_real_engine_bank_remap_cache_epoch_and_scope_mismatches()
{
    for mode in 0..15 {
        let o = super::execute(invalid_native, mode, true, false);
        assert_eq!(o.status.code(), Some(1), "mode {mode}: {o:?}");
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
    }
}

fn rounds(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    publish_primary(h.g);
    dual_mark_enable(h.g);
    let gate = worker_journal_gate(h.g);
    dual_mark_entry(h.g, false, true);
    dual_mark_entry(h.g, true, true);
    worker_journal_open_gate(h.g, gate);
    for _ in 0..2 {
        let again = h.g.create_text_label();
        h.g.bind_text_label(again);
        dual_mark_entry(h.g, true, false);
        dual_mark_entry(h.g, false, false);
        dual_mark_metric(h.g, 4);
        h.g.mov_reg_reg(R::V1, R::V0);
        dual_mark_metric(h.g, 9);
        h.g.or_reg_reg(R::V0, R::V1);
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(crate::portable_asm::Condition::NotEqual, again);
    }
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
}

#[test]
fn production_actual_young_and_old_rounds_close_independent_frontiers_without_moving_bank() {
    let o = run(rounds, 0, true);
    assert_eq!(o.get("young_starts"), 1, "{}", o.text());
    assert_eq!(o.get("young_ends"), 1);
    assert_eq!(o.get("checks"), o.get("retries") + 1);
    assert_eq!(o.get("young_valid"), 1);
    assert_eq!(o.get("old_valid"), 1);
    assert_eq!(o.get("young_active"), 0);
    assert_eq!(o.get("old_active"), 0);
    assert_eq!(o.get("bank"), 1);
    assert!(o.get("young_worker") > 0);
    assert!(o.get("old_worker") > 0);
}

fn parallel(g: &mut crate::NativeCodeGenerator, argument: u64) {
    let mixed = argument & 256 != 0;
    let n = argument & 255;
    let mut h = H::begin(g);
    h.alloc(2, n * 8, 0);
    h.root(0);
    publish_primary(h.g);
    if mixed {
        // Real copying and age-based promotion before cooperative enrollment.
        set_young_auto(h.g, true);
        for _ in 0..2 {
            set_tenuring(h.g, 2);
            h.g.call_label(h.l.young_start);
            h.collect();
            h.finish_collection();
        }
        h.report("hub_old", |h| h.is_old(Src::Root(0)));
        h.report("bank_before", |h| dual_mark_metric(h.g, 12));
        h.report("young_epoch_before", |h| dual_mark_metric(h.g, 6));
        h.report("generation_before", |h| dual_mark_metric(h.g, 15));
    }
    let actors: Vec<_> = (0..n).map(|_| create_actor(h.g, 16, 8)).collect();
    h.g.asm
        .data
        .resize(h.g.asm.data.len().next_multiple_of(8), 0);
    let master = h.g.asm.data_label_with_i64s(&[0]);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    h.g.mov_data_addr(R::V1, master);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    let gate = worker_journal_gate(h.g);
    let p = run_parallel_dual_mark_graph(h.g, &actors, 1024, master, gate);
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
    h.report("phase", |h| h.counter(Counter::AdoptedPhase));
    h.report("hub", |h| h.value(Src::Root(0)));
    if mixed {
        h.report("old_hub_epoch", |h| {
            h.object_metadata(
                Src::Root(0),
                std::mem::offset_of!(crate::gc_abi::MarkRecord, old_epoch),
            )
        });
        h.report_counter("old_cycles", Counter::MarkCycles);
        h.report_counter("remset", Counter::RemsetLen);
    }

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
        h.report(&format!("id{i}"), |h| h.object_id(Src::Raw(i as u32 + 1)));
        if mixed {
            h.report(&format!("parent_epoch{i}"), |h| {
                h.object_metadata(
                    Src::Raw(i as u32 + 1),
                    std::mem::offset_of!(crate::gc_abi::MarkRecord, young_epoch),
                )
            });
        }
    }
    h.report("pending", |h| root_obligation_counter(h.g, 0));
    h.report("members", |h| checkpoint_metric(h.g, 1));
    h.report("completed", |h| checkpoint_metric(h.g, 0));
}

#[test]
fn production_one_two_and_eight_running_owners_mark_both_generations_on_one_shared_graph() {
    for n in [1, 2, 8] {
        let o = run(parallel, n, true);
        assert_eq!(o.get("young_starts"), 1, "{}", o.text());
        assert_eq!(o.get("young_ends"), 1);
        assert_eq!(o.get("checks"), o.get("retries") + 1);
        assert_eq!(o.get("young_valid"), 1);
        assert_eq!(o.get("old_valid"), 1);
        assert_eq!(o.get("young_active"), 0);
        assert_eq!(o.get("old_active"), 0);
        assert_eq!(o.get("bank"), 1);
        assert_eq!(o.get("members"), n as i64);
        assert!(o.get("young_worker") > 0);
        assert!(o.get("old_worker") > 0);
        let mut tids = std::collections::HashSet::new();
        let mut ids = std::collections::HashSet::new();
        for i in 0..n {
            assert!(tids.insert(o.get(&format!("tid{i}"))));
            assert_eq!(o.get(&format!("phase{i}")), o.get("phase"));
            assert!(o.get(&format!("acks{i}")) >= 4);
            assert_eq!(o.get(&format!("acks{i}")), o.get("completed"));
            assert!(o.get(&format!("reads{i}")) > 0);
            assert_eq!(o.get(&format!("leaf{i}")), (((i + 2) << 32) | 1023) as i64);
            assert_eq!(o.get(&format!("back{i}")), o.get("hub"));
            assert!(ids.insert(o.get(&format!("id{i}"))));
        }
    }
}

#[test]
fn production_real_old_hub_and_young_cycles_survive_dual_marking_on_one_two_and_eight_owners() {
    for n in [1, 2, 8] {
        let o = run(parallel, n | 256, true);
        assert_eq!(o.get("hub_old"), 1, "{}", o.text());
        assert_eq!(o.get("young_starts"), 1);
        assert_eq!(o.get("young_ends"), 1);
        assert_eq!(o.get("bank"), o.get("bank_before"));
        assert!(o.get("bank_before") > 1);
        assert_eq!(o.get("old_hub_epoch"), o.get("old_cycles"));
        assert!(o.get("old_hub_epoch") > 0);
        assert!(o.get("young_epoch_before") >= 2);
        assert_eq!(o.get("young_epoch"), o.get("young_epoch_before") + 1);
        assert!(o.get("remset") >= n as i64);
        for i in 0..n {
            assert_eq!(o.get(&format!("leaf{i}")), (((i + 2) << 32) | 1023) as i64);
            assert_eq!(o.get(&format!("back{i}")), o.get("hub"));
            assert_eq!(
                o.get(&format!("parent_epoch{i}")),
                o.get("young_epoch"),
                "n={n}: {}",
                o.text()
            );
            assert_eq!(o.get(&format!("phase{i}")), o.get("phase"));
            assert_eq!(
                o.get(&format!("acks{i}")),
                o.get("completed") - o.get("generation_before")
            );
        }
        assert!(o.get("young_worker") > 0);
        assert!(o.get("old_worker") > 0);
    }
}
