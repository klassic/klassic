//! Actual per-owner root/field calls publishing a shared managed cycle.
use super::{H, Src, run, slot_offset};
use crate::NativeCodeGenerator;
use crate::gc_abi::OBJECT_PUBLISHED;
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
const ITERATIONS: u64 = 1024;
fn graph(g: &mut NativeCodeGenerator, n: u64) {
    graph_body(g, n, false)
}
fn checkpoint_graph(g: &mut NativeCodeGenerator, n: u64) {
    graph_body(g, n, true)
}
fn graph_body(g: &mut NativeCodeGenerator, n: u64, checkpoint: bool) {
    graph_mode_body(g, n, checkpoint, false, false)
}
pub(super) fn retry_running_old_graph(g: &mut NativeCodeGenerator, n: u64) {
    force_first_old_end_retry(g);
    graph_mode_body(g, n, false, true, false)
}
pub(super) fn delayed_running_old_graph(g: &mut NativeCodeGenerator, n: u64) {
    delay_phase_resume(g, n);
    graph_mode_body(g, n, false, true, false)
}
pub(super) fn running_old_graph(g: &mut NativeCodeGenerator, n: u64) {
    graph_mode_body(g, n, false, true, false)
}
pub(super) fn delayed_scheduled_old_graph(g: &mut NativeCodeGenerator, n: u64) {
    let state = delay_scheduler_request(g);
    scheduled_old_graph(g, n);
    let mut h = H::begin(g);
    h.report("delayed_observed_cycle", |h| {
        scheduler_delay_metric(h.g, state, true)
    });
    h.report("delayed_requested_cycle", |h| {
        scheduler_delay_metric(h.g, state, false)
    });
}
pub(super) fn scheduled_old_graph(g: &mut NativeCodeGenerator, n: u64) {
    graph_mode_body(g, n, false, false, true)
}
fn graph_mode_body(
    g: &mut NativeCodeGenerator,
    n: u64,
    checkpoint: bool,
    running_old: bool,
    scheduled_old: bool,
) {
    let iterations = if scheduled_old { 32768 / n } else { ITERATIONS };
    let mut h = H::begin(g);
    h.alloc(2, n * 8, 0);
    h.root(0);
    publish_primary(h.g);
    let actors: Vec<_> = (0..n).map(|_| create_actor(h.g, 16, 8)).collect();
    h.g.asm
        .data
        .resize(h.g.asm.data.len().next_multiple_of(8), 0);
    let master = h.g.asm.data_label_with_i64s(&[0]);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    h.g.mov_data_addr(R::V1, master);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    let p = if scheduled_old {
        run_parallel_scheduled_old_graph(h.g, &actors, iterations, master)
    } else if running_old {
        run_parallel_running_old_graph(h.g, &actors, ITERATIONS, master)
    } else if checkpoint {
        run_parallel_checkpoint_graph(h.g, &actors, ITERATIONS, master)
    } else {
        run_parallel_actor_graph(h.g, &actors, ITERATIONS, master)
    };
    if running_old || scheduled_old {
        for (i, name) in [
            "round_kind",
            "resuming",
            "starts",
            "ends",
            "closed_pub",
            "closed_ack",
            "closed_worker",
            "end_checks",
            "end_retries",
            "bank",
        ]
        .iter()
        .enumerate()
        {
            h.report(name, |h| phase_round_metric(h.g, i));
        }
        for (i, name) in [
            "completed",
            "expected",
            "acks",
            "active",
            "native",
            "primary_acks",
            "native_entries",
            "native_exits",
        ]
        .iter()
        .enumerate()
        {
            h.report(name, |h| checkpoint_metric(h.g, i as u64));
        }
        h.report("delayed_phase", |h| {
            delayed_phase_resume_metric(h.g, &p, false)
        });
        h.report("resume_waits", |h| {
            delayed_phase_resume_metric(h.g, &p, true)
        });
        for (i, name) in ["final_pending", "final_pub", "final_ack"]
            .iter()
            .enumerate()
        {
            h.report(name, |h| root_obligation_counter(h.g, i));
        }
        h.report_counter("valid", Counter::MarkValid);
        h.report_counter("mark_active", Counter::MarkActive);
        h.report_counter("coordinator_slices", Counter::MarkDrainSlices);
    }
    if scheduled_old {
        h.report("worker_journal_entries", |h| worker_journal_counter(h.g, 0));
        for (i, name) in [
            "enabled",
            "allocation_base",
            "auto_starts",
            "auto_ends",
            "scheduler_polls",
            "stale_starts",
            "stale_ends",
            "cycles",
        ]
        .iter()
        .enumerate()
        {
            h.report(name, |h| scheduled_old_metric(h.g, i));
        }
    }
    if checkpoint {
        for (i, name) in [
            "completed",
            "expected",
            "acks",
            "active",
            "native",
            "primary_acks",
            "native_entries",
            "native_exits",
        ]
        .iter()
        .enumerate()
        {
            h.report(name, |h| checkpoint_metric(h.g, i as u64));
        }
    }
    if checkpoint {
        for (i, name) in ["retries", "before_last_acks", "before_last_complete"]
            .iter()
            .enumerate()
        {
            h.report(name, |h| delayed_checkpoint_metric(h.g, &p, i as u64));
        }
    }
    h.report("primary_guard", |h| primary_copy_guard(h.g, true));
    h.report("primary_depth", |h| primary_root_depth(h.g));
    h.report("hub", |h| h.value(Src::Root(0)));
    h.report("heap", |h| actor_shared_heap(h.g));
    h.report("ready", |h| parallel_actor_ready(h.g, &p));
    h.report_counter("phase", Counter::AdoptedPhase);
    h.report_counter("collections", Counter::Collections);
    for (i, name) in ["records", "ids", "published"].iter().enumerate() {
        h.report(name, |h| shared_allocation_value(h.g, i as u64));
    }
    for (i, actor) in actors.into_iter().enumerate() {
        h.report(&format!("reads{i}"), |h| parallel_graph_reads(h.g, &p, i));
        if running_old || scheduled_old {
            h.report(&format!("coordinations{i}"), |h| {
                phase_owner_coordinations(h.g, actor)
            });
            h.report(&format!("adoptions{i}"), |h| {
                phase_owner_adoptions(h.g, actor)
            });
            h.report(&format!("worker{i}"), |h| parallel_old_worker(h.g, &p, i));
        }
        h.report(&format!("acks{i}"), |h| {
            actor_metric(h.g, actor, ActorMetric::CheckpointAcks)
        });
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        for (name, m) in [
            ("bytes", ActorMetric::Bytes),
            ("depth", ActorMetric::RootDepth),
            ("guard", ActorMetric::CopyGuard),
            ("pending", ActorMetric::Pending),
            ("heap", ActorMetric::Heap),
            ("phase", ActorMetric::Phase),
            ("journal_before", ActorMetric::JournalBeforeAck),
            ("journal_after", ActorMetric::JournalAfterAck),
        ] {
            h.report(&format!("{name}{i}"), |h| actor_metric(h.g, actor, m));
        }
        h.load_barriered(Src::Root(0), i as u32);
        h.keep(i as u32 + 1);
        h.report(&format!("parent_state{i}"), |h| {
            h.value(Src::Raw(i as u32 + 1));
            actor_header_state(h.g);
        });
        h.report(&format!("leaf_state{i}"), |h| {
            h.load_barriered(Src::Raw(i as u32 + 1), 0);
            actor_header_state(h.g);
        });
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
fn production_owner_roots_and_fields_share_initialized_cyclic_graphs_on_real_threads_while_idle() {
    for n in [2, 8] {
        for worker in [false, true] {
            let o = run(graph, n, worker);
            assert_eq!(o.get("primary_guard"), 77, "{}", o.text());
            assert_eq!(o.get("primary_depth"), 1);
            assert_eq!(o.get("ready"), n as i64);
            assert_eq!(o.get("phase"), 0);
            assert_eq!(o.get("collections"), 0);
            for key in ["records", "ids", "published"] {
                assert_eq!(o.get(key), (1 + 2 * n * ITERATIONS) as i64, "{}", o.text());
            }
            let mut tids = std::collections::HashSet::new();
            let mut reads = 0;
            for i in 0..n {
                assert!(o.get(&format!("tid{i}")) > 0);
                assert!(tids.insert(o.get(&format!("tid{i}"))));
                reads += o.get(&format!("reads{i}"));
                assert_eq!(o.get(&format!("bytes{i}")), (2 * ITERATIONS * 48) as i64);
                assert_eq!(o.get(&format!("depth{i}")), 1);
                assert_eq!(o.get(&format!("guard{i}")), 0);
                assert_eq!(o.get(&format!("pending{i}")), 0);
                assert_eq!(o.get(&format!("heap{i}")), o.get("heap"));
                assert_eq!(o.get(&format!("phase{i}")), 0);
                assert_eq!(o.get(&format!("parent_state{i}")), OBJECT_PUBLISHED as i64);
                assert_eq!(o.get(&format!("leaf_state{i}")), OBJECT_PUBLISHED as i64);
                assert_eq!(
                    o.get(&format!("leaf{i}")),
                    (((i + 2) << 32) | (ITERATIONS - 1)) as i64
                );
                assert_eq!(o.get(&format!("back{i}")), o.get("hub"));
            }
            assert!(reads > 0, "no actual cross-owner graph reads: {}", o.text());
        }
    }
}

#[test]
fn production_real_owner_checkpoints_publish_close_ack_and_resume_without_changing_phase() {
    for n in [2, 8] {
        for worker in [false, true] {
            let o = run(checkpoint_graph, n, worker);
            assert_eq!(o.get("completed"), 1, "{}", o.text());
            assert!(o.get("retries") > 0);
            assert_eq!(o.get("before_last_acks"), n as i64 - 1);
            assert_eq!(o.get("before_last_complete"), 0);
            assert_eq!(o.get("expected"), n as i64);
            assert_eq!(o.get("acks"), n as i64);
            assert_eq!(o.get("active"), 0);
            assert_eq!(o.get("native"), 0);
            assert_eq!(o.get("primary_acks"), 0);
            assert_eq!(o.get("native_entries"), 1);
            assert_eq!(o.get("native_exits"), 1);
            assert_eq!(o.get("primary_guard"), 0);
            assert_eq!(o.get("primary_depth"), 1);
            assert_eq!(o.get("phase"), 0);
            assert_eq!(o.get("collections"), 0);
            for key in ["records", "ids", "published"] {
                assert_eq!(o.get(key), (1 + 2 * n * ITERATIONS) as i64, "{}", o.text());
            }
            let mut reads = 0;
            for i in 0..n {
                assert_eq!(o.get(&format!("acks{i}")), 1, "{}", o.text());
                assert_eq!(
                    o.get(&format!("journal_before{i}")),
                    o.get(&format!("journal_after{i}"))
                );
                assert_eq!(o.get(&format!("depth{i}")), 1);
                assert_eq!(o.get(&format!("guard{i}")), 0);
                assert_eq!(o.get(&format!("pending{i}")), 0);
                assert_eq!(o.get(&format!("phase{i}")), 0);
                assert_eq!(
                    o.get(&format!("leaf{i}")),
                    (((i + 2) << 32) | (ITERATIONS - 1)) as i64
                );
                assert_eq!(o.get(&format!("back{i}")), o.get("hub"));
                reads += o.get(&format!("reads{i}"));
            }
            assert!(reads > 0);
            assert_eq!(o.get(&format!("journal_before{}", n - 1)), 1);
            assert_eq!(o.get(&format!("journal_after{}", n - 1)), 1);
        }
    }
}

fn invalid(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    publish_primary(h.g);
    let a = create_actor(h.g, 16, 8);
    let b = create_actor(h.g, 16, 8);
    h.value(Src::Root(0));
    let own = adopt_actor_value(h.g, a);
    h.value(Src::Root(0));
    let foreign = adopt_actor_value(h.g, b);
    actor_boundary_fault(h.g, a, own, foreign, mode);
}
#[test]
fn production_owner_boundaries_reject_foreign_roots_copied_unregistered_and_stale_contexts() {
    for mode in 0..6 {
        let o = super::run_failure(invalid, mode);
        assert!(!o.status.success(), "mode {mode} unexpectedly accepted");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant"),
            "mode {mode}: {o:?}"
        );
    }
}

fn checkpoint_fault(g: &mut NativeCodeGenerator, mode: u64) {
    let h = H::begin(g);
    invalid_checkpoint(h.g, mode);
}
#[test]
fn production_checkpoints_reject_live_private_access_copying_and_active_phase() {
    for mode in 0..4 {
        let o = super::run_failure(checkpoint_fault, mode);
        assert!(!o.status.success(), "mode {mode} unexpectedly accepted");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant"),
            "{o:?}"
        );
    }
}
