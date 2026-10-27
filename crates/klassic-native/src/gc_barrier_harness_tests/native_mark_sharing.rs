//! Excluded owners adopt a real Old mark before concurrent managed allocation.
use super::{H, Src, run, run_failure, slot_offset};
use crate::NativeCodeGenerator;
use crate::gc_abi::{Generation, Phase, PhaseSnapshot};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
const ITERATIONS: u64 = 1024;
fn concurrent(g: &mut NativeCodeGenerator, n: u64) {
    graph_body(g, n, false);
}
pub(super) fn closed(g: &mut NativeCodeGenerator, n: u64) {
    graph_body(g, n, true);
}
fn graph_body(g: &mut NativeCodeGenerator, n: u64, close: bool) {
    let mut h = H::begin(g);
    h.alloc(2, n * 8, 0);
    h.root(0);
    let mut old_records = Vec::new();
    // The hub is the sole strong path to these old parent/leaf pairs. N owners
    // overwrite its slots after snapshot capture; retained metadata observers
    // do not add roots, and this test never flips or retires their record bank.
    for i in 0..n {
        h.alloc(1, 16, 1);
        h.set_scalar(Src::Raw(1), 0, (i + 2) << 32);
        h.root(1);
        h.alloc(2, 16, 2);
        h.root(2);
        h.store(Src::Root(2), 0, Src::Root(1), true);
        h.store(Src::Root(2), 1, Src::Root(0), true);
        h.store(Src::Root(0), i as u32, Src::Root(2), true);
        h.value(Src::Root(1));
        old_records.push(remember_mark_record(h.g));
        h.value(Src::Root(2));
        old_records.push(remember_mark_record(h.g));
        h.pop_root();
        h.pop_root();
    }
    publish_primary(h.g);
    let actors: Vec<_> = (0..n).map(|_| create_actor(h.g, 16, 8)).collect();
    h.g.asm
        .data
        .resize(h.g.asm.data.len().next_multiple_of(8), 0);
    let master = h.g.asm.data_label_with_i64s(&[0]);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    h.g.mov_data_addr(R::V1, master);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    let p = if close {
        run_parallel_closed_old_mark_graph(h.g, &actors, ITERATIONS, master)
    } else {
        run_parallel_old_mark_graph(h.g, &actors, ITERATIONS, master)
    };
    h.report("worker_at_join", |h| old_mark_shared_metric(h.g, 1));
    if !close {
        wait_old_worker(h.g);
    }
    for (i, record) in old_records.into_iter().enumerate() {
        h.report(&format!("snapshot_epoch{i}"), |h| {
            remembered_old_epoch(h.g, record)
        });
    }
    h.report_counter("phase", Counter::AdoptedPhase);
    h.report_counter("active", Counter::MarkActive);
    h.report_counter("valid", Counter::MarkValid);
    h.report_counter("queue", Counter::MarkStackLen);
    h.report_counter("receiver", Counter::MarkObject);
    for (i, name) in [
        "obligations",
        "root_published",
        "root_acked",
        "fast_root_acks",
        "leases",
        "walkers",
        "native",
        "expected",
        "peer_acks",
    ]
    .iter()
    .enumerate()
    {
        h.report(name, |h| root_obligation_counter(h.g, i));
    }
    if close {
        h.report("natives_at_close", |h| {
            root_obligation_natives_at_close(h.g)
        });
    }
    h.report_counter("cycles", Counter::MarkCycles);
    h.report_counter("marked", Counter::Marked);
    h.report_counter("collections", Counter::Collections);
    h.report("black", |h| old_mark_shared_metric(h.g, 0));
    h.report("worker_after_gate", |h| old_mark_shared_metric(h.g, 1));
    h.report("bank", |h| old_mark_shared_metric(h.g, 2));
    h.report("ready", |h| parallel_actor_ready(h.g, &p));
    h.report("hub", |h| h.value(Src::Root(0)));
    for (i, actor) in actors.iter().enumerate() {
        if close {
            h.report(&format!("late_pending{i}"), |h| {
                closed_owner_metric(h.g, &p, i, false)
            });
            h.report(&format!("closed_checked{i}"), |h| {
                closed_owner_metric(h.g, &p, i, true)
            });
        }
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        for (name, metric) in [
            ("phase", ActorMetric::Phase),
            ("adoptions", ActorMetric::NativeAdoptions),
            ("entries", ActorMetric::NativeEntries),
            ("exits", ActorMetric::NativeExits),
            ("depth", ActorMetric::RootDepth),
            ("pending", ActorMetric::Pending),
            ("bytes", ActorMetric::Bytes),
        ] {
            h.report(&format!("{name}{i}"), |h| actor_metric(h.g, *actor, metric));
        }
        h.report(&format!("worker_seen{i}"), |h| {
            parallel_old_worker(h.g, &p, i)
        });
        h.report(&format!("reads{i}"), |h| parallel_graph_reads(h.g, &p, i));
        h.load_barriered(Src::Root(0), i as u32);
        h.keep(i as u32 + 1);
        h.report(&format!("parent_epoch{i}"), |h| {
            h.value(Src::Raw(i as u32 + 1));
            old_epoch_of(h.g);
        });
        h.report(&format!("leaf_epoch{i}"), |h| {
            h.load_barriered(Src::Raw(i as u32 + 1), 0);
            old_epoch_of(h.g);
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
fn production_native_owners_adopt_old_mark_then_share_graphs_with_a_real_worker_on_one_two_and_eight_threads()
 {
    for n in [1, 2, 8] {
        let o = run(concurrent, n, true);
        let phase = PhaseSnapshot::from_bits(o.get("phase") as u64).unwrap();
        assert_eq!(phase.phase(Generation::Young), Phase::Idle);
        assert_eq!(phase.phase(Generation::Old), Phase::ConcurrentMark);
        assert_eq!(o.get("cycles"), 1);
        assert_eq!(o.get("active"), 1);
        assert_eq!(o.get("collections"), 0);
        assert_eq!(o.get("bank"), 1);
        assert_eq!(
            o.get("black"),
            (n * ITERATIONS * 2 * 48) as i64,
            "{}",
            o.text()
        );
        assert!(o.get("marked") >= (1 + 2 * n) as i64);
        for i in 0..2 * n {
            assert_eq!(o.get(&format!("snapshot_epoch{i}")), 1, "{}", o.text());
        }
        assert!(o.get("worker_at_join") > 0);
        assert!(
            o.get("worker_after_gate") > 0,
            "no real worker work after all owners ready: {}",
            o.text()
        );
        assert_eq!(o.get("ready"), n as i64);
        let mut tids = std::collections::HashSet::new();
        let mut reads = 0;
        for i in 0..n {
            assert!(o.get(&format!("tid{i}")) > 0);
            assert!(tids.insert(o.get(&format!("tid{i}"))));
            assert_eq!(o.get(&format!("phase{i}")), o.get("phase"));
            for key in [
                "adoptions",
                "entries",
                "exits",
                "depth",
                "parent_epoch",
                "leaf_epoch",
            ] {
                assert_eq!(o.get(&format!("{key}{i}")), 1, "{}", o.text());
            }
            assert_eq!(o.get(&format!("pending{i}")), 0);
            assert_eq!(o.get(&format!("bytes{i}")), (ITERATIONS * 2 * 48) as i64);
            assert_eq!(
                o.get(&format!("leaf{i}")),
                (((i + 2) << 32) | (ITERATIONS - 1)) as i64
            );
            assert_eq!(o.get(&format!("back{i}")), o.get("hub"));
            assert!(o.get(&format!("worker_seen{i}")) > 0);
            reads += o.get(&format!("reads{i}"));
        }
        assert!(reads > 0);
    }
}
fn fault(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    let a = create_actor(h.g, 16, 8);
    actor_alloc(h.g, a, 16);
    h.g.mov_imm64(R::V1, 77);
    h.g.store_ptr_disp32(R::V0, 0, R::V1);
    adopt_actor_value(h.g, a);
    native_actor(h.g, a, true);
    h.mark_start();
    native_return_fault(h.g, a, mode);
}
#[test]
fn production_native_mark_return_rejects_bank_aba_cache_tampering_remap_young_phase_sequence_and_private_access()
 {
    for mode in 0..8 {
        let o = run_failure(fault, mode);
        assert!(!o.status.success(), "mode {mode} accepted");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant"),
            "mode {mode}: {o:?}"
        );
    }
}

fn foreign_fault(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    publish_primary(h.g);
    let a = create_actor(h.g, 16, 8);
    h.value(Src::Root(0));
    adopt_actor_value(h.g, a);
    foreign_native_journal_fault(h.g, a, mode);
}
#[test]
fn production_foreign_empty_journal_admission_rejects_running_pending_claimed_private_and_open_tlab_states()
 {
    for mode in 0..5 {
        let o = run_failure(foreign_fault, mode);
        assert!(!o.status.success(), "mode {mode} accepted");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant"),
            "mode {mode}: {o:?}"
        );
    }
}
