//! Actual journal credit, actual ACK and producer-excluded Old mark closure.
use super::{H, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_abi::{Generation, Phase, PhaseSnapshot};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg};
fn report(h: &mut H<'_>, prefix: &str) {
    for (i, name) in ["pending", "published", "acked", "fast"].iter().enumerate() {
        h.report(&format!("{prefix}_{name}"), |h| {
            root_obligation_counter(h.g, i)
        });
    }
}
fn direct(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    report(&mut h, "zero");
    h.alloc(1, 16, 0); // scratch clear + alloc_into's own direct publication
    report(&mut h, "allocated");
    h.root(0);
    report(&mut h, "rooted");
    h.g.mov_imm64(Reg::V0, 0);
    h.set_root(0);
    h.pop_root(); // clearing null still publishes a real RootChange
    report(&mut h, "cleared");
    root_journal_native_boundary(h.g);
    report(&mut h, "sealed");
    require_no_root_obligations(h.g);
}
#[test]
fn production_root_ledger_covers_direct_allocation_publication_null_mutations_and_real_ack() {
    let o = run(direct, 0, false);
    for key in ["pending", "published", "acked"] {
        assert_eq!(o.get(&format!("zero_{key}")), 0);
    }
    for (stage, count) in [("allocated", 2), ("rooted", 3), ("cleared", 5)] {
        assert_eq!(o.get(&format!("{stage}_pending")), count, "{}", o.text());
        assert_eq!(o.get(&format!("{stage}_published")), count);
        assert_eq!(o.get(&format!("{stage}_acked")), 0);
    }
    assert_eq!(o.get("sealed_pending"), 0);
    assert_eq!(o.get("sealed_published"), 5);
    assert_eq!(o.get("sealed_acked"), 5);
}
fn pressure(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    h.repeat(5000, |h| {
        h.g.mov_imm64(Reg::V0, 0);
        h.set_root(0);
    });
    report(&mut h, "pressure");
    root_journal_native_boundary(h.g);
    report(&mut h, "sealed");
    require_no_root_obligations(h.g);
}
#[test]
fn production_root_ledger_counts_fast_pressure_ack_and_counted_boundary_ack_without_double_retirement()
 {
    let o = run(pressure, 0, false);
    assert_eq!(o.get("pressure_published"), 5003);
    assert_eq!(o.get("pressure_pending"), 4096);
    assert_eq!(o.get("pressure_acked"), 907);
    assert_eq!(o.get("pressure_fast"), 907);
    assert_eq!(o.get("sealed_pending"), 0);
    assert_eq!(o.get("sealed_published"), 5003);
    assert_eq!(o.get("sealed_acked"), 5003);
}
fn fault(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    if mode == 5 {
        root_journal_native_boundary(h.g);
        root_obligation_fault(h.g, 4);
        h.mark_start();
        return;
    }
    if mode == 4 {
        require_no_root_obligations(h.g);
        return;
    }
    root_obligation_fault(h.g, mode);
    if mode < 2 {
        h.g.mov_imm64(Reg::V0, 0);
        h.set_root(0);
    } else {
        root_journal_native_boundary(h.g);
    }
}
#[test]
fn production_root_ledger_rejects_overflow_underflow_and_a_live_obligation_at_end_check() {
    for mode in 0..6 {
        let o = run_failure(fault, mode);
        assert!(!o.status.success(), "mode {mode} accepted");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant"),
            "mode {mode}: {o:?}"
        );
    }
}
#[test]
fn production_one_two_and_eight_native_owners_close_old_mark_with_all_actual_root_obligations_acked_and_adopt_select()
 {
    for n in [1, 2, 8] {
        let o = run(super::native_mark_sharing::closed, n, true);
        let p = PhaseSnapshot::from_bits(o.get("phase") as u64).unwrap();
        assert_eq!(p.phase(Generation::Young), Phase::Idle);
        assert_eq!(p.phase(Generation::Old), Phase::Select);
        assert_eq!(o.get("active"), 0);
        assert_eq!(o.get("valid"), 1);
        for name in [
            "obligations",
            "queue",
            "receiver",
            "leases",
            "walkers",
            "native",
            "collections",
        ] {
            assert_eq!(o.get(name), 0, "{name}: {}", o.text());
        }
        assert_eq!(o.get("bank"), 1);
        assert_eq!(o.get("cycles"), 1);
        assert_eq!(o.get("ready"), n as i64);
        assert_eq!(o.get("natives_at_close"), n as i64);
        assert_eq!(o.get("expected"), 0);
        assert_eq!(o.get("peer_acks"), 0); // completed round resets its counters
        // Each producer's final Native entry clears its last allocation.
        // Already-empty scratch cells do not create 0 -> 0 journal work.
        assert_eq!(
            o.get("root_published"),
            4 + 12298 * n as i64,
            "{}",
            o.text()
        );
        assert_eq!(o.get("root_acked"), o.get("root_published"));
        assert_eq!(o.get("black"), (n * 98304) as i64);
        assert!(o.get("worker_at_join") > 0);
        assert!(o.get(&format!("late_pending{}", n - 1)) > 0);
        for i in 0..2 * n {
            assert_eq!(o.get(&format!("snapshot_epoch{i}")), 1);
        }
        let mut tids = std::collections::HashSet::new();
        for i in 0..n {
            let tid = o.get(&format!("tid{i}"));
            assert!(tid > 0 && tids.insert(tid));
            assert!(o.get(&format!("worker_seen{i}")) > 0);
            assert_eq!(o.get(&format!("depth{i}")), 1);
            assert_eq!(o.get(&format!("bytes{i}")), 98304);
            assert_eq!(o.get(&format!("pending{i}")), 0);
            assert_eq!(o.get(&format!("phase{i}")), o.get("phase"));
            for name in ["adoptions", "entries", "exits"] {
                assert_eq!(o.get(&format!("{name}{i}")), 2, "{}", o.text());
            }
            assert_eq!(o.get(&format!("closed_checked{i}")), 1);
            assert_eq!(o.get(&format!("leaf{i}")), (((i + 2) << 32) | 1023) as i64);
            assert_eq!(o.get(&format!("back{i}")), o.get("hub"));
            assert_eq!(o.get(&format!("parent_epoch{i}")), 1);
            assert_eq!(o.get(&format!("leaf_epoch{i}")), 1);
        }
    }
}

fn batch(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let region = journal_region_new(h.g, 4);
    for _ in 0..4 {
        h.g.mov_imm64(Reg::V0, 0);
        journal_region_store(h.g, region, false);
    }
    report(&mut h, "before");
    root_obligation_batch_ack(h.g, region, 3);
    report(&mut h, "batch");
    h.report("consumed", |h| journal_region_cursor(h.g, region, true));
    root_journal_native_boundary(h.g);
    report(&mut h, "sealed");
}
#[test]
fn production_root_ledger_retires_the_actual_multi_record_claim_count() {
    let o = run(batch, 0, false);
    assert_eq!(o.get("before_pending"), 4);
    assert_eq!(o.get("before_published"), 4);
    assert_eq!(o.get("batch_pending"), 1);
    assert_eq!(o.get("batch_acked"), 3);
    assert_eq!(o.get("consumed"), 3);
    assert_eq!(o.get("sealed_pending"), 0);
    assert_eq!(o.get("sealed_published"), 4);
    assert_eq!(o.get("sealed_acked"), 4);
}
fn whole_scope(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(super::Src::Raw(0), 0, 4042);
    h.root(0);
    publish_primary(h.g);
    let actor = create_actor(h.g, 16, 8);
    h.value(super::Src::Root(0));
    let cell = adopt_actor_value(h.g, actor);
    native_actor(h.g, actor, true);
    h.mark_start();
    h.end_marks();
    if mode < 2 {
        root_obligation_invalidate_closed_mark(h.g, mode == 1);
    } else {
        h.g.mov_imm64(Reg::V0, 0);
        h.set_root(0);
    }
    native_actor(h.g, actor, false);
    h.report("value", |h| {
        root_obligation_actor_value(h.g, actor, cell);
        h.g.load_ptr_disp32(Reg::V0, Reg::V0, 0);
    });
    h.report_counter("phase", Counter::AdoptedPhase);
    h.report_counter("valid", Counter::MarkValid);
    h.report("actor_phase", |h| {
        actor_metric(h.g, actor, ActorMetric::Phase)
    });
    h.report("adoptions", |h| {
        actor_metric(h.g, actor, ActorMetric::NativeAdoptions)
    });
    h.report("new_work", |h| root_obligation_counter(h.g, 0));
}
#[test]
fn production_native_scope_spans_the_whole_nonmoving_old_cycle_and_allows_new_post_snapshot_journal_work()
 {
    let o = run(whole_scope, 2, true);
    assert_eq!(o.get("value"), 4042);
    assert_eq!(o.get("valid"), 1);
    assert_eq!(o.get("actor_phase"), o.get("phase"));
    assert_eq!(o.get("adoptions"), 1);
    assert_eq!(o.get("new_work"), 1);
}
#[test]
fn production_native_select_adoption_rejects_missing_census_or_still_active_mark() {
    for mode in 0..2 {
        let o = run_failure(whole_scope, mode);
        assert!(!o.status.success());
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant"),
            "mode {mode}: {o:?}"
        );
    }
}

fn empty_phase(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    for _ in 0..64 {
        journal_region_new(h.g, 2);
    }
    h.mark_start();
    h.report("before_phase", |h| h.counter(Counter::AdoptedPhase));
    h.report("before_regions", |h| journal_regions_visited(h.g));
    h.report("before_yields", |h| root_journal_yields(h.g));
    h.report("pending", |h| root_obligation_counter(h.g, 0));
    root_journal_advance_old(h.g);
    h.report("after_phase", |h| h.counter(Counter::AdoptedPhase));
    h.report("after_regions", |h| journal_regions_visited(h.g));
    h.report("after_yields", |h| root_journal_yields(h.g));
}
#[test]
fn production_empty_ledger_phase_publication_visits_no_journal_regions_even_with_many_registered_roots()
 {
    let o = run(empty_phase, 0, false);
    assert_eq!(o.get("pending"), 0);
    assert_ne!(o.get("before_phase"), o.get("after_phase"));
    assert_eq!(o.get("before_regions"), o.get("after_regions"));
    assert_eq!(o.get("before_yields"), o.get("after_yields"));
}

fn legacy_borrow(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    if mode == 2 {
        root_legacy_scope(h.g, true, false);
        root_legacy_scope(h.g, false, true);
        return;
    }
    h.alloc(1, 16, 0);
    h.root(0);
    publish_primary(h.g);
    let actor = create_actor(h.g, 16, 8);
    native_actor(h.g, actor, true);
    if mode == 3 {
        root_legacy_scope(h.g, true, true);
        root_legacy_scope(h.g, false, false); // mismatched pair
        return;
    }
    root_legacy_scope(h.g, true, false);
    h.report("borrowed", |h| root_native_borrows(h.g));
    h.report("journal", |h| root_obligation_counter(h.g, 0));
    match mode {
        0 => {
            native_actor(h.g, actor, false);
            root_mark_other_owner(h.g, actor);
        }
        1 => {
            root_legacy_registry_fault(h.g);
            root_legacy_scope(h.g, false, false);
        }
        2 => root_legacy_scope(h.g, false, true), // no-raw-borrow return cannot steal this scope
        4 => {
            root_legacy_scope(h.g, false, false);
            h.report("released", |h| root_native_borrows(h.g));
            h.mark_start();
            h.end_marks();
            native_actor(h.g, actor, false);
            h.report_counter("valid", Counter::MarkValid);
            h.report("final_borrows", |h| root_native_borrows(h.g));
        }
        _ => unreachable!(),
    }
}
#[test]
fn production_empty_journal_never_allows_phase_changes_during_a_real_legacy_borrow_or_a_mismatched_native_return()
 {
    for mode in 0..4 {
        let o = run_failure(legacy_borrow, mode);
        assert!(!o.status.success(), "mode {mode} accepted");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant"),
            "mode {mode}: {o:?}"
        );
    }
}
#[test]
fn production_legacy_borrow_releases_exactly_before_a_later_nonmoving_old_cycle() {
    let o = run(legacy_borrow, 4, true);
    assert_eq!(o.get("borrowed"), 1);
    assert_eq!(o.get("journal"), 0);
    assert_eq!(o.get("released"), 0);
    assert_eq!(o.get("valid"), 1);
    assert_eq!(o.get("final_borrows"), 0);
}

fn run_aarch64_protocol_modes(modes: std::ops::Range<u64>) {
    let mut candidates: Vec<std::path::PathBuf> = std::env::var_os("KLASSIC_UNICORN_PYTHON")
        .map(std::path::PathBuf::from)
        .into_iter()
        .collect();
    candidates.push("python3".into());
    if let Some(home) = std::env::var_os("HOME")
        && let Ok(entries) =
            std::fs::read_dir(std::path::PathBuf::from(home).join(".pyenv/versions"))
    {
        let mut found: Vec<_> = entries
            .filter_map(Result::ok)
            .map(|entry| entry.path().join("bin/python3"))
            .collect();
        found.sort();
        candidates.extend(found);
    }
    let Some(python) = candidates.into_iter().find(|p| {
        std::process::Command::new(p)
            .args(["-c", "import unicorn"])
            .output()
            .is_ok_and(|o| o.status.success())
    }) else {
        eprintln!("AArch64 arithmetic execution requires Python unicorn");
        return;
    };
    let dir = std::env::temp_dir().join(format!(
        "klassic-root-arithmetic-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&dir).unwrap();
    for mode in modes {
        let image = dir.join(format!("case-{mode}.macho"));
        std::fs::write(&image, crate::aarch64::gc_root_arithmetic_test_image(mode)).unwrap();
        let out = std::process::Command::new(&python)
            .arg(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../../tools/run-macho-aarch64.py"
            ))
            .arg(&image)
            .output()
            .unwrap();
        eprintln!("AArch64 arithmetic mode {mode}: {:?}", out.status.code());
        assert_eq!(
            out.status.code(),
            Some(if matches!(mode, 0 | 5 | 8 | 12) { 0 } else { 1 }),
            "mode {mode}: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
    std::fs::remove_dir_all(dir).unwrap();
}

#[test]
fn production_aarch64_root_and_native_borrow_arithmetic_executes_with_explicit_result_checks() {
    run_aarch64_protocol_modes(0..8);
}
#[test]
fn production_aarch64_nonmoving_adoption_tests_actual_remap_mask_result_and_multiple_old_cycles() {
    run_aarch64_protocol_modes(8..15);
}
