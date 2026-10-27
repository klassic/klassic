//! Actual completed root-census receipts, closed source reuse and real Native
//! peer return. No old heap address is handed to a returning owner.
use super::{H, run};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn completed(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let n = arg & 255;
    let cycles = (arg >> 8) & 255;
    let full = arg & 65536 != 0;
    let graph = arg & 131072 != 0;
    let marks = (arg >> 24) & 15;
    let mut h = H::begin(g);
    publish_primary(h.g);
    h.report("records_before", |h| native_receipt_metric(h.g, 6));
    let mut actors = Vec::new();
    let mut roots = Vec::new();
    for i in 0..n {
        let actor = create_actor(h.g, 16, 8);
        let root = if graph {
            native_completed_graph(h.g, actor, 42 + i)
        } else {
            actor_alloc(h.g, actor, 16);
            h.g.mov_imm64(R::V1, 42 + i);
            h.g.store_ptr_disp32(R::V0, 0, R::V1);
            adopt_actor_value(h.g, actor)
        };
        if graph {
            h.report(&format!("leaf_before{i}"), |h| {
                native_completed_leaf_id(h.g, actor, root)
            });
        }
        h.report(&format!("id_before{i}"), |h| {
            native_completed_root(h.g, actor, root, true)
        });
        h.report(&format!("address_before{i}"), |h| {
            native_completed_root(h.g, actor, root, false)
        });
        copy_actor_native_enter(h.g, actor);
        actors.push(actor);
        roots.push(root);
    }
    let p = if marks == 0 {
        run_parallel_native_completed(h.g, &actors, &roots, cycles, full, graph)
    } else {
        run_parallel_native_next_mark(h.g, &actors, &roots, cycles, full, marks - 1)
    };
    for (i, name) in [
        "young_active",
        "young_valid",
        "young_epoch",
        "old_active",
        "old_valid",
        "old_epoch",
        "current_phase",
    ]
    .into_iter()
    .enumerate()
    {
        h.report(name, |h| native_next_metric(h.g, i));
    }
    h.report("ready", |h| parallel_actor_ready(h.g, &p));
    h.report_counter("collections", Counter::Collections);
    h.report_counter("full_collections", Counter::FullCollections);
    for (i, name) in [
        "receipt_bank",
        "receipt_scope",
        "receipt_phase",
        "current_bank",
        "retiring",
        "open",
        "records",
        "used",
    ]
    .into_iter()
    .enumerate()
    {
        h.report(name, |h| native_receipt_metric(h.g, i));
    }
    for (i, actor) in actors.iter().enumerate() {
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        for (field, name) in [(0, "address"), (1, "id")] {
            h.report(&format!("{name}{i}"), |h| {
                parallel_copy_result(h.g, &p, i, field)
            });
        }
        for (field, name) in [
            (64, "value"),
            (72, "allocated_value"),
            (80, "owner_bank"),
            (88, "entry_phase"),
            (96, "phase"),
            (104, "bank"),
            (120, "new_record"),
            (112, "leaf_id"),
        ] {
            h.report(&format!("{name}{i}"), |h| {
                native_completed_metric(h.g, &p, i, field)
            });
        }
        h.report(&format!("adoptions{i}"), |h| {
            actor_metric(h.g, *actor, ActorMetric::NativeAdoptions)
        });
        h.report(&format!("claim{i}"), |h| {
            actor_metric(h.g, *actor, ActorMetric::CopyClaim)
        });
    }
}
fn check(full: bool, graph: bool) {
    for n in [2, 8] {
        for cycles in [1, 2, 4] {
            let o = run(
                completed,
                n | (cycles << 8) | if full { 65536 } else { 0 } | if graph { 131072 } else { 0 },
                true,
            );
            assert_eq!(o.get("ready"), n as i64, "{}", o.text());
            assert_eq!(o.get("collections"), cycles as i64, "{}", o.text());
            assert_eq!(
                o.get("full_collections"),
                if full { cycles as i64 } else { 0 }
            );
            assert_eq!(o.get("receipt_bank"), o.get("current_bank"));
            assert_eq!(o.get("current_bank"), cycles as i64 + 1);
            if cycles % 2 == 0 {
                assert_eq!(o.get("records"), o.get("records_before"));
            }
            assert_eq!(o.get("retiring"), 0);
            assert_eq!(o.get("open"), 0);
            let mut tids = std::collections::HashSet::new();
            let mut records = std::collections::HashSet::new();
            for i in 0..n {
                assert!(tids.insert(o.get(&format!("tid{i}"))));
                assert_eq!(o.get(&format!("id{i}")), o.get(&format!("id_before{i}")));
                assert_eq!(o.get(&format!("value{i}")), 42 + i as i64);
                if graph {
                    assert_eq!(
                        o.get(&format!("leaf_id{i}")),
                        o.get(&format!("leaf_before{i}"))
                    );
                }
                assert_eq!(o.get(&format!("allocated_value{i}")), 99);
                assert_eq!(o.get(&format!("bank{i}")), o.get("current_bank"));
                assert_eq!(o.get(&format!("owner_bank{i}")), o.get("current_bank"));
                assert_eq!(o.get(&format!("phase{i}")), o.get("receipt_phase"));
                assert_ne!(
                    o.get(&format!("phase{i}")),
                    o.get(&format!("entry_phase{i}"))
                );
                if cycles % 2 == 0 {
                    assert_eq!(
                        o.get(&format!("phase{i}")) & 65535,
                        o.get(&format!("entry_phase{i}")) & 65535
                    );
                }
                assert_eq!(o.get(&format!("adoptions{i}")), 1);
                assert_eq!(o.get(&format!("claim{i}")), 0);
                let record = o.get(&format!("new_record{i}"));
                let size = std::mem::size_of::<crate::gc_abi::MarkRecord>() as i64;
                assert!(
                    record >= o.get("records") && record < o.get("records") + o.get("used") * size
                );
                assert_eq!((record - o.get("records")) % size, 0);
                assert!(records.insert(record));
            }
        }
    }
}
#[test]
fn production_real_native_peers_return_after_one_two_and_four_completed_young_moves() {
    check(false, false);
}
#[test]
fn production_real_native_peers_return_after_one_two_and_four_completed_full_old_moves() {
    check(true, false);
}

#[test]
fn production_real_native_peers_preserve_retained_parent_leaf_cycles_across_completed_moves() {
    check(false, true);
    check(true, true);
}
fn fault(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let h = H::begin(g);
    publish_primary(h.g);
    let actor = create_actor(h.g, 16, 8);
    actor_alloc(h.g, actor, 16);
    adopt_actor_value(h.g, actor);
    copy_actor_native_enter(h.g, actor);
    native_completed_collect(h.g);
    native_completed_collect(h.g);
    native_completed_fault(h.g, actor, mode);
}
#[test]
fn production_completed_native_return_rejects_absent_stale_foreign_or_uncovered_actual_receipts() {
    for mode in 0..9 {
        let o = super::execute(fault, mode, true, false);
        assert_eq!(o.status.code(), Some(1), "mode={mode}: {o:?}");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
            "mode={mode}: {o:?}"
        );
    }
}

fn entered_active(g: &mut crate::NativeCodeGenerator, marking: u64) {
    use super::Src;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    publish_primary(h.g);
    let actor = create_actor(h.g, 16, 8);
    h.report("identity_before", |h| h.object_id(Src::Root(0)));
    let enter = h.g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
    let exit = h.g.gc_gen.as_ref().unwrap().labels.native_exit_actor;
    if marking == 0 {
        call_owner(h.g, enter);
        native_completed_open_other(h.g, actor);
        call_owner(h.g, exit);
        h.report("bank_entered", |h| native_receipt_metric(h.g, 3));
        h.report("phase_entered", |h| worker_view_metric(h.g, 4));
        call_owner(h.g, enter); // genuine ConcurrentRelocate entry
        native_completed_finish_other(h.g, actor);
    } else {
        copy_actor_native_enter(h.g, actor);
        h.mark_start();
        h.report("bank_entered", |h| native_receipt_metric(h.g, 3));
        h.report("phase_entered", |h| worker_view_metric(h.g, 4));
        call_owner(h.g, enter); // genuine Old ConcurrentMark entry
        native_completed_resume(h.g, actor);
        native_completed_full_other(h.g, actor);
    }
    call_owner(h.g, exit);
    h.report("bank_returned", |h| native_receipt_metric(h.g, 3));
    h.report("phase_returned", |h| worker_view_metric(h.g, 4));
    h.report("identity_after", |h| h.object_id(Src::Root(0)));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_completed_native_return_accepts_real_moving_and_marking_entries() {
    for marking in [0, 1] {
        let o = run(entered_active, marking, true);
        assert_eq!(o.get("identity_before"), o.get("identity_after"));
        assert_eq!(o.get("value"), 42);
        assert_eq!(
            o.get("phase_entered") & 255,
            if marking == 0 { 6 } else { 32 }
        );
        assert_eq!(o.get("phase_returned") & 255, 0);
        assert_eq!(
            o.get("bank_returned") - o.get("bank_entered"),
            marking as i64
        );
    }
}

fn regions(g: &mut crate::NativeCodeGenerator, count: u64) {
    use super::Src;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    publish_primary(h.g);
    let actor = create_actor(h.g, 16, 8);
    let mut retained = None;
    for _ in 0..count {
        retained = Some(journal_region_new(h.g, 2));
    }
    let region = retained.unwrap();
    h.value(Src::Raw(0));
    journal_region_store(h.g, region, true); // the only strong reference is outside frame storage
    h.report("identity_before", |h| {
        native_completed_region_id(h.g, region)
    });
    let enter = h.g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
    let exit = h.g.gc_gen.as_ref().unwrap().labels.native_exit_actor;
    call_owner(h.g, enter);
    for _ in 0..4 {
        native_completed_full_other(h.g, actor);
    }
    call_owner(h.g, exit);
    h.report("identity_after", |h| {
        native_completed_region_id(h.g, region)
    });
    h.report("value", |h| journal_region_load(h.g, region));
    h.report("scope", |h| native_receipt_metric(h.g, 1));
}
#[test]
fn production_completed_native_return_covers_real_owned_regions_and_empty_inventory_transitions() {
    for count in [1, 19] {
        let o = run(regions, count, true);
        assert_eq!(o.get("identity_before"), o.get("identity_after"));
        assert_eq!(o.get("value"), 42);
        assert!(o.get("scope") >= count as i64 + 2);
    }
}

fn check_next_mark(dual: bool, selected: bool) {
    for full in [false, true] {
        for n in [2, 8] {
            for cycles in [1, 2, 4] {
                let marks = u64::from(dual) | (u64::from(selected) << 1);
                let o = run(
                    completed,
                    n | (cycles << 8) | if full { 65536 } else { 0 } | 131072 | ((marks + 1) << 24),
                    true,
                );
                assert_eq!(o.get("current_bank"), cycles as i64 + 1, "{}", o.text());
                assert_eq!(o.get("receipt_bank"), o.get("current_bank"));
                assert_eq!(o.get("young_epoch"), cycles as i64 + 1);
                assert_eq!(o.get("young_active"), i64::from(!selected));
                assert_eq!(o.get("young_valid"), i64::from(selected));
                assert_eq!(o.get("old_active"), i64::from(dual && !selected));
                assert_eq!(o.get("old_valid"), i64::from(dual && selected));
                assert!(o.get("current_phase") > o.get("receipt_phase"));
                let mut tids = std::collections::HashSet::new();
                for i in 0..n {
                    assert!(tids.insert(o.get(&format!("tid{i}"))));
                    assert_eq!(o.get(&format!("id{i}")), o.get(&format!("id_before{i}")));
                    assert_eq!(
                        o.get(&format!("leaf_id{i}")),
                        o.get(&format!("leaf_before{i}"))
                    );
                    assert_eq!(o.get(&format!("value{i}")), 42 + i as i64);
                    assert_eq!(o.get(&format!("allocated_value{i}")), 99);
                    assert_eq!(o.get(&format!("phase{i}")), o.get("current_phase"));
                    assert_eq!(o.get(&format!("owner_bank{i}")), o.get("current_bank"));
                    assert_eq!(o.get(&format!("adoptions{i}")), 1);
                }
            }
        }
    }
}
#[test]
fn production_native_peers_return_after_moves_into_a_later_young_snapshot() {
    check_next_mark(false, false);
}
#[test]
fn production_native_peers_return_after_moves_into_later_dual_snapshots() {
    check_next_mark(true, false);
}
#[test]
fn production_native_peers_return_after_moves_into_later_selected_snapshots() {
    check_next_mark(false, true);
    check_next_mark(true, true);
}

fn next_mark_fault(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let h = H::begin(g);
    publish_primary(h.g);
    let actor = create_actor(h.g, 16, 8);
    native_completed_graph(h.g, actor, 42);
    copy_actor_native_enter(h.g, actor);
    native_next_fault(h.g, actor, mode);
}
#[test]
fn production_native_return_rejects_inconsistent_later_young_and_old_engines() {
    for mode in 0..9 {
        let o = super::execute(next_mark_fault, mode, true, false);
        assert_eq!(o.status.code(), Some(1), "mode={mode}: {o:?}");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
            "mode={mode}: {o:?}"
        );
    }
}
