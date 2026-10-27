//! Actual two/eight concurrent mutator forwarders, with one real worker parked
//! outside its ticket. The legacy moving phase starts while peers are Native;
//! this does not grant all-Running N-owner moving admission.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn overlap(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let n = arg & 255;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    publish_primary(h.g);
    if arg & 256 != 0 {
        set_tenuring(h.g, 2);
        h.collect();
        h.finish_collection();
    }
    h.report("identity_before", |h| h.object_id(Src::Root(0)));
    h.report("address_before", |h| h.value(Src::Root(0)));
    let actors: Vec<_> = (0..n).map(|_| create_actor(h.g, 16, 8)).collect();
    for actor in &actors {
        copy_actor_native_enter(h.g, *actor);
    }
    h.g.asm
        .data
        .resize(h.g.asm.data.len().next_multiple_of(8), 0);
    let root = h.g.asm.data_label_with_i64s(&[0]);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    h.g.mov_data_addr(R::V1, root);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    let state = prepare_copy_overlap(h.g, &actors, arg >> 16);
    copy_probe_root(h.g, state, root);
    if arg & 256 != 0 {
        set_tenuring(h.g, 2);
    }
    h.collect();
    let p = run_parallel_copy_actors(h.g, &actors, root, state, arg >> 16);
    for (i, name) in [
        (2, "arrived"),
        (3, "done"),
        (4, "peak"),
        (5, "remaining"),
        (7, "source"),
        (8, "phase"),
        (9, "bank"),
        (10, "worker_active_at_park"),
    ] {
        h.report(name, |h| copy_overlap_metric(h.g, state, i));
    }
    h.report("aggregate", |h| copy_aggregate(h.g));
    h.report("ready", |h| parallel_actor_ready(h.g, &p));
    for (i, actor) in actors.iter().enumerate() {
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        for (field, name) in [(0, "address"), (1, "id"), (3, "value")] {
            h.report(&format!("{name}{i}"), |h| {
                parallel_copy_result(h.g, &p, i, field)
            });
        }
        for (metric, name) in [
            (ActorMetric::CopyClaim, "claim"),
            (ActorMetric::CopyWins, "wins"),
            (ActorMetric::CopyLosses, "losses"),
            (ActorMetric::NativeAdoptions, "adoptions"),
        ] {
            h.report(&format!("{name}{i}"), |h| actor_metric(h.g, *actor, metric));
        }
    }
    h.report("old", |h| h.is_old(Src::Root(0)));
    h.finish_collection();
    h.report("promoted_bytes", |h| copy_promoted_bytes(h.g));
    h.report_counter("logical_promoted", Counter::AgePromoted);
    h.report("age", |h| {
        h.object_metadata(
            Src::Root(0),
            std::mem::offset_of!(crate::gc_abi::MarkRecord, age),
        )
    });
    h.report("identity_after", |h| h.object_id(Src::Root(0)));
    h.report("value_after", |h| h.load(Src::Root(0), 0));
}
fn check(promote: bool, writer: bool) {
    for n in [2, 8] {
        let o = run(
            overlap,
            n | if promote { 256 } else { 0 } | if writer { 4 << 16 } else { 0 },
            true,
        );
        assert_eq!(o.get("arrived"), n as i64, "{}", o.text());
        assert_eq!(o.get("done"), n as i64);
        assert_eq!(o.get("peak"), n as i64);
        assert_eq!(o.get("remaining"), 1);
        assert_eq!(o.get("source"), o.get("address_before"));
        assert_eq!(o.get("aggregate"), 0);
        assert_eq!(o.get("worker_active_at_park"), 0);
        assert_eq!(o.get("ready"), n as i64);
        assert_eq!(o.get("old"), i64::from(promote));
        assert_eq!(o.get("identity_before"), o.get("identity_after"));
        assert_eq!(o.get("value_after"), if writer { 99 } else { 42 });
        assert_eq!(o.get("age"), if promote { 2 } else { 1 });
        let bytes = (std::mem::size_of::<crate::gc_abi::ObjectHeader>() + 16) as i64;
        assert_eq!(
            o.get("promoted_bytes"),
            if promote { n as i64 * bytes } else { 0 }
        );
        assert_eq!(o.get("logical_promoted"), if promote { bytes } else { 0 });
        let mut tids = std::collections::HashSet::new();
        let mut wins = 0;
        let mut losses = 0;
        for i in 0..n {
            assert!(tids.insert(o.get(&format!("tid{i}"))));
            assert_eq!(o.get(&format!("address{i}")), o.get("address0"));
            assert_ne!(o.get(&format!("address{i}")), o.get("address_before"));
            assert_eq!(o.get(&format!("id{i}")), o.get("identity_before"));
            assert_eq!(o.get(&format!("value{i}")), if writer { 99 } else { 42 });
            assert_eq!(o.get(&format!("claim{i}")), 0);
            assert_eq!(o.get(&format!("adoptions{i}")), 1);
            wins += o.get(&format!("wins{i}"));
            losses += o.get(&format!("losses{i}"));
        }
        assert_eq!(wins, 1);
        assert_eq!(losses, n as i64 - 1);
        assert_eq!(o.get("wins0"), 0);
        assert_eq!(o.get("losses0"), 1);
    }
}
#[test]
fn production_two_and_eight_real_young_copiers_keep_the_last_owned_source_protected() {
    check(false, false);
}
#[test]
fn production_two_and_eight_real_promoters_keep_the_last_owned_source_protected() {
    check(true, false);
}

#[test]
fn production_live_foreign_copier_blocks_retirement_bank_advance_and_foreign_completion() {
    for mode in 1..=3 {
        let o = super::execute(overlap, 2 | (mode << 16), true, false);
        assert_eq!(o.status.code(), Some(1), "mode={mode}: {o:?}");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
            "mode={mode}: {o:?}"
        );
    }
}

fn fault(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    publish_primary(h.g);
    let primary = primary_copy_probe(h.g);
    let root = h.g.asm.data_label_with_i64s(&[0]);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    h.g.mov_data_addr(R::V1, root);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    let state = prepare_copy_overlap(h.g, &[primary], 0);
    h.collect();
    install_copy_fault(h.g, state, mode);
    invoke_actual_copy(h.g, root);
}
#[test]
fn production_actual_copy_claim_rejects_recursion_underflow_misownership_snapshot_change_and_overflow()
 {
    for mode in 0..8 {
        let o = super::execute(fault, mode, true, false);
        assert_eq!(o.status.code(), Some(1), "mode={mode}: {o:?}");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
            "mode={mode}: {o:?}"
        );
    }
}
fn native_fault(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    publish_primary(h.g);
    let actor = create_actor(h.g, 16, 8);
    copy_actor_native_enter(h.g, actor);
    let state = prepare_copy_overlap(h.g, &[actor], 0);
    disable_copy_probe_hooks(h.g);
    h.collect();
    if mode == 5 || mode == 6 {
        h.finish_collection();
    }
    if mode == 6 {
        copy_worker_release(h.g, state);
        copy_prepare_snapshot(h.g, state);
        h.collect();
    }
    native_young_return_fault(h.g, actor, mode);
}
#[test]
fn production_native_young_return_rejects_invalid_receipts_and_an_unclosed_second_move() {
    for mode in [0, 1, 2, 3, 4, 6] {
        let o = super::execute(native_fault, mode, true, false);
        assert_eq!(o.status.code(), Some(1), "mode={mode}: {o:?}");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
            "mode={mode}: {o:?}"
        );
    }
}

#[test]
fn production_late_young_and_promoted_cas_losers_preserve_the_winners_actual_scalar_update() {
    for promote in [false, true] {
        check(promote, true);
    }
}

fn ready(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    publish_primary(h.g);
    let primary = primary_copy_probe(h.g);
    let root = h.g.asm.data_label_with_i64s(&[0]);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    h.g.mov_data_addr(R::V1, root);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    let state = prepare_copy_overlap(h.g, &[primary], 0);
    let data = copy_ready_observer(h.g, state);
    h.collect();
    resolve_copy(h.g, root);
    for (i, name) in [
        "ready",
        "pending",
        "producers",
        "source_descriptor",
        "copying",
    ]
    .into_iter()
    .enumerate()
    {
        h.report(name, |h| copy_ready_metric(h.g, data, i));
    }
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_destination_is_actually_published_before_the_sources_forwarding_cas() {
    let o = run(ready, 0, true);
    assert_eq!(o.get("ready"), crate::gc_abi::OBJECT_PUBLISHED as i64);
    assert_eq!(o.get("pending"), 0);
    assert_eq!(o.get("producers"), 0);
    assert_eq!(o.get("source_descriptor"), 1);
    assert_eq!(o.get("copying"), 1);
    assert_eq!(o.get("value"), 42);
}
