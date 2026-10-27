//! Actual survival/promotion and shared controller probes in emitted code.
use super::{H, Src, run, run_failure};
use crate::gc_codegen::production::{
    MAX_PAYLOAD,
    test_support::{Counter, request_full_collection, set_tenuring, tenuring_probe, wake_worker},
};
use crate::portable_asm::{PortableAsm, Reg as R};
use crate::{
    NativeCodeGenerator,
    gc_abi::{MAX_AGE, MarkRecord, ObjectHeader},
};
use std::mem::{offset_of, size_of};
const M: u64 = 1 << 20;
fn age(h: &mut H<'_>, s: Src) {
    h.object_metadata(s, offset_of!(MarkRecord, age));
}
fn set_age(h: &mut H<'_>, s: Src, value: u64) {
    h.value(s);
    h.g.load_ptr_disp32(
        R::V1,
        R::V0,
        offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
    );
    h.g.mov_imm64(R::V2, value);
    h.g.store_ptr_disp32(R::V1, offset_of!(MarkRecord, age) as i32, R::V2);
}
fn cycle(h: &mut H<'_>) {
    h.collect();
    h.finish_collection();
}
fn heavy(h: &mut H<'_>) {
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.repeat(200, |h| {
        h.alloc(1, MAX_PAYLOAD, 9);
        h.root(9);
    });
    cycle(h);
    cycle(h); // 25 MiB live, promoted at bootstrap age two.
}
fn raise(h: &mut H<'_>) {
    heavy(h);
    cycle(h);
    cycle(h);
}
fn invariants(h: &mut H<'_>) {
    h.report("id", |h| h.object_id(Src::Root(0)));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report_counter("unmarked", Counter::Unmarked);
    h.report_counter("young_unmarked", Counter::YoungUnmarked);
}
fn rising(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    heavy(&mut h);
    h.report_counter("bootstrap", Counter::AgeThreshold);
    cycle(&mut h);
    h.report_counter("one", Counter::AgeThreshold);
    h.report_counter("vote", Counter::AgeUp);
    cycle(&mut h);
    h.report_counter("two", Counter::AgeThreshold);
    h.report_counter("updates", Counter::AgeUpdates);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 99);
    h.root(1);
    h.report("fresh_id_before", |h| h.object_id(Src::Root(1)));
    cycle(&mut h);
    h.report("age1", |h| age(h, Src::Root(1)));
    h.report("old1", |h| h.is_old(Src::Root(1)));
    cycle(&mut h);
    h.report("age2", |h| age(h, Src::Root(1)));
    h.report("old2", |h| h.is_old(Src::Root(1)));
    h.report_counter("threshold", Counter::AgeThreshold);
    h.report("fresh", |h| h.load(Src::Root(1), 0));
    h.report("fresh_id_after", |h| h.object_id(Src::Root(1)));
    invariants(&mut h);
}
#[test]
fn production_tenuring_raises_after_two_low_survival_samples_and_delays_medium_lived_promotion() {
    let o = run(rising, 0, false);
    assert_eq!(o.get("fresh_id_before"), o.get("fresh_id_after"));
    for (k, n) in [
        ("bootstrap", 2),
        ("one", 2),
        ("vote", 1),
        ("two", 3),
        ("updates", 1),
        ("age1", 1),
        ("age2", 2),
        ("old1", 0),
        ("old2", 0),
        ("threshold", 4),
        ("fresh", 99),
        ("id", 1),
        ("value", 84),
        ("unmarked", 0),
        ("young_unmarked", 0),
    ] {
        assert_eq!(o.get(k), n, "{k}: {}", o.text());
    }
}
fn falling(g: &mut NativeCodeGenerator, _arg: u64) {
    // Isolate adaptive-age sampling from emergency promotion reservation.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    heavy(&mut h);
    // The independent rising test covers threshold acquisition; seed its
    // legal result here so hard-cap headroom does not alter that preparation.
    set_tenuring(h.g, 3);
    h.repeat(128, |h| {
        h.alloc(1, MAX_PAYLOAD, 9);
        h.set_scalar(Src::Raw(9), 0, 42);
        h.root(9);
    });
    cycle(&mut h);
    h.report_counter("one", Counter::AgeThreshold);
    h.report_counter("vote", Counter::AgeDown);
    cycle(&mut h);
    h.report_counter("two", Counter::AgeThreshold);
    h.report("age2", |h| age(h, Src::Root(9)));
    h.report("old2", |h| h.is_old(Src::Root(9)));
    cycle(&mut h);
    h.report_counter("frozen", Counter::AgeFrozen);
    h.report("age3", |h| age(h, Src::Root(9)));
    h.report("old3", |h| h.is_old(Src::Root(9)));
    h.report("fresh", |h| h.load(Src::Root(9), 0));
    h.report_counter("full", Counter::FullCollections);
    invariants(&mut h);
}
#[test]
fn production_tenuring_lowers_on_two_high_survival_samples_and_promotes_by_the_new_age() {
    let o = run(falling, 0, false);
    for (k, n) in [
        ("one", 3),
        ("vote", 1),
        ("two", 2),
        ("age2", 2),
        ("old2", 0),
        ("frozen", 2),
        ("age3", 3),
        ("old3", 1),
        ("fresh", 42),
        ("full", 0),
        ("id", 1),
        ("value", 84),
        ("unmarked", 0),
        ("young_unmarked", 0),
    ] {
        assert_eq!(o.get(k), n, "{k}: {}", o.text());
    }
}
fn frozen(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    raise(&mut h);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 99);
    h.root(1);
    cycle(&mut h);
    h.value(Src::Root(1));
    h.keep(2);
    h.collect();
    set_tenuring(h.g, 2); // Future setting cannot change an open copy cycle.
    h.worker_copy(Src::Raw(2));
    h.finish_collection();
    h.report_counter("frozen", Counter::AgeFrozen);
    h.report_counter("worker", Counter::AgeWorkerYoung);
    h.report("age2", |h| age(h, Src::Root(1)));
    h.report("old2", |h| h.is_old(Src::Root(1)));
    cycle(&mut h);
    h.report("age3", |h| age(h, Src::Root(1)));
    h.report("old3", |h| h.is_old(Src::Root(1)));
    h.report("fresh", |h| h.load(Src::Root(1), 0));
    invariants(&mut h);
}
#[test]
fn production_copiers_use_the_same_frozen_tenuring_threshold_until_the_cycle_closes() {
    let o = run(frozen, 0, false);
    for (k, n) in [
        ("frozen", 3),
        ("worker", 48),
        ("age2", 2),
        ("old2", 0),
        ("age3", 3),
        ("old3", 1),
        ("fresh", 99),
        ("id", 1),
        ("value", 84),
        ("unmarked", 0),
        ("young_unmarked", 0),
    ] {
        assert_eq!(o.get(k), n, "{k}: {}", o.text());
    }
}
fn winners(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 17, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.value(Src::Raw(0));
    h.keep(2);
    h.root(0);
    h.value(Src::Root(0));
    h.keep(1);
    h.root(1);
    h.collect();
    h.worker_copy(Src::Raw(2));
    h.worker_copy(Src::Raw(2));
    h.finish_collection();
    h.finish_collection();
    h.report_counter("young", Counter::AgeYoung);
    h.report_counter("worker_young", Counter::AgeWorkerYoung);
    h.report_counter("owner_young", Counter::AgeOwnerYoung);
    h.report_counter("promoted0", Counter::AgePromoted);
    h.value(Src::Root(0));
    h.keep(2);
    h.collect();
    h.worker_copy(Src::Raw(2));
    h.worker_copy(Src::Raw(2));
    h.finish_collection();
    h.report_counter("promoted", Counter::AgePromoted);
    h.report_counter("worker_promoted", Counter::AgeWorkerPromoted);
    h.report_counter("owner_promoted", Counter::AgeOwnerPromoted);
    h.alloc(1, 16, 3);
    h.set_scalar(Src::Raw(3), 0, 99);
    h.root(3);
    h.alloc(1, 16, 4);
    h.set_scalar(Src::Raw(4), 0, 77);
    h.root(4);
    set_age(&mut h, Src::Root(4), MAX_AGE);
    set_tenuring(h.g, MAX_AGE);
    request_full_collection(h.g);
    cycle(&mut h);
    h.report_counter("full_young", Counter::AgeYoung);
    h.report_counter("full_promoted", Counter::AgePromoted);
    h.report_counter("threshold", Counter::AgeThreshold);
    h.report_counter("full_samples", Counter::AgeFullSamples);
    h.report_counter("young_samples", Counter::AgeYoungSamples);
    h.report_counter("up", Counter::AgeUp);
    h.report_counter("down", Counter::AgeDown);
    h.report("old_age", |h| age(h, Src::Root(0)));
    h.report("forced_age", |h| age(h, Src::Root(3)));
    h.report("saturated_age", |h| age(h, Src::Root(4)));
    h.report("forced_old", |h| h.is_old(Src::Root(3)));
    h.report("forced_value", |h| h.load(Src::Root(3), 0));
    invariants(&mut h);
}
#[test]
fn production_tenuring_counts_winners_once_excludes_old_moves_and_ignores_forced_full_samples() {
    let o = run(winners, 0, false);
    for (k, n) in [
        ("young", 64),
        ("worker_young", 64),
        ("owner_young", 0),
        ("promoted0", 0),
        ("promoted", 64),
        ("worker_promoted", 64),
        ("owner_promoted", 0),
        ("full_young", 0),
        ("full_promoted", 96),
        ("threshold", 15),
        ("full_samples", 1),
        ("young_samples", 2),
        ("up", 0),
        ("down", 0),
        ("old_age", 2),
        ("forced_age", 1),
        ("saturated_age", 15),
        ("forced_old", 1),
        ("forced_value", 99),
        ("id", 1),
        ("value", 84),
        ("unmarked", 0),
        ("young_unmarked", 0),
    ] {
        assert_eq!(o.get(k), n, "{k}: {}", o.text());
    }
}
fn choices(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    for (n, t, y, p, old, up, down) in [
        ("one", 2, 0, 0, 24 * M, 0, 0),
        ("rise", 2, 0, 0, 24 * M, 1, 0),
        ("cap", 15, 0, 0, 24 * M, 1, 0),
        ("fall", 3, 0, 12 * M, 0, 0, 1),
        ("floor", 2, 12 * M, 0, 0, 0, 1),
        ("neutral", 3, 8 * M, 0, 25 * M, 1, 1),
        ("low_old", 3, 0, 0, 23 * M, 1, 0),
    ] {
        tenuring_probe(h.g, t, y, p, old, up, down);
        h.report_counter(n, Counter::AgeThreshold);
    }
    h.report_counter("up", Counter::AgeUp);
    h.report_counter("down", Counter::AgeDown);
}

#[test]
fn production_tenuring_bounds_hysteresis_and_promoted_live_signals_share_the_real_controller() {
    let o = run(choices, 0, false);
    for (k, n) in [
        ("one", 2),
        ("rise", 3),
        ("cap", 15),
        ("fall", 2),
        ("floor", 2),
        ("neutral", 3),
        ("low_old", 3),
        ("up", 0),
        ("down", 0),
    ] {
        assert_eq!(o.get(k), n, "{k}: {}", o.text());
    }
}
fn bad_threshold(g: &mut NativeCodeGenerator, n: u64) {
    let mut h = H::begin(g);
    h.report("before", |h| h.value(Src::Imm(1)));
    set_tenuring(h.g, n);
    cycle(&mut h);
    h.report("after", |h| h.value(Src::Imm(1)));
}
#[test]
fn production_tenuring_rejects_invalid_threshold_before_copying() {
    for n in [0, 1, MAX_AGE + 1] {
        let o = run_failure(bad_threshold, n);
        assert_eq!(o.status.code(), Some(1));
        assert_eq!(o.stdout, b"before=1\n");
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
    }
}
fn bad_age(g: &mut NativeCodeGenerator, kind: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    if kind == 2 {
        cycle(&mut h);
        cycle(&mut h);
    }
    set_age(&mut h, Src::Root(0), MAX_AGE + 1);
    if kind != 0 {
        request_full_collection(h.g);
    }
    h.report("before", |h| h.value(Src::Imm(1)));
    cycle(&mut h);
    h.report("after", |h| h.value(Src::Imm(1)));
}
#[test]
fn production_tenuring_rejects_invalid_object_age_in_young_and_full_copies() {
    for kind in 0..3 {
        let o = run_failure(bad_age, kind);
        assert_eq!(o.status.code(), Some(1));
        assert_eq!(o.stdout, b"before=1\n");
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
    }
}

fn raced(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 65536, 0);
    h.root(0);
    h.alloc(1, MAX_PAYLOAD, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.collect();
    wake_worker(h.g, h.l.unpark);
    h.repeat(128, |h| {
        h.deref_field(Src::Root(0), 0, 0);
        h.keep(3);
    });
    h.finish_collection();
    h.report_counter("young", Counter::AgeYoung);
    h.report_counter("promoted", Counter::AgePromoted);
    h.report_counter("owner", Counter::AgeOwnerYoung);
    h.report_counter("worker", Counter::AgeWorkerYoung);
    h.report_counter("owner_lost", Counter::CopyOwnerLost);
    h.report_counter("worker_lost", Counter::CopyWorkerLost);
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
    h.report("age", |h| age(h, Src::Root(0)));
}
#[test]
fn production_racing_copy_winners_account_each_survivor_once_across_real_threads() {
    let o = run(raced, 0, true);
    assert_eq!(o.get("young"), 65568 + 131072, "{}", o.text());
    assert_eq!(
        o.get("owner") + o.get("worker"),
        o.get("young"),
        "{}",
        o.text()
    );
    assert_eq!(o.get("promoted"), 0);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("id"), 1);
    assert_eq!(o.get("age"), 1);
}

fn remembered_child(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    raise(&mut h);
    h.alloc(2, 16, 1);
    h.root(1);
    cycle(&mut h);
    cycle(&mut h);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 99);
    h.report("child_before", |h| h.object_id(Src::Raw(2)));
    h.store(Src::Root(1), 0, Src::Raw(2), true);
    cycle(&mut h);
    cycle(&mut h);
    h.report("parent_age", |h| age(h, Src::Root(1)));
    h.report("parent_old", |h| h.is_old(Src::Root(1)));
    h.report("child_old", |h| {
        h.load_barriered(Src::Root(1), 0);
        h.keep(2);
        h.is_old(Src::Raw(2));
    });
    h.report_counter("remset", Counter::RemsetLen);
    h.report("slot", |h| h.entry_is_slot(0, Src::Root(1), 0));
    cycle(&mut h); // The Old parent's remembered slot is the only child root.
    h.report("child_age", |h| {
        h.load_barriered(Src::Root(1), 0);
        h.keep(2);
        age(h, Src::Raw(2));
    });
    h.report("child_after", |h| h.object_id(Src::Raw(2)));
    h.report("fresh", |h| h.deref_field(Src::Root(1), 0, 0));
    h.report_counter("young", Counter::AgeYoung);
    invariants(&mut h);
}
#[test]
fn production_adaptive_promotion_retains_younger_children_through_remembered_slots() {
    let o = run(remembered_child, 0, false);
    assert_eq!(o.get("parent_age"), 4);
    assert_eq!(o.get("parent_old"), 1);
    assert_eq!(o.get("child_old"), 0);
    assert!(o.get("remset") > 0);
    assert_eq!(o.get("slot"), 1);
    assert_eq!(o.get("child_age"), 3);
    assert_eq!(o.get("child_after"), o.get("child_before"));
    assert_eq!(o.get("fresh"), 99);
    assert_eq!(o.get("young"), 48);
    assert_eq!(o.get("unmarked"), 0);
    assert_eq!(o.get("young_unmarked"), 0);
}

fn interrupted_normal_samples(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    heavy(&mut h);
    cycle(&mut h);
    // Automatic collections during heavy's allocation batches depend on
    // worker progress. Establish one actual low-survival vote rather than
    // assuming preparation always leaves the bootstrap threshold unchanged.
    let voted = h.g.create_text_label();
    h.counter(Counter::AgeUp);
    h.g.test_reg_reg(R::V0, R::V0);
    h.g.jcc_label(crate::portable_asm::Condition::NotEqual, voted);
    cycle(&mut h);
    h.g.bind_text_label(voted);
    h.report_counter("before_vote", Counter::AgeUp);
    h.report_counter("before_threshold", Counter::AgeThreshold);
    h.report_counter("before_updates", Counter::AgeUpdates);
    h.report_counter("before_full_samples", Counter::AgeFullSamples);
    request_full_collection(h.g);
    cycle(&mut h);
    h.report_counter("full_vote", Counter::AgeUp);
    h.report_counter("full_threshold", Counter::AgeThreshold);
    h.report_counter("full_updates", Counter::AgeUpdates);
    h.report_counter("full_samples", Counter::AgeFullSamples);
    cycle(&mut h);
    h.report_counter("next_threshold", Counter::AgeThreshold);
    h.report_counter("updates", Counter::AgeUpdates);
    invariants(&mut h);
}
#[test]
fn production_forced_full_preserves_normal_tenuring_evidence_without_training_it() {
    for worker in [false, true] {
        let o = run(interrupted_normal_samples, 0, worker);
        assert_eq!(o.get("full_samples"), o.get("before_full_samples") + 1);
        let threshold = o.get("before_threshold");
        assert!((2..15).contains(&threshold), "{}", o.text());
        assert_eq!(o.get("full_threshold"), threshold, "{}", o.text());
        assert_eq!(
            o.get("full_updates"),
            o.get("before_updates"),
            "{}",
            o.text()
        );
        assert_eq!(o.get("next_threshold"), threshold + 1, "{}", o.text());
        assert_eq!(
            o.get("updates"),
            o.get("before_updates") + 1,
            "{}",
            o.text()
        );
        for (name, expected) in [
            ("before_vote", 1),
            ("full_vote", 1),
            ("id", 1),
            ("value", 84),
            ("unmarked", 0),
            ("young_unmarked", 0),
        ] {
            assert_eq!(o.get(name), expected, "{name}: {}", o.text());
        }
    }
}
