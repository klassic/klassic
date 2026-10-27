//! Non-null insertion protection and distinct prior values at one field.
use super::satb_fixture::run;
use super::{H, Src};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};

fn graph(g: &mut crate::NativeCodeGenerator) -> H<'_> {
    let mut h = H::begin(g);
    h.alloc(2, 24, 0);
    h.root(0);
    for child in 1..=3 {
        h.alloc(1, 16, child);
        h.set_scalar(Src::Raw(child), 0, 9700 + u64::from(child));
        h.store(Src::Root(0), child - 1, Src::Raw(child), true);
    }
    // All three values predate the snapshot and are reachable through the
    // holder, but none is an explicit or allocator-scratch snapshot root.
    h.alloc(1, 16, 20);
    publish_primary(h.g);
    dual_mark_enable(h.g);
    h
}
fn marks(h: &mut H<'_>, stage: &str, child: u32) {
    h.report(&format!("{stage}_old{child}"), |h| {
        h.marked(Src::Raw(child))
    });
    h.report(&format!("{stage}_young{child}"), |h| {
        h.value(Src::Raw(child));
        emit_young_marked(h.g);
    });
}
fn overwrite(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = graph(g);
    let gate = satb_gates(h.g);
    dual_mark_entry(h.g, false, true);
    dual_mark_entry(h.g, true, true);
    h.value(Src::Root(0));
    h.keep(4);
    for child in 1..=3 {
        marks(&mut h, "before", child);
    }
    h.report("baseline", |h| lease_metric(h.g, 0));
    h.store(Src::Raw(4), 0, Src::Raw(2), true);
    marks(&mut h, "first", 1);
    marks(&mut h, "first", 2);
    h.report("first_count", |h| satb_metric(h.g, 0));
    // B is already protected by the first store's insertion barrier. Its
    // deletion must preserve that ownership, not fabricate a new white B.
    h.store(Src::Raw(4), 0, Src::Raw(3), true);
    for child in 1..=3 {
        marks(&mut h, "second", child);
    }
    h.report("second_count", |h| satb_metric(h.g, 0));
    h.report("pending_old", |h| satb_metric(h.g, 2));
    h.report("pending_young", |h| satb_metric(h.g, 1));
    h.report("leases", |h| lease_metric(h.g, 0));
    h.report("admitted", |h| satb_metric(h.g, 3));
    satb_poll(h.g);
    marks(&mut h, "flushed", 1);
    h.report("after_count", |h| satb_metric(h.g, 0));
    h.report("after_leases", |h| lease_metric(h.g, 0));
    worker_journal_open_gate(h.g, gate);
    set_young_auto(h.g, true);
    satb_poll_until_closed(h.g);
    h.report("current", |h| h.deref_field(Src::Root(0), 0, 0));
    for child in 1..=3 {
        h.report(&format!("value{child}"), |h| h.load(Src::Raw(child), 0));
    }
}

#[test]
fn production_satb_nonnull_overwrites_protect_new_values_without_losing_white_previous() {
    let (o, _image) = run(overwrite, 0, true);
    for child in 1..=3 {
        for generation in ["old", "young"] {
            assert_eq!(
                o.get(&format!("before_{generation}{child}")),
                0,
                "{}",
                o.text()
            );
            assert_eq!(
                o.get(&format!("second_{generation}{child}")),
                i64::from(child != 1)
            );
        }
        assert_eq!(o.get(&format!("value{child}")), 9700 + child);
    }
    for generation in ["old", "young"] {
        assert_eq!(o.get(&format!("first_{generation}1")), 0);
        assert_eq!(o.get(&format!("first_{generation}2")), 1);
        assert_eq!(o.get(&format!("flushed_{generation}1")), 1);
    }
    assert_eq!(o.get("first_count"), 2);
    assert_eq!(o.get("second_count"), 2);
    assert_eq!(o.get("pending_old"), 1);
    assert_eq!(o.get("pending_young"), 1);
    assert_eq!(o.get("admitted"), 2);
    assert_eq!(o.get("leases"), o.get("baseline") + 4);
    assert_eq!(o.get("after_count"), 0);
    assert_eq!(o.get("after_leases"), o.get("baseline"));
    assert_eq!(o.get("current"), 9703);
}

fn epochs(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = graph(g);
    let gate = satb_gates(h.g);
    for previous in 1..=2 {
        // Reclose the same worker latch, not collector state or epoch stamps.
        // The preceding real Old cycle has completed before this write.
        h.g.mov_data_addr(R::V1, gate);
        h.g.mov_imm64(R::V0, 0);
        h.g.atomic_store_release(R::V1, R::V0);
        dual_mark_entry(h.g, false, true);
        h.value(Src::Root(0));
        h.keep(4);
        h.report_counter(&format!("epoch{previous}"), Counter::MarkCycles);
        h.report(&format!("before{previous}"), |h| {
            h.marked(Src::Raw(previous))
        });
        h.report(&format!("new_before{previous}"), |h| {
            h.marked(Src::Raw(previous + 1))
        });
        h.report(&format!("baseline{previous}"), |h| lease_metric(h.g, 0));
        h.store(Src::Raw(4), 0, Src::Raw(previous + 1), true);
        h.report(&format!("deferred{previous}"), |h| {
            h.marked(Src::Raw(previous))
        });
        h.report(&format!("new_after{previous}"), |h| {
            h.marked(Src::Raw(previous + 1))
        });
        h.report(&format!("pending{previous}"), |h| satb_metric(h.g, 2));
        h.report(&format!("admitted{previous}"), |h| satb_metric(h.g, 3));
        h.report(&format!("leases{previous}"), |h| lease_metric(h.g, 0));
        satb_poll(h.g);
        h.report(&format!("protected{previous}"), |h| {
            h.marked(Src::Raw(previous))
        });
        h.report(&format!("flushed{previous}"), |h| satb_metric(h.g, 4));
        h.report(&format!("released{previous}"), |h| lease_metric(h.g, 0));
        worker_journal_open_gate(h.g, gate);
        satb_poll_until_closed(h.g);
        h.report_counter(&format!("active{previous}"), Counter::MarkActive);
    }
    h.report("current", |h| h.deref_field(Src::Root(0), 0, 0));
}

#[test]
fn production_satb_same_slot_captures_distinct_previous_values_in_successive_real_epochs() {
    let (o, _image) = run(epochs, 0, true);
    assert_eq!(o.get("epoch2"), o.get("epoch1") + 1, "{}", o.text());
    for previous in 1..=2 {
        for name in ["before", "new_before", "deferred", "active"] {
            assert_eq!(o.get(&format!("{name}{previous}")), 0, "{}", o.text());
        }
        for name in ["new_after", "protected", "pending"] {
            assert_eq!(o.get(&format!("{name}{previous}")), 1);
        }
        assert_eq!(o.get(&format!("admitted{previous}")), previous);
        assert_eq!(o.get(&format!("flushed{previous}")), previous);
        assert_eq!(
            o.get(&format!("leases{previous}")),
            o.get(&format!("baseline{previous}")) + 2
        );
        assert_eq!(
            o.get(&format!("released{previous}")),
            o.get(&format!("baseline{previous}"))
        );
    }
    assert_eq!(o.get("current"), 9703);
}
