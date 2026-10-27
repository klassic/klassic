//! Actual SATB capacity handoff and first/second metadata-lease exhaustion.
use super::satb_fixture::run;
use super::{H, Src, slot_offset};
use crate::gc_codegen::production::test_support::*;

fn capacity(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 17 * 8, 0);
    h.root(0);
    for i in 0..17 {
        h.alloc(1, 16, i + 1);
        h.set_scalar(Src::Raw(i + 1), 0, 9200 + u64::from(i));
        h.store(Src::Root(0), i, Src::Raw(i + 1), true);
    }
    // Replace the allocator scratch root before the snapshot. Otherwise the
    // last child is an independent mark-start root rather than a white SATB
    // previous value, and the seventeenth store never fills the buffer.
    h.alloc(1, 16, 21);
    publish_primary(h.g);
    dual_mark_enable(h.g);
    let gate = satb_gates(h.g);
    dual_mark_entry(h.g, false, true);
    h.value(Src::Root(0));
    h.keep(20);
    h.report("leases_baseline", |h| lease_metric(h.g, 0));
    for i in 0..16 {
        h.store(Src::Raw(20), i, Src::Null, true);
    }
    h.report("count16", |h| satb_metric(h.g, 0));
    h.report("pending16", |h| satb_metric(h.g, 2));
    h.report("admitted16", |h| satb_metric(h.g, 3));
    h.report("flushed16", |h| satb_metric(h.g, 4));
    h.report("leases16", |h| lease_metric(h.g, 0));
    for i in 1..=17 {
        h.report(&format!("before{i}"), |h| h.marked(Src::Raw(i)));
    }
    // The seventeenth append must transfer exactly the full sixteen entries.
    // No poll/allocation/native transition intervenes between these stores.
    h.store(Src::Raw(20), 16, Src::Null, true);
    h.report("count17", |h| satb_metric(h.g, 0));
    h.report("pending17", |h| satb_metric(h.g, 2));
    h.report("admitted17", |h| satb_metric(h.g, 3));
    h.report("flushed17", |h| satb_metric(h.g, 4));
    h.report("leases17", |h| lease_metric(h.g, 0));
    for i in 1..=17 {
        h.report(&format!("overflow{i}"), |h| h.marked(Src::Raw(i)));
    }
    satb_poll(h.g);
    h.report("pending_after", |h| satb_metric(h.g, 2));
    h.report("flushed_after", |h| satb_metric(h.g, 4));
    h.report("leases_after", |h| lease_metric(h.g, 0));
    h.report("fallback", |h| satb_metric(h.g, 5));
    h.report("last_marked", |h| h.marked(Src::Raw(17)));
    worker_journal_open_gate(h.g, gate);
    satb_poll_until_closed(h.g);
    for i in 1..=17 {
        h.report(&format!("value{i}"), |h| h.load(Src::Raw(i), 0));
    }
}

#[test]
fn production_satb_capacity_flush_transfers_sixteen_before_admitting_seventeenth() {
    let (o, _image) = run(capacity, 0, true);
    assert_eq!(o.get("count16"), 16, "{}", o.text());
    assert_eq!(o.get("pending16"), 16);
    assert_eq!(o.get("admitted16"), 16);
    assert_eq!(o.get("flushed16"), 0);
    assert_eq!(o.get("leases16"), o.get("leases_baseline") + 32);
    for i in 1..=17 {
        assert_eq!(o.get(&format!("before{i}")), 0, "{}", o.text());
        assert_eq!(
            o.get(&format!("overflow{i}")),
            i64::from(i != 17),
            "{}",
            o.text()
        );
        assert_eq!(o.get(&format!("value{i}")), 9199 + i);
    }
    assert_eq!(o.get("count17"), 1);
    assert_eq!(o.get("pending17"), 1);
    assert_eq!(o.get("admitted17"), 17);
    assert_eq!(o.get("flushed17"), 16);
    assert_eq!(o.get("leases17"), o.get("leases_baseline") + 2);
    assert_eq!(o.get("pending_after"), 0);
    assert_eq!(o.get("flushed_after"), 17);
    assert_eq!(o.get("leases_after"), o.get("leases_baseline"));
    assert_eq!(o.get("fallback"), 0);
    assert_eq!(o.get("last_marked"), 1);
}

fn exhausted(g: &mut crate::NativeCodeGenerator, tokens: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 9401);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    // Keep the previous referent out of the allocator's scratch root at
    // mark-start; only the holder's field should retain it before deletion.
    h.alloc(1, 16, 5);
    publish_primary(h.g);
    dual_mark_enable(h.g);
    let gate = satb_gates(h.g);
    dual_mark_entry(h.g, false, true);
    h.value(Src::Root(0));
    h.keep(4);
    h.report("leases_baseline", |h| lease_metric(h.g, 0));
    let requests = h.g.asm.data_label_with_i64s(&vec![0; tokens as usize * 3]);
    lease_growth_group(h.g, requests, slot_offset(0), 0, tokens, false);
    h.report("capacity", |h| lease_growth_metric(h.g, 0));
    h.report("charged", |h| lease_metric(h.g, 0));
    h.report("before", |h| h.marked(Src::Raw(1)));
    // At4096 the first lease cannot be acquired. At4095 the owner lease
    // consumes the last token, so failure of the previous-value lease must
    // release that owner token before the synchronous marker handoff.
    h.store(Src::Raw(4), 0, Src::Null, true);
    h.report("after", |h| h.marked(Src::Raw(1)));
    h.report("charged_after", |h| lease_metric(h.g, 0));
    h.report("count", |h| satb_metric(h.g, 0));
    h.report("pending", |h| satb_metric(h.g, 2));
    h.report("admitted", |h| satb_metric(h.g, 3));
    h.report("flushed", |h| satb_metric(h.g, 4));
    h.report("fallback", |h| satb_metric(h.g, 5));
    lease_growth_group(h.g, requests, slot_offset(0), 0, tokens, true);
    h.report("leases_released", |h| lease_metric(h.g, 0));
    worker_journal_open_gate(h.g, gate);
    satb_poll_until_closed(h.g);
    h.report("value", |h| h.load(Src::Raw(1), 0));
}

#[test]
fn production_satb_lease_exhaustion_and_second_lease_failure_preserve_previous_value() {
    for tokens in [4096, 4095] {
        let (o, _image) = run(exhausted, tokens, true);
        assert_eq!(o.get("capacity"), 4096, "{}", o.text());
        assert_eq!(o.get("charged"), tokens as i64, "{}", o.text());
        assert_eq!(o.get("before"), 0);
        assert_eq!(o.get("after"), 1);
        assert_eq!(o.get("charged_after"), o.get("charged"));
        for name in ["count", "pending", "admitted", "flushed"] {
            assert_eq!(o.get(name), 0, "{}", o.text());
        }
        assert_eq!(o.get("fallback"), 1);
        assert_eq!(o.get("leases_released"), o.get("leases_baseline"));
        assert_eq!(o.get("value"), 9401);
    }
}
