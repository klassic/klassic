//! Real dynamic token storage and migration interleavings.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn live_weak(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    h.g.mov_imm64(R::V0, 0);
    h.keep(1);
    h.root(1);
    let handles: Vec<_> = (0..5000).map(|_| lease_request(h.g)).collect();
    for handle in &handles {
        weak_growth_create(h.g, *handle, slot_offset(0));
    }
    h.report("capacity", |h| lease_growth_metric(h.g, 0));
    h.report("slots", |h| weak_growth_metric(h.g, 0));
    h.report("active", |h| lease_metric(h.g, 0));
    let saved = lease_request(h.g);
    lease_copy(h.g, handles[4999], saved);
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.g.load_rbp_slot(R::V6, slot_offset(1));
    weak_call(h.g, handles[4999], 1);
    lease_require_ok(h.g);
    h.report("value", |h| h.load(Src::Root(1), 0));
    h.report("rebound", |h| weak_metric(h.g, 3));
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(1);
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("dead_tokens", |h| lease_metric(h.g, 0));
    for handle in &handles {
        weak_call(h.g, *handle, 2);
        lease_require_ok(h.g);
    }
    weak_call(h.g, saved, 2);
    h.keep(9);
    h.report("stale", |h| h.value(Src::Raw(9)));
    h.report("empty", |h| weak_metric(h.g, 0));
}
#[test]
fn production_metadata_lease_growth_funds_5000_live_weak_handles_and_rebinds_after_full_gc() {
    for worker in [false, true] {
        let o = run(live_weak, 0, worker);
        for name in ["capacity", "slots"] {
            assert_eq!(o.get(name), 8192, "{}", o.text());
        }
        assert_eq!(o.get("active"), 5000);
        assert_eq!(o.get("value"), 77);
        assert!(o.get("rebound") >= 5000, "{}", o.text());
        for name in ["dead_tokens", "empty"] {
            assert_eq!(o.get(name), 0, "{}", o.text());
        }
        assert_eq!(o.get("stale"), 3);
    }
}
fn set_index(h: &mut H<'_>, index: u64) {
    h.g.mov_imm64(R::V0, index);
    h.g.store_rbp_slot(1800, R::V0);
}
fn array(h: &mut H<'_>, requests: crate::DataLabel, op: u64) {
    lease_growth_array_call(h.g, requests, slot_offset(0), op);
}
fn tokens(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    let requests = h.g.asm.data_label_with_i64s(&vec![0; 8193 * 3]);
    lease_growth_group(h.g, requests, slot_offset(0), 0, 4096, false);
    weak_growth_map_fail_next(h.g);
    set_index(&mut h, 4096);
    array(&mut h, requests, 0);
    h.keep(9);
    h.report("failed_status", |h| h.value(Src::Raw(9)));
    h.report("map_failures", |h| lease_growth_metric(h.g, 4));
    h.report("failed_token", |h| {
        lease_request_word(h.g, requests, 4096 * 3 + 2)
    });
    array(&mut h, requests, 0); // consume the failed-map status without new map
    lease_growth_pause(h.g, true);
    array(&mut h, requests, 0); // actual candidate map
    lease_growth_pause(h.g, false);
    lease_growth_tick(h.g);
    lease_growth_pause(h.g, true);
    h.report("copied", |h| lease_growth_metric(h.g, 2));
    let stale = lease_request(h.g);
    lease_copy(h.g, requests, stale);
    set_index(&mut h, 0);
    array(&mut h, requests, 1);
    lease_require_ok(h.g);
    array(&mut h, requests, 0);
    lease_require_ok(h.g);
    lease_growth_pause(h.g, false);
    h.repeat(12290, |h| lease_growth_tick(h.g));
    h.report("first_capacity", |h| lease_growth_metric(h.g, 0));
    lease_call(h.g, stale, 2);
    h.keep(9);
    h.report("stale", |h| h.value(Src::Raw(9)));
    lease_growth_group(h.g, requests, slot_offset(0), 4096, 8192, false);
    h.report("old_before", |h| lease_growth_metric(h.g, 11));
    set_index(&mut h, 8192);
    array(&mut h, requests, 0);
    h.repeat(24580, |h| lease_growth_tick(h.g));
    h.report("capacity", |h| lease_growth_metric(h.g, 0));
    h.report("old", |h| lease_growth_metric(h.g, 8));
    h.report("charge_before", |h| weak_growth_committed(h.g));
    native_root_unmap_fail_next(h.g);
    lease_growth_service(h.g);
    h.report("retained", |h| lease_growth_metric(h.g, 1));
    h.report("failed_old", |h| lease_growth_metric(h.g, 8));
    h.report("charge_failed", |h| weak_growth_committed(h.g));
    lease_growth_service(h.g);
    h.report("released", |h| lease_growth_metric(h.g, 7));
    h.report("charge_after", |h| weak_growth_committed(h.g));
    lease_growth_group(h.g, requests, slot_offset(0), 8192, 8193, false);
    h.report("active", |h| lease_metric(h.g, 0));
    lease_growth_group(h.g, requests, slot_offset(0), 0, 8193, true);
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_metadata_lease_growth_preserves_generation_tokens_and_actual_failed_mapping_ownership()
 {
    let o = run(tokens, 0, false);
    assert_eq!(o.get("failed_status"), 2, "{}", o.text());
    assert_eq!(o.get("map_failures"), 1);
    assert_eq!(o.get("failed_token"), 0);
    assert_eq!(o.get("copied"), 1);
    assert_eq!(o.get("first_capacity"), 8192);
    assert_eq!(o.get("stale"), 1);
    assert_eq!(o.get("capacity"), 16384);
    assert_eq!(o.get("old"), o.get("old_before"));
    assert_eq!(o.get("failed_old"), o.get("old"));
    assert_eq!(o.get("retained"), 5);
    assert_eq!(o.get("charge_failed"), o.get("charge_before"));
    assert_eq!(o.get("released"), 1);
    assert_eq!(o.get("charge_before") - o.get("charge_after"), 8192 * 32);
    assert_eq!(o.get("active"), 8193);
    assert_eq!(o.get("empty"), 0);
}
