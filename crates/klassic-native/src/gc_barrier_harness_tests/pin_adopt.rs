use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn adopt(g: &mut crate::NativeCodeGenerator, snapshot: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    h.report("id_before", |h| h.object_id(Src::Root(0)));
    let span = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    pin_destination_call(h.g, span, false);
    lease_require_ok(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    pin_destination_watch(h.g, span);
    lease_require_ok(h.g);
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    pin_destination_call(h.g, span, true);
    lease_require_ok(h.g);
    h.report("address_before", |h| h.value(Src::Root(0)));
    let lease = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    lease_call(h.g, lease, 0);
    lease_require_ok(h.g);
    if snapshot != 0 {
        h.mark_start();
        h.drain_marks();
        if snapshot == 2 {
            h.end_marks();
        }
        h.report("protected_before", |h| old_protected_bytes(h.g));
    }
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    pin_adopt_root(h.g);
    lease_require_ok(h.g);
    if snapshot != 0 {
        h.report("protected_after", |h| old_protected_bytes(h.g));
        if snapshot == 1 {
            h.end_marks();
        }
    }
    h.report("address_adopted", |h| h.value(Src::Root(0)));
    h.report("large_pages", |h| pin_destination_metric(h.g, 3));
    h.report("adopted", |h| pin_adopt_metric(h.g));
    lease_call(h.g, lease, 2);
    lease_require_ok(h.g);
    lease_call(h.g, lease, 1);
    lease_require_ok(h.g);
    // Exercise ordinary reservation shrink before the next real selection:
    // the occupied adopted row must survive trimming beyond the cursor.
    pin_adopt_shrink(h.g);
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("address_moved", |h| h.value(Src::Root(0)));
    h.report("id_after", |h| h.object_id(Src::Root(0)));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_small_pin_adoption_charges_real_marked_footprint_in_active_and_valid_snapshots() {
    for snapshot in [1, 2] {
        let o = run(adopt, snapshot, false);
        assert_eq!(
            o.get("protected_after") - o.get("protected_before"),
            48,
            "{}",
            o.text()
        );
        assert_eq!(o.get("adopted"), 1);
        assert_ne!(o.get("address_adopted"), o.get("address_moved"));
        assert_eq!(o.get("id_before"), o.get("id_after"));
        assert_eq!(o.get("value"), 77);
    }
}
#[test]
fn production_final_small_pin_page_adopts_real_old_inventory_and_moves_after_unpin() {
    let o = run(adopt, 0, false);
    assert_eq!(o.get("adopted"), 1, "{}", o.text());
    assert_eq!(o.get("large_pages"), 0);
    assert_eq!(o.get("address_before"), o.get("address_adopted"));
    assert_ne!(o.get("address_adopted"), o.get("address_moved"));
    assert_eq!(o.get("id_before"), o.get("id_after"));
    assert_eq!(o.get("value"), 77);
}
