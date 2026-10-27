//! Real dynamic weak storage, bounded migration and owned OS retirement.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn create(h: &mut H<'_>, handle: crate::DataLabel) {
    weak_growth_create(h.g, handle, slot_offset(0));
}
fn empty(h: &mut H<'_>, slot: u32) {
    h.g.mov_imm64(R::V0, 0);
    h.keep(slot);
    h.root(slot);
}
fn load(h: &mut H<'_>, handle: crate::DataLabel) {
    h.g.load_rbp_slot(R::V6, slot_offset(1));
    weak_call(h.g, handle, 1);
}
fn nullable(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    empty(&mut h, 0);
    empty(&mut h, 1);
    let handles: Vec<_> = (0..5000).map(|_| lease_request(h.g)).collect();
    for handle in &handles {
        create(&mut h, *handle);
    }
    h.report("capacity", |h| weak_growth_metric(h.g, 0));
    h.report("grown", |h| weak_growth_metric(h.g, 5));
    h.report("retired", |h| weak_growth_metric(h.g, 7));
    h.report("active", |h| weak_metric(h.g, 0));
    h.report("leases", |h| lease_metric(h.g, 0));
    load(&mut h, handles[4999]);
    h.keep(9);
    h.report("null_load", |h| h.value(Src::Raw(9)));
    for handle in &handles {
        weak_call(h.g, *handle, 2);
        lease_require_ok(h.g);
    }
    h.report("empty", |h| weak_metric(h.g, 0));
    h.report("idle", |h| weak_growth_metric(h.g, 1));
}
#[test]
fn production_weak_table_grows_past_bootstrap_and_metadata_token_count_for_null_handles() {
    for worker in [false, true] {
        let o = run(nullable, 0, worker);
        assert_eq!(o.get("capacity"), 8192, "{}", o.text());
        assert_eq!(o.get("grown"), 6, "{}", o.text());
        assert_eq!(o.get("retired"), 5, "{}", o.text());
        assert_eq!(o.get("active"), 5000, "{}", o.text());
        assert_eq!(o.get("null_load"), 1, "{}", o.text());
        for name in ["leases", "empty", "idle"] {
            assert_eq!(o.get(name), 0, "{}", o.text());
        }
    }
}
fn live_migration(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    empty(&mut h, 1);
    let handles: Vec<_> = (0..256).map(|_| lease_request(h.g)).collect();
    for handle in &handles {
        create(&mut h, *handle);
    }
    h.report("first_capacity", |h| weak_growth_metric(h.g, 0));
    h.report("old_before", |h| weak_growth_metric(h.g, 11));
    weak_growth_pause(h.g, true);
    let extra = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    weak_call(h.g, extra, 0);
    h.keep(9);
    h.report("requested", |h| h.value(Src::Raw(9)));
    weak_growth_pause(h.g, false);
    weak_growth_tick(h.g); // copy slot 0 before its actual mutation
    weak_growth_pause(h.g, true);
    h.report("copied_before_mutation", |h| weak_growth_metric(h.g, 2));
    let stale = lease_request(h.g);
    lease_copy(h.g, handles[0], stale);
    weak_call(h.g, handles[0], 2);
    lease_require_ok(h.g);
    let replacement = lease_request(h.g);
    create(&mut h, replacement);
    // A real movement updates references and funded leases while the old
    // array is authoritative; its already-copied slot must be mirrored.
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    weak_growth_pause(h.g, false);
    h.repeat(600, |h| weak_growth_tick(h.g));
    h.report("capacity", |h| weak_growth_metric(h.g, 0));
    h.report("retained", |h| weak_growth_metric(h.g, 1));
    h.report("old_published", |h| weak_growth_metric(h.g, 8));
    h.report("charge_before", |h| weak_growth_committed(h.g));
    native_root_unmap_fail_next(h.g);
    weak_growth_service(h.g); // actual OS failure keeps old mapping/accounting
    h.report("failed_retained", |h| weak_growth_metric(h.g, 1));
    h.report("failed_old", |h| weak_growth_metric(h.g, 8));
    h.report("charge_failed", |h| weak_growth_committed(h.g));
    h.report("failures", |h| weak_growth_metric(h.g, 4));
    weak_growth_service(h.g);
    h.report("released", |h| weak_growth_metric(h.g, 7));
    h.report("old_final", |h| weak_growth_metric(h.g, 8));
    h.report("charge_final", |h| weak_growth_committed(h.g));
    create(&mut h, extra);
    weak_call(h.g, stale, 2);
    h.keep(9);
    h.report("stale", |h| h.value(Src::Raw(9)));
    load(&mut h, replacement);
    lease_require_ok(h.g);
    h.report("value", |h| h.load(Src::Root(1), 0));
    h.g.mov_imm64(R::V0, 0);
    h.set_root(1);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0);
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    load(&mut h, replacement);
    h.keep(9);
    h.report("dead", |h| h.value(Src::Raw(9)));
    for handle in handles.iter().skip(1).chain([&replacement, &extra]) {
        weak_call(h.g, *handle, 2);
        lease_require_ok(h.g);
    }
    h.report("active_final", |h| weak_metric(h.g, 0));
    h.report("leases_final", |h| lease_metric(h.g, 0));
}
#[test]
fn production_weak_growth_preserves_serials_mirrors_rebinds_and_retries_actual_old_mapping_release()
{
    let o = run(live_migration, 0, false);
    assert_eq!(o.get("first_capacity"), 256, "{}", o.text());
    assert_eq!(o.get("capacity"), 512, "{}", o.text());
    assert_eq!(o.get("requested"), 2, "{}", o.text());
    assert_eq!(o.get("copied_before_mutation"), 1, "{}", o.text());
    for name in ["retained", "failed_retained"] {
        assert_eq!(o.get(name), 5, "{}", o.text());
    }
    assert_eq!(o.get("old_before"), o.get("old_published"), "{}", o.text());
    assert_eq!(o.get("old_before"), o.get("failed_old"), "{}", o.text());
    assert_eq!(
        o.get("charge_before"),
        o.get("charge_failed"),
        "{}",
        o.text()
    );
    assert_eq!(
        o.get("charge_before") - o.get("charge_final"),
        16384,
        "{}",
        o.text()
    );
    assert_eq!(o.get("failures"), 1, "{}", o.text());
    assert_eq!(o.get("released"), 1, "{}", o.text());
    assert_eq!(o.get("stale"), 3, "{}", o.text());
    assert_eq!(o.get("value"), 77, "{}", o.text());
    assert_eq!(o.get("dead"), 1, "{}", o.text());
    for name in ["old_final", "active_final", "leases_final"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
}
