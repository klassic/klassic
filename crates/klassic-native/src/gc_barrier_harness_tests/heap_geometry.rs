//! Configured physical extent is shared by selection and reference validation.
use super::{H, run, run_failure};
use crate::gc_codegen::production::test_support::{
    GENERATION_YOUNG, PoolMetric, generation_matches, pool_claim, pool_metric, pool_role,
};
use crate::portable_asm::{PortableAsm, Reg as R};
fn extent(g: &mut crate::NativeCodeGenerator, extra: u64) {
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    h.report("young_end", |h| pool_metric(h.g, PoolMetric::YoungEnd));
    h.report("old", |h| pool_metric(h.g, PoolMetric::OldCapacity));
    pool_claim(h.g, 65, GENERATION_YOUNG);
    h.keep(20);
    h.report("tail", |h| h.g.load_rbp_slot(R::V0, super::slot_offset(20)));
    h.report("last_role", |h| pool_role(h.g, 127));
    h.report("inside", |h| {
        generation_matches(h.g, 128 * 131072, GENERATION_YOUNG)
    });
    crate::gc_codegen::production::test_support::poison_next_generation(h.g);
    h.report("outside", |h| {
        generation_matches(h.g, 129 * 131072, GENERATION_YOUNG)
    });
    if extra != 0 {
        pool_claim(h.g, 1, GENERATION_YOUNG);
    }
}
#[test]
fn production_heap_geometry_claims_the_last_configured_page_and_rejects_the_next() {
    let o = run(extent, 0, false);
    for (name, value) in [
        ("young_end", 21),
        ("old", 42),
        ("tail", 63),
        ("last_role", 1),
        ("inside", 1),
        ("outside", 0),
    ] {
        assert_eq!(o.get(name), value, "{}", o.text());
    }
    let o = run_failure(extent, 1);
    assert_eq!(o.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&o.stderr);
    assert!(stderr.contains("Young run capacity exhausted"), "{stderr}");
}

fn small_moves(g: &mut crate::NativeCodeGenerator, _arg: u64) {
    use super::Src;
    use crate::gc_codegen::production::{MAX_PAYLOAD, test_support::request_old_cycle};
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    h.alloc(1, MAX_PAYLOAD, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    for slot in 1..17 {
        h.alloc(2, MAX_PAYLOAD, slot);
        h.store(Src::Raw(slot), 0, Src::Root(slot - 1), true);
        h.root(slot);
    }
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    // Leave a Young child reachable solely through an Old field. Exercise
    // the configured bitmap/layout before moving both generations twice.
    h.alloc(1, 16, 20);
    h.set_scalar(Src::Raw(20), 0, 84);
    h.root(20);
    h.store(Src::Root(1), 0, Src::Root(20), true);
    h.pop_root();
    h.collect();
    h.finish_collection();
    for pass in 0..2 {
        request_old_cycle(h.g);
        h.collect();
        h.finish_collection();
        h.report(&format!("value{pass}"), |h| {
            h.value(Src::Root(16));
            h.keep(30);
            for _ in 0..16 {
                h.load_barriered(Src::Raw(30), 0);
                h.keep(30);
            }
            h.load(Src::Raw(30), 0);
        });
        h.report(&format!("id{pass}"), |h| h.object_id(Src::Root(16)));
    }
}
#[test]
fn production_heap_geometry_preserves_a_live_chain_through_repeated_small_heap_full_moves() {
    let o = run(small_moves, 0, false);
    for pass in 0..2 {
        assert_eq!(o.get(&format!("value{pass}")), 84, "{}", o.text());
        assert_eq!(o.get(&format!("id{pass}")), 17);
    }
}
