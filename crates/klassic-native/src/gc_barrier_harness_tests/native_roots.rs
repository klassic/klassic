//! Actual native roots, independent of frame roots, through moving collections.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn empty(h: &mut H<'_>, slot: u32) {
    h.g.mov_imm64(R::V0, 0);
    h.keep(slot);
    h.root(slot);
}
fn create(h: &mut H<'_>, root: u32) -> crate::DataLabel {
    let handle = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(root));
    native_root_call(h.g, handle, 0);
    lease_require_ok(h.g);
    handle
}
fn release(h: &mut H<'_>, handle: crate::DataLabel) {
    let again = h.g.create_text_label();
    let done = h.g.create_text_label();
    h.g.bind_text_label(again);
    native_root_call(h.g, handle, 2);
    h.g.cmp_reg_imm8(R::V0, 2);
    h.g.jcc_label(crate::portable_asm::Condition::NotEqual, done);
    let poll = h.g.gc_gen.as_ref().unwrap().labels.poll_actor;
    call_owner(h.g, poll);
    h.g.jmp_label(again);
    h.g.bind_text_label(done);
    lease_require_ok(h.g);
}
fn lifecycle(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, if arg == 2 { 400_000 } else { 16 }, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    if arg == 1 {
        for _ in 0..2 {
            h.collect();
            h.finish_collection();
        }
    }
    empty(&mut h, 1);
    let weak = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    weak_call(h.g, weak, 0);
    lease_require_ok(h.g);
    let handle = create(&mut h, 0);
    let copied = lease_request(h.g);
    lease_copy(h.g, handle, copied);
    native_root_call(h.g, copied, 2);
    h.keep(9);
    h.report("copied", |h| h.value(Src::Raw(9)));
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0);
    for _ in 0..2 {
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
    }
    h.g.load_rbp_slot(R::V6, slot_offset(1));
    native_root_call(h.g, handle, 1);
    lease_require_ok(h.g);
    h.report("value", |h| h.load(Src::Root(1), 0));
    h.report("active", |h| native_root_active(h.g));
    h.g.mov_imm64(R::V0, 0);
    h.set_root(1);
    release(&mut h, handle);
    native_root_call(h.g, copied, 2);
    h.keep(9);
    h.report("stale", |h| h.value(Src::Raw(9)));
    h.report("released", |h| native_root_active(h.g));
    for _ in 0..2 {
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
    }
    h.g.load_rbp_slot(R::V6, slot_offset(1));
    weak_call(h.g, weak, 1);
    h.keep(9);
    h.report("dead", |h| h.value(Src::Raw(9)));
    weak_call(h.g, weak, 2);
    lease_require_ok(h.g);
    h.report("leases", |h| lease_metric(h.g, 0));
    h.report("registrations", |h| {
        root_registry_metric(h.g, RootRegistryMetric::Active)
    });
}
#[test]
fn production_native_roots_own_young_old_and_large_objects_after_source_root_clears() {
    for arg in 0..3 {
        let o = run(lifecycle, arg, false);
        assert_eq!(o.get("value"), 77, "arg={arg}: {}", o.text());
        assert_eq!(o.get("active"), 1);
        assert_eq!(o.get("copied"), 3);
        assert_eq!(o.get("stale"), 3);
        assert_eq!(o.get("released"), 0);
        assert_eq!(o.get("dead"), 1);
        assert_eq!(o.get("leases"), 0);
        assert_eq!(o.get("registrations"), 1);
    }
}
fn capacity(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    let handles: Vec<_> = (0..128).map(|_| create(&mut h, 0)).collect();
    let extra = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    native_root_call(h.g, extra, 0);
    h.keep(9);
    h.report("full", |h| h.value(Src::Raw(9)));
    h.report("owned", |h| native_root_active(h.g));
    h.report("untouched", |h| lease_request_word(h.g, extra, 0));
    let stale = lease_request(h.g);
    lease_copy(h.g, handles[0], stale);
    release(&mut h, handles[0]);
    // Reuse the exact holder address and table slot; holder binding alone
    // cannot reject this stale serial. Restore the real token after probing.
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    native_root_call(h.g, handles[0], 0);
    lease_require_ok(h.g);
    let current = lease_request(h.g);
    lease_copy(h.g, handles[0], current);
    lease_copy(h.g, stale, handles[0]);
    native_root_call(h.g, handles[0], 2);
    h.keep(9);
    h.report("stale_serial", |h| h.value(Src::Raw(9)));
    lease_copy(h.g, current, handles[0]);
    for handle in &handles {
        release(&mut h, *handle);
    }
    h.report("empty", |h| native_root_active(h.g));
    h.report("registrations", |h| {
        root_registry_metric(h.g, RootRegistryMetric::Active)
    });
}
#[test]
fn production_native_root_capacity_and_recycling_preserve_registered_storage_ownership() {
    let o = run(capacity, 0, false);
    assert_eq!(o.get("full"), 6, "{}", o.text());
    assert_eq!(o.get("owned"), 128);
    assert_eq!(o.get("untouched"), 0);
    assert_eq!(o.get("stale_serial"), 3);
    assert_eq!(o.get("empty"), 0);
    assert_eq!(o.get("registrations"), 1);
}
fn cleanup(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    empty(&mut h, 1);
    let handle = create(&mut h, 0);
    let reader = native_root_reader(h.g, handle, None);
    native_root_call(h.g, handle, 2);
    h.keep(9);
    h.report("pending", |h| h.value(Src::Raw(9)));
    h.report("held", |h| native_root_active(h.g));
    h.g.load_rbp_slot(R::V6, slot_offset(1));
    native_root_call(h.g, handle, 1);
    h.keep(9);
    h.report("closing", |h| h.value(Src::Raw(9)));
    native_root_reader(h.g, handle, Some(reader));
    native_root_unmap_fail_next(h.g);
    native_root_call(h.g, handle, 2);
    h.keep(9);
    h.report("os_failure", |h| h.value(Src::Raw(9)));
    h.report("still_owned", |h| native_root_active(h.g));
    h.report("no_registration", |h| {
        root_registry_metric(h.g, RootRegistryMetric::Active)
    });
    release(&mut h, handle);
    h.report("released", |h| native_root_active(h.g));
    h.report("readers", |h| {
        root_registry_metric(h.g, RootRegistryMetric::Leases)
    });
}
#[test]
fn production_native_root_cleanup_waits_for_real_readers_and_retries_actual_unmap_failure() {
    let o = run(cleanup, 0, false);
    assert_eq!(o.get("pending"), 2, "{}", o.text());
    assert_eq!(o.get("held"), 1);
    assert_eq!(o.get("closing"), 3);
    assert_eq!(o.get("os_failure"), 6);
    assert_eq!(o.get("still_owned"), 1);
    assert_eq!(o.get("no_registration"), 1);
    assert_eq!(o.get("released"), 0);
    assert_eq!(o.get("readers"), 0);
}
fn parallel(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let mut h = H::begin(g);
    publish_primary(h.g);
    let n = arg & 255;
    let full = arg & 256 != 0;
    let mut actors = Vec::new();
    let mut roots = Vec::new();
    let mut handles = Vec::new();
    for i in 0..n {
        let actor = create_actor(h.g, 16, 8);
        let target = native_completed_graph(h.g, actor, 42 + i);
        h.report(&format!("before{i}"), |h| {
            native_completed_root(h.g, actor, target, true)
        });
        handles.push(native_root_actor_prepare(h.g, actor, target));
        // The existing native-thread driver observes a separate graph. The
        // target frame root is empty: only its independent native root owns it.
        let dummy = native_completed_graph(h.g, actor, 200 + i);
        copy_actor_native_enter(h.g, actor);
        actors.push(actor);
        roots.push(dummy);
    }
    native_root_call(h.g, handles[0].0, 2);
    h.keep(9);
    h.report("foreign", |h| h.value(Src::Raw(9)));
    let p = run_parallel_native_completed(h.g, &actors, &roots, 2, full, true);
    h.report("owned", |h| native_root_active(h.g));
    for i in 0..n as usize {
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        native_completed_resume(h.g, actors[i]);
        h.report(&format!("after{i}"), |h| {
            native_root_actor_load_id(h.g, actors[i], handles[i].0, handles[i].1)
        });
    }
    h.report("empty", |h| native_root_active(h.g));
}
#[test]
fn production_native_roots_retain_graphs_with_cleared_source_roots_on_real_native_threads() {
    for full in [false, true] {
        for n in [1, 2, 8] {
            let o = run(parallel, n | if full { 256 } else { 0 }, true);
            assert_eq!(o.get("owned"), n as i64, "{}", o.text());
            assert_eq!(o.get("foreign"), 3);
            assert_eq!(o.get("empty"), 0);
            let mut tids = std::collections::HashSet::new();
            for i in 0..n {
                assert!(tids.insert(o.get(&format!("tid{i}"))));
                assert_eq!(o.get(&format!("before{i}")), o.get(&format!("after{i}")));
            }
        }
    }
}
