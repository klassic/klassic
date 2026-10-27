//! Real weak handles over production roots, mark queues and moving metadata.
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
    weak_call(h.g, handle, 0);
    lease_require_ok(h.g);
    handle
}
fn load(h: &mut H<'_>, handle: crate::DataLabel, root: u32) {
    h.g.load_rbp_slot(R::V6, slot_offset(root));
    weak_call(h.g, handle, 1);
}
fn basic(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    empty(&mut h, 1);
    let handle = create(&mut h, 0);
    load(&mut h, handle, 0);
    h.keep(9);
    h.report("nonempty_output", |h| h.value(Src::Raw(9)));
    h.g.mov_imm64(R::V6, 0);
    weak_call(h.g, handle, 1);
    h.keep(9);
    h.report("null_output", |h| h.value(Src::Raw(9)));
    load(&mut h, handle, 1);
    h.keep(9);
    h.report("load", |h| h.value(Src::Raw(9)));
    h.report("value", |h| h.load(Src::Root(1), 0));
    h.report("owned", |h| lease_metric(h.g, 0));
    let stale = lease_request(h.g);
    lease_copy(h.g, handle, stale);
    weak_call(h.g, handle, 2);
    h.keep(9);
    h.report("release", |h| h.value(Src::Raw(9)));
    weak_call(h.g, stale, 2);
    h.keep(9);
    h.report("stale", |h| h.value(Src::Raw(9)));
    let replacement = create(&mut h, 0);
    load(&mut h, stale, 1);
    h.keep(9);
    h.report("reused_stale", |h| h.value(Src::Raw(9)));
    weak_call(h.g, replacement, 2);
    lease_require_ok(h.g);
    h.report("empty", |h| lease_metric(h.g, 0));
    h.report("active", |h| weak_metric(h.g, 0));
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0);
    let null_handle = create(&mut h, 0);
    empty(&mut h, 2);
    load(&mut h, null_handle, 2);
    h.keep(9);
    h.report("null_load", |h| h.value(Src::Raw(9)));
    weak_call(h.g, null_handle, 2);
    lease_require_ok(h.g);
    h.report("null_leases", |h| lease_metric(h.g, 0));
}
#[test]
fn production_weak_handles_publish_registered_roots_and_reject_released_serials() {
    let o = run(basic, 0, false);
    assert_eq!(o.get("load"), 0, "{}", o.text());
    assert_eq!(o.get("value"), 77);
    assert_eq!(o.get("nonempty_output"), 3);
    assert_eq!(o.get("null_output"), 3);
    assert_eq!(o.get("owned"), 1);
    assert_eq!(o.get("release"), 0);
    assert_eq!(o.get("stale"), 3);
    assert_eq!(o.get("reused_stale"), 3);
    assert_eq!(o.get("empty"), 0);
    assert_eq!(o.get("active"), 0);
    assert_eq!(o.get("null_load"), 1);
    assert_eq!(o.get("null_leases"), 0);
}
fn lifecycle(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let dead = arg & 1 != 0;
    let old = arg & 2 != 0;
    let large = arg & 4 != 0;
    let mut h = H::begin(g);
    h.alloc(1, if large { 400_000 } else { 16 }, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    if old && !large {
        h.collect();
        h.finish_collection();
        h.collect();
        h.finish_collection();
    }
    empty(&mut h, 1);
    let handle = create(&mut h, 0);
    if dead {
        h.g.mov_imm64(R::V0, 0);
        h.set_root(0);
    }
    if old || large {
        request_full_collection(h.g);
    }
    h.collect();
    h.finish_collection();
    load(&mut h, handle, 1);
    h.keep(9);
    h.report("load", |h| h.value(Src::Raw(9)));
    h.report("leases", |h| lease_metric(h.g, 0));
    h.report("gate", |h| weak_metric(h.g, 1));
    h.report("cleared", |h| weak_metric(h.g, 2));
    h.report("rebound", |h| weak_metric(h.g, 3));
    if !dead {
        h.report("value", |h| h.load(Src::Root(1), 0));
    }
    weak_call(h.g, handle, 2);
    lease_require_ok(h.g);
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_weak_references_clear_dead_and_relocate_live_young_old_large_targets() {
    for worker in [false, true] {
        for mode in [0, 1, 2, 3, 4, 5] {
            let o = run(lifecycle, mode, worker);
            let dead = mode & 1 != 0;
            assert_eq!(
                o.get("load"),
                i64::from(dead),
                "mode={mode} worker={worker} {}",
                o.text()
            );
            assert_eq!(o.get("leases"), i64::from(!dead));
            assert_eq!(o.get("gate"), 0);
            assert_eq!(o.get("empty"), 0);
            if dead {
                assert!(o.get("cleared") > 0);
            } else {
                assert_eq!(o.get("value"), 77);
                if mode != 4 {
                    assert!(o.get("rebound") > 0);
                }
            }
        }
    }
}

fn keep_alive(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    h.alloc(2, 16, 1);
    h.root(1);
    h.store(Src::Root(1), 0, Src::Root(0), true);
    h.collect();
    h.finish_collection();
    h.collect();
    h.finish_collection();
    empty(&mut h, 2);
    let handle = create(&mut h, 1);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(1);
    h.mark_start();
    load(&mut h, handle, 2);
    h.keep(9);
    h.report("load", |h| h.value(Src::Raw(9)));
    h.end_marks();
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.load_barriered(Src::Root(2), 0);
    h.keep(3);
    h.report("child", |h| h.load(Src::Raw(3), 0));
    h.report("gate", |h| weak_metric(h.g, 1));
    weak_call(h.g, handle, 2);
    lease_require_ok(h.g);
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_weak_load_keeps_a_white_old_graph_alive_through_real_mark_termination() {
    let o = run(keep_alive, 0, false);
    assert_eq!(o.get("load"), 0, "{}", o.text());
    assert_eq!(o.get("child"), 77);
    assert_eq!(o.get("gate"), 0);
    assert_eq!(o.get("empty"), 0);
}
fn parallel(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let mut h = H::begin(g);
    publish_primary(h.g);
    let n = arg & 255;
    let full = arg & 256 != 0;
    let mut actors = Vec::new();
    let mut roots = Vec::new();
    let mut weak = Vec::new();
    for i in 0..n {
        let actor = create_actor(h.g, 16, 8);
        let root = native_completed_graph(h.g, actor, 42 + i);
        h.report(&format!("before{i}"), |h| {
            native_completed_root(h.g, actor, root, true)
        });
        weak.push(weak_actor_prepare(h.g, actor, root));
        copy_actor_native_enter(h.g, actor);
        actors.push(actor);
        roots.push(root);
    }
    let p = run_parallel_native_completed(h.g, &actors, &roots, 2, full, true);
    h.report("leases", |h| lease_metric(h.g, 0));
    for i in 0..n as usize {
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        native_completed_resume(h.g, actors[i]);
        h.report(&format!("weak{i}"), |h| {
            weak_actor_load_id(h.g, actors[i], weak[i].0, weak[i].1)
        });
    }
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_native_owners_keep_weak_handles_across_complete_collections_and_real_thread_returns()
{
    for full in [false, true] {
        for n in [1, 2, 8] {
            let o = run(parallel, n | if full { 256 } else { 0 }, true);
            assert_eq!(o.get("leases"), n as i64, "{}", o.text());
            assert_eq!(o.get("empty"), 0);
            let mut tids = std::collections::HashSet::new();
            for i in 0..n {
                assert!(tids.insert(o.get(&format!("tid{i}"))));
                assert_eq!(o.get(&format!("before{i}")), o.get(&format!("weak{i}")));
            }
        }
    }
}

fn capacity(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    let handles: Vec<_> = (0..128).map(|_| create(&mut h, 0)).collect();
    let extra = lease_request(h.g);
    weak_growth_map_fail_next(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    weak_call(h.g, extra, 0); // owns no handle; actual OS map fails during service
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    weak_call(h.g, extra, 0);
    h.keep(9);
    h.report("full", |h| h.value(Src::Raw(9)));
    h.report("owned", |h| lease_metric(h.g, 0));
    h.report("untouched", |h| lease_request_word(h.g, extra, 0));
    let stale = lease_request(h.g);
    lease_copy(h.g, handles[0], stale);
    weak_call(h.g, handles[0], 2);
    lease_require_ok(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    weak_call(h.g, extra, 0);
    lease_require_ok(h.g);
    weak_call(h.g, stale, 2);
    h.keep(9);
    h.report("stale", |h| h.value(Src::Raw(9)));
    for handle in handles.iter().skip(1).chain(std::iter::once(&extra)) {
        weak_call(h.g, *handle, 2);
        lease_require_ok(h.g);
    }
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_weak_growth_mapping_failure_preserves_ownership_and_reuse_rejects_old_serials() {
    let o = run(capacity, 0, false);
    assert_eq!(o.get("full"), 6, "{}", o.text());
    assert_eq!(o.get("owned"), 128);
    assert_eq!(o.get("untouched"), 0);
    assert_eq!(o.get("stale"), 3);
    assert_eq!(o.get("empty"), 0);
}
fn lease_group(h: &mut H<'_>, requests: crate::DataLabel, release: bool) {
    h.g.mov_imm64(R::V0, 0);
    h.g.store_rbp_slot(1856, R::V0);
    let again = h.g.create_text_label();
    h.g.bind_text_label(again);
    h.g.load_rbp_slot(R::V0, 1856);
    h.g.mov_reg_reg(R::V1, R::V0);
    h.g.shl_reg_imm8(R::V1, 1);
    h.g.add_reg_reg(R::V0, R::V1);
    h.g.shl_reg_imm8(R::V0, 3);
    h.g.mov_data_addr(R::V7, requests);
    h.g.add_reg_reg(R::V7, R::V0);
    h.g.load_rbp_slot(R::V0, slot_offset(1));
    let l = &h.g.gc_gen.as_ref().unwrap().labels;
    let entry = if release {
        l.metadata_lease_release_actor
    } else {
        l.metadata_lease_root_actor
    };
    call_owner(h.g, entry);
    lease_require_ok(h.g);
    h.g.load_rbp_slot(R::V0, 1856);
    h.g.add_reg_imm32(R::V0, 1);
    h.g.store_rbp_slot(1856, R::V0);
    h.g.cmp_reg_imm32(R::V0, 4095);
    h.g.jcc_label(crate::portable_asm::Condition::Below, again);
}
fn saturated(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    let handle = create(&mut h, 0);
    h.alloc(1, 400_000, 1);
    h.root(1);
    empty(&mut h, 2);
    let requests = h.g.asm.data_label_with_i64s(&vec![0; 4095 * 3]);
    lease_group(&mut h, requests, false);
    h.report("full", |h| lease_metric(h.g, 0));
    h.collect();
    h.finish_collection();
    h.report("after", |h| lease_metric(h.g, 0));
    load(&mut h, handle, 2);
    lease_require_ok(h.g);
    h.report("value", |h| h.load(Src::Root(2), 0));
    h.report("rebound", |h| weak_metric(h.g, 3));
    lease_group(&mut h, requests, true);
    weak_call(h.g, handle, 2);
    lease_require_ok(h.g);
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_weak_relocation_rebinds_its_funded_token_with_all_metadata_slots_occupied() {
    let o = run(saturated, 0, false);
    assert_eq!(o.get("full"), 4096, "{}", o.text());
    assert_eq!(o.get("after"), 4096);
    assert_eq!(o.get("value"), 77);
    assert!(o.get("rebound") > 0);
    assert_eq!(o.get("empty"), 0);
}

fn selected_fresh(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.mark_start();
    h.end_marks();
    h.alloc(1, 400_000, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    empty(&mut h, 1);
    let handle = create(&mut h, 0);
    load(&mut h, handle, 1);
    h.keep(9);
    h.report("load", |h| h.value(Src::Raw(9)));
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(1), 0));
    weak_call(h.g, handle, 2);
    lease_require_ok(h.g);
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_weak_load_preserves_fresh_large_objects_allocated_after_old_selection() {
    let o = run(selected_fresh, 0, false);
    assert_eq!(o.get("load"), 0, "{}", o.text());
    assert_eq!(o.get("value"), 77);
    assert_eq!(o.get("empty"), 0);
}

fn old_closure(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    empty(&mut h, 1);
    let handle = create(&mut h, 0);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0);
    h.mark_start();
    h.end_marks();
    load(&mut h, handle, 1);
    h.keep(9);
    h.report("load", |h| h.value(Src::Raw(9)));
    h.report("cleared", |h| weak_metric(h.g, 2));
    h.report("leases", |h| lease_metric(h.g, 0));
    weak_call(h.g, handle, 2);
    lease_require_ok(h.g);
}
#[test]
fn production_old_weak_closure_clears_young_targets_before_their_own_mark_cycle() {
    let o = run(old_closure, 0, false);
    assert_eq!(o.get("load"), 1, "{}", o.text());
    assert_eq!(o.get("cleared"), 1);
    assert_eq!(o.get("leases"), 0);
}
