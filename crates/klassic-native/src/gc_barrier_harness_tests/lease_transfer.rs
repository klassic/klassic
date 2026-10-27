//! Retarget actual source metadata ownership before resuming retirement.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn transfer(h: &mut H<'_>, r: crate::DataLabel, root: u32) {
    h.g.load_rbp_slot(R::V0, slot_offset(root));
    reference_transfer(h.g, r, false);
}
fn lifecycle(g: &mut crate::NativeCodeGenerator, kind: u64) {
    let mut h = H::begin(g);
    h.alloc(1, if kind == 2 { 400_000 } else { 16 }, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    if kind == 1 {
        h.collect();
        h.finish_collection();
        h.collect();
        h.finish_collection();
    }
    let r = reference_lease(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    reference_acquire(h.g, r);
    h.report("generation_before", |h| reference_generation(h.g, r));
    h.report("page_before", |h| lease_request_word(h.g, r, 1));
    h.report("token_before", |h| lease_request_word(h.g, r, 3));
    if kind != 0 {
        request_full_collection(h.g);
    }
    h.collect();
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.close_relocation);
    emit_budget(h.g, 10_000);
    h.g.call_label(h.l.retire_step);
    h.g.call_label(h.l.unlock);
    h.report("waits", |h| lease_metric(h.g, 1));
    transfer(&mut h, r, 0);
    h.keep(9);
    h.report("transfer", |h| h.value(Src::Raw(9)));
    h.report("generation_after", |h| reference_generation(h.g, r));
    h.report("page_after", |h| lease_request_word(h.g, r, 1));
    h.report("token_after", |h| lease_request_word(h.g, r, 3));
    h.report("active", |h| lease_metric(h.g, 0));
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report_counter("stage", Counter::RetireStage);
    transfer(&mut h, r, 0);
    h.keep(9);
    h.report("same", |h| h.value(Src::Raw(9)));
    h.report("token_same", |h| lease_request_word(h.g, r, 3));
    reference_transfer(h.g, r, true);
    h.keep(9);
    h.report("release", |h| h.value(Src::Raw(9)));
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_reference_metadata_transfers_young_old_and_nonmoving_large_ownership_through_retire()
{
    for kind in [0, 1, 2] {
        let o = run(lifecycle, kind, false);
        assert_eq!(o.get("transfer"), 0, "{}", o.text());
        assert_eq!(o.get("generation_before"), i64::from(kind != 0));
        assert_eq!(o.get("generation_after"), i64::from(kind != 0));
        assert_eq!(o.get("active"), 1);
        assert_eq!(o.get("value"), 77);
        assert_eq!(o.get("stage"), 0);
        assert_eq!(o.get("same"), 0);
        assert_eq!(o.get("token_same"), o.get("token_after"));
        assert_eq!(o.get("release"), 0);
        assert_eq!(o.get("empty"), 0);
        if kind != 2 {
            assert!(o.get("waits") > 0);
            assert_ne!(o.get("page_before"), o.get("page_after"));
            assert_ne!(o.get("token_before"), o.get("token_after"));
        } else {
            assert_eq!(o.get("page_before"), o.get("page_after"));
            assert_eq!(o.get("token_before"), o.get("token_after"));
        }
    }
}
fn invalid(g: &mut crate::NativeCodeGenerator, interior: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 32, 0);
    h.root(0);
    let r = reference_lease(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    reference_acquire(h.g, r);
    h.report("before", |h| lease_request_word(h.g, r, 3));
    h.alloc(1, 16, 1);
    h.root(1);
    if interior == 1 {
        h.g.mov_data_addr(R::V1, r);
        h.g.load_ptr_disp32(R::V0, R::V1, 0);
        h.g.add_reg_imm32(R::V0, 16 << 16);
        h.g.store_ptr_disp32(R::V1, 0, R::V0);
    }
    if interior == 2 {
        h.g.mov_imm64(R::V0, 0);
        h.set_root(1);
    } else if interior == 3 {
        let external = h.g.asm.data_label_with_i64s(&[0; 4]);
        h.g.mov_data_addr(R::V0, external);
        h.set_root(1);
    }
    transfer(&mut h, r, if interior == 1 { 0 } else { 1 });
    h.keep(9);
    h.report("status", |h| h.value(Src::Raw(9)));
    h.report("after", |h| lease_request_word(h.g, r, 3));
    h.report("active", |h| lease_metric(h.g, 0));
    reference_transfer(h.g, r, true);
    h.keep(9);
    h.report("release", |h| h.value(Src::Raw(9)));
}
#[test]
fn production_reference_metadata_transfer_rejects_other_objects_and_interior_words_without_losing_ownership()
 {
    for mode in [0, 1, 2, 3] {
        let o = run(invalid, mode, false);
        assert_eq!(o.get("status"), 1, "{}", o.text());
        assert_eq!(o.get("before"), o.get("after"));
        assert_eq!(o.get("active"), 1);
        assert_eq!(o.get("release"), 0);
    }
}

fn parallel(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let mut h = H::begin(g);
    publish_primary(h.g);
    let n = arg & 255;
    let full = arg & 256 != 0;
    let mut actors = Vec::new();
    let mut roots = Vec::new();
    let mut requests = Vec::new();
    for i in 0..n {
        let actor = create_actor(h.g, 16, 8);
        let root = native_completed_graph(h.g, actor, 42 + i);
        h.report(&format!("before{i}"), |h| {
            native_completed_root(h.g, actor, root, true)
        });
        copy_actor_native_enter(h.g, actor);
        actors.push(actor);
        roots.push(root);
        requests.push(reference_lease(h.g));
    }
    if full {
        native_completed_collect(h.g);
        native_completed_collect(h.g);
    }
    for i in 0..n as usize {
        native_completed_resume(h.g, actors[i]);
        reference_actor_acquire(h.g, actors[i], roots[i], requests[i]);
        copy_actor_native_enter(h.g, actors[i]);
    }
    let p = run_parallel_metadata_transfers(h.g, &actors, &roots, &requests, full);
    h.report("active", |h| lease_metric(h.g, 0));
    h.report("waits", |h| lease_metric(h.g, 1));
    h.report_counter("stage", Counter::RetireStage);
    for (i, request) in requests.iter().enumerate() {
        h.report(&format!("after{i}"), |h| {
            parallel_copy_result(h.g, &p, i, 1)
        });
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        h.report(&format!("generation{i}"), |h| {
            reference_generation(h.g, *request)
        });
        h.report(&format!("value{i}"), |h| {
            native_completed_metric(h.g, &p, i, 64)
        });
        h.report(&format!("fresh{i}"), |h| {
            native_completed_metric(h.g, &p, i, 72)
        });
        native_completed_resume(h.g, actors[i]);
        reference_actor_release(h.g, actors[i], *request);
    }
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_native_owners_transfer_source_metadata_to_real_young_and_old_destinations() {
    for full in [false, true] {
        for n in [1, 2, 8] {
            let o = run(parallel, n | if full { 256 } else { 0 }, true);
            assert_eq!(o.get("active"), n as i64, "{}", o.text());
            assert!(o.get("waits") > 0);
            assert_eq!(o.get("stage"), 0);
            assert_eq!(o.get("empty"), 0);
            let mut tids = std::collections::HashSet::new();
            for i in 0..n {
                assert!(tids.insert(o.get(&format!("tid{i}"))));
                assert_eq!(o.get(&format!("after{i}")), o.get(&format!("before{i}")));
                assert_eq!(o.get(&format!("generation{i}")), i64::from(full));
                assert_eq!(o.get(&format!("value{i}")), 42 + i as i64);
                assert_eq!(o.get(&format!("fresh{i}")), 99);
            }
        }
    }
}

fn group(h: &mut H<'_>, requests: crate::DataLabel, count: u64, release: bool) {
    let l = &h.g.gc_gen.as_ref().unwrap().labels;
    let entry = if release {
        l.metadata_lease_release_actor
    } else {
        l.metadata_lease_root_actor
    };
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
    call_owner(h.g, entry);
    lease_require_ok(h.g);
    h.g.load_rbp_slot(R::V0, 1856);
    h.g.add_reg_imm32(R::V0, 1);
    h.g.store_rbp_slot(1856, R::V0);
    h.g.cmp_reg_imm32(R::V0, count as i32);
    h.g.jcc_label(crate::portable_asm::Condition::Below, again);
}
fn exhausted(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    let r = reference_lease(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    reference_acquire(h.g, r);
    h.report("reference_before", |h| lease_request_word(h.g, r, 0));
    h.report("token_before", |h| lease_request_word(h.g, r, 3));
    h.alloc(1, 400_000, 1);
    h.root(1);
    h.g.asm
        .data
        .resize(h.g.asm.data.len().next_multiple_of(8), 0);
    let requests = h.g.asm.data_label_with_i64s(&vec![0; 4095 * 3]);
    group(&mut h, requests, 4095, false);
    h.collect();
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.close_relocation);
    emit_budget(h.g, 10_000);
    h.g.call_label(h.l.retire_step);
    h.g.call_label(h.l.unlock);
    transfer(&mut h, r, 0);
    h.keep(9);
    h.report("full", |h| h.value(Src::Raw(9)));
    h.report("reference_failed", |h| lease_request_word(h.g, r, 0));
    h.report("token_failed", |h| lease_request_word(h.g, r, 3));
    h.report("active_full", |h| lease_metric(h.g, 0));
    let release =
        h.g.gc_gen
            .as_ref()
            .unwrap()
            .labels
            .metadata_lease_release_actor;
    h.g.mov_data_addr(R::V7, requests);
    h.g.add_reg_imm32(R::V7, 4094 * 24);
    call_owner(h.g, release);
    lease_require_ok(h.g);
    transfer(&mut h, r, 0);
    h.keep(9);
    h.report("retry", |h| h.value(Src::Raw(9)));
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("active_retry", |h| lease_metric(h.g, 0));
    group(&mut h, requests, 4094, true);
    reference_transfer(h.g, r, true);
    lease_require_ok(h.g);
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_reference_transfer_keeps_source_ownership_when_destination_lease_capacity_is_exhausted()
 {
    let o = run(exhausted, 0, false);
    assert_eq!(o.get("full"), 2, "{}", o.text());
    assert_eq!(o.get("reference_failed"), o.get("reference_before"));
    assert_eq!(o.get("token_failed"), o.get("token_before"));
    assert_eq!(o.get("active_full"), 4096);
    assert_eq!(o.get("retry"), 0);
    assert_eq!(o.get("value"), 77);
    assert_eq!(o.get("active_retry"), 4095);
    assert_eq!(o.get("empty"), 0);
}
