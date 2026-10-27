//! Real source bytes/identity metadata survive until the token owner releases.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};

fn quantum(h: &mut H<'_>, units: u64) {
    h.g.call_label(h.l.lock);
    emit_budget(h.g, units);
    h.g.call_label(h.l.retire_step);
    h.g.call_label(h.l.unlock);
}
fn acquire(h: &mut H<'_>, r: crate::DataLabel, root: u32) {
    h.g.load_rbp_slot(R::V0, slot_offset(root));
    lease_call(h.g, r, 0);
}
fn held(g: &mut crate::NativeCodeGenerator, old: u64) {
    let mut h = H::begin(g);
    h.alloc(1, if old == 2 { 400_000 } else { 16 }, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    if old == 1 {
        h.collect();
        h.finish_collection();
        h.collect();
        h.finish_collection();
    }
    h.value(Src::Root(0));
    h.keep(8);
    h.g.load_ptr_disp32(R::V0, R::V0, -16);
    h.keep(7);
    let id = (std::mem::offset_of!(crate::gc_abi::MarkRecord, object_id) / 8) as u32;
    h.report("record_before", |h| h.load(Src::Raw(7), id));
    let r = lease_request(h.g);
    acquire(&mut h, r, 0);
    h.keep(9);
    h.report("acquire", |h| h.value(Src::Raw(9)));
    h.report("token", |h| lease_request_word(h.g, r, 2));
    h.pop_root(); // metadata retention is not a strong root
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 42);
    h.root(1);
    if old != 0 {
        request_full_collection(h.g);
    }
    h.collect();
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.close_relocation);
    h.g.call_label(h.l.unlock);
    quantum(&mut h, 0);
    h.report("units0", |h| lease_metric(h.g, 2));
    quantum(&mut h, 1);
    h.report("units1", |h| lease_metric(h.g, 2));
    quantum(&mut h, 10_000);
    h.report("record_held", |h| h.load(Src::Raw(7), id));
    h.report("held", |h| h.load(Src::Raw(8), 0));
    h.report("active", |h| lease_metric(h.g, 0));
    h.report("waits", |h| lease_metric(h.g, 1));
    h.report_counter("stage", Counter::RetireStage);
    lease_call(h.g, r, 2);
    h.keep(9);
    h.report("resolve", |h| h.value(Src::Raw(9)));
    let retry = lease_request(h.g);
    acquire(&mut h, retry, 1);
    h.keep(9);
    h.report("admission", |h| h.value(Src::Raw(9)));
    h.report("retry_token", |h| lease_request_word(h.g, retry, 2));
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 99);
    h.root(2);
    h.report("progress", |h| h.load(Src::Root(2), 0));
    lease_call(h.g, r, 1);
    h.keep(9);
    h.report("release", |h| h.value(Src::Raw(9)));
    h.finish_collection();
    h.report("cleared", |h| h.load(Src::Raw(8), 0));
    h.report("active_after", |h| lease_metric(h.g, 0));
    h.report("token_after", |h| lease_request_word(h.g, r, 2));
    h.report("live", |h| h.load(Src::Root(1), 0));
}
#[test]
fn production_metadata_leases_delay_young_and_full_old_source_cleanup_until_actual_release() {
    for old in [0, 1, 2] {
        let o = run(held, old, false);
        assert_eq!(o.get("acquire"), 0, "{}", o.text());
        assert!(o.get("token") > 0);
        assert_eq!(o.get("held"), 77);
        assert_eq!(o.get("record_held"), o.get("record_before"));
        assert_eq!(o.get("units0"), 0);
        assert_eq!(o.get("units1"), 1);
        assert_eq!(o.get("active"), 1);
        assert!(o.get("waits") > 0);
        assert!(o.get("stage") > 0);
        assert_eq!(o.get("resolve"), 0);
        assert_eq!(o.get("admission"), 4);
        assert_eq!(o.get("retry_token"), 0);
        assert_eq!(o.get("progress"), 99);
        assert_eq!(o.get("release"), 0);
        assert_eq!(o.get("cleared"), 0);
        assert_eq!(o.get("active_after"), 0);
        assert_eq!(o.get("token_after"), 0);
        assert_eq!(o.get("live"), 42);
    }
}

fn reuse(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    let r = lease_request(h.g);
    let stale = lease_request(h.g);
    acquire(&mut h, r, 0);
    h.keep(9);
    h.report("first", |h| h.value(Src::Raw(9)));
    h.report("token1", |h| lease_request_word(h.g, r, 2));
    lease_copy(h.g, r, stale);
    lease_call(h.g, r, 1);
    acquire(&mut h, r, 0);
    h.keep(9);
    h.report("second", |h| h.value(Src::Raw(9)));
    h.report("token2", |h| lease_request_word(h.g, r, 2));
    for (op, name) in [(1, "stale_release"), (2, "stale_resolve")] {
        lease_call(h.g, stale, op);
        h.keep(9);
        h.report(name, |h| h.value(Src::Raw(9)));
    }
    h.report("active", |h| lease_metric(h.g, 0));
    lease_call(h.g, r, 1);
    h.keep(9);
    h.report("released", |h| h.value(Src::Raw(9)));
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_metadata_lease_tokens_reject_released_slots_after_real_reuse() {
    let o = run(reuse, 0, false);
    assert_eq!(o.get("first"), 0, "{}", o.text());
    assert_eq!(o.get("second"), 0);
    assert!(o.get("token2") > o.get("token1"));
    assert_eq!(o.get("token1") & 0xffff_ffff, o.get("token2") & 0xffff_ffff);
    assert_eq!(o.get("stale_release"), 1);
    assert_eq!(o.get("stale_resolve"), 1);
    assert_eq!(o.get("active"), 1);
    assert_eq!(o.get("released"), 0);
    assert_eq!(o.get("empty"), 0);
}

fn unrelated(g: &mut crate::NativeCodeGenerator, destination: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    if destination == 0 {
        h.collect();
        h.finish_collection();
        h.collect();
        h.finish_collection();
    } else {
        h.collect();
    }
    let r = lease_request(h.g);
    acquire(&mut h, r, 0);
    h.keep(9);
    h.report("acquire", |h| h.value(Src::Raw(9)));
    if destination == 0 {
        h.collect();
    }
    h.finish_collection();
    h.report("active", |h| lease_metric(h.g, 0));
    h.report("waits", |h| lease_metric(h.g, 1));
    h.report("value", |h| h.load(Src::Root(0), 0));
    lease_call(h.g, r, 1);
    h.keep(9);
    h.report("release", |h| h.value(Src::Raw(9)));
}
#[test]
fn production_retained_old_and_current_destination_metadata_leases_do_not_block_young_cleanup() {
    for destination in [0, 1] {
        let o = run(unrelated, destination, false);
        assert_eq!(o.get("acquire"), 0, "{}", o.text());
        assert_eq!(o.get("active"), 1);
        assert_eq!(o.get("waits"), 0);
        assert_eq!(o.get("value"), 77);
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
        requests.push(lease_request(h.g));
    }
    if full {
        native_completed_collect(h.g);
        native_completed_collect(h.g);
    }
    for i in 0..n as usize {
        native_completed_resume(h.g, actors[i]);
        lease_actor_acquire(h.g, actors[i], roots[i], requests[i]);
        copy_actor_native_enter(h.g, actors[i]);
    }
    let p = run_parallel_metadata_leases(h.g, &actors, &roots, &requests, full);
    h.report("active", |h| lease_metric(h.g, 0));
    h.report("waits", |h| lease_metric(h.g, 1));
    h.report_counter("stage", Counter::RetireStage);
    for (i, request) in requests.iter().enumerate() {
        h.report(&format!("after{i}"), |h| {
            parallel_copy_result(h.g, &p, i, 1)
        });
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        h.report(&format!("token{i}"), |h| {
            lease_request_word(h.g, *request, 2)
        });
        h.report(&format!("value{i}"), |h| {
            native_completed_metric(h.g, &p, i, 64)
        });
        h.report(&format!("fresh{i}"), |h| {
            native_completed_metric(h.g, &p, i, 72)
        });
    }
}
#[test]
fn production_one_two_and_eight_native_owners_release_actual_source_leases_after_young_and_old_moves()
 {
    for full in [false, true] {
        for n in [1, 2, 8] {
            let o = run(parallel, n | if full { 256 } else { 0 }, true);
            assert_eq!(o.get("active"), 0, "{}", o.text());
            assert!(o.get("waits") > 0);
            assert_eq!(o.get("stage"), 0);
            let mut tids = std::collections::HashSet::new();
            for i in 0..n {
                assert!(tids.insert(o.get(&format!("tid{i}"))));
                assert_eq!(o.get(&format!("after{i}")), o.get(&format!("before{i}")));
                assert_eq!(o.get(&format!("token{i}")), 0);
                assert_eq!(o.get(&format!("value{i}")), 42 + i as i64);
                assert_eq!(o.get(&format!("fresh{i}")), 99);
            }
        }
    }
}

fn capacity(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    let requests = h.g.asm.data_label_with_i64s(&vec![0; 4097 * 3]);
    let l = &h.g.gc_gen.as_ref().unwrap().labels;
    let (acquire, release) = (l.metadata_lease_root_actor, l.metadata_lease_release_actor);
    for op in 0..2 {
        h.g.mov_imm64(R::V0, 0);
        h.g.store_rbp_slot(1800, R::V0);
        let again = h.g.create_text_label();
        h.g.bind_text_label(again);
        h.g.load_rbp_slot(R::V0, 1800);
        h.g.mov_reg_reg(R::V1, R::V0);
        h.g.shl_reg_imm8(R::V1, 1);
        h.g.add_reg_reg(R::V0, R::V1);
        h.g.shl_reg_imm8(R::V0, 3);
        h.g.mov_data_addr(R::V7, requests);
        h.g.add_reg_reg(R::V7, R::V0);
        h.g.load_rbp_slot(R::V0, slot_offset(0));
        call_owner(h.g, if op == 0 { acquire } else { release });
        lease_require_ok(h.g);
        h.g.load_rbp_slot(R::V0, 1800);
        h.g.add_reg_imm32(R::V0, 1);
        h.g.store_rbp_slot(1800, R::V0);
        h.g.cmp_reg_imm32(R::V0, 4096);
        h.g.jcc_label(crate::portable_asm::Condition::Below, again);
        if op == 0 {
            h.report("full_active", |h| lease_metric(h.g, 0));
            h.g.mov_data_addr(R::V7, requests);
            h.g.add_reg_imm32(R::V7, 4096 * 24);
            h.g.load_rbp_slot(R::V0, slot_offset(0));
            call_owner(h.g, acquire);
            h.keep(9);
            h.report("full_status", |h| h.value(Src::Raw(9)));
            h.report("failed_token", |h| {
                lease_request_word(h.g, requests, 4096 * 3 + 2)
            });
        }
    }
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_metadata_lease_pool_fills_and_releases_every_real_slot_without_leaking_failed_requests()
 {
    let o = run(capacity, 0, false);
    assert_eq!(o.get("full_active"), 4096);
    assert_eq!(o.get("full_status"), 2);
    assert_eq!(o.get("failed_token"), 0);
    assert_eq!(o.get("empty"), 0);
}
