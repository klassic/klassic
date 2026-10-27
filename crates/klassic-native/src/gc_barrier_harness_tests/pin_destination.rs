use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn reserve(h: &mut H<'_>, source: u32) -> crate::DataLabel {
    let token = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(source));
    pin_destination_call(h.g, token, false);
    lease_require_ok(h.g);
    token
}
fn lifecycle(g: &mut crate::NativeCodeGenerator, bytes: u64) {
    let mut h = H::begin(g);
    h.alloc(1, bytes, 0);
    h.root(0);
    if bytes > 0 {
        h.set_scalar(Src::Root(0), 0, 77);
    }
    let token = reserve(&mut h, 0);
    let copy = lease_request(h.g);
    lease_copy(h.g, token, copy);
    pin_destination_call(h.g, copy, true);
    h.keep(9);
    h.report("copied", |h| h.value(Src::Raw(9)));
    h.report("reserved", |h| pin_destination_metric(h.g, 1));
    for _ in 0..2 {
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
    }
    h.report("holder", |h| pin_destination_geometry(h.g, token, 0));
    h.report("top", |h| pin_destination_geometry(h.g, token, 1));
    h.report("start", |h| pin_destination_geometry(h.g, token, 2));
    h.report("state", |h| pin_destination_geometry(h.g, token, 3));
    h.report("active", |h| pin_destination_metric(h.g, 0));
    h.report("kept", |h| pin_destination_metric(h.g, 3));
    if bytes > 0 {
        h.report("source", |h| h.load(Src::Root(0), 0));
    }
    pin_destination_call(h.g, token, true);
    lease_require_ok(h.g);
    h.report("cancelled", |h| pin_destination_metric(h.g, 2));
    h.report("no_reservations", |h| pin_destination_metric(h.g, 0));
    h.report("deferred", |h| pin_destination_metric(h.g, 3));
    pin_destination_call(h.g, copy, true);
    h.keep(9);
    h.report("stale", |h| h.value(Src::Raw(9)));
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("after", |h| pin_destination_metric(h.g, 3));
    h.report("reclaimed", |h| pin_destination_metric(h.g, 4));
}
#[test]
fn production_pin_destinations_hold_unpublished_zero_small_and_multi_page_spans_across_full_gc() {
    for bytes in [0, 16, 400_000] {
        let o = run(lifecycle, bytes, false);
        let pages = if bytes == 400_000 { 4 } else { 1 };
        let source_pages = if bytes == 400_000 { 4 } else { 0 };
        assert_eq!(o.get("copied"), 3, "{}", o.text());
        assert_eq!(o.get("reserved"), pages);
        assert_eq!(o.get("holder"), 0);
        assert_eq!(o.get("top"), o.get("start"));
        assert_eq!(o.get("state"), 5);
        assert_eq!(o.get("active"), 1);
        assert_eq!(o.get("kept"), pages + source_pages);
        assert_eq!(o.get("cancelled"), 1);
        assert_eq!(o.get("no_reservations"), 0);
        assert_eq!(o.get("deferred"), pages + source_pages);
        assert_eq!(o.get("stale"), 3);
        assert_eq!(o.get("after"), source_pages);
        assert_eq!(o.get("reclaimed"), pages);
        if bytes > 0 {
            assert_eq!(o.get("source"), 77);
        }
    }
}
fn token_at(h: &mut H<'_>, tokens: crate::DataLabel) {
    h.g.load_rbp_slot(R::V0, 1856);
    h.g.mov_reg_reg(R::V1, R::V0);
    h.g.shl_reg_imm8(R::V1, 1);
    h.g.add_reg_reg(R::V0, R::V1);
    h.g.shl_reg_imm8(R::V0, 3);
    h.g.mov_data_addr(R::V7, tokens);
    h.g.add_reg_reg(R::V7, R::V0);
}
fn capacity(g: &mut crate::NativeCodeGenerator, _: u64) {
    use crate::portable_asm::Condition as C;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    let limit = pin_destination_limit(h.g);
    h.g.asm
        .data
        .resize(h.g.asm.data.len().next_multiple_of(8), 0);
    let tokens =
        h.g.asm
            .data_label_with_i64s(&vec![0; (limit as usize + 1) * 3]);
    h.g.mov_imm64(R::V0, 0);
    h.g.store_rbp_slot(1856, R::V0);
    let again = h.g.create_text_label();
    let full = h.g.create_text_label();
    h.g.bind_text_label(again);
    token_at(&mut h, tokens);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    pin_destination_dynamic(h.g, false);
    h.g.cmp_reg_imm8(R::V0, 6);
    h.g.jcc_label(C::Equal, full);
    lease_require_ok(h.g);
    h.g.load_rbp_slot(R::V0, 1856);
    h.g.add_reg_imm32(R::V0, 1);
    h.g.store_rbp_slot(1856, R::V0);
    h.g.cmp_reg_imm32(R::V0, limit as i32);
    h.g.jcc_label(C::Below, again);
    // Even before all physical pages are claimed, admission must leave real
    // contiguous Young and aggregate Old destination capacity available.
    h.g.mov_imm64(R::V0, 1);
    lease_require_ok(h.g);
    h.g.bind_text_label(full);
    h.report("owned", |h| pin_destination_metric(h.g, 0));
    h.report("reserved_pages", |h| pin_destination_metric(h.g, 1));
    token_at(&mut h, tokens);
    h.g.load_ptr_disp32(R::V0, R::V7, 0);
    h.g.load_ptr_disp32(R::V1, R::V7, 8);
    h.g.or_reg_reg(R::V0, R::V1);
    h.g.load_ptr_disp32(R::V1, R::V7, 16);
    h.g.or_reg_reg(R::V0, R::V1);
    h.keep(9);
    h.report("untouched", |h| h.value(Src::Raw(9)));
    h.g.load_rbp_slot(R::V0, 1856);
    h.g.store_rbp_slot(1848, R::V0);
    // Collect at actual maximum reservation occupancy before cancellation.
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("retained", |h| pin_destination_metric(h.g, 0));
    let cancel = h.g.create_text_label();
    h.g.bind_text_label(cancel);
    h.g.load_rbp_slot(R::V0, 1856);
    h.g.sub_reg_imm8(R::V0, 1);
    h.g.store_rbp_slot(1856, R::V0);
    token_at(&mut h, tokens);
    pin_destination_dynamic(h.g, true);
    lease_require_ok(h.g);
    h.g.load_rbp_slot(R::V0, 1856);
    h.g.test_reg_reg(R::V0, R::V0);
    h.g.jcc_label(C::NotEqual, cancel);
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("empty", |h| pin_destination_metric(h.g, 0));
    h.report("reclaimed", |h| pin_destination_metric(h.g, 4));
    h.report("no_large", |h| pin_destination_metric(h.g, 3));
    // Reuse an actual reclaimed native holder and span after the full sweep.
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    pin_destination_call(h.g, tokens, false);
    lease_require_ok(h.g);
    pin_destination_call(h.g, tokens, true);
    lease_require_ok(h.g);
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("reused_empty", |h| pin_destination_metric(h.g, 3));
}
#[test]
fn production_pin_destination_exhaustion_preserves_output_and_full_gc_destination_capacity() {
    let o = run(capacity, 0, false);
    assert!(o.get("owned") > 0, "{}", o.text());
    assert_eq!(o.get("owned"), o.get("reserved_pages"));
    assert_eq!(o.get("retained"), o.get("owned"));
    assert_eq!(o.get("untouched"), 0);
    assert_eq!(o.get("empty"), 0);
    assert_eq!(o.get("reclaimed"), o.get("owned"));
    assert_eq!(o.get("no_large"), 0);
    assert_eq!(o.get("reused_empty"), 0);
}
fn parallel(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let mut h = H::begin(g);
    publish_primary(h.g);
    let n = arg & 255;
    let full = arg & 256 != 0;
    let mut actors = Vec::new();
    let mut roots = Vec::new();
    let mut spans = Vec::new();
    for i in 0..n {
        let actor = create_actor(h.g, 16, 8);
        let root = native_completed_graph(h.g, actor, 42 + i);
        spans.push(pin_destination_actor_reserve(h.g, actor, root));
        copy_actor_native_enter(h.g, actor);
        actors.push(actor);
        roots.push(root);
    }
    let p = run_parallel_native_completed(h.g, &actors, &roots, 2, full, true);
    h.report("held", |h| pin_destination_metric(h.g, 0));
    pin_destination_call(h.g, spans[0], true);
    h.keep(9);
    h.report("foreign", |h| h.value(Src::Raw(9)));
    for i in 0..n as usize {
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        h.report(&format!("holder{i}"), |h| {
            pin_destination_geometry(h.g, spans[i], 0)
        });
        native_completed_resume(h.g, actors[i]);
        pin_destination_actor_cancel(h.g, actors[i], spans[i]);
        copy_actor_native_enter(h.g, actors[i]);
    }
    h.report("cancelled", |h| pin_destination_metric(h.g, 2));
    h.report("empty", |h| pin_destination_metric(h.g, 0));
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("reclaimed", |h| pin_destination_metric(h.g, 4));
    h.report("no_large", |h| pin_destination_metric(h.g, 3));
}
#[test]
fn production_pin_destinations_survive_real_native_owner_rounds_and_reject_foreign_cancellation() {
    for full in [false, true] {
        for n in [1, 2, 8] {
            let o = run(parallel, n | if full { 256 } else { 0 }, true);
            assert_eq!(o.get("held"), n as i64, "{}", o.text());
            assert_eq!(o.get("foreign"), 3);
            assert_eq!(o.get("cancelled"), n as i64);
            assert_eq!(o.get("empty"), 0);
            assert_eq!(o.get("reclaimed"), n as i64);
            assert_eq!(o.get("no_large"), 0);
            let mut tids = std::collections::HashSet::new();
            for i in 0..n {
                assert!(tids.insert(o.get(&format!("tid{i}"))));
                assert_eq!(o.get(&format!("holder{i}")), 0);
            }
        }
    }
}
