//! Actual queue mappings grow while record-backed overflow keeps all work.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn offer(h: &mut H<'_>, root: u32) {
    h.g.load_rbp_slot(R::V0, slot_offset(root));
    mark_queue_offer(h.g, false);
}
fn grow(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    mark_queue_capacity(h.g, false, 2);
    for i in 0..16 {
        h.alloc(1, 16, i);
        h.set_scalar(Src::Raw(i), 0, 77 + u64::from(i));
        h.root(i);
    }
    h.mark_start();
    for i in 0..4 {
        offer(&mut h, i);
    }
    h.report("spill_before", |h| mark_queue_metric(h.g, false, 1));
    weak_growth_map_fail_next(h.g);
    mark_queue_service(h.g);
    h.report("failed_map", |h| mark_queue_growth_metric(h.g, false, 4));
    h.report("owned_after_fail", |h| mark_queue_metric(h.g, false, 2));
    for i in 4..6 {
        offer(&mut h, i);
    } // real offers consume error then request retry
    mark_queue_service(h.g);
    h.repeat(3, |h| mark_queue_tick(h.g));
    h.report("first_capacity", |h| mark_queue_metric(h.g, false, 0));
    for i in 6..10 {
        offer(&mut h, i);
    }
    h.report("old_before", |h| mark_queue_growth_metric(h.g, false, 11));
    mark_queue_service(h.g);
    h.repeat(5, |h| mark_queue_tick(h.g));
    h.report("capacity", |h| mark_queue_metric(h.g, false, 0));
    h.report("old", |h| mark_queue_growth_metric(h.g, false, 8));
    h.report("charge_before", |h| weak_growth_committed(h.g));
    native_root_unmap_fail_next(h.g);
    mark_queue_service(h.g);
    h.report("retained", |h| mark_queue_growth_metric(h.g, false, 1));
    h.report("charge_failed", |h| weak_growth_committed(h.g));
    mark_queue_service(h.g);
    h.report("released", |h| mark_queue_growth_metric(h.g, false, 7));
    h.report("charge_after", |h| weak_growth_committed(h.g));
    for i in 10..16 {
        offer(&mut h, i);
    }
    h.drain_marks();
    h.end_marks();
    h.report("empty", |h| mark_queue_metric(h.g, false, 1));
    h.report("holders", |h| mark_queue_metric(h.g, false, 2));
    for i in [0, 7, 15] {
        h.report(&format!("value{i}"), |h| h.load(Src::Root(i), 0));
    }
}
#[test]
fn production_mark_queue_growth_retains_real_spill_work_on_os_map_and_release_failure() {
    let o = run(grow, 0, false);
    assert_eq!(o.get("spill_before"), 2, "{}", o.text());
    assert_eq!(o.get("failed_map"), 1);
    assert_eq!(o.get("owned_after_fail"), 2);
    assert_eq!(o.get("first_capacity"), 4);
    assert_eq!(o.get("capacity"), 8);
    assert_eq!(o.get("old"), o.get("old_before"));
    assert_eq!(o.get("retained"), 5);
    assert_eq!(o.get("charge_before"), o.get("charge_failed"));
    assert_eq!(o.get("released"), 1);
    assert_eq!(o.get("charge_before") - o.get("charge_after"), 4096);
    for name in ["empty", "holders"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
    for i in [0, 7, 15] {
        assert_eq!(o.get(&format!("value{i}")), 77 + i);
    }
}

fn partial(g: &mut crate::NativeCodeGenerator, worker_copy: u64) {
    let mut h = H::begin(g);
    mark_queue_capacity(h.g, false, 2);
    mark_queue_capacity(h.g, true, 2);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 77);
    h.root(2);
    h.alloc(2, 32 * 8, 0);
    h.root(0);
    for i in 0..32 {
        h.alloc(2, 64 * 8, 1);
        h.store(Src::Raw(1), 0, Src::Root(2), true);
        h.store(Src::Root(0), i, Src::Raw(1), true);
    }
    h.mark_start();
    mark_queue_claim_active(h.g);
    h.report("active_before", |h| mark_queue_metric(h.g, false, 5));
    mark_receiver(h.g, None);
    h.keep(3);
    h.report("id_before", |h| h.object_id(Src::Raw(3)));
    h.pop_root();
    for cycle in 0..3 {
        h.collect();
        h.relocate_quantum(1, worker_copy != 0);
        h.finish_collection();
        h.report(&format!("active{cycle}"), |h| {
            mark_queue_metric(h.g, false, 5)
        });
        mark_receiver(h.g, None);
        h.keep(3);
        h.report(&format!("id{cycle}"), |h| h.object_id(Src::Raw(3)));
        h.report(&format!("value{cycle}"), |h| {
            h.deref_field(Src::Raw(3), 0, 0)
        });
    }
    h.drain_marks();
    h.end_marks();
    h.report("holders", |h| mark_queue_metric(h.g, false, 2));
}
#[test]
fn production_mark_queue_partial_spill_scan_transfers_native_receiver_holder_across_young_moves() {
    for worker in [0, 1] {
        let o = run(partial, worker, false);
        assert!(o.get("active_before") > 0, "{}", o.text());
        for cycle in 0..3 {
            assert!(o.get(&format!("active{cycle}")) > 0, "{}", o.text());
            assert_eq!(o.get(&format!("id{cycle}")), o.get("id_before"));
            assert_eq!(o.get(&format!("value{cycle}")), 77);
        }
        assert_eq!(o.get("holders"), 0, "{}", o.text());
    }
}

fn mirror_move(g: &mut crate::NativeCodeGenerator, worker_copy: u64) {
    let mut h = H::begin(g);
    mark_queue_capacity(h.g, false, 2);
    for i in 0..6 {
        h.alloc(1, 16, i);
        h.set_scalar(Src::Raw(i), 0, 100 + u64::from(i));
        h.root(i);
    }
    h.mark_start();
    for i in 0..4 {
        offer(&mut h, i);
    }
    mark_queue_service(h.g);
    mark_queue_tick(h.g); // copied queue[0] under original owner
    h.report("copied", |h| mark_queue_growth_metric(h.g, false, 2));
    h.mark_quantum(1);
    h.mark_quantum(1); // actual pops release both original words
    offer(&mut h, 5); // overwrite the already-copied index0: candidate must mirror
    h.collect();
    h.relocate_quantum(1, worker_copy != 0);
    h.finish_collection();
    // Young relocation rewrites that same old-frontier word while the candidate
    // remains unpublished, and actually retires its original source mapping.
    h.repeat(2, |h| mark_queue_tick(h.g));
    h.report("capacity", |h| mark_queue_metric(h.g, false, 0));
    h.report("queue_is_root", |h| {
        mark_receiver(h.g, Some(0));
        h.g.push_reg(R::V0);
        h.value(Src::Root(5));
        h.g.pop_reg(R::V1);
        let no = h.g.create_text_label();
        let done = h.g.create_text_label();
        h.g.cmp_reg_reg(R::V0, R::V1);
        h.g.jcc_label(crate::portable_asm::Condition::NotEqual, no);
        h.g.mov_imm64(R::V0, 1);
        h.g.jmp_label(done);
        h.g.bind_text_label(no);
        h.g.mov_imm64(R::V0, 0);
        h.g.bind_text_label(done);
    });
    h.drain_marks();
    h.end_marks();
    h.report("holders", |h| mark_queue_metric(h.g, false, 2));
    h.report("value", |h| h.load(Src::Root(5), 0));
}
#[test]
fn production_mark_queue_candidate_mirrors_reused_words_and_actual_young_frontier_relocation() {
    for worker in [0, 1] {
        let o = run(mirror_move, worker, false);
        assert_eq!(o.get("copied"), 1, "{}", o.text());
        assert_eq!(o.get("capacity"), 4);
        assert_eq!(o.get("queue_is_root"), 1, "{}", o.text());
        assert_eq!(o.get("holders"), 0);
        assert_eq!(o.get("value"), 105);
    }
}

fn spill_color(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    mark_queue_capacity(h.g, true, 1);
    for i in 0..3 {
        h.alloc(1, 16, i);
        h.set_scalar(Src::Raw(i), 0, 77 + u64::from(i));
        if i == 1 {
            h.keep(6);
        } // actual payload; root(i) replaces its own stack slot
        h.root(i);
    }
    h.mark_start();
    h.g.call_label(h.l.young_start);
    for i in 0..2 {
        h.g.load_rbp_slot(R::V0, slot_offset(i));
        mark_queue_offer_captured_young(h.g);
    }
    h.marked(Src::Raw(6));
    h.keep(9);
    mark_queue_reference(h.g, true, false);
    h.g.and_reg_imm32(R::V0, i32::from(crate::gc_abi::OLD_MARK_MASK));
    h.keep(7);
    mark_queue_reference(h.g, true, true);
    h.g.and_reg_imm32(R::V0, i32::from(crate::gc_abi::OLD_MARK_MASK));
    h.keep(8);
    h.report("old_white", |h| h.value(Src::Raw(9)));
    h.report("normal_old_bits", |h| h.value(Src::Raw(7)));
    h.report("spill_old_bits", |h| h.value(Src::Raw(8)));
    h.g.call_label(h.l.lock);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
    h.end_marks();
    h.collect();
    h.finish_collection();
    h.report("holders", |h| mark_queue_metric(h.g, true, 2));
}
#[test]
fn production_young_spill_preserves_the_actual_uncertified_old_mark_color() {
    let o = run(spill_color, 0, false);
    assert_eq!(o.get("old_white"), 0, "{}", o.text());
    assert_eq!(
        o.get("normal_old_bits"),
        o.get("spill_old_bits"),
        "{}",
        o.text()
    );
    assert_eq!(o.get("holders"), 0);
}
