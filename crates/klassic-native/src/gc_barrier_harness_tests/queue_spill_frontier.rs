//! Actual record-backed spill roots survive Young copying and source retirement.
use super::{H, Src, run};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};

fn preserve(g: &mut crate::NativeCodeGenerator, worker_copy: u64) {
    let mut h = H::begin(g);
    mark_queue_capacity(h.g, false, 2);
    mark_queue_capacity(h.g, true, 2);
    h.alloc(2, 32 * 8, 0);
    h.root(0);
    for i in 0..32 {
        h.alloc(1, 16, 1);
        h.set_scalar(Src::Raw(1), 0, 84 + u64::from(i));
        h.store(Src::Root(0), i, Src::Raw(1), true);
    }
    h.mark_start();
    h.mark_quantum(10); // Leave the 32-field receiver suspended with real spills.
    h.report("spill_before", |h| mark_queue_metric(h.g, false, 1));
    h.report("holders_before", |h| mark_queue_metric(h.g, false, 2));
    h.pop_root(); // Only actual mark jobs/receiver now retain this graph.
    for cycle in 0..3 {
        h.collect();
        h.relocate_quantum(1, worker_copy != 0);
        h.finish_collection(); // Includes real source metadata OS retirement.
        h.report(&format!("spill{cycle}"), |h| {
            mark_queue_metric(h.g, false, 1)
        });
        h.report(&format!("audited{cycle}"), |h| {
            audit_old_spill_destinations(h.g)
        });
        h.report(&format!("pending{cycle}"), |h| {
            mark_queue_metric(h.g, false, 7)
        });
        h.report(&format!("frozen{cycle}"), |h| {
            mark_queue_metric(h.g, false, 6)
        });
        mark_receiver(h.g, None);
        h.keep(3);
        h.report(&format!("id{cycle}"), |h| h.object_id(Src::Raw(3)));
        for i in [0, 5, 31] {
            h.report(&format!("value{cycle}_{i}"), |h| {
                h.deref_field(Src::Raw(3), i, 0)
            });
        }
    }
    h.drain_marks();
    h.report("spill_done", |h| mark_queue_metric(h.g, false, 1));
    h.report("holders_done", |h| mark_queue_metric(h.g, false, 2));
    h.end_marks();
}

fn coalesce(g: &mut crate::NativeCodeGenerator, variant: u64) {
    let mut h = H::begin(g);
    mark_queue_capacity(h.g, false, 1);
    h.alloc(2, 16, 0);
    h.root(0);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.value(Src::Root(0));
    h.keep(3); // Actual Old holder remains physically stable in Young moves.
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Raw(3), 0, Src::Raw(1), true);
    h.mark_start();
    h.collect();
    h.relocate_quantum(64, variant & 1 != 0);
    h.load(Src::Raw(3), 0);
    h.g.shr_reg_imm8(R::V0, 16);
    h.g.load_ptr_disp32(R::V1, R::V3, heap_base_offset());
    h.g.add_reg_reg(R::V0, R::V1);
    h.keep(2); // Observe the real white copy without the protecting load barrier.
    h.report("white", |h| h.marked(Src::Raw(2)));
    h.g.call_label(h.l.lock);
    h.value(Src::Raw(3));
    enqueue_mark(h.g, h.l.mark_ref); // Occupy the sole ordinary queue slot.
    // Real late SATB can retain the captured source after a white copy exists.
    // Exercise both list orders; neither job's metadata is manufactured.
    for value in if variant & 2 == 0 { [1, 2] } else { [2, 1] } {
        h.value(Src::Raw(value));
        enqueue_mark(h.g, h.l.mark_ref);
    }
    h.g.call_label(h.l.unlock);
    h.report("before", |h| mark_queue_metric(h.g, false, 1));
    h.relocate_quantum(256, variant & 1 != 0);
    h.report("coalesced", |h| mark_queue_metric(h.g, false, 1));
    h.report("pending", |h| mark_queue_metric(h.g, false, 7));
    h.finish_collection();
    h.report("audited", |h| audit_old_spill_destinations(h.g));
    h.report("remaining", |h| mark_queue_metric(h.g, false, 1));
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
    h.drain_marks();
    h.report("holders_done", |h| mark_queue_metric(h.g, false, 2));
    h.end_marks();
}

#[test]
fn production_old_spill_coalesces_real_source_and_existing_white_copy_jobs_in_both_list_orders() {
    for variant in 0..4 {
        let o = run(coalesce, variant, false);
        assert_eq!(o.get("white"), 0, "{}", o.text());
        assert_eq!(o.get("before"), 2, "{}", o.text());
        assert_eq!(o.get("coalesced"), 1, "{}", o.text());
        assert_eq!(o.get("pending"), 0, "{}", o.text());
        assert_eq!(o.get("audited"), o.get("remaining"), "{}", o.text());
        assert_eq!(o.get("value"), 84, "{}", o.text());
        assert_eq!(o.get("holders_done"), 0, "{}", o.text());
    }
}

#[test]
fn production_old_spill_nodes_and_active_receipts_survive_young_copy_promotion_and_source_retirement()
 {
    if let Some(path) = std::env::var_os("KLASSIC_SPILL_DEBUG_LAYOUT") {
        let text = spill_debug_offsets()
            .into_iter()
            .map(|(name, offset)| format!("{name} {offset:x}\n"))
            .collect::<String>();
        std::fs::write(path, text).unwrap();
    }
    for worker_copy in [0, 1] {
        let o = run(preserve, worker_copy, false);
        assert!(o.get("spill_before") > 0, "{}", o.text());
        assert!(o.get("holders_before") > 0, "{}", o.text());
        for cycle in 0..3 {
            assert!(o.get(&format!("spill{cycle}")) > 0, "{}", o.text());
            assert_eq!(
                o.get(&format!("audited{cycle}")),
                o.get(&format!("spill{cycle}")),
                "{}",
                o.text()
            );
            assert_eq!(o.get(&format!("pending{cycle}")), 0, "{}", o.text());
            assert_eq!(o.get(&format!("frozen{cycle}")), 0, "{}", o.text());
            assert_eq!(o.get(&format!("id{cycle}")), o.get("id0"), "{}", o.text());
            for i in [0, 5, 31] {
                assert_eq!(o.get(&format!("value{cycle}_{i}")), 84 + i, "{}", o.text());
            }
        }
        assert_eq!(o.get("spill_done"), 0, "{}", o.text());
        assert_eq!(o.get("holders_done"), 0, "{}", o.text());
    }
}
