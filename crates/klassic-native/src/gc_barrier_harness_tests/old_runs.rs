//! Actual fragmented allocation/forwarding/scanning/retirement with a live chain.
use super::{H, Src, run};
use crate::gc_codegen::production::{
    MAX_PAYLOAD,
    test_support::{
        fragment_old_destinations, gap_address, old_run_metric, old_source_tag, request_old_cycle,
        request_young_pages,
    },
};
use crate::portable_asm::{PortableAsm, Reg as R};
use crate::{NativeCodeGenerator, gc_abi::HEAP_PAGE_BYTES};
fn protect(h: &mut H<'_>, permissions: u64) {
    gap_address(h.g);
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.mov_imm64(R::V6, HEAP_PAGE_BYTES);
    h.g.mov_imm64(R::V2, permissions);
    h.g.mov_imm64(R::V0, 10);
    h.g.asm.syscall();
    h.keep(40);
    h.report(
        if permissions == 0 {
            "protected"
        } else {
            "restored"
        },
        |h| h.value(Src::Raw(40)),
    );
}
fn chain_value(h: &mut H<'_>) {
    h.value(Src::Root(16));
    h.keep(30);
    for _ in 0..16 {
        h.load_barriered(Src::Raw(30), 0);
        h.keep(30);
    }
    h.load(Src::Raw(30), 0);
}
fn save_leaf(h: &mut H<'_>) {
    h.value(Src::Root(16));
    h.keep(30);
    for _ in 0..16 {
        h.load_barriered(Src::Raw(30), 0);
        h.keep(30);
    }
    h.value(Src::Raw(30));
    h.keep(31);
}
fn fragmented(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, MAX_PAYLOAD, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    for slot in 1..17 {
        h.alloc(2, MAX_PAYLOAD, slot);
        h.store(Src::Raw(slot), 0, Src::Root(slot - 1), true);
        h.root(slot);
    }
    h.repeat(239, |h| {
        h.alloc(1, MAX_PAYLOAD, 20);
        h.root(20);
    });
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.repeat(256, |h| {
        h.alloc(1, MAX_PAYLOAD, 20);
        h.root(20);
    });
    // This setup needs all new pages in Old, including their age-zero objects.
    crate::gc_codegen::production::test_support::request_full_collection(h.g);
    h.collect();
    h.finish_collection(); // All 512 Old pages are actually populated.
    h.repeat(495, |h| h.pop_root());
    for slot in 0..16 {
        h.value(Src::Null);
        h.set_root(slot);
    }
    fragment_old_destinations(h.g);
    protect(&mut h, 0);
    request_young_pages(h.g, 64);
    for pass in 0..2 {
        save_leaf(&mut h);
        request_old_cycle(h.g);
        h.collect();
        h.report(&format!("source{pass}"), |h| {
            h.value(Src::Raw(31));
            old_source_tag(h.g);
        });
        h.finish_collection();
        h.report(&format!("retired{pass}"), |h| {
            h.value(Src::Raw(31));
            old_source_tag(h.g);
        });
        h.report(&format!("runs{pass}"), |h| old_run_metric(h.g, false));
        h.report(&format!("switches{pass}"), |h| old_run_metric(h.g, true));
        h.report(&format!("value{pass}"), chain_value);
        h.report(&format!("id{pass}"), |h| h.object_id(Src::Root(16)));
    }
    protect(&mut h, 3);
}
#[test]
fn production_old_runs_preserve_a_chain_across_protected_gaps_and_repeated_full_moves() {
    let o = run(fragmented, 0, false);
    assert_eq!(o.get("protected"), 0);
    assert_eq!(o.get("restored"), 0);
    assert!(o.get("runs0") > 2, "{}", o.text());
    assert!(o.get("switches0") >= 2, "{}", o.text());
    for pass in 0..2 {
        assert_eq!(o.get(&format!("source{pass}")), 1, "{}", o.text());
        assert_eq!(o.get(&format!("retired{pass}")), 0, "{}", o.text());
        assert_eq!(o.get(&format!("value{pass}")), 84, "{}", o.text());
        assert_eq!(o.get(&format!("id{pass}")), 17);
    }
}

fn concurrent(g: &mut NativeCodeGenerator, _arg: u64) {
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
    for slot in 0..16 {
        h.value(Src::Null);
        h.set_root(slot);
    }
    crate::gc_codegen::production::test_support::idle_fragment(h.g);
    h.g.load_rbp_slot(R::V0, 8 * 44);
    h.keep(45);
    // Protect a dynamically selected, independently reserved gap page.
    for (pass, permissions) in [(0, 0), (1, 3)] {
        h.value(Src::Raw(45));
        h.g.mov_reg_reg(R::V7, R::V0);
        h.g.mov_imm64(R::V6, HEAP_PAGE_BYTES);
        h.g.mov_imm64(R::V2, permissions);
        h.g.mov_imm64(R::V0, 10);
        h.g.asm.syscall();
        h.keep(40);
        h.report(&format!("permission{pass}"), |h| h.value(Src::Raw(40)));
        if pass == 0 {
            request_young_pages(h.g, 64);
            for cycle in 0..2 {
                request_old_cycle(h.g);
                h.collect();
                h.finish_collection();
                h.report(&format!("runs{cycle}"), |h| old_run_metric(h.g, false));
                h.report(&format!("value{cycle}"), chain_value);
            }
        }
    }
    h.report("switches", |h| old_run_metric(h.g, true));
    h.report_counter(
        "worker",
        crate::gc_codegen::production::test_support::Counter::WorkerScanned,
    );
}
#[test]
fn production_old_runs_real_worker_copies_and_retires_across_protected_gaps() {
    let o = run(concurrent, 0, true);
    for name in ["permission0", "permission1"] {
        assert_eq!(o.get(name), 0);
    }
    for cycle in 0..2 {
        assert_eq!(o.get(&format!("value{cycle}")), 84, "{}", o.text());
    }
    assert!(o.get("runs0") > 2, "{}", o.text());
    assert!(o.get("switches") >= 2, "{}", o.text());
    assert!(o.get("worker") > 0, "{}", o.text());
}

#[test]
fn production_old_runs_load_barrier_repairs_a_good_colored_second_run_source_reference() {
    let o = run(super::pool::smaller_old_snapshot, 1, false);
    assert_eq!(o.get("good_source_repaired"), 1, "{}", o.text());
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("linked"), 84);
}

fn delayed_publication(g: &mut NativeCodeGenerator, _arg: u64) {
    use crate::gc_codegen::production::test_support::{
        Counter, idle_fragment, old_scan_cursor, private_old_copy, publish_private_old,
    };
    let mut h = H::begin(g);
    for slot in 0..9 {
        h.alloc(1, MAX_PAYLOAD, slot);
        h.set_scalar(Src::Raw(slot), 0, 84);
        h.root(slot);
    }
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    idle_fragment(h.g);
    for slot in 0..9 {
        h.value(Src::Root(slot));
        h.keep(60 + slot);
    }
    request_young_pages(h.g, 64);
    request_old_cycle(h.g);
    h.collect();
    for slot in 0..9 {
        h.value(Src::Raw(60 + slot));
        private_old_copy(h.g, slot < 8);
    }
    h.keep(50); // First private header in the second eight-page Old run.
    h.relocate_quantum(128, true);
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.try_close_relocation);
    h.g.call_label(h.l.unlock);
    h.report_counter("waiting", Counter::PhaseOpen);
    h.report("at_unpublished", |h| {
        old_scan_cursor(h.g);
        h.g.load_rbp_slot(R::V1, super::slot_offset(50));
        h.g.sub_reg_reg(R::V0, R::V1);
    });
    h.value(Src::Raw(50));
    publish_private_old(h.g);
    h.finish_collection();
    h.report_counter("closed", Counter::PhaseOpen);
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("switches", |h| old_run_metric(h.g, true));
}
#[test]
fn production_old_runs_pause_at_an_unpublished_second_run_copy_then_resume() {
    let o = run(delayed_publication, 0, false);
    assert_eq!(o.get("waiting"), 1, "{}", o.text());
    assert_eq!(o.get("at_unpublished"), 0, "{}", o.text());
    assert_eq!(o.get("closed"), 0, "{}", o.text());
    assert_eq!(o.get("value"), 84);
    assert!(o.get("switches") >= 2, "{}", o.text());
}
