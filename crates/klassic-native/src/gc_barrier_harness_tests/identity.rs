//! Identity/age checks on the production collector, including worker-private copies.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_abi::MarkRecord;
use crate::gc_codegen::production::test_support::Counter;
use std::mem::offset_of;

fn moves(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    for slot in 0..2 {
        h.alloc(1, 16, slot);
        h.set_scalar(Src::Raw(slot), 0, 100 + u64::from(slot));
        h.root(slot);
    }
    h.report("a0", |h| h.object_id(Src::Root(0)));
    h.report("b0", |h| h.object_id(Src::Root(1)));
    h.collect();
    h.finish_collection();
    h.report("a1", |h| h.object_id(Src::Root(0)));
    h.report("b1", |h| h.object_id(Src::Root(1)));
    h.report("age1", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, age))
    });
    h.collect();
    h.finish_collection();
    h.report("a2", |h| h.object_id(Src::Root(0)));
    h.report("b2", |h| h.object_id(Src::Root(1)));
    h.report("age2", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, age))
    });
    h.report("old", |h| h.is_old(Src::Root(0)));
    // Both survivors were promoted, leaving the original Young range empty.
    // A new allocation reuses its first address, but must get a fresh identity.
    h.alloc(1, 16, 2);
    h.report("fresh", |h| h.object_id(Src::Raw(2)));
    h.report("value", |h| h.load(Src::Root(0), 0));
}

#[test]
fn production_identity_survives_young_copy_promotion_and_address_reuse() {
    let o = run(moves, 0, false);
    assert!(o.get("a0") > 0);
    assert_ne!(o.get("a0"), o.get("b0"));
    for name in ["a1", "a2"] {
        assert_eq!(o.get(name), o.get("a0"), "{}", o.text());
    }
    for name in ["b1", "b2"] {
        assert_eq!(o.get(name), o.get("b0"), "{}", o.text());
    }
    assert_eq!(o.get("age1"), 1);
    assert_eq!(o.get("age2"), 2);
    assert_eq!(o.get("old"), 1);
    assert!(o.get("fresh") > o.get("b0"), "{}", o.text());
    assert_eq!(o.get("value"), 100);
}

fn worker_move(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 12345);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.report("before", |h| h.object_id(Src::Raw(1)));
    h.collect();
    // No OS worker is started. Invoke its real copier before Cheney scanning
    // to exercise the worker-private metadata path deterministically.
    h.worker_copy(Src::Raw(1));
    h.keep(2);
    h.report("worker_id", |h| h.object_id(Src::Raw(2)));
    h.report("worker_age", |h| {
        h.object_metadata(Src::Raw(2), offset_of!(MarkRecord, age))
    });
    h.report_counter("worker_copies", Counter::WorkerCopies);
    h.finish_collection();
    h.collect();
    h.finish_collection();
    h.load_barriered(Src::Root(0), 0);
    h.keep(2);
    h.report("promoted_id", |h| h.object_id(Src::Raw(2)));
    h.report("promoted_age", |h| {
        h.object_metadata(Src::Raw(2), offset_of!(MarkRecord, age))
    });
    h.report("old", |h| h.is_old(Src::Raw(2)));
    h.report("value", |h| h.load(Src::Raw(2), 0));
}

#[test]
fn production_worker_copies_keep_private_identity_and_survival_age() {
    let o = run(worker_move, 0, false);
    assert!(o.get("before") > 0);
    assert_eq!(o.get("worker_id"), o.get("before"), "{}", o.text());
    assert_eq!(o.get("promoted_id"), o.get("before"));
    assert_eq!(o.get("worker_age"), 1);
    assert_eq!(o.get("promoted_age"), 2);
    assert_eq!(o.get("old"), 1);
    assert_eq!(o.get("value"), 12345);
    assert!(o.get("worker_copies") > 0);
}

fn old_page_edge(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    // 2731 consecutive 48-byte Old allocations include a header in the last
    // partial metadata interval of a 128-KiB heap page. Old objects straddle
    // pages, unlike Young TLAB allocations.
    h.repeat(2730, |h| {
        h.alloc(2, 16, 1);
        h.store(Src::Raw(1), 0, Src::Root(0), true);
        h.value(Src::Raw(1));
        h.set_root(0);
    });
    h.report("before", |h| h.object_id(Src::Root(0)));
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.report("after", |h| h.object_id(Src::Root(0)));
    h.report("old", |h| h.is_old(Src::Root(0)));
    h.report("age", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, age))
    });
    h.repeat(2730, |h| {
        h.load_barriered(Src::Root(0), 0);
        h.set_root(0);
    });
    h.report("tail_id", |h| h.object_id(Src::Root(0)));
    h.report("tail_age", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, age))
    });
}

#[test]
fn production_identity_handles_old_header_in_final_page_interval() {
    let o = run(old_page_edge, 0, false);
    assert_eq!(o.get("before"), o.get("after"), "{}", o.text());
    assert_eq!(o.get("old"), 1);
    assert_eq!(o.get("age"), 2);
    assert_eq!(o.get("tail_id"), 1);
    assert_eq!(o.get("tail_age"), 2);
}

fn concurrent_moves(g: &mut NativeCodeGenerator, nodes: u64) {
    use crate::portable_asm::{PortableAsm, Reg};
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.repeat(nodes - 1, |h| {
        h.alloc(2, 16, 1);
        h.store(Src::Raw(1), 0, Src::Root(0), true);
        h.value(Src::Raw(1));
        h.set_root(0);
    });
    h.report("before", |h| h.object_id(Src::Root(0)));
    h.repeat(2000, |h| {
        h.alloc(1, 60000, 1);
        // Give a woken worker a turn even on a single-CPU affinity mask.
        // No raw heap address is kept live across this native scheduling point.
        h.g.mov_imm64(Reg::V0, 24); // Linux sched_yield
        h.g.asm.syscall();
    });
    h.finish_collection();
    h.report("after", |h| h.object_id(Src::Root(0)));
    h.report("age", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, age))
    });
    h.report_counter("collections", Counter::Collections);
    h.report_counter("worker_copies", Counter::WorkerCopies);
    h.report_counter("worker_scanned", Counter::WorkerScanned);
    h.report_counter("allocation_stalls", Counter::AllocationStalls);
    // Count the retained chain after both mutator and worker relocations.
    h.g.mov_imm64(Reg::V0, 0);
    h.keep(3);
    h.value(Src::Root(0));
    h.keep(2);
    let top = h.g.create_text_label();
    let done = h.g.create_text_label();
    h.g.bind_text_label(top);
    h.value(Src::Raw(2));
    h.g.test_reg_reg(Reg::V0, Reg::V0);
    h.g.jcc_label(crate::portable_asm::Condition::Equal, done);
    h.load_barriered(Src::Raw(2), 0);
    h.keep(2);
    h.value(Src::Raw(3));
    h.g.add_reg_imm32(Reg::V0, 1);
    h.keep(3);
    h.g.jmp_label(top);
    h.g.bind_text_label(done);
    h.report("chain", |h| h.value(Src::Raw(3)));
}

#[test]
fn production_identity_survives_concurrent_worker_copy_races() {
    let stress = std::env::var_os("KLASSIC_IDENTITY_RACE_STRESS").is_some();
    let o = super::run_with_stress(concurrent_moves, 513, true, stress);
    assert_eq!(o.get("before"), o.get("after"), "{}", o.text());
    assert_eq!(o.get("age"), 2, "{}", o.text());
    assert_eq!(o.get("chain"), 513, "{}", o.text());
    assert!(o.get("collections") >= 2, "{}", o.text());
    if !stress {
        assert!(o.get("worker_copies") > 0, "{}", o.text());
        assert!(o.get("worker_scanned") > 0, "{}", o.text());
    }
}

#[test]
fn production_worker_retains_relocation_space_for_a_large_live_graph() {
    // About 2.4 MiB of live data fits easily in the 32-MiB destination.
    // Fresh 60-KiB garbage allocations must not exhaust it ahead of the worker.
    let o = run(concurrent_moves, 50_001, true);
    assert_eq!(o.get("before"), o.get("after"), "{}", o.text());
    assert_eq!(o.get("age"), 2, "{}", o.text());
    assert_eq!(o.get("chain"), 50_001, "{}", o.text());
    assert!(o.get("worker_copies") > 0, "{}", o.text());
    assert!(o.get("worker_scanned") > 0, "{}", o.text());
}
