//! Frame relocation outside the start pause, late publications and CAS heals.
use super::{H, Src, run, slot_offset};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::{Counter, heal_relocated_root};
use crate::portable_asm::{Condition, PortableAsm, Reg};

fn word(h: &mut H<'_>, slot: u32) {
    h.g.load_rbp_slot(Reg::V0, slot_offset(slot));
    h.g.atomic_load_acquire(Reg::V0, Reg::V0);
}

fn many_roots(g: &mut NativeCodeGenerator, worker: u64) {
    let mut h = H::begin(g);
    h.repeat(1000, |h| {
        h.alloc(1, 16, 0);
        h.set_scalar(Src::Raw(0), 0, 42);
        h.root(0);
    });
    h.collect();
    h.report_counter("initial_cursor", Counter::RelocateRootIndex);
    h.report_counter("bound", Counter::RelocateRootBound);
    h.report_counter("initial_mutator", Counter::MutatorCopies);
    h.report_counter("initial_worker", Counter::WorkerCopies);
    h.relocate_quantum(7, worker != 0);
    h.keep(2);
    h.report("done7", |h| h.value(Src::Raw(2)));
    h.report_counter("cursor7", Counter::RelocateRootIndex);
    h.report_counter("mutator7", Counter::MutatorCopies);
    h.report_counter("worker7", Counter::WorkerCopies);
    h.report("unvisited_value", |h| h.load(Src::Root(0), 0));
    h.report("unvisited_id", |h| h.object_id(Src::Root(0)));
    h.report_counter("cursor_after_read", Counter::RelocateRootIndex);
    h.finish_collection();
    h.report_counter("cursor_done", Counter::RelocateRootIndex);
    h.report_counter("open_done", Counter::PhaseOpen);
    h.report("value", |h| h.load(Src::Root(0), 0));
}

#[test]
fn production_relocation_defers_frame_roots_and_charges_each_discovered_cell() {
    for worker in [0, 1] {
        let o = run(many_roots, worker, false);
        assert_eq!(o.get("initial_cursor"), 1);
        assert_eq!(o.get("bound"), 1001);
        assert_eq!(o.get("initial_mutator"), 0);
        assert_eq!(o.get("initial_worker"), 0);
        assert_eq!(o.get("done7"), 0);
        assert_eq!(o.get("cursor7"), 8);
        assert_eq!(o.get("mutator7"), if worker == 0 { 7 } else { 0 });
        assert_eq!(o.get("worker7"), if worker == 0 { 0 } else { 7 });
        assert_eq!(o.get("unvisited_value"), 42);
        assert_eq!(o.get("unvisited_id"), 1000);
        assert_eq!(o.get("cursor_after_read"), 8);
        assert_eq!(o.get("cursor_done"), o.get("bound"));
        assert_eq!(o.get("open_done"), 0);
        assert_eq!(o.get("value"), 42);
    }
}

fn late_publication(g: &mut NativeCodeGenerator, field: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.value(Src::Raw(1));
    h.keep(2);
    h.root(1);
    h.collect();
    h.value(Src::Null);
    h.set_root(1);
    if field != 0 {
        // Finish scanning the current holder before publishing a new edge.
        h.value(Src::Root(0));
        h.relocate_quantum(64, false);
        h.keep(3);
        h.report("scanned", |h| h.value(Src::Raw(3)));
        h.store(Src::Root(0), 0, Src::Raw(2), true);
    } else {
        h.value(Src::Raw(2));
        h.keep(3);
        h.root(3); // A new cell beyond the captured root bound.
    }
    h.finish_collection();
    h.report("value", |h| {
        if field != 0 {
            h.deref_field(Src::Root(0), 0, 0);
        } else {
            h.load(Src::Root(3), 0);
        }
    });
}

#[test]
fn production_late_root_and_field_publications_resolve_before_current_color() {
    for field in [0, 1] {
        let o = run(late_publication, field, false);
        if field != 0 {
            assert_eq!(o.get("scanned"), 1);
        }
        assert_eq!(o.get("value"), 84, "{field}: {}", o.text());
    }
}

fn stale_heal(g: &mut NativeCodeGenerator, reuse: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 99);
    h.collect();
    h.value(Src::Root(0));
    word(&mut h, 0);
    h.keep(3);
    if reuse == 0 {
        h.value(Src::Raw(1));
        h.set_root(0);
    } else {
        h.pop_root();
        h.value(Src::Raw(1));
        h.keep(0);
        h.root(0);
    }
    h.report("replacement", |h| word(h, 0));
    h.g.call_label(h.l.lock);
    h.g.load_rbp_slot(Reg::V10, slot_offset(0));
    h.value(Src::Raw(3));
    h.g.mov_reg_reg(Reg::V1, Reg::V0);
    heal_relocated_root(h.g);
    h.g.call_label(h.l.unlock);
    h.report("after", |h| word(h, 0));
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
}

#[test]
fn production_root_heal_preserves_an_intervening_store_or_pop_reuse() {
    for reuse in [0, 1] {
        let o = run(stale_heal, reuse, false);
        assert_eq!(o.get("after"), o.get("replacement"));
        assert_eq!(o.get("value"), 99);
    }
}

fn real_worker(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.repeat(1000, |h| {
        h.alloc(1, 16, 0);
        h.set_scalar(Src::Raw(0), 0, 42);
        h.root(0);
    });
    h.collect();
    h.alloc(1, 16, 1); // Wake the worker; the mutator never calls cheney.
    crate::gc_codegen::production::test_support::publish_primary(h.g);
    let done = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::PhaseOpen);
        h.g.test_reg_reg(Reg::V0, Reg::V0);
        h.g.jcc_label(Condition::Equal, done);
        h.counter(Counter::WorkerDone);
        h.g.test_reg_reg(Reg::V0, Reg::V0);
        h.g.jcc_label(Condition::NotEqual, done);
        h.g.mov_imm64(Reg::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(done);
    h.report_counter("cursor", Counter::RelocateRootIndex);
    h.report_counter("bound", Counter::RelocateRootBound);
    h.report_counter("mutator", Counter::MutatorCopies);
    h.report_counter("worker", Counter::WorkerCopies);
    h.report_counter("objects", Counter::WorkerScanned);
    h.report("value", |h| h.load(Src::Root(0), 0));
}

#[test]
fn production_worker_relocates_frame_roots_without_mutator_scan_assistance() {
    let o = run(real_worker, 0, true);
    assert_eq!(o.get("cursor"), o.get("bound"));
    assert_eq!(o.get("worker"), 1000);
    assert_eq!(o.get("mutator"), 0);
    assert!(
        (1000..=1001).contains(&o.get("objects")),
        "root cells are not objects: {}",
        o.text()
    );
    assert_eq!(o.get("value"), 42);
}

fn allocation_handoff(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.repeat(1000, |h| {
        h.alloc(1, 16, 0);
        h.set_scalar(Src::Raw(0), 0, 42);
        h.root(0);
    });
    h.collect();
    let missing = h.g.create_text_label();
    let finish = h.g.create_text_label();
    h.counter(Counter::TlabPage);
    h.g.test_reg_reg(Reg::V0, Reg::V0);
    h.g.jcc_label(Condition::Equal, missing);
    // Deterministically model a live worker descheduled during this leaf.
    // No real thread runs in this fixture; the independent fixture above
    // verifies OS-worker completion. The guard avoids a hang in mutations.
    crate::gc_codegen::production::test_support::set_worker_alive(h.g, true);
    h.alloc(1, 16, 1);
    crate::gc_codegen::production::test_support::set_worker_alive(h.g, false);
    h.set_scalar(Src::Raw(1), 0, 99);
    h.report("resumed", |h| h.value(Src::Imm(1)));
    h.report_counter("cursor", Counter::RelocateRootIndex);
    h.report_counter("bound", Counter::RelocateRootBound);
    h.report_counter("open", Counter::PhaseOpen);
    h.report_counter("stalls", Counter::AllocationStalls);
    h.g.mov_imm64(Reg::V0, 0);
    h.keep(4);
    h.repeat(100, |h| {
        h.value(Src::Raw(4));
        h.g.add_reg_imm32(Reg::V0, 1);
        h.keep(4);
    });
    h.report("progress", |h| h.value(Src::Raw(4)));
    h.report("fresh", |h| h.load(Src::Raw(1), 0));
    h.g.jmp_label(finish);
    h.g.bind_text_label(missing);
    h.report("resumed", |h| h.value(Src::Imm(0)));
    h.g.bind_text_label(finish);
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
}

#[test]
fn production_initial_tlab_allows_allocation_before_deferred_roots_finish() {
    let o = run(allocation_handoff, 0, false);
    assert_eq!(o.get("resumed"), 1, "{}", o.text());
    assert_eq!(o.get("cursor"), 1);
    assert_eq!(o.get("bound"), 1001);
    assert_eq!(o.get("open"), 1);
    assert_eq!(o.get("stalls"), 0);
    assert_eq!(o.get("progress"), 100);
    assert_eq!(o.get("fresh"), 99);
    assert_eq!(o.get("value"), 42);
}
