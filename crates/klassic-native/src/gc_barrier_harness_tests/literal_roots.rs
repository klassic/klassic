//! Permanent native-data cells use the compiler's real root-aware accessors.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::Counter;
use crate::portable_asm::{Condition, PortableAsm, Reg};
use crate::{NativeCodeGenerator, NativeValue, Reg as X};

fn budgeted(g: &mut NativeCodeGenerator, actor: u64) {
    let cells: Vec<_> = (0..1000)
        .map(|_| g.new_literal_scalar_slot(NativeValue::HeapPointer))
        .collect();
    let mut h = H::begin(g);
    h.report("null", |h| h.g.emit_load_literal_slot(X::Rax, cells[999]));
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    for cell in &cells {
        h.value(Src::Raw(0));
        h.g.emit_store_rax_to_data_slot(*cell);
    }
    h.mark_start();
    h.report_counter("mark_initial", Counter::Marked);
    h.report_counter("mark_index", Counter::MarkRootIndex);
    h.report_counter("mark_bound", Counter::MarkRootBound);
    h.mark_quantum(1);
    h.report_counter("mark_one_index", Counter::MarkRootIndex);
    h.report_counter("mark_one", Counter::Marked);
    h.collect();
    h.report_counter("relocate_index", Counter::RelocateRootIndex);
    h.report_counter("relocate_bound", Counter::RelocateRootBound);
    h.report_counter("initial_mutator", Counter::MutatorCopies);
    h.report_counter("initial_worker", Counter::WorkerCopies);
    h.relocate_quantum(7, actor != 0);
    h.keep(2);
    h.report("done7", |h| h.value(Src::Raw(2)));
    h.report_counter("index7", Counter::RelocateRootIndex);
    h.report("unvisited", |h| {
        h.g.emit_load_literal_slot(X::Rax, cells[999]);
        h.g.load_ptr_disp32(Reg::V0, Reg::V0, 0);
    });
    h.report_counter("after_read", Counter::RelocateRootIndex);
    h.finish_collection();
    h.report("value", |h| {
        h.g.emit_load_literal_slot(X::Rax, cells[999]);
        h.g.load_ptr_disp32(Reg::V0, Reg::V0, 0);
    });
    h.report_counter("finished", Counter::PhaseOpen);
}

#[test]
fn production_literal_roots_share_budgeted_mark_and_relocation_discovery() {
    for actor in [0, 1] {
        let o = run(budgeted, actor, false);
        assert_eq!(o.get("null"), 0);
        assert_eq!(o.get("mark_initial"), 0);
        assert_eq!(o.get("mark_index"), 1);
        assert_eq!(o.get("mark_bound"), 1001);
        assert_eq!(o.get("mark_one_index"), 2);
        assert_eq!(o.get("mark_one"), 1);
        assert_eq!(o.get("relocate_index"), 1);
        assert_eq!(o.get("relocate_bound"), 1001);
        assert_eq!(o.get("initial_mutator"), 0);
        assert_eq!(o.get("initial_worker"), 0);
        assert_eq!(o.get("done7"), 0);
        assert_eq!(o.get("index7"), 6); // Active receiver + queued ref cost two units.
        assert_eq!(o.get("unvisited"), 42);
        assert_eq!(o.get("after_read"), 6);
        assert_eq!(o.get("value"), 42);
        assert_eq!(o.get("finished"), 0);
    }
}

fn deletion(g: &mut NativeCodeGenerator, _arg: u64) {
    let cell = g.new_literal_scalar_slot(NativeValue::HeapPointer);
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Raw(0), 0, Src::Raw(1), true);
    h.value(Src::Raw(0));
    h.g.emit_store_rax_to_data_slot(cell);
    h.mark_start();
    h.report_counter("initial", Counter::Marked);
    h.value(Src::Null);
    h.g.emit_store_rax_to_data_slot(cell);
    h.drain_marks();
    h.report("holder", |h| h.marked(Src::Raw(0)));
    h.report("child", |h| h.marked(Src::Raw(1)));
    h.report("null", |h| h.g.emit_load_literal_slot(X::Rax, cell));
    // Publish from a retained raw referent while its permanent cell is not yet
    // visited by relocation. Resolution must precede the new color bits.
    h.value(Src::Raw(0));
    h.g.emit_store_rax_to_data_slot(cell);
    h.collect();
    h.value(Src::Raw(0));
    h.g.emit_store_rax_to_data_slot(cell);
    h.finish_collection();
    h.g.emit_load_literal_slot(X::Rax, cell);
    h.keep(2);
    h.report("value", |h| h.deref_field(Src::Raw(2), 0, 0));
}

#[test]
fn production_literal_deletion_uses_satb_and_late_stores_resolve_before_publish() {
    let o = run(deletion, 0, false);
    assert_eq!(o.get("initial"), 0);
    assert_eq!(o.get("holder"), 1);
    assert_eq!(o.get("child"), 1);
    assert_eq!(o.get("null"), 0);
    assert_eq!(o.get("value"), 84);
}

fn access_contract(g: &mut NativeCodeGenerator, _arg: u64) {
    let cell = g.new_literal_scalar_slot(NativeValue::HeapPointer);
    let scalar = g.new_literal_scalar_slot(NativeValue::Int);
    let mut h = H::begin(g);
    h.g.asm.mov_data_addr(X::R10, cell);
    h.g.asm.load_ptr_disp32(X::Rax, X::R10, 0);
    h.keep(2);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.value(Src::Raw(0));
    h.g.asm.mov_imm64(X::Rcx, 1234);
    h.g.asm.cmp_reg_reg(X::Rax, X::Rax);
    h.g.emit_store_rax_to_data_slot(cell);
    h.g.asm.setcc_al(Condition::Equal.into());
    h.g.asm.movzx_rax_al();
    h.keep(5);
    h.g.asm.mov_reg_reg(X::Rax, X::Rcx);
    h.keep(6);
    h.report("cell_stable", |h| {
        h.g.asm.mov_data_addr(X::R10, cell);
        h.g.asm.load_ptr_disp32(X::Rax, X::R10, 0);
        h.g.load_rbp_slot(Reg::V1, slot_offset(2));
        h.g.asm.cmp_reg_reg(X::Rax, X::Rcx);
        h.g.asm.setcc_al(Condition::Equal.into());
        h.g.asm.movzx_rax_al();
    });
    h.report("store_flags", |h| h.value(Src::Raw(5)));
    h.report("store_rcx", |h| h.value(Src::Raw(6)));
    h.value(Src::Imm(8595182864));
    h.g.emit_store_rax_to_data_slot(scalar);
    h.g.asm.cmp_reg_reg(X::Rax, X::Rax);
    h.g.emit_load_literal_slot(X::Rcx, cell);
    h.keep(3); // The non-result Rax must survive this load.
    h.g.asm.setcc_al(Condition::Equal.into());
    h.g.asm.movzx_rax_al();
    h.keep(5);
    h.g.asm.mov_reg_reg(X::Rax, X::Rcx);
    h.keep(4);
    h.report("load_flags", |h| h.value(Src::Raw(5)));
    h.report("load_rax", |h| h.value(Src::Raw(3)));
    h.report("payload", |h| h.load(Src::Raw(4), 0));
    h.report("scalar", |h| h.g.emit_load_literal_slot(X::Rax, scalar));
    // Ordinary LIFO frame roots sit above the permanent data cells.
    h.root(0);
    h.pop_root();
    h.collect();
    h.report_counter("bound_after_pop", Counter::RelocateRootBound);
    h.finish_collection();
    h.report("permanent", |h| {
        h.g.emit_load_literal_slot(X::Rax, cell);
        h.g.load_ptr_disp32(Reg::V0, Reg::V0, 0);
    });
}

#[test]
fn production_literal_accesses_preserve_registers_flags_scalars_and_permanent_cells() {
    let o = run(access_contract, 0, false);
    assert_eq!(o.get("cell_stable"), 1);
    assert_eq!(o.get("store_flags"), 1);
    assert_eq!(o.get("store_rcx"), 1234);
    assert_eq!(o.get("load_flags"), 1);
    assert_eq!(o.get("load_rax"), 8595182864);
    assert_eq!(o.get("payload"), 42);
    assert_eq!(o.get("scalar"), 8595182864);
    assert_eq!(o.get("bound_after_pop"), 3);
    assert_eq!(o.get("permanent"), 42);
}

fn worker(g: &mut NativeCodeGenerator, _arg: u64) {
    let cell = g.new_literal_scalar_slot(NativeValue::HeapPointer);
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.value(Src::Raw(0));
    h.g.emit_store_rax_to_data_slot(cell);
    h.collect();
    h.alloc(1, 16, 1);
    let done = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::WorkerDone);
        h.g.test_reg_reg(Reg::V0, Reg::V0);
        h.g.jcc_label(Condition::NotEqual, done);
        h.counter(Counter::PhaseOpen);
        h.g.test_reg_reg(Reg::V0, Reg::V0);
        h.g.jcc_label(Condition::Equal, done);
        h.g.mov_imm64(Reg::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(done);
    h.report_counter("mutator", Counter::MutatorCopies);
    h.report_counter("worker", Counter::WorkerCopies);
    h.report_counter("index", Counter::RelocateRootIndex);
    h.report_counter("bound", Counter::RelocateRootBound);
    h.finish_collection();
    h.report("value", |h| {
        h.g.emit_load_literal_slot(X::Rax, cell);
        h.g.load_ptr_disp32(Reg::V0, Reg::V0, 0);
    });
}

#[test]
fn production_worker_relocates_permanent_literal_roots_without_mutator_scanning() {
    let o = run(worker, 0, true);
    assert_eq!(o.get("mutator"), 0);
    assert!(o.get("worker") > 0);
    assert_eq!(o.get("index"), o.get("bound"));
    assert_eq!(o.get("value"), 42);
}
