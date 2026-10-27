//! G06/G03: the SATB pre-barrier in `field_store`, observed on the real
//! emitted runtime.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::Counter;

const A: u32 = 0;
const B: u32 = 1;
const C: u32 = 2;
const B_AGAIN: u32 = 3;
const MAGIC_B: u64 = 0x1b1b;

/// A.f0 = B at the snapshot. Read A and C (protecting those roots), but
/// deliberately never read B through the load barrier before overwriting it.
/// This isolates deletion SATB from load/root-publication keep-alive. After
/// observing the snapshot loss, a new strong root rescues B before relocation.
fn overwrite_while_marking(g: &mut NativeCodeGenerator, arg: u64) {
    let barrier = arg == 0;
    let mut h = H::begin(g);
    h.alloc(2, 16, A);
    h.alloc(2, 16, B);
    h.alloc(2, 16, C);
    h.set_scalar(Src::Raw(B), 1, MAGIC_B);
    h.store(Src::Raw(A), 0, Src::Raw(B), true);
    h.root(A);
    h.root(C);
    h.mark_start();
    h.report("b_marked_at_start", |h| h.marked(Src::Raw(B)));
    h.report_counter("marked_at_start", Counter::Marked);
    h.report_counter("mark_active", Counter::MarkActive);
    h.value(Src::Root(A));
    h.keep(4);
    h.value(Src::Root(C));
    h.keep(5);
    h.report_counter("marked_before_store", Counter::Marked);
    h.store(Src::Raw(4), 0, Src::Raw(5), barrier);
    h.report("b_marked_after_store", |h| h.marked(Src::Raw(B)));
    h.report_counter("marked_after_store", Counter::Marked);
    h.drain_marks();
    h.report("b_marked_after_drain", |h| h.marked(Src::Raw(B)));
    h.report("c_marked_after_drain", |h| h.marked(Src::Root(C)));
    h.report("a_marked_after_drain", |h| h.marked(Src::Root(A)));
    h.value(Src::Raw(B));
    h.keep(B_AGAIN);
    h.root(B_AGAIN); // Strong publication must protect a post-snapshot root.
    h.report("b_rescued", |h| h.marked(Src::Raw(B)));
    h.gc_until(1);
    h.settle();
    h.report_counter("collections", Counter::Collections);
    h.report_counter("unmarked_copied", Counter::Unmarked);
    h.report("b_magic_after_gc", |h| {
        h.load(Src::Root(B_AGAIN), 1);
    });
    h.report("a_f0_is_c", |h| h.field_is(Src::Root(A), 0, Src::Root(C)));
}

#[test]
fn satb_prebarrier_marks_the_overwritten_referent() {
    let o = run(overwrite_while_marking, 0, false);
    assert_eq!(o.get("mark_active"), 1, "{}", o.text());
    // B is only reachable through A.f0 and A has not been traced yet.
    assert_eq!(o.get("b_marked_at_start"), 0);
    // The overwrite marks B (and counts it) before the store lands.
    assert_eq!(o.get("b_marked_after_store"), 1);
    assert_eq!(
        o.get("marked_after_store"),
        o.get("marked_before_store") + 1,
        "the SATB mark is counted exactly once: {}",
        o.text()
    );
    assert_eq!(o.get("b_marked_after_drain"), 1);
    assert_eq!(o.get("a_marked_after_drain"), 1);
    assert_eq!(o.get("c_marked_after_drain"), 1);
    // The relocation that follows copies no object the mark missed.
    assert!(o.get("collections") >= 1);
    assert_eq!(o.get("unmarked_copied"), 0, "{}", o.text());
    assert_eq!(o.get("b_magic_after_gc"), MAGIC_B as i64);
    assert_eq!(o.get("a_f0_is_c"), 1);
}

#[test]
fn without_the_prebarrier_an_unread_snapshot_edge_is_lost() {
    // Negative control: deletion bypasses SATB, so the unread snapshot
    // referent is missed. A later strong-root publication rescues it.
    let o = run(overwrite_while_marking, 1, false);
    assert_eq!(o.get("b_marked_at_start"), 0);
    assert_eq!(o.get("b_marked_after_store"), 0, "{}", o.text());
    assert_eq!(o.get("marked_after_store"), o.get("marked_before_store"));
    assert_eq!(o.get("b_marked_after_drain"), 0, "{}", o.text());
    assert_eq!(o.get("c_marked_after_drain"), 1);
    assert!(o.get("collections") >= 1);
    assert_eq!(o.get("b_rescued"), 1);
    assert_eq!(
        o.get("unmarked_copied"),
        0,
        "new strong roots protect before publication"
    );
    assert_eq!(o.get("b_magic_after_gc"), MAGIC_B as i64);
}

/// Overwrites that must not mark: null, an external/scalar word, and any
/// overwrite while no mark cycle is active.
fn overwrites_that_do_not_mark(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, A);
    h.alloc(2, 16, B);
    h.alloc(2, 16, C);
    h.store(Src::Raw(A), 0, Src::Raw(B), true);
    h.store(Src::Raw(A), 1, Src::Imm(0x1235), true);
    h.root(A);
    h.root(C);
    // No mark cycle: the overwrite of B does not mark it.
    h.store(Src::Root(A), 0, Src::Root(C), true);
    h.report("b_marked_without_cycle", |h| h.marked(Src::Raw(B)));
    h.report_counter("marked_without_cycle", Counter::Marked);
    h.mark_start();
    // Root discovery is asynchronous now. Finish it so C is already marked
    // and this fixture isolates null/scalar stores rather than root discovery.
    h.drain_marks();
    h.report_counter("marked_at_start", Counter::Marked);
    // Previous value null: nothing to mark.
    h.store(Src::Root(A), 0, Src::Null, true);
    h.store(Src::Root(A), 0, Src::Root(C), true);
    // Previous value a scalar outside the heap: nothing to mark.
    h.store(Src::Root(A), 1, Src::Null, true);
    h.report_counter("marked_after_stores", Counter::Marked);
    h.report("b_marked_after_stores", |h| h.marked(Src::Raw(B)));
}

#[test]
fn overwriting_null_scalars_or_outside_a_cycle_marks_nothing() {
    let o = run(overwrites_that_do_not_mark, 0, false);
    assert_eq!(o.get("b_marked_without_cycle"), 0, "{}", o.text());
    assert_eq!(o.get("marked_without_cycle"), 0);
    // Overwriting A.f0 (= C, already marked as a root) during the cycle is
    // the only SATB mark candidate, and it is already marked.
    assert_eq!(
        o.get("marked_after_stores"),
        o.get("marked_at_start"),
        "{}",
        o.text()
    );
    assert_eq!(o.get("b_marked_after_stores"), 0);
}
