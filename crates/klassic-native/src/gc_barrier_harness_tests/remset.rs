//! G07: the double-buffered remembered sets, observed on the real emitted
//! runtime. Old objects are made the honest way (two collections), then
//! `field_store` writes into them.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::Counter;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

const X: u32 = 0;
const W: u32 = 1;
const Y: u32 = 2;
const HOLD: u32 = 3;
const HEAD: u32 = 4;
const NODE: u32 = 5;
const MAGIC_Y: u64 = 0x7a7a;
const MAGIC_A: u64 = 0x5151;
const MAGIC_B: u64 = 0x6262;

fn young(h: &mut H, slot: u32, magic: u64) {
    h.alloc(2, 16, slot);
    h.set_scalar(Src::Raw(slot), 1, magic);
}

/// Two Old objects X and W; stores into X.
fn registration(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, X);
    h.alloc(2, 16, W);
    h.root(X);
    h.root(W);
    h.gc_until(2);
    h.settle();
    h.report("x_old", |h| h.is_old(Src::Root(X)));
    h.report("w_old", |h| h.is_old(Src::Root(W)));
    h.report_counter("len_start", Counter::RemsetLen);
    young(&mut h, Y, MAGIC_Y);
    h.alloc(2, 16, HOLD);
    h.report("y_old", |h| h.is_old(Src::Raw(Y)));
    // Old field <- Young referent: registered.
    h.store(Src::Root(X), 0, Src::Raw(Y), true);
    h.report_counter("len_after_young_store", Counter::RemsetLen);
    h.report("entry0_is_x_f0", |h| h.entry_is_slot(0, Src::Root(X), 0));
    // Old field <- null, a non-heap word, an Old object; Young field <-
    // Young referent: none of these registers anything.
    h.store(Src::Root(X), 1, Src::Null, true);
    h.store(Src::Root(X), 1, Src::Imm(0x1235), true);
    h.store(Src::Root(X), 1, Src::Root(W), true);
    h.store(Src::Raw(HOLD), 0, Src::Raw(Y), true);
    h.report_counter("len_after_other_stores", Counter::RemsetLen);
    // The same slot stored a thousand times is still one entry.
    h.repeat(1000, |h| h.store(Src::Root(X), 0, Src::Raw(Y), true));
    h.report_counter("len_after_repeats", Counter::RemsetLen);
    // Two slots stored alternately: two entries, however long it runs.
    h.repeat(500, |h| {
        h.store(Src::Root(X), 0, Src::Raw(Y), true);
        h.store(Src::Root(X), 1, Src::Raw(Y), true);
    });
    h.report_counter("len_after_alternating", Counter::RemsetLen);
}

#[test]
fn only_old_to_young_stores_register_and_a_slot_registers_once() {
    let o = run(registration, 0, false);
    assert_eq!(o.get("x_old"), 1, "{}", o.text());
    assert_eq!(o.get("w_old"), 1);
    assert_eq!(o.get("y_old"), 0);
    assert_eq!(o.get("len_start"), 0);
    assert_eq!(o.get("len_after_young_store"), 1);
    assert_eq!(o.get("entry0_is_x_f0"), 1);
    assert_eq!(
        o.get("len_after_other_stores"),
        1,
        "null, a non-heap word, an Old referent and a Young holder must not register: {}",
        o.text()
    );
    assert_eq!(o.get("len_after_repeats"), 1);
    assert_eq!(o.get("len_after_alternating"), 2, "{}", o.text());
}

/// An entry survives the flip while its referent lives, is dropped once the
/// referent is Old or the slot no longer holds a Young address, and a slot
/// re-stored after being nulled registers again (Old.slot = null -> Young A
/// -> null -> flip -> Young B keeps B alive through that slot only).
fn lifecycle(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, X);
    h.root(X);
    h.gc_until(2);
    h.settle();
    young(&mut h, Y, MAGIC_Y);
    h.store(Src::Root(X), 0, Src::Raw(Y), true);
    h.store(Src::Root(X), 1, Src::Raw(Y), true);
    h.store(Src::Root(X), 1, Src::Null, true);
    h.report_counter("len_before", Counter::RemsetLen);
    h.report_counter("flips_before", Counter::RemsetFlips);
    h.report_counter("flipped_before", Counter::RemsetFlipped);
    h.report_counter("kept_before", Counter::RemsetKept);
    // Collection 3: Y survives for the first time and stays Young.
    h.gc_until(3);
    h.settle();
    h.report_counter("flips_3", Counter::RemsetFlips);
    h.report_counter("flipped_3", Counter::RemsetFlipped);
    h.report_counter("kept_3", Counter::RemsetKept);
    h.report_counter("len_3", Counter::RemsetLen);
    h.report("entry0_is_x_f0_3", |h| h.entry_is_slot(0, Src::Root(X), 0));
    h.report("y_moved_3", |h| h.field_is(Src::Root(X), 0, Src::Raw(Y)));
    h.report("y_old_3", |h| h.field_is_old(Src::Root(X), 0));
    h.report("y_magic_3", |h| h.deref_field(Src::Root(X), 0, 1));
    // Collection 4: Y survives again and is promoted; the entry goes away.
    h.gc_until(4);
    h.settle();
    h.report_counter("flipped_4", Counter::RemsetFlipped);
    h.report_counter("kept_4", Counter::RemsetKept);
    h.report_counter("len_4", Counter::RemsetLen);
    h.report("y_old_4", |h| h.field_is_old(Src::Root(X), 0));
    h.report("y_magic_4", |h| h.deref_field(Src::Root(X), 0, 1));
    // Young A stored, then the slot nulled: the entry is dropped at the flip.
    young(&mut h, Y, MAGIC_A);
    h.store(Src::Root(X), 0, Src::Raw(Y), true);
    h.store(Src::Root(X), 0, Src::Null, true);
    h.report_counter("len_5_before", Counter::RemsetLen);
    h.gc_until(5);
    h.settle();
    h.report_counter("len_5", Counter::RemsetLen);
    h.report_counter("kept_5", Counter::RemsetKept);
    // Young B stored into the same slot: registered again, and alive only
    // through it.
    young(&mut h, Y, MAGIC_B);
    h.store(Src::Root(X), 0, Src::Raw(Y), true);
    h.report_counter("len_6_before", Counter::RemsetLen);
    h.gc_until(6);
    h.settle();
    h.report_counter("len_6", Counter::RemsetLen);
    h.report_counter("kept_6", Counter::RemsetKept);
    h.report("b_magic_6", |h| h.deref_field(Src::Root(X), 0, 1));
    h.report("b_old_6", |h| h.field_is_old(Src::Root(X), 0));
}

#[test]
fn entries_flip_survive_while_young_and_are_dropped_when_old_or_dead() {
    let o = run(lifecycle, 0, false);
    // X.f0 and the nulled X.f1 are both registered going in.
    assert_eq!(o.get("len_before"), 2, "{}", o.text());
    // Collection 3: one flip consumes both entries; only X.f0 (a live Young
    // referent) is re-registered, and Y survived through it alone.
    assert_eq!(o.get("flips_3"), o.get("flips_before") + 1);
    assert_eq!(o.get("flipped_3"), o.get("flipped_before") + 2);
    assert_eq!(o.get("kept_3"), o.get("kept_before") + 1);
    assert_eq!(o.get("len_3"), 1);
    assert_eq!(o.get("entry0_is_x_f0_3"), 1);
    assert_eq!(o.get("y_moved_3"), 0, "Y must have been evacuated");
    assert_eq!(o.get("y_old_3"), 0, "first survival stays Young");
    assert_eq!(o.get("y_magic_3"), MAGIC_Y as i64);
    // Collection 4: Y is promoted, so its entry is not kept.
    assert_eq!(o.get("flipped_4"), o.get("flipped_3") + 1);
    assert_eq!(o.get("kept_4"), o.get("kept_3"));
    assert_eq!(o.get("len_4"), 0, "{}", o.text());
    assert_eq!(o.get("y_old_4"), 1);
    assert_eq!(o.get("y_magic_4"), MAGIC_Y as i64);
    // Nulled slot: the entry stays until the flip, then is not kept.
    assert_eq!(o.get("len_5_before"), 1);
    assert_eq!(o.get("len_5"), 0);
    assert_eq!(o.get("kept_5"), o.get("kept_4"));
    // The same slot, re-stored, is a fresh entry whose referent lives on.
    assert_eq!(o.get("len_6_before"), 1);
    assert_eq!(o.get("len_6"), 1);
    assert_eq!(o.get("kept_6"), o.get("kept_5") + 1);
    assert_eq!(o.get("b_magic_6"), MAGIC_B as i64);
    assert_eq!(o.get("b_old_6"), 0);
}

/// X is Old with an entry for a Young Y, but nothing reaches X any more and
/// a complete mark says so: the entry is dropped (counted) instead of
/// keeping Y alive, and Y is not copied.
fn dead_holder(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, X);
    h.root(X);
    h.gc_until(2);
    h.settle();
    young(&mut h, Y, MAGIC_Y);
    h.store(Src::Root(X), 0, Src::Raw(Y), true);
    h.report_counter("len_before", Counter::RemsetLen);
    h.pop_root();
    h.mark_start();
    h.report_counter("dropped_before", Counter::RemsetDropped);
    h.gc_until(3);
    h.settle();
    h.report_counter("dropped_after", Counter::RemsetDropped);
    h.report_counter("len_after", Counter::RemsetLen);
    h.report_counter("kept", Counter::RemsetKept);
    h.report_counter("unmarked_copied", Counter::Unmarked);
}

#[test]
fn an_entry_of_a_dead_holder_is_dropped_by_a_complete_mark() {
    let o = run(dead_holder, 0, false);
    assert_eq!(o.get("len_before"), 1, "{}", o.text());
    assert_eq!(o.get("dropped_after"), o.get("dropped_before") + 1);
    assert_eq!(o.get("len_after"), 0);
    assert_eq!(o.get("kept"), 0);
    assert_eq!(o.get("unmarked_copied"), 0);
}

/// Grow a live chain until Old fills and a full collection runs, storing a
/// fresh Young object into the Old holder X on every step so an entry is
/// live whenever a collection starts. The full collection moves X; both sets
/// are discarded rather than left pointing into the evacuated half, and
/// stores afterwards register X's slot at its *new* address.
fn full_collection(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, X);
    h.root(X);
    h.alloc(2, 16, HEAD);
    h.root(HEAD);
    h.gc_until(2);
    h.settle();
    h.report("x_old_before", |h| h.is_old(Src::Root(X)));
    h.report("x_before", |h| h.value(Src::Root(X)));
    let top = h.g.create_text_label();
    h.g.bind_text_label(top);
    h.alloc(2, 30000, NODE);
    h.store(Src::Raw(NODE), 0, Src::Root(HEAD), true);
    h.value(Src::Raw(NODE));
    h.set_root(HEAD);
    young(&mut h, Y, MAGIC_A);
    h.store(Src::Root(X), 1, Src::Raw(Y), true);
    h.alloc(1, 60000, NODE);
    h.counter(Counter::FullCollections);
    h.g.test_reg_reg(R::V0, R::V0);
    h.g.jcc_label(C::Equal, top);
    h.settle();
    h.report_counter("full_collections", Counter::FullCollections);
    h.report_counter("len_after_full", Counter::RemsetLen);
    h.report("x_after", |h| h.value(Src::Root(X)));
    h.report("x_old_after", |h| h.is_old(Src::Root(X)));
    h.report("entry0_is_x_f1", |h| h.entry_is_slot(0, Src::Root(X), 1));
    h.report("f1_magic", |h| h.deref_field(Src::Root(X), 1, 1));
    // Further collections consume whatever the sets hold; a dangling entry
    // (a slot in the zeroed, evacuated half) would corrupt or crash here.
    young(&mut h, Y, MAGIC_B);
    h.store(Src::Root(X), 0, Src::Raw(Y), true);
    h.gc_more(3);
    h.settle();
    h.report("f0_magic_later", |h| h.deref_field(Src::Root(X), 0, 1));
    h.report("f1_magic_later", |h| h.deref_field(Src::Root(X), 1, 1));
    h.report_counter("len_later", Counter::RemsetLen);
}

#[test]
fn a_full_collection_discards_both_sets_and_new_stores_use_the_moved_slot() {
    let o = run(full_collection, 0, false);
    assert_eq!(o.get("x_old_before"), 1, "{}", o.text());
    assert!(o.get("full_collections") >= 1, "{}", o.text());
    assert_ne!(o.get("x_before"), o.get("x_after"), "X must have moved");
    assert_eq!(o.get("x_old_after"), 1);
    // Either no store followed the full collection (0 entries) or the one
    // that did registered X.f1 at its new address.
    let len = o.get("len_after_full");
    assert!(len <= 1, "{}", o.text());
    if len == 1 {
        assert_eq!(o.get("entry0_is_x_f1"), 1, "{}", o.text());
    }
    assert_eq!(o.get("f1_magic"), MAGIC_A as i64);
    assert_eq!(o.get("f0_magic_later"), MAGIC_B as i64);
    assert_eq!(o.get("f1_magic_later"), MAGIC_A as i64);
    assert!(o.get("len_later") <= 2);
}

/// With the real worker thread running (marking and relocating concurrently
/// with the mutator), two Old slots are overwritten with fresh Young
/// objects thousands of times while collections come and go. The sets stay
/// bounded by the two distinct slots and the last values survive.
fn racing_stores(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, X);
    h.root(X);
    h.gc_until(2);
    h.settle();
    h.repeat(3000, |h| {
        young(h, Y, MAGIC_A);
        h.store(Src::Root(X), 0, Src::Raw(Y), true);
        h.alloc(1, 60000, HOLD);
        young(h, Y, MAGIC_B);
        h.store(Src::Root(X), 1, Src::Raw(Y), true);
        h.alloc(1, 60000, HOLD);
    });
    h.settle();
    h.report("a_magic", |h| h.deref_field(Src::Root(X), 0, 1));
    h.report("b_magic", |h| h.deref_field(Src::Root(X), 1, 1));
    h.report_counter("len", Counter::RemsetLen);
    h.report_counter("collections", Counter::Collections);
    h.report_counter("mark_cycles", Counter::MarkCycles);
    h.report_counter("worker_scanned", Counter::WorkerScanned);
    h.report_counter("flips", Counter::RemsetFlips);
}

#[test]
fn stores_racing_with_the_worker_keep_the_sets_bounded_and_the_values_alive() {
    let o = run(racing_stores, 0, true);
    assert_eq!(o.get("a_magic"), MAGIC_A as i64, "{}", o.text());
    assert_eq!(o.get("b_magic"), MAGIC_B as i64, "{}", o.text());
    assert!(o.get("len") <= 2, "{}", o.text());
    assert!(o.get("collections") >= 5, "{}", o.text());
    assert!(o.get("flips") >= 5);
    assert!(o.get("worker_scanned") > 0, "the worker must have run");
}
