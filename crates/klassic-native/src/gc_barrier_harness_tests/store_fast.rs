//! Actual root/heap stores retain SATB duties and skip protected referents.
use super::{H, Src, run, slot_offset};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::{Counter, completed_mark_hint, enqueue_mark};
use crate::portable_asm::{PortableAsm, Reg as R};

fn setup(h: &mut H<'_>, root: bool) {
    h.alloc(2, 16, 0);
    h.alloc(1, 16, 1);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(1), 0, 42);
    h.set_scalar(Src::Raw(2), 0, 84);
    if root {
        h.value(Src::Raw(1));
        h.keep(0);
    } else {
        h.value(Src::Raw(0));
        h.keep(3);
        h.store(Src::Raw(3), 0, Src::Raw(1), true);
    }
    h.root(0);
    h.mark_start();
}
fn admit(h: &mut H<'_>, slot: u32) {
    h.g.call_label(h.l.lock);
    h.value(Src::Raw(slot));
    enqueue_mark(h.g, h.l.mark_ref);
    h.g.call_label(h.l.unlock);
}
fn snapshot(h: &mut H<'_>) {
    completed_mark_hint(h.g); // A pending hint is independently rechecked at end.
    for (slot, counter) in [
        (20, Counter::LockTickets),
        (21, Counter::WorkSequence),
        (22, Counter::Marked),
        (23, Counter::MarkStackLen),
    ] {
        h.counter(counter);
        h.keep(slot);
    }
}
fn delta(h: &mut H<'_>, name: &str, counter: Counter, slot: u32) {
    h.report(name, |h| {
        h.counter(counter);
        h.g.load_rbp_slot(R::V1, slot_offset(slot));
        h.g.sub_reg_reg(R::V0, R::V1);
    });
}
fn report(h: &mut H<'_>, root: bool) {
    for (name, counter, slot) in [
        ("tickets", Counter::LockTickets, 20),
        ("requests", Counter::WorkSequence, 21),
        ("marked", Counter::Marked, 22),
        ("queued", Counter::MarkStackLen, 23),
    ] {
        delta(h, name, counter, slot);
    }
    h.report_counter("hint", Counter::MarkDone);
    h.report_counter("wake", Counter::WakePending);
    h.report("previous", |h| h.marked(Src::Raw(1)));
    h.report("next", |h| h.marked(Src::Raw(2)));
    h.report("value", |h| {
        if root {
            h.load(Src::Root(0), 0);
        } else {
            h.deref_field(Src::Raw(3), 0, 0);
        }
    });
}
fn store(h: &mut H<'_>, root: bool) {
    if root {
        h.value(Src::Raw(2));
        h.set_root(0);
    } else {
        h.store(Src::Raw(3), 0, Src::Raw(2), true);
    }
}
fn combinations(g: &mut NativeCodeGenerator, variant: u64) {
    let mut h = H::begin(g);
    let root = variant & 4 != 0;
    setup(&mut h, root);
    for (bit, slot) in [(1, 1), (2, 2)] {
        if variant & bit != 0 {
            admit(&mut h, slot);
        }
    }
    snapshot(&mut h);
    store(&mut h, root);
    report(&mut h, root);
    h.drain_marks();
    h.report("retained", |h| h.marked(Src::Raw(1)));
}
#[test]
fn production_store_epoch_check_skips_only_already_protected_old_and_new_values() {
    for variant in 0u64..8 {
        let o = run(combinations, variant, false);
        let pending = 2 - (variant & 3).count_ones() as i64;
        for key in ["tickets", "requests", "marked", "queued"] {
            assert_eq!(o.get(key), pending, "{variant}/{key}: {}", o.text());
        }
        assert_eq!(o.get("hint"), i64::from(pending == 0));
        assert_eq!(o.get("wake"), i64::from(pending == 0));
        for key in ["previous", "next", "retained"] {
            assert_eq!(o.get(key), 1);
        }
        assert_eq!(o.get("value"), 84);
    }
}
fn protected(g: &mut NativeCodeGenerator, root: u64) {
    let root = root != 0;
    let mut h = H::begin(g);
    setup(&mut h, root);
    admit(&mut h, 1);
    admit(&mut h, 2);
    h.drain_marks();
    snapshot(&mut h);
    h.repeat(100_000, |h| store(h, root));
    report(&mut h, root);
}
fn check_protected(root: bool) {
    let o = run(protected, u64::from(root), false);
    for key in ["tickets", "requests", "marked", "queued"] {
        assert_eq!(o.get(key), 0, "{key}: {}", o.text());
    }
    assert_eq!(o.get("hint"), 1);
    assert_eq!(o.get("value"), 84);
}
#[test]
fn production_protected_heap_stores_do_not_lock_or_rearm_old_marking() {
    check_protected(false);
}
#[test]
fn production_protected_root_stores_do_not_lock_or_rearm_old_marking() {
    check_protected(true);
}

fn stale(g: &mut NativeCodeGenerator, mode: u64) {
    use crate::gc_abi::{MarkRecord, ObjectHeader};
    use crate::gc_codegen::production::test_support::set_mark_cycles;
    use std::mem::{offset_of, size_of};
    let root = mode != 0;
    let mut h = H::begin(g);
    setup(&mut h, root);
    admit(&mut h, 1);
    admit(&mut h, 2);
    h.value(Src::Raw(2));
    h.keep(4);
    h.root(4);
    h.drain_marks();
    h.end_marks();
    h.collect();
    h.finish_collection();
    if root {
        h.value(Src::Root(0));
    } else {
        h.value(Src::Root(0));
        h.keep(3);
        h.load_barriered(Src::Raw(3), 0);
    }
    h.keep(1);
    h.value(Src::Root(4));
    h.keep(2);
    // Both Old stamps are one; three shares its parity but is a new epoch.
    // A matching Young stamp must not establish Old protection either.
    set_mark_cycles(h.g, 2);
    h.mark_start();
    for slot in [1, 2] {
        h.value(Src::Raw(slot));
        h.g.load_ptr_disp32(
            R::V1,
            R::V0,
            offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
        );
        h.g.add_reg_imm32(R::V1, offset_of!(MarkRecord, young_epoch) as i32);
        h.g.mov_imm64(R::V0, 3);
        h.g.atomic_store_release(R::V1, R::V0);
    }
    snapshot(&mut h);
    store(&mut h, root);
    report(&mut h, root);
    for slot in [1, 2] {
        h.report(&format!("epoch{slot}"), |h| {
            h.object_metadata(Src::Raw(slot), offset_of!(MarkRecord, old_epoch))
        });
    }
}
#[test]
fn production_store_fast_check_rejects_stale_old_and_matching_young_epochs() {
    for root in [0, 1] {
        let o = run(stale, root, false);
        for key in ["tickets", "requests", "marked", "queued"] {
            assert_eq!(o.get(key), 2, "{root}/{key}: {}", o.text());
        }
        assert_eq!(o.get("epoch1"), 3);
        assert_eq!(o.get("epoch2"), 3);
        assert_eq!(o.get("value"), 84);
    }
}
fn remembered(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.value(Src::Root(0));
    h.keep(3); // Stable Old owner; both slots are initially null.
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 84);
    h.mark_start();
    admit(&mut h, 2);
    snapshot(&mut h);
    h.repeat(1000, |h| store(h, false));
    h.report_counter("first_set", Counter::RemsetLen);
    delta(&mut h, "first_tickets", Counter::LockTickets, 20);
    delta(&mut h, "first_requests", Counter::WorkSequence, 21);
    h.collect();
    h.finish_collection();
    h.load_barriered(Src::Raw(3), 0);
    h.keep(2); // Canonical Young copy, whose Old epoch was carried forward.
    snapshot(&mut h);
    h.repeat(1000, |h| h.store(Src::Raw(3), 1, Src::Raw(2), true));
    h.report_counter("second_set", Counter::RemsetLen);
    delta(&mut h, "second_tickets", Counter::LockTickets, 20);
    delta(&mut h, "second_requests", Counter::WorkSequence, 21);
    h.collect();
    h.finish_collection();
    h.report_counter("promoted_set", Counter::RemsetLen);
    h.report("value", |h| h.deref_field(Src::Root(0), 1, 0));
    h.report("old", |h| h.field_is_old(Src::Root(0), 1));
}
#[test]
fn production_epoch_fast_stores_keep_remembered_slots_through_copy_and_promotion() {
    let o = run(remembered, 0, false);
    assert_eq!(o.get("first_set"), 1, "{}", o.text());
    assert_eq!(o.get("second_set"), 2);
    for key in [
        "first_tickets",
        "first_requests",
        "second_tickets",
        "second_requests",
    ] {
        assert_eq!(o.get(key), 0, "{key}: {}", o.text());
    }
    assert_eq!(o.get("promoted_set"), 0);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("old"), 1);
}
fn worker(g: &mut NativeCodeGenerator, _arg: u64) {
    use crate::portable_asm::Condition as C;
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.keep(3);
    h.root(0);
    h.alloc(2, 16, 1);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 84);
    h.store(Src::Raw(1), 0, Src::Raw(2), true);
    h.mark_start();
    crate::gc_codegen::production::test_support::wake_worker(h.g, h.l.unpark);
    let idle = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::MarkDone);
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(C::NotEqual, idle);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(idle);
    h.report_counter("idle", Counter::MarkDone);
    // Give the completed worker ample opportunity to enter its futex wait.
    h.repeat(1000, |h| {
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.report("before", |h| h.marked(Src::Raw(2)));
    h.store(Src::Raw(3), 0, Src::Raw(1), true); // No further allocation/root load.
    let traced = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.marked(Src::Raw(2));
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(C::NotEqual, traced);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(traced);
    h.report("child", |h| h.marked(Src::Raw(2)));
    h.report_counter("worker", Counter::MarkWorkerUnits);
    h.report("value", |h| h.load(Src::Raw(2), 0));
}
#[test]
fn production_unknown_heap_store_wakes_old_tracing_without_another_allocation() {
    let o = run(worker, 0, true);
    assert_eq!(o.get("idle"), 1, "{}", o.text());
    assert_eq!(o.get("before"), 0);
    assert_eq!(o.get("child"), 1);
    assert!(o.get("worker") > 0);
    assert_eq!(o.get("value"), 84);
}

fn certify(g: &mut NativeCodeGenerator, _arg: u64) {
    use crate::gc_codegen::production::test_support::protect_stale_root_reference;
    let mut h = H::begin(g);
    setup(&mut h, true);
    admit(&mut h, 1);
    admit(&mut h, 2);
    h.value(Src::Root(0)); // Prepare the real frame handle, closing its guard.
    snapshot(&mut h);
    h.value(Src::Raw(2));
    let runtime = h.g.gc_gen.take().unwrap();
    protect_stale_root_reference(h.g, &runtime.labels);
    h.g.gc_gen = Some(runtime);
    h.keep(5);
    delta(&mut h, "tickets", Counter::LockTickets, 20);
    delta(&mut h, "requests", Counter::WorkSequence, 21);
    h.report("reference", |h| h.value(Src::Raw(5)));
    h.report("offset", |h| {
        h.value(Src::Raw(2));
        h.g.load_ptr_disp32(
            R::V1,
            R::V3,
            crate::gc_codegen::production::test_support::heap_base_offset(),
        );
        h.g.sub_reg_reg(R::V0, R::V1);
    });
}
#[test]
fn production_fast_root_publication_recertifies_a_reference_protected_after_encoding() {
    use crate::gc_abi::{OLD_MARK_MASK, Phase, PhaseSnapshot};
    let o = run(certify, 0, false);
    assert_eq!(o.get("tickets"), 0, "{}", o.text());
    assert_eq!(o.get("requests"), 0);
    assert_eq!((o.get("reference") as u64) >> 16, o.get("offset") as u64);
    let current = PhaseSnapshot::new(Phase::Idle, Phase::ConcurrentMark, 2, 0)
        .unwrap()
        .color();
    assert_eq!(
        o.get("reference") & i64::from(OLD_MARK_MASK),
        i64::from(current & OLD_MARK_MASK)
    );
}
