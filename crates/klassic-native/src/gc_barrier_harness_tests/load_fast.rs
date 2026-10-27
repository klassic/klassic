//! Real load-barrier fast paths must not acquire a collector ticket or allocate.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::{Counter, enqueue_mark, weak_growth_committed};
use crate::portable_asm::{PortableAsm, Reg as R};

fn exercise(g: &mut crate::NativeCodeGenerator, marking: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.value(Src::Root(0));
    h.keep(3); // No collection occurs while this raw owner is borrowed.
    if marking != 0 {
        h.mark_start();
        h.g.call_label(h.l.lock);
        for slot in [1, 3] {
            h.value(Src::Raw(slot));
            enqueue_mark(h.g, h.l.mark_ref);
        }
        h.g.call_label(h.l.unlock);
        h.drain_marks();
    }
    // Recertify once before measuring the already-good fast path.
    h.load_barriered(Src::Raw(3), 0);
    h.keep(4);
    for (slot, counter) in [
        (20, Counter::LockTickets),
        (21, Counter::WorkSequence),
        (22, Counter::Healed),
        (23, Counter::MutatorCopies),
        (24, Counter::TlabTop),
        (28, Counter::ObjectIds),
        (29, Counter::MetadataMaps),
    ] {
        h.counter(counter);
        h.keep(slot);
    }
    weak_growth_committed(h.g);
    h.keep(25);
    h.repeat(100_000, |h| {
        h.load_barriered(Src::Raw(3), 0);
    });
    for (name, slot, counter) in [
        ("tickets", 20, Counter::LockTickets),
        ("requests", 21, Counter::WorkSequence),
        ("healed", 22, Counter::Healed),
        ("copies", 23, Counter::MutatorCopies),
        ("tlab", 24, Counter::TlabTop),
        ("objects", 28, Counter::ObjectIds),
        ("maps", 29, Counter::MetadataMaps),
    ] {
        h.report(name, |h| {
            h.counter(counter);
            h.g.load_rbp_slot(R::V1, slot_offset(slot));
            h.g.sub_reg_reg(R::V0, R::V1);
        });
    }
    h.report("metadata", |h| {
        weak_growth_committed(h.g);
        h.g.load_rbp_slot(R::V1, slot_offset(25));
        h.g.sub_reg_reg(R::V0, R::V1);
    });
    h.report("value", |h| h.load(Src::Raw(4), 0));
    if marking != 0 {
        // Negative control: change only the Old color parity on the same
        // logical reference, forcing the real slow path under active marking.
        h.value(Src::Raw(3));
        h.g.load_ptr_disp32(R::V1, R::V0, 0);
        h.g.mov_imm64(R::V2, u64::from(crate::gc_abi::OLD_MARK_MASK));
        h.g.xor_reg_reg(R::V1, R::V2);
        h.g.store_ptr_disp32(R::V0, 0, R::V1);
        h.counter(Counter::LockTickets);
        h.keep(26);
        h.counter(Counter::Healed);
        h.keep(27);
        h.load_barriered(Src::Raw(3), 0);
        h.report("stale_tickets", |h| {
            h.counter(Counter::LockTickets);
            h.g.load_rbp_slot(R::V1, slot_offset(26));
            h.g.sub_reg_reg(R::V0, R::V1);
        });
        h.report("stale_healed", |h| {
            h.counter(Counter::Healed);
            h.g.load_rbp_slot(R::V1, slot_offset(27));
            h.g.sub_reg_reg(R::V0, R::V1);
        });
        h.end_marks();
    }
}

#[test]
fn production_good_field_loads_do_not_lock_copy_or_allocate() {
    for marking in [0, 1] {
        let o = run(exercise, marking, false);
        for name in [
            "tickets", "requests", "healed", "copies", "tlab", "metadata", "objects", "maps",
        ] {
            assert_eq!(o.get(name), 0, "marking={marking}: {}", o.text());
        }
        assert_eq!(o.get("value"), 84, "{}", o.text());
        if marking != 0 {
            assert!(o.get("stale_tickets") > 0, "{}", o.text());
            assert_eq!(o.get("stale_healed"), 1, "{}", o.text());
        }
    }
}
