//! SATB owners include Young holders, including real worker-copy destinations.
use super::satb_fixture::run;
use super::{H, Src};
use crate::gc_codegen::production::{MAX_PAYLOAD, test_support::*};
use crate::portable_asm::{PortableAsm, Reg as R};

fn query(h: &mut H<'_>, displacement: i32, satb: bool) {
    h.value(Src::Root(0));
    h.g.add_reg_imm32(R::V0, displacement);
    h.g.mov_reg_reg(R::V7, R::V0);
    if satb {
        satb_slot_owner_query(h.g);
    } else {
        h.g.call_label(h.l.slot_owner);
    }
}

fn owner(h: &mut H<'_>) {
    h.value(Src::Root(0));
    satb_slot_owner_offset(h.g);
}

fn published_young(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 8192, 0);
    h.value(Src::Raw(0));
    h.keep(2);
    h.root(0);
    publish_primary(h.g);
    h.report("source", owner);
    if mode == 1 {
        h.collect();
        h.worker_copy(Src::Raw(2));
        h.finish_collection();
        h.report_counter("worker_young", Counter::AgeWorkerYoung);
    }
    h.report("owner", owner);
    h.report("is_old", |h| h.is_old(Src::Root(0)));
    for displacement in [0, 1024, 2048, 8184] {
        h.report(&format!("satb_{displacement}"), |h| {
            query(h, displacement, true)
        });
        h.report(&format!("old_{displacement}"), |h| {
            query(h, displacement, false)
        });
    }
    if mode == 2 {
        // A whole-page allocation forces the following real reference holder
        // onto a different Young page, with its own exact start membership.
        h.alloc(1, MAX_PAYLOAD, 3);
        h.root(3);
        h.alloc(2, 8192, 1);
        h.root(1);
        publish_primary(h.g);
        h.report("other", |h| {
            h.value(Src::Root(1));
            satb_slot_owner_offset(h.g);
        });
        h.value(Src::Root(1));
        h.g.mov_reg_reg(R::V6, R::V0);
        h.value(Src::Root(0));
        h.g.add_reg_imm32(R::V0, 2048);
        h.g.mov_reg_reg(R::V7, R::V0);
        satb_slot_cross_page_hint(h.g);
        h.report("cross_page", |h| query(h, 2048, true));
    } else if mode == 3 {
        h.value(Src::Root(0));
        clear_owner_start(h.g);
        h.report("stale_hint", |h| query(h, 2048, true));
    }
}

#[test]
fn production_satb_slot_owner_finds_fresh_and_worker_copied_young_interior_fields() {
    for mode in [0, 1] {
        let (o, _image) = run(published_young, mode, false);
        assert_eq!(o.get("is_old"), 0, "{}", o.text());
        for displacement in [0, 1024, 2048, 8184] {
            assert_eq!(
                o.get(&format!("satb_{displacement}")),
                o.get("owner"),
                "{}",
                o.text()
            );
            assert_eq!(o.get(&format!("old_{displacement}")), 0);
        }
        if mode == 1 {
            assert_ne!(o.get("source"), o.get("owner"));
            assert!(o.get("worker_young") >= 8192 + 32, "{}", o.text());
        }
    }
}

#[test]
fn production_satb_slot_owner_rejects_cross_page_and_stale_young_card_hints() {
    let (o, _image) = run(published_young, 2, false);
    assert_ne!(o.get("owner") >> 17, o.get("other") >> 17, "{}", o.text());
    assert_eq!(o.get("satb_2048"), o.get("owner"), "{}", o.text());
    assert_eq!(o.get("cross_page"), 0, "{}", o.text());
    let (o, _image) = run(published_young, 3, false);
    assert_eq!(o.get("satb_2048"), o.get("owner"), "{}", o.text());
    assert_eq!(o.get("stale_hint"), 0, "{}", o.text());
}
