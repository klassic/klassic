//! Real OS mutators seal independently captured previous values at boundaries.
use super::satb_fixture::run;
use super::{H, Src, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};

fn deleted(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let owners = mode >> 2;
    let seal = mode & 3;
    let mut h = H::begin(g);
    h.alloc(2, owners * 8, 0);
    h.root(0);
    let children = h.g.asm.data_label_with_i64s(&vec![0; owners as usize]);
    for i in 0..owners {
        h.alloc(1, 16, i as u32 + 1);
        h.set_scalar(Src::Raw(i as u32 + 1), 0, 9600 + i);
        h.store(Src::Root(0), i as u32, Src::Raw(i as u32 + 1), true);
        h.value(Src::Raw(i as u32 + 1));
        h.g.mov_data_addr(R::V1, children);
        h.g.store_ptr_disp32(R::V1, (i * 8) as i32, R::V0);
    }
    // The allocator scratch root must not keep the last previous child alive.
    h.alloc(1, 16, 20);
    publish_primary(h.g);
    let actors: Vec<_> = (0..owners).map(|_| create_actor(h.g, 16, 8)).collect();
    let state = run_parallel_satb_deletions(h.g, &actors, slot_offset(0), children, seal);
    for metric in 0..11 {
        h.report(&format!("global{metric}"), |h| {
            satb_parallel_metric(h.g, state, metric)
        });
    }
    for i in 0..owners as usize {
        for metric in 0..6 {
            h.report(&format!("owner{i}_{metric}"), |h| {
                satb_parallel_owner_metric(h.g, state, i, metric)
            });
        }
        h.report(&format!("old{i}"), |h| h.marked(Src::Raw(i as u32 + 1)));
        h.report(&format!("young{i}"), |h| {
            h.value(Src::Raw(i as u32 + 1));
            emit_young_marked(h.g);
        });
        h.report(&format!("value{i}"), |h| h.load(Src::Raw(i as u32 + 1), 0));
    }
    h.report("admitted", |h| satb_metric(h.g, 3));
    h.report("flushed", |h| satb_metric(h.g, 4));
    h.report("members", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
}

#[test]
fn production_satb_real_one_two_eight_owners_seal_previous_values_on_ack_native_and_detach() {
    for owners in [1, 2, 8] {
        for seal in 0..3 {
            let (o, _image) = run(deleted, owners << 2 | seal, true);
            assert_eq!(o.get("global0"), owners as i64, "{}", o.text());
            assert_eq!(o.get("global1"), owners as i64);
            assert_eq!(o.get("global2"), o.get("global9") + 4 * owners as i64);
            for metric in [3, 4] {
                assert_eq!(o.get(&format!("global{metric}")), 0);
            }
            assert_eq!(o.get("global5"), o.get("global9"));
            assert_eq!(o.get("global6"), 1);
            assert_eq!(o.get("global7"), 1);
            if seal == 0 {
                assert!(o.get("global8") > o.get("global10"), "{}", o.text());
            }
            for i in 0..owners {
                assert_eq!(o.get(&format!("owner{i}_0")), 2);
                assert_eq!(o.get(&format!("owner{i}_1")), 0);
                assert_eq!(o.get(&format!("owner{i}_2")), 0);
                assert_eq!(o.get(&format!("owner{i}_3")), 0);
                if seal == 0 && i != 0 {
                    assert!(o.get(&format!("owner{i}_4")) > 0, "{}", o.text());
                }
                assert_eq!(o.get(&format!("owner{i}_5")), if seal == 2 { 3 } else { 2 });
                assert_eq!(o.get(&format!("old{i}")), 1);
                assert_eq!(o.get(&format!("young{i}")), 1);
                assert_eq!(o.get(&format!("value{i}")), 9600 + i as i64);
            }
            assert_eq!(o.get("admitted"), 2 * owners as i64);
            assert_eq!(o.get("flushed"), o.get("admitted"));
            assert_eq!(
                o.get("members"),
                if seal == 2 { 1 } else { owners as i64 + 1 }
            );
        }
    }
}
