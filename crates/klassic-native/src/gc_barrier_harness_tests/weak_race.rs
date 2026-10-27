//! Deterministic weak-load / actual strong-termination overlap on OS threads.
use super::{H, Src, run, slot_offset};
use crate::gc_abi::{MarkRecord, ObjectHeader};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};
fn identity(h: &mut H<'_>, root: u32) {
    h.value(Src::Root(root));
    h.g.load_ptr_disp32(
        R::V0,
        R::V0,
        (offset_of!(ObjectHeader, mark_record) as i32) - size_of::<ObjectHeader>() as i32,
    );
    h.g.load_ptr_disp32(R::V0, R::V0, offset_of!(MarkRecord, object_id) as i32);
}
fn race(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let n = arg & 255;
    let young = arg & 256 != 0;
    let gate_first = arg & 512 != 0;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    h.alloc(2, 16, 1);
    h.root(1);
    h.store(Src::Root(1), 0, Src::Root(0), true);
    if !young {
        h.collect();
        h.finish_collection();
        h.collect();
        h.finish_collection();
    }
    h.report("parent_id", |h| identity(h, 1));
    h.report("leaf_id", |h| identity(h, 0));
    let handle = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(1));
    weak_call(h.g, handle, 0);
    lease_require_ok(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(1));
    let state = prepare_weak_race(h.g, handle, gate_first);
    let mut actors = Vec::new();
    let mut outputs = Vec::new();
    for _ in 0..n {
        let actor = create_actor(h.g, 16, 8);
        h.g.mov_imm64(R::V0, 0);
        let output = adopt_actor_value(h.g, actor);
        copy_actor_native_enter(h.g, actor);
        actors.push(actor);
        outputs.push(output);
    }
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(1);
    let p = run_parallel_weak_race(h.g, &actors, &outputs, state, young, gate_first);
    for (index, name) in [
        "published",
        "end_entered",
        "attempted",
        "finished",
        "closed_gate",
        "debt_at_end",
        "deferred",
        "white",
    ]
    .iter()
    .enumerate()
    {
        h.report(name, |h| weak_race_metric(h.g, state, index));
    }
    h.report("leases", |h| lease_metric(h.g, 0));
    h.report("gate", |h| weak_metric(h.g, 1));
    h.report_counter("retire", Counter::RetireStage);
    for i in 0..n as usize {
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        h.report(&format!("first{i}"), |h| {
            native_completed_metric(h.g, &p, i, 64)
        });
        h.report(&format!("final{i}"), |h| {
            native_completed_metric(h.g, &p, i, 72)
        });
        native_completed_resume(h.g, actors[i]);
        if !gate_first {
            h.report(&format!("parent{i}"), |h| {
                native_completed_root(h.g, actors[i], outputs[i], true)
            });
            h.report(&format!("leaf{i}"), |h| {
                native_completed_leaf_id(h.g, actors[i], outputs[i])
            });
            h.report(&format!("value{i}"), |h| {
                native_completed_root(h.g, actors[i], outputs[i], false);
                h.g.mov_reg_reg(R::V10, R::V0);
                h.g.load_ptr_disp32(R::V0, R::V10, 0);
                h.g.call_label(h.l.field_load);
                h.g.load_ptr_disp32(R::V0, R::V0, 0);
            });
        } else {
            h.report(&format!("empty_out{i}"), |h| {
                native_completed_root(h.g, actors[i], outputs[i], false)
            });
        }
    }
    weak_call(h.g, handle, 2);
    lease_require_ok(h.g);
    h.report("empty", |h| lease_metric(h.g, 0));
}
#[test]
fn production_weak_load_publication_and_strong_termination_have_both_real_thread_orders() {
    for young in [false, true] {
        for gate_first in [false, true] {
            for n in [1, 2, 8] {
                let o = run(
                    race,
                    n | if young { 256 } else { 0 } | if gate_first { 512 } else { 0 },
                    true,
                );
                assert_eq!(o.get("white"), 1, "{}", o.text());
                assert_eq!(o.get("attempted"), n as i64);
                assert_eq!(o.get("finished"), n as i64);
                assert_eq!(o.get("leases"), i64::from(!gate_first));
                assert_eq!(o.get("gate"), 0);
                assert_eq!(o.get("retire"), 0);
                assert_eq!(o.get("empty"), 0);
                if gate_first {
                    assert_eq!(o.get("published"), 0);
                    assert_eq!(o.get("closed_gate"), 1);
                    assert!(o.get("deferred") > 0);
                } else {
                    assert_eq!(o.get("published"), n as i64);
                    assert_eq!(o.get("end_entered"), 1);
                    assert!(o.get("debt_at_end") > 0);
                }
                let mut tids = std::collections::HashSet::new();
                let mut successes = 0;
                for i in 0..n {
                    assert!(tids.insert(o.get(&format!("tid{i}"))));
                    let first = o.get(&format!("first{i}"));
                    assert!(first == 0 || first == 2);
                    successes += usize::from(first == 0);
                    assert_eq!(o.get(&format!("final{i}")), i64::from(gate_first));
                    if gate_first {
                        assert_eq!(first, 2);
                        assert_eq!(o.get(&format!("empty_out{i}")), 0);
                    } else {
                        assert_eq!(o.get(&format!("parent{i}")), o.get("parent_id"));
                        assert_eq!(o.get(&format!("leaf{i}")), o.get("leaf_id"));
                        assert_eq!(o.get(&format!("value{i}")), 77);
                    }
                }
                if !gate_first {
                    assert!(successes > 0);
                }
            }
        }
    }
}
