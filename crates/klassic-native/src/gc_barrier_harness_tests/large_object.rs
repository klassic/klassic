//! Real multi-page Old allocations, interior fields, relocation and dead-span reuse.
use super::{H, Src, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_abi::{HEAP_PAGE_BYTES as P, ObjectHeader};
use crate::gc_codegen::production::{
    large_object as large,
    test_support::{Counter, request_old_cycle},
};
use crate::portable_asm::{PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};
fn configure(g: &mut NativeCodeGenerator) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
}
fn metric(h: &mut H<'_>, name: &str, offset: usize) {
    h.report(name, |h| {
        crate::gc_codegen::production::test_support::runtime_value(h.g, offset)
    });
}
fn raw(g: &mut NativeCodeGenerator, pretouch: u64) {
    configure(g);
    g.gc_pre_touch = pretouch != 0;
    let mut h = H::begin(g);
    h.alloc(1, 3 * P + 16, 0);
    h.report("initializing", |h| {
        h.value(Src::Raw(0));
        h.g.load_ptr_disp32(
            R::V0,
            R::V0,
            offset_of!(ObjectHeader, state) as i32 - size_of::<ObjectHeader>() as i32,
        );
    });
    for (i, index) in [0, 1, 2, 3, P / 8, 2 * P / 8, 3 * P / 8]
        .into_iter()
        .enumerate()
    {
        h.set_scalar(Src::Raw(0), index as u32, 80 + i as u64);
    }
    h.root(0);
    h.report("address", |h| h.value(Src::Root(0)));
    h.report("id", |h| h.object_id(Src::Root(0)));
    for cycle in 0..3 {
        request_old_cycle(h.g);
        h.collect();
        h.finish_collection();
        h.report(&format!("address{cycle}"), |h| h.value(Src::Root(0)));
        for (i, index) in [0, 1, 2, 3, P / 8, 2 * P / 8, 3 * P / 8]
            .into_iter()
            .enumerate()
        {
            h.report(&format!("v{cycle}_{i}"), |h| {
                h.load(Src::Root(0), index as u32)
            });
        }
        h.report(&format!("id{cycle}"), |h| h.object_id(Src::Root(0)));
    }
    metric(&mut h, "allocations", large::ALLOCATIONS);
    metric(&mut h, "bytes", large::BYTES);
    h.report_counter("full", Counter::FullCollections);
    h.report_counter("mutator_copies", Counter::MutatorCopies);
    h.report_counter("worker_copies", Counter::WorkerCopies);
}
#[test]
fn production_large_old_raw_span_stays_at_the_same_address_without_payload_copies() {
    for pretouch in [0, 1] {
        for worker in [false, true] {
            let r = run(raw, pretouch, worker);
            assert_eq!(r.get("initializing"), 1);
            assert_eq!(r.get("allocations"), 1);
            assert_eq!(r.get("bytes"), (3 * P + 48) as i64);
            assert_eq!(r.get("full"), 3);
            assert_eq!(r.get("mutator_copies"), 0);
            assert_eq!(r.get("worker_copies"), 0);
            for c in 0..3 {
                assert_eq!(r.get(&format!("address{c}")), r.get("address"));
                assert_eq!(r.get(&format!("id{c}")), r.get("id"));
                for i in 0..7 {
                    assert_eq!(r.get(&format!("v{c}_{i}")), 80 + i);
                }
            }
        }
    }
}
const FIELD: u32 = (3 * P / 8 + 1) as u32;
fn fields(g: &mut NativeCodeGenerator, _: u64) {
    configure(g);
    let mut h = H::begin(g);
    h.alloc(2, 4 * P, 0);
    h.root(0);
    h.report("null", |h| h.load(Src::Root(0), FIELD));
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), FIELD, Src::Raw(1), true);
    h.report_counter("remembered", Counter::RemsetLen);
    for (name, off) in [
        ("state", offset_of!(ObjectHeader, state)),
        ("desc", offset_of!(ObjectHeader, descriptor_id)),
        ("size", offset_of!(ObjectHeader, payload_bytes)),
    ] {
        h.report(name, |h| {
            h.value(Src::Root(0));
            h.g.load_ptr_disp32(R::V0, R::V0, off as i32 - size_of::<ObjectHeader>() as i32);
        });
    }
    for displacement in [0, 1024, 2 * P as i32, FIELD as i32 * 8] {
        h.report(&format!("owner{displacement}"), |h| {
            h.value(Src::Root(0));
            h.g.add_reg_imm32(R::V0, displacement);
            h.g.mov_reg_reg(R::V7, R::V0);
            h.g.call_label(h.l.slot_owner);
        });
    }

    h.report("before", |h| {
        h.load_barriered(Src::Root(0), FIELD);
        h.g.load_ptr_disp32(R::V0, R::V0, 0);
    });
    for cycle in 0..4 {
        h.collect();
        h.finish_collection();
        h.report(&format!("child{cycle}"), |h| {
            h.load_barriered(Src::Root(0), FIELD);
            h.g.load_ptr_disp32(R::V0, R::V0, 0);
        });
        // Replace the only retained edge at a deep interior slot each cycle.
        h.alloc(1, 16, 1);
        h.set_scalar(Src::Raw(1), 0, 85 + cycle);
        h.store(Src::Root(0), FIELD, Src::Raw(1), true);
    }
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report("final", |h| {
        h.load_barriered(Src::Root(0), FIELD);
        h.g.load_ptr_disp32(R::V0, R::V0, 0);
    });
    h.report_counter("unmarked", Counter::YoungUnmarked);
}
#[test]
fn production_large_old_interior_pointer_slots_retain_young_children_and_survive_full_relocation() {
    for worker in [false, true] {
        let r = run(fields, 0, worker);
        assert_eq!(r.get("null"), 0);
        assert_eq!(r.get("before"), 84);
        assert_eq!(r.get("state"), 2);
        assert_eq!(r.get("desc"), 2);
        assert_eq!(r.get("size"), 4 * P as i64);
        assert_eq!(r.get("remembered"), 1);
        for displacement in [0, 1024, 2 * P as i32, FIELD as i32 * 8] {
            assert!(r.get(&format!("owner{displacement}")) > 0, "{}", r.text());
        }
        for c in 0..4 {
            assert_eq!(r.get(&format!("child{c}")), 84 + c);
        }
        assert_eq!(r.get("final"), 88);
        assert_eq!(r.get("unmarked"), 0);
    }
}
fn reuse(g: &mut NativeCodeGenerator, _: u64) {
    configure(g);
    // Retain actual Old tracing until capacity retry has really happened.
    // Otherwise a fast worker can enable planned collection before exhaustion,
    // and this test never exercises the fallback it is meant to validate.
    let gate = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.phase_mark_gate = Some(gate);
    g.jmp_label(body);
    g.bind_text_label(gate);
    crate::gc_codegen::production::test_support::runtime_value(g, large::RETRIES);
    g.ret();
    g.bind_text_label(body);
    let mut h = H::begin(g);
    h.repeat(20, |h| {
        h.alloc(1, 1 << 20, 0);
        h.set_scalar(Src::Raw(0), 65536, 99);
    });
    h.alloc(1, 1 << 20, 0);
    h.set_scalar(Src::Raw(0), 65536, 84);
    h.root(0);
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 65536));
    metric(&mut h, "allocations", large::ALLOCATIONS);
    metric(&mut h, "retries", large::RETRIES);
    h.report_counter("full", Counter::FullCollections);
}
#[test]
fn production_large_old_exhaustion_collects_dead_spans_and_retries_without_losing_live_values() {
    for worker in [false, true] {
        let r = run(reuse, 0, worker);
        assert_eq!(r.get("value"), 84);
        assert_eq!(r.get("allocations"), 21);
        assert!(r.get("retries") > 0);
        assert!(r.get("full") > 1);
    }
}
fn too_large(g: &mut NativeCodeGenerator, case: u64) {
    configure(g);
    let mut h = H::begin(g);
    h.report("before", |h| h.value(Src::Imm(1)));
    let size = if case == 0 {
        large::max_payload(h.g) + 1
    } else {
        u64::MAX
    };
    h.alloc(1, size, 0);
    h.report("after", |h| h.value(Src::Imm(1)));
}
#[test]
fn production_large_old_unrepresentable_size_fails_before_returning_an_object() {
    for case in 0..2 {
        let r = run_failure(too_large, case);
        assert_eq!(r.status.code(), Some(1), "{r:?}");
        assert_eq!(r.stdout, b"before=1\n");
        assert!(String::from_utf8_lossy(&r.stderr).contains("Old span capacity"));
    }
}

fn live_full(g: &mut NativeCodeGenerator, _: u64) {
    configure(g);
    let mut h = H::begin(g);
    // Both spans fit the size bound independently, but the live first span
    // leaves too little room for the second, even after a full collection.
    h.alloc(1, 3 << 20, 0);
    h.set_scalar(Src::Raw(0), 65536, 84);
    h.root(0);
    h.report("before", |h| h.load(Src::Root(0), 65536));
    h.alloc(1, 3 << 20, 1);
    h.report("after", |h| h.value(Src::Imm(1)));
}
#[test]
fn production_large_old_live_capacity_exhaustion_reports_oom_after_collection() {
    let r = run_failure(live_full, 0);
    assert_eq!(r.status.code(), Some(1), "{r:?}");
    assert_eq!(r.stdout, b"before=84\n");
    assert!(
        String::from_utf8_lossy(&r.stderr).contains("Old run capacity exhausted"),
        "{r:?}"
    );
}

fn metadata_full(g: &mut NativeCodeGenerator, _: u64) {
    configure(g);
    let mut h = H::begin(g);
    let [limit, initial, ..] = crate::gc_codegen::production::metadata::live_offsets();
    h.g.load_ptr_disp32(R::V0, R::V3, initial);
    h.g.store_ptr_disp32(R::V3, limit, R::V0);
    h.report("before", |h| h.value(Src::Imm(1)));
    h.alloc(1, 3 * P, 0);
    h.report("after", |h| h.value(Src::Imm(1)));
}
#[test]
fn production_large_old_metadata_admission_refuses_unfunded_identity_before_return() {
    let r = run_failure(metadata_full, 0);
    assert_eq!(r.status.code(), Some(1), "{r:?}");
    assert_eq!(r.stdout, b"before=1\n");
    assert!(String::from_utf8_lossy(&r.stderr).contains("metadata budget exhausted"));
}

fn active_mark(g: &mut NativeCodeGenerator, _: u64) {
    configure(g);
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.mark_start();
    h.alloc(4, 4 * P + 8, 1);
    h.set_scalar(Src::Raw(1), 0, 4 * P / 8);
    h.store(Src::Raw(1), FIELD, Src::Root(0), true);
    h.root(1);
    h.value(Src::Null);
    h.set_root(0);
    h.drain_marks();
    h.end_marks();
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report("length", |h| h.load(Src::Root(1), 0));
    h.report("child", |h| {
        h.load_barriered(Src::Root(1), FIELD);
        h.g.load_ptr_disp32(R::V0, R::V0, 0);
    });
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_large_old_pointer_list_allocated_during_old_mark_protects_new_fields_and_keeps_scalar_length()
 {
    for worker in [false, true] {
        let r = run(active_mark, 0, worker);
        assert_eq!(r.get("length"), (4 * P / 8) as i64);
        assert_eq!(r.get("child"), 84);
        assert_eq!(r.get("unmarked"), 0);
    }
}

fn cyclic(g: &mut NativeCodeGenerator, _: u64) {
    configure(g);
    let mut h = H::begin(g);
    h.alloc(2, 3 * P, 0);
    h.root(0);
    h.alloc(2, 3 * P, 1);
    h.root(1);
    h.store(Src::Root(0), FIELD - 2, Src::Root(1), true);
    h.store(Src::Root(1), FIELD - 2, Src::Root(0), true);
    h.report("a", |h| h.value(Src::Root(0)));
    h.report("b", |h| h.value(Src::Root(1)));
    h.value(Src::Null);
    // Root the higher-address object: its lower-address child is discovered
    // after the descriptor pass has already skipped that page. A rescan is
    // necessary to traverse the complete cycle.
    h.set_root(0);
    for c in 0..3 {
        request_old_cycle(h.g);
        h.collect();
        h.finish_collection();
        h.report(&format!("a{c}"), |h| {
            h.load_barriered(Src::Root(1), FIELD - 2)
        });
        h.report(&format!("b{c}"), |h| h.value(Src::Root(1)));
    }
    metric(&mut h, "scanned", large::report_offsets()[5] as usize);
    metric(&mut h, "live", large::report_offsets()[3] as usize);
    h.value(Src::Null);
    h.set_root(1); // A cycle with no external root must be reclaimed.
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    metric(&mut h, "dead_live", large::report_offsets()[3] as usize);
    metric(&mut h, "reclaimed", large::report_offsets()[4] as usize);
    h.alloc(2, 3 * P, 2);
    h.root(2);
    h.report("reused_null", |h| h.load(Src::Root(2), FIELD - 2));
    metric(&mut h, "reused_live", large::report_offsets()[3] as usize);
}
#[test]
fn production_large_old_transitive_cycles_stay_live_then_reclaim_and_clear_all_pages() {
    for worker in [false, true] {
        let r = run(cyclic, 0, worker);
        for c in 0..3 {
            assert_eq!(r.get(&format!("a{c}")), r.get("a"));
            assert_eq!(r.get(&format!("b{c}")), r.get("b"));
        }
        assert_eq!(r.get("scanned"), 3 * 2 * (3 * P / 8) as i64);
        assert_eq!(r.get("live"), 8);
        assert_eq!(r.get("dead_live"), 0);
        assert_eq!(r.get("reclaimed"), 8);
        assert_eq!(r.get("reused_null"), 0);
        assert_eq!(r.get("reused_live"), 4);
    }
}

fn budgeted_scan(g: &mut NativeCodeGenerator, _: u64) {
    configure(g);
    let mut h = H::begin(g);
    h.alloc(2, 3 * P, 0);
    h.root(0);
    request_old_cycle(h.g);
    h.collect();
    metric(&mut h, "before", large::report_offsets()[5] as usize);
    h.relocate_quantum(0, false);
    metric(&mut h, "zero", large::report_offsets()[5] as usize);
    for i in 0..100 {
        h.relocate_quantum(8, false);
        metric(
            &mut h,
            &format!("scan{i}"),
            large::report_offsets()[5] as usize,
        );
    }
    h.finish_collection();
    metric(&mut h, "complete", large::report_offsets()[5] as usize);
}
#[test]
fn production_large_old_reference_scan_yields_at_the_requested_field_budget() {
    let r = run(budgeted_scan, 0, false);
    let mut previous = r.get("before");
    assert_eq!(r.get("zero"), previous);
    for i in 0..100 {
        let current = r.get(&format!("scan{i}"));
        assert!((previous..=previous + 8).contains(&current));
        previous = current;
    }
    assert!(previous > r.get("before"));
    assert_eq!(r.get("complete"), (3 * P / 8) as i64);
}

fn release_failure(g: &mut NativeCodeGenerator, _: u64) {
    configure(g);
    let mut h = H::begin(g);
    h.alloc(1, 3 * P, 0);
    h.alloc(1, 16, 1); // Publish the large object without retaining it.
    crate::gc_codegen::production::metadata::fail_release_next(h.g);
    h.report("before", |h| h.value(Src::Imm(1)));
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report("after", |h| h.value(Src::Imm(1)));
}
#[test]
fn production_large_old_sweep_reports_failed_identity_backing_release() {
    let r = run_failure(release_failure, 0);
    assert_eq!(r.status.code(), Some(1));
    assert_eq!(r.stdout, b"before=1\n");
    assert!(String::from_utf8_lossy(&r.stderr).contains("native metadata release failed"));
}
