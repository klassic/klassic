//! Shared production pool: real role handoff, source quarantine and reuse.
use super::{H, Src, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::{
    MAX_PAYLOAD,
    test_support::{
        Counter, GENERATION_OLD as O, GENERATION_YOUNG as Y, PoolMetric as M, emit_budget,
        pool_claim, pool_metric, pool_page_state, pool_role, request_old_cycle,
        request_young_pages,
    },
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

fn handoff(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, MAX_PAYLOAD, 0);
    h.root(0);
    request_young_pages(h.g, 64);
    for _ in 0..2 {
        crate::gc_codegen::production::test_support::young_trace_complete(h.g, &h.l);
        h.collect();
        h.finish_collection();
    }
    for pass in 0..3 {
        request_old_cycle(h.g);
        h.collect();
        h.finish_collection();
        if pass == 0 {
            // Record a populated destination under the actual demand budget.
            pool_metric(h.g, M::OldBase);
            h.keep(20);
        }
        h.report(&format!("old{pass}"), |h| pool_metric(h.g, M::OldBase));
        h.report(&format!("young{pass}"), |h| pool_metric(h.g, M::YoungBase));
    }
    request_young_pages(h.g, 129);
    // Rotate an empty Young window first. The next worker upper stream can
    // then reuse the populated page recorded above after its Full retirement.
    crate::gc_codegen::production::test_support::young_trace_complete(h.g, &h.l);
    h.collect();
    h.finish_collection();
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.value(Src::Raw(1));
    h.keep(2);
    h.root(1);
    h.store(
        Src::Root(0),
        (MAX_PAYLOAD / 8 - 1) as u32,
        Src::Root(1),
        true,
    );
    crate::gc_codegen::production::test_support::young_trace_complete(h.g, &h.l);
    h.collect();
    h.g.call_label(h.l.lock);
    h.worker_copy(Src::Raw(2));
    h.g.call_label(h.l.unlock);
    h.keep(3);
    h.finish_collection();
    h.report("reused_old_page", |h| {
        h.value(Src::Root(1));
        crate::gc_codegen::production::test_support::pool_payload_page(h.g);
        h.g.load_rbp_slot(R::V1, super::slot_offset(20));
        let bad = h.g.create_text_label();
        let done = h.g.create_text_label();
        h.g.cmp_reg_reg(R::V0, R::V1);
        h.g.jcc_label(C::NotEqual, bad);
        h.g.mov_imm64(R::V0, 1);
        h.g.jmp_label(done);
        h.g.bind_text_label(bad);
        h.g.mov_imm64(R::V0, 0);
        h.g.bind_text_label(done);
    });
    h.report("fresh_is_young", |h| h.is_old(Src::Root(1)));
    h.pop_root();
    h.collect();
    h.finish_collection();
    h.report("child", |h| {
        h.deref_field(Src::Root(0), (MAX_PAYLOAD / 8 - 1) as u32, 0)
    });
    h.report("id", |h| h.object_id(Src::Root(0)));
    h.report("transfers", |h| pool_metric(h.g, M::Transfers));
    h.report("claims", |h| pool_metric(h.g, M::Claims));
    h.report("released", |h| pool_metric(h.g, M::Released));
}
#[test]
fn production_shared_pool_reuses_a_previously_populated_old_page_as_young_and_preserves_edges() {
    let o = run(handoff, 0, false);
    assert_eq!(o.get("reused_old_page"), 1, "{}", o.text());
    assert_eq!(o.get("fresh_is_young"), 0);
    assert_eq!(o.get("child"), 84);
    assert_eq!(o.get("id"), 1);
    assert!(o.get("transfers") > 0);
    assert!(o.get("claims") > 2);
    assert!(o.get("released") > 0);
}
fn first_fit(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.report("young", |h| pool_metric(h.g, M::YoungBase));
    h.report("young_end", |h| pool_metric(h.g, M::YoungEnd));
    h.report("old", |h| pool_metric(h.g, M::OldBase));
    h.report("guard", |h| pool_role(h.g, 0));
    h.report("last_old", |h| pool_role(h.g, 767));
    h.report("first_free", |h| pool_role(h.g, 768));
    pool_claim(h.g, 2, Y);
    h.keep(1);
    pool_claim(h.g, 3, O);
    h.keep(2);
    h.report("young_run", |h| h.value(Src::Raw(1)));
    h.report("old_run", |h| h.value(Src::Raw(2)));
    h.report("new_young", |h| pool_role(h.g, 769));
    h.report("new_old", |h| pool_role(h.g, 772));
    h.report("next_free", |h| pool_role(h.g, 773));
}
#[test]
fn production_shared_pool_claims_disjoint_runs_from_one_free_directory() {
    let o = run(first_fit, 0, false);
    for (name, value) in [
        ("young", 0),
        ("young_end", 256),
        ("old", 256),
        ("guard", Y),
        ("last_old", O),
        ("first_free", 0),
        ("young_run", 768),
        ("old_run", 770),
        ("new_young", Y),
        ("new_old", O),
        ("next_free", 0),
    ] {
        assert_eq!(o.get(name), value as i64, "{name}: {}", o.text());
    }
}
fn invalid(g: &mut NativeCodeGenerator, fault: u64) {
    let mut h = H::begin(g);
    match fault {
        0 => pool_claim(h.g, 0, Y),
        1 => pool_claim(h.g, 1, 99),
        2 => pool_claim(h.g, 1537, Y),
        3 => pool_claim(h.g, 769, O), // Only 768 free pages remain.
        4 => {
            pool_page_state(h.g, 768, 0);
            pool_claim(h.g, 1, Y);
        }
        5 => {
            pool_claim(h.g, 768, Y);
            crate::gc_codegen::production::test_support::pool_largest_old(h.g);
        }
        6 | 7 => {
            crate::gc_codegen::production::test_support::pool_dirty_owner_or_forward(
                h.g,
                768,
                fault == 7,
            );
            pool_claim(h.g, 1, Y);
        }
        8 => {
            h.alloc(1, 16, 0);
            h.root(0);
            for _ in 0..2 {
                h.collect();
                h.finish_collection();
            }
            h.value(Src::Root(0));
            crate::gc_codegen::production::test_support::pool_exhaust_incarnation(h.g);
            request_old_cycle(h.g);
            h.collect();
            h.finish_collection();
        }
        _ => unreachable!(),
    }
}
#[test]
fn production_shared_pool_rejects_invalid_requests_dirty_free_pages_and_insufficient_contiguous_space()
 {
    for fault in 0..9 {
        let o = run_failure(invalid, fault);
        assert_eq!(o.status.code(), Some(1), "fault {fault}");
        assert!(!o.stderr.is_empty());
    }
}

fn source_role(h: &mut H<'_>, next: bool) {
    h.value(Src::Raw(10));
    if next {
        h.g.add_reg_imm32(R::V0, crate::gc_abi::HEAP_PAGE_BYTES as i32);
    }
    crate::gc_codegen::production::test_support::pool_address_role(h.g);
}
fn quantum(h: &mut H<'_>, units: u64) {
    h.g.call_label(h.l.lock);
    emit_budget(h.g, units);
    h.g.call_label(h.l.retire_step);
    h.g.call_label(h.l.unlock);
}
fn quarantine(g: &mut NativeCodeGenerator, old: u64) {
    let mut h = H::begin(g);
    h.alloc(1, MAX_PAYLOAD, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    if old != 0 {
        h.alloc(1, 16, 1);
        h.root(1);
        for _ in 0..2 {
            h.collect();
            h.finish_collection();
        }
        request_old_cycle(h.g);
    }
    h.value(Src::Root(0));
    h.keep(10);
    h.collect();
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.close_relocation);
    h.g.call_label(h.l.unlock);
    h.report("before", |h| source_role(h, false));
    let found = h.g.create_text_label();
    h.repeat(400_000, |h| {
        quantum(h, 1);
        h.counter(Counter::RetireStage);
        h.g.cmp_reg_imm8(R::V0, if old == 0 { 15 } else { 16 });
        h.g.jcc_label(C::Equal, found);
    });
    h.g.bind_text_label(found);
    h.report_counter("stage", Counter::RetireStage);
    h.report("after_metadata", |h| source_role(h, false));
    h.report("inc_before", |h| {
        h.value(Src::Raw(10));
        crate::gc_codegen::production::test_support::pool_page_incarnation(h.g);
    });
    h.report_counter("units_before", Counter::RetireUnits);
    h.report_counter("cursor_before", Counter::RetireCursor);
    quantum(&mut h, 0);
    h.report_counter("units_zero", Counter::RetireUnits);
    h.report("zero_budget", |h| source_role(h, false));
    quantum(&mut h, 1);
    h.report_counter("units_attempt", Counter::RetireUnits);
    h.report("role_after_attempt", |h| source_role(h, false));
    if old != 0 {
        // This Old page owns copy metadata. One unit queues its OS release;
        // the role remains quarantined until the next unit acknowledges it.
        quantum(&mut h, 1);
    }
    h.report("inc_after", |h| {
        h.value(Src::Raw(10));
        crate::gc_codegen::production::test_support::pool_page_incarnation(h.g);
    });
    h.report_counter("units_one", Counter::RetireUnits);
    h.report_counter("cursor_one", Counter::RetireCursor);
    h.report("first_free", |h| source_role(h, false));
    h.report("next_held", |h| source_role(h, true));
    h.finish_collection();
    h.report("after", |h| source_role(h, true));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_shared_pool_quarantines_both_source_roles_until_budgeted_metadata_cleanup() {
    for old in [0, 1] {
        let o = run(quarantine, old, false);
        let role = if old == 0 { Y } else { O } as i64;
        for name in ["before", "after_metadata", "zero_budget", "next_held"] {
            assert_eq!(o.get(name), role, "{name}, old {old}: {}", o.text());
        }
        assert_eq!(o.get("stage"), if old == 0 { 15 } else { 16 });
        assert_eq!(o.get("units_zero"), o.get("units_before"));
        assert_eq!(o.get("units_attempt") - o.get("units_before"), 1);
        assert_eq!(o.get("role_after_attempt"), if old == 0 { 0 } else { role });
        assert_eq!(o.get("units_one") - o.get("units_before"), 1 + old as i64);
        assert_eq!(o.get("cursor_one") - o.get("cursor_before"), 8);
        if old != 0 {
            assert_eq!(
                o.get("inc_after") - o.get("inc_before"),
                crate::gc_codegen::production::test_support::pool_pages() as i64
            );
        }
        assert_eq!(o.get("first_free"), 0);
        assert_eq!(o.get("after"), 0);
        assert_eq!(o.get("value"), 84);
    }
}

fn largest(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    pool_claim(h.g, 300, Y);
    crate::gc_codegen::production::test_support::pool_largest_old(h.g);
    h.keep(1);
    h.g.mov_reg_reg(R::V0, R::V7);
    h.keep(2);
    h.report("base", |h| h.value(Src::Raw(1)));
    h.report("count", |h| h.value(Src::Raw(2)));
    h.report("reduced", |h| pool_metric(h.g, M::ReducedOldRuns));
    h.report("last", |h| pool_role(h.g, 1535));
    h.report("capacity", |h| pool_metric(h.g, M::OldCapacity));
}
#[test]
fn production_shared_pool_takes_an_available_old_run_smaller_than_the_cap() {
    let o = run(largest, 0, false);
    assert_eq!(o.get("base"), 1068);
    assert_eq!(o.get("count"), 468);
    assert_eq!(o.get("reduced"), 1);
    assert_eq!(o.get("last"), O as i64);
    assert_eq!(o.get("capacity"), 512); // Probe leaves the active allocator intact.
}

#[test]
fn production_shared_pool_real_worker_preserves_edges_across_old_to_young_page_reuse() {
    let o = run(handoff, 0, true);
    assert_eq!(o.get("reused_old_page"), 1, "{}", o.text());
    assert_eq!(o.get("child"), 84);
    assert_eq!(o.get("id"), 1);
    assert!(o.get("transfers") > 0);
}
fn copy_limit(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    for slot in 0..2 {
        h.alloc(1, MAX_PAYLOAD, slot);
        h.root(slot);
    }
    h.collect();
    h.finish_collection();
    h.collect();
    crate::gc_codegen::production::test_support::pool_old_copy_limit(h.g, 1);
    h.finish_collection();
}
#[test]
fn production_shared_pool_old_copy_respects_the_actual_run_capacity() {
    let o = run_failure(copy_limit, 0);
    assert_eq!(o.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&o.stderr).contains("Old run capacity exhausted"));
}

fn preflight(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    const MIB: u64 = 1 << 20;
    for (i, (pages, used, protected)) in [
        (512, 32 * MIB, None),
        (256, 31 * MIB, Some(0)),
        (256, 31 * MIB + 5 * 131072, Some(0)),
        (256, 0, None),
        (512, 31 * MIB, None),
        (256, 8 * MIB, Some(4 * MIB)),
        (256, 16 * MIB, Some(4 * MIB)),
        (256, 0, Some(16 * MIB)),
    ]
    .into_iter()
    .enumerate()
    {
        h.report(&format!("p{i}"), |h| {
            crate::gc_codegen::production::test_support::pool_preflight(h.g, pages, used, protected)
        });
    }
}
#[test]
fn production_shared_pool_full_preflight_uses_actual_old_capacity_and_young_protection() {
    let o = run(preflight, 0, false);
    for (i, v) in [0, 0, 1, 0, 0, 0, 1, 1].into_iter().enumerate() {
        assert_eq!(o.get(&format!("p{i}")), v, "{}", o.text());
    }
}
pub(super) fn smaller_old_snapshot(g: &mut NativeCodeGenerator, good_source: u64) {
    let mut h = H::begin(g);
    h.alloc(1, MAX_PAYLOAD, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    // Hold an independent pool reservation to force a smaller genuine Old run.
    pool_claim(h.g, 512, Y);
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report("smaller", |h| pool_metric(h.g, M::OldCapacity));
    h.repeat(255, |h| {
        h.alloc(1, MAX_PAYLOAD, 9);
        h.root(9);
    });
    h.alloc(2, MAX_PAYLOAD, 9);
    h.store(Src::Raw(9), 0, Src::Root(0), true);
    h.root(9);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    let active = h.g.create_text_label();
    h.counter(Counter::MarkActive);
    h.g.test_reg_reg(R::V0, R::V0);
    h.g.jcc_label(C::NotEqual, active);
    h.mark_start();
    h.g.bind_text_label(active);
    request_young_pages(h.g, 64);
    // The retained Old space has 254 pages left. After one normal move,
    // these 64 real Young pages need 260 pages of promotion reserve.
    h.repeat(64, |h| {
        h.alloc(1, MAX_PAYLOAD, 20);
        h.root(20);
    });
    h.collect();
    h.finish_collection();
    h.report_counter("full_before", Counter::FullCollections);
    if good_source != 0 {
        h.value(Src::Root(9));
        h.keep(21);
    }
    h.collect();
    if good_source != 0 {
        // Root 9 is the Record in the second populated source run. Deliberately
        // encode its old address with current colors, then require physical
        // source membership to repair the field before normal scanning.
        h.store(Src::Root(9), 0, Src::Raw(21), false);
        h.load_barriered(Src::Root(9), 0);
        h.keep(22);
        h.report("good_source_repaired", |h| {
            h.value(Src::Root(9));
            h.g.load_rbp_slot(R::V1, super::slot_offset(22));
            let bad = h.g.create_text_label();
            let done = h.g.create_text_label();
            h.g.cmp_reg_reg(R::V0, R::V1);
            h.g.jcc_label(C::NotEqual, bad);
            h.g.mov_imm64(R::V0, 1);
            h.g.jmp_label(done);
            h.g.bind_text_label(bad);
            h.g.mov_imm64(R::V0, 0);
            h.g.bind_text_label(done);
        });
        h.store(Src::Root(9), 0, Src::Root(0), true);
    }
    h.finish_collection();
    h.report_counter("full_after", Counter::FullCollections);
    h.report_counter("active", Counter::MarkActive);
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("linked", |h| h.deref_field(Src::Root(9), 0, 0));
}
#[test]
fn production_shared_pool_smaller_old_run_closes_its_snapshot_before_emergency_full_relocation() {
    let o = run(smaller_old_snapshot, 0, false);
    assert!(o.get("smaller") < 512, "{}", o.text());
    assert_eq!(
        o.get("full_after"),
        o.get("full_before") + 1,
        "{}",
        o.text()
    );
    assert_eq!(o.get("active"), 0, "{}", o.text());
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("linked"), 84, "{}", o.text());
}

fn mark_bank_identity(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    h.collect();
    h.finish_collection();
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.root(1);
    h.collect();
    // Owner copy consumes fresh-arena scratch metadata. The source still has
    // fresh metadata from the previous bank until this copy is published.
    h.value(Src::Root(1));
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.close_relocation);
    h.g.call_label(h.l.unlock);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 99);
    h.root(2);
    h.report_counter("cleanup", Counter::RetireStage);
    h.finish_collection();
    h.report("copied_id", |h| h.object_id(Src::Root(1)));
    h.report("fresh_id", |h| h.object_id(Src::Root(2)));
    h.report("copied_value", |h| h.load(Src::Root(1), 0));
    h.report("fresh_value", |h| h.load(Src::Root(2), 0));
}
#[test]
fn production_shared_pool_mark_banks_preserve_fresh_identity_during_source_cleanup() {
    let o = run(mark_bank_identity, 0, false);
    assert!(o.get("cleanup") > 0);
    assert_eq!(o.get("copied_id"), 2, "{}", o.text());
    assert_eq!(o.get("fresh_id"), 3, "{}", o.text());
    assert_eq!(o.get("copied_value"), 84);
    assert_eq!(o.get("fresh_value"), 99);
}

fn ordered_old_claims(g: &mut NativeCodeGenerator, partial: u64) {
    use crate::gc_codegen::production::test_support::{
        old_claim_cursor, old_claim_probe, pool_claim_at,
    };
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    // Startup owns Young [0,21) and Old [21,63). Real independent gaps
    // leave [63,70), [71,75), [76,128) free; all metadata stays Ready.
    pool_claim_at(h.g, 70);
    pool_claim_at(h.g, 75);
    for pass in 0..8 {
        old_claim_probe(
            h.g,
            pass == 0,
            if pass == 1 && partial != 0 { 3 } else { 20 },
        );
        h.report(&format!("base{pass}"), |h| {
            h.g.load_rbp_slot(R::V0, 8 * 46);
        });
        h.report(&format!("pages{pass}"), |h| {
            h.g.load_rbp_slot(R::V0, 8 * 47);
        });
        h.report(&format!("cursor{pass}"), |h| old_claim_cursor(h.g));
    }
    for page in [63, 69, 71, 74, 76, 95, 96, 127] {
        h.report(&format!("role{page}"), |h| pool_role(h.g, page));
    }
    for page in [70, 75] {
        h.report(&format!("gap{page}"), |h| pool_role(h.g, page));
    }
}
#[test]
fn production_old_pool_claims_keep_small_runs_and_resume_without_revisiting_pages() {
    for worker in [false, true] {
        for partial in [0, 1] {
            let o = run(ordered_old_claims, partial, worker);
            let expected: &[(i64, i64, i64)] = if partial == 0 {
                &[
                    (76, 20, 0),
                    (63, 7, 71),
                    (71, 4, 76),
                    (96, 20, 116),
                    (116, 12, 128),
                    (0, 0, 128),
                    (0, 0, 128),
                    (0, 0, 128),
                ]
            } else {
                &[
                    (76, 20, 0),
                    (63, 3, 66),
                    (66, 4, 71),
                    (71, 4, 76),
                    (96, 20, 116),
                    (116, 12, 128),
                    (0, 0, 128),
                    (0, 0, 128),
                ]
            };
            for (pass, &(base, pages, cursor)) in expected.iter().enumerate() {
                assert_eq!(o.get(&format!("base{pass}")), base, "{}", o.text());
                assert_eq!(o.get(&format!("pages{pass}")), pages, "{}", o.text());
                assert_eq!(o.get(&format!("cursor{pass}")), cursor, "{}", o.text());
            }
            for page in [63, 69, 71, 74, 76, 95, 96, 127] {
                assert_eq!(o.get(&format!("role{page}")), O as i64, "{}", o.text());
            }
            for page in [70, 75] {
                assert_eq!(o.get(&format!("gap{page}")), Y as i64, "{}", o.text());
            }
        }
    }
}

fn full_old_claim_order(g: &mut NativeCodeGenerator, budget: u64) {
    use crate::gc_codegen::production::test_support::{
        old_claim_budget, old_claim_cursor, old_claim_remaining, old_claim_run, old_run_metric,
        pool_claim_at,
    };
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    // Startup Old has no objects. Free runs have lengths 4, 7 and 52, so
    // repeated largest searches would select 52, 7, 4 rather than 52, 4, 7.
    pool_claim_at(h.g, 67);
    pool_claim_at(h.g, 75);
    old_claim_budget(h.g, budget);
    h.report("count", |h| old_run_metric(h.g, false));
    h.report("capacity", |h| pool_metric(h.g, M::OldCapacity));
    h.report("remaining", |h| old_claim_remaining(h.g));
    h.report("cursor", |h| old_claim_cursor(h.g));
    for index in 0..3 {
        h.report(&format!("base{index}"), |h| {
            old_claim_run(h.g, index, false)
        });
        h.report(&format!("pages{index}"), |h| {
            old_claim_run(h.g, index, true)
        });
    }
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
}
#[test]
fn production_full_old_claimant_uses_a_single_forward_pass_after_its_first_run() {
    for worker in [false, true] {
        for budget in [60, 63, 70] {
            let o = run(full_old_claim_order, budget, worker);
            assert_eq!(o.get("count"), 3, "{}", o.text());
            assert_eq!(o.get("capacity"), budget.min(63) as i64, "{}", o.text());
            assert_eq!(o.get("remaining"), budget.saturating_sub(63) as i64);
            assert_eq!(
                o.get("cursor"),
                if budget == 60 {
                    72
                } else if budget == 63 {
                    75
                } else {
                    128
                }
            );
            for (index, (base, pages)) in
                [(76, 52), (63, 4), (68, if budget == 60 { 4 } else { 7 })]
                    .into_iter()
                    .enumerate()
            {
                assert_eq!(o.get(&format!("base{index}")), base, "{}", o.text());
                assert_eq!(o.get(&format!("pages{index}")), pages, "{}", o.text());
            }
            assert_eq!(o.get("value"), 84);
            assert_eq!(o.get("id"), 1);
        }
    }
}
