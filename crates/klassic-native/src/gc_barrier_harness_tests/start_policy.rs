//! Production controller arithmetic, observed timestamps and real trigger use.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::{
    Counter, start_allocation_sample, start_mark_sample, start_policy_input,
};
use crate::portable_asm::{PortableAsm, Reg as R};
fn rates(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    start_allocation_sample(h.g, [1_000_000, 0, 1 << 20, 2_000_000]);
    h.report_counter("rate1", Counter::StartAllocationRate);
    start_allocation_sample(h.g, [2_000_000, 1 << 20, 5 << 20, 3_000_000]);
    h.report_counter("rate2", Counter::StartAllocationRate);
    h.report_counter("samples", Counter::StartRateSamples);
    for input in [
        [2, 0, 100, 2],   // equal clock
        [2, 0, 100, 1],   // backwards clock
        [2, 0, 100, 0],   // failed clock
        [1, 100, 99, 2],  // allocation counter wrap
        [1, 100, 100, 2], // no fresh work
    ] {
        start_allocation_sample(h.g, input);
    }
    h.report_counter("invalid", Counter::StartInvalid);
    h.report_counter("unchanged", Counter::StartAllocationRate);
    start_allocation_sample(h.g, [0, 0, 50, 7]); // Rebase, no rate sample.
    h.report_counter("rebased", Counter::StartRateSamples);
    start_allocation_sample(h.g, [1, 0, u64::MAX, 2]); // Wide product saturation.
    h.report_counter("capped", Counter::StartAllocationRate);
    start_mark_sample(h.g, [1_000_000, 64, 1064, 2_000_000]);
    h.report_counter("mark", Counter::StartMarkRate);
    h.report_counter("work", Counter::StartUnits);
    start_mark_sample(h.g, [2_000_000, 1064, 5064, 3_000_000]);
    h.report_counter("mark2", Counter::StartMarkRate);
    for input in [
        [2, 10, 11, 2],
        [0, 10, 11, 2],
        [1, 11, 10, 2],
        [1, 10, 10, 2],
    ] {
        start_mark_sample(h.g, input);
    }
    h.report_counter("mark_samples", Counter::StartMarkSamples);
    h.report_counter("invalid2", Counter::StartInvalid);
}
#[test]
fn production_start_rates_use_elapsed_time_smooth_and_reject_invalid_samples_without_division_traps()
 {
    let o = run(rates, 0, false);
    for (name, value) in [
        ("rate1", 1 << 20),
        ("rate2", 7 * (1 << 18)),
        ("samples", 2),
        ("invalid", 5),
        ("unchanged", 7 * (1 << 18)),
        ("rebased", 2),
        ("capped", (3 * 7 * (1 << 18) + (1 << 30)) / 4),
        ("mark", 1000),
        ("mark2", 1750),
        ("work", 1000),
        ("mark_samples", 2),
        ("invalid2", 9),
    ] {
        assert_eq!(o.get(name), value, "{name}: {}", o.text());
    }
}
fn policy(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.report_counter("bootstrap", Counter::StartTarget);
    start_policy_input(h.g, [64, 1000, 100, 1000, 0]);
    h.report_counter("first", Counter::StartTarget);
    start_policy_input(h.g, [64, 1000, 100, 1000, 0]);
    h.report_counter("up", Counter::StartTarget);
    h.report_counter("updates", Counter::StartUpdates);
    start_policy_input(h.g, [80, 1 << 20, 1000, 100_000, 0]);
    h.report_counter("opposite_first", Counter::StartTarget);
    start_policy_input(h.g, [80, 1 << 20, 1000, 100_000, 0]);
    h.report_counter("down", Counter::StartTarget);
    // Repeated extremes stop at the bounds; they cannot wrap below MIN.
    for _ in 0..2 {
        start_policy_input(h.g, [13, 1 << 20, 1, u64::MAX, 0]);
    }
    h.report_counter("minimum", Counter::StartTarget);
    for _ in 0..2 {
        start_policy_input(h.g, [190, 1, 1 << 30, 1, 0]);
    }
    h.report_counter("maximum", Counter::StartTarget); // Within four-page deadband.
    for _ in 0..2 {
        start_policy_input(h.g, [180, 1, 1 << 30, 1, 0]);
    }
    h.report_counter("upper", Counter::StartTarget);
    for _ in 0..2 {
        start_policy_input(h.g, [64, 1000, 100, 1000, 30 << 20]);
    }
    h.report_counter("survival", Counter::StartTarget);
    // Opposite direction cancels the first vote, neutral cancels both.
    start_policy_input(h.g, [64, 1000, 100, 1000, 0]);
    start_policy_input(h.g, [64, 1 << 20, 1000, 100_000, 0]);
    start_policy_input(h.g, [64, 1000, 100, 1000, 0]);
    h.report_counter("alternating", Counter::StartTarget);
    start_policy_input(h.g, [190, 1, 1 << 30, 1, 0]);
    start_policy_input(h.g, [64, 1000, 100, 1000, 0]);
    h.report_counter("neutral_reset", Counter::StartTarget);
}
#[test]
fn production_start_target_uses_rates_survival_bounds_deadband_and_two_cycle_hysteresis() {
    let o = run(policy, 0, false);
    for (name, value) in [
        ("updates", 1),
        ("bootstrap", 64),
        ("first", 64),
        ("up", 80),
        ("opposite_first", 80),
        ("down", 64),
        ("minimum", 8),
        ("maximum", 190),
        ("upper", 192),
        ("survival", 48),
        ("alternating", 64),
        ("neutral_reset", 64),
    ] {
        assert_eq!(o.get(name), value, "{name}: {}", o.text());
    }
}
fn clock(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.g.call_label(h.l.clock);
    h.keep(0);
    h.g.mov_imm64(R::V6, 123456);
    h.g.mov_imm64(R::V2, 789012);
    h.g.call_label(h.l.clock);
    h.keep(1);
    h.g.mov_reg_reg(R::V0, R::V6);
    h.keep(2);
    h.g.mov_reg_reg(R::V0, R::V2);
    h.keep(3);
    for (name, slot) in [("first", 0), ("second", 1), ("saved6", 2), ("saved2", 3)] {
        h.report(name, |h| h.value(Src::Raw(slot)));
    }
}
#[test]
fn production_start_clock_is_monotonic_and_preserves_leaf_registers() {
    let o = run(clock, 0, false);
    assert!(o.get("first") > 0);
    assert!(o.get("second") >= o.get("first"));
    assert_eq!(o.get("saved6"), 123456);
    assert_eq!(o.get("saved2"), 789012);
}

fn early(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    crate::gc_codegen::production::test_support::set_young_auto(h.g, true);
    for _ in 0..2 {
        // Numerical warm-up chooses eight pages. All following allocations,
        // snapshot starts, barriers, marking and moves are the real runtime.
        start_policy_input(h.g, [13, 1 << 20, 1, u64::MAX, 0]);
    }
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.repeat(9, |h| {
        h.alloc(1, crate::gc_codegen::production::MAX_PAYLOAD, 9)
    });
    h.report_counter("target", Counter::StartTarget);
    h.report_counter("epoch", Counter::YoungEpoch);
    h.report_counter("old", Counter::MarkCycles);
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
}
#[test]
fn production_start_target_drives_real_young_and_old_marking_before_the_former_fixed_trigger() {
    let o = run(early, 0, true);
    assert_eq!(o.get("target"), 8, "{}", o.text());
    assert!(o.get("epoch") > 0, "{}", o.text());
    assert!(o.get("old") > 0, "{}", o.text());
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("id"), 1);
}

fn evidence(g: &mut NativeCodeGenerator, _arg: u64) {
    use crate::gc_codegen::production::test_support::observe_start_policy;
    let mut h = H::begin(g);
    start_policy_input(h.g, [64, 1000, 100, 1000, 0]);
    observe_start_policy(h.g, false, 0);
    h.report_counter("reused", Counter::StartTarget);
    start_policy_input(h.g, [64, 1000, 100, 1000, 0]);
    h.report_counter("new", Counter::StartTarget);
    // Rounded units/ms cannot shorten an observed long wall-time sample.
    start_policy_input(h.g, [64, 100_000, 1 << 30, 1, 0]);
    observe_start_policy(h.g, true, 1_000_000_000);
    observe_start_policy(h.g, true, 1_000_000_000);
    h.report_counter("duration", Counter::StartTarget);
}
#[test]
fn production_start_feedback_requires_new_measurements_and_never_predicts_less_than_observed_wall_time()
 {
    let o = run(evidence, 0, false);
    assert_eq!(o.get("reused"), 64, "{}", o.text());
    assert_eq!(o.get("new"), 80, "{}", o.text());
    assert_eq!(o.get("duration"), 48, "{}", o.text());
}
fn chains(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    for i in 0..8 {
        h.alloc(1, crate::gc_codegen::production::MAX_PAYLOAD, i);
        h.value(Src::Raw(i));
        h.keep(20 + i);
        h.root(i);
    }
    h.collect();
    for i in 0..8 {
        h.worker_copy(Src::Raw(20 + i));
    }
    h.finish_collection();
    for _ in 0..2 {
        start_policy_input(h.g, [13, 1 << 20, 1, u64::MAX, 0]);
    }
    h.report("trigger", |h| {
        crate::gc_codegen::production::test_support::at_start_trigger(h.g)
    });
    h.report("owner_pages", |h| {
        crate::gc_codegen::production::test_support::young_mutator_pages(h.g)
    });
}
#[test]
fn production_start_headroom_counts_worker_private_pages_alongside_the_mutator_chain() {
    let o = run(chains, 0, false);
    assert_eq!(o.get("trigger"), 1, "{}", o.text());
    assert!(o.get("owner_pages") < 8, "{}", o.text());
}
