//! Deterministic clock faults and bounded phase history in emitted native code.
use super::{H, Src, run};
use crate::gc_abi::Phase;
use crate::gc_codegen::production::trace;
use crate::portable_asm::{PortableAsm, Reg as R};
use crate::{GcLogFormat, NativeCodeGenerator};
fn configure(g: &mut NativeCodeGenerator, cap: u64) {
    g.gc_log = true;
    g.gc_log_format = GcLogFormat::Json;
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = 4 << 20;
    g.gc_trace_capacity = cap;
}
fn report(h: &mut H<'_>, name: &str, offset: usize) {
    h.report(name, |h| {
        crate::gc_codegen::production::test_support::runtime_value(h.g, offset)
    });
}
fn rendezvous_clock_faults(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, 16);
    let mut h = H::begin(g);
    for (request, ready, end) in [
        (100, 110, 120),
        (100, 90, 120),
        (100, 110, 105),
        (0, 110, 120),
        (100, 110, (i64::MAX as u64) + 1),
        (100, 100, 100),
    ] {
        crate::gc_codegen::production::rendezvous_time::test_interval(h.g, request, ready, end);
    }
    let base = trace::buffer_offset(h.g) as usize;
    for row in 0..6 {
        for (name, field) in [("wait", 32), ("hold", 40), ("status", 56)] {
            report(&mut h, &format!("{name}{row}"), base + row * 64 + field);
        }
    }
}
#[test]
fn production_rendezvous_marks_clock_failures_and_regressions() {
    let r = run(rendezvous_clock_faults, 0, false);
    assert_eq!(
        (r.get("wait0"), r.get("hold0"), r.get("status0")),
        (10, 10, 7)
    );
    for row in 1..5 {
        assert_eq!(
            (
                r.get(&format!("wait{row}")),
                r.get(&format!("hold{row}")),
                r.get(&format!("status{row}"))
            ),
            (0, 0, 0)
        );
    }
    assert_eq!(
        (r.get("wait5"), r.get("hold5"), r.get("status5")),
        (0, 0, 7)
    );
}
fn park_clock_faults(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, 16);
    let mut h = H::begin(g);
    for (start, end) in [
        (100, 110),
        (100, 100),
        (100, 90),
        (0, 110),
        (100, 0),
        (100, (i64::MAX as u64) + 1),
    ] {
        crate::gc_codegen::production::rendezvous_time::test_park_interval(h.g, start, end);
    }
    let base = trace::buffer_offset(h.g) as usize;
    for row in 0..6 {
        report(&mut h, &format!("duration{row}"), base + row * 64 + 40);
        report(&mut h, &format!("status{row}"), base + row * 64 + 56);
    }
}
#[test]
fn production_park_trace_distinguishes_zero_duration_from_clock_faults() {
    let r = run(park_clock_faults, 0, false);
    assert_eq!((r.get("duration0"), r.get("status0")), (10, 3));
    assert_eq!((r.get("duration1"), r.get("status1")), (0, 3));
    for row in 2..6 {
        assert_eq!(
            (
                r.get(&format!("duration{row}")),
                r.get(&format!("status{row}"))
            ),
            (0, 0)
        );
    }
}
fn faults(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, 32);
    let mut h = H::begin(g);
    let runtime = h.g.gc_gen.take().unwrap();
    for (generation, phase, ns) in [
        (1, Phase::MarkStart, 100),
        (2, Phase::MarkStart, 110),
        (1, Phase::ConcurrentMark, 130),
        (2, Phase::ConcurrentMark, 0),
        (1, Phase::Select, 120),
        (2, Phase::Select, 150),
        (1, Phase::RelocateStart, 160),
        (1, Phase::ConcurrentRelocate, 170),
        (1, Phase::Retire, 180),
        (1, Phase::Idle, 190),
        (1, Phase::RelocateStart, 200),
        (2, Phase::RelocateStart, 210),
    ] {
        trace::record_at(h.g, &runtime.labels, generation, phase, ns);
    }
    // Same phase is not a new transition and does not sample the clock.
    trace::record_at(h.g, &runtime.labels, 2, Phase::RelocateStart, 0);
    trace::record_at(
        h.g,
        &runtime.labels,
        2,
        Phase::Retire,
        (i64::MAX as u64) + 1,
    );
    trace::record_at(h.g, &runtime.labels, 2, Phase::Idle, 210);
    trace::record_at(h.g, &runtime.labels, 2, Phase::MarkStart, 220);
    for (i, reg) in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11]
        .into_iter()
        .enumerate()
    {
        h.g.mov_imm64(reg, 60 + i as u64);
    }
    trace::record_at(h.g, &runtime.labels, 1, Phase::ConcurrentRelocate, 230);
    for (i, reg) in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11]
        .into_iter()
        .enumerate()
    {
        h.g.mov_reg_reg(R::V0, reg);
        h.keep(30 + i as u32);
    }
    h.g.gc_gen = Some(runtime);
    for i in 0..6 {
        h.report(&format!("saved{i}"), |h| h.value(Src::Raw(30 + i)));
    }
    for (name, offset) in [
        ("count", trace::COUNT),
        ("invalid", trace::INVALID),
        ("dropped", trace::DROPPED),
        ("young_cycle", trace::YOUNG),
        ("old_cycle", trace::OLD),
    ] {
        report(&mut h, name, offset);
    }
    let base = trace::buffer_offset(h.g) as usize;
    for (name, row, field) in [
        ("young_original", 2, 16),
        ("old_original", 5, 16),
        ("failed_status", 3, 56),
        ("backward_status", 4, 56),
        ("failed_ns", 3, 8),
        ("backward_ns", 4, 8),
        ("old_after_failure", 5, 56),
        ("young_after_failure", 6, 56),
        ("valid_elapsed", 7, 48),
        ("valid_status", 7, 56),
        ("young_new", 10, 16),
        ("old_retained", 11, 16),
        ("high_clock_status", 12, 56),
        ("old_restart", 14, 16),
    ] {
        report(&mut h, name, base + row * 64 + field);
    }
}
#[test]
fn production_phase_trace_preserves_independent_cycles_and_marks_clock_failures_explicitly() {
    let r = run(faults, 0, false);
    for (name, want) in [
        ("count", 16),
        ("invalid", 3),
        ("dropped", 0),
        ("young_cycle", 3),
        ("old_cycle", 4),
        ("young_original", 1),
        ("old_original", 2),
        ("failed_status", 0),
        ("backward_status", 0),
        ("failed_ns", 0),
        ("backward_ns", 0),
        ("old_after_failure", 1),
        ("young_after_failure", 1),
        ("valid_elapsed", 10),
        ("valid_status", 3),
        ("young_new", 3),
        ("old_retained", 2),
        ("high_clock_status", 0),
        ("old_restart", 4),
        ("saved0", 60),
        ("saved1", 61),
        ("saved2", 62),
        ("saved3", 63),
        ("saved4", 64),
        ("saved5", 65),
    ] {
        assert_eq!(r.get(name), want, "{name}");
    }
}
fn full(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, 2);
    let mut h = H::begin(g);
    let end = trace::buffer_offset(h.g) + 128;
    h.g.mov_imm64(R::V0, 42);
    h.g.store_ptr_disp32(R::V3, end, R::V0);
    let runtime = h.g.gc_gen.take().unwrap();
    for (phase, ns) in [
        (Phase::MarkStart, 100),
        (Phase::ConcurrentMark, 110),
        (Phase::Select, 120),
        (Phase::RelocateStart, 130),
        (Phase::Idle, 140),
        (Phase::MarkStart, 150),
    ] {
        trace::record_at(h.g, &runtime.labels, 1, phase, ns);
    }
    h.g.gc_gen = Some(runtime);
    for (name, offset) in [
        ("count", trace::COUNT),
        ("dropped", trace::DROPPED),
        ("invalid", trace::INVALID),
        ("cycle", trace::YOUNG),
        ("phase", trace::YPHASE),
        ("canary", end as usize),
    ] {
        report(&mut h, name, offset);
    }
    let base = trace::buffer_offset(h.g) as usize;
    report(&mut h, "first_ns", base + 8);
    report(&mut h, "last_ns", base + 64 + 8);
}
#[test]
fn production_phase_trace_overflow_retains_history_and_updates_cycle_state_without_overwrite() {
    let r = run(full, 0, false);
    for (name, want) in [
        ("count", 2),
        ("dropped", 4),
        ("invalid", 0),
        ("cycle", 2),
        ("phase", 1),
        ("canary", 42),
        ("first_ns", 100),
        ("last_ns", 110),
    ] {
        assert_eq!(r.get(name), want, "{name}");
    }
}

fn completed_old(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, 128);
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.mark_start();
    h.drain_marks();
    // Real charged Old occupancy gets a temporary capacity ceiling; no phase
    // or trace record is fabricated. The real collect/finish path closes Old.
    crate::gc_codegen::production::test_support::request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("value", |h| {
        h.value(Src::Root(0));
        h.g.load_ptr_disp32(R::V0, R::V0, 0);
    });
}
#[test]
fn production_phase_trace_records_real_old_mark_full_relocation_and_idle_with_one_cycle_id() {
    let output = super::execute(completed_old, 0, false, false);
    assert!(output.status.success(), "{output:?}");
    assert_eq!(output.stdout, b"value=84\n");
    let text = String::from_utf8(output.stderr).unwrap();
    let field = |line: &str, key: &str| -> u64 {
        line.split_once(&format!(",\"{key}\":"))
            .unwrap()
            .1
            .split([',', '}'])
            .next()
            .unwrap()
            .parse()
            .unwrap()
    };
    let events = text
        .lines()
        .filter(|s| s.contains("\"event\":\"phase_transition\""))
        .collect::<Vec<_>>();
    let rendezvous = text
        .lines()
        .filter(|s| s.contains("\"event\":\"rendezvous\""))
        .collect::<Vec<_>>();
    assert!(!rendezvous.is_empty(), "{text}");
    assert!(
        rendezvous.iter().all(|s| {
            field(s, "round_id") > 0
                && field(s, "participants") >= 1
                && field(s, "timestamp_ns") > 0
                && field(s, "clock_status") == 7
                && field(s, "time_to_safepoint_ns") <= field(s, "timestamp_ns")
                && field(s, "hold_until_end_ns") <= field(s, "timestamp_ns")
        }),
        "{text}"
    );
    assert!(
        rendezvous
            .windows(2)
            .all(|rows| field(rows[0], "round_id") < field(rows[1], "round_id")),
        "{text}"
    );
    let old = events
        .iter()
        .filter(|s| field(s, "generation") == 2)
        .collect::<Vec<_>>();
    assert_eq!(
        old.iter().map(|s| field(s, "phase")).collect::<Vec<_>>(),
        vec![1, 2, 3, 4, 5, 6, 7, 0],
        "{text}"
    );
    let cycle = field(old[0], "cycle_id");
    assert!(cycle > 0);
    assert!(old.iter().all(|s| field(s, "cycle_id") == cycle));
    assert!(
        events
            .iter()
            .filter(|s| field(s, "generation") == 1)
            .all(|s| field(s, "cycle_id") != cycle)
    );
    assert!(
        events
            .iter()
            .all(|s| field(s, "timestamp_ns") > 0 && field(s, "clock_status") != 0)
    );
    assert!(
        text.lines()
            .last()
            .unwrap()
            .contains("\"event\":\"exit_snapshot\"")
    );
}
