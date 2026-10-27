//! Emitted collection intervals: clock faults, preserved callers and real wait/finish work.
use super::{H, Src, run, run_failure};
use crate::gc_codegen::production::{mutator_time as t, trace};
use crate::portable_asm::{PortableAsm, Reg as R};
use crate::{GcLogFormat, NativeCodeGenerator};
fn configure(g: &mut NativeCodeGenerator, capacity: u64) {
    g.gc_log = true;
    g.gc_log_format = GcLogFormat::Json;
    g.gc_trace_capacity = capacity;
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = 4 << 20;
}
fn report(h: &mut H<'_>, name: &str, offset: usize) {
    h.report(name, |h| {
        crate::gc_codegen::production::test_support::runtime_value(h.g, offset)
    });
}
fn faults(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, 32);
    let mut h = H::begin(g);
    let runtime = h.g.gc_gen.take().unwrap();
    let high = i64::MAX as u64 + 1;
    for (start, end) in [
        (100, 110),
        (110, 110),
        (0, 120),
        (130, 0),
        (140, 139),
        (high, 150),
        (160, high),
        (170, 180),
    ] {
        t::begin_at(h.g, &runtime.labels, start);
        t::end_at(h.g, &runtime.labels, end);
    }
    // Both endpoints must preserve the interrupted collector's inputs/result.
    let regs = [
        R::V0,
        R::V1,
        R::V2,
        R::V6,
        R::V7,
        R::V8,
        R::V9,
        R::V10,
        R::V11,
    ];
    for (i, r) in regs.into_iter().enumerate() {
        h.g.mov_imm64(r, 60 + i as u64);
    }
    t::begin_at(h.g, &runtime.labels, 190);
    t::end_at(h.g, &runtime.labels, 190);
    // Save all values before report() itself clobbers its caller registers.
    for (i, r) in regs.into_iter().enumerate() {
        h.g.store_rbp_slot(super::slot_offset(20 + i as u32), r);
    }
    h.g.gc_gen = Some(runtime);
    for i in 0..9 {
        h.report(&format!("saved{i}"), |h| h.value(Src::Raw(20 + i)));
    }
    for (name, offset) in [
        ("calls", t::CALLS),
        ("valid", t::VALID),
        ("invalid", t::INVALID),
        ("total", t::TOTAL),
        ("max", t::MAX),
        ("last", t::LAST),
        ("clock_invalid", t::CLOCK_INVALID),
        ("trace_invalid", trace::INVALID),
        ("count", trace::COUNT),
        ("active", t::ACTIVE),
        ("last_status", t::LAST_STATUS),
    ] {
        report(&mut h, name, offset);
    }
    let base = trace::buffer_offset(h.g) as usize;
    for row in 0..9 {
        for (name, field) in [("duration", 40), ("start_valid", 48), ("status", 56)] {
            report(&mut h, &format!("{name}{row}"), base + row * 64 + field);
        }
    }
}
#[test]
fn production_mutator_collection_timer_distinguishes_invalid_and_measured_zero_and_preserves_registers()
 {
    let r = run(faults, 0, false);
    for (name, want) in [
        ("calls", 9),
        ("valid", 4),
        ("invalid", 5),
        ("total", 20),
        ("max", 10),
        ("last", 0),
        ("clock_invalid", 5),
        ("trace_invalid", 5),
        ("count", 9),
        ("active", 0),
        ("last_status", 3),
    ] {
        assert_eq!(r.get(name), want, "{name}");
    }
    for i in 0..9 {
        assert_eq!(r.get(&format!("saved{i}")), 60 + i);
    }
    for (row, (duration, start_valid, status)) in [
        (10, 1, 3),
        (0, 1, 3),
        (0, 0, 1),
        (0, 1, 0),
        (0, 1, 0),
        (0, 0, 1),
        (0, 1, 0),
        (10, 1, 3),
        (0, 1, 3),
    ]
    .into_iter()
    .enumerate()
    {
        for (name, want) in [
            ("duration", duration),
            ("start_valid", start_valid),
            ("status", status),
        ] {
            assert_eq!(r.get(&format!("{name}{row}")), want);
        }
    }
}
fn overflow(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, 2);
    let mut h = H::begin(g);
    let base = trace::buffer_offset(h.g) as usize;
    h.g.mov_imm64(R::V0, 42);
    h.g.store_ptr_disp32(R::V3, (base + 128) as i32, R::V0);
    let runtime = h.g.gc_gen.take().unwrap();
    trace::record_at(
        h.g,
        &runtime.labels,
        1,
        crate::gc_abi::Phase::MarkStart,
        100,
    );
    for (start, end) in [(110, 120), (130, 140)] {
        t::begin_at(h.g, &runtime.labels, start);
        t::end_at(h.g, &runtime.labels, end);
    }
    trace::record_at(
        h.g,
        &runtime.labels,
        1,
        crate::gc_abi::Phase::ConcurrentMark,
        150,
    );
    h.g.gc_gen = Some(runtime);
    for (name, offset) in [
        ("count", trace::COUNT),
        ("dropped", trace::DROPPED),
        ("calls", t::CALLS),
        ("total", t::TOTAL),
        ("canary", base + 128),
        ("first_tag", base + 24),
        ("second_tag", base + 64 + 24),
        ("second_duration", base + 64 + 40),
    ] {
        report(&mut h, name, offset);
    }
}
#[test]
fn production_mutator_collection_timer_shares_bounded_history_and_keeps_aggregating_after_overflow()
{
    let r = run(overflow, 0, false);
    for (name, want) in [
        ("count", 2),
        ("dropped", 2),
        ("calls", 2),
        ("total", 20),
        ("canary", 42),
        ("first_tag", 1),
        ("second_tag", 16),
        ("second_duration", 10),
    ] {
        assert_eq!(r.get(name), want, "{name}");
    }
}
fn invalid(g: &mut NativeCodeGenerator, case: u64) {
    configure(g, 16);
    let mut h = H::begin(g);
    h.report("before", |h| h.value(Src::Imm(1)));
    let runtime = h.g.gc_gen.take().unwrap();
    if case == 0 {
        t::end_at(h.g, &runtime.labels, 100);
    } else {
        t::begin_at(h.g, &runtime.labels, 100);
        if case == 1 {
            t::begin_at(h.g, &runtime.labels, 110);
        } else {
            h.g.mov_imm64(R::V0, i64::MAX as u64);
            crate::gc_codegen::production::test_support::runtime_store(h.g, t::TOTAL, R::V0);
            t::end_at(h.g, &runtime.labels, 110);
        }
    }
    h.g.gc_gen = Some(runtime);
    h.report("after", |h| h.value(Src::Imm(1)));
}
#[test]
fn production_mutator_collection_timer_rejects_unbalanced_intervals_and_total_overflow() {
    for case in 0..3 {
        let r = run_failure(invalid, case);
        assert_eq!(r.status.code(), Some(1), "{r:?}");
        assert_eq!(r.stdout, b"before=1\n");
    }
}
fn real(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, 128);
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    // Start and drain a real Young mark. collect's actual wait path seals it
    // and publishes Select; those operations must lie inside the interval.
    h.g.call_label(h.l.young_start);
    h.g.call_label(h.l.lock);
    crate::gc_codegen::production::test_support::emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
    h.collect();
    // Leave relocation open. The second collect must include its real
    // Retire/Idle work, before starting another Young cycle.
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_mutator_collection_timer_encloses_real_young_wait_and_previous_relocation_finish() {
    let r = super::execute(real, 0, false, false);
    assert!(r.status.success(), "{r:?}");
    assert_eq!(r.stdout, b"value=84\n");
    let text = String::from_utf8(r.stderr).unwrap();
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
        .filter(|l| l.contains("\"event\":\"mutator_collection\""))
        .collect::<Vec<_>>();
    assert_eq!(events.len(), 2, "{text}");
    for e in &events {
        assert_eq!(field(e, "clock_status"), 3);
        assert!(field(e, "duration_ns") > 0);
        assert_eq!(
            field(e, "duration_ns"),
            field(e, "timestamp_ns") - field(e, "started_ns")
        );
    }
    let phases = text
        .lines()
        .filter(|l| l.contains("\"event\":\"phase_transition\"") && field(l, "generation") == 1)
        .collect::<Vec<_>>();
    let select = phases
        .iter()
        .find(|l| field(l, "phase") == crate::gc_abi::Phase::Select as u64)
        .unwrap();
    let retire = phases.iter().find(|l| field(l, "phase") == 7).unwrap();
    for (i, phase) in [(0, select), (1, retire)] {
        assert!(
            field(events[i], "started_ns") <= field(phase, "timestamp_ns"),
            "{text}"
        );
        assert!(
            field(phase, "timestamp_ns") <= field(events[i], "timestamp_ns"),
            "{text}"
        );
    }
    let summary = text.lines().last().unwrap();
    assert_eq!(field(summary, "mutator_collect_calls"), 2);
    assert_eq!(field(summary, "mutator_collect_valid"), 2);
    assert_eq!(field(summary, "mutator_collect_invalid"), 0);
}
