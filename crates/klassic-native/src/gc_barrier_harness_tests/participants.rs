//! Registered production ownership, native exclusion and phase acknowledgements.
use super::{H, Src, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::PortableAsm;
use ParticipantMetric as M;

fn metric(h: &mut H<'_>, name: &str, m: M) {
    h.report(name, |h| participant_metric(h.g, m));
}
fn lifecycle(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    metric(&mut h, "members", M::Members);
    metric(&mut h, "registered", M::Registered);
    metric(&mut h, "identity", M::Identity);
    metric(&mut h, "initial", M::Generation);
    h.alloc(1, 24, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.mark_start();
    metric(&mut h, "mark_round", M::Generation);
    metric(&mut h, "mark_ack", M::OwnerGeneration);
    h.g.call_label(h.l.native_enter);
    metric(&mut h, "native", M::Native);
    metric(&mut h, "blocked_round", M::Generation);
    h.drain_marks();
    h.g.call_label(h.l.native_exit);
    metric(&mut h, "returned_native", M::Native);
    h.collect();
    h.finish_collection();
    metric(&mut h, "final_round", M::Generation);
    metric(&mut h, "final_ack", M::OwnerGeneration);
    metric(&mut h, "expected", M::Expected);
    metric(&mut h, "acknowledged", M::Acknowledged);
    metric(&mut h, "active", M::Active);
    h.report("state", |h| native_context_state(h.g));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
}
#[test]
fn production_participants_register_and_acknowledge_each_real_phase() {
    for worker in [false, true] {
        let o = run(lifecycle, 0, worker);
        for name in ["members", "registered", "identity", "native"] {
            assert_eq!(o.get(name), 1, "{}", o.text());
        }
        assert_eq!(o.get("initial"), 0);
        assert!(o.get("mark_round") > 0, "{}", o.text());
        assert_eq!(o.get("mark_round"), o.get("mark_ack"));
        assert_eq!(o.get("blocked_round"), o.get("mark_round"));
        assert!(o.get("final_round") > o.get("mark_round"));
        assert_eq!(o.get("final_round"), o.get("final_ack"));
        for name in [
            "returned_native",
            "expected",
            "acknowledged",
            "active",
            "state",
        ] {
            assert_eq!(o.get(name), 0, "{}", o.text());
        }
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("id"), 1);
    }
}
fn round(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    for (name, op) in [
        ("begin", 6),
        ("early_end", 7),
        ("ack", 1),
        ("duplicate", 1),
        ("end", 7),
        ("resume", 2),
    ] {
        h.report(name, |h| participant_step(h.g, op));
    }
    metric(&mut h, "round", M::Generation);
    metric(&mut h, "owner_round", M::OwnerGeneration);
    h.report("state", |h| native_context_state(h.g));
}
#[test]
fn production_participant_round_rejects_early_completion_and_duplicate_ack() {
    for worker in [false, true] {
        let o = run(round, 0, worker);
        for name in ["begin", "ack", "end", "resume", "state"] {
            assert_eq!(o.get(name), 0, "{}", o.text());
        }
        assert_eq!(o.get("early_end"), 5);
        assert_eq!(o.get("duplicate"), 4);
        assert_eq!(o.get("round"), o.get("owner_round"));
        assert_eq!(o.get("round"), 1);
    }
}
fn invalid(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    match mode {
        0 => participant_set(h.g, M::Registered, 0),
        1 => participant_set(h.g, M::Identity, 2),
        2 => participant_set(h.g, M::Generation, u64::MAX),
        3 => {
            h.g.call_label(h.l.native_enter);
        }
        _ => unreachable!(),
    }
    h.mark_start();
}
#[test]
fn production_phase_rejects_unregistered_stale_exhausted_and_native_owner() {
    for mode in 0..4 {
        let o = run_failure(invalid, mode);
        assert!(!o.status.success(), "mode {mode}");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
            "{o:?}"
        );
    }
}

fn incomplete(g: &mut NativeCodeGenerator, total: u64) {
    let mut h = H::begin(g);
    participant_add_unacknowledged(h.g, total);
    metric(&mut h, "members", M::Members);
    h.mark_start();
}
#[test]
fn production_phase_requires_acknowledgement_from_every_registered_context() {
    for total in [2, 8] {
        let o = run_failure(incomplete, total);
        assert!(
            String::from_utf8_lossy(&o.stdout).contains(&format!("members={total}\n")),
            "{o:?}"
        );
        assert!(!o.status.success());
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
            "{o:?}"
        );
    }
}

fn publication(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.report_counter("before", Counter::AdoptedPhase);
    if mode > 0 {
        h.report("begin", |h| participant_step(h.g, 6));
    }
    if mode == 2 {
        h.report("ack", |h| participant_step(h.g, 1));
    }
    h.report("published", |h| participant_adopt(h.g));
    h.report_counter("after", Counter::AdoptedPhase);
    if mode == 1 {
        h.report("ack", |h| participant_step(h.g, 1));
    }
    if mode > 0 {
        h.report("end", |h| participant_step(h.g, 7));
        h.report("resume", |h| participant_step(h.g, 2));
    }
}
#[test]
fn production_phase_publication_requires_active_round_and_all_acks_before_the_store() {
    for worker in [false, true] {
        for mode in 0..3 {
            let o = run(publication, mode, worker);
            assert_eq!(o.get("published"), i64::from(mode == 2), "{}", o.text());
            assert_eq!(
                o.get("after") - o.get("before"),
                if mode == 2 { 0x10000 } else { 0 }
            );
            if mode > 0 {
                for name in ["begin", "ack", "end", "resume"] {
                    assert_eq!(o.get(name), 0, "{}", o.text());
                }
            }
        }
    }
}
