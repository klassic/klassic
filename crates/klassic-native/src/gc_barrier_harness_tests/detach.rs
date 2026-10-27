//! Real owner state, registered-region teardown and actual OS backing release.
use super::{H, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::*;

fn lifecycle(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let actor = create_actor(h.g, 32, 16);
    actor_detach_seed(h.g, actor, 3);
    actor_native_boundary(h.g, actor, true);
    actor_native_boundary(h.g, actor, false);
    h.report("members_before", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
    h.report("begin", |h| actor_detach_call(h.g, actor, true));
    h.report("detaching", |h| actor_detach_metric(h.g, actor, 0));
    actor_detach_wait(h.g, actor);
    for (name, field) in [
        ("state", 0),
        ("registered", 1),
        ("roots", 2),
        ("retiring", 3),
        ("submitted", 4),
        ("finished", 5),
        ("handles", 6),
    ] {
        h.report(name, |h| actor_detach_metric(h.g, actor, field));
    }
    h.report("members_after", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
    native_root_unmap_fail_next(h.g);
    h.report("dispose_failed", |h| actor_detach_dispose(h.g, actor));
    h.report("retained_detached", |h| actor_detach_metric(h.g, actor, 0));
    h.report("dispose", |h| actor_detach_dispose(h.g, actor));
    // A subsequent actual collection must never discover the unmapped owner.
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("members_final", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
}
#[test]
fn production_detach_retires_every_owned_region_removes_participant_and_releases_context_backing() {
    for worker in [false, true] {
        let o = run(lifecycle, 0, worker);
        assert_eq!(o.get("members_before"), 2, "{}", o.text());
        assert_eq!(o.get("detaching"), 3, "{}", o.text());
        assert_eq!(o.get("state"), 4, "{}", o.text());
        assert_eq!(o.get("dispose_failed"), 6, "{}", o.text());
        assert_eq!(o.get("retained_detached"), 4, "{}", o.text());
        assert_eq!(o.get("submitted"), 4, "{}", o.text());
        assert_eq!(o.get("finished"), 4, "{}", o.text());
        for name in ["members_after", "members_final"] {
            assert_eq!(o.get(name), 1, "{}", o.text());
        }
        for name in [
            "begin",
            "registered",
            "roots",
            "retiring",
            "handles",
            "dispose",
        ] {
            assert_eq!(o.get(name), 0, "{}", o.text());
        }
    }
}

fn native_handles(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let actor = create_actor(h.g, 32, 16);
    let source = actor_detach_seed(h.g, actor, 0);
    let (handle, _) = native_root_actor_prepare(h.g, actor, source);
    h.report("handles", |h| actor_detach_metric(h.g, actor, 6));
    h.report("busy", |h| actor_detach_call(h.g, actor, true));
    h.report("running", |h| actor_detach_metric(h.g, actor, 0));
    // Failed actual munmap retains both the token and its creator context.
    native_root_unmap_fail_next(h.g);
    h.report("release_failed", |h| {
        actor_native_release(h.g, actor, handle)
    });
    h.report("handles_failed", |h| actor_detach_metric(h.g, actor, 6));
    h.report("still_busy", |h| actor_detach_call(h.g, actor, true));
    h.report("release", |h| actor_native_release(h.g, actor, handle));
    h.report("handles_released", |h| actor_detach_metric(h.g, actor, 6));
    h.report("begin", |h| actor_detach_call(h.g, actor, true));
    actor_detach_wait(h.g, actor);
    h.report("dispose", |h| actor_detach_dispose(h.g, actor));
}
#[test]
fn production_detach_keeps_native_handle_creator_running_until_actual_release_succeeds() {
    let o = run(native_handles, 0, true);
    assert_eq!(o.get("handles"), 1, "{}", o.text());
    assert_eq!(o.get("handles_failed"), 1, "{}", o.text());
    for name in ["busy", "still_busy"] {
        assert_eq!(o.get(name), 2, "{}", o.text());
    }
    assert_eq!(o.get("release_failed"), 6, "{}", o.text());
    for name in ["running", "release", "handles_released", "begin", "dispose"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
}

fn threads(g: &mut NativeCodeGenerator, arg: u64) {
    let count = if arg & 1 == 0 { 2 } else { 8 };
    let moving = arg & 2 != 0;
    let tls = arg & 4 != 0;
    let mut h = H::begin(g);
    if tls {
        h.report("tls_init", |h| tls_runtime(h.g, true));
    }

    if moving {
        let enable = h.g.gc_gen.as_ref().unwrap().labels.moving_enable_actor;
        call_owner(h.g, enable);
    } else {
        dual_mark_enable(h.g);
    }
    let threads: Vec<_> = (0..count)
        .map(|_| {
            if tls {
                start_tls_detach_thread(h.g)
            } else {
                start_detach_thread(h.g)
            }
        })
        .collect();
    let readers: Vec<_> = threads
        .iter()
        .map(|t| {
            detach_wait_metric(h.g, t.actor, 0, 3);
            actor_frame_reader(h.g, t.actor)
        })
        .collect();
    if tls {
        h.report("tls_users", |h| tls_metric(h.g, true));
        h.report("tls_busy", |h| tls_runtime(h.g, false));
    }
    if !moving {
        dual_mark_entry(h.g, false, true);
        dual_mark_entry(h.g, true, true);
    }
    for t in &threads {
        detach_thread_retire(h.g, t);
    }
    // A real reader prevents completion while the actual owner keeps ACKing.
    for t in &threads {
        detach_wait_metric(h.g, t.actor, 3, 1);
    }
    if moving {
        detach_request_full_move(h.g);
        finish_move(h.g);
        h.report_counter("full", Counter::FullCollections);
    } else {
        dual_mark_wait(h.g, None);
        worker_journal_wait(h.g, 4, 1);
        dual_mark_entry(h.g, true, false);
        dual_mark_entry(h.g, false, false);
    }
    for (i, t) in threads.iter().enumerate() {
        h.report(&format!("state{i}"), |h| {
            actor_detach_metric(h.g, t.actor, 0)
        });
        h.report(&format!("value{i}"), |h| detach_retained_value(h.g, t.root));
        h.report(&format!("acks{i}"), |h| {
            actor_metric(h.g, t.actor, ActorMetric::CheckpointAcks)
        });
    }
    for q in &readers {
        retirement_reader_release(h.g, *q);
    }
    if tls {
        for t in &threads {
            detach_wait_metric(h.g, t.actor, 0, 4);
        }
        h.report("tls_detached_users", |h| tls_metric(h.g, true));
        h.report("tls_detached_busy", |h| tls_runtime(h.g, false));
        for (i, t) in threads.iter().enumerate() {
            h.report(&format!("wrong_clear{i}"), |h| {
                tls_actor_clear(h.g, t.actor)
            });
        }
        h.report("tls_after_wrong_clear", |h| tls_metric(h.g, true));
        for t in &threads {
            detach_thread_allow_tls_clear(h.g, t);
        }
    }
    for (i, t) in threads.iter().enumerate() {
        join_attach_thread(h.g, t);
        h.report(&format!("tls{i}"), |h| detach_thread_flag(h.g, t, 1));
        h.report(&format!("detached{i}"), |h| {
            actor_detach_metric(h.g, t.actor, 0)
        });
        h.report(&format!("dispose{i}"), |h| {
            actor_detach_dispose_wait(h.g, t.actor)
        });
    }
    if tls {
        h.report("tls_users_final", |h| tls_metric(h.g, true));
        h.report("tls_current_final", |h| tls_query_value(h.g));
        h.report("tls_delete", |h| tls_runtime(h.g, false));
    }
    h.report("members", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
    if moving {
        detach_request_full_move(h.g);
        finish_move(h.g);
    } else {
        dual_mark_entry(h.g, false, true);
        dual_mark_entry(h.g, true, true);
        dual_mark_wait(h.g, None);
        worker_journal_wait(h.g, 4, 1);
        dual_mark_entry(h.g, true, false);
        dual_mark_entry(h.g, false, false);
    }
}
#[test]
fn production_two_and_eight_detaching_threads_ack_real_dual_marks_and_full_moves_before_tls_context_release()
 {
    for arg in 0..4 {
        let o = run(threads, arg, true);
        let count = if arg & 1 == 0 { 2 } else { 8 };
        for i in 0..count {
            assert_eq!(o.get(&format!("state{i}")), 3, "arg {arg}: {}", o.text());
            assert_eq!(o.get(&format!("value{i}")), 84, "arg {arg}: {}", o.text());
            assert!(o.get(&format!("acks{i}")) >= 2, "arg {arg}: {}", o.text());
            assert_eq!(o.get(&format!("tls{i}")), 0, "arg {arg}: {}", o.text());
            assert_eq!(o.get(&format!("detached{i}")), 4, "arg {arg}: {}", o.text());
            assert_eq!(o.get(&format!("dispose{i}")), 0, "arg {arg}: {}", o.text());
        }
        assert_eq!(o.get("members"), 1, "arg {arg}: {}", o.text());
        if arg & 2 != 0 {
            assert!(o.get("full") > 0, "{}", o.text());
        }
    }
}

fn invalid_disposal(g: &mut NativeCodeGenerator, mode: u64) {
    let h = H::begin(g);
    let actor = create_actor(h.g, 16, 8);
    actor_detach_seed(h.g, actor, 0);
    if mode == 1 || mode == 3 {
        actor_detach_call(h.g, actor, true);
    }
    if mode == 2 {
        actor_native_boundary(h.g, actor, true);
    }
    if mode == 3 {
        actor_detach_wait(h.g, actor);
        actor_dispose_while_bound(h.g, actor);
    } else {
        actor_detach_dispose(h.g, actor);
    }
}
#[test]
fn production_context_disposal_rejects_running_native_detaching_and_still_bound_owners() {
    for mode in 0..4 {
        let o = super::execute(invalid_disposal, mode, true, false);
        assert_eq!(o.status.code(), Some(1), "mode {mode}: {o:?}");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
            "mode {mode}: {o:?}"
        );
    }
}

#[test]
fn production_actual_gs_tls_is_thread_private_through_detaching_dual_marks_and_full_moves() {
    for arg in 4..8 {
        let o = run(threads, arg, true);
        let count = if arg & 1 == 0 { 2 } else { 8 };
        assert_eq!(o.get("tls_users"), count, "arg {arg}: {}", o.text());
        assert_eq!(o.get("tls_busy"), 2, "arg {arg}: {}", o.text());
        assert_eq!(
            o.get("tls_detached_users"),
            count,
            "arg {arg}: {}",
            o.text()
        );
        assert_eq!(o.get("tls_detached_busy"), 2, "arg {arg}: {}", o.text());
        assert_eq!(
            o.get("tls_after_wrong_clear"),
            count,
            "arg {arg}: {}",
            o.text()
        );
        for i in 0..count {
            assert_eq!(
                o.get(&format!("wrong_clear{i}")),
                3,
                "arg {arg}: {}",
                o.text()
            );
        }
        for name in [
            "tls_init",
            "tls_users_final",
            "tls_current_final",
            "tls_delete",
        ] {
            assert_eq!(o.get(name), 0, "arg {arg}: {}", o.text());
        }
        for i in 0..count {
            for (name, v) in [
                ("state", 3),
                ("value", 84),
                ("tls", 0),
                ("detached", 4),
                ("dispose", 0),
            ] {
                assert_eq!(o.get(&format!("{name}{i}")), v, "arg {arg}: {}", o.text());
            }
            assert!(o.get(&format!("acks{i}")) >= 2, "{}", o.text());
        }
        if arg & 2 != 0 {
            assert!(o.get("full") > 0, "{}", o.text());
        }
        assert_eq!(o.get("members"), 1, "{}", o.text());
    }
}
