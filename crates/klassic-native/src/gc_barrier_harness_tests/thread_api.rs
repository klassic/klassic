//! Public unary native ABI owns the actual context allocation and cleanup.
use super::{H, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::PortableAsm;
fn lifecycle(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let f = thread_api_fixture(h.g);
    h.report("empty_current", |h| thread_api_call(h.g, f, 3));
    h.report("attach", |h| thread_api_call(h.g, f, 0));
    let actor = thread_api_actor(h.g, f);
    h.report("current", |h| thread_api_same_current(h.g, f, actor));
    h.report("repeat", |h| thread_api_call(h.g, f, 0));
    h.report("busy_shutdown", |h| thread_api_call(h.g, f, 4));
    native_root_unmap_fail_next(h.g);
    h.report("release_failure", |h| thread_api_detach_wait(h.g, f));
    h.report("retained_stage", |h| actor_detach_metric(h.g, actor, 0));
    h.report("shutdown_while_retained", |h| thread_api_call(h.g, f, 4));
    h.report("retry", |h| thread_api_detach_wait(h.g, f));
    h.report("key_after_retry", |h| tls_metric(h.g, false));
    h.report("token_empty", |h| thread_api_token_empty(h.g, f));
    h.report("shutdown", |h| thread_api_call(h.g, f, 4));
    h.report("reattach", |h| thread_api_call(h.g, f, 0));
    h.report("redetach", |h| thread_api_detach_wait(h.g, f));
    h.report("reshutdown", |h| thread_api_call(h.g, f, 4));
    h.report("empty_detach", |h| thread_api_call(h.g, f, 2));
    h.report("key_after_empty", |h| tls_metric(h.g, false));
}
#[test]
fn production_public_thread_api_allocates_and_retains_context_through_actual_release_failure() {
    let o = run(lifecycle, 0, true);
    for name in [
        "empty_current",
        "attach",
        "repeat",
        "retry",
        "shutdown_while_retained",
        "key_after_retry",
        "empty_detach",
        "key_after_empty",
        "shutdown",
        "reattach",
        "redetach",
        "reshutdown",
    ] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
    for name in ["current", "token_empty"] {
        assert_eq!(o.get(name), 1, "{}", o.text());
    }
    assert_eq!(o.get("busy_shutdown"), 2, "{}", o.text());
    assert_eq!(o.get("release_failure"), 6, "{}", o.text());
    assert_eq!(o.get("retained_stage"), 4, "{}", o.text());
}
fn refusals(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let f = thread_api_fixture(h.g);
    thread_api_cells(h.g, f, 0);
    h.report("invalid_cells", |h| thread_api_call(h.g, f, 0));
    h.report("invalid_empty", |h| thread_api_token_empty(h.g, f));
    thread_api_cells(h.g, f, 16);
    tls_set_unmapped_gs(h.g, 0x1000);
    h.report("foreign", |h| thread_api_call(h.g, f, 0));
    h.report("foreign_value", |h| tls_query_value(h.g));
    h.report("foreign_empty", |h| thread_api_token_empty(h.g, f));
    tls_set_unmapped_gs(h.g, 0);
    thread_api_fail_map(h.g);
    h.report("map_failure", |h| thread_api_call(h.g, f, 0));
    h.report("failure_empty", |h| thread_api_token_empty(h.g, f));
    h.report("members_failed", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
    h.report("attach", |h| thread_api_call(h.g, f, 0));
    let actor = thread_api_actor(h.g, f);
    let copied = thread_api_copied(h.g, f);
    h.report("copy_rejected", |h| thread_api_call(h.g, copied, 2));
    h.report("copy_outputs", |h| thread_api_outputs_zero(h.g, copied));
    h.report("original_current", |h| {
        thread_api_same_current(h.g, f, actor)
    });
    h.report("detach", |h| thread_api_detach_wait(h.g, f));
    h.report("shutdown", |h| thread_api_call(h.g, f, 4));
}
#[test]
fn production_public_thread_api_refuses_foreign_gs_copied_tokens_and_failed_mapping_without_registration()
 {
    let o = run(refusals, 0, true);
    for name in ["invalid_cells", "foreign", "copy_rejected"] {
        assert_eq!(o.get(name), 3, "{}", o.text());
    }
    for name in [
        "invalid_empty",
        "foreign_empty",
        "failure_empty",
        "members_failed",
        "copy_outputs",
        "original_current",
    ] {
        assert_eq!(o.get(name), 1, "{}", o.text());
    }
    for name in ["attach", "detach", "shutdown"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
    assert_eq!(o.get("foreign_value"), 0x1000, "{}", o.text());
    assert_eq!(o.get("map_failure"), 6, "{}", o.text());
}
fn moved_root(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let f = thread_api_fixture(h.g);
    h.report("attach", |h| thread_api_call(h.g, f, 0));
    let actor = thread_api_actor(h.g, f);
    let root = actor_detach_seed(h.g, actor, 0);
    h.report("enter", |h| thread_api_boundary(h.g, f, 0));
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("exit", |h| thread_api_boundary(h.g, f, 1));
    h.report("value", |h| {
        actor_load_root(h.g, actor, root);
        h.g.load_ptr_disp32(super::R::V0, super::R::V0, 0);
    });
    h.report("current", |h| thread_api_same_current(h.g, f, actor));
    h.report("detach", |h| thread_api_detach_wait(h.g, f));
    h.report("empty", |h| thread_api_token_empty(h.g, f));
    h.report("shutdown", |h| thread_api_call(h.g, f, 4));
}
#[test]
fn production_public_thread_api_binding_preserves_actual_frame_root_across_full_native_return() {
    let o = run(moved_root, 0, true);
    for name in ["attach", "enter", "exit", "detach", "shutdown"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
    for name in ["current", "empty"] {
        assert_eq!(o.get(name), 1, "{}", o.text());
    }
    assert_eq!(o.get("value"), 84, "{}", o.text());
}
fn public_threads(g: &mut NativeCodeGenerator, n: u64) {
    public_threads_impl(g, n, false, false);
}
fn inherited_public_threads(g: &mut NativeCodeGenerator, n: u64) {
    public_threads_impl(g, n, true, false);
}
fn owned_public_threads(g: &mut NativeCodeGenerator, n: u64) {
    public_threads_impl(g, n, false, true);
}
fn public_threads_impl(g: &mut NativeCodeGenerator, n: u64, inherited: bool, owned: bool) {
    let mut h = H::begin(g);
    let api = thread_api_fixture(h.g);
    let parent = if inherited || owned {
        h.report("parent_attach", |h| thread_api_call(h.g, api, 0));
        let parent = thread_api_actor(h.g, api);
        h.report("parent_enter", |h| thread_api_boundary(h.g, api, 0));
        Some(parent)
    } else {
        None
    };
    let threads: Vec<_> = (0..n)
        .map(|_| {
            if owned {
                start_owned_public_api_thread(h.g)
            } else if inherited {
                start_inherited_public_api_thread(h.g)
            } else {
                start_public_api_thread(h.g)
            }
        })
        .collect();
    if owned {
        h.report("parent_exit", |h| thread_api_boundary(h.g, api, 1));
        h.report("parent_detach", |h| thread_api_detach_wait(h.g, api));
        h.report("parent_empty", |h| thread_api_token_empty(h.g, api));
        h.report("users_before_admit", |h| tls_metric(h.g, true));
        for t in &threads {
            public_api_thread_admit(h.g, t);
        }
    }
    for t in &threads {
        public_api_thread_wait(h.g, t, false);
    }
    h.report("users", |h| tls_metric(h.g, true));
    h.report("members_ready", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
    h.report("busy_shutdown", |h| thread_api_call(h.g, api, 4));
    if let Some(parent) = parent.filter(|_| !owned) {
        h.report("parent_current_ready", |h| {
            thread_api_same_current(h.g, api, parent)
        });
    }
    for (i, t) in threads.iter().enumerate() {
        h.report(&format!("wrong{i}"), |h| thread_api_call(h.g, t.wrong, 2));
        h.report(&format!("wrong_zero{i}"), |h| {
            thread_api_outputs_zero(h.g, t.wrong)
        });
    }
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    for t in &threads {
        public_api_thread_go(h.g, t);
    }
    for t in &threads {
        public_api_thread_wait(h.g, t, true);
        public_api_thread_join(h.g, t);
    }
    for (i, t) in threads.iter().enumerate() {
        for (field, name) in [
            "attach", "detach", "value", "os", "creator", "identity", "vm_slot",
        ]
        .iter()
        .enumerate()
        {
            h.report(&format!("{name}{i}"), |h| {
                public_api_thread_metric(h.g, t, field)
            });
        }
    }
    if let Some(parent) = parent.filter(|_| !owned) {
        for (i, t) in threads.iter().enumerate() {
            h.report(&format!("inherited{i}"), |h| {
                public_api_thread_metric(h.g, t, 7)
            });
        }
        h.report("parent_current_final", |h| {
            thread_api_same_current(h.g, api, parent)
        });
        h.report("parent_exit", |h| thread_api_boundary(h.g, api, 1));
        h.report("parent_detach", |h| thread_api_detach_wait(h.g, api));
        h.report("parent_empty", |h| thread_api_token_empty(h.g, api));
    }
    if owned {
        for (i, t) in threads.iter().enumerate() {
            h.report(&format!("fresh_gs{i}"), |h| {
                public_api_thread_metric(h.g, t, 7)
            });
            h.report(&format!("bootstrap{i}"), |h| {
                public_api_thread_metric(h.g, t, 8)
            });
        }
    }
    h.report("users_final", |h| tls_metric(h.g, true));
    h.report("members_final", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
    h.report("shutdown", |h| thread_api_call(h.g, api, 4));
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
}

#[test]
fn production_owned_thread_launch_clears_gs_before_attach_after_creator_context_release() {
    for n in [2, 8] {
        let o = run(owned_public_threads, n, true);
        for name in [
            "parent_attach",
            "parent_enter",
            "parent_exit",
            "parent_detach",
            "users_before_admit",
            "users_final",
            "shutdown",
        ] {
            assert_eq!(o.get(name), 0, "{}", o.text());
        }
        assert_eq!(o.get("parent_empty"), 1, "{}", o.text());
        assert_eq!(o.get("users"), n as i64, "{}", o.text());
        assert_eq!(o.get("members_ready"), n as i64 + 1, "{}", o.text());
        assert_eq!(o.get("members_final"), 1, "{}", o.text());
        let mut tids = std::collections::HashSet::new();
        for i in 0..n {
            for name in ["attach", "detach", "fresh_gs", "vm_slot"] {
                assert_eq!(o.get(&format!("{name}{i}")), 0, "{}", o.text());
            }
            assert_eq!(o.get(&format!("bootstrap{i}")), 1, "{}", o.text());
            assert_eq!(o.get(&format!("value{i}")), 84, "{}", o.text());
            assert_eq!(o.get(&format!("wrong{i}")), 3, "{}", o.text());
            assert_eq!(o.get(&format!("wrong_zero{i}")), 1, "{}", o.text());
            let tid = o.get(&format!("os{i}"));
            assert!(tid > 0 && tids.insert(tid), "{}", o.text());
            assert_eq!(o.get(&format!("creator{i}")), tid, "{}", o.text());
        }
    }
}

fn owned_foreign_parent(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.report("initialize", |h| tls_runtime(h.g, true));
    tls_set_unmapped_gs(h.g, 0x1000);
    let t = start_owned_public_api_thread(h.g);
    h.report("parent_gs", |h| tls_query_value(h.g));
    tls_set_unmapped_gs(h.g, 0);
    public_api_thread_admit(h.g, &t);
    public_api_thread_wait(h.g, &t, false);
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    public_api_thread_go(h.g, &t);
    public_api_thread_wait(h.g, &t, true);
    public_api_thread_join(h.g, &t);
    for (field, name) in [
        (0, "attach"),
        (1, "detach"),
        (2, "value"),
        (7, "child_gs"),
        (8, "bootstrap"),
    ] {
        h.report(name, |h| public_api_thread_metric(h.g, &t, field));
    }
    h.report("users_final", |h| tls_metric(h.g, true));
    let f = thread_api_fixture(h.g);
    h.report("shutdown", |h| thread_api_call(h.g, f, 4));
}

#[test]
fn production_owned_thread_launch_clears_only_child_gs_and_keeps_foreign_parent_value() {
    let o = run(owned_foreign_parent, 0, true);
    assert_eq!(o.get("parent_gs"), 0x1000, "{}", o.text());
    assert_eq!(o.get("bootstrap"), 1, "{}", o.text());
    assert_eq!(o.get("value"), 84, "{}", o.text());
    for name in [
        "initialize",
        "attach",
        "detach",
        "child_gs",
        "users_final",
        "shutdown",
    ] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
}
#[test]
fn production_public_thread_api_two_and_eight_os_threads_allocate_root_adopt_full_and_release_their_own_contexts()
 {
    for n in [2, 8] {
        let o = run(public_threads, n, true);
        assert_eq!(o.get("users"), n as i64, "{}", o.text());
        assert_eq!(o.get("members_ready"), n as i64 + 1, "{}", o.text());
        assert_eq!(o.get("busy_shutdown"), 2, "{}", o.text());
        for name in ["users_final", "shutdown"] {
            assert_eq!(o.get(name), 0, "{}", o.text());
        }
        assert_eq!(o.get("members_final"), 1, "{}", o.text());
        let mut tids = std::collections::HashSet::new();
        let mut identities = std::collections::HashSet::new();
        for i in 0..n {
            for name in ["attach", "detach", "vm_slot"] {
                assert_eq!(o.get(&format!("{name}{i}")), 0, "{}", o.text());
            }
            assert_eq!(o.get(&format!("value{i}")), 84, "{}", o.text());
            assert_eq!(o.get(&format!("wrong{i}")), 3, "{}", o.text());
            assert_eq!(o.get(&format!("wrong_zero{i}")), 1, "{}", o.text());
            let tid = o.get(&format!("os{i}"));
            assert!(tid > 0 && tids.insert(tid), "{}", o.text());
            assert_eq!(o.get(&format!("creator{i}")), tid, "{}", o.text());
            let identity = o.get(&format!("identity{i}"));
            assert!(identity > 1 && identities.insert(identity), "{}", o.text());
        }
    }
}

#[test]
fn production_public_thread_api_bootstraps_inherited_gs_without_releasing_parent_association() {
    for n in [2, 8] {
        let o = run(inherited_public_threads, n, true);
        assert_eq!(o.get("users"), n as i64 + 1, "{}", o.text());
        assert_eq!(o.get("members_ready"), n as i64 + 2, "{}", o.text());
        assert_eq!(o.get("busy_shutdown"), 2, "{}", o.text());
        for name in [
            "parent_current_ready",
            "parent_current_final",
            "parent_empty",
        ] {
            assert_eq!(o.get(name), 1, "{}", o.text());
        }
        for name in [
            "parent_attach",
            "parent_enter",
            "parent_exit",
            "parent_detach",
            "users_final",
            "shutdown",
        ] {
            assert_eq!(o.get(name), 0, "{}", o.text());
        }
        assert_eq!(o.get("members_final"), 1, "{}", o.text());
        let mut tids = std::collections::HashSet::new();
        for i in 0..n {
            assert_eq!(o.get(&format!("inherited{i}")), 3, "{}", o.text());
            assert_eq!(o.get(&format!("wrong{i}")), 3, "{}", o.text());
            assert_eq!(o.get(&format!("wrong_zero{i}")), 1, "{}", o.text());
            for name in ["attach", "detach", "vm_slot"] {
                assert_eq!(o.get(&format!("{name}{i}")), 0, "{}", o.text());
            }
            assert_eq!(o.get(&format!("value{i}")), 84, "{}", o.text());
            let tid = o.get(&format!("os{i}"));
            assert!(tid > 0 && tids.insert(tid), "{}", o.text());
            assert_eq!(o.get(&format!("creator{i}")), tid, "{}", o.text());
        }
    }
}
