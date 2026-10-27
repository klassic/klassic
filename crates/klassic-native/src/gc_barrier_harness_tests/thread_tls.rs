//! Actual GS context association and rejection of a foreign native GS owner.
use super::{H, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::*;
fn foreign(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let actor = create_actor(h.g, 16, 8);
    h.report("init", |h| tls_runtime(h.g, true));
    let foreign = tls_foreign_gs(h.g, true);
    h.report("occupied", |h| tls_actor_install(h.g, actor));
    h.report("users", |h| tls_metric(h.g, true));
    h.report("foreign_same", |h| tls_foreign_matches(h.g, foreign));
    tls_foreign_gs(h.g, false);
    h.report("empty", |h| tls_query_value(h.g));
    h.report("install", |h| tls_actor_install(h.g, actor));
    h.report("current", |h| tls_current_is_actor(h.g, actor));
    h.report("users_installed", |h| tls_metric(h.g, true));
    h.report("busy", |h| tls_runtime(h.g, false));
    h.report("begin", |h| actor_detach_call(h.g, actor, true));
    actor_detach_wait(h.g, actor);
    h.report("clear", |h| tls_actor_clear(h.g, actor));
    h.report("dispose", |h| actor_detach_dispose(h.g, actor));
    h.report("delete", |h| tls_runtime(h.g, false));
}
#[test]
fn production_fresh_tls_install_keeps_foreign_gs_unchanged_and_rolls_back_reservation() {
    let o = run(foreign, 0, true);
    for name in [
        "init", "users", "empty", "install", "begin", "clear", "dispose", "delete",
    ] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
    assert_eq!(o.get("foreign_same"), 1, "{}", o.text());
    assert_eq!(o.get("occupied"), 2, "{}", o.text());
    assert_eq!(o.get("users_installed"), 1, "{}", o.text());
    assert_eq!(o.get("busy"), 2, "{}", o.text());
}

fn bound_disposal(g: &mut NativeCodeGenerator, _: u64) {
    let h = H::begin(g);
    let actor = create_actor(h.g, 16, 8);
    tls_runtime(h.g, true);
    tls_actor_install(h.g, actor);
    actor_detach_call(h.g, actor, true);
    actor_detach_wait(h.g, actor);
    // Participant/root teardown is complete, but a real OS TLS association
    // still retains this native context. The disposal entry must reject it.
    actor_detach_dispose(h.g, actor);
}
#[test]
fn production_context_backing_stays_owned_until_its_actual_os_tls_association_is_cleared() {
    let o = super::execute(bound_disposal, 0, true, false);
    assert_eq!(o.status.code(), Some(1), "{o:?}");
    assert!(
        String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
        "{o:?}"
    );
}

fn inherited(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let actor = create_actor(h.g, 16, 8);
    h.report("init", |h| tls_runtime(h.g, true));
    h.report("install", |h| tls_actor_install(h.g, actor));
    h.report("begin", |h| actor_detach_call(h.g, actor, true));
    actor_detach_wait(h.g, actor);
    let child = start_inherited_tls_clear_thread(h.g, actor);
    join_attach_thread(h.g, &child);
    h.report("inherited_current", |h| {
        inherited_tls_current_result(h.g, &child)
    });
    h.report("inherited_wrong_clear", |h| {
        inherited_tls_clear_result(h.g, &child)
    });
    h.report("users_retained", |h| tls_metric(h.g, true));
    h.report("parent_kept_value", |h| tls_value_is_actor(h.g, actor));
    h.report("probe_release", |h| {
        tls_release_unregistered_probe(h.g, child.actor)
    });
    h.report("clear", |h| tls_actor_clear(h.g, actor));
    h.report("dispose", |h| actor_detach_dispose(h.g, actor));
    h.report("delete", |h| tls_runtime(h.g, false));
}
#[test]
fn production_inherited_gs_pointer_does_not_let_a_new_os_thread_release_its_creator_context() {
    let o = run(inherited, 0, true);
    assert_eq!(o.get("inherited_wrong_clear"), 3, "{}", o.text());
    assert_eq!(o.get("inherited_current"), 3, "{}", o.text());
    for name in ["users_retained", "parent_kept_value"] {
        assert_eq!(o.get(name), 1, "{}", o.text());
    }
    for name in [
        "init",
        "install",
        "begin",
        "probe_release",
        "clear",
        "dispose",
        "delete",
    ] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
}

fn authenticated_current(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.report("not_ready", |h| tls_current_status(h.g));
    h.report("init", |h| tls_runtime(h.g, true));
    h.report("empty", |h| tls_current_status(h.g));
    // A canonical but unmapped host GS value must never be dereferenced.
    tls_set_unmapped_gs(h.g, 0x1000);
    h.report("unmapped", |h| tls_current_status(h.g));
    tls_set_unmapped_gs(h.g, 0);
    let actor = create_actor(h.g, 16, 8);
    h.report("install", |h| tls_actor_install(h.g, actor));
    h.report("current", |h| tls_current_is_actor(h.g, actor));
    // This is an actual retained traversal, not a manufactured reader count.
    let lookup = tls_lookup_start(h.g);
    h.report("readers", |h| tls_readers(h.g));
    h.report("begin", |h| actor_detach_call(h.g, actor, true));
    actor_detach_wait(h.g, actor);
    h.report("detached_current", |h| tls_current_is_actor(h.g, actor));
    h.report("clear", |h| tls_actor_clear(h.g, actor));
    h.report("retained_dispose", |h| actor_detach_dispose(h.g, actor));
    h.report("retained_delete", |h| tls_runtime(h.g, false));
    h.report("lookup_removed", |h| tls_lookup_resume(h.g, lookup));
    h.report("readers_final", |h| tls_readers(h.g));
    h.report("dispose", |h| actor_detach_dispose(h.g, actor));
    h.report("current_final", |h| tls_current_status(h.g));
    h.report("delete", |h| tls_runtime(h.g, false));
}
#[test]
fn production_current_context_authenticates_unmapped_gs_and_retains_removed_cursor_backing() {
    let o = run(authenticated_current, 0, true);
    for name in ["not_ready", "retained_dispose", "retained_delete"] {
        assert_eq!(o.get(name), 2, "{}", o.text());
    }
    for name in ["unmapped", "lookup_removed"] {
        assert_eq!(o.get(name), 3, "{}", o.text());
    }
    for name in ["current", "readers", "detached_current"] {
        assert_eq!(o.get(name), 1, "{}", o.text());
    }
    for name in [
        "init",
        "empty",
        "install",
        "begin",
        "clear",
        "readers_final",
        "dispose",
        "current_final",
        "delete",
    ] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
}
