//! A Native primary must not supply the independently executing worker's phase.
use super::{H, Src, run};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::PortableAsm;
use crate::portable_asm::Reg as R;

fn native_primary(g: &mut crate::NativeCodeGenerator, moving: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.store(Src::Raw(0), 0, Src::Null, true);
    h.set_scalar(Src::Raw(0), 1, 91);
    h.root(0);
    if moving != 0 {
        h.repeat(1024, |h| {
            h.alloc(2, 16, 1);
            h.store(Src::Raw(1), 0, Src::Root(0), true);
            h.set_scalar(Src::Raw(1), 1, 91);
            h.value(Src::Raw(1));
            h.set_root(0);
        });
    }
    publish_primary(h.g);
    let actor = create_actor(h.g, 8, 8);
    h.report("address_before", |h| h.value(Src::Root(0)));
    h.report("identity_before", |h| h.object_id(Src::Root(0)));
    // Enter primary Native through its actual ABI. The other actual owner is
    // the only Running context; this is not N simultaneous allocating threads.
    let enter = h.g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
    call_owner(h.g, enter);
    h.report("primary_before", |h| worker_view_metric(h.g, 4));
    other_owner_cycle(h.g, actor, moving != 0);
    if moving != 0 {
        worker_view_wait_relocation(h.g);
        for (kind, name) in [
            (0, "root_color"),
            (1, "address_after"),
            (2, "identity_after"),
            (3, "graph_nodes"),
        ] {
            h.report(name, |h| {
                h.g.load_rbp_slot(R::V0, super::slot_offset(0));
                worker_view_native_root(h.g, kind);
            });
        }
    }
    worker_view_wait(h.g);
    let epoch = worker_epoch_sample(h.g);
    for (i, name) in [
        "epoch_active",
        "epoch_entered",
        "epoch_completed",
        "epoch_phase",
        "epoch_bank",
        "epoch_cache",
        "current_bank",
    ]
    .into_iter()
    .enumerate()
    {
        h.report(name, |h| worker_epoch_metric(h.g, epoch, i));
    }
    h.report("primary_after", |h| worker_view_metric(h.g, 4));
    h.report("worker_phase", |h| worker_view_metric(h.g, 0));
    h.report("shared_phase", |h| worker_view_metric(h.g, 5));
    h.report("worker_identity", |h| worker_view_metric(h.g, 1));
    h.report("worker_segment", |h| worker_view_metric(h.g, 2));
    h.report("worker_registered", |h| worker_view_metric(h.g, 3));
    h.report("worker_tlab", |h| worker_view_metric(h.g, 6));
    h.report("worker_owner_lock", |h| worker_view_metric(h.g, 7));
    h.report_counter("worker_units", Counter::MarkWorkerUnits);
    h.report("worker_copies", |h| worker_view_metric(h.g, 8));
    // The Native owner does not return across a moving bank change. Its
    // cached snapshot and private producer storage must remain untouched.
}

#[test]
fn production_worker_relocates_native_primary_graph_with_another_owner_phase_snapshot() {
    let o = run(native_primary, 1, true);
    assert_eq!(o.get("epoch_active"), 0);
    assert!(o.get("epoch_entered") > 0);
    assert_eq!(o.get("epoch_entered"), o.get("epoch_completed"));
    assert_eq!(o.get("epoch_phase"), o.get("epoch_cache"));
    assert_eq!(o.get("epoch_bank"), o.get("current_bank"));
    assert_eq!(
        o.get("primary_before"),
        o.get("primary_after"),
        "{}",
        o.text()
    );
    assert_ne!(o.get("address_before"), o.get("address_after"));
    assert_eq!(o.get("identity_before"), o.get("identity_after"));
    assert_eq!(o.get("graph_nodes"), 1025);
    assert!(o.get("worker_copies") > 0);
    assert_ne!(o.get("worker_phase"), o.get("primary_after"));
    assert_eq!(o.get("worker_phase"), o.get("shared_phase"));
    let phase = crate::gc_abi::PhaseSnapshot::from_bits(o.get("shared_phase") as u64).unwrap();
    assert_eq!(o.get("root_color"), i64::from(phase.color_unremembered()));
    for field in [
        "worker_identity",
        "worker_segment",
        "worker_registered",
        "worker_tlab",
        "worker_owner_lock",
    ] {
        assert_eq!(o.get(field), 0, "{field}: {}", o.text());
    }
}

#[test]
fn production_worker_uses_its_own_phase_view_while_primary_remains_native() {
    let o = run(native_primary, 0, true);
    assert_eq!(o.get("epoch_active"), 0);
    assert!(o.get("epoch_entered") > 0);
    assert_eq!(o.get("epoch_entered"), o.get("epoch_completed"));
    assert_eq!(o.get("epoch_phase"), o.get("epoch_cache"));
    assert_eq!(o.get("epoch_bank"), o.get("current_bank"));
    assert_eq!(
        o.get("primary_before"),
        o.get("primary_after"),
        "{}",
        o.text()
    );
    assert_ne!(o.get("worker_phase"), o.get("primary_after"));
    assert_eq!(o.get("worker_phase"), o.get("shared_phase"));
    for field in [
        "worker_identity",
        "worker_segment",
        "worker_registered",
        "worker_tlab",
        "worker_owner_lock",
    ] {
        assert_eq!(o.get(field), 0, "{field}: {}", o.text());
    }
    assert!(o.get("worker_units") > 0);
}

fn epoch_fault(g: &mut crate::NativeCodeGenerator, mode: u64) {
    worker_epoch_fault(g, mode);
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    publish_primary(h.g);
    worker_view_wait(h.g);
}
#[test]
fn production_actual_worker_epoch_rejects_changed_snapshot_unclosed_access_and_bank_advance() {
    for mode in 0..5 {
        let o = super::execute(epoch_fault, mode, true, false);
        assert_eq!(o.status.code(), Some(1), "mode={mode}: {o:?}");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
            "mode={mode}: {o:?}"
        );
    }
}

fn epoch_exhausted(g: &mut crate::NativeCodeGenerator, _: u64) {
    let h = H::begin(g);
    worker_epoch_exhaust(h.g);
}
#[test]
fn production_actual_worker_rejects_epoch_identity_exhaustion_before_reuse() {
    let o = super::execute(epoch_exhausted, 0, true, false);
    assert_eq!(o.status.code(), Some(1), "{o:?}");
    assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
}
