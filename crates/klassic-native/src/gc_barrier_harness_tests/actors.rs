//! Real owner-private contexts, real shared heap/arena and legacy ABI state.
//! Idle interleaving is groundwork; no concurrent mark/phase claim is made.
use super::{H, Src, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_abi::{OBJECT_INITIALIZING, OBJECT_PUBLISHED};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn owners(g: &mut NativeCodeGenerator, n: u64) {
    let mut h = H::begin(g);
    let actors = (0..n).map(|_| create_actor(h.g, 16, 8)).collect::<Vec<_>>();
    h.report("regions", |h| actor_active_regions(h.g));
    h.report("heap", |h| actor_shared_heap(h.g));
    h.report("arena", |h| actor_shared_arena(h.g));
    h.report("records", |h| actor_shared_records(h.g));
    for (i, actor) in actors.iter().copied().enumerate() {
        actor_alloc_without_ambient_runtime(h.g, actor, 24 + 16 * i as u64);
        h.keep(i as u32);
        h.set_scalar(Src::Raw(i as u32), 0, 100 + i as u64);
    }
    // Other owners must not publish this owner's pending object.
    for i in 0..n as u32 {
        h.report(&format!("initial{i}"), |h| {
            h.value(Src::Raw(i));
            actor_header_state(h.g);
        });
    }
    for (i, actor) in actors.iter().copied().enumerate() {
        actor_alloc(h.g, actor, 40 + 16 * i as u64);
        h.keep(n as u32 + i as u32);
        h.set_scalar(Src::Raw(n as u32 + i as u32), 0, 200 + i as u64);
        h.report(&format!("first{i}"), |h| {
            h.value(Src::Raw(i as u32));
            actor_header_state(h.g);
        });
        if i + 1 < actors.len() {
            h.report(&format!("next{i}"), |h| {
                h.value(Src::Raw(i as u32 + 1));
                actor_header_state(h.g);
            });
        }
    }
    for (i, actor) in actors.iter().copied().enumerate() {
        for (name, m) in [
            ("id", ActorMetric::Identity),
            ("scope", ActorMetric::Scope),
            ("heap", ActorMetric::Heap),
            ("arena", ActorMetric::Arena),
            ("record", ActorMetric::ArenaRecord),
            ("bytes", ActorMetric::Bytes),
            ("fresh", ActorMetric::FreshSize),
            ("lock", ActorMetric::Lock),
        ] {
            h.report(&format!("{name}{i}"), |h| actor_metric(h.g, actor, m));
        }
        h.report(&format!("pending{i}"), |h| {
            actor_metric(h.g, actor, ActorMetric::Pending);
            h.g.load_ptr_disp32(R::V0, R::V0, 0);
        });
        actor_publish(h.g, actor);
        h.report(&format!("ready{i}"), |h| {
            h.value(Src::Raw(n as u32 + i as u32));
            actor_header_state(h.g);
        });
        h.report(&format!("clear{i}"), |h| {
            actor_metric(h.g, actor, ActorMetric::Pending)
        });
    }
}
#[test]
fn production_explicit_owners_keep_private_pending_tlabs_and_requests_on_one_real_heap() {
    for n in [2, 8] {
        for worker in [false, true] {
            let o = run(owners, n, worker);
            assert_eq!(o.get("regions"), n as i64 + 1, "{}", o.text());
            for i in 0..n {
                assert_eq!(o.get(&format!("id{i}")), i as i64 + 2, "{}", o.text());
                assert_eq!(o.get(&format!("scope{i}")), i as i64 + 2, "{}", o.text());
                assert_eq!(o.get(&format!("heap{i}")), o.get("heap"), "{}", o.text());
                // Requests use disjoint owner-exclusive arena headers, with
                // exact reservations in the single real shared backing bank.
                assert_ne!(o.get(&format!("arena{i}")), o.get("arena"), "{}", o.text());
                for previous in 0..i {
                    assert_ne!(
                        o.get(&format!("arena{i}")),
                        o.get(&format!("arena{previous}"))
                    );
                }
                assert_eq!(
                    o.get(&format!("record{i}")),
                    o.get("records")
                        + (n + i) as i64 * std::mem::size_of::<crate::gc_abi::MarkRecord>() as i64,
                    "{}",
                    o.text()
                );
                assert_eq!(
                    o.get(&format!("initial{i}")),
                    OBJECT_INITIALIZING as i64,
                    "{}",
                    o.text()
                );
                assert_eq!(
                    o.get(&format!("first{i}")),
                    OBJECT_PUBLISHED as i64,
                    "{}",
                    o.text()
                );
                if i + 1 < n {
                    assert_eq!(
                        o.get(&format!("next{i}")),
                        OBJECT_INITIALIZING as i64,
                        "{}",
                        o.text()
                    );
                }
                assert_eq!(
                    o.get(&format!("bytes{i}")),
                    144 + 32 * i as i64,
                    "{}",
                    o.text()
                );
                assert_eq!(
                    o.get(&format!("fresh{i}")),
                    40 + 16 * i as i64,
                    "{}",
                    o.text()
                );
                assert_eq!(o.get(&format!("lock{i}")), 0, "{}", o.text());
                assert_eq!(
                    o.get(&format!("pending{i}")),
                    200 + i as i64,
                    "{}",
                    o.text()
                );
                assert_eq!(
                    o.get(&format!("ready{i}")),
                    OBJECT_PUBLISHED as i64,
                    "{}",
                    o.text()
                );
                assert_eq!(o.get(&format!("clear{i}")), 0, "{}", o.text());
            }
        }
    }
}
fn legacy(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.g.mov_imm64(R::OwnerContext, 12345);
    h.alloc(1, 24, 0);
    h.g.mov_reg_reg(R::V0, R::OwnerContext);
    h.keep(20);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.g.mov_reg_reg(R::V0, R::OwnerContext);
    h.keep(21);
    h.value(Src::Root(0));
    h.g.mov_reg_reg(R::V0, R::OwnerContext);
    h.keep(22);
    root_journal_native_boundary(h.g);
    h.g.mov_reg_reg(R::V0, R::OwnerContext);
    h.keep(23);
    bind_primary_owner(h.g);
    for i in 20..24 {
        h.report(&format!("r{i}"), |h| {
            h.g.load_rbp_slot(R::V0, super::slot_offset(i))
        });
    }
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("allocated", |h| primary_allocated_diagnostic(h.g));
}
#[test]
fn production_legacy_entries_preserve_the_callers_owner_context_register() {
    for worker in [false, true] {
        let o = run(legacy, 0, worker);
        for i in 20..24 {
            assert_eq!(o.get(&format!("r{i}")), 12345, "{}", o.text());
        }
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("allocated"), 64, "{}", o.text());
    }
}

fn invalid_attach(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    if mode != 0 {
        h.mark_start();
    }
    let actor = create_actor(h.g, 16, 8);
    actor_reattach(h.g, actor);
}
#[test]
fn production_explicit_owner_attach_rejects_reused_storage_in_idle_and_mark() {
    for mode in [0, 1] {
        let o = run_failure(invalid_attach, mode);
        assert!(!o.status.success());
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant"));
    }
}

fn late_attach(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.mark_start();
    if mode & 1 != 0 {
        h.drain_marks();
        h.end_marks();
    }
    h.report_counter("phase", Counter::AdoptedPhase);
    let actors = (0..(mode / 2 + 2))
        .map(|_| create_actor(h.g, 16, 8))
        .collect::<Vec<_>>();
    let mut roots = Vec::new();
    for (i, actor) in actors.iter().copied().enumerate() {
        h.report(&format!("phase{i}"), |h| {
            actor_metric(h.g, actor, ActorMetric::Phase)
        });
        actor_alloc(h.g, actor, 24);
        h.g.mov_imm64(R::V1, 84 + i as u64);
        h.g.store_ptr_disp32(R::V0, 0, R::V1);
        roots.push(adopt_actor_value(h.g, actor));
        actor_publish(h.g, actor);
        actor_native_boundary(h.g, actor, true);
    }
    if mode & 1 == 0 {
        h.drain_marks();
        h.end_marks();
    }
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("regions", |h| actor_active_regions(h.g));
    for (i, (actor, root)) in actors.iter().copied().zip(roots).enumerate() {
        actor_native_boundary(h.g, actor, false);
        h.report(&format!("value{i}"), |h| {
            actor_load_root(h.g, actor, root);
            h.g.load_ptr_disp32(R::V0, R::V0, 0);
        });
    }
}
#[test]
fn production_fresh_owners_attach_during_real_old_mark_and_select_then_survive_movement() {
    for n in [2, 8] {
        for select in [0, 1] {
            for worker in [false, true] {
                let o = run(late_attach, (n - 2) * 2 + select, worker);
                assert_eq!(o.get("regions"), n as i64 + 1, "{}", o.text());
                assert_ne!(o.get("phase"), 0, "{}", o.text());
                for i in 0..n {
                    assert_eq!(o.get(&format!("phase{i}")), o.get("phase"), "{}", o.text());
                    assert_eq!(o.get(&format!("value{i}")), 84 + i as i64, "{}", o.text());
                }
            }
        }
    }
}

fn attach_race(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    let n = if mode & 2 == 0 { 2 } else { 8 };
    let moving = mode & 1 != 0;
    if moving {
        h.alloc(1, 24, 0);
        h.set_scalar(Src::Raw(0), 0, 123);
        h.root(0);
        h.collect();
        h.report_counter("moving", Counter::PhaseOpen);
    } else {
        h.report("begin", |h| participant_step(h.g, 6));
    }
    let threads = (0..n).map(|_| start_attach_thread(h.g)).collect::<Vec<_>>();
    for t in &threads {
        wait_attach_thread(h.g, t, false);
    }
    h.report("members_before", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
    h.report("regions_before", |h| actor_active_regions(h.g));
    for (i, t) in threads.iter().enumerate() {
        h.report(&format!("unregistered{i}"), |h| {
            actor_metric(h.g, t.actor, ActorMetric::Identity)
        });
    }
    if moving {
        h.finish_collection();
    } else {
        for (name, op) in [("ack", 1), ("end", 7), ("resume", 2)] {
            h.report(name, |h| participant_step(h.g, op));
        }
    }
    for t in &threads {
        join_attach_thread(h.g, t);
    }
    h.report("members_after", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
    h.report("regions_after", |h| actor_active_regions(h.g));
    h.report_counter("phase_after", Counter::AdoptedPhase);
    for (i, t) in threads.iter().enumerate() {
        h.report(&format!("phase{i}"), |h| {
            actor_metric(h.g, t.actor, ActorMetric::Phase)
        });
        h.report(&format!("waits{i}"), |h| {
            actor_metric(h.g, t.actor, ActorMetric::AttachWaits)
        });
        h.report(&format!("tid{i}"), |h| attach_thread_tid(h.g, t));
        actor_native_boundary(h.g, t.actor, false);
        h.report(&format!("value{i}"), |h| {
            actor_load_root(h.g, t.actor, t.root);
            h.g.load_ptr_disp32(R::V0, R::V0, 0);
        });
        actor_native_boundary(h.g, t.actor, true);
    }
    // The newly registered owners must also take part in subsequent real root
    // inventories and native exclusion, beyond merely returning from attach.
    h.mark_start();
    h.drain_marks();
    h.end_marks();
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report_counter("full", Counter::FullCollections);
    for (i, t) in threads.iter().enumerate() {
        actor_native_boundary(h.g, t.actor, false);
        h.report(&format!("after_full{i}"), |h| {
            actor_load_root(h.g, t.actor, t.root);
            h.g.load_ptr_disp32(R::V0, R::V0, 0);
        });
        actor_native_boundary(h.g, t.actor, true);
    }
}
#[test]
fn production_attach_waits_unregistered_for_real_handshake_and_relocation_without_holding_ticket() {
    for mode in 0..4 {
        let n = if mode & 2 == 0 { 2 } else { 8 };
        for worker in [false, true] {
            let o = run(attach_race, mode, worker);
            assert_eq!(o.get("members_before"), 1, "{}", o.text());
            assert_eq!(o.get("regions_before"), 1, "{}", o.text());
            assert_eq!(o.get("members_after"), n + 1, "{}", o.text());
            assert_eq!(o.get("regions_after"), n + 1, "{}", o.text());
            assert!(o.get("full") > 0, "{}", o.text());
            if mode & 1 == 0 {
                for name in ["begin", "ack", "end", "resume"] {
                    assert_eq!(o.get(name), 0, "{}", o.text());
                }
            } else {
                assert_eq!(o.get("moving"), 1, "{}", o.text());
            }
            let mut tids = std::collections::HashSet::new();
            for i in 0..n {
                assert_eq!(o.get(&format!("unregistered{i}")), 0, "{}", o.text());
                assert!(o.get(&format!("waits{i}")) > 0, "{}", o.text());
                assert_eq!(
                    o.get(&format!("phase{i}")),
                    o.get("phase_after"),
                    "{}",
                    o.text()
                );
                assert_eq!(o.get(&format!("value{i}")), 84, "{}", o.text());
                assert_eq!(o.get(&format!("after_full{i}")), 84, "{}", o.text());
                assert!(o.get(&format!("tid{i}")) > 0, "{}", o.text());
                assert!(tids.insert(o.get(&format!("tid{i}"))), "{}", o.text());
            }
        }
    }
}

fn parallel(g: &mut NativeCodeGenerator, n: u64) {
    let mut h = H::begin(g);
    let actors: Vec<_> = (0..n).map(|_| create_actor(h.g, 16, 8)).collect();
    h.report_counter("phase_before", Counter::AdoptedPhase);
    let p = run_parallel_actors(h.g, &actors, 8192);
    h.report_counter("phase_after", Counter::AdoptedPhase);
    h.report_counter("collections", Counter::Collections);
    h.report("verified", |h| verify_parallel_actors(h.g, &p));
    h.report("ready", |h| parallel_actor_ready(h.g, &p));
    h.report_counter("rate_samples", Counter::StartRateSamples);
    h.report("late_pages", |h| replay_older_page_bound(h.g));
    for (i, actor) in actors.into_iter().enumerate() {
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        h.report(&format!("phase{i}"), |h| {
            actor_metric(h.g, actor, ActorMetric::Phase)
        });
        h.report(&format!("tick{i}"), |h| {
            actor_metric(h.g, actor, ActorMetric::MaintenanceTick)
        });
        h.report(&format!("bytes{i}"), |h| {
            actor_metric(h.g, actor, ActorMetric::Bytes)
        });
        h.report(&format!("pending{i}"), |h| {
            actor_metric(h.g, actor, ActorMetric::Pending)
        });
    }
    for (i, name) in ["records", "ids", "published", "allocated", "pages", "base"]
        .iter()
        .enumerate()
    {
        h.report(name, |h| shared_allocation_value(h.g, i as u64));
    }
}
#[test]
fn production_explicit_owners_allocate_on_real_two_and_eight_linux_threads_while_idle() {
    for n in [2, 8] {
        for worker in [false, true] {
            let o = run(parallel, n, worker);
            let total = n as i64 * 8192;
            assert_eq!(o.get("collections"), 0);
            assert_eq!(o.get("phase_before"), o.get("phase_after"));
            assert_eq!(o.get("phase_after") & 255, 0);
            for key in ["verified", "records", "ids", "published"] {
                assert_eq!(o.get(key), total, "{}", o.text());
            }
            assert_eq!(o.get("allocated"), total * 48, "{}", o.text());
            assert_eq!(o.get("pages") - o.get("base"), 4 * n as i64, "{}", o.text());
            assert_eq!(o.get("late_pages"), o.get("pages"));
            assert_eq!(o.get("ready"), n as i64);
            if n == 8 && worker {
                assert!(o.get("rate_samples") > 0, "{}", o.text());
            }
            let mut tids = std::collections::HashSet::new();
            for i in 0..n {
                assert_eq!(o.get(&format!("phase{i}")), o.get("phase_after"));
                assert_eq!(o.get(&format!("tick{i}")), if worker { 0 } else { 8192 });
                assert!(o.get(&format!("tid{i}")) > 0);
                assert!(tids.insert(o.get(&format!("tid{i}"))));
                assert_eq!(o.get(&format!("bytes{i}")), 8192 * 48);
                assert_eq!(o.get(&format!("pending{i}")), 0);
            }
        }
    }
}

fn bank_aba(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.collect();
    h.finish_collection();
    // The kept object is age one. It will promote without using this fresh
    // arena; this unrooted dummy merely binds the owner's physical TLAB.
    h.alloc(1, 16, 1);
    prepare_primary_record(h.g);
    h.report("reserved_used", |h| primary_record_state(h.g, 0));
    h.report("reserved_revision", |h| primary_record_state(h.g, 1));
    h.report("bank_before", |h| actor_shared_records(h.g));
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.report("stale_used", |h| primary_record_state(h.g, 0));
    h.report("stale_revision", |h| primary_record_state(h.g, 1));
    h.report("revision", |h| primary_record_state(h.g, 2));
    h.report("bank_after", |h| actor_shared_records(h.g));
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 55);
    h.root(2);
    h.report("fresh_record", |h| {
        h.value(Src::Root(2));
        header_record(h.g);
    });
    h.report("adopted_revision", |h| primary_record_state(h.g, 1));
    h.report("fresh_used", |h| shared_allocation_value(h.g, 0));
    h.report("first", |h| h.load(Src::Root(0), 0));
    h.report("fresh", |h| h.load(Src::Root(2), 0));
    h.report("first_id", |h| h.object_id(Src::Root(0)));
    h.report("fresh_id", |h| h.object_id(Src::Root(2)));
}
#[test]
fn production_unused_record_reservation_expires_even_when_two_bank_flips_reuse_its_address() {
    for worker in [false, true] {
        let o = run(bank_aba, 0, worker);
        assert_eq!(o.get("reserved_used"), 0, "{}", o.text());
        assert_eq!(o.get("stale_used"), 0, "{}", o.text());
        assert_eq!(o.get("stale_revision"), o.get("reserved_revision"));
        assert_eq!(o.get("revision"), o.get("reserved_revision") + 2);
        assert_eq!(o.get("bank_after"), o.get("bank_before"));
        assert_eq!(o.get("adopted_revision"), o.get("revision"));
        assert_eq!(o.get("fresh_record"), o.get("bank_after"));
        assert_eq!(o.get("fresh_used"), 1);
        assert_eq!(o.get("first"), 84);
        assert_eq!(o.get("fresh"), 55);
        assert_eq!(o.get("first_id"), 1);
        assert_eq!(o.get("fresh_id"), 3);
    }
}

fn allocation_edge(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    allocation_counter_edge(h.g, mode);
    if mode == 1 {
        h.collect();
    } else {
        h.alloc(1, 16, 0);
        publish_primary(h.g);
        h.report("allocated", |h| shared_allocation_value(h.g, 3));
        h.report("published", |h| shared_allocation_value(h.g, 2));
        h.report("bytes", |h| primary_allocated_diagnostic(h.g));
    }
}
#[test]
fn production_shared_allocation_totals_saturate_and_identity_revision_exhaustion_fails_closed() {
    for mode in [0, 1] {
        let o = run_failure(allocation_edge, mode);
        assert!(!o.status.success());
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant"));
    }
    let o = run(allocation_edge, 2, false);
    // The harness's signed printer represents the saturated u64 as -1.
    assert_eq!(o.get("allocated"), -1);
    assert_eq!(o.get("published"), -1);
    assert_eq!(o.get("bytes"), 48);
}
