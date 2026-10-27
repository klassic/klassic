//! Real source forwarding into a precommitted reservation, with independent
//! registered native reachability and repeated actual Full collections.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn copy(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    if arg & 1 != 0 {
        for _ in 0..2 {
            h.collect();
            h.finish_collection();
        }
    }
    h.report("before_old", |h| h.is_old(Src::Root(0)));
    h.report("before_id", |h| h.object_id(Src::Root(0)));
    let owned = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    native_root_call(h.g, owned, 0);
    lease_require_ok(h.g);
    let destination = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    pin_destination_call(h.g, destination, false);
    lease_require_ok(h.g);
    if arg & 512 != 0 {
        request_full_collection(h.g);
        h.collect();
        h.report_counter("selected_first", Counter::PhaseOpen);
    }
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    pin_destination_watch(h.g, destination);
    lease_require_ok(h.g);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0);
    if arg & 512 != 0 {
        h.finish_collection();
        h.report("early_pin_wins", |h| pin_copy_metric(h.g, 0));
        h.report("early_pin_worker_wins", |h| pin_copy_metric(h.g, 1));
    }
    request_full_collection(h.g);
    h.collect();
    if arg & 256 != 0 {
        pin_copy_wait_worker(h.g);
    }
    h.finish_collection();
    // Publish through the actual independent-root loader, after source root
    // removal and the winning source forwarding word's real publication.
    h.g.mov_imm64(R::V0, 0);
    h.keep(1);
    h.root(1);
    h.g.load_rbp_slot(R::V6, slot_offset(1));
    native_root_call(h.g, owned, 1);
    lease_require_ok(h.g);
    h.report("owner_wins", |h| pin_copy_metric(h.g, 0));
    h.report("worker_wins", |h| pin_copy_metric(h.g, 1));
    h.report("after_id", |h| h.object_id(Src::Root(1)));
    h.report("large", |h| {
        h.value(Src::Root(1));
        pin_destination_is_large(h.g);
    });
    h.report("value", |h| h.load(Src::Root(1), 0));
    h.report("top", |h| pin_destination_geometry(h.g, destination, 1));
    h.report("start", |h| pin_destination_geometry(h.g, destination, 2));
    // A winning destination is managed: consuming the reservation must retain
    // its actual epoch, rather than enqueueing the live object as dead storage.
    pin_destination_call(h.g, destination, true);
    lease_require_ok(h.g);
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("retained", |h| h.load(Src::Root(1), 0));
    native_root_call(h.g, owned, 2);
    lease_require_ok(h.g);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(1);
    for _ in 0..2 {
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
    }
    h.report("no_large", |h| pin_destination_metric(h.g, 3));
    h.report("no_reservations", |h| pin_destination_metric(h.g, 0));
}
#[test]
fn production_pin_copy_uses_actual_young_and_old_forwarding_winners_and_independent_native_roots() {
    for (arg, worker) in [(0, false), (1, false), (0, true), (1, true)] {
        let o = run(copy, arg | if worker { 256 } else { 0 }, worker);
        assert_eq!(o.get("before_id"), o.get("after_id"), "{}", o.text());
        assert_eq!(o.get("owner_wins") + o.get("worker_wins"), 1);
        assert_eq!(o.get(if worker { "worker_wins" } else { "owner_wins" }), 1);
        assert_eq!(o.get("large"), 1);
        assert_eq!(o.get("value"), 77);
        assert_eq!(o.get("top") - o.get("start"), 48);
        assert_eq!(o.get("retained"), 77);
        assert_eq!(o.get("no_large"), 0);
        assert_eq!(o.get("no_reservations"), 0);
    }
}
fn graph(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    h.alloc(2, 16, 1);
    h.root(1);
    h.store(Src::Root(1), 0, Src::Root(0), true);
    h.store(Src::Root(1), 1, Src::Root(1), true);
    let owned = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(1));
    native_root_call(h.g, owned, 0);
    lease_require_ok(h.g);
    let destination = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(1));
    pin_destination_call(h.g, destination, false);
    lease_require_ok(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(1));
    pin_destination_watch(h.g, destination);
    lease_require_ok(h.g);
    for i in [0, 1] {
        h.g.mov_imm64(R::V0, 0);
        h.set_root(i);
    }
    request_full_collection(h.g);
    h.collect();
    if arg & 256 != 0 {
        pin_copy_wait_worker(h.g);
    }
    h.finish_collection();
    h.g.load_rbp_slot(R::V6, slot_offset(1));
    native_root_call(h.g, owned, 1);
    lease_require_ok(h.g);
    h.load_barriered(Src::Root(1), 0);
    h.keep(3);
    h.report("child", |h| h.load(Src::Raw(3), 0));
    h.load_barriered(Src::Root(1), 1);
    h.keep(4);
    h.value(Src::Root(1));
    h.g.load_rbp_slot(R::V1, slot_offset(4));
    h.g.sub_reg_reg(R::V0, R::V1);
    h.keep(5);
    h.report("self_delta", |h| h.value(Src::Raw(5)));
    pin_destination_call(h.g, destination, true);
    lease_require_ok(h.g);
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.load_barriered(Src::Root(1), 0);
    h.keep(3);
    h.report("child_again", |h| h.load(Src::Raw(3), 0));
    h.report("owner_wins", |h| pin_copy_metric(h.g, 0));
    h.report("worker_wins", |h| pin_copy_metric(h.g, 1));
    native_root_call(h.g, owned, 2);
    lease_require_ok(h.g);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(1);
    for _ in 0..2 {
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
    }
    h.report("dead", |h| pin_destination_metric(h.g, 3));
}
#[test]
fn production_pin_copy_enters_actual_large_reference_and_self_cycle_tracing() {
    for worker in [false, true] {
        let o = run(graph, if worker { 256 } else { 0 }, worker);
        assert_eq!(o.get("child"), 77, "{}", o.text());
        assert_eq!(o.get("self_delta"), 0);
        assert_eq!(o.get("child_again"), 77);
        assert_eq!(o.get("owner_wins") + o.get("worker_wins"), 1);
        assert_eq!(o.get(if worker { "worker_wins" } else { "owner_wins" }), 1);
        assert_eq!(o.get("dead"), 0);
    }
}

#[test]
fn production_pin_copy_follows_a_source_selected_before_the_watch_into_its_next_real_full_destination()
 {
    for worker in [false, true] {
        let o = run(copy, 512 | if worker { 256 } else { 0 }, worker);
        assert_eq!(o.get("selected_first"), 1, "{}", o.text());
        assert_eq!(o.get("early_pin_wins") + o.get("early_pin_worker_wins"), 0);
        assert_eq!(o.get("before_id"), o.get("after_id"));
        assert_eq!(o.get("large"), 1);
        assert_eq!(o.get(if worker { "worker_wins" } else { "owner_wins" }), 1);
        assert_eq!(o.get("value"), 77);
        assert_eq!(o.get("retained"), 77);
        assert_eq!(o.get("no_large"), 0);
    }
}
fn competing(g: &mut crate::NativeCodeGenerator, n: u64) {
    use crate::portable_asm::Condition as C;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    publish_primary(h.g);
    h.report("before_id", |h| h.object_id(Src::Root(0)));
    let destination = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    pin_destination_call(h.g, destination, false);
    lease_require_ok(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    pin_destination_watch(h.g, destination);
    lease_require_ok(h.g);
    let actors: Vec<_> = (0..n).map(|_| create_actor(h.g, 16, 8)).collect();
    for actor in &actors {
        copy_actor_native_enter(h.g, *actor);
    }
    h.g.asm
        .data
        .resize(h.g.asm.data.len().next_multiple_of(8), 0);
    let root = h.g.asm.data_label_with_i64s(&[0]);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    h.g.mov_data_addr(R::V1, root);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    pin_copy_enable_moving(h.g);
    let state = prepare_full_copy_overlap(h.g, &actors);
    if n == 8 {
        pin_copy_force_private_loss(h.g);
    }
    copy_probe_root(h.g, state, root);
    pin_copy_start_full(h.g);
    full_copy_wait_parked(h.g, state);
    let p = run_parallel_copy_actors(h.g, &actors, root, state, 0);
    h.report("arrived", |h| copy_overlap_metric(h.g, state, 2));
    h.report("peak", |h| copy_overlap_metric(h.g, state, 4));
    h.report("aggregate", |h| copy_aggregate(h.g));
    for i in 0..n as usize {
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        h.report(&format!("address{i}"), |h| {
            parallel_copy_result(h.g, &p, i, 0)
        });
        h.report(&format!("value{i}"), |h| {
            parallel_copy_result(h.g, &p, i, 3)
        });
    }
    finish_move(h.g);
    h.report("first_losses", |h| pin_copy_metric(h.g, 2));
    // If a normal candidate won before the private candidate, rearm from the
    // actual canonical root after closure; the same private resource is reused.
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    pin_destination_watch(h.g, destination);
    let ready = h.g.create_text_label();
    h.g.cmp_reg_imm8(R::V0, 1);
    h.g.jcc_label(C::Equal, ready);
    lease_require_ok(h.g);
    h.g.bind_text_label(ready);
    pin_copy_start_full(h.g);
    finish_move(h.g);
    h.report("large", |h| {
        h.value(Src::Root(0));
        pin_destination_is_large(h.g);
    });
    h.report("after_id", |h| h.object_id(Src::Root(0)));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("pin_wins", |h| pin_copy_metric(h.g, 0));
    h.report("pin_worker_wins", |h| pin_copy_metric(h.g, 1));
    pin_destination_call(h.g, destination, true);
    lease_require_ok(h.g);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0);
    for _ in 0..2 {
        pin_copy_start_full(h.g);
        finish_move(h.g);
    }
    h.report("dead", |h| pin_destination_metric(h.g, 3));
}
#[test]
fn production_pin_copy_competes_with_real_two_and_eight_mutator_forwarders_on_one_destination() {
    for n in [2, 8] {
        let o = run(competing, n, true);
        assert_eq!(o.get("arrived"), n as i64, "{}", o.text());
        assert_eq!(o.get("peak"), n as i64);
        assert_eq!(o.get("aggregate"), 0);
        assert_eq!(o.get("first_losses"), if n == 8 { 1 } else { 0 });
        assert_eq!(o.get("before_id"), o.get("after_id"));
        assert_eq!(o.get("large"), 1);
        assert_eq!(o.get("value"), 42);
        assert_eq!(o.get("pin_wins") + o.get("pin_worker_wins"), 1);
        assert_eq!(o.get("dead"), 0);
        let mut tids = std::collections::HashSet::new();
        for i in 0..n {
            assert!(tids.insert(o.get(&format!("tid{i}"))));
            assert_eq!(o.get(&format!("address{i}")), o.get("address0"));
            assert_eq!(o.get(&format!("value{i}")), 42);
        }
    }
}
