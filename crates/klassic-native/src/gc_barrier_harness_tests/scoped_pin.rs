use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
fn release(h: &mut H<'_>, token: crate::DataLabel) {
    let again = h.g.create_text_label();
    let done = h.g.create_text_label();
    h.g.bind_text_label(again);
    scoped_pin_call(h.g, token, 2);
    h.g.cmp_reg_imm8(R::V0, 2);
    h.g.jcc_label(C::NotEqual, done);
    let poll = h.g.gc_gen.as_ref().unwrap().labels.poll_actor;
    call_owner(h.g, poll);
    h.g.jmp_label(again);
    h.g.bind_text_label(done);
    lease_require_ok(h.g);
}
fn prepare(h: &mut H<'_>, token: crate::DataLabel, root: u32) {
    h.g.load_rbp_slot(R::V0, slot_offset(root));
    scoped_pin_call(h.g, token, 0);
    h.g.sub_reg_imm8(R::V0, 2);
    lease_require_ok(h.g);
}
fn lifecycle(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, if arg == 2 { 400_000 } else { 16 }, 0);
    h.set_scalar(Src::Raw(0), 0, 77);
    h.root(0);
    if arg == 1 {
        for _ in 0..2 {
            h.collect();
            h.finish_collection();
        }
    }
    h.report("id_before", |h| h.object_id(Src::Root(0)));
    let token = scoped_pin_token(h.g);
    prepare(&mut h, token, 0);
    if arg != 2 {
        scoped_pin_call(h.g, token, 1); // actual private reservation
        h.keep(9);
        h.report("pending_reserve", |h| h.value(Src::Raw(9)));
        h.report("unready_address", |h| scoped_pin_field(h.g, token, 1));
        scoped_pin_call(h.g, token, 1); // arm actual copier
        h.keep(9);
        h.report("pending_watch", |h| h.value(Src::Raw(9)));
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
    }
    scoped_pin_call(h.g, token, 1);
    lease_require_ok(h.g);
    h.report("address", |h| scoped_pin_field(h.g, token, 1));
    h.report("id_token", |h| scoped_pin_field(h.g, token, 2));
    let copied = scoped_pin_token(h.g);
    scoped_pin_copy(h.g, token, copied);
    scoped_pin_call(h.g, copied, 2);
    h.keep(9);
    h.report("copied", |h| h.value(Src::Raw(9)));
    let nested = scoped_pin_token(h.g);
    prepare(&mut h, nested, 0);
    scoped_pin_call(h.g, nested, 1);
    lease_require_ok(h.g);
    h.report("nested_address", |h| scoped_pin_field(h.g, nested, 1));
    release(&mut h, token);
    h.report("adopted_while_nested", |h| pin_adopt_metric(h.g));
    for _ in 0..2 {
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
    }
    h.report("nested_stable", |h| scoped_pin_field(h.g, nested, 1));
    h.report("root_stable", |h| h.value(Src::Root(0)));
    h.report("nested_value", |h| {
        scoped_pin_field(h.g, nested, 1);
        h.g.load_ptr_disp32(R::V0, R::V0, 0);
    });
    release(&mut h, nested);
    scoped_pin_call(h.g, copied, 2);
    h.keep(9);
    h.report("stale", |h| h.value(Src::Raw(9)));
    h.report("adopted_final", |h| pin_adopt_metric(h.g));
    h.report("roots", |h| native_root_active(h.g));
    h.report("leases", |h| lease_metric(h.g, 0));
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("after", |h| h.value(Src::Root(0)));
    h.report("id_after", |h| h.object_id(Src::Root(0)));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_scoped_pin_tokens_own_nested_pins_and_restore_small_object_mobility() {
    for arg in 0..3 {
        let o = run(lifecycle, arg, false);
        assert_eq!(o.get("copied"), 3, "arg={arg}: {}", o.text());
        assert_eq!(o.get("stale"), 3);
        assert_eq!(o.get("address"), o.get("nested_address"));
        assert_eq!(o.get("address"), o.get("nested_stable"));
        assert_eq!(o.get("address"), o.get("root_stable"));
        assert_eq!(o.get("adopted_while_nested"), 0);
        assert_eq!(o.get("adopted_final"), i64::from(arg != 2));
        assert_eq!(o.get("roots"), 0);
        assert_eq!(o.get("leases"), 0);
        assert_eq!(o.get("nested_value"), 77);
        assert_eq!(o.get("value"), 77);
        assert_eq!(o.get("id_before"), o.get("id_token"));
        assert_eq!(o.get("id_before"), o.get("id_after"));
        if arg != 2 {
            assert_eq!(o.get("pending_reserve"), 2);
            assert_eq!(o.get("pending_watch"), 2);
            assert_eq!(o.get("unready_address"), 0);
            assert_ne!(o.get("address"), o.get("after"));
        }
    }
}

fn callback_cleanup(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 91);
    h.root(0);
    let weak = lease_request(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    weak_call(h.g, weak, 0);
    lease_require_ok(h.g);
    let token = scoped_pin_token(h.g);
    prepare(&mut h, token, 0);
    for _ in 0..2 {
        scoped_pin_call(h.g, token, 1);
        h.g.sub_reg_imm8(R::V0, 2);
        lease_require_ok(h.g);
    }
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    scoped_pin_call(h.g, token, 1);
    lease_require_ok(h.g);
    h.report("before", |h| scoped_pin_field(h.g, token, 1));
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0); // Only the token owns the object through callbacks/GC.
    scoped_pin_call(h.g, token, 3); // actual root-only Native entry
    scoped_pin_call(h.g, token, 5); // actual callback Running adoption
    lease_require_ok(h.g);
    scoped_pin_call(h.g, token, 2);
    h.keep(9);
    h.report("busy_unpin", |h| h.value(Src::Raw(9)));
    h.report("callback_address", |h| scoped_pin_field(h.g, token, 1));
    h.report("outer_uses", |h| scoped_pin_field(h.g, token, 4));
    for _ in 0..2 {
        h.alloc(1, 32, 2);
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
    }
    h.report("value", |h| {
        scoped_pin_field(h.g, token, 1);
        h.g.load_ptr_disp32(R::V0, R::V0, 0);
    });
    scoped_pin_call(h.g, token, 3); // nested native use
    scoped_pin_call(h.g, token, 4); // nested return, outer use remains
    lease_require_ok(h.g);
    h.report("nested_return_uses", |h| scoped_pin_field(h.g, token, 4));
    scoped_pin_call(h.g, token, 6); // callback returns to outer Native scope
    scoped_pin_call(h.g, token, 4); // error/cleanup path resumes Running
    lease_require_ok(h.g);
    h.report("ended_uses", |h| scoped_pin_field(h.g, token, 4));
    native_root_unmap_fail_next(h.g);
    scoped_pin_call(h.g, token, 2);
    h.keep(9);
    h.report("unmap_failure", |h| h.value(Src::Raw(9)));
    h.report("retry_stage", |h| scoped_pin_field(h.g, token, 0));
    h.report("retry_address", |h| scoped_pin_field(h.g, token, 1));
    h.report("retry_owned", |h| native_root_active(h.g));
    release(&mut h, token);
    h.report("roots", |h| native_root_active(h.g));
    h.report("adopted", |h| pin_adopt_metric(h.g));
    for _ in 0..2 {
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
    }
    h.g.mov_imm64(R::V0, 0);
    h.keep(1);
    h.root(1);
    h.g.load_rbp_slot(R::V6, slot_offset(1));
    weak_call(h.g, weak, 1);
    h.keep(9);
    h.report("dead", |h| h.value(Src::Raw(9)));
    weak_call(h.g, weak, 2);
    lease_require_ok(h.g);
    h.report("leases", |h| lease_metric(h.g, 0));
}
#[test]
fn production_scoped_pin_native_callbacks_keep_outer_use_and_retry_error_cleanup() {
    let o = run(callback_cleanup, 0, false);
    assert_eq!(o.get("busy_unpin"), 2, "{}", o.text());
    assert_eq!(o.get("before"), o.get("callback_address"));
    assert_eq!(o.get("outer_uses"), 1);
    assert_eq!(o.get("nested_return_uses"), 1);
    assert_eq!(o.get("ended_uses"), 0);
    assert_eq!(o.get("value"), 91);
    assert_eq!(o.get("unmap_failure"), 6);
    assert_eq!(o.get("retry_stage"), 5);
    assert_eq!(o.get("retry_address"), 0);
    assert_eq!(o.get("retry_owned"), 1);
    assert_eq!(o.get("roots"), 0);
    assert_eq!(o.get("adopted"), 1);
    assert_eq!(o.get("dead"), 1);
    assert_eq!(o.get("leases"), 0);
}

fn native_threads(g: &mut crate::NativeCodeGenerator, arg: u64) {
    let n = arg & 255;
    let shared = arg & 256 != 0;
    let retained_reader = arg & 512 != 0;
    let mut h = H::begin(g);
    publish_primary(h.g);
    let primary_pin = if shared {
        h.alloc(1, 16, 0);
        h.root(0);
        let token = scoped_pin_token(h.g);
        prepare(&mut h, token, 0);
        for _ in 0..2 {
            scoped_pin_call(h.g, token, 1);
            h.g.sub_reg_imm8(R::V0, 2);
            lease_require_ok(h.g);
        }
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
        scoped_pin_call(h.g, token, 1);
        lease_require_ok(h.g);
        Some(token)
    } else {
        None
    };
    let mut actors = Vec::new();
    let mut targets = Vec::new();
    let mut tokens = Vec::new();
    for i in 0..n {
        let actor = create_actor(h.g, 16, 8);
        let root = if let Some(primary_pin) = primary_pin {
            scoped_pin_field(h.g, primary_pin, 1);
            adopt_actor_value(h.g, actor)
        } else {
            native_completed_graph(h.g, actor, 42 + i)
        };
        let token = scoped_pin_token(h.g);
        scoped_pin_actor_call(h.g, actor, token, Some(root), 0);
        h.g.sub_reg_imm8(R::V0, 2);
        lease_require_ok(h.g);
        for _ in 0..if shared { 1 } else { 2 } {
            scoped_pin_actor_call(h.g, actor, token, None, 1);
            if !shared {
                h.g.sub_reg_imm8(R::V0, 2);
            }
            lease_require_ok(h.g);
        }
        copy_actor_native_enter(h.g, actor);
        actors.push(actor);
        targets.push(root);
        tokens.push(token);
    }
    if let Some(primary_pin) = primary_pin {
        release(&mut h, primary_pin);
        h.report("adopted_with_peers", |h| pin_adopt_metric(h.g));
        h.g.mov_imm64(R::V0, 0);
        h.set_root(0);
    }
    pin_copy_enable_moving(h.g);
    if !shared {
        pin_copy_start_full(h.g);
        finish_move(h.g);
    }
    let mut dummy = Vec::new();
    for i in 0..n as usize {
        native_completed_resume(h.g, actors[i]);
        scoped_pin_actor_call(h.g, actors[i], tokens[i], None, 1);
        lease_require_ok(h.g);
        h.report(&format!("before{i}"), |h| {
            scoped_pin_field(h.g, tokens[i], 1)
        });
        scoped_pin_clear_actor_root(h.g, actors[i], targets[i]);
        dummy.push(native_completed_graph(h.g, actors[i], 200 + i as u64));
        scoped_pin_actor_call(h.g, actors[i], tokens[i], None, 3);
        lease_require_ok(h.g);
    }
    h.report("whole_heap_borrows", |h| root_native_borrows(h.g));
    scoped_pin_call(h.g, tokens[0], 2);
    h.keep(9);
    h.report("foreign", |h| h.value(Src::Raw(9)));
    let p = run_parallel_native_completed(h.g, &actors, &dummy, 2, true, true);
    for i in 0..n as usize {
        h.report(&format!("tid{i}"), |h| parallel_actor_tid(h.g, &p, i));
        h.report(&format!("after{i}"), |h| {
            scoped_pin_field(h.g, tokens[i], 1)
        });
        scoped_pin_actor_call(h.g, actors[i], tokens[i], None, 4);
        lease_require_ok(h.g);
        if retained_reader {
            let reader = scoped_pin_reader(h.g, tokens[i], None);
            scoped_pin_actor_call(h.g, actors[i], tokens[i], None, 2);
            h.keep(9);
            h.report(&format!("release_pending{i}"), |h| h.value(Src::Raw(9)));
            h.value(Src::Raw(9));
            h.g.sub_reg_imm8(R::V0, 2);
            lease_require_ok(h.g);
            scoped_pin_reader(h.g, tokens[i], Some(reader));
        }
        // A worker can retain the native root's actual reader even after the
        // OS thread has joined. Preserve the token until Release completes.
        scoped_pin_actor_release_wait(h.g, actors[i], tokens[i]);
        h.report(&format!("adopted{i}"), |h| pin_adopt_metric(h.g));
        copy_actor_native_enter(h.g, actors[i]);
    }
    h.report("roots", |h| native_root_active(h.g));
    h.report("leases", |h| lease_metric(h.g, 0));
    h.report("adopted", |h| pin_adopt_metric(h.g));
}
#[test]
fn production_scoped_pin_shared_actor_release_retries_after_actual_native_root_reader() {
    for n in [2, 8] {
        let o = run(native_threads, n | 256 | 512, true);
        assert_eq!(o.get("roots"), 0, "{}", o.text());
        assert_eq!(o.get("leases"), 0, "{}", o.text());
        assert_eq!(o.get("adopted"), 1, "{}", o.text());
        for i in 0..n {
            assert_eq!(o.get(&format!("release_pending{i}")), 2, "{}", o.text());
            assert_eq!(o.get(&format!("before{i}")), o.get(&format!("after{i}")));
            assert_eq!(o.get(&format!("adopted{i}")), i64::from(i + 1 == n));
        }
    }
}
#[test]
fn production_scoped_pin_families_share_one_object_across_native_owners_until_last_unpin() {
    for n in [2, 8] {
        let o = run(native_threads, n | 256, true);
        assert_eq!(o.get("adopted_with_peers"), 0, "n={n}: {}", o.text());
        assert_eq!(o.get("whole_heap_borrows"), 0);
        assert_eq!(o.get("roots"), 0);
        assert_eq!(o.get("leases"), 0);
        assert_eq!(o.get("adopted"), 1);
        for i in 0..n {
            assert_eq!(o.get(&format!("before{i}")), o.get("before0"));
            assert_eq!(o.get(&format!("before{i}")), o.get(&format!("after{i}")));
            assert_eq!(o.get(&format!("adopted{i}")), i64::from(i + 1 == n));
        }
    }
}
#[test]
fn production_scoped_pins_keep_native_thread_addresses_across_real_full_collections() {
    for n in [2, 8] {
        let o = run(native_threads, n, true);
        assert_eq!(o.get("whole_heap_borrows"), 0, "n={n}: {}", o.text());
        assert_eq!(o.get("foreign"), 3);
        assert_eq!(o.get("roots"), 0);
        assert_eq!(o.get("leases"), 0);
        assert_eq!(o.get("adopted"), n as i64);
        let mut tids = std::collections::HashSet::new();
        for i in 0..n {
            assert!(tids.insert(o.get(&format!("tid{i}"))));
            assert_eq!(o.get(&format!("before{i}")), o.get(&format!("after{i}")));
        }
    }
}

fn cancel_pending(g: &mut crate::NativeCodeGenerator, stage: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    let token = scoped_pin_token(h.g);
    prepare(&mut h, token, 0);
    for _ in 0..stage.min(2) {
        scoped_pin_call(h.g, token, 1);
        h.g.sub_reg_imm8(R::V0, 2);
        lease_require_ok(h.g);
    }
    if stage == 3 {
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
    }
    h.report("unready_address", |h| scoped_pin_field(h.g, token, 1));
    release(&mut h, token);
    h.report("roots", |h| native_root_active(h.g));
    h.report("reservations", |h| pin_destination_metric(h.g, 0));
    h.report("adopted", |h| pin_adopt_metric(h.g));
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0);
    for _ in 0..2 {
        request_full_collection(h.g);
        h.collect();
        h.finish_collection();
    }
    h.report("large", |h| pin_destination_metric(h.g, 3));
    h.report("leases", |h| lease_metric(h.g, 0));
    // The exact same native holder is reusable only with its new serial.
    h.alloc(1, 400_000, 2);
    h.root(2);
    prepare(&mut h, token, 2);
    scoped_pin_call(h.g, token, 1);
    lease_require_ok(h.g);
    let stale = scoped_pin_token(h.g);
    scoped_pin_copy(h.g, token, stale);
    release(&mut h, token);
    prepare(&mut h, token, 2);
    scoped_pin_call(h.g, token, 1);
    lease_require_ok(h.g);
    let current = scoped_pin_token(h.g);
    scoped_pin_copy(h.g, token, current);
    scoped_pin_copy(h.g, stale, token);
    scoped_pin_call(h.g, token, 2);
    h.keep(9);
    h.report("stale_serial", |h| h.value(Src::Raw(9)));
    scoped_pin_copy(h.g, current, token);
    release(&mut h, token);
    h.report("reused_roots", |h| native_root_active(h.g));
}
#[test]
fn production_scoped_pin_pending_cancellation_closes_every_owned_resource_and_reuses_holder() {
    for stage in 0..4 {
        let o = run(cancel_pending, stage, false);
        assert_eq!(o.get("unready_address"), 0, "stage={stage}: {}", o.text());
        assert_eq!(o.get("roots"), 0);
        assert_eq!(o.get("reservations"), 0);
        assert_eq!(o.get("adopted"), i64::from(stage == 3));
        assert_eq!(o.get("large"), 0);
        assert_eq!(o.get("leases"), 0);
        assert_eq!(o.get("stale_serial"), 3);
        assert_eq!(o.get("reused_roots"), 0);
    }
}

fn automatic_progress(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 123);
    h.root(0);
    let token = scoped_pin_token(h.g);
    prepare(&mut h, token, 0);
    h.g.mov_imm64(R::V0, 0);
    h.set_root(0);
    pin_copy_enable_moving(h.g);
    scoped_pin_wait(h.g, token, false); // Steps drive the real Full coordinator.
    h.report("address", |h| scoped_pin_field(h.g, token, 1));
    h.report("value", |h| {
        scoped_pin_field(h.g, token, 1);
        h.g.load_ptr_disp32(R::V0, R::V0, 0);
    });
    pin_copy_start_full(h.g); // Real selection while this token is Ready.
    scoped_pin_call(h.g, token, 2); // End drives real moving/retire completion.
    h.keep(9);
    h.report("ending", |h| h.value(Src::Raw(9)));
    scoped_pin_wait(h.g, token, true);
    h.report("roots", |h| native_root_active(h.g));
    h.report("leases", |h| lease_metric(h.g, 0));
    h.report("adopted", |h| pin_adopt_metric(h.g));
    h.report("cleared_address", |h| scoped_pin_field(h.g, token, 1));
}
#[test]
fn production_scoped_pin_steps_drive_actual_enrolled_moving_collection_and_pending_unpin() {
    let o = run(automatic_progress, 0, true);
    assert_ne!(o.get("address"), 0, "{}", o.text());
    assert_eq!(o.get("value"), 123);
    assert_eq!(o.get("ending"), 2);
    assert_eq!(o.get("roots"), 0);
    assert_eq!(o.get("leases"), 0);
    assert_eq!(o.get("adopted"), 1);
    assert_eq!(o.get("cleared_address"), 0);
}

fn unowned_retry(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 57);
    h.root(0);
    request_full_collection(h.g);
    h.collect(); // Real open move: independent-root registration cannot enter.
    let token = scoped_pin_token(h.g);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    scoped_pin_call(h.g, token, 0);
    h.keep(9);
    h.report("retry", |h| h.value(Src::Raw(9)));
    h.report("stage", |h| scoped_pin_field(h.g, token, 0));
    h.report("address", |h| scoped_pin_field(h.g, token, 1));
    h.report("unowned", |h| native_root_active(h.g));
    h.finish_collection();
    h.report("source", |h| h.load(Src::Root(0), 0));
    prepare(&mut h, token, 0); // Retry New using the retained real frame root.
    h.report("owned_stage", |h| scoped_pin_field(h.g, token, 0));
    h.report("owned", |h| native_root_active(h.g));
    release(&mut h, token);
    h.report("released", |h| native_root_active(h.g));
}
#[test]
fn production_scoped_pin_new_distinguishes_unowned_retry_from_owned_pending() {
    let o = run(unowned_retry, 0, false);
    assert_eq!(o.get("retry"), 4, "{}", o.text());
    assert_eq!(o.get("stage"), 0);
    assert_eq!(o.get("address"), 0);
    assert_eq!(o.get("unowned"), 0);
    assert_eq!(o.get("source"), 57);
    assert_eq!(o.get("owned_stage"), 2);
    assert_eq!(o.get("owned"), 1);
    assert_eq!(o.get("released"), 0);
}
