//! Actual old-slot-only reachability through owner buffers and moving quorums.
use super::{H, Src, run, slot_offset};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn buffered(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 256, 0);
    h.root(0);
    h.alloc(2, 32, 1);
    h.root(1);
    remembered_buffer_enable_moving(h.g);
    remembered_buffer_cycle(h.g, false);
    remembered_buffer_cycle(h.g, false);
    h.value(Src::Root(0));
    h.keep(2);
    h.root(2); // independent actual owner root
    h.alloc(1, 16, 3);
    h.set_scalar(Src::Raw(3), 0, 8307);
    let requests = if mode == 3 {
        let r = h.g.asm.data_label_with_i64s(&vec![0; 4096 * 3]);
        lease_growth_group(h.g, r, slot_offset(0), 0, 4096, false);
        Some(r)
    } else {
        None
    };
    if mode == 6 {
        // The alias is the top frame root so root_pop really releases it.
        h.value(Src::Root(0));
        h.keep(4);
        h.root(4);
        h.g.load_rbp_slot(R::V0, slot_offset(4));
        h.keep(5);
    }
    h.store(
        Src::Root(if mode == 6 { 4 } else { 0 }),
        0,
        Src::Raw(3),
        true,
    );
    if mode == 5 {
        for i in 1..17 {
            h.store(Src::Root(0), i, Src::Raw(3), true);
        }
    }
    h.report("buffered", |h| remembered_buffer_metric(h.g, 0));
    h.report("pending", |h| remembered_buffer_metric(h.g, 1));
    h.report_counter("global_before", Counter::RemsetLen);
    h.report("leases_before", |h| lease_metric(h.g, 0));
    if mode == 4 {
        remembered_buffer_unsealed_flip(h.g);
        return;
    }
    // A captured owner must not follow a mutable or released/reused cell.
    if mode == 6 {
        h.pop_root();
        h.value(Src::Root(1));
        h.keep(4);
        h.root(4);
        h.report("released_cell", |h| h.value(Src::Raw(5)));
        h.report("reused_cell", |h| {
            h.g.load_rbp_slot(R::V0, slot_offset(4));
        });
        h.report("pending_after_reuse", |h| remembered_buffer_metric(h.g, 1));
    } else {
        h.value(Src::Root(1));
        h.set_root(0);
    }
    if let Some(r) = requests {
        lease_growth_group(h.g, r, slot_offset(0), 0, 4096, true);
    }
    if mode == 0 || mode == 5 || mode == 6 {
        h.g.call_label(h.l.native_enter);
        h.g.call_label(h.l.native_exit);
        if mode == 0 || mode == 6 {
            h.report("entry_original", |h| h.entry_is_slot(0, Src::Root(2), 0));
        }
        h.report_counter("global_after", Counter::RemsetLen);
    } else {
        remembered_buffer_cycle(h.g, mode == 2);
    }
    h.report("after_pending", |h| remembered_buffer_metric(h.g, 1));
    h.report("after_buffered", |h| remembered_buffer_metric(h.g, 0));
    h.report("admitted", |h| remembered_buffer_metric(h.g, 2));
    h.report("flushed", |h| remembered_buffer_metric(h.g, 3));
    h.report("synchronous", |h| remembered_buffer_metric(h.g, 4));
    h.report("leases_after", |h| lease_metric(h.g, 0));
    h.report("value", |h| h.deref_field(Src::Root(2), 0, 0));
}
#[test]
fn production_enrolled_remembered_buffer_seals_before_native_young_flip_and_full_discard() {
    for mode in [0, 1, 2, 3, 5, 6] {
        let o = run(buffered, mode, true);
        assert_eq!(o.get("value"), 8307, "{}", o.text());
        assert_eq!(o.get("after_pending"), 0, "{}", o.text());
        assert_eq!(o.get("after_buffered"), 0);
        assert_eq!(o.get("leases_after"), 0);
        if mode == 3 {
            assert_eq!(o.get("synchronous"), 1, "{}", o.text());
            assert_eq!(o.get("buffered"), 0);
            assert_eq!(o.get("global_before"), 1);
        } else {
            assert_eq!(o.get("buffered"), 1, "{}", o.text());
            assert_eq!(o.get("pending"), 1);
            assert_eq!(o.get("leases_before"), 1);
            assert_eq!(o.get("global_before"), if mode == 5 { 16 } else { 0 });
            assert_eq!(o.get("admitted"), if mode == 5 { 17 } else { 1 });
            assert_eq!(o.get("flushed"), o.get("admitted"));
        }
        if mode == 0 || mode == 6 {
            assert_eq!(o.get("entry_original"), 1);
        }
        if mode == 6 {
            assert_ne!(o.get("released_cell"), 0);
            assert_eq!(o.get("released_cell"), o.get("reused_cell"));
            assert_eq!(o.get("pending_after_reuse"), 1);
        }
        if mode == 5 {
            assert_eq!(o.get("global_after"), 17);
        }
    }
}
#[test]
fn production_remembered_flip_rejects_a_real_unsealed_owner_entry() {
    let o = super::execute(buffered, 4, true, false);
    assert_eq!(o.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&o.stdout).contains("buffered=1\n"));
    assert!(String::from_utf8_lossy(&o.stdout).contains("pending=1\n"));
    assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
}
fn parallel(g: &mut crate::NativeCodeGenerator, owners: u64) {
    let mut h = H::begin(g);
    h.alloc(2, owners * 8, 0);
    h.root(0);
    publish_primary(h.g);
    // The shared hub must already be Old while every slot is still null.
    // Letting the graph promote it later can install collector registrations
    // before a mutator store, so a valid run may exercise no private buffer.
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.report("hub_old_before_actors", |h| h.is_old(Src::Root(0)));
    h.report_counter("registered_before_actors", Counter::RemsetLen);
    let actors: Vec<_> = (0..owners).map(|_| create_actor(h.g, 16, 8)).collect();
    let root = h.g.asm.data_label_with_i64s(&[0]);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    h.g.mov_data_addr(R::V1, root);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    let p = run_parallel_buffered_remembered_graph(h.g, &actors, root);
    for i in 0..owners {
        h.report(&format!("value{i}"), |h| {
            h.load_barriered(Src::Root(0), i as u32);
            h.keep(4);
            h.deref_field(Src::Raw(4), 0, 0);
        });
    }
    h.report("admitted", |h| remembered_buffer_metric(h.g, 2));
    h.report("flushed", |h| remembered_buffer_metric(h.g, 3));
    h.report("pending", |h| remembered_buffer_metric(h.g, 1));
    h.report("starts", |h| moving_metric(h.g, 0));
    h.report("hub_old_after", |h| h.is_old(Src::Root(0)));
    h.report_counter("registered_after", Counter::RemsetLen);
    h.report("synchronous", |h| remembered_buffer_metric(h.g, 4));
    for i in 0..owners {
        h.report(&format!("reads{i}"), |h| {
            parallel_graph_reads(h.g, &p, i as usize)
        });
    }
}
#[test]
fn production_one_two_and_eight_real_owners_flush_slot_buffers_before_moving_quorum() {
    for n in [1, 2, 8] {
        let (output, image) = super::execute_with_image_retention(
            parallel,
            n,
            true,
            false,
            None,
            None,
            super::ImageRetention::Always,
        );
        let _image = KeepOnFailure(image.expect("retained parallel fixture"));
        let text = String::from_utf8_lossy(&output.stdout).into_owned();
        assert!(
            output.status.success(),
            "{text}\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        let values = text
            .lines()
            .filter_map(|line| line.split_once('='))
            .filter_map(|(name, value)| Some((name.to_string(), value.trim().parse().ok()?)))
            .collect();
        let o = super::Observed { values, text };
        assert_eq!(o.get("hub_old_before_actors"), 1, "{}", o.text());
        assert_eq!(o.get("registered_before_actors"), 0, "{}", o.text());
        assert!(o.get("admitted") >= n as i64, "{}", o.text());
        assert_eq!(o.get("admitted"), o.get("flushed"));
        assert_eq!(o.get("pending"), 0);
        assert_eq!(o.get("starts"), 3);
        for i in 0..n {
            assert_eq!(o.get(&format!("value{i}")), ((i + 2) << 32 | 11) as i64);
            assert!(o.get(&format!("reads{i}")) > 0);
        }
    }
}

fn detach_buffered(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    remembered_buffer_enable_moving(h.g);
    remembered_buffer_cycle(h.g, false);
    remembered_buffer_cycle(h.g, false);
    let actor = create_actor(h.g, 16, 8);
    h.g.load_rbp_slot(R::V0, slot_offset(0));
    remembered_buffer_seed_detaching_actor(h.g, actor);
    h.report("pending_before", |h| remembered_buffer_metric(h.g, 1));
    h.report("lease_before", |h| lease_metric(h.g, 0));
    h.report("begin", |h| actor_detach_call(h.g, actor, true));
    h.report("pending_after_begin", |h| remembered_buffer_metric(h.g, 1));
    h.report("lease_after_begin", |h| lease_metric(h.g, 0));
    actor_detach_wait(h.g, actor);
    actor_detach_dispose_wait(h.g, actor);
    remembered_buffer_cycle(h.g, false);
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
    h.report("members", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
}
#[test]
fn production_detach_flushes_real_private_remembered_ownership_before_retiring_context() {
    let o = run(detach_buffered, 0, true);
    assert_eq!(o.get("pending_before"), 1, "{}", o.text());
    assert_eq!(o.get("lease_before"), 1);
    assert_eq!(o.get("begin"), 0);
    assert_eq!(o.get("pending_after_begin"), 0);
    assert_eq!(o.get("lease_after_begin"), 0);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("members"), 1);
}

// Each child is deliberately unrooted: the Old holder's field is its sole
// surviving edge. Fresh stores after each resume must enter the new epoch.
fn generation_transitions(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 32, 0);
    h.root(0);
    remembered_buffer_enable_moving(h.g);
    remembered_buffer_cycle(h.g, false);
    remembered_buffer_cycle(h.g, false);
    h.report_counter("full_before", Counter::FullCollections);
    for (step, full) in [false, true, false].into_iter().enumerate() {
        h.alloc(1, 16, 1);
        h.set_scalar(Src::Raw(1), 0, 9000 + step as u64);
        h.store(Src::Root(0), step as u32, Src::Raw(1), true);
        h.report(&format!("pending_before{step}"), |h| {
            remembered_buffer_metric(h.g, 1)
        });
        h.report(&format!("lease_before{step}"), |h| lease_metric(h.g, 0));
        if full {
            h.report("owner_before_full", |h| h.value(Src::Root(0)));
        }
        remembered_buffer_cycle(h.g, full);
        if full {
            h.report("owner_after_full", |h| h.value(Src::Root(0)));
        }
        h.report(&format!("value{step}"), |h| {
            h.deref_field(Src::Root(0), step as u32, 0)
        });
        h.report(&format!("pending_after{step}"), |h| {
            remembered_buffer_metric(h.g, 1)
        });
        h.report(&format!("lease_after{step}"), |h| lease_metric(h.g, 0));
    }
    h.report_counter("full_after", Counter::FullCollections);
    h.report("admitted", |h| remembered_buffer_metric(h.g, 2));
    h.report("flushed", |h| remembered_buffer_metric(h.g, 3));
}
#[test]
fn production_remembered_sealing_survives_young_full_young_and_owner_movement() {
    let o = run(generation_transitions, 0, true);
    assert_eq!(
        o.get("full_after"),
        o.get("full_before") + 1,
        "{}",
        o.text()
    );
    assert_ne!(
        o.get("owner_before_full"),
        o.get("owner_after_full"),
        "{}",
        o.text()
    );
    for step in 0..3 {
        assert_eq!(o.get(&format!("pending_before{step}")), 1, "{}", o.text());
        assert_eq!(o.get(&format!("lease_before{step}")), 1);
        assert_eq!(o.get(&format!("value{step}")), 9000 + step);
        assert_eq!(o.get(&format!("pending_after{step}")), 0);
        assert_eq!(o.get(&format!("lease_after{step}")), 0);
    }
    assert_eq!(o.get("admitted"), 3);
    assert_eq!(o.get("flushed"), 3);
}

// A program may exit successfully while a Rust-level invariant fails. Keep
// this fixture's exact image until its value/counter assertions also succeed.
struct KeepOnFailure(std::path::PathBuf);
impl Drop for KeepOnFailure {
    fn drop(&mut self) {
        if std::thread::panicking() {
            eprintln!("parallel remembered fixture retained: {}", self.0.display());
        } else {
            let _ = std::fs::remove_file(&self.0);
        }
    }
}
