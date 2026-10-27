//! Native mapping probes across production Retire publication and OS release.
use super::{H, Src, execute, run};
use crate::portable_asm::{PortableAsm, Reg as R};
use crate::{
    NativeCodeGenerator,
    gc_codegen::production::{metadata, test_support::*},
};

fn mapping(h: &mut H<'_>, slot: u32) {
    h.value(Src::Raw(slot));
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.mov_imm64(R::V6, 4096);
    h.g.sub_reg_imm32(R::V4, 16);
    h.g.mov_reg_reg(R::V2, R::V4);
    h.g.mov_imm64(R::V0, 27); // mincore(base,4096,vec): ENOMEM after real munmap
    h.g.asm.syscall();
    h.g.add_reg_imm32(R::V4, 16);
}
fn field(h: &mut H<'_>, name: &str, offset: i32) {
    h.report(name, |h| h.g.load_ptr_disp32(R::V0, R::V3, offset));
}
fn prepared(h: &mut H<'_>) {
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.collect();
    h.finish_collection();
    h.value(Src::Root(0));
    metadata::backing_for(h.g);
    h.keep(20);
    h.collect(); // Copy source is now A, but no Retire cleanup has run yet.
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.close_relocation);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.retire_step); // queues the source mapping, then pauses
    h.counter(Counter::RetireCursor);
    h.keep(21);
}
fn handshake(g: &mut NativeCodeGenerator, failure: u64) {
    let mut h = H::begin(g);
    prepared(&mut h);
    let [state, entry, address, unmaps, failures] = metadata::release_offsets();
    let [_, _, charged, committed, ..] = metadata::live_offsets();
    field(&mut h, "queued", state);
    field(&mut h, "unmaps_before", unmaps);
    field(&mut h, "charged_before", charged);
    h.report("source_mapped", |h| mapping(h, 20));
    h.report("role_before", |h| h.load(Src::Raw(21), 0));
    metadata::unlock_ticket_only(h.g);
    if failure != 0 {
        metadata::fail_release_next(h.g);
    }
    let reclaim = h.g.gc_gen.as_ref().unwrap().labels.metadata_reclaim;
    h.g.call_label(reclaim);
    h.keep(22);
    h.report("result", |h| h.value(Src::Raw(22)));
    field(&mut h, "state_after", state);
    field(&mut h, "charged_after", charged);
    field(&mut h, "committed_after", committed);
    field(&mut h, "failures", failures);
    h.report("mapped_after", |h| mapping(h, 20));
    h.report("role_after_os", |h| h.load(Src::Raw(21), 0));
    h.report("entry_after", |h| {
        h.g.load_ptr_disp32(R::V0, R::V3, entry);
        h.g.load_ptr_disp32(R::V0, R::V0, 0);
    });
    field(&mut h, "job_address", address);
    if failure != 0 {
        metadata::retry_release(h.g);
        h.g.call_label(reclaim);
    }
    // A second claimant cannot release/debit a completed job twice.
    h.g.call_label(reclaim);
    field(&mut h, "unmaps_after", unmaps);
    h.finish_collection();
    field(&mut h, "final_state", state);
    h.report("final_role", |h| h.load(Src::Raw(21), 0));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
    h.report("copy_bytes", |h| {
        let bytes = metadata::copy_bytes(h.g);
        h.g.mov_imm64(R::V0, bytes);
    });
}
#[test]
fn production_metadata_retirement_releases_the_real_mapping_before_free_role_and_debits_once() {
    let o = run(handshake, 0, false);
    assert_eq!(o.get("queued"), 1, "{}", o.text());
    assert_eq!(o.get("source_mapped"), 0);
    assert!(o.get("role_before") > 0);
    assert_eq!(o.get("result"), 1);
    assert_eq!(o.get("state_after"), 3);
    assert_eq!(o.get("mapped_after"), -12);
    assert_eq!(o.get("entry_after"), 0);
    assert!(o.get("role_after_os") > 0);
    assert_eq!(
        o.get("charged_before") - o.get("charged_after"),
        o.get("copy_bytes")
    );
    assert_eq!(o.get("charged_after"), o.get("committed_after"));
    assert_eq!(o.get("unmaps_after") - o.get("unmaps_before"), 1);
    assert_eq!(o.get("failures"), 0);
    assert_eq!(o.get("final_state"), 0);
    assert_eq!(o.get("final_role"), 0);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("id"), 1);
}
#[test]
fn production_metadata_release_failure_retains_mapping_charge_and_role_until_successful_retry() {
    let o = run(handshake, 1, false);
    assert_eq!(o.get("queued"), 1, "{}", o.text());
    assert_eq!(o.get("result"), 0);
    assert_eq!(o.get("state_after"), 2);
    assert_eq!(o.get("mapped_after"), 0);
    assert_eq!(o.get("entry_after"), o.get("job_address"));
    assert!(o.get("role_after_os") > 0);
    assert_eq!(o.get("charged_before"), o.get("charged_after"));
    assert_eq!(o.get("charged_after"), o.get("committed_after"));
    assert_eq!(o.get("failures"), 1);
    assert_eq!(o.get("unmaps_after") - o.get("unmaps_before"), 1);
    assert_eq!(o.get("final_role"), 0);
    assert_eq!(o.get("value"), 84);
}
fn fatal(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    prepared(&mut h);
    metadata::fail_release_next(h.g);
    h.g.call_label(h.l.unlock);
}
#[test]
fn production_metadata_release_failure_has_a_controlled_distinct_diagnostic() {
    let o = execute(fatal, 0, false, false);
    assert_eq!(o.status.code(), Some(1), "{o:?}");
    assert!(
        String::from_utf8_lossy(&o.stderr).contains("native metadata release failed"),
        "{o:?}"
    );
}

fn old_receiver(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 8192 * 8, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), 8191, Src::Raw(1), true);
    h.collect();
    h.finish_collection();
    h.value(Src::Root(0));
    metadata::backing_for(h.g);
    h.keep(20);
    h.mark_start();
    h.mark_quantum(5);
    h.report_counter("field_before", Counter::MarkField);
    h.report_counter("epoch_before", Counter::MarkCycles);
    h.store(Src::Root(0), 8191, Src::Null, true);
    h.pop_root(); // only Old's active receiver and SATB queue retain the graph
    h.collect();
    h.finish_collection();
    h.report("source_mapping", |h| mapping(h, 20));
    h.report_counter("active", Counter::MarkActive);
    h.report_counter("epoch_after", Counter::MarkCycles);
    h.report_counter("field_after", Counter::MarkField);
    h.report("receiver_id", |h| {
        mark_receiver(h.g, None);
        h.keep(22);
        h.object_id(Src::Raw(22));
    });
    h.report("child", |h| {
        mark_receiver(h.g, Some(0));
        h.g.load_ptr_disp32(R::V0, R::V0, 0);
    });
    h.drain_marks();
    h.report_counter("object_done", Counter::MarkObject);
    h.report_counter("queue_done", Counter::MarkStackLen);
}
#[test]
fn production_metadata_unmaps_sources_without_losing_the_live_old_receiver_or_satb_queue() {
    let o = run(old_receiver, 0, false);
    assert_eq!(o.get("source_mapping"), -12, "{}", o.text());
    assert_eq!(o.get("active"), 1);
    assert_eq!(o.get("epoch_before"), o.get("epoch_after"));
    assert_eq!(o.get("field_before"), o.get("field_after"));
    assert_eq!(o.get("receiver_id"), 1);
    assert_eq!(o.get("child"), 84);
    assert_eq!(o.get("object_done"), 0);
    assert_eq!(o.get("queue_done"), 0);
}
fn bounded(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    let [limit, initial, charged, committed, maps, ..] = metadata::live_offsets();
    h.g.load_ptr_disp32(R::V0, R::V3, initial);
    h.g.add_reg_imm32(R::V0, (2 * metadata::copy_bytes(h.g)) as i32);
    h.g.store_ptr_disp32(R::V3, limit, R::V0);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.repeat(32, |h| {
        request_old_cycle(h.g);
        h.collect();
        h.finish_collection();
    });
    for (name, offset) in [
        ("limit", limit),
        ("initial", initial),
        ("charged", charged),
        ("committed", committed),
        ("maps", maps),
        ("unmaps", metadata::release_offsets()[3]),
    ] {
        field(&mut h, name, offset);
    }
    h.report("backings", |h| metadata::copy_backings(h.g));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
}
#[test]
fn production_metadata_recycles_a_two_backing_budget_through_repeated_full_moves() {
    for worker in [false, true] {
        let o = run(bounded, 0, worker);
        assert!(o.get("maps") >= 33, "{}", o.text());
        assert!(o.get("unmaps") >= 31);
        assert_eq!(o.get("maps") - o.get("unmaps"), 1 + o.get("backings"));
        assert_eq!(o.get("charged"), o.get("committed"));
        assert!(o.get("charged") <= o.get("limit"));
        assert!(o.get("charged") > o.get("initial"));
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("id"), 1);
    }
}
