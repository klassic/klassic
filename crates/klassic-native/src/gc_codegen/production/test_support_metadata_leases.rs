use super::super::*;
pub(crate) fn lease_request(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    g.asm.data_label_with_i64s(&[0, 0, 0])
}
pub(crate) fn lease_require_ok(g: &mut crate::NativeCodeGenerator) {
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
}
pub(crate) fn reference_lease(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    g.asm.data_label_with_i64s(&[0; 4])
}
/// V0=actual registered RootCell. Fixture initialization while Idle; the
/// production acquire validates the same strong root and grants the token.
pub(crate) fn reference_acquire(g: &mut crate::NativeCodeGenerator, request: crate::DataLabel) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (load, acquire) = (l.root_load_actor, l.metadata_lease_root_actor);
    g.push_reg(R::V0);
    super::call_owner(g, load);
    encode(g, R::V0, R::V1);
    g.mov_data_addr(R::V7, request);
    g.store_ptr_disp32(R::V7, 0, R::V0);
    g.add_reg_imm32(R::V7, 8);
    g.pop_reg(R::V0);
    super::call_owner(g, acquire);
    lease_require_ok(g);
}
pub(crate) fn reference_transfer(
    g: &mut crate::NativeCodeGenerator,
    request: crate::DataLabel,
    release: bool,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = if release {
        l.metadata_lease_release_actor
    } else {
        l.metadata_lease_retarget_actor
    };
    g.mov_data_addr(R::V7, request);
    if release {
        g.add_reg_imm32(R::V7, 8);
    }
    super::call_owner(g, entry);
}
pub(crate) fn reference_generation(g: &mut crate::NativeCodeGenerator, request: crate::DataLabel) {
    g.mov_data_addr(R::V1, request);
    g.load_ptr_disp32(R::V1, R::V1, 8);
    g.load_ptr_disp32(R::V0, R::V1, d(offset_of!(AllocationPage, generation)));
}
pub(crate) fn reference_actor_acquire(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    root: crate::DataLabel,
    request: crate::DataLabel,
) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    g.mov_data_addr(R::V0, root);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    reference_acquire(g, request);
    g.pop_reg(R::OwnerContext);
}
pub(crate) fn reference_actor_release(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    request: crate::DataLabel,
) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    reference_transfer(g, request, true);
    lease_require_ok(g);
    g.pop_reg(R::OwnerContext);
}
/// V0 already contains the registered RootHandle address for acquire.
pub(crate) fn lease_call(g: &mut crate::NativeCodeGenerator, request: crate::DataLabel, op: u64) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = match op {
        0 => l.metadata_lease_root_actor,
        1 => l.metadata_lease_release_actor,
        2 => l.metadata_lease_resolve_actor,
        _ => unreachable!(),
    };
    g.mov_data_addr(R::V7, request);
    super::call_owner(g, entry);
}
pub(crate) fn lease_metric(g: &mut crate::NativeCodeGenerator, metric: usize) {
    let f = [
        metadata_leases::POOL + offset_of!(crate::gc_abi::MetadataLeasePool, active),
        metadata_leases::WAITS,
        metadata_leases::UNITS,
        metadata_leases::CURSOR,
    ][metric];
    load_rt(g, R::V0, f);
}
pub(crate) fn lease_request_word(
    g: &mut crate::NativeCodeGenerator,
    request: crate::DataLabel,
    index: i32,
) {
    g.mov_data_addr(R::V1, request);
    g.load_ptr_disp32(R::V0, R::V1, index * 8);
}
pub(crate) fn lease_copy(
    g: &mut crate::NativeCodeGenerator,
    src: crate::DataLabel,
    dest: crate::DataLabel,
) {
    g.mov_data_addr(R::V1, src);
    g.mov_data_addr(R::V2, dest);
    for off in [0, 8, 16] {
        g.load_ptr_disp32(R::V0, R::V1, off);
        g.store_ptr_disp32(R::V2, off, R::V0);
    }
}

pub(crate) fn lease_actor_acquire(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    root: crate::DataLabel,
    request: crate::DataLabel,
) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    g.mov_data_addr(R::V0, root);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    lease_call(g, request, 0);
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.pop_reg(R::OwnerContext);
}

pub(crate) fn lease_open_and_wait(g: &mut crate::NativeCodeGenerator, full: bool) {
    if full {
        store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
    }
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (start, end, poll, clock, relax, fail) = (
        l.moving_start_actor,
        l.moving_end_actor,
        l.poll_actor,
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    super::call_owner(g, start);
    g.call_label(clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1504, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    super::call_owner(g, end);
    super::call_owner(g, poll);
    load_rt(g, R::V0, metadata_leases::WAITS);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1504);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}

pub(crate) fn weak_call(g: &mut crate::NativeCodeGenerator, handle: crate::DataLabel, op: u64) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = match op {
        0 => l.weak_new_actor,
        1 => l.weak_load_actor,
        2 => l.weak_release_actor,
        _ => unreachable!(),
    };
    g.mov_data_addr(R::V7, handle);
    super::call_owner(g, entry);
}
pub(crate) fn weak_metric(g: &mut crate::NativeCodeGenerator, index: usize) {
    let field = [
        weak::ACTIVE,
        weak::CLOSED,
        weak::CLEARED,
        weak::REBOUND,
        weak::UNITS,
    ][index];
    load_rt(g, R::V0, field);
}

pub(crate) fn weak_actor_prepare(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    source: crate::DataLabel,
) -> (crate::DataLabel, crate::DataLabel) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    let handle = lease_request(g);
    g.mov_data_addr(R::V0, source);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    weak_call(g, handle, 0);
    lease_require_ok(g);
    let output = g.asm.data_label_with_i64s(&[0]);
    let adopt = g.gc_gen.as_ref().unwrap().labels.root_adopt_actor;
    g.mov_data_addr(R::V0, output);
    super::call_owner(g, adopt);
    g.pop_reg(R::OwnerContext);
    (handle, output)
}
pub(crate) fn weak_actor_load_id(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    handle: crate::DataLabel,
    output: crate::DataLabel,
) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    g.mov_data_addr(R::V6, output);
    g.load_ptr_disp32(R::V6, R::V6, 0);
    weak_call(g, handle, 1);
    lease_require_ok(g);
    let read = g.gc_gen.as_ref().unwrap().labels.root_load_actor;
    g.mov_data_addr(R::V0, output);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    super::call_owner(g, read);
    g.load_ptr_disp32(R::V0, R::V0, copy::MARK_RECORD);
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(MarkRecord, object_id)));
    g.push_reg(R::V0);
    weak_call(g, handle, 2);
    lease_require_ok(g);
    g.pop_reg(R::V0);
    g.pop_reg(R::OwnerContext);
}

/// Probe the same shared reference validation leaf called by the production
/// root acquisition entry. Invalid-reference probes never invent a context.
pub(crate) fn reference_probe(g: &mut crate::NativeCodeGenerator, request: crate::DataLabel) {
    let rt = g.gc_gen.take().unwrap();
    g.push_reg(R::V3);
    owner_state::bind_running(g, &rt.labels);
    worker::lock(g, &rt.labels);
    g.mov_data_addr(R::V6, request);
    lea_rt(g, R::V7, metadata_leases::POOL);
    g.call_label(rt.labels.metadata_reference_step);
    worker::unlock(g, &rt.labels);
    g.pop_reg(R::V3);
    g.gc_gen = Some(rt);
}
/// V0=canonical root payload -> diagnostic geometry, no safepoint.
pub(crate) fn reference_geometry(g: &mut crate::NativeCodeGenerator, metric: usize) {
    load_rt(g, R::V1, RT_HEAP_BASE);
    g.sub_reg_reg(R::V0, R::V1);
    if metric == 0 {
        g.and_reg_imm32(R::V0, HEAP_PAGE_BYTES as i32 - 1);
        return;
    }
    g.shr_reg_imm8(R::V0, 17);
    g.sub_reg_imm8(R::V0, 1);
    scan::page_ptr(g, R::V8, R::V0, R::V1);
    if metric == 1 {
        g.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, state)));
        return;
    }
    if metric == 2 {
        g.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
        return;
    }
    g.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, run_head)));
    scan::page_ptr(g, R::V8, R::V0, R::V1);
    let f = [
        offset_of!(AllocationPage, start),
        offset_of!(AllocationPage, limit),
        offset_of!(AllocationPage, allocated_top),
        offset_of!(AllocationPage, starts_words),
        offset_of!(AllocationPage, incarnation),
    ][metric - 3];
    g.load_ptr_disp32(R::V0, R::V8, d(f));
}

pub(crate) fn weak_growth_metric(g: &mut crate::NativeCodeGenerator, index: usize) {
    weak::growth_metric(g, index);
}
pub(crate) fn weak_growth_pause(g: &mut crate::NativeCodeGenerator, paused: bool) {
    weak::growth_pause(g, paused);
}
pub(crate) fn weak_growth_tick(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    weak::growth_tick(g, &rt.labels);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}
pub(crate) fn weak_growth_service(g: &mut crate::NativeCodeGenerator) {
    let label = g.gc_gen.as_ref().unwrap().labels.weak_grow_service;
    g.call_label(label);
}
pub(crate) fn weak_growth_map_fail_next(g: &mut crate::NativeCodeGenerator) {
    metadata::fail_next(g);
}
pub(crate) fn weak_growth_committed(g: &mut crate::NativeCodeGenerator) {
    let off = metadata::live_offsets()[3];
    g.load_ptr_disp32(R::V0, R::V3, off);
}

pub(crate) fn weak_growth_create(
    g: &mut crate::NativeCodeGenerator,
    handle: crate::DataLabel,
    source_slot: i32,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 30_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1504, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    g.load_rbp_slot(R::V0, source_slot);
    weak_call(g, handle, 0);
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, done);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1504);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    super::lease_require_ok(g);
}

pub(crate) fn lease_growth_metric(g: &mut crate::NativeCodeGenerator, index: usize) {
    super::super::metadata_leases::growth_metric(g, index);
}
pub(crate) fn lease_growth_pause(g: &mut crate::NativeCodeGenerator, paused: bool) {
    super::super::metadata_leases::growth_pause(g, paused);
}
pub(crate) fn lease_growth_tick(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    super::super::metadata_leases::work(g, &rt.labels);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}
pub(crate) fn lease_growth_service(g: &mut crate::NativeCodeGenerator) {
    let entry = g.gc_gen.as_ref().unwrap().labels.lease_grow_service;
    g.call_label(entry);
}
/// Emit a real ABI request for one element of a caller-owned request array.
/// The index lives in fixture slot1800; no runtime counter is fabricated.
pub(crate) fn lease_growth_array_call(
    g: &mut crate::NativeCodeGenerator,
    requests: crate::DataLabel,
    source_slot: i32,
    op: u64,
) {
    g.load_rbp_slot(R::V0, 1800);
    g.mov_reg_reg(R::V1, R::V0);
    g.shl_reg_imm8(R::V1, 1);
    g.add_reg_reg(R::V0, R::V1);
    g.shl_reg_imm8(R::V0, 3);
    g.mov_data_addr(R::V7, requests);
    g.add_reg_reg(R::V7, R::V0);
    g.load_rbp_slot(R::V0, source_slot);
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = match op {
        0 => l.metadata_lease_root_actor,
        1 => l.metadata_lease_release_actor,
        2 => l.metadata_lease_resolve_actor,
        _ => unreachable!(),
    };
    super::call_owner(g, entry);
}
pub(crate) fn lease_growth_group(
    g: &mut crate::NativeCodeGenerator,
    requests: crate::DataLabel,
    source_slot: i32,
    start: u64,
    end: u64,
    release: bool,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 30_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1504, R::V0);
    g.mov_imm64(R::V0, start);
    g.store_rbp_slot(1800, R::V0);
    let again = g.create_text_label();
    let retry = g.create_text_label();
    let next = g.create_text_label();
    g.bind_text_label(again);
    lease_growth_array_call(g, requests, source_slot, u64::from(release));
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, next);
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, fail);
    g.bind_text_label(retry);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1504);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(next);
    g.load_rbp_slot(R::V0, 1800);
    g.add_reg_imm32(R::V0, 1);
    g.store_rbp_slot(1800, R::V0);
    g.mov_imm64(R::V1, end);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::Below, again);
}
