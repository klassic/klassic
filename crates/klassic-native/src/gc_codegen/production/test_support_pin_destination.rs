use super::super::*;
pub(crate) fn pin_destination_call(
    g: &mut crate::NativeCodeGenerator,
    token: crate::DataLabel,
    cancel: bool,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = if cancel {
        l.pin_destination_cancel_actor
    } else {
        l.pin_destination_reserve_actor
    };
    g.mov_data_addr(R::V7, token);
    super::call_owner(g, entry);
}
pub(crate) fn pin_destination_metric(g: &mut crate::NativeCodeGenerator, index: usize) {
    let off = [
        pin_destination::ACTIVE,
        pin_destination::PAGES,
        pin_destination::CANCELLED,
        large_object::LIVE_PAGES,
        large_object::RECLAIMED_PAGES,
    ][index];
    load_rt(g, R::V0, off);
}
/// Diagnostic inspection only: no managed pointer is returned by the API.
pub(crate) fn pin_destination_geometry(
    g: &mut crate::NativeCodeGenerator,
    token: crate::DataLabel,
    index: usize,
) {
    g.mov_data_addr(R::V7, token);
    g.load_ptr_disp32(R::V0, R::V7, 8);
    if index == 0 {
        large_object::record(g, R::V0);
        g.load_ptr_disp32(R::V0, R::V8, pin_destination::RESERVATION);
        g.sub_reg_reg(R::V0, R::V7); // zero means the actual unique holder
        return;
    }
    g.sub_reg_imm8(R::V0, 1);
    scan::page_ptr(g, R::V8, R::V0, R::V1);
    let off = [
        0,
        offset_of!(AllocationPage, allocated_top),
        offset_of!(AllocationPage, start),
        offset_of!(AllocationPage, state),
        offset_of!(AllocationPage, incarnation),
    ][index];
    g.load_ptr_disp32(R::V0, R::V8, d(off));
}
pub(crate) fn pin_destination_limit(g: &crate::NativeCodeGenerator) -> u64 {
    geometry(g).pages
}
/// Dynamic token address is already in V7; the actual actor authenticates it.
pub(crate) fn pin_destination_dynamic(g: &mut crate::NativeCodeGenerator, cancel: bool) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = if cancel {
        l.pin_destination_cancel_actor
    } else {
        l.pin_destination_reserve_actor
    };
    super::call_owner(g, entry);
}
pub(crate) fn pin_destination_actor_reserve(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    source: crate::DataLabel,
) -> crate::DataLabel {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    let token = super::lease_request(g);
    g.mov_data_addr(R::V0, source);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    pin_destination_call(g, token, false);
    super::lease_require_ok(g);
    g.pop_reg(R::OwnerContext);
    token
}
pub(crate) fn pin_destination_actor_cancel(
    g: &mut crate::NativeCodeGenerator,
    actor: super::TestActor,
    token: crate::DataLabel,
) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    pin_destination_call(g, token, true);
    super::lease_require_ok(g);
    g.pop_reg(R::OwnerContext);
}

pub(crate) fn pin_destination_watch(g: &mut crate::NativeCodeGenerator, token: crate::DataLabel) {
    let entry = g
        .gc_gen
        .as_ref()
        .unwrap()
        .labels
        .pin_destination_watch_actor;
    g.mov_data_addr(R::V7, token);
    super::call_owner(g, entry);
}
pub(crate) fn pin_destination_is_large(g: &mut crate::NativeCodeGenerator) {
    // V0 is an actual canonical payload; emit a scalar role query.
    load_rt(g, R::V1, RT_HEAP_BASE);
    g.sub_reg_reg(R::V0, R::V1);
    generation::test_role(g, R::V0, generation::LARGE);
    let no = g.create_text_label();
    let done = g.create_text_label();
    g.jcc_label(C::NotEqual, no);
    g.mov_imm64(R::V0, 1);
    g.jmp_label(done);
    g.bind_text_label(no);
    g.mov_imm64(R::V0, 0);
    g.bind_text_label(done);
}

pub(crate) fn pin_copy_metric(g: &mut crate::NativeCodeGenerator, index: usize) {
    let off = [
        pin_copy::OWNER_WINS,
        pin_copy::WORKER_WINS,
        pin_copy::LOSSES,
    ][index];
    load_rt(g, R::V0, off);
}
/// The real phase is open; wake and wait for the actual OS worker to win
/// before allowing the mutator's explicit finish to assist any graph work.
pub(crate) fn pin_copy_wait_worker(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    let w = rt.labels.worker.as_ref().unwrap();
    let (clock, relax) = (w.hooks.clock, w.hooks.relax);
    worker::wake_if_pending(g, &rt.labels);
    g.call_label(clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1856, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    worker::lock(g, &rt.labels);
    load_rt(g, R::V0, pin_copy::WORKER_WINS);
    worker::unlock(g, &rt.labels);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1856);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, rt.labels.fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    g.gc_gen = Some(rt);
}

pub(crate) fn pin_copy_start_full(g: &mut crate::NativeCodeGenerator) {
    // The moving coordinator can return unused reservations before selecting
    // capacity. Use its real Full demand, rather than a temporary Old ceiling.
    store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
    let entry = g.gc_gen.as_ref().unwrap().labels.moving_start_actor;
    super::call_owner(g, entry);
}

pub(crate) fn pin_copy_enable_moving(g: &mut crate::NativeCodeGenerator) {
    let entry = g.gc_gen.as_ref().unwrap().labels.moving_enable_actor;
    super::call_owner(g, entry);
}
/// Only the actual private-candidate initializer waits here, outside a ticket.
/// Other real mutators publish their ordinary source CAS before it resumes.
pub(crate) fn pin_copy_force_private_loss(g: &mut crate::NativeCodeGenerator) {
    let mut rt = g.gc_gen.take().unwrap();
    let w = rt.labels.worker.as_ref().unwrap();
    let (clock, relax) = (w.hooks.clock, w.hooks.relax);
    let hook = g.create_text_label();
    let body = g.create_text_label();
    rt.labels.copy_ready_hook = Some(hook);
    g.jmp_label(body);
    g.bind_text_label(hook);
    quiescence::save(g);
    let done = g.create_text_label();
    load_rt(g, R::V0, pin_copy::slot(false));
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    load_rt(g, R::V0, root_journal::OWNER_LOCKED);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, rt.labels.fail);
    g.call_label(clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1856, R::V0);
    let again = g.create_text_label();
    let pending = g.create_text_label();
    g.bind_text_label(again);
    load_rt(g, R::V1, RT_SCAN_OLD);
    g.add_reg_imm32(R::V1, copy::DESCRIPTOR);
    g.atomic_load_acquire(R::V0, R::V1);
    copy::unless_forwarded(g, R::V0, R::V1, pending);
    g.jmp_label(done);
    g.bind_text_label(pending);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1856);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, rt.labels.fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(body);
    g.gc_gen = Some(rt);
}
