//! Actual per-generation queue backing growth; spill work remains owned
//! independently while mappings/copy/publication or failed release are pending.
use super::*;
struct Fields {
    stage: usize,
    buffer: usize,
    capacity: usize,
    cursor: usize,
    tail: usize,
    old: usize,
    old_bytes: usize,
    error: usize,
    failures: usize,
    grown: usize,
    copied: usize,
    released: usize,
}
fn fields(young: bool) -> Fields {
    let g = lane(young) + offset_of!(Lane, growth);
    Fields {
        stage: g + offset_of!(Growth, stage),
        buffer: g + offset_of!(Growth, buffer),
        capacity: g + offset_of!(Growth, capacity),
        cursor: g + offset_of!(Growth, cursor),
        tail: g + offset_of!(Growth, tail),
        old: g + offset_of!(Growth, old),
        old_bytes: g + offset_of!(Growth, old_bytes),
        error: g + offset_of!(Growth, error),
        failures: g + offset_of!(Growth, failures),
        grown: g + offset_of!(Growth, grown),
        copied: g + offset_of!(Growth, copied),
        released: g + offset_of!(Growth, released),
    }
}
/// Nonbinding lock-free hint only. A stale true result is rechecked under
/// the ticket; a stale false only defers this optional assist to a later poll.
pub(super) fn hint<E: PortableAsm>(a: &mut E, pending: E::TextLabel, young: bool) {
    let f = fields(young);
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    lea_rt(a, R::V1, f.stage);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
    a.jcc_label(C::NotEqual, pending);
}
pub(super) fn retry<E: PortableAsm>(a: &mut E, release: E::TextLabel, young: bool) {
    let f = fields(young);
    a.push_reg(R::V0);
    load_rt(a, R::V0, f.stage);
    a.test_reg_reg(R::V0, R::V0);
    a.pop_reg(R::V0);
    a.jcc_label(C::NotEqual, release);
}
pub(super) fn after_unlock<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, young: bool) {
    a.push_reg(R::V0);
    let done = a.create_text_label();
    lea_rt(a, R::V0, offset_of!(Runtime, worker_view));
    a.cmp_reg_reg(R::OwnerContext, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.call_label(l.queue_grow_services[usize::from(!young)]);
    a.bind_text_label(done);
    a.pop_reg(R::V0);
}
/// Called under the ticket only when no free slot exists. Return 2 while the
/// actual copy/OS work is pending, or 6 after a failed map / address exhaustion.
pub(super) fn request<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, young: bool) {
    let f = fields(young);
    step(a, l, young);
    let pending = a.create_text_label();
    let failed = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, f.stage);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    load_rt(
        a,
        R::V0,
        if young {
            young_mark::LEN
        } else {
            RT_MSTACK_LEN
        },
    );
    load_rt(a, R::V1, super::capacity(young));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, pending);
    load_rt(a, R::V0, f.error);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, failed);
    load_rt(a, R::V0, super::capacity(young));
    a.mov_imm64(R::V1, u64::MAX / (2 * size_of::<u64>() as u64));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, failed);
    a.shl_reg_imm8(R::V0, 1);
    store_rt(a, f.capacity, R::V0);
    store_rt_imm(a, f.stage, 1);
    a.bind_text_label(pending);
    worker::request_work(a);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(failed);
    store_rt_imm(a, f.error, 0);
    a.mov_imm64(R::V0, 6);
    a.bind_text_label(done);
}
/// Mirror one queued reference without acquiring another work/holder owner.
/// The candidate is shadow backing until one locked base publication.
pub(super) fn mirror<E: PortableAsm>(a: &mut E, young: bool) {
    let f = fields(young);
    for r in [R::V0, R::V1, R::V9, R::V10] {
        a.push_reg(r);
    }
    let done = a.create_text_label();
    load_rt(a, R::V0, f.stage);
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::Below, done);
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Above, done);
    load_rt(a, R::V9, super::entries(young));
    a.mov_reg_reg(R::V1, R::V8);
    a.sub_reg_reg(R::V1, R::V9);
    load_rt(a, R::V10, f.buffer);
    a.add_reg_reg(R::V10, R::V1);
    for off in (0..size_of::<u64>()).step_by(8) {
        a.load_ptr_disp32(R::V0, R::V8, d(off));
        a.store_ptr_disp32(R::V10, d(off), R::V0);
    }
    a.bind_text_label(done);
    for r in [R::V10, R::V9, R::V1, R::V0] {
        a.pop_reg(r);
    }
}
/// R8=index -> byte offset R1, preserving R8.
fn offset<E: PortableAsm>(a: &mut E) {
    a.mov_reg_reg(R::V1, R::V8);
    a.shl_reg_imm8(R::V1, 3);
}
/// One old queue word per ticket hold, plus constant publication metadata.
/// The candidate's zeroed native pages are pre-touched outside the ticket.
pub(super) fn step<E: PortableAsm>(a: &mut E, _l: &Labels<E::TextLabel>, young: bool) {
    let f = fields(young);
    // The worker usually has no backing job. Preserve just V0 until actual
    // copy/publication work is observed under this already-held ticket.
    let quick_done = a.create_text_label();
    let active = a.create_text_label();
    a.push_reg(R::V0);
    load_rt(a, R::V0, f.stage);
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::Equal, active);
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Equal, active);
    a.pop_reg(R::V0);
    a.jmp_label(quick_done);
    a.bind_text_label(active);
    a.pop_reg(R::V0);
    quiescence::save(a);
    let done = a.create_text_label();
    let tail = a.create_text_label();
    let commit = a.create_text_label();
    load_rt(a, R::V0, f.stage);
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Equal, tail);
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::NotEqual, done);
    load_rt(a, R::V8, f.cursor);
    load_rt(a, R::V0, super::capacity(young));
    a.cmp_reg_reg(R::V8, R::V0);
    let copying = a.create_text_label();
    a.jcc_label(C::Below, copying);
    store_rt_imm(a, f.stage, 4);
    a.jmp_label(tail);
    a.bind_text_label(copying);
    offset(a);
    load_rt(a, R::V9, super::entries(young));
    a.add_reg_reg(R::V9, R::V1);
    load_rt(a, R::V10, f.buffer);
    a.add_reg_reg(R::V10, R::V1);
    for off in (0..size_of::<u64>()).step_by(8) {
        a.load_ptr_disp32(R::V0, R::V9, d(off));
        a.store_ptr_disp32(R::V10, d(off), R::V0);
    }
    copy::bump(a, f.cursor, R::V0);
    copy::bump(a, f.copied, R::V0);
    a.jmp_label(done);
    a.bind_text_label(tail);
    a.bind_text_label(commit);
    // All queue borrowers retain logical indices across ticket releases.
    // Actual queue-word mutations mirror until the base publishes atomically.
    load_rt(a, R::V0, super::capacity(young));
    a.mov_reg_reg(R::V8, R::V0);
    offset(a);
    store_rt(a, f.old_bytes, R::V1);
    load_rt(a, R::V0, super::entries(young));
    store_rt(a, f.old, R::V0);
    load_rt(a, R::V0, f.buffer);
    store_rt(a, super::entries(young), R::V0);
    load_rt(a, R::V0, f.capacity);
    store_rt(a, super::capacity(young), R::V0);
    store_rt_imm(a, f.buffer, 0);
    load_rt(a, R::V0, f.old);
    lea_rt(
        a,
        R::V1,
        if young {
            layout(a).ystack
        } else {
            layout(a).mstack
        } as usize,
    );
    a.cmp_reg_reg(R::V0, R::V1);
    let retire = a.create_text_label();
    a.jcc_label(C::NotEqual, retire);
    store_rt_imm(a, f.old, 0);
    store_rt_imm(a, f.old_bytes, 0);
    store_rt_imm(a, f.stage, 0);
    let published = a.create_text_label();
    a.jmp_label(published);
    a.bind_text_label(retire);
    store_rt_imm(a, f.stage, 5);
    a.bind_text_label(published);
    copy::bump(a, f.grown, R::V0);
    worker::request_work(a);
    a.bind_text_label(done);
    quiescence::restore(a);
    a.bind_text_label(quick_done);
}
fn enter_native<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, _young: bool) {
    let worker = a.create_text_label();
    lea_rt(a, R::V0, offset_of!(Runtime, worker_view));
    a.cmp_reg_reg(R::OwnerContext, R::V0);
    a.jcc_label(C::Equal, worker);
    a.call_label(l.native_enter_actor);
    a.bind_text_label(worker);
}
fn exit_native<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, _young: bool) {
    let worker = a.create_text_label();
    lea_rt(a, R::V0, offset_of!(Runtime, worker_view));
    a.cmp_reg_reg(R::OwnerContext, R::V0);
    a.jcc_label(C::Equal, worker);
    a.call_label(l.native_exit_actor);
    a.bind_text_label(worker);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, young: bool) {
    let f = fields(young);
    a.bind_text_label(l.queue_grow_services[usize::from(!young)]);
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    let quick_done = a.create_text_label();
    let work = a.create_text_label();
    // A nonbinding hint avoids a second collector ticket on ordinary weak
    // operations. Actual ownership and every parameter are rechecked locked.
    lea_rt(a, R::V1, f.stage);
    a.atomic_load_acquire(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, work);
    a.cmp_reg_imm32(R::V0, 5);
    a.jcc_label(C::NotEqual, quick_done);
    a.bind_text_label(work);
    quiescence::save(a);
    worker::lock_raw(a, l);
    let done = a.create_text_label();
    let release = a.create_text_label();
    let leave_native = a.create_text_label();
    load_rt(a, R::V0, f.stage);
    a.cmp_reg_imm32(R::V0, 5);
    a.jcc_label(C::Equal, release);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, done);
    store_rt_imm(a, f.stage, 2);
    load_rt(a, R::V8, f.capacity);
    offset(a);
    a.mov_reg_reg(R::V7, R::V1);
    worker::unlock_raw(a, l);
    enter_native(a, l, young);
    a.call_label(l.metadata_map);
    let map_failed = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, map_failed);
    a.mov_reg_reg(R::V10, R::V0);
    // Fault/commit each native page outside the collector ticket. OS mappings
    // are prezeroed, so these writes do not initialize unpublished GC values.
    a.mov_reg_reg(R::V9, R::V0);
    load_rt(a, R::V8, f.capacity);
    offset(a);
    a.add_reg_reg(R::V1, R::V10);
    a.mov_imm64(R::V0, 0);
    let touch = a.create_text_label();
    a.bind_text_label(touch);
    a.store_ptr_disp32(R::V9, 0, R::V0);
    a.add_reg_imm32(R::V9, 4096);
    a.cmp_reg_reg(R::V9, R::V1);
    a.jcc_label(C::Below, touch);
    worker::lock_raw(a, l);
    store_rt(a, f.buffer, R::V10);
    store_rt_imm(a, f.cursor, 0);
    load_rt(a, R::V0, super::capacity(young));
    store_rt(a, f.tail, R::V0);
    store_rt_imm(a, f.stage, 3);
    worker::request_work(a);
    worker::unlock_raw(a, l);
    a.jmp_label(leave_native);
    a.bind_text_label(map_failed);
    worker::lock_raw(a, l);
    store_rt_imm(a, f.error, 6);
    store_rt_imm(a, f.stage, 0);
    copy::bump(a, f.failures, R::V0);
    worker::unlock_raw(a, l);
    a.jmp_label(leave_native);
    a.bind_text_label(release);
    store_rt_imm(a, f.stage, 6);
    load_rt(a, R::V7, f.old);
    load_rt(a, R::V6, f.old_bytes);
    worker::unlock_raw(a, l);
    enter_native(a, l, young);
    a.call_label(l.metadata_native_release);
    worker::lock_raw(a, l);
    let retained = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, retained);
    store_rt_imm(a, f.old, 0);
    store_rt_imm(a, f.old_bytes, 0);
    store_rt_imm(a, f.stage, 0);
    copy::bump(a, f.released, R::V0);
    let released = a.create_text_label();
    a.jmp_label(released);
    a.bind_text_label(retained);
    store_rt_imm(a, f.stage, 5);
    copy::bump(a, f.failures, R::V0);
    worker::request_work(a);
    a.bind_text_label(released);
    worker::unlock_raw(a, l);
    a.bind_text_label(leave_native);
    exit_native(a, l, young);
    quiescence::restore(a);
    a.jmp_label(quick_done);
    a.bind_text_label(done);
    worker::unlock_raw(a, l);
    quiescence::restore(a);
    a.bind_text_label(quick_done);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
    a.ret();
}

#[cfg(test)]
pub(super) fn metric_offsets(young: bool) -> [usize; 12] {
    let f = fields(young);
    [
        super::capacity(young),
        f.stage,
        f.cursor,
        f.tail,
        f.failures,
        f.grown,
        f.copied,
        f.released,
        f.old,
        f.buffer,
        f.error,
        super::entries(young),
    ]
}
