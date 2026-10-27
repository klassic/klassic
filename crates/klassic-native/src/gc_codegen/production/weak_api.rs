use super::*;
#[repr(C)]
pub(in super::super) struct Owner {
    caller: u64,
    output: u64,
    value: u64,
    entry: u64,
}
const OWN: usize = owner_state::offset(offset_of!(OwnerState, weak));
const CALLER: usize = OWN + offset_of!(Owner, caller);
const OUTPUT: usize = OWN + offset_of!(Owner, output);
const VALUE: usize = OWN + offset_of!(Owner, value);
const ENTRY: usize = OWN + offset_of!(Owner, entry);
fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, label: E::TextLabel) {
    a.bind_text_label(label);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    owner_state::bind_running(a, l);
    load_rt(a, R::V1, CALLER);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt(a, CALLER, R::V7);
    store_rt(a, OUTPUT, R::V6);
    a.push_reg(R::V0);
    a.call_label(l.publish_pending);
    quiescence::poll(a, l);
    a.pop_reg(R::V0);
}
fn finish<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.mov_imm64(R::V1, 0);
    for off in (0..size_of::<Owner>()).step_by(8) {
        store_rt(a, OWN + off, R::V1);
    }
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.call_label(l.weak_grow_service);
    a.call_label(l.lease_grow_service);
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();
}
fn admission<E: PortableAsm>(a: &mut E, retry: E::TextLabel) {
    for off in [CLOSED, RT_PHASE_OPEN, RT_RETIRE_STAGE] {
        load_rt(a, R::V1, off);
        a.test_reg_reg(R::V1, R::V1);
        a.jcc_label(C::NotEqual, retry);
    }
}
// Actual table + one-based index + nonreused serial, never caller slot pointers.
fn lookup<E: PortableAsm>(a: &mut E, invalid: E::TextLabel) {
    load_rt(a, R::V7, CALLER);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V0, R::V7, 0);
    lea_rt(a, R::V1, TABLE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V8, R::V7, 8);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, invalid);
    load_rt(a, R::V1, SIZE);
    a.cmp_reg_reg(R::V8, R::V1);
    a.jcc_label(C::Above, invalid);
    a.sub_reg_imm8(R::V8, 1);
    entry(a);
    a.load_ptr_disp32(R::V0, R::V7, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V1, R::V8, 8);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    store_rt(a, ENTRY, R::V8);
}
fn status<E: PortableAsm>(a: &mut E, label: E::TextLabel, value: u64, finish: E::TextLabel) {
    a.bind_text_label(label);
    a.mov_imm64(R::V0, value);
    a.jmp_label(finish);
}
fn create<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l, l.weak_new_actor); // V0=source RootCell, V7=empty handle
    a.call_label(l.root_load_actor);
    encode(a, R::V0, R::V1);
    store_rt(a, VALUE, R::V0);
    worker::lock(a, l);
    let retry = a.create_text_label();
    let invalid = a.create_text_label();
    let exhausted = a.create_text_label();
    let overflow = a.create_text_label();
    let resource = a.create_text_label();
    let acquired = a.create_text_label();
    let finish_label = a.create_text_label();
    admission(a, retry);
    if l.worker.is_none() {
        a.jmp_label(exhausted);
    }
    load_rt(a, R::V7, CALLER);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    for off in [0, 8, 16] {
        a.load_ptr_disp32(R::V1, R::V7, off);
        a.test_reg_reg(R::V1, R::V1);
        a.jcc_label(C::NotEqual, invalid);
    }
    load_rt(a, R::V0, SERIAL);
    a.mov_imm64(R::V1, u64::MAX);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, overflow);
    load_rt(a, R::V8, HEAD);
    a.test_reg_reg(R::V8, R::V8);
    let available = a.create_text_label();
    a.jcc_label(C::NotEqual, available);
    growth::request(a, l);
    a.jmp_label(finish_label);
    a.bind_text_label(available);
    a.sub_reg_imm8(R::V8, 1);
    entry(a);
    store_rt(a, ENTRY, R::V8);
    load_rt(a, R::V0, VALUE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, acquired);
    fields::offset(a, R::V0, R::V2, invalid);
    a.mov_reg_reg(R::V0, R::V2);
    a.shr_reg_imm8(R::V0, 17);
    a.sub_reg_imm8(R::V0, 1);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, run_head)));
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    load_rt(a, R::V6, ENTRY);
    a.store_ptr_disp32(R::V6, 24, R::V8);
    a.load_ptr_disp32(R::V1, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.store_ptr_disp32(R::V6, 32, R::V1);
    a.add_reg_imm32(R::V6, 24);
    lea_rt(a, R::V7, metadata_leases::POOL);
    a.mov_imm64(R::V2, 0);
    a.call_label(l.metadata_lease_step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, resource);
    a.bind_text_label(acquired);
    load_rt(a, R::V8, ENTRY);
    a.load_ptr_disp32(R::V1, R::V8, 48);
    load_rt(a, R::V7, CALLER);
    load_rt(a, R::V0, HEAD);
    a.store_ptr_disp32(R::V7, 8, R::V0);
    store_rt(a, HEAD, R::V1);
    lea_rt(a, R::V1, TABLE);
    a.store_ptr_disp32(R::V7, 0, R::V1);
    copy::bump(a, SERIAL, R::V0);
    load_rt(a, R::V0, SERIAL);
    a.store_ptr_disp32(R::V7, 16, R::V0);
    a.store_ptr_disp32(R::V8, 8, R::V0);
    lea_rt(a, R::V0, GATE);
    a.store_ptr_disp32(R::V8, 0, R::V0);
    load_rt(a, R::V0, VALUE);
    a.store_ptr_disp32(R::V8, 16, R::V0);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V8, 48, R::V0);
    copy::bump(a, ACTIVE, R::V0);
    growth::mirror(a);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(finish_label);
    a.bind_text_label(resource);
    load_rt(a, R::V8, ENTRY);
    a.mov_imm64(R::V1, 0);
    for off in [24, 32, 40] {
        a.store_ptr_disp32(R::V8, off, R::V1);
    }
    let failed_resource = a.create_text_label();
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::NotEqual, failed_resource);
    metadata_leases::weak_pending(a, retry);
    a.bind_text_label(failed_resource);
    a.mov_imm64(R::V0, 6);
    a.jmp_label(finish_label);
    status(a, retry, 2, finish_label);
    status(a, invalid, 3, finish_label);
    status(a, exhausted, 6, finish_label);
    status(a, overflow, 5, finish_label);
    a.bind_text_label(finish_label);
    finish(a, l);
}
fn release<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l, l.weak_release_actor);
    worker::lock(a, l);
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    lookup(a, invalid);
    a.call_label(l.weak_clear);
    load_rt(a, R::V0, HEAD);
    a.store_ptr_disp32(R::V8, 48, R::V0);
    load_rt(a, R::V7, CALLER);
    a.load_ptr_disp32(R::V0, R::V7, 8);
    store_rt(a, HEAD, R::V0);
    a.mov_imm64(R::V0, 0);
    for off in [0, 8] {
        a.store_ptr_disp32(R::V8, off, R::V0);
    }
    for off in [0, 8, 16] {
        a.store_ptr_disp32(R::V7, off, R::V0);
    }
    load_rt(a, R::V1, ACTIVE);
    a.sub_reg_imm8(R::V1, 1);
    store_rt(a, ACTIVE, R::V1);
    growth::mirror(a);
    worker::request_work(a);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    status(a, invalid, 3, done);
    a.bind_text_label(done);
    finish(a, l);
}
fn load<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l, l.weak_load_actor); // V7=handle, V6=empty registered output cell
    let again = a.create_text_label();
    let retry = a.create_text_label();
    let invalid = a.create_text_label();
    let empty = a.create_text_label();
    let dead = a.create_text_label();
    let flush = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(again);
    worker::lock(a, l);
    lookup(a, invalid);
    admission(a, retry);
    load_rt(a, R::V8, OUTPUT);
    load_rt(a, R::V2, RT_SEGMENT);
    a.load_ptr_disp32(R::V1, R::V2, d(offset_of!(RootSegment, cells)));
    a.mov_reg_reg(R::V0, R::V8);
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid);
    a.mov_reg_reg(R::V1, R::V0);
    a.and_reg_imm32(R::V1, 31);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V2, d(offset_of!(RootSegment, used)));
    a.shl_reg_imm8(R::V1, 5);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, invalid);
    roots::frame_handle(a, R::V8, R::V2);
    lea_rt(a, R::V7, RT_CTX);
    lea_rt(a, R::V6, RT_FRAME_ROOT);
    a.mov_imm64(R::V2, ROOT_READ_RAW);
    a.call_label(l.step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    load_rt(a, R::V0, RT_FRAME_ROOT + REQUEST_VALUE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    load_rt(a, R::V8, ENTRY);
    a.load_ptr_disp32(R::V0, R::V8, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    store_rt(a, VALUE, R::V0);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    require_live(a, dead);
    if let Some(w) = &l.worker {
        let old_done = a.create_text_label();
        load_rt(a, R::V0, RT_MARK_ACTIVE);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, old_done);
        a.call_label(w.mark_ref);
        store_rt_imm(a, RT_MARK_DONE, 0);
        a.bind_text_label(old_done);
        a.call_label(w.young.reference);
        store_rt_imm(a, young_mark::DONE, 0);
    }
    load_rt(a, R::V0, VALUE);
    store_rt(a, RT_FRAME_ROOT + REQUEST_VALUE, R::V0);
    lea_rt(a, R::V7, RT_CTX);
    lea_rt(a, R::V6, RT_FRAME_ROOT);
    a.mov_imm64(R::V2, ROOT_STORE);
    a.call_label(l.step);
    a.cmp_reg_imm8(R::V0, 3); // actual tracked root journal pressure
    a.jcc_label(C::Equal, flush);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    #[cfg(test)]
    if let Some(hook) = l.weak_publish_hook {
        a.call_label(hook);
    }
    worker::request_work(a);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(flush);
    a.call_label(l.root_journal_seal_owner);
    worker::unlock(a, l);
    a.jmp_label(again);
    a.bind_text_label(dead);
    a.call_label(l.weak_clear);
    status(a, empty, 1, done);
    status(a, retry, 2, done);
    status(a, invalid, 3, done);
    a.bind_text_label(done);
    finish(a, l);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    create(a, l);
    load(a, l);
    release(a, l);
}
