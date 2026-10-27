//! Fund concurrent fresh allocation before admitting bytes. Next-fit copying
//! and fresh objects share streams, so two bytes of destination allowance are
//! reserved per fresh byte, in addition to every possible copier's bound.
use super::*;
const RESERVED: usize = OWN + offset_of!(Owner, fresh_reserved);
const BANK: usize = OWN + offset_of!(Owner, fresh_bank);
fn members<E: PortableAsm>(a: &mut E, reg: R) {
    load_rt(
        a,
        reg,
        participants::HANDSHAKE + offset_of!(HandshakeState, members),
    );
}
pub(in super::super) fn request_window<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    load_rt(a, R::V0, ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    members(a, R::V0);
    a.mov_imm64(R::V1, 4);
    a.push_reg(R::V2);
    a.mul_unsigned_wide(R::V1);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.oom);
    a.pop_reg(R::V2);
    a.add_reg_imm32(R::V0, 2);
    a.jcc_label(C::Below, l.oom);
    let bounded = a.create_text_label();
    a.mov_imm64(R::V1, super::super::geometry(a).young_pages);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, bounded);
    a.mov_reg_reg(R::V0, R::V1);
    a.bind_text_label(bounded);
    a.cmp_reg_reg(R::V2, R::V0);
    a.jcc_label(C::AboveOrEqual, done);
    a.mov_reg_reg(R::V2, R::V0);
    a.bind_text_label(done);
}
pub(in super::super) fn add_fresh_window<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    load_rt(a, R::V1, ENABLED);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, done);
    members(a, R::V1);
    a.add_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Below, l.oom);
    a.add_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.oom);
    a.bind_text_label(done);
}
pub(in super::super) fn fund<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    quiescence::save(a);
    let done = a.create_text_label();
    let full = a.create_text_label();
    let chosen = a.create_text_label();
    load_rt(a, R::V0, ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, RT_FULL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, full);
    capacity::promotion_pages(a, l);
    a.jmp_label(chosen);
    a.bind_text_label(full);
    a.mov_imm64(R::V0, 0);
    capacity::copy_bound(a, l);
    a.bind_text_label(chosen);
    load_rt(a, R::V1, capacity::ACTIVE);
    let none = a.create_text_label();
    let budget = a.create_text_label();
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, none);
    a.sub_reg_reg(R::V1, R::V0);
    a.shr_reg_imm8(R::V1, 1);
    members(a, R::V0);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, budget);
    a.mov_reg_reg(R::V1, R::V0);
    a.jmp_label(budget);
    a.bind_text_label(none);
    a.mov_imm64(R::V1, 0);
    a.bind_text_label(budget);
    a.shl_reg_imm8(R::V1, 17);
    store_rt(a, capacity::FRESH_BUDGET, R::V1);
    a.bind_text_label(done);
    quiescence::restore(a);
}
/// Ticket held; an unconsumed reservation precedes allocation, never a live
/// initializer. Release it before acknowledging a phase/bank change.
pub(in super::super) fn refund<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    quiescence::save(a);
    let done = a.create_text_label();
    load_rt(a, R::V0, RESERVED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V1, BANK);
    load_rt(a, R::V2, allocation_records::REVISION);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V1, capacity::FRESH_BUDGET);
    a.add_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    store_rt(a, capacity::FRESH_BUDGET, R::V0);
    store_rt_imm(a, RESERVED, 0);
    store_rt_imm(a, BANK, 0);
    a.bind_text_label(done);
    quiescence::restore(a);
}
pub(in super::super) fn reserve<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    quiescence::save(a);
    let retry = a.create_text_label();
    let ready = a.create_text_label();
    let wait = a.create_text_label();
    let sized = a.create_text_label();
    let already = a.create_text_label();
    a.bind_text_label(retry);
    worker::lock(a, l);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, ready);
    load_rt(a, R::V0, RT_FRESH_SIZE);
    a.cmp_reg_imm32(R::V0, MAX_PAYLOAD as i32);
    a.jcc_label(C::Above, ready);
    a.add_reg_imm32(R::V0, 15);
    a.and_reg_imm32(R::V0, -16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V0, copy::HEADER);
    a.mov_reg_reg(R::V6, R::V0);
    load_rt(a, R::V1, RESERVED);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, already);
    load_rt(a, R::V1, capacity::FRESH_BUDGET);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, wait);
    a.sub_reg_reg(R::V1, R::V0);
    store_rt(a, capacity::FRESH_BUDGET, R::V1);
    store_rt(a, RESERVED, R::V0);
    load_rt(a, R::V0, allocation_records::REVISION);
    store_rt(a, BANK, R::V0);
    a.jmp_label(ready);
    a.bind_text_label(already);
    a.cmp_reg_reg(R::V1, R::V6);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, BANK);
    load_rt(a, R::V1, allocation_records::REVISION);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.jmp_label(ready);
    a.bind_text_label(wait);
    copy::bump(a, RT_ALLOCATION_STALLS, R::V0);
    worker::request_work(a);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.call_label(l.moving_end_actor);
    a.call_label(l.owner_checkpoint_poll);
    a.call_label(l.worker.as_ref().unwrap().hooks.relax);
    a.jmp_label(retry);
    a.bind_text_label(ready);
    worker::unlock(a, l);
    quiescence::restore(a);
}
pub(in super::super) fn consume<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    quiescence::save(a);
    let done = a.create_text_label();
    load_rt(a, R::V0, RESERVED);
    a.test_reg_reg(R::V0, R::V0);
    let funded = a.create_text_label();
    a.jcc_label(C::NotEqual, funded);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, RT_FRESH_SIZE);
    a.cmp_reg_imm32(R::V0, MAX_PAYLOAD as i32 + 1);
    a.jcc_label(C::Below, l.fail);
    a.jmp_label(done);
    a.bind_text_label(funded);
    load_rt(a, R::V0, BANK);
    load_rt(a, R::V1, allocation_records::REVISION);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt_imm(a, RESERVED, 0);
    store_rt_imm(a, BANK, 0);
    a.bind_text_label(done);
    quiescence::restore(a);
}

/// Ticket held, pending initialization already published. Discarded TLAB
/// bytes consume the same finite window as fresh allocation. If they cannot
/// be funded, retain the actual page and complete relocation before closing
/// it. Never saturate the debit: that would silently spend the copy reserve.
/// No safepoint may separate this charge from close_owner_tlab.
pub(in super::super) fn native_tail<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    wait: E::TextLabel,
) {
    let done = a.create_text_label();
    let funded = a.create_text_label();
    load_rt(a, R::V0, ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, RESERVED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, RT_REQUEST + offset_of!(AllocationRequest, page));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V2, RT_CTX + offset_of!(MutatorContext, tlab_end));
    load_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, tlab_top));
    a.sub_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Below, l.fail);
    a.cmp_reg_imm32(R::V2, HEAP_PAGE_BYTES as i32);
    a.jcc_label(C::Above, l.fail);
    load_rt(a, R::V0, capacity::FRESH_BUDGET);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::AboveOrEqual, funded);
    copy::bump(a, NATIVE_TAIL_WAITS, R::V0);
    a.jmp_label(wait);
    a.bind_text_label(funded);
    a.sub_reg_reg(R::V0, R::V2);
    store_rt(a, capacity::FRESH_BUDGET, R::V0);
    copy::add_bytes(a, NATIVE_TAIL_BYTES, R::V2);
    a.bind_text_label(done);
}
