//! Slot-budgeted Young graph/root/remembered discovery. No Old field walk.
use super::young_mark as y;
use super::{
    Labels, RT_HEAP_BASE, RT_PHASE_OPEN,
    copy::{DESCRIPTOR, PAYLOAD},
    load_rt, store_rt,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let w = l.worker.as_ref().unwrap();
    trace(a, l, w.young.trace, w.young.reference);
}
fn trace<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    entry: E::TextLabel,
    mark_ref: E::TextLabel,
) {
    let top = a.create_text_label();
    let fields = a.create_text_label();
    let field_loop = a.create_text_label();
    let next_field = a.create_text_label();
    let empty = a.create_text_label();
    let paused = a.create_text_label();
    let roots = a.create_text_label();
    let fields_done = a.create_text_label();
    a.bind_text_label(entry);
    #[cfg(test)]
    if let Some(gate) = l.young_mark_gate {
        let proceed = a.create_text_label();
        a.call_label(gate);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, proceed);
        a.ret(); // No consumed graph budget or claimed completion while gated.
        a.bind_text_label(proceed);
    }
    // Young is sealed before relocation; never trace a moving phase.
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, paused);
    a.bind_text_label(top);
    // A suspended object's fields are a live frontier even when the queue
    // is empty. Resume that cursor before taking another object or root.
    load_rt(a, R::V0, y::OBJECT);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, field_loop);
    super::mark_queue::finish_active(a, l, true);
    let spill = a.create_text_label();
    let popped = a.create_text_label();
    load_rt(a, R::V0, y::LEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, spill);
    super::scan::spend(a, paused);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, y::LEN, R::V0);
    a.shl_reg_imm8(R::V0, 3);
    load_rt(a, R::V1, super::mark_queue::entries(true));
    a.add_reg_reg(R::V0, R::V1);
    a.load_ptr_disp32(R::V2, R::V0, 0);
    a.jmp_label(popped);
    a.bind_text_label(spill);
    load_rt(a, R::V0, super::mark_queue::count(true));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, roots);
    super::scan::spend(a, paused);
    super::mark_queue::take(a, l, true, roots);
    a.bind_text_label(popped);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.mov_reg_reg(R::V6, R::V2);
    a.shr_reg_imm8(R::V6, 16);
    a.add_reg_reg(R::V1, R::V6);
    a.load_ptr_disp32(R::V0, R::V1, DESCRIPTOR);
    a.load_ptr_disp32(R::V6, R::V1, PAYLOAD);
    a.shr_reg_imm8(R::V6, 3);
    store_rt(a, y::COUNT, R::V6);
    a.mov_imm64(R::V7, 0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, fields);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::NotEqual, top);
    a.mov_imm64(R::V7, 1);
    a.bind_text_label(fields);
    store_rt(a, y::OBJECT, R::V2);
    store_rt(a, y::FIELD, R::V7);
    a.bind_text_label(field_loop);
    load_rt(a, R::V7, y::FIELD);
    load_rt(a, R::V6, y::COUNT);
    a.cmp_reg_reg(R::V7, R::V6);
    a.jcc_label(C::AboveOrEqual, fields_done);
    super::scan::spend(a, paused);
    a.shl_reg_imm8(R::V7, 3);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.add_reg_reg(R::V7, R::V1);
    load_rt(a, R::V1, y::OBJECT);
    a.shr_reg_imm8(R::V1, 16);
    a.add_reg_reg(R::V7, R::V1);
    a.load_ptr_disp32(R::V0, R::V7, 0);
    super::fields::young_offset(a, R::V0, R::V2, next_field);
    for reg in [R::V10, R::V10, R::V7, R::V0] {
        a.push_reg(reg);
    }
    a.call_label(mark_ref);
    a.pop_reg(R::V0);
    a.pop_reg(R::V10);
    super::young_barrier::heal(a);
    a.pop_reg(R::V10);
    a.pop_reg(R::V10);
    a.bind_text_label(next_field);
    load_rt(a, R::V7, y::FIELD);
    a.add_reg_imm32(R::V7, 1);
    store_rt(a, y::FIELD, R::V7);
    a.jmp_label(field_loop);
    a.bind_text_label(fields_done);
    a.mov_imm64(R::V0, 0);
    store_rt(a, y::OBJECT, R::V0);
    a.jmp_label(top);
    a.bind_text_label(roots);
    discover(a, l, mark_ref, top, empty, paused);
    a.bind_text_label(empty);
    // Old's active receiver may have changed while its captured queue shrank.
    let finished = a.create_text_label();
    load_rt(a, R::V0, super::RT_M_OBJ);
    super::fields::young_offset(a, R::V0, R::V2, finished);
    y::test(a, R::V2, R::V6, R::V7);
    a.jcc_label(C::NotEqual, finished);
    a.call_label(mark_ref);
    a.jmp_label(top);
    a.bind_text_label(finished);
    a.mov_imm64(R::V0, 1);
    a.ret();
    a.bind_text_label(paused);
    a.mov_imm64(R::V0, 0);
    a.ret();
}

/// One unit per root/retained Old job/remembered slot, including nulls.
fn discover<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    reference: E::TextLabel,
    top: E::TextLabel,
    empty: E::TextLabel,
    paused: E::TextLabel,
) {
    let old = a.create_text_label();
    let spill = a.create_text_label();
    let remset = a.create_text_label();
    let next_root = a.create_text_label();
    super::registered_roots::prepare(a, l, super::registered_roots::YOUNG, top, old, paused);
    super::registered_roots::capture(a, l, super::registered_roots::YOUNG);
    visit(a, reference, next_root);
    a.bind_text_label(next_root);
    super::registered_roots::acknowledge(a, l, super::registered_roots::YOUNG);
    a.jmp_label(top);
    a.bind_text_label(old);
    super::registered_roots::finish(a, l, super::registered_roots::YOUNG);
    load_rt(a, R::V0, y::OLD_INDEX);
    load_rt(a, R::V1, y::OLD_BOUND);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, spill);
    super::scan::spend(a, paused);
    let next_old = a.create_text_label();
    super::mark_frontier::old_slot(a, R::V0, R::V10);
    a.load_ptr_disp32(R::V0, R::V10, 0);
    // The Young mark helper may grow/replace Old queue backing while making
    // progress. Keep only the logical index and observed word across it.
    super::fields::young_offset(a, R::V0, R::V2, next_old);
    load_rt(a, R::V10, y::OLD_INDEX);
    a.push_reg(R::V10);
    a.push_reg(R::V0);
    a.call_label(reference);
    a.pop_reg(R::V0);
    a.pop_reg(R::V10);
    super::mark_frontier::old_slot(a, R::V10, R::V10);
    super::young_barrier::heal(a);
    load_rt(a, R::V1, y::OLD_INDEX);
    a.test_reg_reg(R::V1, R::V1);
    let mirrored = a.create_text_label();
    a.jcc_label(C::Equal, mirrored);
    super::mark_queue::mirror_address(a, false, R::V10);
    a.bind_text_label(mirrored);
    a.bind_text_label(next_old);
    super::copy::bump(a, y::OLD_INDEX, R::V0);
    a.jmp_label(top);
    a.bind_text_label(spill);
    discover_spill(a, l, reference, top, remset, paused);
    a.bind_text_label(remset);
    load_rt(a, R::V0, y::REMSET);
    load_rt(a, R::V1, y::REMSET_BOUND);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, empty);
    super::scan::spend(a, paused);
    a.shl_reg_imm8(R::V0, 3);
    load_rt(a, R::V10, y::REMSET_BASE);
    a.add_reg_reg(R::V10, R::V0);
    a.load_ptr_disp32(R::V10, R::V10, 0);
    let next_slot = a.create_text_label();
    a.push_reg(R::V7);
    a.push_reg(R::V7);
    a.mov_reg_reg(R::V7, R::V10);
    a.call_label(l.remembered_owner);
    a.pop_reg(R::V7);
    a.pop_reg(R::V7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, next_slot);
    a.atomic_load_acquire(R::V0, R::V10);
    visit(a, reference, next_slot);
    a.bind_text_label(next_slot);
    super::copy::bump(a, y::REMSET, R::V0);
    a.jmp_label(top);
}

/// Young start retains the existing Old spill prefix. Old's normal stack may
/// advance, but these nodes cannot pop/reuse until its captured tail is visited.
/// Each node already owns a native page holder, independent of token capacity.
fn discover_spill<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    reference: E::TextLabel,
    top: E::TextLabel,
    empty: E::TextLabel,
    paused: E::TextLabel,
) {
    use super::mark_queue::{self as q, SpillNode};
    load_rt(a, R::V0, q::discovery_frozen());
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    super::scan::spend(a, paused);
    load_rt(a, R::V8, q::discovery_current());
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, l.fail);
    q::node(a, false, R::V8, R::V10);
    a.load_ptr_disp32(R::V1, R::V10, super::d(offset_of!(SpillNode, state)));
    a.cmp_reg_imm32(R::V1, 1);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V1, R::V10, super::d(offset_of!(SpillNode, record)));
    a.cmp_reg_reg(R::V1, R::V8);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V1, R::V10, super::d(offset_of!(SpillNode, epoch)));
    load_rt(a, R::V0, super::RT_MARK_CYCLES);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, R::V10, super::d(offset_of!(SpillNode, reference)));
    let next = a.create_text_label();
    super::fields::young_offset(a, R::V0, R::V2, next);
    a.push_reg(R::V8);
    a.push_reg(R::V0);
    a.call_label(reference);
    a.pop_reg(R::V0);
    a.pop_reg(R::V8);
    q::node(a, false, R::V8, R::V10);
    a.add_reg_imm32(R::V10, super::d(offset_of!(SpillNode, reference)));
    super::young_barrier::heal(a);
    a.bind_text_label(next);
    // Reference admission may change queue backing. The retained node identity
    // is resolved again; no native node/slot address crossed the call.
    load_rt(a, R::V8, q::discovery_current());
    load_rt(a, R::V1, q::discovery_tail());
    a.cmp_reg_reg(R::V8, R::V1);
    let finished = a.create_text_label();
    a.jcc_label(C::Equal, finished);
    q::node(a, false, R::V8, R::V10);
    a.load_ptr_disp32(R::V0, R::V10, super::d(offset_of!(SpillNode, next)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    store_rt(a, q::discovery_current(), R::V0);
    a.jmp_label(top);
    a.bind_text_label(finished);
    for field in [
        q::discovery_current(),
        q::discovery_tail(),
        q::discovery_frozen(),
    ] {
        super::store_rt_imm(a, field, 0);
    }
    a.jmp_label(top);
}
fn visit<E: PortableAsm>(a: &mut E, reference: E::TextLabel, next: E::TextLabel) {
    super::fields::young_offset(a, R::V0, R::V2, next);
    a.push_reg(R::V10);
    a.push_reg(R::V0);
    a.call_label(reference);
    a.pop_reg(R::V0);
    a.pop_reg(R::V10);
    super::young_barrier::heal(a);
}
