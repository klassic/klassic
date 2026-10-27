//! Conservative relocation reserve for fresh mutator allocations.
//!
//! Fresh objects may consume their current TLAB, but must not bind another
//! page while relocation still needs to-space. Otherwise garbage allocations
//! can fill the destination before its small live graph has finished copying.
//! Copies use alloc_raw and can still reserve pages. This is a conservative
//! legacy policy. Opt-in moving rounds use member-dependent copy bounds and
//! checked owner-local fresh reservations; general G10/G11 remains incomplete.
use super::{
    Labels, MAX_PAYLOAD, RT_ALLOCATION_STALLS, RT_CTX, RT_FRESH_SIZE, RT_PHASE_OPEN,
    copy::{HEADER, bump},
    load_rt,
};
use crate::gc_abi::MutatorContext;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

pub(super) fn wait_for_refill<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let legacy = a.create_text_label();
    let moving_done = a.create_text_label();
    load_rt(a, R::V0, super::moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, legacy);
    super::moving_round::admission::reserve(a, l);
    a.jmp_label(moving_done);
    a.bind_text_label(legacy);
    let ready = a.create_text_label();
    let rounded = a.create_text_label();
    let progress = a.create_text_label();
    let stall = a.create_text_label();
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, ready);
    load_rt(a, R::V0, RT_FRESH_SIZE);
    a.cmp_reg_imm32(R::V0, MAX_PAYLOAD as i32);
    a.jcc_label(C::Above, ready); // Let the allocator diagnose invalid sizes.
    a.add_reg_imm32(R::V0, 15);
    a.and_reg_imm32(R::V0, -16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, rounded);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(rounded);
    a.add_reg_imm32(R::V0, HEADER);
    load_rt(a, R::V2, super::capacity::FRESH_BUDGET);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Above, stall);
    load_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, tlab_top));
    a.add_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, tlab_end));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, ready);
    a.jcc_label(C::Equal, ready);
    a.bind_text_label(stall);
    bump(a, RT_ALLOCATION_STALLS, R::V0);
    // A stress collection may have opened this phase before any allocation
    // could wake the worker. The initial TLAB is reserved, while root work
    // remains part of the normal relocation frontier.
    super::worker::wake_if_pending(a, l);
    a.bind_text_label(progress);
    // The existing bounded quantum advances the mutator fallback, or waits
    // for the live worker and performs the normal finish only once it is done.
    a.call_label(l.quantum);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, ready);
    if let Some(w) = &l.worker {
        a.call_label(w.hooks.relax);
    }
    a.jmp_label(progress);
    a.bind_text_label(ready);
    a.bind_text_label(moving_done);
}
