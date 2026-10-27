//! Admit a nonmoving span without consuming the next moving destinations.
//! The owner has completed retirement and
//! holds the collector lock. This is a physical capacity/shape preflight,
//! not a prediction of live bytes or a general N-owner reservation protocol.
use super::*;
use crate::gc_codegen::production::{lea_rt, load_rt, store_rt};

/// Fold a complete free run: V2 length, V10 start. V11/V1 hold the largest
/// run and its start; the second-largest length is private owner scratch.
fn consider<E: PortableAsm>(a: &mut E) {
    let lesser = a.create_text_label();
    let done = a.create_text_label();
    a.cmp_reg_reg(R::V2, R::V11);
    a.jcc_label(C::Below, lesser);
    a.jcc_label(C::Equal, lesser);
    store_rt(a, large_object::RESERVE_SECOND_RUN, R::V11);
    a.mov_reg_reg(R::V11, R::V2);
    a.mov_reg_reg(R::V1, R::V10);
    a.jmp_label(done);
    a.bind_text_label(lesser);
    load_rt(a, R::V0, large_object::RESERVE_SECOND_RUN);
    a.cmp_reg_reg(R::V2, R::V0);
    a.jcc_label(C::Below, done);
    a.jcc_label(C::Equal, done);
    store_rt(a, large_object::RESERVE_SECOND_RUN, R::V2);
    a.bind_text_label(done);
}

/// V7=requested pages -> V0=zero-based first page. On failure no page role
/// changes. Keep a maximum Young contiguous run and aggregate maximum Old
/// destination capacity; Old destinations may use disjoint free runs.
pub(super) fn claim_or<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, failure: E::TextLabel) {
    let search = a.create_text_label();
    let occupied = a.create_text_label();
    let advance = a.create_text_label();
    let checked = a.create_text_label();
    let found = a.create_text_label();
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, l.fail);
    store_rt_imm(a, large_object::RESERVE_SECOND_RUN, 0);
    // A Full detach returns unused Old tails before claiming destinations.
    // Count those pages, without changing current promotion reservations.
    old_runs::unused_pages(a, l, R::V6);
    for r in [R::V1, R::V2, R::V8, R::V10, R::V11] {
        a.mov_imm64(r, 0);
    }
    lea_rt(a, R::V9, (layout(a).generations + 8) as usize);
    a.bind_text_label(search);
    a.cmp_reg_imm32(R::V8, geometry(a).pages as i32);
    a.jcc_label(C::AboveOrEqual, checked);
    a.atomic_load_acquire(R::V0, R::V9);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, occupied);
    let started = a.create_text_label();
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, started);
    a.mov_reg_reg(R::V10, R::V8);
    a.bind_text_label(started);
    a.add_reg_imm32(R::V2, 1);
    a.add_reg_imm32(R::V6, 1); // free now + pages returned by Full detach
    a.jmp_label(advance);
    a.bind_text_label(occupied);
    consider(a);
    a.mov_imm64(R::V2, 0);
    a.bind_text_label(advance);
    a.add_reg_imm32(R::V8, 1);
    a.add_reg_imm32(R::V9, 8);
    a.jmp_label(search);
    a.bind_text_label(checked);
    consider(a);
    // Large is taken from an edge of the largest free run. An independent
    // Young-sized run, or its remainder, must survive this choice.
    a.cmp_reg_reg(R::V11, R::V7);
    a.jcc_label(C::Below, failure);
    a.sub_reg_reg(R::V6, R::V7);
    // Keep the largest run's start across the shared Full demand calculation.
    a.push_reg(R::V1);
    a.push_reg(R::V1);
    old_runs::destination_pages(a, l);
    let floor_met = a.create_text_label();
    a.cmp_reg_imm32(R::V0, (geometry(a).old_bytes / HEAP_PAGE_BYTES) as i32);
    a.jcc_label(C::AboveOrEqual, floor_met);
    a.mov_imm64(R::V0, geometry(a).old_bytes / HEAP_PAGE_BYTES);
    a.bind_text_label(floor_met);
    a.add_reg_imm32(R::V0, geometry(a).young_pages as i32);
    a.pop_reg(R::V1);
    a.pop_reg(R::V1);
    a.cmp_reg_reg(R::V6, R::V0);
    a.jcc_label(C::Below, failure);
    a.sub_reg_reg(R::V11, R::V7);
    a.cmp_reg_imm32(R::V11, geometry(a).young_pages as i32);
    a.jcc_label(C::AboveOrEqual, found);
    load_rt(a, R::V0, large_object::RESERVE_SECOND_RUN);
    a.cmp_reg_imm32(R::V0, geometry(a).young_pages as i32);
    a.jcc_label(C::Below, failure);
    a.bind_text_label(found);
    a.mov_reg_reg(R::V2, R::V7);
    a.mov_imm64(R::V6, generation::LARGE);
    pool::publish(a, l);
}
