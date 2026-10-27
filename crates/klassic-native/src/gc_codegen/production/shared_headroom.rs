//! Preflight normal Young copying against retained nonmoving Large spans.
//! The quiescent owner holds the collector lock; Old snapshot closure and
//! collection selection call the same predicate before changing any roles.
use super::*;
use crate::gc_codegen::production::{lea_rt, load_rt};

pub(super) fn must_full<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, full: E::TextLabel) {
    let done = a.create_text_label();
    load_rt(a, R::V0, large_object::LIVE_PAGES);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    capacity::normal_promotion_pages(a, l);
    a.mov_reg_reg(R::V11, R::V0);
    // Promotion must fit the current Old destination even when its bound
    // exceeds a whole Young window. Do not apply the Young-window cap here.
    old_runs::used(a, R::V0);
    load_rt(a, R::V1, pool::OLD_PAGES);
    a.shl_reg_imm8(R::V1, 17);
    a.sub_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, full);
    a.shr_reg_imm8(R::V1, 17);
    a.cmp_reg_reg(R::V1, R::V11);
    a.jcc_label(C::Below, full);
    old_runs::unused_pages(a, l, R::V6);
    for r in [R::V7, R::V8, R::V10] {
        a.mov_imm64(r, 0);
    }
    lea_rt(a, R::V9, (layout(a).generations + 8) as usize);
    let next = a.create_text_label();
    let occupied = a.create_text_label();
    let advance = a.create_text_label();
    let counted = a.create_text_label();
    a.bind_text_label(next);
    a.cmp_reg_imm32(R::V8, geometry(a).pages as i32);
    a.jcc_label(C::AboveOrEqual, counted);
    a.atomic_load_acquire(R::V0, R::V9);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, occupied);
    a.add_reg_imm32(R::V6, 1);
    a.add_reg_imm32(R::V10, 1);
    a.cmp_reg_reg(R::V10, R::V7);
    a.jcc_label(C::Below, advance);
    a.mov_reg_reg(R::V7, R::V10);
    a.jmp_label(advance);
    a.bind_text_label(occupied);
    a.mov_imm64(R::V10, 0);
    a.bind_text_label(advance);
    a.add_reg_imm32(R::V8, 1);
    a.add_reg_imm32(R::V9, 8);
    a.jmp_label(next);
    a.bind_text_label(counted);
    capacity::young_destination(a, l);
    a.cmp_reg_reg(R::V7, R::V2);
    a.jcc_label(C::Below, full);
    // After a Young flip: its source returns, the new window remains, and
    // worst-case promotion consumes returnable Old tail capacity. Retain
    // the next maximum Young/Old destinations beyond that entire charge.
    load_rt(a, R::V0, capacity::ACTIVE);
    a.add_reg_reg(R::V6, R::V0);
    // destination_pages uses V2 as scratch; retain the next Young request.
    a.push_reg(R::V2);
    a.push_reg(R::V2);
    old_runs::destination_pages(a, l);
    let floor_met = a.create_text_label();
    a.cmp_reg_imm32(R::V0, (geometry(a).old_bytes / HEAP_PAGE_BYTES) as i32);
    a.jcc_label(C::AboveOrEqual, floor_met);
    a.mov_imm64(R::V0, geometry(a).old_bytes / HEAP_PAGE_BYTES);
    a.bind_text_label(floor_met);
    a.add_reg_imm32(R::V0, geometry(a).young_pages as i32);
    a.pop_reg(R::V2);
    a.pop_reg(R::V2);
    a.add_reg_reg(R::V2, R::V0);
    a.add_reg_reg(R::V2, R::V11);
    a.cmp_reg_reg(R::V6, R::V2);
    a.jcc_label(C::Below, full);
    a.bind_text_label(done);
}
