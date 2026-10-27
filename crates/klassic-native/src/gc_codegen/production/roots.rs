//! M-G1 frame roots (LIFO RootCells) and the `--gc-log` heap walk.
use super::{
    HANDLE_CELL, HANDLE_GENERATION, HANDLE_SCOPE, Labels, REQUEST_VALUE, ROOT_READ_RAW,
    ROOT_RESERVE, ROOT_STORE, RT_ADOPT_SLOT, RT_CELL_SP, RT_COPY_LOW, RT_FRAME_ROOT, RT_HEAP_BASE,
    RT_NEXT_PAGE, RT_OBJECTS, RT_PAGES_USED, RT_SEGMENT, RT_SPACE_BASE, RT_SPACE_END, RT_VERIFIED,
    d, decode, encode, load_rt, load_runtime, root_step, store_rt,
};
use crate::gc_abi::{AllocationPage, OBJECT_PUBLISHED, ObjectHeader, RootCell, RootSegment};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

/// Fill `frame_root.handle` for the cell pointer in `cell`.
pub(super) fn frame_handle<E: PortableAsm>(a: &mut E, cell: R, scratch: R) {
    store_rt(a, RT_FRAME_ROOT + HANDLE_CELL, cell);
    a.load_ptr_disp32(scratch, cell, d(offset_of!(RootCell, reuse_generation)));
    store_rt(a, RT_FRAME_ROOT + HANDLE_GENERATION, scratch);
    load_rt(a, scratch, RT_SEGMENT);
    a.load_ptr_disp32(scratch, scratch, d(offset_of!(RootSegment, scope)));
    store_rt(a, RT_FRAME_ROOT + HANDLE_SCOPE, scratch);
}

const SAVED: [R; 11] = [
    R::OwnerContext,
    R::V0,
    R::V1,
    R::V2,
    R::V6,
    R::V7,
    R::V8,
    R::V9,
    R::V10,
    R::V11,
    R::V3,
];

fn save_all<E: PortableAsm>(a: &mut E, skip_v0: bool) {
    for reg in SAVED {
        if !(skip_v0 && reg == R::V0) {
            a.push_reg(reg);
        }
    }
}

fn restore_all<E: PortableAsm>(a: &mut E, skip_v0: bool) {
    for reg in SAVED.iter().rev() {
        if !(skip_v0 && *reg == R::V0) {
            a.pop_reg(*reg);
        }
    }
}

/// Raw referent in V0 -> resolved reference bits. A late root publication
/// must not hide a from-space address behind the newly adopted color.
fn encode_resolved<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    encode(a, R::V0, R::V1);
    a.mov_imm64(R::V2, 1);
    store_rt(a, super::RT_COLLECTING, R::V2);
    a.call_label(l.forward);
    a.mov_imm64(R::V2, 0);
    store_rt(a, super::RT_COLLECTING, R::V2);
}

pub(super) fn emit_roots<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
    // root_adopt: V0 = slot address; slot holds a payload address or null.
    let root_adopt_body = a.create_text_label();
    a.bind_text_label(l.root_adopt);
    save_all(a, false);
    load_runtime(a, runtime);
    a.jmp_label(root_adopt_body);
    a.bind_text_label(l.root_adopt_actor);
    save_all(a, false);
    super::owner_state::bind_running(a, l);
    a.bind_text_label(root_adopt_body);
    store_rt(a, RT_ADOPT_SLOT, R::V0);
    a.load_ptr_disp32(R::V0, R::V0, 0);
    encode_resolved(a, l);
    store_rt(a, RT_FRAME_ROOT + REQUEST_VALUE, R::V0);
    // Frame cell k (k >= 1) is reused once reserved: RESERVE never hands a
    // cell out twice, so LIFO reuse keeps loops from exhausting the segment.
    let reuse = a.create_text_label();
    let have = a.create_text_label();
    load_rt(a, R::V0, RT_CELL_SP);
    a.add_reg_imm32(R::V0, 1);
    load_rt(a, R::V2, RT_SEGMENT);
    a.load_ptr_disp32(R::V1, R::V2, d(offset_of!(RootSegment, used)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, reuse);
    root_step(a, l, RT_FRAME_ROOT, ROOT_RESERVE);
    a.jmp_label(have);
    a.bind_text_label(reuse);
    a.load_ptr_disp32(R::V1, R::V2, d(offset_of!(RootSegment, cells)));
    a.shl_reg_imm8(R::V0, 5);
    a.add_reg_reg(R::V1, R::V0);
    frame_handle(a, R::V1, R::V2);
    a.bind_text_label(have);
    root_step(a, l, RT_FRAME_ROOT, ROOT_STORE);
    load_rt(a, R::V0, RT_CELL_SP);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, RT_CELL_SP, R::V0);
    load_rt(a, R::V0, RT_ADOPT_SLOT);
    load_rt(a, R::V1, RT_FRAME_ROOT + HANDLE_CELL);
    a.store_ptr_disp32(R::V0, 0, R::V1);
    restore_all(a, false);
    a.ret();

    // root_pop: clear the top frame cell (cells[cell_sp]) and pop it.
    let root_pop_body = a.create_text_label();
    a.bind_text_label(l.root_pop);
    save_all(a, false);
    load_runtime(a, runtime);
    a.jmp_label(root_pop_body);
    a.bind_text_label(l.root_pop_actor);
    save_all(a, false);
    super::owner_state::bind_running(a, l);
    a.bind_text_label(root_pop_body);
    load_rt(a, R::V0, RT_CELL_SP);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V2, RT_SEGMENT);
    a.load_ptr_disp32(R::V1, R::V2, d(offset_of!(RootSegment, cells)));
    a.shl_reg_imm8(R::V0, 5);
    a.add_reg_reg(R::V1, R::V0);
    frame_handle(a, R::V1, R::V2);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_FRAME_ROOT + REQUEST_VALUE, R::V0);
    root_step(a, l, RT_FRAME_ROOT, ROOT_STORE);
    load_rt(a, R::V0, RT_CELL_SP);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, RT_CELL_SP, R::V0);
    restore_all(a, false);
    a.ret();

    // root_store: V0 = payload address or null, V1 = slot address.
    let root_store_body = a.create_text_label();
    a.bind_text_label(l.root_store);
    save_all(a, false);
    load_runtime(a, runtime);
    a.jmp_label(root_store_body);
    a.bind_text_label(l.root_store_actor);
    save_all(a, false);
    super::owner_state::bind_running(a, l);
    a.bind_text_label(root_store_body);
    a.load_ptr_disp32(R::V1, R::V1, 0);
    frame_handle(a, R::V1, R::V2);
    encode_resolved(a, l);
    store_rt(a, RT_FRAME_ROOT + REQUEST_VALUE, R::V0);
    root_step(a, l, RT_FRAME_ROOT, ROOT_STORE);
    restore_all(a, false);
    a.ret();

    // root_load: V0 = cell pointer -> V0 = payload address or null. The
    // range check runs before the handle is built, so a frame slot that
    // still holds a raw address (a store that bypassed root_store) traps
    // here instead of being read as a cell.
    let root_load_body = a.create_text_label();
    a.bind_text_label(l.root_load);
    save_all(a, true);
    load_runtime(a, runtime);
    a.jmp_label(root_load_body);
    a.bind_text_label(l.root_load_actor);
    save_all(a, true);
    super::owner_state::bind_running(a, l);
    a.bind_text_label(root_load_body);
    load_rt(a, R::V2, RT_SEGMENT);
    a.load_ptr_disp32(R::V1, R::V2, d(offset_of!(RootSegment, cells)));
    a.mov_reg_reg(R::V6, R::V0);
    a.sub_reg_reg(R::V6, R::V1);
    a.jcc_label(C::Below, l.fail);
    a.mov_reg_reg(R::V7, R::V6);
    a.and_reg_imm32(R::V7, 31);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V7, R::V2, d(offset_of!(RootSegment, used)));
    a.shl_reg_imm8(R::V7, 5);
    a.cmp_reg_reg(R::V6, R::V7);
    a.jcc_label(C::AboveOrEqual, l.fail);
    frame_handle(a, R::V0, R::V2);
    root_step(a, l, RT_FRAME_ROOT, ROOT_READ_RAW);
    load_rt(a, R::V0, RT_FRAME_ROOT + REQUEST_VALUE);
    // The root access guard is closed before the barrier can wait on the
    // collector lock. The permanent segment retains this cell throughout.
    let external = a.create_text_label();
    let decoded = a.create_text_label();
    a.mov_reg_reg(R::V1, R::V0);
    a.and_reg_imm32(R::V1, super::EXTERNAL);
    a.jcc_label(C::NotEqual, external);
    load_rt(a, R::V10, RT_FRAME_ROOT + HANDLE_CELL);
    a.call_label(l.field_load_actor);
    a.jmp_label(decoded);
    a.bind_text_label(external);
    decode(a, R::V0, R::V1);
    a.bind_text_label(decoded);
    restore_all(a, true);
    a.ret();
}

/// Walk pages [0, next_page) object by object: count objects and headers
/// that carry the raw-bytes descriptor in the PUBLISHED state.
pub(super) fn emit_verify<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
    a.bind_text_label(l.verify);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    load_runtime(a, runtime);
    super::worker::lock(a, l);
    a.call_label(l.finish);
    a.call_label(l.publish_pending);
    super::old_runs::used(a, R::V0);
    store_rt(a, super::RT_OLD_CHARGED_BYTES, R::V0);
    let pages = a.create_text_label();
    let objects = a.create_text_label();
    let next_page = a.create_text_label();
    let unpublished = a.create_text_label();
    let done = a.create_text_label();
    // Walk the current semispace: the TLAB's pages [space_base, next_page)
    // and the worker's pages [copy_low, space_end).
    load_rt(a, R::V9, RT_SPACE_BASE); // page index
    load_rt(a, R::V0, RT_NEXT_PAGE);
    a.sub_reg_reg(R::V0, R::V9);
    load_rt(a, R::V1, RT_SPACE_END);
    a.add_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, RT_COPY_LOW);
    a.sub_reg_reg(R::V0, R::V1);
    store_rt(a, RT_PAGES_USED, R::V0);
    a.mov_imm64(R::V10, 0); // objects
    a.mov_imm64(R::V11, 0); // published
    let walk = a.create_text_label();
    let locate = a.create_text_label();
    a.bind_text_label(locate);
    super::scan::page_ptr(a, R::V8, R::V9, R::V0);
    a.bind_text_label(pages);
    load_rt(a, R::V0, RT_SPACE_END);
    a.cmp_reg_reg(R::V9, R::V0);
    a.jcc_label(C::AboveOrEqual, done);
    load_rt(a, R::V0, RT_NEXT_PAGE);
    a.cmp_reg_reg(R::V9, R::V0);
    a.jcc_label(C::Below, walk);
    load_rt(a, R::V0, RT_COPY_LOW);
    a.cmp_reg_reg(R::V9, R::V0);
    a.jcc_label(C::AboveOrEqual, walk);
    a.mov_reg_reg(R::V9, R::V0);
    a.jmp_label(locate);
    a.bind_text_label(walk);
    a.load_ptr_disp32(R::V6, R::V8, d(offset_of!(AllocationPage, start)));
    a.load_ptr_disp32(R::V7, R::V8, d(offset_of!(AllocationPage, allocated_top)));
    a.bind_text_label(objects);
    a.cmp_reg_reg(R::V6, R::V7);
    a.jcc_label(C::AboveOrEqual, next_page);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.add_reg_reg(R::V1, R::V6);
    a.add_reg_imm32(R::V10, 1);
    a.load_ptr_disp32(R::V2, R::V1, d(offset_of!(ObjectHeader, descriptor_id)));
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, unpublished);
    a.load_ptr_disp32(R::V2, R::V1, d(offset_of!(ObjectHeader, state)));
    a.cmp_reg_imm8(R::V2, OBJECT_PUBLISHED as i8);
    a.jcc_label(C::NotEqual, unpublished);
    a.add_reg_imm32(R::V11, 1);
    a.bind_text_label(unpublished);
    // Next object: header + max(16, round_up(payload, 16)).
    let sized = a.create_text_label();
    a.load_ptr_disp32(R::V2, R::V1, d(offset_of!(ObjectHeader, payload_bytes)));
    a.add_reg_imm32(R::V2, 15);
    a.and_reg_imm32(R::V2, -16);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V2, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V2, d(size_of::<ObjectHeader>()));
    a.add_reg_reg(R::V6, R::V2);
    a.jmp_label(objects);
    a.bind_text_label(next_page);
    a.add_reg_imm32(R::V8, d(size_of::<AllocationPage>()));
    a.add_reg_imm32(R::V9, 1);
    a.jmp_label(pages);
    a.bind_text_label(done);
    store_rt(a, RT_OBJECTS, R::V10);
    store_rt(a, RT_VERIFIED, R::V11);
    super::commit::snapshot(a);
    super::large_pages::snapshot(a);
    super::metadata::snapshot(a);
    super::worker::unlock(a, l);
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();
}
