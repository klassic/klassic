//! Persistent Chain-A frontiers for independently growing owner TLABs.
//! All scanner state is protected by the collector ticket. Object/header
//! readiness and actual root publication retain their release/acquire edges.
//! This does not grant N-owner moving or retirement permission.
use super::*;

#[repr(C)]
pub(super) struct State {
    epoch: u64,
    active: u64,
    page_opened: u64,
    pass_serial: u64,
    sealed_serial: u64,
    ready: u64,
}
const BASE: usize = offset_of!(Runtime, relocate_pages);
pub(super) const EPOCH: usize = BASE + offset_of!(State, epoch);
pub(super) const ACTIVE: usize = BASE + offset_of!(State, active);
pub(super) const PAGE_OPENED: usize = BASE + offset_of!(State, page_opened);
const PASS_SERIAL: usize = BASE + offset_of!(State, pass_serial);
const SEALED_SERIAL: usize = BASE + offset_of!(State, sealed_serial);
const READY: usize = BASE + offset_of!(State, ready);

pub(super) fn begin<E: PortableAsm>(a: &mut E, fail: E::TextLabel) {
    load_rt(a, R::V0, EPOCH);
    a.mov_imm64(R::V1, u64::MAX);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, fail);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, EPOCH, R::V0);
    for f in [ACTIVE, PAGE_OPENED, READY] {
        store_rt_imm(a, f, 0);
    }
}

pub(super) fn start_pass<E: PortableAsm>(a: &mut E) {
    store_rt_imm(a, ACTIVE, 0);
    store_rt_imm(a, READY, 0);
    lea_rt(a, R::V1, root_work::PUBLISHED);
    a.atomic_load_acquire(R::V0, R::V1);
    store_rt(a, PASS_SERIAL, R::V0);
    load_rt(a, R::V0, RT_SPACE_BASE);
    store_rt(a, RT_CS_PAGE, R::V0);
    store_rt_imm(a, PAGE_OPENED, 0);
}

/// V9 = side-table entry for the actual current page (epoch, cursor).
fn entry<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V9, RT_CS_PAGE);
    a.shl_reg_imm8(R::V9, 4);
    lea_rt(a, R::V0, layout(a).relocate_pages as usize);
    a.add_reg_reg(R::V9, R::V0);
}

/// V8 = actual AllocationPage. A new relocation lazily resets this page;
/// no whole page/owner walk is introduced at the phase edge.
pub(super) fn load_cursor<E: PortableAsm>(a: &mut E) {
    entry(a);
    load_rt(a, R::V1, EPOCH);
    a.load_ptr_disp32(R::V0, R::V9, 0);
    let have = a.create_text_label();
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, have);
    a.store_ptr_disp32(R::V9, 0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, start)));
    a.store_ptr_disp32(R::V9, 8, R::V0);
    a.bind_text_label(have);
    a.load_ptr_disp32(R::V0, R::V9, 8);
    store_rt(a, RT_CS_OFF, R::V0);
    store_rt_imm(a, PAGE_OPENED, 1);
}

pub(super) fn save_cursor<E: PortableAsm>(a: &mut E) {
    entry(a);
    load_rt(a, R::V0, RT_CS_OFF);
    a.store_ptr_disp32(R::V9, 8, R::V0);
}

/// A completed sweep is stale if *any* actual journal publication occurred
/// since its start. This includes alloc_into's slot/header publication. The
/// counter alone is not termination: all graph/root frontiers still close.
pub(super) fn seal_pass<E: PortableAsm>(a: &mut E, retry: E::TextLabel) {
    lea_rt(a, R::V1, root_work::PUBLISHED);
    a.atomic_load_acquire(R::V0, R::V1);
    load_rt(a, R::V1, PASS_SERIAL);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, retry);
    store_rt(a, SEALED_SERIAL, R::V0);
    store_rt_imm(a, ACTIVE, 0);
    store_rt_imm(a, READY, 1);
}

pub(super) fn pending<E: PortableAsm>(a: &mut E, pending: E::TextLabel) {
    load_rt(a, R::V0, READY);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, pending);
    lea_rt(a, R::V1, root_work::PUBLISHED);
    a.atomic_load_acquire(R::V0, R::V1);
    load_rt(a, R::V1, SEALED_SERIAL);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, pending);
}
