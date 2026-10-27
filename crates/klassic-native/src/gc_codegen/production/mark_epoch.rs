//! Logical mark membership belongs to the object, not its heap address.
//! Old snapshots use old_epoch. Young relocation does not advance that serial
//! or write young_epoch. Both stamps survive copies; zero is unmarked.
use super::{RT_HEAP_BASE, RT_MARK_CYCLES, load_rt};
use crate::gc_abi::{MarkRecord, ObjectHeader};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};
const RECORD: i32 = offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32;
const EPOCH: i32 = offset_of!(MarkRecord, old_epoch) as i32;

/// NotEqual means current membership. Preserve off/V0; change epoch, record, V1.
pub(super) fn test<E: PortableAsm>(a: &mut E, off: R, epoch: R, record: R) {
    let no = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, epoch, RT_MARK_CYCLES);
    a.test_reg_reg(epoch, epoch);
    a.jcc_label(C::Equal, no);
    load_rt(a, record, RT_HEAP_BASE);
    a.add_reg_reg(record, off);
    a.load_ptr_disp32(record, record, RECORD);
    a.test_reg_reg(record, record);
    a.jcc_label(C::Equal, no);
    a.add_reg_imm32(record, EPOCH);
    a.atomic_load_acquire(record, record);
    a.cmp_reg_reg(record, epoch);
    a.jcc_label(C::NotEqual, no);
    a.mov_imm64(record, 1);
    a.jmp_label(done);
    a.bind_text_label(no);
    a.mov_imm64(record, 0);
    a.bind_text_label(done);
    a.test_reg_reg(record, record);
}

/// Called under the collector lock for admission, or on a private new object
/// before any strong publication for black allocation. Preserve off and V0.
pub(super) fn set<E: PortableAsm>(a: &mut E, off: R, record: R, epoch: R) {
    load_rt(a, record, RT_HEAP_BASE);
    a.add_reg_reg(record, off);
    a.load_ptr_disp32(record, record, RECORD);
    a.add_reg_imm32(record, EPOCH);
    load_rt(a, epoch, RT_MARK_CYCLES);
    a.atomic_store_release(record, epoch);
}
