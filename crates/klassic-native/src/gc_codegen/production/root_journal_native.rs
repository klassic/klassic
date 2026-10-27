//! A foreign producer may be skipped only while genuinely BlockedNative and
//! its registered journal is already empty. The inventory's independent reader
//! lease protects this node across slices. No foreign entry is claimed/dropped,
//! and no foreign cache, cursor or ACK is written. Running producers still fail.
use super::*;
use crate::gc_abi::BufferCursor;

/// V0 = authenticated registration, V1 = its context. Claim serial/end were
/// checked by the common entry; the caller holds the actual collector ticket.
pub(super) fn empty_excluded<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.push_reg(R::V0);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, l.fail);
    a.mov_reg_reg(R::V0, R::V1);
    a.and_reg_imm32(R::V0, 7);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_reg_reg(R::V2, R::V1);
    a.atomic_load_acquire(R::V1, R::V1);
    a.mov_imm64(R::V0, u64::from(ABI_VERSION) | (2 << 32));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    for f in [
        offset_of!(MutatorContext, private_producers),
        offset_of!(MutatorContext, access_epoch),
        offset_of!(MutatorContext, tlab_start),
        offset_of!(MutatorContext, tlab_top),
        offset_of!(MutatorContext, tlab_end),
    ] {
        a.load_ptr_disp32(R::V0, R::V2, d(f));
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    a.pop_reg(R::V0);
    a.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootRegistration, segment)));
    a.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootSegment, journal)));
    let empty = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    a.mov_reg_reg(R::V2, R::V0);
    a.add_reg_imm32(R::V2, d(offset_of!(BufferCursor, published)));
    a.atomic_load_acquire(R::V1, R::V2);
    a.add_reg_imm32(R::V0, d(offset_of!(BufferCursor, consumed)));
    a.atomic_load_acquire(R::V0, R::V0);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(empty);
}
