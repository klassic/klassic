//! Fresh native root storage initialization, before registration/publication.
//! Same disjoint zeroed-mapping contract as gc_root_storage.h. No managed loads.
use crate::gc_abi::{BufferCursor, RootRegistration, RootSegment, RootStorage, RootStorageSpec};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

#[allow(dead_code)]
pub(crate) fn emit_prepare<E: PortableAsm>(a: &mut E) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let cells = a.create_text_label();
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V7);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V6, offset_of!(RootStorageSpec, owner) as i32);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, invalid);
    a.mov_imm64(R::V0, u32::MAX as u64);
    for (r, offset) in [
        (R::V2, offset_of!(RootStorageSpec, cells)),
        (R::V8, offset_of!(RootStorageSpec, entries)),
    ] {
        a.load_ptr_disp32(r, R::V6, offset as i32);
        a.test_reg_reg(r, r);
        a.jcc_label(C::Equal, invalid);
        a.cmp_reg_reg(r, R::V0);
        a.jcc_label(C::Above, invalid);
    }
    a.load_ptr_disp32(R::V9, R::V6, offset_of!(RootStorageSpec, bytes) as i32);
    a.mov_reg_reg(R::V0, R::V2);
    a.add_reg_reg(R::V0, R::V8);
    a.shl_reg_imm8(R::V0, 5); // RootCell and RootChange are each 32 bytes.
    a.add_reg_imm32(R::V0, size_of::<RootStorage>() as i32);
    a.cmp_reg_reg(R::V9, R::V0);
    a.jcc_label(C::Below, invalid);
    a.mov_reg_reg(R::V10, R::V7);
    a.add_reg_reg(R::V10, R::V9);
    a.cmp_reg_reg(R::V10, R::V7);
    a.jcc_label(C::Below, invalid);
    let segment = offset_of!(RootStorage, segment);
    let journal = offset_of!(RootStorage, journal);
    a.mov_reg_reg(R::V0, R::V7);
    a.add_reg_imm32(R::V0, segment as i32);
    a.store_ptr_disp32(R::V7, offset_of!(RootRegistration, segment) as i32, R::V0);
    a.store_ptr_disp32(
        R::V7,
        (segment + offset_of!(RootSegment, owner)) as i32,
        R::V1,
    );
    a.store_ptr_disp32(
        R::V7,
        (segment + offset_of!(RootSegment, capacity)) as i32,
        R::V2,
    );
    a.mov_reg_reg(R::V10, R::V7);
    a.add_reg_imm32(R::V10, size_of::<RootStorage>() as i32);
    a.store_ptr_disp32(
        R::V7,
        (segment + offset_of!(RootSegment, cells)) as i32,
        R::V10,
    );
    a.mov_reg_reg(R::V0, R::V7);
    a.add_reg_imm32(R::V0, journal as i32);
    a.store_ptr_disp32(
        R::V7,
        (segment + offset_of!(RootSegment, journal)) as i32,
        R::V0,
    );
    a.store_ptr_disp32(
        R::V7,
        (journal + offset_of!(BufferCursor, capacity)) as i32,
        R::V8,
    );
    a.mov_reg_reg(R::V8, R::V2);
    a.shl_reg_imm8(R::V8, 5);
    a.add_reg_reg(R::V8, R::V10);
    a.store_ptr_disp32(
        R::V7,
        (journal + offset_of!(BufferCursor, entries)) as i32,
        R::V8,
    );
    a.mov_imm64(R::V0, 0);
    for offset in [
        offset_of!(RootRegistration, cleanup_cursor),
        segment + offset_of!(RootSegment, used),
        segment + offset_of!(RootSegment, state),
        journal + offset_of!(BufferCursor, published),
        journal + offset_of!(BufferCursor, consumed),
    ] {
        a.store_ptr_disp32(R::V7, offset as i32, R::V0);
    }
    a.bind_text_label(cells);
    a.store_ptr_disp32(R::V10, 0, R::V0);
    a.add_reg_imm32(R::V10, 32);
    a.cmp_reg_reg(R::V10, R::V8);
    a.jcc_label(C::Below, cells);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 1);
    a.bind_text_label(done);
}

/// Exclusive backing owner, with no concurrent attachment. Read-only fixed
/// prefix check; returns OK/INVALID 0/1 before the owner attempts OS disposal.
#[allow(dead_code)]
pub(crate) fn emit_can_dispose<E: PortableAsm>(a: &mut E) {
    let invalid = a.create_text_label();
    let segment_ok = a.create_text_label();
    let done = a.create_text_label();
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V7);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    let segment = offset_of!(RootStorage, segment);
    let journal = offset_of!(RootStorage, journal);
    // FINISH publishes PREPARED last. Until acquired, registration words may
    // still be mutated by the exclusive cleaner on another thread.
    a.mov_reg_reg(R::V1, R::V7);
    a.add_reg_imm32(R::V1, (segment + offset_of!(RootSegment, state)) as i32);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V7, offset_of!(RootRegistration, segment) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, segment_ok);
    a.mov_reg_reg(R::V1, R::V7);
    a.add_reg_imm32(R::V1, segment as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(segment_ok);
    // FINISH resets every registration word except the segment address.
    for offset in (8..size_of::<RootRegistration>()).step_by(8) {
        if offset == offset_of!(RootRegistration, cleanup_cursor) {
            a.mov_reg_reg(R::V1, R::V7);
            a.add_reg_imm32(R::V1, offset as i32);
            a.atomic_load_acquire(R::V0, R::V1);
        } else {
            a.load_ptr_disp32(R::V0, R::V7, offset as i32);
        }
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    for (offset, atomic) in [
        (offset_of!(RootSegment, scope), false),
        (offset_of!(RootSegment, registration), false),
        (offset_of!(RootSegment, used), true),
    ] {
        if atomic {
            a.mov_reg_reg(R::V1, R::V7);
            a.add_reg_imm32(R::V1, (segment + offset) as i32);
            a.atomic_load_acquire(R::V0, R::V1);
        } else {
            a.load_ptr_disp32(R::V0, R::V7, (segment + offset) as i32);
        }
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    for (r, offset) in [
        (R::V8, offset_of!(BufferCursor, consumed)),
        (R::V9, offset_of!(BufferCursor, published)),
    ] {
        a.mov_reg_reg(R::V1, R::V7);
        a.add_reg_imm32(R::V1, (journal + offset) as i32);
        a.atomic_load_acquire(r, R::V1);
    }
    a.cmp_reg_reg(R::V8, R::V9);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(
        R::V0,
        R::V7,
        (journal + offset_of!(BufferCursor, top)) as i32,
    );
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, invalid);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 1);
    a.bind_text_label(done);
}
