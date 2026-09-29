//! Immutable native descriptor publication and bounded field-offset scanning.
//! Same storage/lifetime contract as gc_descriptors.h; no managed heap loads.
use crate::gc_abi::{
    DescriptorRegistry as T, DescriptorScan as S, MAX_OFFSET, TypeDescriptor as D,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

fn registry<E: PortableAsm>(a: &mut E, invalid: E::TextLabel) {
    a.load_ptr_disp32(R::V8, R::V7, offset_of!(T, slots) as i32);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V8);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V9, R::V7, offset_of!(T, capacity) as i32);
    a.cmp_reg_imm32(R::V9, 2);
    a.jcc_label(C::Below, invalid);
    a.mov_imm64(R::V0, 0x1_0000_0000);
    a.cmp_reg_reg(R::V9, R::V0);
    a.jcc_label(C::Above, invalid);
    a.mov_reg_reg(R::V0, R::V9);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V8);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::Below, invalid);
    // Both ABI argument structures start with the descriptor ID.
    a.load_ptr_disp32(R::V0, R::V6, 0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::AboveOrEqual, invalid);
}

fn bitmap<E: PortableAsm>(
    a: &mut E,
    bytes: usize,
    pointer: usize,
    words: usize,
    invalid: E::TextLabel,
) {
    let scalar = a.create_text_label();
    let done = a.create_text_label();
    a.load_ptr_disp32(R::V8, R::V6, bytes as i32);
    a.mov_imm64(R::V0, MAX_OFFSET);
    a.cmp_reg_reg(R::V8, R::V0);
    a.jcc_label(C::Above, invalid);
    a.load_ptr_disp32(R::V9, R::V6, pointer as i32);
    a.load_ptr_disp32(R::V10, R::V6, words as i32);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, scalar);
    a.mov_reg_reg(R::V0, R::V9);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.test_reg_reg(R::V10, R::V10);
    a.jcc_label(C::Equal, invalid);
    a.shr_reg_imm8(R::V8, 3);
    a.mov_reg_reg(R::V0, R::V8);
    a.add_reg_imm32(R::V0, 63);
    a.shr_reg_imm8(R::V0, 6);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, invalid);
    a.shl_reg_imm8(R::V10, 3);
    a.add_reg_reg(R::V10, R::V9);
    a.cmp_reg_reg(R::V10, R::V9);
    a.jcc_label(C::Below, invalid);
    a.mov_reg_reg(R::V1, R::V8);
    a.and_reg_imm32(R::V1, 63);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, done);
    a.load_ptr_disp32(R::V0, R::V10, -8);
    a.shr_reg_count(R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.jmp_label(done);
    a.bind_text_label(scalar);
    a.test_reg_reg(R::V10, R::V10);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(done);
}

/// `(V7=registry, V6=descriptor) -> OK/INVALID/DUPLICATE 0/1/2`.
#[allow(dead_code)]
pub(crate) fn emit_register<E: PortableAsm>(a: &mut E) {
    let invalid = a.create_text_label();
    let publish = a.create_text_label();
    let duplicate = a.create_text_label();
    let done = a.create_text_label();
    registry(a, invalid);
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(D, alignment) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.cmp_reg_imm32(R::V0, 16);
    a.jcc_label(C::Above, invalid);
    a.mov_reg_reg(R::V1, R::V0);
    a.sub_reg_imm8(R::V1, 1);
    a.and_reg_reg(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    bitmap(
        a,
        offset_of!(D, fixed_bytes),
        offset_of!(D, fixed_bitmap),
        offset_of!(D, fixed_bitmap_words),
        invalid,
    );
    bitmap(
        a,
        offset_of!(D, element_bytes),
        offset_of!(D, element_bitmap),
        offset_of!(D, element_bitmap_words),
        invalid,
    );
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(D, element_bitmap) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, publish);
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(D, fixed_bytes) as i32);
    a.load_ptr_disp32(R::V1, R::V6, offset_of!(D, element_bytes) as i32);
    a.or_reg_reg(R::V0, R::V1);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(publish);
    a.load_ptr_disp32(R::V8, R::V7, offset_of!(T, slots) as i32);
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(D, id) as i32);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V8, R::V0);
    a.mov_imm64(R::V0, 0);
    a.atomic_compare_exchange(R::V8, R::V6);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, duplicate);
    a.jmp_label(done);
    a.bind_text_label(duplicate);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 1);
    a.bind_text_label(done);
}

/// `(V7=registry, V6=scan) -> DONE/MORE/INVALID 0/1/2`.
#[allow(dead_code)]
pub(crate) fn emit_scan<E: PortableAsm>(a: &mut E) {
    let invalid = a.create_text_label();
    let aligned = a.create_text_label();
    let fixed = a.create_text_label();
    let validated = a.create_text_label();
    let scan = a.create_text_label();
    let array = a.create_text_label();
    let skip_fixed = a.create_text_label();
    let skip_array = a.create_text_label();
    let candidate = a.create_text_label();
    let advance = a.create_text_label();
    let finish = a.create_text_label();
    let more = a.create_text_label();
    let done = a.create_text_label();
    // Scratch locals: output, capacity, budget, written, examined.
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 48);
    registry(a, invalid);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V8, R::V0);
    a.atomic_load_acquire(R::V8, R::V8);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(D, id) as i32);
    a.load_ptr_disp32(R::V1, R::V6, offset_of!(S, descriptor_id) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V10, R::V6, offset_of!(S, payload_bytes) as i32);
    a.mov_imm64(R::V0, MAX_OFFSET);
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::Above, invalid);
    a.load_ptr_disp32(R::V9, R::V6, offset_of!(S, next_offset) as i32);
    a.cmp_reg_reg(R::V9, R::V10);
    a.jcc_label(C::Above, invalid);
    a.jcc_label(C::Equal, aligned);
    a.mov_reg_reg(R::V0, R::V9);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(aligned);
    for (field, local) in [
        (offset_of!(S, slot_budget), -24),
        (offset_of!(S, output_capacity), -16),
    ] {
        a.load_ptr_disp32(R::V0, R::V6, field as i32);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, invalid);
        a.cmp_reg_imm32(R::V0, 256);
        a.jcc_label(C::Above, invalid);
        a.store_rbp_slot(-local, R::V0);
    }
    a.load_ptr_disp32(R::V7, R::V6, offset_of!(S, output) as i32);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V1, R::V7);
    a.and_reg_imm32(R::V1, 7);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.shl_reg_imm8(R::V0, 3); // capacity from loop above
    a.add_reg_reg(R::V0, R::V7);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::Below, invalid);
    a.store_rbp_slot(8, R::V7);
    a.load_ptr_disp32(R::V1, R::V8, offset_of!(D, fixed_bytes) as i32);
    a.cmp_reg_reg(R::V10, R::V1);
    a.jcc_label(C::Below, invalid);
    a.mov_reg_reg(R::V0, R::V10);
    a.sub_reg_reg(R::V0, R::V1);
    a.load_ptr_disp32(R::V11, R::V8, offset_of!(D, element_bytes) as i32);
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, fixed);
    a.div_unsigned(R::V11);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, invalid);
    a.jmp_label(validated);
    a.bind_text_label(fixed);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(validated);
    a.mov_imm64(R::V0, 0);
    a.store_rbp_slot(32, R::V0);
    a.store_rbp_slot(40, R::V0);
    a.bind_text_label(scan);
    a.cmp_reg_reg(R::V9, R::V10);
    a.jcc_label(C::AboveOrEqual, finish);
    a.load_ptr_disp32(R::V1, R::V8, offset_of!(D, fixed_bytes) as i32);
    a.cmp_reg_reg(R::V9, R::V1);
    a.jcc_label(C::AboveOrEqual, array);
    a.load_ptr_disp32(R::V7, R::V8, offset_of!(D, fixed_bitmap) as i32);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, skip_fixed);
    a.mov_reg_reg(R::V0, R::V1);
    a.sub_reg_reg(R::V0, R::V9);
    a.cmp_reg_imm32(R::V0, 8);
    a.jcc_label(C::Below, skip_fixed);
    a.mov_reg_reg(R::V0, R::V9);
    a.shr_reg_imm8(R::V0, 3);
    a.jmp_label(candidate);
    a.bind_text_label(skip_fixed);
    a.mov_reg_reg(R::V9, R::V1);
    a.jmp_label(scan);
    a.bind_text_label(array);
    a.load_ptr_disp32(R::V7, R::V8, offset_of!(D, element_bitmap) as i32);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, skip_array);
    a.mov_reg_reg(R::V0, R::V9);
    a.sub_reg_reg(R::V0, R::V1);
    a.load_ptr_disp32(R::V11, R::V8, offset_of!(D, element_bytes) as i32);
    a.div_unsigned(R::V11);
    a.mov_reg_reg(R::V0, R::V2);
    a.shr_reg_imm8(R::V0, 3);
    a.bind_text_label(candidate);
    a.load_rbp_slot(R::V2, 40);
    a.load_rbp_slot(R::V11, 24);
    a.cmp_reg_reg(R::V2, R::V11);
    a.jcc_label(C::Equal, finish);
    a.load_rbp_slot(R::V11, 32);
    a.load_rbp_slot(R::V1, 16);
    a.cmp_reg_reg(R::V11, R::V1);
    a.jcc_label(C::Equal, finish);
    a.add_reg_imm32(R::V2, 1);
    a.store_rbp_slot(40, R::V2);
    a.mov_reg_reg(R::V1, R::V0);
    a.and_reg_imm32(R::V1, 63);
    a.shr_reg_imm8(R::V0, 6);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V7);
    a.load_ptr_disp32(R::V0, R::V0, 0);
    a.shr_reg_count(R::V0);
    a.and_reg_imm32(R::V0, 1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, advance);
    a.mov_reg_reg(R::V0, R::V11);
    a.shl_reg_imm8(R::V0, 3);
    a.load_rbp_slot(R::V1, 8);
    a.add_reg_reg(R::V0, R::V1);
    a.store_ptr_disp32(R::V0, 0, R::V9);
    a.add_reg_imm32(R::V11, 1);
    a.store_rbp_slot(32, R::V11);
    a.bind_text_label(advance);
    a.add_reg_imm32(R::V9, 8);
    a.jmp_label(scan);
    a.bind_text_label(skip_array);
    a.mov_reg_reg(R::V9, R::V10);
    a.bind_text_label(finish);
    a.store_ptr_disp32(R::V6, offset_of!(S, next_offset) as i32, R::V9);
    a.load_rbp_slot(R::V0, 32);
    a.store_ptr_disp32(R::V6, offset_of!(S, written) as i32, R::V0);
    a.load_rbp_slot(R::V0, 40);
    a.store_ptr_disp32(R::V6, offset_of!(S, examined) as i32, R::V0);
    a.cmp_reg_reg(R::V9, R::V10);
    a.jcc_label(C::NotEqual, more);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(more);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
    a.leave();
}

#[repr(C)]
pub(super) struct Builtins {
    pub(super) pointer_bit: u64,
    pub(super) raw: D,
    pub(super) record: D,
    pub(super) list: D,
}
const _: () = assert!(std::mem::size_of::<Builtins>() == 200);

/// Fresh native storage only; never overwrite a published descriptor package.
/// Layout and immutable-lifetime contract: gc_builtin_descriptors.h.
#[allow(dead_code)]
pub(crate) fn emit_builtin_prepare<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V7, offset_of!(Builtins, pointer_bit) as i32, R::V0);
    for (base, shape) in [
        offset_of!(Builtins, raw),
        offset_of!(Builtins, record),
        offset_of!(Builtins, list),
    ]
    .into_iter()
    .zip(crate::gc_shape::BuiltinShape::ALL)
    {
        let d = shape.descriptor(0);
        let values = [
            d.id,
            d.alignment,
            d.fixed_bytes,
            d.element_bytes,
            d.fixed_bitmap,
            d.element_bitmap,
            d.fixed_bitmap_words,
            d.element_bitmap_words,
        ];
        for (index, value) in values.into_iter().enumerate() {
            if index * 8 == offset_of!(D, element_bitmap) && d.element_bitmap_words != 0 {
                a.store_ptr_disp32(R::V7, (base + index * 8) as i32, R::V7);
            } else {
                a.mov_imm64(R::V0, value);
                a.store_ptr_disp32(R::V7, (base + index * 8) as i32, R::V0);
            }
        }
    }
}

/// Registry V7, AllocationShape V6; modifies only the request's shape fields.
#[allow(dead_code)]
pub(crate) fn emit_allocation_shape<E: PortableAsm>(a: &mut E) {
    use crate::gc_abi::{AllocationRequest, AllocationShape};
    let invalid = a.create_text_label();
    let fixed = a.create_text_label();
    let valid = a.create_text_label();
    let done = a.create_text_label();
    for reg in [R::V7, R::V6] {
        a.test_reg_reg(reg, reg);
        a.jcc_label(C::Equal, invalid);
        a.mov_reg_reg(R::V0, reg);
        a.and_reg_imm32(R::V0, 7);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    registry(a, invalid);
    a.mov_reg_reg(R::V9, R::V0); // validated ID
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V8);
    a.atomic_load_acquire(R::V10, R::V0);
    a.load_ptr_disp32(R::V8, R::V6, offset_of!(AllocationShape, allocation) as i32);
    for reg in [R::V10, R::V8] {
        a.test_reg_reg(reg, reg);
        a.jcc_label(C::Equal, invalid);
        a.mov_reg_reg(R::V0, reg);
        a.and_reg_imm32(R::V0, 7);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.load_ptr_disp32(R::V0, R::V10, offset_of!(D, id) as i32);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(
        R::V0,
        R::V6,
        offset_of!(AllocationShape, payload_bytes) as i32,
    );
    a.mov_imm64(R::V1, MAX_OFFSET);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, invalid);
    a.load_ptr_disp32(R::V11, R::V10, offset_of!(D, element_bytes) as i32);
    a.cmp_reg_reg(R::V11, R::V1);
    a.jcc_label(C::Above, invalid);
    a.load_ptr_disp32(R::V1, R::V10, offset_of!(D, fixed_bytes) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid);
    a.sub_reg_reg(R::V0, R::V1);
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, fixed);
    a.div_unsigned(R::V11);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, invalid);
    a.jmp_label(valid);
    a.bind_text_label(fixed);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(valid);
    a.store_ptr_disp32(
        R::V8,
        offset_of!(AllocationRequest, descriptor) as i32,
        R::V10,
    );
    a.store_ptr_disp32(R::V8, offset_of!(AllocationRequest, elements) as i32, R::V0);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
}
