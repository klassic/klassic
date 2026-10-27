//! Actual AAPCS64 entry/body/return instructions, independent of OS services.
use super::{Assembler, Reg};
use crate::portable_asm::{PortableAsm, Reg as P};

pub(crate) fn code(locals: u32, backward: bool) -> (Vec<u8>, usize, usize) {
    let mut a = Assembler::default();
    let entry = a.new_label();
    let leaf = a.new_label();
    let mut leaf_at = 0;
    let emit_leaf = |a: &mut Assembler| {
        a.bind(leaf);
        let at = a.code.len();
        a.native_entry_begin(0);
        a.mov_imm64(Reg::X0, 0x4c45_4146);
        a.native_entry_end();
        a.ret();
        at
    };
    if backward {
        leaf_at = emit_leaf(&mut a);
    }
    a.bind(entry);
    let entry_at = a.code.len();
    a.native_entry_begin(locals);
    a.mov_reg(Reg::X9, Reg::X5); // portable V7 contains the actual native arg
    a.store_ptr_disp32(P::V11, 0, P::V7);
    a.word(0x9100_03e0); // mov x0,sp
    a.store_ptr_disp32(P::V11, 8, P::V0);
    a.mov_text_addr(P::V0, leaf);
    a.store_ptr_disp32(P::V11, 16, P::V0);
    if locals != 0 {
        a.store_rbp_slot(8, P::V7);
    }
    for reg in 19..=28 {
        a.word(0xaa1f_03e0 | reg); // mov xn,xzr
    }
    for reg in 8..=15 {
        // eor vn.16b,vn.16b,vn.16b: destroy the full SIMD register.
        a.word(0x6e20_1c00 | (reg << 16) | (reg << 5) | reg);
    }
    a.word(0xaa1f_03fe); // internal body can destroy native LR
    if locals != 0 {
        a.load_rbp_slot(P::V11, 8);
    }
    a.mov_imm64(Reg::X0, 0x444f_4e45);
    a.store_ptr_disp32(P::V11, 24, P::V0);
    a.mov_imm64(Reg::X0, 0x1234_5678_9abc_def0);
    a.native_entry_end();
    a.ret();
    if !backward {
        leaf_at = emit_leaf(&mut a);
    }
    a.finish();
    (a.code, entry_at, leaf_at)
}

#[test]
fn native_frame_arm_rejects_unaligned_and_unencodable_locals() {
    for locals in [1, 15, 4096, u32::MAX] {
        let mut a = Assembler::default();
        assert!(
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                a.native_entry_begin(locals);
            }))
            .is_err()
        );
        assert!(a.code.is_empty());
    }
}
