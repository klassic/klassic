//! Explicit root-only Native return in an authenticated moving window. A
//! completed census can bridge older bank/color reuse; a current open source
//! still retains all references from the immediately preceding bank.
use super::*;
pub(in super::super) fn adopt<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    native_io::adoption::private_closed(a, l);
    journal::lifetime(a, l);
    for f in [
        native_io::SCOPE_BORROWED,
        RT_REQUEST + offset_of!(AllocationRequest, page),
        RT_CTX + offset_of!(MutatorContext, tlab_start),
        RT_CTX + offset_of!(MutatorContext, tlab_top),
        RT_CTX + offset_of!(MutatorContext, tlab_end),
    ] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    load_rt(a, R::V8, native_io::PHASE);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, l.fail);
    lea_rt(
        a,
        R::V1,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    a.atomic_load_acquire(R::V9, R::V1);
    engine(a, l, false);
    engine(a, l, true);
    a.mov_reg_reg(R::V0, R::V8);
    a.or_reg_reg(R::V0, R::V9);
    a.and_reg_imm32(R::V0, 0xe000);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    for shift in [0, 4] {
        a.mov_reg_reg(R::V0, R::V8);
        a.shr_reg_imm8(R::V0, shift);
        a.and_reg_imm32(R::V0, 15);
        let valid = a.create_text_label();
        for stage in [0, 2, 4, 6, 7] {
            a.cmp_reg_imm8(R::V0, stage);
            a.jcc_label(C::Equal, valid);
        }
        a.jmp_label(l.fail);
        a.bind_text_label(valid);
    }
    a.mov_reg_reg(R::V0, R::V8);
    a.shr_reg_imm8(R::V0, 16);
    a.mov_reg_reg(R::V1, R::V9);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    load_rt(a, R::V0, native_io::BANK);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V1, allocation_records::REVISION);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    let closed = a.create_text_label();
    let older = a.create_text_label();
    let covered = a.create_text_label();
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, closed);
    load_rt(a, R::V0, TARGET_PHASE);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, native_io::BANK);
    load_rt(a, R::V1, CURRENT_BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, covered);
    load_rt(a, R::V1, SOURCE_BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, covered);
    a.jcc_label(C::Above, l.fail);
    // Roots of an older excluded owner were healed by the last completed
    // census. Its backing bank must be the source of this one open move.
    load_rt(a, R::V0, native_completion::BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_reg_reg(R::V0, R::V8);
    a.shr_reg_imm8(R::V0, 16);
    load_rt(a, R::V1, native_completion::PHASE);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    load_rt(a, R::V1, native_completion::SCOPE);
    a.jmp_label(older);
    a.bind_text_label(closed);
    journal::require_closed(a, l);
    load_rt(a, R::V0, native_completion::PENDING_BANK);
    load_rt(a, R::V1, CURRENT_BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    let completed = a.create_text_label();
    a.jcc_label(C::NotEqual, completed);
    // Retire's real empty-ledger/census rendezvous already made these cells
    // canonical; physical cleanup proceeds independently in bounded quanta.
    load_rt(a, R::V1, native_completion::PENDING_SCOPE);
    a.jmp_label(older);
    a.bind_text_label(completed);
    load_rt(a, R::V0, native_completion::BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, native_completion::PHASE);
    a.mov_reg_reg(R::V2, R::V0);
    a.xor_reg_reg(R::V2, R::V9);
    a.and_reg_imm32(R::V2, 0x1c00);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    a.shr_reg_imm8(R::V0, 16);
    a.mov_reg_reg(R::V2, R::V9);
    a.shr_reg_imm8(R::V2, 16);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Above, l.fail);
    load_rt(a, R::V1, native_completion::SCOPE);
    a.bind_text_label(older);
    load_rt(a, R::V0, native_io::ROOT_SCOPE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    a.bind_text_label(covered);
    load_rt(a, R::V0, native_io::ROOT_SCOPE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.atomic_store_release(R::V1, R::V9);
    allocation_records::adopt(a, l);
    copy::bump(a, native_io::ADOPTIONS, R::V0);
}

/// Logical marks may advance after a completed physical census. Bind each
/// slot to its real engine and full serial before adopting its color parity.
fn engine<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, old: bool) {
    let (active, valid, epoch, shift) = if old {
        (RT_MARK_ACTIVE, RT_MARK_VALID, RT_MARK_CYCLES, 4)
    } else {
        (young_mark::ACTIVE, young_mark::VALID, young_mark::EPOCH, 0)
    };
    let concurrent = a.create_text_label();
    let selected = a.create_text_label();
    let passive = a.create_text_label();
    let serial = a.create_text_label();
    a.mov_reg_reg(R::V0, R::V9);
    a.shr_reg_imm8(R::V0, shift);
    a.and_reg_imm32(R::V0, 15);
    for stage in [0, 6, 7] {
        a.cmp_reg_imm32(R::V0, stage);
        a.jcc_label(C::Equal, passive);
    }
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::Equal, concurrent);
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Equal, selected);
    a.jmp_label(l.fail);
    a.bind_text_label(concurrent);
    load_rt(a, R::V0, active);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, epoch);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.jmp_label(serial);
    a.bind_text_label(selected);
    load_rt(a, R::V0, valid);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, epoch);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.bind_text_label(passive);
    load_rt(a, R::V0, active);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(serial);
    load_rt(a, R::V0, epoch);
    a.and_reg_imm32(R::V0, 1);
    a.mov_reg_reg(R::V1, R::V9);
    a.shr_reg_imm8(R::V1, 8 + shift / 4);
    a.and_reg_imm32(R::V1, 1);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
}
