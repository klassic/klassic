//! Owner-only pressure completion for work already protected in every active
//! generation. Startup storage is permanent; the owner is its sole producer
//! and consumer, so no storage job is handed to another actor. Phase fencing
//! prevents these entries from outliving their referent metadata. A live
//! journal claim is forbidden. Unknown values use the counted locked consumer.
//! No queue, hint, wake, or collector-ticket state changes on this path.
use super::*;
use crate::gc_abi::{BufferCursor, RootSegment};
use crate::gc_codegen::production::{load_rt, store_rt};
const _: () = assert!(JOURNAL_ENTRIES.is_power_of_two());

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.root_journal_protected);
    for r in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(r);
    }
    let unknown = a.create_text_label();
    let handled = a.create_text_label();
    let done = a.create_text_label();
    // A worker may consume this ring after cooperative enrollment. All owner
    // completion must then use the same ticket as the counted consumer.
    lea_rt(a, R::V0, cooperative_old::ENABLED);
    a.atomic_load_acquire(R::V0, R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, unknown);
    for field in [
        offset_of!(RootRegistration, journal_serial),
        offset_of!(RootRegistration, journal_end),
    ] {
        load_rt(a, R::V0, registered_roots::registration(a) + field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    for field in [
        RT_CTX + offset_of!(MutatorContext, private_producers),
        RT_CTX + offset_of!(MutatorContext, access_epoch),
    ] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    load_rt(
        a,
        R::V0,
        registered_roots::registration(a) + offset_of!(RootRegistration, registered),
    );
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, OWNER_LOCKED);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::Above, l.fail);
    load_rt(
        a,
        R::V0,
        registered_roots::registration(a) + offset_of!(RootRegistration, scope),
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V1, RT_SEGMENT);
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(RootSegment, scope)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, RT_SEGMENT);
    a.add_reg_imm32(R::V0, d(offset_of!(RootSegment, state)));
    a.atomic_load_acquire(R::V0, R::V0);
    a.cmp_reg_imm8(R::V0, 1); // Retiring storage needs the counted consumer.
    a.jcc_label(C::NotEqual, unknown);
    load_rt(a, R::V10, RT_JOURNAL);
    a.load_ptr_disp32(R::V6, R::V10, d(offset_of!(BufferCursor, capacity)));
    a.cmp_reg_imm32(R::V6, JOURNAL_ENTRIES as i32);
    a.jcc_label(C::NotEqual, unknown);
    a.load_ptr_disp32(R::V8, R::V10, d(offset_of!(BufferCursor, consumed)));
    a.load_ptr_disp32(R::V0, R::V10, d(offset_of!(BufferCursor, published)));
    a.cmp_reg_reg(R::V8, R::V0);
    a.jcc_label(C::Above, unknown);
    a.jcc_label(C::Equal, handled);
    a.sub_reg_reg(R::V0, R::V8);
    a.cmp_reg_imm32(R::V0, JOURNAL_ENTRIES as i32);
    a.jcc_label(C::Above, unknown);
    a.load_ptr_disp32(R::V9, R::V10, d(offset_of!(BufferCursor, entries)));
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, unknown);
    a.mov_reg_reg(R::V0, R::V9);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, unknown);
    a.mov_reg_reg(R::V0, R::V9);
    a.add_reg_imm32(
        R::V0,
        (JOURNAL_ENTRIES * size_of::<RootChange>() as u64) as i32,
    );
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::Below, unknown);
    a.mov_reg_reg(R::V0, R::V8);
    a.and_reg_imm32(R::V0, (JOURNAL_ENTRIES - 1) as i32);
    a.shl_reg_imm8(R::V0, 5);
    a.add_reg_reg(R::V9, R::V0);
    for (field, current) in [
        (
            offset_of!(RootChange, scope),
            registered_roots::registration(a) + offset_of!(RootRegistration, scope),
        ),
        (
            offset_of!(RootChange, phase),
            RT_CTX + offset_of!(MutatorContext, adopted_phase),
        ),
    ] {
        a.load_ptr_disp32(R::V0, R::V9, d(field));
        load_rt(a, R::V1, current);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, l.fail);
    }
    a.mov_imm64(R::V11, 0); // pending-allocation sides, committed only on success
    for field in [
        offset_of!(RootChange, previous),
        offset_of!(RootChange, next),
    ] {
        let next = a.create_text_label();
        let young = a.create_text_label();
        let protected = a.create_text_label();
        a.load_ptr_disp32(R::V0, R::V9, d(field));
        fields::offset(a, R::V0, R::V2, next);
        load_rt(a, R::V0, RT_MARK_ACTIVE);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, young);
        mark::test_mark(a, R::V2, R::V6, R::V7);
        a.jcc_label(C::Equal, unknown);
        a.bind_text_label(young);
        load_rt(a, R::V0, young_mark::ACTIVE);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, protected);
        generation::test(a, R::V2, generation::YOUNG);
        a.jcc_label(C::NotEqual, protected);
        young_mark::test(a, R::V2, R::V6, R::V7);
        a.jcc_label(C::Equal, unknown);
        a.bind_text_label(protected);
        load_rt(a, R::V1, RT_PENDING_REF);
        a.shr_reg_imm8(R::V1, 16);
        a.cmp_reg_reg(R::V2, R::V1);
        a.jcc_label(C::NotEqual, next);
        a.add_reg_imm32(R::V11, 1);
        a.bind_text_label(next);
    }
    // All required work was already owned before this read. No new handoff or
    // storage lease is needed on the sole owner's permanent SPSC buffer.
    a.add_reg_imm32(R::V8, 1);
    a.add_reg_imm32(R::V10, d(offset_of!(BufferCursor, consumed)));
    a.atomic_store_release(R::V10, R::V8);
    root_work::ack(a, l, None);
    copy::bump(a, BASE + offset_of!(State, entries), R::V0);
    copy::bump(a, BASE + offset_of!(State, fast_entries), R::V0);
    load_rt(a, R::V0, BASE + offset_of!(State, pending_offers));
    a.add_reg_reg(R::V0, R::V11);
    store_rt(a, BASE + offset_of!(State, pending_offers), R::V0);
    a.bind_text_label(handled);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(unknown);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(done);
    for r in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(r);
    }
    a.ret();
}
