//! Colored heap fields and the compiler's typed/raw read boundary.
//!
//! Record slots contain GcRef bits; RawBytes payloads are opaque. Scalars
//! boxed by enum lowering must bypass decoding even if their bits happen to
//! be a valid reference. Colors and load masks use the adopted ABI snapshot;
//! production phase publication and generation-specific adoption remain open.
use super::{
    Labels, RT_COLLECTING, RT_HEALED, RT_HEAP_BASE, RT_MARK_ACTIVE, RT_MARK_DONE, RT_PHASE_OPEN,
    copy::DESCRIPTOR, decode, load_rt, load_runtime, store_rt,
};
use crate::gc_abi::REMEMBERED_MASK;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

pub(super) use super::colours::offset;

/// A nonnull GC-only heal cannot certify a mutator store's bookkeeping.
/// Keep both remembered bits until an actual store or remset handoff completes.
pub(super) fn forget_nonnull<E: PortableAsm>(a: &mut E, value: R, scratch: R) {
    a.mov_imm64(scratch, u64::from(REMEMBERED_MASK));
    a.or_reg_reg(value, scratch);
}

pub(super) fn young_offset<E: PortableAsm>(a: &mut E, value: R, scratch: R, outside: E::TextLabel) {
    offset(a, value, scratch, outside);
    super::generation::test(a, scratch, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, outside);
}

/// Emit an untyped enum read: V1 = canonical owner, V0 = field address.
/// The descriptor decides whether to decode; the loaded bits never decide
/// whether a scalar is a reference. V10 is the load-barrier slot argument.
pub(crate) fn emit_untyped_load<E: PortableAsm>(a: &mut E, barrier: E::TextLabel) {
    let raw = a.create_text_label();
    let reference = a.create_text_label();
    a.mov_reg_reg(R::V10, R::V0);
    a.load_ptr_disp32(R::V0, R::V10, 0);
    a.push_reg(R::V1);
    a.load_ptr_disp32(R::V1, R::V1, DESCRIPTOR);
    a.cmp_reg_imm8(R::V1, 1);
    a.pop_reg(R::V1);
    a.jcc_label(C::Equal, raw);
    a.push_reg(R::V1);
    a.load_ptr_disp32(R::V1, R::V1, DESCRIPTOR);
    a.cmp_reg_imm8(R::V1, 4);
    a.pop_reg(R::V1);
    a.jcc_label(C::NotEqual, reference);
    a.cmp_reg_reg(R::V10, R::V1); // PointerList length is scalar too.
    a.jcc_label(C::Equal, raw);
    a.bind_text_label(reference);
    a.call_label(barrier);
    a.bind_text_label(raw);
}

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    emit_load(a, l, runtime);
    emit_store(a, l, runtime);
}

/// V0 = field bits, V10 = slot address -> V0 = resolved payload address.
/// A GC heal stores encoded bits by CAS; it never writes a raw address.
fn emit_load<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    let pass = a.create_text_label();
    let unchanged = a.create_text_label();
    let resolved = a.create_text_label();
    let hit = a.create_text_label();
    let recolour = a.create_text_label();
    let body = a.create_text_label();
    a.bind_text_label(l.field_load);
    for reg in [R::V3, R::V1, R::V2, R::OwnerContext] {
        a.push_reg(reg);
    }
    load_runtime(a, runtime);
    a.jmp_label(body);
    a.bind_text_label(l.field_load_actor);
    for reg in [R::V3, R::V1, R::V2, R::OwnerContext] {
        a.push_reg(reg);
    }
    super::owner_state::bind_running(a, l);
    a.bind_text_label(body);
    offset(a, R::V0, R::V1, pass);
    a.push_reg(R::V0);
    load_rt(a, R::V2, RT_PHASE_OPEN);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, unchanged);
    {
        let (lo, hi) = (super::RT_RY_LO, super::RT_RY_HI);
        a.mov_reg_reg(R::V2, R::V1);
        load_rt(a, R::V0, lo);
        a.sub_reg_reg(R::V2, R::V0);
        load_rt(a, R::V0, hi);
        a.push_reg(R::V1);
        load_rt(a, R::V1, lo);
        a.sub_reg_reg(R::V0, R::V1);
        a.pop_reg(R::V1);
        a.cmp_reg_reg(R::V2, R::V0);
        a.jcc_label(C::Below, hit);
    }
    super::old_runs::source(a, R::V1);
    a.jcc_label(C::Equal, hit);
    a.bind_text_label(unchanged);
    a.pop_reg(R::V0);
    super::colours::load_access(a);
    a.xor_reg_reg(R::V1, R::V0);
    a.and_reg_reg(R::V1, R::V2);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, recolour);
    a.jmp_label(resolved);
    a.bind_text_label(recolour);
    a.push_reg(R::V0);
    a.mov_reg_reg(R::V1, R::V0);
    a.shr_reg_imm8(R::V1, 16);
    a.bind_text_label(hit);
    for reg in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(reg);
    }
    if l.worker.is_some() {
        a.push_reg(R::V1);
        a.push_reg(R::V1);
        a.mov_reg_reg(R::V2, R::V1);
        super::young_barrier::protect(a, l);
        a.pop_reg(R::V1);
        a.pop_reg(R::V1);
    }
    if let Some(w) = &l.worker {
        let not_marking = a.create_text_label();
        load_rt(a, R::V0, RT_MARK_ACTIVE);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, not_marking);
        a.push_reg(R::V1);
        a.push_reg(R::V1);
        a.mov_reg_reg(R::V2, R::V1);
        super::worker::lock(a, l);
        a.call_label(w.mark_ref);
        a.mov_imm64(R::V0, 0);
        store_rt(a, RT_MARK_DONE, R::V0);
        super::worker::request_work(a);
        super::worker::unlock(a, l);
        super::worker::wake_if_pending(a, l);
        a.pop_reg(R::V1);
        a.pop_reg(R::V1);
        a.bind_text_label(not_marking);
    }
    a.mov_imm64(R::V2, 1);
    store_rt(a, RT_COLLECTING, R::V2);
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 16);
    super::colours::load_current(a, R::V2);
    a.or_reg_reg(R::V0, R::V2);
    a.call_label(l.forward);
    forget_nonnull(a, R::V0, R::V1);
    a.mov_imm64(R::V1, 0);
    store_rt(a, RT_COLLECTING, R::V1);
    super::copy::bump(a, RT_HEALED, R::V1);
    for reg in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(reg);
    }
    a.mov_reg_reg(R::V1, R::V0);
    a.pop_reg(R::V0);
    a.atomic_compare_exchange(R::V10, R::V1);
    a.mov_reg_reg(R::V0, R::V1);
    a.bind_text_label(resolved);
    decode(a, R::V0, R::V1);
    a.bind_text_label(pass);
    for reg in [R::OwnerContext, R::V2, R::V1, R::V3] {
        a.pop_reg(reg);
    }
    a.ret();
}

/// V7 = slot, V6 = new payload address/null (preserved).
fn emit_store<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    let done = a.create_text_label();
    let raw = a.create_text_label();
    let publish = a.create_text_label();
    let body = a.create_text_label();
    a.bind_text_label(l.field_store);
    for reg in [
        R::V0,
        R::V1,
        R::V2,
        R::V3,
        R::V6,
        R::V7,
        R::V8,
        R::V9,
        R::V10,
        R::V11,
        R::OwnerContext,
    ] {
        a.push_reg(reg);
    }
    load_runtime(a, runtime);
    a.jmp_label(body);
    a.bind_text_label(l.field_store_actor);
    for reg in [
        R::V0,
        R::V1,
        R::V2,
        R::V3,
        R::V6,
        R::V7,
        R::V8,
        R::V9,
        R::V10,
        R::V11,
        R::OwnerContext,
    ] {
        a.push_reg(reg);
    }
    super::owner_state::bind_running(a, l);
    a.bind_text_label(body);
    // Resolve a managed new referent before publishing current color bits.
    // Non-heap scalar words stay opaque. The caller supplies a current slot.
    let resolved_value = a.create_text_label();
    a.mov_reg_reg(R::V0, R::V6);
    super::enums::heap_offset(a, R::V0, R::V2, resolved_value);
    a.mov_reg_reg(R::V0, R::V2);
    a.shl_reg_imm8(R::V0, 16);
    super::colours::load_current(a, R::V2);
    a.or_reg_reg(R::V0, R::V2);
    a.push_reg(R::V7);
    a.push_reg(R::V7);
    a.mov_imm64(R::V2, 1);
    store_rt(a, RT_COLLECTING, R::V2);
    a.call_label(l.forward);
    a.mov_imm64(R::V2, 0);
    store_rt(a, RT_COLLECTING, R::V2);
    decode(a, R::V0, R::V1);
    a.mov_reg_reg(R::V6, R::V0);
    a.pop_reg(R::V7);
    a.pop_reg(R::V7);
    a.bind_text_label(resolved_value);
    super::young_barrier::field_store(a, l);
    if let Some(w) = &l.worker {
        let satb_done = a.create_text_label();
        load_rt(a, R::V0, RT_MARK_ACTIVE);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, satb_done);
        for previous in [true, false] {
            let next = a.create_text_label();
            if previous {
                a.load_ptr_disp32(R::V0, R::V7, 0);
                offset(a, R::V0, R::V2, next);
            } else {
                // The store publishes current mark bits, so protect its new
                // raw referent too; callers need not rely on an earlier load.
                a.mov_reg_reg(R::V0, R::V6);
                super::enums::heap_offset(a, R::V0, R::V2, next);
            }
            for reg in [R::V6, R::V7, R::V8, R::V9] {
                a.push_reg(reg);
            }
            let protected = a.create_text_label();
            // The sole owner's phase/metadata cannot retire during this store.
            // A matching acquired epoch already owns a mark obligation. Any
            // admission still enqueuing it holds the end-check collector lock.
            super::mark::test_mark(a, R::V2, R::V6, R::V7);
            a.jcc_label(C::NotEqual, protected);
            super::worker::lock(a, l);
            a.call_label(w.mark_ref);
            a.mov_imm64(R::V0, 0);
            store_rt(a, RT_MARK_DONE, R::V0);
            super::worker::request_work(a);
            super::worker::unlock(a, l);
            // Newly admitted SATB work must run even without another allocation.
            super::worker::wake_if_pending(a, l);
            a.bind_text_label(protected);
            for reg in [R::V9, R::V8, R::V7, R::V6] {
                a.pop_reg(reg);
            }
            a.bind_text_label(next);
        }
        a.bind_text_label(satb_done);
    }
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.mov_reg_reg(R::V0, R::V7);
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, publish);
    super::generation::test(a, R::V0, super::generation::OLD);
    a.jcc_label(C::NotEqual, publish);
    a.mov_reg_reg(R::V2, R::V6);
    super::enums::young_offset(a, R::V2, R::V0, publish);
    super::remset::remember(a, l, R::V7, R::V1, R::V2);
    a.bind_text_label(publish);
    a.mov_reg_reg(R::V0, R::V6);
    super::enums::heap_offset(a, R::V0, R::V2, raw);
    a.shl_reg_imm8(R::V2, 16);
    super::colours::load_current(a, R::V0);
    a.or_reg_reg(R::V2, R::V0);
    a.atomic_store_release(R::V7, R::V2);
    a.jmp_label(done);
    a.bind_text_label(raw);
    a.atomic_store_release(R::V7, R::V6);
    a.bind_text_label(done);
    for reg in [
        R::OwnerContext,
        R::V11,
        R::V10,
        R::V9,
        R::V8,
        R::V7,
        R::V6,
        R::V3,
        R::V2,
        R::V1,
        R::V0,
    ] {
        a.pop_reg(reg);
    }
    a.ret();
}
