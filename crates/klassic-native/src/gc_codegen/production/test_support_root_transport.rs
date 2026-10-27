//! Real root transport and initialization observations for native harnesses.
use super::super::*;
use crate::gc_abi::{MarkRecord, ObjectHeader};
use crate::gc_codegen::production::{OwnerState, lea_rt, load_rt, owner_state};

pub(crate) fn root_journal_native_boundary(g: &mut crate::NativeCodeGenerator) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (enter, exit) = (l.native_enter, l.native_exit);
    g.call_label(enter);
    g.call_label(exit);
}
pub(crate) fn root_journal_pressure_store(g: &mut crate::NativeCodeGenerator, locked: bool) {
    let runtime = g.gc_gen.take().unwrap();
    if locked {
        worker::lock(g, &runtime.labels);
    }
    store_rt_imm(g, RT_ALLOC_ROOT + REQUEST_VALUE, 0);
    root_step(g, &runtime.labels, RT_ALLOC_ROOT, ROOT_STORE);
    load_rt(g, R::V0, root_journal::OWNER_LOCKED);
    if locked {
        worker::unlock(g, &runtime.labels);
    }
    g.gc_gen = Some(runtime);
}
pub(crate) fn root_journal_pressure_once(g: &mut crate::NativeCodeGenerator) {
    let label = g.gc_gen.as_ref().unwrap().labels.drain;
    g.call_label(label);
}
pub(crate) fn root_journal_owner(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, root_journal::OWNER_LOCKED);
}
pub(crate) fn root_journal_pending_offers(g: &mut crate::NativeCodeGenerator) {
    load_rt(
        g,
        R::V0,
        owner_state::offset(offset_of!(OwnerState, root_journal))
            + offset_of!(root_journal::State, pending_offers),
    );
}
pub(crate) fn root_journal_pending(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, RT_PENDING_REF);
}
pub(crate) fn root_journal_published(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, RT_PUBLISHED);
}
pub(crate) fn root_journal_pending_state(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, RT_PENDING_REF);
    g.shr_reg_imm8(R::V0, 16);
    load_rt(g, R::V1, RT_HEAP_BASE);
    g.add_reg_reg(R::V0, R::V1);
    g.load_ptr_disp32(
        R::V0,
        R::V0,
        d(offset_of!(ObjectHeader, state)) - size_of::<ObjectHeader>() as i32,
    );
}
/// Invalidate an actual initializing object's protection for a rejection probe.
pub(crate) fn root_journal_forget_pending(g: &mut crate::NativeCodeGenerator, young: bool) {
    load_rt(g, R::V0, RT_PENDING_REF);
    g.shr_reg_imm8(R::V0, 16);
    load_rt(g, R::V1, RT_HEAP_BASE);
    g.add_reg_reg(R::V0, R::V1);
    g.load_ptr_disp32(
        R::V0,
        R::V0,
        d(offset_of!(ObjectHeader, mark_record)) - size_of::<ObjectHeader>() as i32,
    );
    g.mov_imm64(R::V1, 0);
    g.store_ptr_disp32(
        R::V0,
        d(if young {
            offset_of!(MarkRecord, young_epoch)
        } else {
            offset_of!(MarkRecord, old_epoch)
        }),
        R::V1,
    );
}

pub(crate) fn root_journal_bad_owner_lock(g: &mut crate::NativeCodeGenerator, nested: bool) {
    let runtime = g.gc_gen.take().unwrap();
    if nested {
        worker::lock(g, &runtime.labels);
        worker::lock(g, &runtime.labels);
    } else {
        worker::unlock(g, &runtime.labels);
    }
    g.gc_gen = Some(runtime);
}

/// Append through the actual producer until its real FLUSH response. There
/// is no concurrent journal consumer in these deterministic full-ring probes.
pub(crate) fn root_journal_fill_frame(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().unwrap();
    let again = g.create_text_label();
    let full = g.create_text_label();
    g.bind_text_label(again);
    lea_rt(g, R::V7, RT_CTX);
    lea_rt(g, R::V6, RT_FRAME_ROOT);
    g.mov_imm64(R::V2, ROOT_STORE);
    g.call_label(runtime.labels.step);
    g.cmp_reg_imm8(R::V0, ROOT_FLUSH as i8);
    g.jcc_label(C::Equal, full);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, runtime.labels.fail);
    g.jmp_label(again);
    g.bind_text_label(full);
    g.gc_gen = Some(runtime);
}
pub(crate) fn root_journal_outstanding(g: &mut crate::NativeCodeGenerator) {
    use crate::gc_abi::BufferCursor;
    load_rt(g, R::V1, RT_JOURNAL);
    g.load_ptr_disp32(R::V0, R::V1, d(offset_of!(BufferCursor, published)));
    g.load_ptr_disp32(R::V1, R::V1, d(offset_of!(BufferCursor, consumed)));
    g.sub_reg_reg(R::V0, R::V1);
}

pub(crate) fn root_journal_claimed_entries(g: &mut crate::NativeCodeGenerator) {
    let base = owner_state::offset(offset_of!(OwnerState, root_journal));
    load_rt(g, R::V0, base + offset_of!(root_journal::State, entries));
    load_rt(
        g,
        R::V1,
        base + offset_of!(root_journal::State, fast_entries),
    );
    g.sub_reg_reg(R::V0, R::V1);
}
