//! Finite nonmoving root storage operations, matching gc_roots.h.
//! No allocation, poll, forwarding resolution or synchronous frame traversal.
use crate::gc_abi::{
    ABI_VERSION, BufferCursor as B, MutatorContext as M, RootCell as Cell, RootChange as Change,
    RootHandle as H, RootRequest as Q, RootSegment as S,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;
const _: () = assert!(std::mem::size_of::<Cell>() == 32);
const _: () = assert!(std::mem::size_of::<Change>() == 32);

fn acquire<E: PortableAsm>(a: &mut E, dst: R, base: R, offset: usize) {
    a.mov_reg_reg(R::V0, base);
    a.add_reg_imm32(R::V0, offset as i32);
    a.atomic_load_acquire(dst, R::V0);
}

/// `(V7=ctx, V6=request, V2=op) -> OK/INVALID/FULL/FLUSH/EXHAUSTED 0..4`.
#[allow(dead_code)]
pub(crate) fn emit_step<E: PortableAsm>(a: &mut E) {
    emit_step_impl(a, false, false);
}
/// Internal finite handoff after the caller validates a live source scope.
pub(crate) fn emit_guarded_store<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V2, 1);
    emit_step_impl(a, true, false);
}
/// Cancel only a null owner root; keep scan bounds, journals and generations.
#[allow(dead_code)]
pub(crate) fn emit_cancel_empty<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V2, 3);
    emit_step_impl(a, false, true);
}
fn emit_step_impl<E: PortableAsm>(a: &mut E, guarded_store: bool, cancel_empty: bool) {
    let invalid = a.create_text_label();
    let full = a.create_text_label();
    let flush = a.create_text_label();
    let exhausted = a.create_text_label();
    let reserve = a.create_text_label();
    let retire = a.create_text_label();
    let read = a.create_text_label();
    let have_next = a.create_text_label();
    let publish_revision = a.create_text_label();
    let success = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 32);
    a.store_rbp_slot(8, R::V2);
    a.store_rbp_slot(24, R::V7);
    a.cmp_reg_imm32(R::V2, 5);
    a.jcc_label(C::Above, invalid);
    a.atomic_load_acquire_u32(R::V0, R::V7);
    a.cmp_reg_imm32(R::V0, ABI_VERSION as i32);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V10, R::V7, offset_of!(M, thread_id) as i32);
    a.test_reg_reg(R::V10, R::V10);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V7);
    a.add_reg_imm32(R::V0, offset_of!(M, state) as i32);
    a.atomic_load_acquire_u32(R::V0, R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    acquire(a, R::V0, R::V7, offset_of!(M, access_epoch));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(if guarded_store { C::Equal } else { C::NotEqual }, invalid);
    a.load_ptr_disp32(R::V8, R::V6, offset_of!(Q, segment) as i32);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V8);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(S, owner) as i32);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(S, scope) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V9, R::V8, offset_of!(S, cells) as i32);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V9);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V11, R::V8, offset_of!(S, capacity) as i32);
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, invalid);
    a.mov_imm64(R::V0, u32::MAX as u64);
    a.cmp_reg_reg(R::V11, R::V0);
    a.jcc_label(C::Above, invalid);
    a.mov_reg_reg(R::V0, R::V11);
    a.shl_reg_imm8(R::V0, 5);
    a.add_reg_reg(R::V0, R::V9);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::Below, invalid);
    acquire(a, R::V0, R::V8, offset_of!(S, state));
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, invalid);
    acquire(a, R::V10, R::V8, offset_of!(S, used));
    a.cmp_reg_reg(R::V10, R::V11);
    a.jcc_label(C::Above, invalid);
    a.cmp_reg_imm32(R::V2, 4);
    a.jcc_label(C::Equal, retire);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, reserve);
    a.load_ptr_disp32(
        R::V0,
        R::V6,
        (offset_of!(Q, handle) + offset_of!(H, scope)) as i32,
    );
    a.load_ptr_disp32(R::V1, R::V8, offset_of!(S, scope) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(
        R::V11,
        R::V6,
        (offset_of!(Q, handle) + offset_of!(H, cell)) as i32,
    );
    a.cmp_reg_reg(R::V11, R::V9);
    a.jcc_label(C::Below, invalid);
    a.mov_reg_reg(R::V0, R::V11);
    a.sub_reg_reg(R::V0, R::V9);
    a.shl_reg_imm8(R::V10, 5);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.and_reg_imm32(R::V0, 31);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.mov_reg_reg(R::V9, R::V11);
    a.load_ptr_disp32(R::V0, R::V9, offset_of!(Cell, active) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(
        R::V0,
        R::V6,
        (offset_of!(Q, handle) + offset_of!(H, reuse_generation)) as i32,
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V1, R::V9, offset_of!(Cell, reuse_generation) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.atomic_load_acquire(R::V0, R::V9); // cell.value at zero
    a.store_rbp_slot(16, R::V0);
    a.cmp_reg_imm32(R::V2, 3);
    a.jcc_label(C::Equal, read);
    a.load_ptr_disp32(R::V11, R::V8, offset_of!(S, journal) as i32);
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V11);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V7, R::V11, offset_of!(B, entries) as i32);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V7);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V11, offset_of!(B, capacity) as i32);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, invalid);
    a.mov_imm64(R::V0, u32::MAX as u64);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Above, invalid);
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 5);
    a.add_reg_reg(R::V0, R::V7);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::Below, invalid);
    a.load_ptr_disp32(R::V10, R::V11, offset_of!(B, top) as i32);
    acquire(a, R::V0, R::V11, offset_of!(B, published));
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, invalid);
    acquire(a, R::V0, R::V11, offset_of!(B, consumed));
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::Above, invalid);
    a.mov_reg_reg(R::V2, R::V10);
    a.sub_reg_reg(R::V2, R::V0);
    a.cmp_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Above, invalid);
    a.mov_imm64(R::V0, u64::MAX);
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::Equal, exhausted);
    a.cmp_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Equal, flush);
    a.load_rbp_slot(R::V0, 24);
    acquire(a, R::V0, R::V0, offset_of!(M, root_revision));
    a.mov_imm64(R::V2, u64::MAX);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Equal, exhausted);
    let mutation = a.create_text_label();
    a.load_rbp_slot(R::V2, 8);
    a.cmp_reg_imm32(R::V2, 5);
    a.jcc_label(C::NotEqual, mutation);
    a.load_rbp_slot(R::V0, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.jmp_label(success);
    a.bind_text_label(mutation);
    a.add_reg_imm32(R::V0, 1);
    a.store_rbp_slot(32, R::V0);
    a.mov_reg_reg(R::V0, R::V10);
    a.div_unsigned(R::V1);
    a.shl_reg_imm8(R::V2, 5);
    a.add_reg_reg(R::V7, R::V2); // ring entry
    a.load_rbp_slot(R::V0, 16);
    a.store_ptr_disp32(R::V7, offset_of!(Change, previous) as i32, R::V0);
    a.load_rbp_slot(R::V2, 8);
    a.mov_imm64(R::V1, 0);
    a.cmp_reg_imm32(R::V2, 2);
    a.jcc_label(C::Equal, have_next);
    a.load_ptr_disp32(R::V1, R::V6, offset_of!(Q, value) as i32);
    a.bind_text_label(have_next);
    a.store_ptr_disp32(R::V7, offset_of!(Change, next) as i32, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(S, scope) as i32);
    a.store_ptr_disp32(R::V7, offset_of!(Change, scope) as i32, R::V0);
    a.load_rbp_slot(R::V0, 24);
    a.add_reg_imm32(R::V0, offset_of!(M, adopted_phase) as i32);
    a.atomic_load_acquire(R::V0, R::V0);
    a.store_ptr_disp32(R::V7, offset_of!(Change, phase) as i32, R::V0);
    a.add_reg_imm32(R::V10, 1);
    a.store_ptr_disp32(R::V11, offset_of!(B, top) as i32, R::V10);
    a.mov_reg_reg(R::V0, R::V11);
    a.add_reg_imm32(R::V0, offset_of!(B, published) as i32);
    a.atomic_store_release(R::V0, R::V10);
    a.atomic_store_release(R::V9, R::V1);
    a.cmp_reg_imm32(R::V2, 2);
    a.jcc_label(C::NotEqual, publish_revision);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V9, offset_of!(Cell, active) as i32, R::V0);
    a.bind_text_label(publish_revision);
    a.load_rbp_slot(R::V0, 24);
    a.add_reg_imm32(R::V0, offset_of!(M, root_revision) as i32);
    a.load_rbp_slot(R::V1, 32);
    a.atomic_store_release(R::V0, R::V1);
    a.jmp_label(success);
    a.bind_text_label(read);
    if cancel_empty {
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
        a.store_ptr_disp32(R::V9, offset_of!(Cell, active) as i32, R::V0);
    } else {
        a.store_ptr_disp32(R::V6, offset_of!(Q, value) as i32, R::V0);
    }
    a.jmp_label(success);
    a.bind_text_label(retire);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(S, registration) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.mov_reg_reg(R::V0, R::V8);
    a.add_reg_imm32(R::V0, offset_of!(S, state) as i32);
    a.mov_imm64(R::V1, 2);
    a.atomic_store_release(R::V0, R::V1);
    a.jmp_label(success);
    a.bind_text_label(reserve);
    a.cmp_reg_reg(R::V10, R::V11);
    a.jcc_label(C::Equal, full);
    a.mov_reg_reg(R::V0, R::V10);
    a.shl_reg_imm8(R::V0, 5);
    a.add_reg_reg(R::V9, R::V0);
    a.load_ptr_disp32(R::V0, R::V9, offset_of!(Cell, active) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.atomic_load_acquire(R::V0, R::V9);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V9, offset_of!(Cell, reuse_generation) as i32);
    a.mov_imm64(R::V0, u64::MAX);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Equal, exhausted);
    a.add_reg_imm32(R::V1, 1);
    a.store_ptr_disp32(R::V9, offset_of!(Cell, reuse_generation) as i32, R::V1);
    a.store_ptr_disp32(
        R::V6,
        (offset_of!(Q, handle) + offset_of!(H, reuse_generation)) as i32,
        R::V1,
    );
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V9, offset_of!(Cell, active) as i32, R::V0);
    a.store_ptr_disp32(
        R::V6,
        (offset_of!(Q, handle) + offset_of!(H, cell)) as i32,
        R::V9,
    );
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(S, scope) as i32);
    a.store_ptr_disp32(
        R::V6,
        (offset_of!(Q, handle) + offset_of!(H, scope)) as i32,
        R::V0,
    );
    a.add_reg_imm32(R::V10, 1);
    a.mov_reg_reg(R::V0, R::V8);
    a.add_reg_imm32(R::V0, offset_of!(S, used) as i32);
    a.atomic_store_release(R::V0, R::V10);
    a.bind_text_label(success);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    for (label, code) in [(invalid, 1), (full, 2), (flush, 3), (exhausted, 4)] {
        a.bind_text_label(label);
        a.mov_imm64(R::V0, code);
        a.jmp_label(done);
    }
    a.bind_text_label(done);
    a.leave();
}

/// `(V7=request) -> OK/INVALID/MORE 0/1/2`. Transfers bounded work into
/// caller-owned, phase-protected output; drain is a sole SPSC consumer.
#[allow(dead_code)]
pub(crate) fn emit_transfer<E: PortableAsm>(a: &mut E, drain: bool) {
    use crate::gc_abi::{RootDrain as Drain, RootScan as Scan};
    let invalid = a.create_text_label();
    let count_ready = a.create_text_label();
    let copy = a.create_text_label();
    let finish = a.create_text_label();
    let more = a.create_text_label();
    let done = a.create_text_label();
    let output = if drain {
        offset_of!(Drain, output)
    } else {
        offset_of!(Scan, output)
    };
    let capacity = if drain {
        offset_of!(Drain, capacity)
    } else {
        offset_of!(Scan, capacity)
    };
    let written = if drain {
        offset_of!(Drain, written)
    } else {
        offset_of!(Scan, written)
    };
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 16);
    // Request and output storage must be disjoint from the source.
    for field in [0, output] {
        a.load_ptr_disp32(R::V0, R::V7, field as i32);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, invalid);
        a.and_reg_imm32(R::V0, 7);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.load_ptr_disp32(R::V0, R::V7, capacity as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.cmp_reg_imm32(R::V0, 256);
    a.jcc_label(C::Above, invalid);
    a.shl_reg_imm8(R::V0, if drain { 5 } else { 3 });
    a.load_ptr_disp32(R::V1, R::V7, output as i32);
    a.add_reg_reg(R::V0, R::V1);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid);
    a.load_ptr_disp32(R::V6, R::V7, 0);
    if !drain {
        for field in [offset_of!(S, owner), offset_of!(S, scope)] {
            a.load_ptr_disp32(R::V0, R::V6, field as i32);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::Equal, invalid);
        }
        acquire(a, R::V0, R::V6, offset_of!(S, state));
        a.sub_reg_imm8(R::V0, 1);
        a.cmp_reg_imm32(R::V0, 1);
        a.jcc_label(C::Above, invalid);
    }
    a.load_ptr_disp32(
        R::V8,
        R::V6,
        if drain {
            offset_of!(B, entries)
        } else {
            offset_of!(S, cells)
        } as i32,
    );
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V8);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(
        R::V9,
        R::V6,
        if drain {
            offset_of!(B, capacity)
        } else {
            offset_of!(S, capacity)
        } as i32,
    );
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, invalid);
    a.mov_imm64(R::V0, u32::MAX as u64);
    a.cmp_reg_reg(R::V9, R::V0);
    a.jcc_label(C::Above, invalid);
    a.mov_reg_reg(R::V0, R::V9);
    a.shl_reg_imm8(R::V0, 5);
    a.add_reg_reg(R::V0, R::V8);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::Below, invalid);
    if drain {
        acquire(a, R::V10, R::V6, offset_of!(B, consumed));
        acquire(a, R::V11, R::V6, offset_of!(B, published));
    } else {
        a.load_ptr_disp32(R::V10, R::V7, offset_of!(Scan, cursor) as i32);
        acquire(a, R::V11, R::V6, offset_of!(S, used));
        a.cmp_reg_reg(R::V11, R::V9);
        a.jcc_label(C::Above, invalid);
    }
    a.cmp_reg_reg(R::V10, R::V11);
    a.jcc_label(C::Above, invalid);
    a.mov_reg_reg(R::V0, R::V11);
    a.sub_reg_reg(R::V0, R::V10);
    if drain {
        a.cmp_reg_reg(R::V0, R::V9);
        a.jcc_label(C::Above, invalid);
    }
    a.load_ptr_disp32(R::V1, R::V7, capacity as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, count_ready);
    a.mov_reg_reg(R::V0, R::V1);
    a.bind_text_label(count_ready);
    a.store_rbp_slot(8, R::V0);
    a.load_ptr_disp32(R::V6, R::V7, output as i32);
    a.mov_imm64(R::V1, 0);
    a.bind_text_label(copy);
    a.load_rbp_slot(R::V0, 8);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Equal, finish);
    a.mov_reg_reg(R::V0, R::V10);
    a.add_reg_reg(R::V0, R::V1);
    if drain {
        a.div_unsigned(R::V9);
    } else {
        a.mov_reg_reg(R::V2, R::V0);
    }
    a.shl_reg_imm8(R::V2, 5);
    a.add_reg_reg(R::V2, R::V8);
    if drain {
        for field in [0, 8, 16, 24] {
            a.load_ptr_disp32(R::V0, R::V2, field);
            a.store_ptr_disp32(R::V6, field, R::V0);
        }
    } else {
        a.atomic_load_acquire(R::V0, R::V2);
        a.store_ptr_disp32(R::V6, 0, R::V0);
    }
    a.add_reg_imm32(R::V6, if drain { 32 } else { 8 });
    a.add_reg_imm32(R::V1, 1);
    a.jmp_label(copy);
    a.bind_text_label(finish);
    a.store_ptr_disp32(R::V7, written as i32, R::V1);
    a.add_reg_reg(R::V10, R::V1);
    if drain {
        a.load_ptr_disp32(R::V0, R::V7, 0);
        a.add_reg_imm32(R::V0, offset_of!(B, consumed) as i32);
        a.atomic_store_release(R::V0, R::V10);
    } else {
        a.store_ptr_disp32(R::V7, offset_of!(Scan, cursor) as i32, R::V10);
    }
    a.cmp_reg_reg(R::V10, R::V11);
    a.jcc_label(C::NotEqual, more);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(more);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 1);
    a.bind_text_label(done);
    a.leave();
}
