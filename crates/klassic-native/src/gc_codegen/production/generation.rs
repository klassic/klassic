//! Authoritative page-generation lookup for the production barriers.
//! The shared pool publishes roles before allocation and returns them only
//! after source cleanup/quiescence. Free pages match neither generation. This
//! lookup alone does not authorize object access or general N-owner reuse.
use super::{HEAP_BYTES, HEAP_PAGE_BYTES};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
pub(super) const YOUNG: u64 = 1;
pub(super) const OLD: u64 = 2;
/// Dedicated nonmoving Old pages; excluded from ordinary Old source runs.
pub(super) const LARGE: u64 = 3;
#[cfg(test)]
pub(super) const ENTRIES: u64 = HEAP_BYTES / HEAP_PAGE_BYTES;
const _: () = assert!(HEAP_BYTES.is_multiple_of(HEAP_PAGE_BYTES));

/// Equal iff the managed offset belongs to `kind`; every register survives.
/// Bounds are checked before the acquire read. Guard/outside/unknown entries
/// compare unequal. The caller still owns object-start/lifetime validation.
pub(super) fn test<E: PortableAsm>(a: &mut E, off: R, kind: u64) {
    lookup(a, off, kind, true);
}

/// Exact physical role for a reservation destination. Dedicated large Old
/// pages are Old for tracing, but cannot receive an ordinary moving copy.
pub(super) fn test_role<E: PortableAsm>(a: &mut E, off: R, kind: u64) {
    lookup(a, off, kind, false);
}

fn lookup<E: PortableAsm>(a: &mut E, off: R, kind: u64, include_large_old: bool) {
    assert!(matches!(kind, YOUNG | OLD | LARGE));
    assert!(!matches!(off, R::V3 | R::V4 | R::V5));
    let scratch = if off == R::V11 { R::V10 } else { R::V11 };
    let invalid = a.create_text_label();
    let compare = a.create_text_label();
    a.push_reg(scratch);
    a.mov_reg_reg(scratch, off);
    a.cmp_reg_imm32(scratch, HEAP_PAGE_BYTES as i32);
    a.jcc_label(C::Below, invalid);
    a.cmp_reg_imm32(scratch, super::geometry(a).heap_bytes as i32);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.shr_reg_imm8(scratch, 17);
    a.shl_reg_imm8(scratch, 3);
    a.add_reg_reg(scratch, R::V3);
    a.add_reg_imm32(scratch, super::layout(a).generations as i32);
    a.atomic_load_acquire(scratch, scratch);
    a.jmp_label(compare);
    a.bind_text_label(invalid);
    a.mov_imm64(scratch, 0);
    a.bind_text_label(compare);
    if kind == OLD && include_large_old {
        let ordinary = a.create_text_label();
        a.cmp_reg_imm8(scratch, LARGE as i8);
        a.jcc_label(C::NotEqual, ordinary);
        a.mov_imm64(scratch, OLD);
        a.bind_text_label(ordinary);
    }
    a.cmp_reg_imm32(scratch, kind as i32);
    a.pop_reg(scratch);
}
