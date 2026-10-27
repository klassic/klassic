//! Touch only newly committed payload, before its accessible tag is published.
use super::*;
use crate::gc_codegen::production::{load_rt, store_rt};

/// V7=base, V6=nonempty aligned length. Preserve all registers. The commit
/// lock (or single-threaded startup) covers the range and cumulative counter.
/// All direct targets use the metadata mapping alignment as their OS page
/// granule: 4 KiB on x86 Linux/Windows, 16 KiB on Darwin ARM64.
pub(super) fn emit<E: PortableAsm>(a: &mut E) {
    for r in [R::V0, R::V1, R::V6, R::V7] {
        a.push_reg(r);
    }
    let step = a.gc_metadata_alignment() as i32;
    a.mov_reg_reg(R::V1, R::V6);
    a.mov_imm64(R::V0, 0);
    let next = a.create_text_label();
    a.bind_text_label(next);
    a.store_ptr_disp32(R::V7, 0, R::V0);
    a.add_reg_imm32(R::V7, step);
    a.sub_reg_imm32(R::V6, step);
    a.jcc_label(C::NotEqual, next);
    load_rt(a, R::V0, PRETOUCHED);
    a.add_reg_reg(R::V0, R::V1);
    store_rt(a, PRETOUCHED, R::V0);
    for r in [R::V7, R::V6, R::V1, R::V0] {
        a.pop_reg(r);
    }
}
