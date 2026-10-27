//! Inspect the actual source Old metadata without fabricating allocations.
use super::super::*;
use crate::gc_codegen::production::{lea_rt, load_rt};
/// V0 = payload; erase only its exact publication bit for a negative probe.
pub(crate) fn clear_owner_start<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    a.sub_reg_imm32(R::V0, HEAP_PAGE_BYTES as i32);
    a.shr_reg_imm8(R::V0, 10);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, super::super::layout(a).starts as i32);
    // This fixture's sole Old object starts at payload granule two.
    a.mov_imm64(R::V1, 0);
    a.atomic_store_release(R::V0, R::V1);
}
/// V0 = address of the source's first start word or second card hint.
pub(crate) fn old_owner_word<E: PortableAsm>(a: &mut E, hint: bool) {
    load_rt(a, R::V0, RT_OLD_FROM);
    if !hint {
        a.sub_reg_imm32(R::V0, HEAP_PAGE_BYTES as i32);
    }
    a.shr_reg_imm8(R::V0, 10);
    if hint {
        a.add_reg_imm32(R::V0, 1);
    }
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(
        R::V0,
        if hint {
            super::super::layout(a).owner_cards
        } else {
            super::super::layout(a).starts
        } as i32,
    );
}

/// Real OS park after a u64 work sequence crosses the futex-word boundary.
pub(crate) fn park_wrapped_work_sequence(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    load_runtime(g, runtime.data.runtime);
    worker::lock(g, &runtime.labels);
    store_rt_imm(g, RT_WORK_SEQ, 0x1_0000_0005);
    worker::unlock(g, &runtime.labels);
    lea_rt(g, R::V7, RT_WORK_SEQ);
    g.mov_imm64(R::V6, 0x1_0000_0005);
    g.push_reg(R::OwnerContext);
    lea_rt(g, R::OwnerContext, super::super::worker_pool::slot(0));
    // The startup broadcast can leave one auto-reset notification pending.
    // A second unchanged-sequence wait exercises the actual timeout path.
    for _ in 0..2 {
        lea_rt(g, R::V7, RT_WORK_SEQ);
        g.mov_imm64(R::V6, 0x1_0000_0005);
        g.call_label(runtime.labels.worker_hooks().unwrap().park);
    }
    g.pop_reg(R::OwnerContext);
    g.gc_gen = Some(runtime);
}
