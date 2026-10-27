//! Bounded idle maintenance, called with the collector lock held.
//! The clock argument makes elapsed-time decisions deterministic in probes.
use super::*;
use crate::gc_codegen::production::{load_rt, store_rt};
const QUANTUM: u64 = 64;

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.uncommit_poll);
    let done = a.create_text_label();
    let next = a.create_text_label();
    let advance = a.create_text_label();
    let observed = a.create_text_label();
    let eligible = a.create_text_label();
    let clear = a.create_text_label();
    let in_range = a.create_text_label();
    let saved = [R::V1, R::V2, R::V6, R::V7, R::V8, R::V9, R::V10, R::V11];
    for r in saved {
        a.push_reg(r);
    }
    a.mov_reg_reg(R::V11, R::V0);
    // A failed monotonic clock never authorizes reclaiming payload.
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, done);
    a.mov_imm64(R::V6, QUANTUM);
    load_rt(a, R::V10, commit::SCAN_CURSOR);
    a.bind_text_label(next);
    a.add_reg_imm32(R::V10, 1);
    a.cmp_reg_imm32(R::V10, geometry(a).pages as i32 + 1);
    a.jcc_label(C::Below, in_range);
    a.mov_imm64(R::V10, 1);
    a.bind_text_label(in_range);
    a.mov_reg_reg(R::V9, R::V10);
    a.shl_reg_imm8(R::V9, 3);
    a.add_reg_reg(R::V9, R::V3);
    a.load_ptr_disp32(R::V0, R::V9, layout(a).generations as i32);
    a.add_reg_imm32(R::V9, layout(a).free_since as i32);
    a.test_reg_reg(R::V0, R::V0); // Free role is zero.
    a.jcc_label(C::NotEqual, clear);
    a.load_ptr_disp32(R::V0, R::V9, 0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, observed);
    a.store_ptr_disp32(R::V9, 0, R::V11);
    if a.gc_uncommit_delay_ns() == 0 {
        a.jmp_label(eligible);
    } else {
        a.jmp_label(advance);
    }
    a.bind_text_label(observed);
    // A backwards/reset clock restarts the waiting period.
    a.cmp_reg_reg(R::V11, R::V0);
    let elapsed = a.create_text_label();
    a.jcc_label(C::AboveOrEqual, elapsed);
    a.store_ptr_disp32(R::V9, 0, R::V11);
    a.jmp_label(advance);
    a.bind_text_label(elapsed);
    a.mov_reg_reg(R::V1, R::V11);
    a.sub_reg_reg(R::V1, R::V0);
    a.mov_imm64(R::V0, a.gc_uncommit_delay_ns());
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, advance);
    a.bind_text_label(eligible);
    if a.gc_large_pages() == crate::GcLargePages::Required {
        commit::grouped::check_idle_age(a, advance);
    }
    a.mov_reg_reg(R::V0, R::V10);
    // The primitive rechecks ownership, Ready state, forwarding, source tags,
    // accessibility and minimum under the commit lock before the OS call.
    a.call_label(l.uncommit_heap_page);
    a.jmp_label(advance);
    a.bind_text_label(clear);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V9, 0, R::V0);
    a.bind_text_label(advance);
    a.sub_reg_imm8(R::V6, 1);
    a.jcc_label(C::NotEqual, next);
    store_rt(a, commit::SCAN_CURSOR, R::V10);
    a.bind_text_label(done);
    for r in saved.into_iter().rev() {
        a.pop_reg(r);
    }
    a.ret();
}

/// Allocation safepoint fallback when no background worker is alive. The
/// caller holds no collector lock; all input registers survive. Sample once
/// per 64 maintenance safepoints (allocation or explicit finish).
pub(super) fn owner_poll<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    emit_owner_poll(a, l, false);
}
/// Explicit finish has already drained cleanup and holds the collector lock.
/// Observe newly Free pages before the next collection can reserve them.
pub(super) fn owner_poll_locked<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    emit_owner_poll(a, l, true);
}
fn emit_owner_poll<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, locked: bool) {
    let done = a.create_text_label();
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    if l.worker.is_some() {
        load_rt(a, R::V0, RT_WORKER_ALIVE);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
    }
    load_rt(a, R::V0, commit::OWNER_TICK);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, commit::OWNER_TICK, R::V0);
    a.and_reg_imm32(R::V0, 63);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    let saved = [R::V1, R::V2, R::V6, R::V7, R::V8, R::V9, R::V10, R::V11];
    for r in saved {
        a.push_reg(r);
    }
    if let Some(w) = &l.worker {
        a.call_label(w.hooks.clock);
    } else {
        a.plat_read_monotonic_ns();
    }
    if !locked {
        worker::lock(a, l);
    }
    a.call_label(l.uncommit_poll);
    if !locked {
        worker::unlock(a, l);
    }
    for r in saved.into_iter().rev() {
        a.pop_reg(r);
    }
    a.bind_text_label(done);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
}

#[cfg(test)]
mod tests {
    #[test]
    fn public_uncommit_delay_requires_generational_mode() {
        let config = crate::NativeCompilerConfig {
            gc_uncommit_delay_ns: Some(0),
            ..Default::default()
        };
        let e = crate::compile_source_for_target("delay.kl", "1", config).unwrap_err();
        assert!(e.diagnostic().message.contains("uncommit"));
    }
}
