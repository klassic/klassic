//! One owned release job between source retirement and Free-role publication.
//! The source's roots, retained Old receivers and copier guards are already
//! quiescent on this one-owner path. This is not an N-mutator lease protocol.
//! OS release runs after dropping the collector lock. Its charge and directory
//! ownership survive failure; successful accounting/publication reacquires it.
use super::*;
use crate::gc_codegen::production::{lea_rt, load_rt, store_rt};

pub(super) const UNMAPS: usize = offset_of!(Runtime, metadata) + offset_of!(State, unmaps);
pub(super) const FAILURES: usize =
    offset_of!(Runtime, metadata) + offset_of!(State, unmap_failures);
pub(super) const STATE: usize = offset_of!(Runtime, metadata) + offset_of!(State, release_state);
const ENTRY: usize = offset_of!(Runtime, metadata) + offset_of!(State, release_entry);
const ADDRESS: usize = offset_of!(Runtime, metadata) + offset_of!(State, release_address);
#[cfg(test)]
const FAIL_AFTER: usize = offset_of!(Runtime, metadata) + offset_of!(State, release_fail_after);
// 0 empty, 1 published, 2 claimed (OS call in flight), 3 released, awaiting
// the retirement cursor's acknowledgement before its role becomes Free.

pub(super) fn after_unlock<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    lea_rt(a, R::V1, STATE);
    a.atomic_load_acquire(R::V0, R::V1);
    let done = a.create_text_label();
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, done);
    a.call_label(l.metadata_reclaim);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.metadata_release_failed);
    a.bind_text_label(done);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
}

/// Called under the collector lock for a source role word V1 (stage V0).
/// Preserve V0/V1/V2/V8. An outstanding release keeps this cursor and its
/// role occupied; one source mapping is queued per cleanup quantum at most.
pub(super) fn retire_page<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    paused: E::TextLabel,
) {
    let ready = a.create_text_label();
    let blocked = a.create_text_label();
    let empty = a.create_text_label();
    let done = a.create_text_label();
    for r in [R::V0, R::V1, R::V2, R::V8] {
        a.push_reg(r);
    }
    // Role directory entry i+1 corresponds to copy backing i+1.
    a.mov_reg_reg(R::V9, R::V1);
    a.sub_reg_reg(R::V9, R::V3);
    a.sub_reg_imm32(R::V9, super::super::layout(a).generations as i32);
    a.add_reg_reg(R::V9, R::V3);
    a.add_reg_imm32(R::V9, super::super::layout(a).copy_marks as i32);
    lea_rt(a, R::V8, STATE);
    a.atomic_load_acquire(R::V0, R::V8);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    a.cmp_reg_imm8(R::V0, 3);
    a.jcc_label(C::NotEqual, blocked);
    load_rt(a, R::V0, ENTRY);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_imm64(R::V0, 0);
    a.atomic_store_release(R::V8, R::V0);
    a.bind_text_label(empty);
    a.atomic_load_acquire(R::V7, R::V9);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, ready);
    a.cmp_reg_imm8(R::V7, 1); // no installer can remain after source quiescence
    a.jcc_label(C::Equal, l.fail);
    for f in [super::super::RT_PHASE_OPEN, super::super::RT_CS_KIND] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    super::super::copy_claims::require_empty(a, l);
    store_rt(a, ENTRY, R::V9);
    store_rt(a, ADDRESS, R::V7);
    a.mov_imm64(R::V0, 1);
    a.atomic_store_release(R::V8, R::V0);
    a.bind_text_label(blocked);
    for r in [R::V8, R::V2, R::V1, R::V0] {
        a.pop_reg(r);
    }
    a.jmp_label(paused);
    a.bind_text_label(ready);
    for r in [R::V8, R::V2, R::V1, R::V0] {
        a.pop_reg(r);
    }
    a.jmp_label(done);
    a.bind_text_label(done);
}

/// Outside the collector lock: claim one job and attempt the real OS release.
/// V0=bool; all other portable registers survive. Failure leaves the job
/// claimed, its directory pointer intact and both charges unchanged. The
/// normal unlock caller reports the distinct terminal resource diagnostic.
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.metadata_reclaim);
    let saved = [R::V1, R::V2, R::V6, R::V7, R::V8, R::V9, R::V10, R::V11];
    for r in saved {
        a.push_reg(r);
    }
    let skipped = a.create_text_label();
    let failed = a.create_text_label();
    let restore = a.create_text_label();
    lea_rt(a, R::V9, STATE);
    a.mov_imm64(R::V0, 1);
    a.mov_imm64(R::V1, 2);
    a.atomic_compare_exchange(R::V9, R::V1);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, skipped);
    load_rt(a, R::V7, ADDRESS);
    a.mov_imm64(R::V6, copy_bytes(a));
    #[cfg(test)]
    {
        let ordinary = a.create_text_label();
        load_rt(a, R::V0, FAIL_AFTER);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, ordinary);
        a.sub_reg_imm32(R::V0, 1);
        store_rt(a, FAIL_AFTER, R::V0);
        a.jcc_label(C::NotEqual, ordinary);
        a.mov_imm64(R::V7, u64::MAX); // Real OS failure; the recorded owned address is unchanged.
        a.bind_text_label(ordinary);
    }
    a.call_label(l.memory.release);
    // OS calls never hold the collector ticket. Publish ownership/accounting
    // together with other collector state so the diagnostic snapshot is coherent.
    super::super::worker::lock_raw(a, l);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, failed);
    load_rt(a, R::V7, ENTRY);
    load_rt(a, R::V0, ADDRESS);
    a.atomic_load_acquire(R::V1, R::V7);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_imm64(R::V0, 0);
    a.atomic_store_release(R::V7, R::V0);
    a.mov_imm64(R::V8, 0_u64.wrapping_sub(copy_bytes(a)));
    adjust(a, COMMITTED);
    adjust(a, CHARGED);
    a.mov_imm64(R::V8, 1);
    adjust(a, UNMAPS);
    lea_rt(a, R::V9, STATE);
    a.mov_imm64(R::V0, 3);
    a.atomic_store_release(R::V9, R::V0);
    super::super::worker::unlock_raw(a, l);
    a.jmp_label(skipped);
    a.bind_text_label(failed);
    a.mov_imm64(R::V8, 1);
    adjust(a, FAILURES);
    super::super::worker::unlock_raw(a, l);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(restore);
    a.bind_text_label(skipped);
    a.mov_imm64(R::V0, 1);
    a.bind_text_label(restore);
    for r in saved.into_iter().rev() {
        a.pop_reg(r);
    }
    a.ret();
}

#[cfg(test)]
pub(crate) fn release_offsets() -> [i32; 5] {
    [STATE, ENTRY, ADDRESS, UNMAPS, FAILURES].map(d)
}
#[cfg(test)]
pub(crate) fn fail_release_next<E: PortableAsm>(a: &mut E) {
    store_rt_imm(a, FAIL_AFTER, 1);
}
#[cfg(test)]
pub(crate) fn retry_release<E: PortableAsm>(a: &mut E) {
    store_rt_imm(a, STATE, 1);
}
