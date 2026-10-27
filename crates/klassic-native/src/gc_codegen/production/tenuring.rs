//! Adaptive Young promotion age. One owner freezes a threshold at a flip;
//! real owner/worker copiers atomically account winning Young-source copies.
use super::{Labels, Runtime, load_rt, store_rt};
use crate::gc_abi::{MAX_AGE, MarkRecord};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;
pub(super) const MIN: u64 = 2;
#[repr(C)]
pub(super) struct State {
    threshold: u64,
    frozen: u64,
    up: u64,
    down: u64,
    young_samples: u64,
    full_samples: u64,
    updates: u64,
    last_young: u64,
    last_promoted: u64,
    owner_young: u64,
    worker_young: u64,
    owner_promoted: u64,
    worker_promoted: u64,
}
macro_rules! field { ($($name:ident:$member:ident),* $(,)?)=> { $(
    pub(super) const $name:usize=offset_of!(Runtime,tenuring.$member);
)* }; }
field! { THRESHOLD:threshold,FROZEN:frozen,UP:up,DOWN:down,
YOUNG_SAMPLES:young_samples,FULL_SAMPLES:full_samples,UPDATES:updates,
LAST_YOUNG:last_young,LAST_PROMOTED:last_promoted,
OWNER_YOUNG:owner_young,WORKER_YOUNG:worker_young,
OWNER_PROMOTED:owner_promoted,WORKER_PROMOTED:worker_promoted }
/// The previous relocation is complete and the collector lock is held.
pub(super) fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, THRESHOLD);
    a.cmp_reg_imm8(R::V0, MIN as i8);
    a.jcc_label(C::Below, l.fail);
    a.cmp_reg_imm8(R::V0, MAX_AGE as i8);
    a.jcc_label(C::Above, l.fail);
    store_rt(a, FROZEN, R::V0);
    a.mov_imm64(R::V0, 0);
    for f in [OWNER_YOUNG, WORKER_YOUNG, OWNER_PROMOTED, WORKER_PROMOTED] {
        store_rt(a, f, R::V0);
    }
}
/// V0 is the canonical source payload (preserved). Only a prospective Young
/// age meeting the frozen threshold promotes. Full collection overrides this.
pub(super) fn promote<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, yes: E::TextLabel) {
    a.load_ptr_disp32(R::V2, R::V0, super::copy::MARK_RECORD);
    a.load_ptr_disp32(R::V2, R::V2, offset_of!(MarkRecord, age) as i32);
    a.cmp_reg_imm8(R::V2, MAX_AGE as i8);
    a.jcc_label(C::Above, l.fail);
    a.add_reg_imm32(R::V2, 1);
    load_rt(a, R::V1, FROZEN);
    a.cmp_reg_reg(R::V2, R::V1);
    a.jcc_label(C::AboveOrEqual, yes);
}
/// The forwarding winner is already published. Called before clearing the
/// owner's copying obligation; no publication, allocation or safepoint here.
pub(super) fn winner<E: PortableAsm>(a: &mut E, src: usize, new: usize, worker: bool) {
    let done = a.create_text_label();
    let promoted = a.create_text_label();
    let sized = a.create_text_label();
    load_rt(a, R::V6, src);
    load_rt(a, R::V1, super::RT_HEAP_BASE);
    a.mov_reg_reg(R::V2, R::V6);
    a.sub_reg_reg(R::V2, R::V1);
    super::generation::test(a, R::V2, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, done);
    a.load_ptr_disp32(R::V0, R::V6, super::copy::PAYLOAD);
    a.add_reg_imm32(R::V0, 15);
    a.and_reg_imm32(R::V0, -16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V0, super::copy::HEADER);
    load_rt(a, R::V2, new);
    a.shr_reg_imm8(R::V2, 16);
    super::generation::test(a, R::V2, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, promoted);
    add(a, if worker { WORKER_YOUNG } else { OWNER_YOUNG });
    a.jmp_label(done);
    a.bind_text_label(promoted);
    add(
        a,
        if worker {
            WORKER_PROMOTED
        } else {
            OWNER_PROMOTED
        },
    );
    a.bind_text_label(done);
}
fn add<E: PortableAsm>(a: &mut E, field: usize) {
    super::copy::add_bytes(a, field, R::V0);
}
/// Fixed-count, race-free owner observation after both copy frontiers close.
pub(super) fn observe<E: PortableAsm>(a: &mut E) {
    for (dst, owner, worker) in [
        (LAST_YOUNG, OWNER_YOUNG, WORKER_YOUNG),
        (LAST_PROMOTED, OWNER_PROMOTED, WORKER_PROMOTED),
    ] {
        load_rt(a, R::V0, owner);
        load_rt(a, R::V1, worker);
        a.add_reg_reg(R::V0, R::V1);
        store_rt(a, dst, R::V0);
    }
    let normal = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, super::RT_FULL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, normal);
    super::copy::bump(a, FULL_SAMPLES, R::V0);
    // Preserve the preceding normal sample: forced promotion supplies no
    // survival evidence. The next normal sample still validates both signals.
    a.jmp_label(done); // Forced promotion is not policy evidence.
    a.bind_text_label(normal);
    super::copy::bump(a, YOUNG_SAMPLES, R::V0);
    super::old_runs::used(a, R::V2);
    choose(a);
    a.bind_text_label(done);
}
fn clear_votes<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V0, 0);
    store_rt(a, UP, R::V0);
    store_rt(a, DOWN, R::V0);
}
/// V2 = Old reserved bytes. Shared with directed controller-input probes.
/// Two consecutive low-live/high-Old samples raise age; two high-live samples
/// lower it. Intermediate or opposite signals reset the corresponding votes.
pub(super) fn choose<E: PortableAsm>(a: &mut E) {
    let high = a.create_text_label();
    let neutral = a.create_text_label();
    let lower = a.create_text_label();
    let change = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, LAST_YOUNG);
    load_rt(a, R::V1, LAST_PROMOTED);
    a.add_reg_reg(R::V0, R::V1);
    super::capacity::bytes(a, R::V6);
    a.shr_reg_imm8(R::V6, 3);
    a.mov_reg_reg(R::V7, R::V6);
    a.shl_reg_imm8(R::V7, 1);
    a.add_reg_reg(R::V7, R::V6);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::AboveOrEqual, high);
    a.cmp_reg_reg(R::V0, R::V6);
    a.jcc_label(C::Above, neutral);
    a.cmp_reg_imm32(R::V2, (super::geometry(a).old_max * 3 / 4) as i32);
    a.jcc_label(C::Below, neutral);
    a.mov_imm64(R::V0, 0);
    store_rt(a, DOWN, R::V0);
    super::copy::bump(a, UP, R::V0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Below, done);
    clear_votes(a);
    load_rt(a, R::V2, THRESHOLD);
    a.cmp_reg_imm8(R::V2, MAX_AGE as i8);
    a.jcc_label(C::AboveOrEqual, done);
    a.add_reg_imm32(R::V2, 1);
    a.jmp_label(change);
    a.bind_text_label(high);
    a.mov_imm64(R::V0, 0);
    store_rt(a, UP, R::V0);
    super::copy::bump(a, DOWN, R::V0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::AboveOrEqual, lower);
    a.jmp_label(done);
    a.bind_text_label(lower);
    clear_votes(a);
    load_rt(a, R::V2, THRESHOLD);
    a.cmp_reg_imm8(R::V2, MIN as i8);
    a.jcc_label(C::Below, done);
    a.jcc_label(C::Equal, done);
    a.sub_reg_imm8(R::V2, 1);
    a.jmp_label(change);
    a.bind_text_label(neutral);
    clear_votes(a);
    a.jmp_label(done);
    a.bind_text_label(change);
    store_rt(a, THRESHOLD, R::V2);
    super::copy::bump(a, UPDATES, R::V0);
    a.bind_text_label(done);
}
