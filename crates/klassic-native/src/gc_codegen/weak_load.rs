//! Bounded weak-slot publication into a registered strong root; gc_weak_load.h.
use super::*;
use crate::gc_abi::{
    HandshakeState as H, MetadataLeaseRequest as L, RootRequest as O, RootedLoadAdmission as A,
    ThreadRegistration as P, WeakLoadGate as Gt, WeakLoadRequest as T, WeakSlot as W,
};
use std::mem::size_of;

#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let ret: Vec<_> = (0..6).map(|_| a.create_text_label()).collect();
    let done = a.create_text_label();
    let nonnull = a.create_text_label();
    let root_fail = a.create_text_label();
    let roots = a.create_text_label();
    let metadata = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    for _ in 0..2 {
        a.sub_reg_imm8(R::V4, 112);
    }
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    for (slot, off) in [(KIND, 160), (104, 192)] {
        a.mov_reg_reg(R::V0, R::V5);
        a.sub_reg_imm32(R::V0, off);
        a.store_rbp_slot(slot, R::V0);
    }
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(T, $f), 0)
        };
    }
    b.branch(local(OP), n(0), C::NotEqual, ret[3]);
    for slot in [G, Q] {
        b.pointer(local(slot), ret[3]);
    }
    b.branch(field(G, offset_of!(Gt, closed), 0), n(1), C::Above, ret[3]);
    b.pointer(field(G, offset_of!(Gt, pool), 0), ret[3]);
    for value in [q!(admission), q!(context), q!(slot), q!(output_root)] {
        b.pointer(value, ret[3]);
    }
    b.branch(q!(serial), n(0), C::Equal, ret[3]);
    b.branch(
        field(G, offset_of!(Gt, closed), 0),
        n(0),
        C::NotEqual,
        ret[2],
    );
    validate_admission(&mut b, q!(admission), q!(context), ret[3], ret[2]);
    b.set(local(PREV), q!(slot));
    b.branch(
        field(PREV, offset_of!(W, gate), 0),
        local(G),
        C::NotEqual,
        ret[3],
    );
    b.branch(
        field(PREV, offset_of!(W, serial), 0),
        q!(serial),
        C::NotEqual,
        ret[3],
    );
    b.set(local(NEXT), q!(output_root));
    b.branch(
        field(NEXT, offset_of!(O, value), 0),
        n(0),
        C::NotEqual,
        ret[3],
    );
    b.set(local(SEG), field(NEXT, offset_of!(O, segment), 0));
    b.pointer(local(SEG), ret[3]);
    b.set(
        local(112),
        field(SEG, offset_of!(RootSegment, registration), 0),
    );
    b.pointer(local(112), ret[3]);
    for (left, right) in [
        (
            field(112, offset_of!(RootRegistration, segment), 0),
            local(SEG),
        ),
        (
            field(112, offset_of!(RootRegistration, scope), 0),
            field(SEG, offset_of!(RootSegment, scope), 0),
        ),
        (
            field(112, offset_of!(RootRegistration, context), 0),
            local(CTX),
        ),
        (
            field(112, offset_of!(RootRegistration, registered), 0),
            n(1),
        ),
    ] {
        b.branch(left, right, C::NotEqual, ret[3]);
    }
    for off in (0..size_of::<O>()).step_by(8) {
        b.set(field(KIND, off, 0), field(NEXT, off, 0));
    }
    call(&mut b, roots, local(CTX), KIND, 5);
    b.branch(local(STATE), n(0), C::NotEqual, root_fail);
    b.branch(
        field(PREV, offset_of!(W, reference), 0),
        n(0),
        C::NotEqual,
        nonnull,
    );
    for off in (0..size_of::<L>()).step_by(8) {
        b.branch(
            field(PREV, offset_of!(W, lease) + off, 0),
            n(0),
            C::NotEqual,
            ret[3],
        );
    }
    b.a.jmp_label(ret[1]);
    b.a.bind_text_label(nonnull);
    for off in (0..size_of::<L>()).step_by(8) {
        b.set(
            field(104, off, 0),
            field(PREV, offset_of!(W, lease) + off, 0),
        );
    }
    call(&mut b, metadata, field(G, offset_of!(Gt, pool), 0), 104, 2);
    b.branch(local(STATE), n(0), C::NotEqual, ret[3]);
    for off in [0, 8] {
        b.branch(
            field(104, off, 0),
            field(PREV, offset_of!(W, lease) + off, 0),
            C::NotEqual,
            ret[3],
        );
    }
    b.set(local(OP), local(KIND));
    super::barrier::reference(&mut b, field(PREV, offset_of!(W, reference), 0), ret[3]);
    b.set(local(KIND), local(OP));
    b.set(local(SEG), field(104, 0, 0));
    b.set(
        local(LEASE),
        shr(field(PREV, offset_of!(W, reference), 0), 16),
    );
    use crate::gc_abi::AllocationPage as Pg;
    macro_rules! page {
        ($f:ident) => {
            field(SEG, offset_of!(Pg, $f), 0)
        };
    }
    b.set(local(USED), field(SEG, offset_of!(Pg, allocated_top), 8));
    for (left, right, condition) in [
        (page!(start), n(crate::gc_abi::HEAP_PAGE_BYTES), C::Below),
        (page!(start), page!(limit), C::AboveOrEqual),
        (page!(limit), n(crate::gc_abi::MAX_OFFSET), C::Above),
        (
            and(page!(start), n(crate::gc_abi::HEAP_PAGE_BYTES - 1)),
            n(0),
            C::NotEqual,
        ),
        (
            and(page!(limit), n(crate::gc_abi::HEAP_PAGE_BYTES - 1)),
            n(0),
            C::NotEqual,
        ),
        (local(USED), page!(start), C::Below),
        (local(USED), page!(limit), C::Above),
        (and(local(USED), n(15)), n(0), C::NotEqual),
        (local(LEASE), add(page!(start), n(32)), C::Below),
        (local(LEASE), local(USED), C::AboveOrEqual),
        (
            page!(starts_words),
            shr(sub(page!(limit), page!(start)), 10),
            C::Below,
        ),
        (page!(starts_words), n(u64::MAX / 8), C::Above),
        (
            add(page!(starts), shl(page!(starts_words), 3)),
            page!(starts),
            C::Below,
        ),
    ] {
        b.branch(left, right, condition, ret[3]);
    }
    b.pointer(page!(starts), ret[3]);
    b.set(local(USED), shr(sub(local(LEASE), page!(start)), 4));
    b.set(local(REG), add(page!(starts), shl(shr(local(USED), 6), 3)));
    b.value(and(local(USED), n(63)));
    b.a.mov_reg_reg(R::V1, R::V0);
    b.a.mov_imm64(R::V2, 1);
    b.a.shl_reg_count(R::V2);
    b.a.load_rbp_slot(R::V0, REG);
    b.a.atomic_load_acquire(R::V0, R::V0);
    b.a.and_reg_reg(R::V0, R::V2);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::Equal, ret[3]);
    b.set(
        field(KIND, offset_of!(O, value), 0),
        field(PREV, offset_of!(W, reference), 0),
    );
    call(&mut b, roots, local(CTX), KIND, 1);
    b.branch(local(STATE), n(0), C::NotEqual, root_fail);
    for off in (0..size_of::<O>()).step_by(8) {
        b.set(field(NEXT, off, 0), field(KIND, off, 0));
    }
    b.a.jmp_label(ret[0]);
    b.a.bind_text_label(root_fail);
    b.branch(local(STATE), n(3), C::Equal, ret[4]);
    b.branch(local(STATE), n(4), C::Equal, ret[5]);
    b.a.jmp_label(ret[3]);
    for (i, label) in ret.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, i as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(roots);
    crate::gc_codegen::roots::emit_step(b.a);
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
}
fn call<B: PortableAsm>(b: &mut Emit<'_, B>, label: B::TextLabel, g: E, q: i32, op: u64) {
    b.value(g);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, q);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
}

// G is the validated gate; admission/context pointers were checked by caller.
// Leaves CTX as context, and uses REG/SEG/LEASE/USED for admission validation.
pub(super) fn validate_admission<B: PortableAsm>(
    b: &mut Emit<'_, B>,
    admission: E,
    context: E,
    invalid: B::TextLabel,
    wait: B::TextLabel,
) {
    let active = b.a.create_text_label();
    let phase = b.a.create_text_label();
    macro_rules! h {
        ($f:ident) => {
            field(LEASE, offset_of!(H, $f), 0)
        };
    }
    macro_rules! p {
        ($f:ident) => {
            field(USED, offset_of!(P, $f), 0)
        };
    }
    b.set(local(REG), admission);
    b.branch(
        field(REG, offset_of!(A, registry), 0),
        field(G, offset_of!(Gt, registry), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(REG, offset_of!(A, handshake), 0),
        field(G, offset_of!(Gt, handshake), 0),
        C::NotEqual,
        invalid,
    );
    b.set(local(CTX), context);
    for (slot, off) in [
        (SEG, offset_of!(A, registry)),
        (LEASE, offset_of!(A, handshake)),
        (USED, offset_of!(A, participant)),
    ] {
        b.set(local(slot), field(REG, off, 0));
        b.pointer(local(slot), invalid);
    }
    for (left, right, condition) in [
        (
            field(CTX, offset_of!(MutatorContext, abi_version), 4),
            n(crate::gc_abi::ABI_VERSION as u64),
            C::NotEqual,
        ),
        (
            field(CTX, offset_of!(MutatorContext, thread_id), 0),
            n(0),
            C::Equal,
        ),
        (
            field(CTX, offset_of!(MutatorContext, state), 4),
            n(0),
            C::NotEqual,
        ),
        (
            field(CTX, offset_of!(MutatorContext, access_epoch), 8),
            n(0),
            C::NotEqual,
        ),
        (p!(context), local(CTX), C::NotEqual),
        (p!(registry), local(LEASE), C::NotEqual),
        (p!(registered), n(1), C::NotEqual),
        (
            p!(identity),
            field(CTX, offset_of!(MutatorContext, thread_id), 0),
            C::NotEqual,
        ),
        (h!(members), n(0), C::Equal),
        (h!(head), n(0), C::Equal),
        (h!(native), h!(members), C::Above),
        (h!(active), n(1), C::Above),
        (p!(identity), h!(last_identity), C::Above),
        (p!(acknowledged_generation), h!(generation), C::Above),
    ] {
        b.branch(left, right, condition, invalid);
    }
    b.branch(h!(active), n(0), C::NotEqual, active);
    b.branch(h!(expected), n(0), C::NotEqual, invalid);
    b.branch(h!(acknowledged), n(0), C::NotEqual, invalid);
    b.a.jmp_label(phase);
    b.a.bind_text_label(active);
    b.branch(h!(generation), n(0), C::Equal, invalid);
    b.branch(
        h!(expected),
        sub(h!(members), h!(native)),
        C::NotEqual,
        invalid,
    );
    b.branch(h!(acknowledged), h!(expected), C::Above, invalid);
    b.a.bind_text_label(phase);
    let snapshot = field(SEG, offset_of!(RootRegistry, phase), 0);
    super::remembered::phase(b, snapshot.clone(), invalid);
    b.branch(h!(active), n(0), C::NotEqual, wait);
    b.branch(
        p!(acknowledged_generation),
        h!(generation),
        C::NotEqual,
        wait,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, adopted_phase), 8),
        snapshot,
        C::NotEqual,
        wait,
    );
}
