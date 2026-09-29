//! Registered native root package publication, private-root adoption and retirement.
use super::*;
use crate::gc_abi::{
    MetadataCharge, NativePin as Pin, NativePinStart as Start, RootHandle, RootPin, RootPinStart,
    RootRequest, RootStorage,
};
use std::mem::size_of;
fn pin(off: usize) -> E {
    field(Q, off, 0)
}
fn call<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel, first: E, second: E, third: E) {
    b.value(first);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(second);
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(third);
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(label);
}
pub(super) fn validate<A: PortableAsm>(b: &mut Emit<'_, A>, bad: A::TextLabel) {
    b.pointer(local(Q), bad);
    b.branch(pin(offset_of!(Pin, self_)), local(Q), C::NotEqual, bad);
    for off in [
        offset_of!(Pin, registry),
        offset_of!(Pin, context),
        offset_of!(Pin, charge),
        offset_of!(Pin, storage),
    ] {
        b.pointer(pin(off), bad);
    }
    b.set(local(NEXT), pin(offset_of!(Pin, charge)));
    b.branch(
        field(NEXT, offset_of!(MetadataCharge, stage), 0),
        n(2),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(NEXT, offset_of!(MetadataCharge, allocation), 0),
        pin(offset_of!(Pin, storage)),
        C::NotEqual,
        bad,
    );
}
#[allow(dead_code)]
pub(crate) fn emit_publish<A: PortableAsm>(a: &mut A) {
    emit_owner(a, 0);
}
#[allow(dead_code)]
pub(crate) fn emit_prepare<A: PortableAsm>(a: &mut A) {
    emit_owner(a, 1);
}
#[allow(dead_code)]
pub(crate) fn emit_retire<A: PortableAsm>(a: &mut A) {
    emit_owner(a, 2);
}
fn emit_owner<A: PortableAsm>(a: &mut A, op: u64) {
    const {
        assert!(size_of::<Pin>() == 344);
        assert!(size_of::<RootLifeRequest>() == 72);
    }
    let status: [_; 9] = std::array::from_fn(|_| a.create_text_label());
    let done = a.create_text_label();
    let after = a.create_text_label();
    let core = a.create_text_label();
    let roots = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 80);
    a.store_rbp_slot(Q, R::V7);
    if op < 2 {
        a.store_rbp_slot(REG, R::V6);
    }
    if op == 1 {
        a.store_rbp_slot(SEG, R::V2);
    }
    let mut b = Emit { a };
    let invalid = status[4];
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 192);
    b.a.store_rbp_slot(PREV, R::V0);
    if op == 0 {
        b.pointer(local(REG), invalid);
        b.pointer(local(Q), invalid);
        for off in (0..size_of::<Pin>()).step_by(8) {
            b.branch(pin(off), n(0), C::NotEqual, invalid);
        }
        b.set(local(CTX), field(REG, offset_of!(Start, context), 0));
        b.set(local(LEASE), field(REG, offset_of!(Start, charge), 0));
        b.set(local(REG), field(REG, offset_of!(Start, registry), 0));
        call(&mut b, core, local(REG), local(CTX), local(LEASE));
        b.a.cmp_reg_imm32(R::V0, 3);
        b.a.jcc_label(C::Equal, status[6]);
        b.a.test_reg_reg(R::V0, R::V0);
        b.a.jcc_label(C::NotEqual, invalid);
        b.set(pin(offset_of!(Pin, self_)), local(Q));
        b.set(pin(offset_of!(Pin, registry)), local(REG));
        b.set(pin(offset_of!(Pin, context)), local(CTX));
        b.set(pin(offset_of!(Pin, charge)), local(LEASE));
        b.set(
            local(SEG),
            field(LEASE, offset_of!(MetadataCharge, allocation), 0),
        );
        b.set(pin(offset_of!(Pin, storage)), local(SEG));
        b.set(pin(offset_of!(Pin, stage)), n(1));
        b.set(
            pin(offset_of!(Pin, reserved) + offset_of!(RootRequest, segment)),
            add(local(SEG), n(offset_of!(RootStorage, segment) as u64)),
        );
        b.a.jmp_label(status[0]);
    } else {
        validate(&mut b, invalid);
        b.set(local(CTX), pin(offset_of!(Pin, context)));
        let stage = pin(offset_of!(Pin, stage));
        if op == 1 {
            b.branch(stage.clone(), n(1), C::Below, invalid);
            b.branch(stage.clone(), n(2), C::Above, invalid);
            b.pointer(local(SEG), invalid);
            b.branch(
                field(SEG, offset_of!(RootRequest, segment), 0),
                pin(offset_of!(Pin, reserved) + offset_of!(RootRequest, segment)),
                C::Equal,
                invalid,
            );
            b.set(
                local(USED),
                add(local(Q), n(offset_of!(Pin, reserved) as u64)),
            );
            let prepare = b.a.create_text_label();
            let reserved = b.a.create_text_label();
            b.branch(stage.clone(), n(2), C::Equal, prepare);
            call(&mut b, roots, local(CTX), local(USED), n(0));
            b.a.test_reg_reg(R::V0, R::V0);
            b.a.jcc_label(C::Equal, reserved);
            for (raw, mapped) in [(3, 3), (2, 5), (4, 6)] {
                b.a.cmp_reg_imm32(R::V0, raw);
                b.a.jcc_label(C::Equal, status[mapped]);
            }
            b.a.jmp_label(invalid);
            b.a.bind_text_label(reserved);
            b.set(stage.clone(), n(2));
            b.a.bind_text_label(prepare);
            for (off, value) in [
                (offset_of!(RootPinStart, registry), local(REG)),
                (offset_of!(RootPinStart, context), local(CTX)),
                (offset_of!(RootPinStart, source), local(SEG)),
                (offset_of!(RootPinStart, reserved), local(USED)),
            ] {
                b.set(field(PREV, off, 0), value);
            }
            call(
                &mut b,
                core,
                add(local(Q), n(offset_of!(Pin, pin) as u64)),
                local(PREV),
                n(0),
            );
            b.a.test_reg_reg(R::V0, R::V0);
            b.a.jcc_label(C::NotEqual, done);
            b.set(stage, n(3));
            b.a.jmp_label(status[0]);
        } else {
            b.branch(stage.clone(), n(1), C::Below, invalid);
            b.branch(stage.clone(), n(3), C::Above, invalid);
            let release = b.a.create_text_label();
            b.branch(stage.clone(), n(3), C::NotEqual, release);
            b.branch(
                pin(offset_of!(Pin, pin) + offset_of!(RootPin, self_)),
                add(local(Q), n(offset_of!(Pin, pin) as u64)),
                C::NotEqual,
                status[2],
            );
            b.pointer(
                pin(offset_of!(Pin, pin) + offset_of!(RootPin, registry)),
                status[2],
            );
            b.branch(
                pin(offset_of!(Pin, pin) + offset_of!(RootPin, stage)),
                n(6),
                C::NotEqual,
                status[2],
            );
            b.a.bind_text_label(release);
            let retire = b.a.create_text_label();
            b.branch(stage.clone(), n(2), C::NotEqual, retire);
            call(
                &mut b,
                roots,
                local(CTX),
                add(local(Q), n(offset_of!(Pin, reserved) as u64)),
                n(0),
            );
            b.a.test_reg_reg(R::V0, R::V0);
            b.a.jcc_label(C::NotEqual, invalid);
            for off in (0..size_of::<RootHandle>()).step_by(8) {
                b.set(
                    pin(offset_of!(Pin, reserved) + offset_of!(RootRequest, handle) + off),
                    n(0),
                );
            }
            b.set(stage.clone(), n(1));
            b.a.bind_text_label(retire);
            b.set(local(LEASE), pin(offset_of!(Pin, storage)));
            for off in (0..size_of::<RootLifeRequest>()).step_by(8) {
                b.set(field(PREV, off, 0), n(0));
            }
            b.set(
                field(PREV, offset_of!(RootLifeRequest, registration), 0),
                add(
                    local(LEASE),
                    n(offset_of!(RootStorage, registration) as u64),
                ),
            );
            b.set(
                field(PREV, offset_of!(RootLifeRequest, context), 0),
                local(CTX),
            );
            b.set(
                field(PREV, offset_of!(RootLifeRequest, scope), 0),
                field(
                    LEASE,
                    offset_of!(RootStorage, segment) + offset_of!(RootSegment, scope),
                    0,
                ),
            );
            call(
                &mut b,
                core,
                pin(offset_of!(Pin, registry)),
                local(PREV),
                n(3),
            );
            b.a.cmp_reg_imm32(R::V0, 3);
            b.a.jcc_label(C::Equal, status[6]);
            b.a.test_reg_reg(R::V0, R::V0);
            b.a.jcc_label(C::NotEqual, invalid);
            b.set(stage, n(4));
            b.a.jmp_label(status[0]);
        }
    }
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(core);
    match op {
        0 => super::native_root_publish::emit(b.a),
        1 => super::root_pin::emit_prepare(b.a),
        _ => super::emit_life_locked(b.a),
    }
    b.a.ret();
    b.a.bind_text_label(roots);
    if op == 1 {
        crate::gc_codegen::roots::emit_step(b.a);
        b.a.ret();
    }
    if op == 2 {
        crate::gc_codegen::roots::emit_cancel_empty(b.a);
        b.a.ret();
    }
    b.a.bind_text_label(after);
}

/// Native-only backing disposal. Failed OS unmap preserves charge and package.
#[allow(dead_code)]
pub(crate) fn emit_dispose<A: PortableAsm>(a: &mut A) {
    use crate::gc_abi::NativeMetadataOps as Ops;
    let invalid = a.create_text_label();
    let wait = a.create_text_label();
    let retry = a.create_text_label();
    let done = a.create_text_label();
    let check = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(G, R::V6);
    let mut b = Emit { a };
    validate(&mut b, invalid);
    b.branch(pin(offset_of!(Pin, stage)), n(4), C::NotEqual, invalid);
    b.set(local(CTX), pin(offset_of!(Pin, context)));
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, state), 4),
        n(2),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.pointer(local(G), invalid);
    for off in [offset_of!(Ops, map), offset_of!(Ops, unmap)] {
        b.branch(field(G, off, 0), n(0), C::Equal, invalid);
    }
    b.set(local(USED), field(G, offset_of!(Ops, quantum), 0));
    b.branch(local(USED), n(8), C::Below, invalid);
    b.set(local(STATE), sub(local(USED), n(1)));
    b.branch(and(local(USED), local(STATE)), n(0), C::NotEqual, invalid);
    b.pointer(field(NEXT, offset_of!(MetadataCharge, owner), 0), invalid);
    b.set(
        local(USED),
        field(NEXT, offset_of!(MetadataCharge, bytes), 0),
    );
    b.branch(local(USED), n(0), C::Equal, invalid);
    b.branch(and(local(USED), local(STATE)), n(0), C::NotEqual, invalid);
    b.set(local(LEASE), pin(offset_of!(Pin, storage)));
    b.branch(and(local(LEASE), local(STATE)), n(0), C::NotEqual, wait);
    b.branch(
        local(USED),
        n(size_of::<RootStorage>() as u64),
        C::Below,
        wait,
    );
    b.a.load_rbp_slot(R::V7, LEASE);
    b.a.call_label(check);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, wait);
    b.value(field(G, offset_of!(Ops, unmap), 0));
    b.a.mov_reg_reg(R::V10, R::V0);
    b.a.load_rbp_slot(R::V7, LEASE);
    b.a.load_rbp_slot(R::V6, USED);
    b.a.call_native_binary(R::V10);
    // C bool is returned in the low byte; upper return-register bits are not a receipt.
    b.a.and_reg_imm32(R::V0, 255);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::Equal, retry);
    // Do not inspect storage after this point: unmap succeeded.
    b.set(field(NEXT, offset_of!(MetadataCharge, allocation), 0), n(0));
    b.set(pin(offset_of!(Pin, storage)), n(0));
    b.set(pin(offset_of!(Pin, stage)), n(5));
    b.a.mov_imm64(R::V0, 8);
    b.a.jmp_label(done);
    for (label, status) in [(invalid, 4), (wait, 2), (retry, 7)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(check);
    crate::gc_codegen::root_storage::emit_can_dispose(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}

/// Complete the disposed package's budget obligation under its heap lock.
#[allow(dead_code)]
pub(crate) fn emit_release<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let budget = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(G, R::V6);
    let mut b = Emit { a };
    b.pointer(local(Q), invalid);
    b.branch(pin(offset_of!(Pin, self_)), local(Q), C::NotEqual, invalid);
    b.branch(pin(offset_of!(Pin, stage)), n(5), C::NotEqual, invalid);
    b.branch(pin(offset_of!(Pin, storage)), n(0), C::NotEqual, invalid);
    b.pointer(local(G), invalid);
    b.pointer(pin(offset_of!(Pin, charge)), invalid);
    b.pointer(pin(offset_of!(Pin, context)), invalid);
    b.set(local(CTX), pin(offset_of!(Pin, context)));
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, state), 4),
        n(2),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    call(&mut b, budget, local(G), pin(offset_of!(Pin, charge)), n(3));
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, invalid);
    b.set(pin(offset_of!(Pin, stage)), n(6));
    b.a.mov_imm64(R::V0, 8);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 4);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(budget);
    crate::gc_codegen::root_lifetime::budget::emit_budget_step(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
