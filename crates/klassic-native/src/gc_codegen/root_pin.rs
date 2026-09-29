//! Independent rooted pin ownership; gc_root_pin.h contract.
use super::*;
use crate::gc_abi::{
    AllocationPage, MetadataLeaseRequest, PinPageRegistry, ResolveRequest, RootAccess, RootFollow,
    RootHandle, RootPin as Pin, RootPinStart as Start, RootRequest,
};
use std::mem::size_of;
const ROOT: usize = offset_of!(Pin, root);
const FOLLOW: usize = ROOT + offset_of!(RootAccess, follow);
const CLEANUP: usize = FOLLOW + offset_of!(RootFollow, cleanup);
const ACCESS: usize = ROOT + offset_of!(RootAccess, access);
fn pin(off: usize) -> E {
    field(Q, off, 0)
}
fn call<A: PortableAsm>(b: &mut Emit<'_, A>, target: A::TextLabel, first: E, second: E, third: E) {
    // Expressions use V0/V1 only, so installing earlier arguments is safe.
    b.value(first);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(second);
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(third);
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(target);
}
fn root_status<A: PortableAsm>(b: &mut Emit<'_, A>, status: &[A::TextLabel; 11]) {
    for (raw, mapped) in [(0, 0), (3, 3), (2, 5), (4, 6)] {
        b.a.cmp_reg_imm32(R::V0, raw);
        b.a.jcc_label(C::Equal, status[mapped]);
    }
    b.a.jmp_label(status[4]);
}
fn returns<A: PortableAsm>(b: &mut Emit<'_, A>, status: [A::TextLabel; 11], done: A::TextLabel) {
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
#[allow(dead_code)]
pub(crate) fn emit_prepare<A: PortableAsm>(a: &mut A) {
    const {
        assert!(size_of::<Pin>() == 256);
        assert!(size_of::<RootRequest>() == 40);
    }
    let status: [_; 11] = std::array::from_fn(|_| a.create_text_label());
    let done = a.create_text_label();
    let after = a.create_text_label();
    let root = a.create_text_label();
    let copy = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 48);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(G, R::V6);
    let mut b = Emit { a };
    let invalid = status[4];
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    for (slot, off) in [
        (REG, offset_of!(Start, registry)),
        (CTX, offset_of!(Start, context)),
        (SEG, offset_of!(Start, source)),
        (NEXT, offset_of!(Start, reserved)),
    ] {
        b.set(local(slot), field(G, off, 0));
        b.pointer(local(slot), invalid);
    }
    b.pointer(field(REG, offset_of!(PinPageRegistry, pool), 0), invalid);
    let cell = offset_of!(RootRequest, handle) + offset_of!(RootHandle, cell);
    b.branch(field(SEG, cell, 0), field(NEXT, cell, 0), C::Equal, invalid);
    for off in (0..size_of::<Pin>()).step_by(8) {
        b.branch(pin(off), n(0), C::NotEqual, invalid);
    }
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 160);
    b.a.store_rbp_slot(PREV, R::V0);
    for off in (0..size_of::<RootRequest>()).step_by(8) {
        b.set(field(PREV, off, 0), field(NEXT, off, 0));
    }
    b.set(field(PREV, offset_of!(RootRequest, value), 0), n(0));
    call(&mut b, root, local(CTX), local(PREV), n(5));
    let copied = b.a.create_text_label();
    let ready = b.a.create_text_label();
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::Equal, ready);
    root_status(&mut b, &status);
    b.a.bind_text_label(ready);
    call(&mut b, copy, local(CTX), local(SEG), local(PREV));
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::Equal, copied);
    root_status(&mut b, &status);
    b.a.bind_text_label(copied);
    b.set(pin(offset_of!(Pin, self_)), local(Q));
    b.set(pin(offset_of!(Pin, registry)), local(REG));
    b.set(pin(offset_of!(Pin, stage)), n(1));
    b.set(pin(FOLLOW + offset_of!(RootFollow, context)), local(CTX));
    for off in (0..size_of::<RootRequest>()).step_by(8) {
        b.set(
            pin(FOLLOW + offset_of!(RootFollow, root) + off),
            field(PREV, off, 0),
        );
    }
    b.set(
        pin(offset_of!(Pin, token)),
        add(local(Q), n(offset_of!(Pin, permit) as u64)),
    );
    b.a.jmp_label(status[0]);
    returns(&mut b, status, done);
    b.a.jmp_label(after);
    b.a.bind_text_label(root);
    crate::gc_codegen::roots::emit_step(b.a);
    b.a.ret();
    b.a.bind_text_label(copy);
    super::root_copy::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
/// 0 ENTER (locked), 1 EXIT, 2 CLEANUP (locked), 3 CANCEL (locked), 4 FINISH.
#[allow(dead_code)]
pub(crate) fn emit_step<A: PortableAsm>(a: &mut A, op: u64) {
    assert!(op < 5);
    let status: [_; 11] = std::array::from_fn(|_| a.create_text_label());
    let done = a.create_text_label();
    let after = a.create_text_label();
    let access = a.create_text_label();
    let cleanup = a.create_text_label();
    let page_pin = a.create_text_label();
    let metadata = a.create_text_label();
    let root = a.create_text_label();
    let cancel_empty = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 48);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    let invalid = status[4];
    b.pointer(local(Q), invalid);
    b.branch(pin(offset_of!(Pin, self_)), local(Q), C::NotEqual, invalid);
    b.set(local(REG), pin(offset_of!(Pin, registry)));
    b.pointer(local(REG), invalid);
    let stage = pin(offset_of!(Pin, stage));
    if op == 3 {
        let valid = b.a.create_text_label();
        for value in [1, 3, 4] {
            b.branch(stage.clone(), n(value), C::Equal, valid);
        }
        b.a.jmp_label(invalid);
        b.a.bind_text_label(valid);
    } else {
        b.branch(
            stage.clone(),
            n(if op == 1 {
                2
            } else if op == 4 {
                5
            } else {
                1
            }),
            C::NotEqual,
            invalid,
        );
    }
    b.set(local(SEG), add(local(Q), n(ROOT as u64)));
    b.set(local(NEXT), add(local(Q), n(CLEANUP as u64)));
    b.set(local(PREV), add(local(Q), n(offset_of!(Pin, token) as u64)));
    let token = pin(CLEANUP + offset_of!(MetadataLeaseRequest, token));
    match op {
        0 => {
            b.branch(token, n(0), C::NotEqual, status[7]);
            call(
                &mut b,
                access,
                field(REG, offset_of!(PinPageRegistry, pool), 0),
                local(SEG),
                n(0),
            );
            let ready = b.a.create_text_label();
            b.a.cmp_reg_imm32(R::V0, 8);
            b.a.jcc_label(C::Equal, ready);
            for (raw, mapped) in [(3, 0), (0, 10), (2, 2), (4, 3), (5, 5), (6, 6), (7, 7)] {
                b.a.cmp_reg_imm32(R::V0, raw);
                b.a.jcc_label(C::Equal, status[mapped]);
            }
            b.a.jmp_label(invalid);
            b.a.bind_text_label(ready);
            b.set(
                pin(offset_of!(Pin, reference)),
                pin(ACCESS + offset_of!(ResolveRequest, reference)),
            );
            call(&mut b, page_pin, local(REG), local(NEXT), local(PREV));
            b.a.store_rbp_slot(STATE, R::V0);
            let pending = b.a.create_text_label();
            b.set(local(USED), n(4));
            for (raw, mapped) in [(0, 1), (2, 2), (3, 6)] {
                let next = b.a.create_text_label();
                b.branch(local(STATE), n(raw as u64), C::NotEqual, next);
                b.a.mov_imm64(R::V0, mapped);
                b.a.store_rbp_slot(USED, R::V0);
                b.a.jmp_label(pending);
                b.a.bind_text_label(next);
            }
            b.a.bind_text_label(pending);
            b.set(pin(offset_of!(Pin, pending)), local(USED));
            b.set(stage, n(2));
            b.a.jmp_label(status[9]);
        }
        1 => {
            call(&mut b, cleanup, n(0), local(SEG), n(0));
            b.a.cmp_reg_imm32(R::V0, 2);
            b.a.jcc_label(C::Equal, status[9]);
            b.a.test_reg_reg(R::V0, R::V0);
            b.a.jcc_label(C::NotEqual, invalid);
            let ready = b.a.create_text_label();
            b.branch(pin(offset_of!(Pin, pending)), n(1), C::Equal, ready);
            b.set(stage.clone(), n(1));
            b.a.jmp_label(status[7]);
            b.a.bind_text_label(ready);
            b.set(
                local(USED),
                pin(CLEANUP + offset_of!(MetadataLeaseRequest, page)),
            );
            b.set(
                pin(offset_of!(Pin, address)),
                add(
                    field(USED, offset_of!(AllocationPage, heap_base), 0),
                    shr(pin(offset_of!(Pin, reference)), 16),
                ),
            );
            b.set(stage, n(3));
            b.a.jmp_label(status[1]);
        }
        2 => {
            b.branch(token, n(0), C::Equal, status[0]);
            call(
                &mut b,
                cleanup,
                field(REG, offset_of!(PinPageRegistry, pool), 0),
                local(SEG),
                n(1),
            );
            b.a.test_reg_reg(R::V0, R::V0);
            b.a.jcc_label(C::NotEqual, status[7]);
            b.set(local(USED), pin(offset_of!(Pin, pending)));
            b.set(pin(offset_of!(Pin, pending)), n(0));
            b.set(pin(offset_of!(Pin, reference)), n(0));
            b.a.load_rbp_slot(R::V0, USED);
            b.a.jmp_label(done);
        }
        3 => {
            let lease = b.a.create_text_label();
            let finished = b.a.create_text_label();
            b.branch(stage.clone(), n(3), C::NotEqual, lease);
            call(&mut b, page_pin, local(REG), local(NEXT), local(PREV));
            b.a.test_reg_reg(R::V0, R::V0);
            b.a.jcc_label(C::NotEqual, status[7]);
            b.set(pin(offset_of!(Pin, address)), n(0));
            b.a.bind_text_label(lease);
            b.set(stage.clone(), n(4));
            b.branch(token, n(0), C::Equal, finished);
            call(
                &mut b,
                metadata,
                field(REG, offset_of!(PinPageRegistry, pool), 0),
                local(NEXT),
                n(1),
            );
            b.a.test_reg_reg(R::V0, R::V0);
            b.a.jcc_label(C::NotEqual, status[7]);
            b.a.bind_text_label(finished);
            b.set(stage, n(5));
            b.a.jmp_label(status[0]);
        }
        4 => {
            b.set(local(CTX), pin(FOLLOW + offset_of!(RootFollow, context)));
            b.a.mov_reg_reg(R::V0, R::V5);
            b.a.sub_reg_imm32(R::V0, 160);
            b.a.store_rbp_slot(PREV, R::V0);
            for off in (0..size_of::<RootRequest>()).step_by(8) {
                b.set(
                    field(PREV, off, 0),
                    pin(FOLLOW + offset_of!(RootFollow, root) + off),
                );
            }
            call(&mut b, root, local(CTX), local(PREV), n(3));
            b.a.test_reg_reg(R::V0, R::V0);
            b.a.jcc_label(C::NotEqual, invalid);
            let empty = b.a.create_text_label();
            let released = b.a.create_text_label();
            let success = b.a.create_text_label();
            b.branch(
                field(PREV, offset_of!(RootRequest, value), 0),
                n(0),
                C::Equal,
                empty,
            );
            call(&mut b, root, local(CTX), local(PREV), n(2));
            b.a.jmp_label(released);
            b.a.bind_text_label(empty);
            call(&mut b, cancel_empty, local(CTX), local(PREV), n(0));
            b.a.bind_text_label(released);
            b.a.test_reg_reg(R::V0, R::V0);
            b.a.jcc_label(C::Equal, success);
            root_status(&mut b, &status);
            b.a.bind_text_label(success);
            for off in (0..size_of::<RootAccess>()).step_by(8) {
                b.set(pin(ROOT + off), n(0));
            }
            b.set(pin(offset_of!(Pin, reference)), n(0));
            b.set(pin(offset_of!(Pin, pending)), n(0));
            b.set(stage, n(6));
            b.a.jmp_label(status[8]);
        }
        _ => unreachable!(),
    }
    returns(&mut b, status, done);
    b.a.jmp_label(after);
    b.a.bind_text_label(access);
    if op == 0 {
        super::root_access::emit_root_access(b.a);
        b.a.ret();
    }
    b.a.bind_text_label(cleanup);
    if op == 1 || op == 2 {
        super::root_access::emit_cleanup(b.a);
        b.a.ret();
    }
    b.a.bind_text_label(page_pin);
    if op == 0 || op == 3 {
        super::pin_registry::emit_step(b.a, if op == 0 { 0 } else { 1 });
        b.a.ret();
    }
    b.a.bind_text_label(metadata);
    if op == 3 {
        super::metadata::emit_metadata_step(b.a);
        b.a.ret();
    }
    b.a.bind_text_label(root);
    if op == 4 {
        crate::gc_codegen::roots::emit_step(b.a);
        b.a.ret();
    }
    b.a.bind_text_label(cancel_empty);
    if op == 4 {
        crate::gc_codegen::roots::emit_cancel_empty(b.a);
        b.a.ret();
    }
    b.a.bind_text_label(after);
}
