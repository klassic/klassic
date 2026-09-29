//! Heap-lock serialized pin inventory binding; gc_pin_registry.h contract.
use super::*;
use crate::gc_abi::{
    AllocationPage as P, HeapPool as H, MetadataLeasePool as M, MetadataLeaseRequest as L,
    PinPageRecord as Record, PinPageRegistry as Registry, RelocationGate as Gate,
    RelocationGateRequest as Request,
};
use std::mem::size_of;
fn registry(off: usize) -> E {
    field(G, off, 0)
}
fn admission(off: usize) -> E {
    field(REG, offset_of!(Record, admission) + off, 0)
}
#[allow(dead_code)]
pub(crate) fn emit_bind<A: PortableAsm>(a: &mut A) {
    emit_owner(a, Mode::Bind);
}
#[allow(dead_code)]
pub(crate) fn emit_unbind<A: PortableAsm>(a: &mut A) {
    emit_owner(a, Mode::Unbind);
}
#[derive(Clone, Copy)]
enum Mode {
    Bind,
    Unbind,
    Step(u64),
    Selection,
}
/// Operation is fixed by the entry point, keeping the native call ABI to three arguments.
#[allow(dead_code)]
pub(crate) fn emit_step<A: PortableAsm>(a: &mut A, op: u64) {
    assert!(op < 4);
    emit_owner(a, Mode::Step(op));
}
#[allow(dead_code)]
pub(crate) fn emit_selection<A: PortableAsm>(a: &mut A) {
    emit_owner(a, Mode::Selection);
}
fn emit_owner<A: PortableAsm>(a: &mut A, mode: Mode) {
    const {
        assert!(size_of::<Record>() == 64);
        assert!(size_of::<L>() == 24);
    }
    let invalid = a.create_text_label();
    let wait = a.create_text_label();
    let exhausted = a.create_text_label();
    let full = a.create_text_label();
    let ok = a.create_text_label();
    let done = a.create_text_label();
    let metadata = a.create_text_label();
    let after = a.create_text_label();
    let gate = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 48);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    if matches!(mode, Mode::Step(_) | Mode::Selection) {
        a.store_rbp_slot(NEXT, R::V2);
    }
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.set(local(LEASE), registry(offset_of!(Registry, pool)));
    b.pointer(local(LEASE), invalid);
    b.pointer(registry(offset_of!(Registry, records)), invalid);
    let capacity = registry(offset_of!(Registry, capacity));
    b.branch(capacity.clone(), n(0), C::Equal, invalid);
    b.branch(capacity.clone(), n(u64::MAX / 64), C::Above, invalid);
    b.branch(
        add(
            registry(offset_of!(Registry, records)),
            shl(capacity.clone(), 6),
        ),
        registry(offset_of!(Registry, records)),
        C::Below,
        invalid,
    );
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 144);
    b.a.store_rbp_slot(CTX, R::V0);
    for off in [0, 8, 16] {
        b.set(field(CTX, off, 0), field(Q, off, 0));
    }
    metadata_call(&mut b, metadata, CTX, 2);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, invalid);
    for off in [0, 8] {
        b.branch(field(CTX, off, 0), field(Q, off, 0), C::NotEqual, invalid);
    }
    b.set(local(SEG), field(Q, offset_of!(L, page), 0));
    b.set(local(STATE), field(LEASE, offset_of!(M, heap), 0));
    b.branch(
        capacity,
        field(STATE, offset_of!(H, page_count), 0),
        C::NotEqual,
        invalid,
    );
    b.value(sub(local(SEG), field(STATE, offset_of!(H, pages), 0)));
    b.a.mov_imm64(R::V1, size_of::<P>() as u64);
    b.a.div_unsigned(R::V1);
    b.a.store_rbp_slot(USED, R::V0);
    b.set(
        local(REG),
        add(registry(offset_of!(Registry, records)), shl(local(USED), 6)),
    );
    if matches!(mode, Mode::Bind) {
        for value in [
            field(REG, 0, 0),
            field(REG, 8, 0),
            field(REG, 16, 0),
            admission(offset_of!(Gate, head)),
            admission(offset_of!(Gate, active)),
            admission(offset_of!(Gate, closed)),
        ] {
            b.branch(value, n(0), C::NotEqual, invalid);
        }
        let live = b.a.create_text_label();
        b.set(local(STATE), field(SEG, offset_of!(P, state), 8));
        b.branch(
            local(STATE),
            n(crate::gc_abi::PAGE_ALLOCATING),
            C::Equal,
            live,
        );
        b.branch(
            local(STATE),
            n(crate::gc_abi::PAGE_CLOSED),
            C::NotEqual,
            invalid,
        );
        b.a.bind_text_label(live);
        b.branch(
            admission(offset_of!(Gate, epoch)),
            n(u64::MAX),
            C::Equal,
            exhausted,
        );
        b.set(field(CTX, 16, 0), n(0));
        metadata_call(&mut b, metadata, CTX, 0);
        b.a.cmp_reg_imm32(R::V0, 2);
        b.a.jcc_label(C::Equal, full);
        b.a.cmp_reg_imm32(R::V0, 3);
        b.a.jcc_label(C::Equal, exhausted);
        b.a.test_reg_reg(R::V0, R::V0);
        b.a.jcc_label(C::NotEqual, invalid);
        for off in [0, 8, 16] {
            b.set(field(REG, off, 0), field(CTX, off, 0));
        }
        b.set(
            admission(offset_of!(Gate, epoch)),
            add(admission(offset_of!(Gate, epoch)), n(1)),
        );
    } else {
        for off in [0, 8] {
            b.branch(field(REG, off, 0), field(Q, off, 0), C::NotEqual, invalid);
        }
        for off in [0, 8, 16] {
            b.set(field(CTX, off, 0), field(REG, off, 0));
        }
        metadata_call(&mut b, metadata, CTX, 2);
        b.a.test_reg_reg(R::V0, R::V0);
        b.a.jcc_label(C::NotEqual, invalid);
        for off in [0, 8] {
            b.branch(field(CTX, off, 0), field(REG, off, 0), C::NotEqual, invalid);
        }
        match mode {
            Mode::Unbind => {
                for off in [
                    offset_of!(Gate, active),
                    offset_of!(Gate, head),
                    offset_of!(Gate, closed),
                ] {
                    b.branch(admission(off), n(0), C::NotEqual, wait);
                }
                // RELEASE clears only the record's independently owned lease.
                metadata_call(&mut b, metadata, REG, 1);
                b.a.test_reg_reg(R::V0, R::V0);
                b.a.jcc_label(C::NotEqual, invalid);
            }
            Mode::Step(op) => {
                if op == 0 || op == 2 {
                    let live = b.a.create_text_label();
                    b.set(local(STATE), field(SEG, offset_of!(P, state), 8));
                    b.branch(
                        local(STATE),
                        n(crate::gc_abi::PAGE_ALLOCATING),
                        C::Equal,
                        live,
                    );
                    b.branch(
                        local(STATE),
                        n(crate::gc_abi::PAGE_CLOSED),
                        C::NotEqual,
                        invalid,
                    );
                    b.a.bind_text_label(live);
                }
                b.value(add(local(REG), n(offset_of!(Record, admission) as u64)));
                b.a.mov_reg_reg(R::V7, R::V0);
                b.a.load_rbp_slot(R::V6, NEXT);
                b.a.mov_imm64(R::V2, op);
                b.a.call_label(gate);
                b.a.jmp_label(done);
            }
            Mode::Selection => {
                b.pointer(local(NEXT), invalid);
                for off in [offset_of!(Request, permit), offset_of!(Request, serial)] {
                    b.branch(field(NEXT, off, 0), n(0), C::NotEqual, invalid);
                }
                b.branch(
                    field(NEXT, offset_of!(Request, epoch), 0),
                    n(0),
                    C::Equal,
                    invalid,
                );
                b.branch(
                    field(NEXT, offset_of!(Request, epoch), 0),
                    admission(offset_of!(Gate, epoch)),
                    C::NotEqual,
                    invalid,
                );
                b.value(add(local(REG), n(offset_of!(Record, admission) as u64)));
                b.a.mov_reg_reg(R::V7, R::V0);
                b.a.mov_imm64(R::V6, 0);
                b.a.mov_imm64(R::V2, 3); // GC_RG_CHECK
                b.a.call_label(gate);
                b.a.jmp_label(done);
            }
            Mode::Bind => unreachable!(),
        }
    }
    b.a.jmp_label(ok);
    for (label, status) in [(ok, 0), (invalid, 1), (wait, 2), (exhausted, 3), (full, 4)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
    b.a.bind_text_label(gate);
    if matches!(mode, Mode::Step(_) | Mode::Selection) {
        if matches!(mode, Mode::Selection) {
            super::relocation_gate::emit_relocation_gate(b.a);
        } else {
            super::relocation_gate::emit_pin_page(b.a);
        }
        b.a.ret();
    }
    b.a.bind_text_label(after);
}
fn metadata_call<A: PortableAsm>(b: &mut Emit<'_, A>, target: A::TextLabel, request: i32, op: u64) {
    b.a.load_rbp_slot(R::V7, LEASE);
    b.a.load_rbp_slot(R::V6, request);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(target);
}
