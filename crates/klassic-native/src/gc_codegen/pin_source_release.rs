//! Selection-authenticated source drain and physical-uncommit transactions.
use super::*;
use crate::gc_abi::{
    AllocationPage as Page, HeapPool, MetadataLeasePool, MetadataLeaseRequest as Lease,
    PinPageRegistry, PinSourceDrain as Drain, PinSourceRequest as Request,
    PinSourceUncommit as Uncommit, RelocationGateRequest as Selection, SourceDrain, SourceUncommit,
};
use std::mem::size_of;
#[allow(dead_code)]
pub(crate) fn emit_drain<A: PortableAsm>(a: &mut A) {
    emit_release(a, true);
}
#[allow(dead_code)]
pub(crate) fn emit_uncommit<A: PortableAsm>(a: &mut A) {
    emit_release(a, false);
}
fn emit_release<A: PortableAsm>(a: &mut A, drain: bool) {
    let invalid = a.create_text_label();
    let wait = a.create_text_label();
    let done = a.create_text_label();
    let result = a.create_text_label();
    let selected = a.create_text_label();
    let core = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    if !drain {
        a.store_rbp_slot(OP, R::V2);
    }
    let mut b = Emit { a };
    b.pointer(local(Q), invalid);
    b.set(local(REG), field(Q, offset_of!(Request, work), 0));
    b.pointer(local(REG), invalid);
    if !drain {
        b.branch(local(OP), n(1), C::Above, invalid);
    }
    b.set(local(LEASE), field(Q, offset_of!(Request, lease), 0));
    b.set(local(CTX), field(Q, offset_of!(Request, selection), 0));
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, LEASE);
    b.a.load_rbp_slot(R::V2, CTX);
    b.a.call_label(selected);
    if drain {
        b.a.cmp_reg_imm32(R::V0, 2);
        b.a.jcc_label(C::Equal, wait);
    }
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, invalid);
    let epoch = if drain {
        offset_of!(Drain, epoch)
    } else {
        offset_of!(Uncommit, epoch)
    };
    let call = b.a.create_text_label();
    let fresh = b.a.create_text_label();
    if drain {
        b.branch(field(REG, epoch, 0), n(0), C::Equal, fresh);
    } else {
        b.branch(local(OP), n(0), C::Equal, fresh);
    }
    b.branch(field(REG, 0, 0), local(G), C::NotEqual, invalid);
    b.branch(
        field(REG, epoch, 0),
        field(CTX, offset_of!(Selection, epoch), 0),
        C::NotEqual,
        invalid,
    );
    if drain {
        for (off, source) in [
            (offset_of!(Drain, page), offset_of!(Lease, page)),
            (
                offset_of!(Drain, incarnation),
                offset_of!(Lease, incarnation),
            ),
        ] {
            b.branch(
                field(REG, off, 0),
                field(LEASE, source, 0),
                C::NotEqual,
                invalid,
            );
        }
    }
    b.a.jmp_label(call);
    b.a.bind_text_label(fresh);
    b.branch(field(REG, 0, 0), n(0), C::NotEqual, invalid);
    if drain {
        for off in [
            offset_of!(Drain, page),
            offset_of!(Drain, incarnation),
            offset_of!(Drain, scan) + offset_of!(SourceDrain, cursor),
        ] {
            b.branch(field(REG, off, 0), n(0), C::NotEqual, invalid);
        }
    } else {
        b.branch(field(REG, epoch, 0), n(0), C::NotEqual, invalid);
    }
    b.a.bind_text_label(call);
    if drain {
        b.value(field(LEASE, offset_of!(Lease, page), 0));
        b.a.mov_reg_reg(R::V7, R::V0);
        b.value(add(local(REG), n(offset_of!(Drain, scan) as u64)));
        b.a.mov_reg_reg(R::V6, R::V0);
    } else {
        b.set(local(SEG), field(G, offset_of!(PinPageRegistry, pool), 0));
        b.set(
            local(SEG),
            field(SEG, offset_of!(MetadataLeasePool, heap), 0),
        );
        b.value(sub(
            field(LEASE, offset_of!(Lease, page), 0),
            field(SEG, offset_of!(HeapPool, pages), 0),
        ));
        b.a.mov_imm64(R::V1, size_of::<Page>() as u64);
        b.a.div_unsigned(R::V1);
        b.a.store_rbp_slot(USED, R::V0);
        b.branch(
            field(
                REG,
                offset_of!(Uncommit, work) + offset_of!(SourceUncommit, head_index),
                0,
            ),
            local(USED),
            C::NotEqual,
            invalid,
        );
        b.branch(
            field(
                REG,
                offset_of!(Uncommit, work) + offset_of!(SourceUncommit, incarnation),
                0,
            ),
            field(LEASE, offset_of!(Lease, incarnation), 0),
            C::NotEqual,
            invalid,
        );
        b.a.load_rbp_slot(R::V7, SEG);
        b.value(add(local(REG), n(offset_of!(Uncommit, work) as u64)));
        b.a.mov_reg_reg(R::V6, R::V0);
        b.a.load_rbp_slot(R::V2, OP);
    }
    b.a.call_label(core);
    b.a.store_rbp_slot(USED, R::V0);
    b.branch(local(USED), n(1), C::Equal, result);
    if drain {
        b.branch(field(REG, epoch, 0), n(0), C::NotEqual, result);
    } else {
        b.branch(local(OP), n(0), C::NotEqual, result);
    }
    b.set(field(REG, 0, 0), local(G));
    b.set(
        field(REG, epoch, 0),
        field(CTX, offset_of!(Selection, epoch), 0),
    );
    if drain {
        b.set(
            field(REG, offset_of!(Drain, page), 0),
            field(LEASE, offset_of!(Lease, page), 0),
        );
        b.set(
            field(REG, offset_of!(Drain, incarnation), 0),
            field(LEASE, offset_of!(Lease, incarnation), 0),
        );
    }
    b.a.bind_text_label(result);
    b.a.load_rbp_slot(R::V0, USED);
    b.a.jmp_label(done);
    for (label, status) in [(invalid, 1), (wait, 3)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(selected);
    super::pin_registry::emit_selection(b.a);
    b.a.ret();
    b.a.bind_text_label(core);
    if drain {
        super::source_drain::emit_source_drain(b.a);
    } else {
        super::source_uncommit::emit_source_uncommit(b.a);
    }
    b.a.ret();
    b.a.bind_text_label(after);
}
