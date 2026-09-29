//! Finite TLAB allocation and journaled publication into an empty output root.
use super::*;
use crate::gc_abi::{
    AllocationInto, AllocationRequest, AllocationResult, BufferCursor, RootCell, RootChange,
    RootHandle, RootRequest,
};
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(AllocationInto, $f), 0)
    };
}
macro_rules! root {
    ($f:ident) => {
        field(SEG, offset_of!(RootRequest, $f), 0)
    };
}
macro_rules! buffer {
    ($f:ident,$a:expr) => {
        field(LEASE, offset_of!(BufferCursor, $f), $a)
    };
}
#[allow(dead_code)]
pub(crate) fn emit_alloc_into<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let flush = a.create_text_label();
    let exhausted = a.create_text_label();
    let committed = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let root_step = a.create_text_label();
    let allocate = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.pointer(req!(allocation), invalid);
    b.pointer(req!(root), invalid);
    b.set(local(REG), req!(allocation));
    b.set(local(SEG), req!(root));
    b.pointer(
        field(REG, offset_of!(AllocationRequest, output), 0),
        invalid,
    );
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, SEG);
    b.a.mov_imm64(R::V2, 5);
    b.a.call_label(root_step);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(3), C::Equal, flush);
    b.branch(local(STATE), n(4), C::Equal, exhausted);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, REG);
    b.a.call_label(allocate);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::Equal, committed);
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    b.a.bind_text_label(committed);
    b.set(local(CTX), root!(segment));
    b.set(
        local(LEASE),
        field(CTX, offset_of!(RootSegment, journal), 0),
    );
    b.set(
        local(KIND),
        field(
            SEG,
            offset_of!(RootRequest, handle) + offset_of!(RootHandle, cell),
            0,
        ),
    );
    b.set(local(USED), buffer!(top, 0));
    b.set(
        local(STATE),
        field(REG, offset_of!(AllocationRequest, output), 0),
    );
    b.set(
        local(PREV),
        field(STATE, offset_of!(AllocationResult, reference), 0),
    );
    b.value(buffer!(capacity, 0));
    b.a.mov_reg_reg(R::V1, R::V0);
    b.a.load_rbp_slot(R::V0, USED);
    b.a.div_unsigned(R::V1);
    b.a.shl_reg_imm8(R::V2, 5);
    b.a.store_rbp_slot(NEXT, R::V2);
    b.set(local(NEXT), add(buffer!(entries, 0), local(NEXT)));
    for (offset, value) in [
        (offset_of!(RootChange, previous), n(0)),
        (offset_of!(RootChange, next), local(PREV)),
        (
            offset_of!(RootChange, scope),
            field(CTX, offset_of!(RootSegment, scope), 0),
        ),
        (
            offset_of!(RootChange, phase),
            field(G, offset_of!(MutatorContext, adopted_phase), 1),
        ),
    ] {
        b.set(field(NEXT, offset, 0), value);
    }
    b.set(root!(value), local(PREV));
    b.set(buffer!(top, 0), add(local(USED), n(1)));
    b.set(buffer!(published, 1), add(local(USED), n(1)));
    b.set(field(KIND, offset_of!(RootCell, value), 1), local(PREV));
    b.set(
        field(G, offset_of!(MutatorContext, root_revision), 1),
        add(field(G, offset_of!(MutatorContext, root_revision), 1), n(1)),
    );
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    for (label, status) in [(invalid, 2), (flush, 5), (exhausted, 6)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(root_step);
    crate::gc_codegen::roots::emit_step(b.a);
    b.a.ret();
    b.a.bind_text_label(allocate);
    crate::gc_codegen::heap::emit_try_tlab_allocate(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
