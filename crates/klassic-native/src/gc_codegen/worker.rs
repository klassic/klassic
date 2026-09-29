//! Owner-side coherent phase adoption with registered worker rendezvous.
use super::*;
use crate::gc_abi::{
    BufferCursor, HandshakeState, RootRegistry, ThreadRegistration, WorkerRequest,
};
#[allow(dead_code)]
pub(crate) fn emit_worker_step<A: PortableAsm>(a: &mut A) {
    let status: [_; 8] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[3];
    let attach = a.create_text_label();
    let ready = a.create_text_label();
    let invoke = a.create_text_label();
    let adopt = a.create_text_label();
    let duplicate = a.create_text_label();
    let result = a.create_text_label();
    let core = a.create_text_label();
    let after = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(3), C::Above, invalid);
    b.set(
        local(LEASE),
        field(Q, offset_of!(WorkerRequest, handshake), 0),
    );
    b.pointer(local(LEASE), invalid);
    b.set(
        local(REG),
        field(Q, offset_of!(WorkerRequest, participant), 0),
    );
    b.pointer(local(REG), invalid);
    b.set(local(USED), field(G, offset_of!(RootRegistry, phase), 0));
    b.set(
        local(CTX),
        field(REG, offset_of!(ThreadRegistration, context), 0),
    );
    b.pointer(local(CTX), invalid);
    b.set(
        local(STATE),
        field(CTX, offset_of!(MutatorContext, adopted_phase), 8),
    );
    for phase in [local(USED), local(STATE)] {
        b.branch(and(phase.clone(), n(0xe000)), n(0), C::NotEqual, invalid);
        b.branch(and(phase.clone(), n(15)), n(7), C::Above, invalid);
        b.branch(and(shr(phase, 4), n(15)), n(7), C::Above, invalid);
    }
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(local(OP), n(3), C::Equal, attach);
    b.branch(local(STATE), local(USED), C::Equal, ready);
    b.branch(
        shr(local(USED), 16),
        shr(local(STATE), 16),
        C::Below,
        invalid,
    );
    b.branch(
        shr(local(USED), 16),
        shr(local(STATE), 16),
        C::Equal,
        invalid,
    );
    let not_enter = b.a.create_text_label();
    b.branch(local(OP), n(1), C::NotEqual, not_enter);
    b.branch(
        field(LEASE, offset_of!(HandshakeState, active), 0),
        n(1),
        C::Equal,
        status[2],
    );
    b.a.jmp_label(invalid);
    b.a.bind_text_label(not_enter);
    b.branch(local(OP), n(0), C::NotEqual, ready);
    b.branch(
        field(LEASE, offset_of!(HandshakeState, active), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.a.jmp_label(ready);
    b.a.bind_text_label(attach);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    for off in [
        offset_of!(MutatorContext, root_segments),
        offset_of!(MutatorContext, retiring_roots),
        offset_of!(MutatorContext, tlab_start),
        offset_of!(MutatorContext, tlab_top),
        offset_of!(MutatorContext, tlab_end),
    ] {
        b.branch(field(CTX, off, 0), n(0), C::NotEqual, invalid);
    }
    b.a.bind_text_label(ready);
    for off in [
        offset_of!(MutatorContext, tlab_start),
        offset_of!(MutatorContext, tlab_top),
        offset_of!(MutatorContext, tlab_end),
    ] {
        b.branch(field(CTX, off, 0), n(0), C::NotEqual, status[6]);
    }
    for (i, off) in [
        offset_of!(MutatorContext, satb),
        offset_of!(MutatorContext, remembered),
    ]
    .into_iter()
    .enumerate()
    {
        b.set(
            local(PREV),
            field(CTX, off + offset_of!(BufferCursor, published), 8),
        );
        b.branch(
            local(PREV),
            field(CTX, off + offset_of!(BufferCursor, top), 0),
            C::Above,
            invalid,
        );
        b.branch(
            field(CTX, off + offset_of!(BufferCursor, consumed), 8),
            local(PREV),
            C::Above,
            invalid,
        );
        b.branch(
            local(PREV),
            field(CTX, off + offset_of!(BufferCursor, top), 0),
            C::NotEqual,
            status[4 + i],
        );
    }
    for (op, hs) in [(3, 0), (1, 3), (2, 4)] {
        let next = b.a.create_text_label();
        b.branch(local(OP), n(op), C::NotEqual, next);
        b.set(local(KIND), n(hs));
        b.a.jmp_label(invoke);
        b.a.bind_text_label(next);
    }
    b.set(local(KIND), n(2));
    b.branch(
        field(LEASE, offset_of!(HandshakeState, active), 0),
        n(0),
        C::Equal,
        invoke,
    );
    b.set(local(KIND), n(1));
    b.a.bind_text_label(invoke);
    b.a.load_rbp_slot(R::V7, LEASE);
    b.a.load_rbp_slot(R::V6, REG);
    b.a.load_rbp_slot(R::V2, KIND);
    b.a.call_label(core);
    b.a.store_rbp_slot(PREV, R::V0);
    b.branch(local(PREV), n(1), C::Equal, status[2]);
    b.branch(local(PREV), n(3), C::Equal, status[7]);
    b.branch(local(PREV), n(4), C::Equal, duplicate);
    b.branch(local(PREV), n(0), C::NotEqual, invalid);
    b.branch(local(OP), n(3), C::Equal, adopt);
    b.branch(local(OP), n(2), C::Equal, adopt);
    b.branch(local(KIND), n(1), C::NotEqual, result);
    b.a.bind_text_label(adopt);
    b.set(
        field(CTX, offset_of!(MutatorContext, adopted_phase), 8),
        local(USED),
    );
    b.a.bind_text_label(result);
    b.branch(local(KIND), n(1), C::Equal, status[1]);
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(duplicate);
    b.branch(local(STATE), local(USED), C::NotEqual, invalid);
    b.a.jmp_label(status[1]);
    for (v, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, v as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(core);
    crate::gc_codegen::handshake::emit_step_locked(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
