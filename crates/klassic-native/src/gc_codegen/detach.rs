//! Bounded detach orchestration. Child routines are the existing shared emitters,
//! not duplicated implementations. Matching contract: runtime/gc/gc_detach.h.
use super::*;
use crate::gc_abi::{BufferCursor, DetachRequest, HandshakeState, ThreadRegistration};
const HANDSHAKE: i32 = OP;
const PARTICIPANT: i32 = REG;
const RETIRE: i32 = 104;

#[allow(dead_code)]
pub(crate) fn emit_detach_step<A: PortableAsm>(a: &mut A) {
    let handshake = a.create_text_label();
    let lifetime = a.create_text_label();
    let after = a.create_text_label();
    let statuses: [_; 9] = std::array::from_fn(|_| a.create_text_label());
    let invalid = statuses[3];
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    for (offset, slot) in [
        (offset_of!(DetachRequest, roots), G),
        (offset_of!(DetachRequest, handshake), HANDSHAKE),
        (offset_of!(DetachRequest, participant), PARTICIPANT),
    ] {
        let value = field(Q, offset, 0);
        b.pointer(value.clone(), invalid);
        b.set(local(slot), value);
    }
    let context = field(PARTICIPANT, offset_of!(ThreadRegistration, context), 0);
    b.pointer(context.clone(), invalid);
    b.set(local(CTX), context);
    b.branch(ctx!(state), n(3), C::NotEqual, invalid);
    let inactive = b.a.create_text_label();
    b.branch(
        field(HANDSHAKE, offset_of!(HandshakeState, active), 0),
        n(0),
        C::Equal,
        inactive,
    );
    call_handshake(&mut b, handshake, 1);
    b.a.cmp_reg_imm32(R::V0, 0);
    b.a.jcc_label(C::Equal, statuses[1]);
    b.a.cmp_reg_imm32(R::V0, 4);
    b.a.jcc_label(C::Equal, statuses[2]);
    b.a.jmp_label(invalid);
    b.a.bind_text_label(inactive);
    call_handshake(&mut b, handshake, 2);
    b.a.cmp_reg_imm32(R::V0, 0);
    b.a.jcc_label(C::NotEqual, invalid);
    for value in [ctx!(tlab_start), ctx!(tlab_top), ctx!(tlab_end)] {
        b.branch(value, n(0), C::NotEqual, statuses[5]);
    }
    for (i, base) in [
        offset_of!(MutatorContext, satb),
        offset_of!(MutatorContext, remembered),
    ]
    .into_iter()
    .enumerate()
    {
        b.set(
            local(PREV),
            field(CTX, base + offset_of!(BufferCursor, published), 8),
        );
        b.set(
            local(NEXT),
            field(CTX, base + offset_of!(BufferCursor, consumed), 8),
        );
        let top = field(CTX, base + offset_of!(BufferCursor, top), 0);
        b.branch(local(PREV), top.clone(), C::Above, invalid);
        b.branch(local(NEXT), local(PREV), C::Above, invalid);
        b.branch(local(PREV), top, C::NotEqual, statuses[6 + i]);
    }
    let finish = b.a.create_text_label();
    b.branch(ctx!(root_segments), n(0), C::Equal, finish);
    b.branch(ctx!(adopted_phase), g!(phase), C::NotEqual, statuses[8]);
    b.pointer(ctx!(root_segments), invalid);
    b.set(local(SEG), ctx!(root_segments));
    // A private RootLifeRequest occupies [fp-224, fp-152), below saved locals.
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 224);
    b.a.store_rbp_slot(RETIRE, R::V0);
    for offset in (0..std::mem::size_of::<RootLifeRequest>()).step_by(8) {
        b.set(field(RETIRE, offset, 0), n(0));
    }
    b.set(
        field(RETIRE, offset_of!(RootLifeRequest, registration), 0),
        local(SEG),
    );
    b.set(
        field(RETIRE, offset_of!(RootLifeRequest, scope), 0),
        field(SEG, offset_of!(RootRegistration, scope), 0),
    );
    b.set(
        field(RETIRE, offset_of!(RootLifeRequest, context), 0),
        local(CTX),
    );
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, RETIRE);
    b.a.mov_imm64(R::V2, 3);
    b.a.call_label(lifetime);
    b.a.cmp_reg_imm32(R::V0, 0);
    b.a.jcc_label(C::Equal, statuses[1]);
    b.a.cmp_reg_imm32(R::V0, 3);
    b.a.jcc_label(C::Equal, statuses[4]);
    b.a.jmp_label(invalid);
    b.a.bind_text_label(finish);
    call_handshake(&mut b, handshake, 5);
    b.a.cmp_reg_imm32(R::V0, 0);
    b.a.jcc_label(C::Equal, statuses[0]);
    b.a.cmp_reg_imm32(R::V0, 1);
    b.a.jcc_label(C::Equal, statuses[2]);
    b.a.jmp_label(invalid);
    for (status, label) in statuses.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(handshake);
    crate::gc_codegen::handshake::emit_step_locked(b.a);
    b.a.ret();
    b.a.bind_text_label(lifetime);
    emit_life_locked(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
fn call_handshake<A: PortableAsm>(b: &mut Emit<'_, A>, target: A::TextLabel, op: u64) {
    b.a.load_rbp_slot(R::V7, HANDSHAKE);
    b.a.load_rbp_slot(R::V6, PARTICIPANT);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(target);
}
