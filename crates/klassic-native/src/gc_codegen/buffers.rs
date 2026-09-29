//! Native SPSC BarrierEntry transport, matching runtime/gc/gc_buffers.h.
use super::*;
use crate::gc_abi::{BarrierDrain, BarrierEntry, BufferCursor};
const BUFFER: i32 = SEG;
const TOP: i32 = USED;
const PUBLISHED: i32 = PREV;
const CONSUMED: i32 = NEXT;
const INDEX: i32 = KIND;
macro_rules! buffer {
    (published) => {
        field(BUFFER, offset_of!(BufferCursor, published), 8)
    };
    (consumed) => {
        field(BUFFER, offset_of!(BufferCursor, consumed), 8)
    };
    ($f:ident) => {
        field(BUFFER, offset_of!(BufferCursor, $f), 0)
    };
}
const _: () = assert!(std::mem::size_of::<BarrierEntry>() == 48);
fn bytes(n: E) -> E {
    add(shl(n.clone(), 5), shl(n, 4))
}
fn geometry<A: PortableAsm>(b: &mut Emit<'_, A>, invalid: A::TextLabel) {
    b.pointer(buffer!(entries), invalid);
    b.branch(buffer!(capacity), n(0), C::Equal, invalid);
    b.branch(buffer!(capacity), n(u32::MAX as u64), C::Above, invalid);
    b.branch(
        add(buffer!(entries), bytes(buffer!(capacity))),
        buffer!(entries),
        C::Below,
        invalid,
    );
}
fn ring_address<A: PortableAsm>(b: &mut Emit<'_, A>, position: E, dest: i32) {
    b.value(position);
    b.a.store_rbp_slot(INDEX, R::V0);
    b.value(buffer!(capacity));
    b.a.mov_reg_reg(R::V1, R::V0);
    b.a.load_rbp_slot(R::V0, INDEX);
    b.a.div_unsigned(R::V1);
    b.a.store_rbp_slot(dest, R::V2);
    b.set(local(dest), add(buffer!(entries), bytes(local(dest))));
}
fn finish<A: PortableAsm>(b: &mut Emit<'_, A>, labels: &[A::TextLabel]) {
    let done = b.a.create_text_label();
    for (status, label) in labels.iter().enumerate() {
        b.a.bind_text_label(*label);
        b.a.mov_imm64(R::V0, status as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
/// V7=ctx, V6=entry, V2=append/publish selector. Constant work, no calls/polls.
#[allow(dead_code)]
pub(crate) fn emit_buffer_step<A: PortableAsm>(a: &mut A) {
    let statuses: [_; 4] = std::array::from_fn(|_| a.create_text_label());
    let invalid = statuses[1];
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(CTX, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(3), C::Above, invalid);
    b.branch(
        ctx!(abi_version),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(ctx!(thread_id), n(0), C::Equal, invalid);
    b.set(local(STATE), ctx!(state));
    let state_ok = b.a.create_text_label();
    b.branch(local(STATE), n(0), C::Equal, state_ok);
    b.branch(local(OP), n(2), C::Below, invalid);
    b.branch(local(STATE), n(1), C::Equal, state_ok);
    b.branch(local(STATE), n(3), C::NotEqual, invalid);
    b.a.bind_text_label(state_ok);
    b.set(
        local(BUFFER),
        add(local(CTX), n(offset_of!(MutatorContext, satb) as u64)),
    );
    let selected = b.a.create_text_label();
    b.branch(and(local(OP), n(1)), n(0), C::Equal, selected);
    b.set(
        local(BUFFER),
        add(local(CTX), n(offset_of!(MutatorContext, remembered) as u64)),
    );
    b.a.bind_text_label(selected);
    geometry(&mut b, invalid);
    b.set(local(TOP), buffer!(top));
    b.set(local(PUBLISHED), buffer!(published));
    b.set(local(CONSUMED), buffer!(consumed));
    b.branch(local(PUBLISHED), local(TOP), C::Above, invalid);
    b.branch(local(CONSUMED), local(PUBLISHED), C::Above, invalid);
    b.branch(
        sub(local(TOP), local(CONSUMED)),
        buffer!(capacity),
        C::Above,
        invalid,
    );
    let publish = b.a.create_text_label();
    b.branch(local(OP), n(2), C::AboveOrEqual, publish);
    b.pointer(local(Q), invalid);
    b.branch(local(TOP), n(u64::MAX), C::Equal, statuses[3]);
    b.branch(
        sub(local(TOP), local(CONSUMED)),
        buffer!(capacity),
        C::Equal,
        statuses[2],
    );
    ring_address(&mut b, local(TOP), REG);
    for offset in (0..48).step_by(8) {
        b.set(field(REG, offset, 0), field(Q, offset, 0));
    }
    b.set(buffer!(top), add(local(TOP), n(1)));
    b.a.jmp_label(statuses[0]);
    b.a.bind_text_label(publish);
    b.set(buffer!(published), local(TOP));
    b.a.jmp_label(statuses[0]);
    finish(&mut b, &statuses);
}
/// V7=BarrierDrain. Copies <=256 records before release-consumed; no top read.
#[allow(dead_code)]
pub(crate) fn emit_buffer_drain<A: PortableAsm>(a: &mut A) {
    let statuses: [_; 3] = std::array::from_fn(|_| a.create_text_label());
    let invalid = statuses[1];
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    let source = field(Q, offset_of!(BarrierDrain, buffer), 0);
    let out = field(Q, offset_of!(BarrierDrain, output), 0);
    let capacity = field(Q, offset_of!(BarrierDrain, capacity), 0);
    b.pointer(source.clone(), invalid);
    b.pointer(out.clone(), invalid);
    b.branch(capacity.clone(), n(0), C::Equal, invalid);
    b.branch(capacity.clone(), n(256), C::Above, invalid);
    b.branch(
        add(out.clone(), bytes(capacity.clone())),
        out.clone(),
        C::Below,
        invalid,
    );
    b.set(local(BUFFER), source);
    geometry(&mut b, invalid);
    b.set(local(CONSUMED), buffer!(consumed));
    b.set(local(PUBLISHED), buffer!(published));
    b.branch(local(CONSUMED), local(PUBLISHED), C::Above, invalid);
    b.set(local(REG), sub(local(PUBLISHED), local(CONSUMED)));
    b.branch(local(REG), buffer!(capacity), C::Above, invalid);
    let count_ready = b.a.create_text_label();
    b.branch(local(REG), capacity.clone(), C::Below, count_ready);
    b.set(local(REG), capacity);
    b.a.bind_text_label(count_ready);
    b.set(local(TOP), n(0)); // loop counter, NOT producer top
    ring_address(&mut b, local(CONSUMED), G);
    b.set(local(CTX), out);
    b.set(local(OP), add(buffer!(entries), bytes(buffer!(capacity))));
    let copy = b.a.create_text_label();
    let copied = b.a.create_text_label();
    b.a.bind_text_label(copy);
    b.branch(local(TOP), local(REG), C::Equal, copied);
    b.a.load_rbp_slot(R::V8, G);
    b.a.load_rbp_slot(R::V9, CTX);
    for offset in (0..48).step_by(8) {
        b.a.load_ptr_disp32(R::V0, R::V8, offset);
        b.a.store_ptr_disp32(R::V9, offset, R::V0);
    }
    b.a.add_reg_imm32(R::V8, 48);
    b.a.add_reg_imm32(R::V9, 48);
    b.a.store_rbp_slot(G, R::V8);
    b.a.store_rbp_slot(CTX, R::V9);
    let no_wrap = b.a.create_text_label();
    b.branch(local(G), local(OP), C::NotEqual, no_wrap);
    b.set(local(G), buffer!(entries));
    b.a.bind_text_label(no_wrap);
    b.set(local(TOP), add(local(TOP), n(1)));
    b.a.jmp_label(copy);
    b.a.bind_text_label(copied);
    b.set(field(Q, offset_of!(BarrierDrain, written), 0), local(REG));
    b.set(buffer!(consumed), add(local(CONSUMED), local(REG)));
    b.branch(
        add(local(CONSUMED), local(REG)),
        local(PUBLISHED),
        C::Equal,
        statuses[0],
    );
    b.a.jmp_label(statuses[2]);
    finish(&mut b, &statuses);
}
