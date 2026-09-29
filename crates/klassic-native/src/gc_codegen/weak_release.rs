//! Explicit weak-handle disposal; matches gc_weak_release.h.
use super::*;
use crate::gc_abi::{MetadataLeaseRequest as L, WeakLoadGate as T, WeakSlot as S};
use std::mem::size_of;
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let held = a.create_text_label();
    let clear = a.create_text_label();
    let metadata = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    for _ in 0..2 {
        a.sub_reg_imm8(R::V4, 80);
    }
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    a.mov_reg_reg(R::V0, R::V5);
    a.sub_reg_imm32(R::V0, 144);
    a.store_rbp_slot(NEXT, R::V0);
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.branch(local(OP), n(0), C::Equal, invalid);
    b.pointer(field(G, offset_of!(T, pool), 0), invalid);
    b.branch(field(G, offset_of!(T, closed), 0), n(1), C::Above, invalid);
    b.branch(
        field(Q, offset_of!(S, gate), 0),
        local(G),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(Q, offset_of!(S, serial), 0),
        local(OP),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(Q, offset_of!(S, reference), 0),
        n(0),
        C::NotEqual,
        held,
    );
    for off in (0..size_of::<L>()).step_by(8) {
        b.branch(
            field(Q, offset_of!(S, lease) + off, 0),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.a.jmp_label(clear);
    b.a.bind_text_label(held);
    for off in (0..size_of::<L>()).step_by(8) {
        b.set(field(NEXT, off, 0), field(Q, offset_of!(S, lease) + off, 0));
    }
    call(&mut b, metadata, NEXT, 2);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    for off in [0, 8] {
        b.branch(
            field(NEXT, off, 0),
            field(Q, offset_of!(S, lease) + off, 0),
            C::NotEqual,
            invalid,
        );
    }
    b.set(local(REG), add(local(Q), n(offset_of!(S, lease) as u64)));
    call(&mut b, metadata, REG, 1);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(clear);
    for off in (0..size_of::<S>()).step_by(8) {
        b.set(field(Q, off, 0), n(0));
    }
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 3);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
}
fn call<B: PortableAsm>(b: &mut Emit<'_, B>, label: B::TextLabel, q: i32, op: u64) {
    b.value(field(G, offset_of!(T, pool), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, q);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
}
