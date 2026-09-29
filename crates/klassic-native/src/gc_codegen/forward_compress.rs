//! Frozen-publication forwarding shortening, gc_forward_compress.h.
use super::*;
use crate::gc_abi::{ForwardCell as F, ForwardCompress as T, MAX_OFFSET, RelocationGate};
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let status: [_; 4] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let done = a.create_text_label();
    let gate = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(T, $f), 0)
        };
    }
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.pointer(q!(cell), invalid);
    b.branch(q!(epoch), n(0), C::Equal, invalid);
    b.branch(q!(source_offset), n(0), C::Equal, invalid);
    b.branch(and(q!(source_offset), n(15)), n(0), C::NotEqual, invalid);
    b.branch(q!(source_offset), n(MAX_OFFSET), C::Above, invalid);
    b.branch(q!(incarnation), n(0), C::Equal, invalid);
    b.a.load_rbp_slot(R::V7, G);
    b.a.mov_imm64(R::V6, 0);
    b.a.mov_imm64(R::V2, 3);
    b.a.call_label(gate);
    b.a.cmp_reg_imm8(R::V0, 2);
    b.a.jcc_label(C::Equal, status[2]);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, invalid);
    b.branch(
        q!(epoch),
        field(G, offset_of!(RelocationGate, epoch), 0),
        C::NotEqual,
        status[3],
    );
    b.set(local(SEG), q!(cell));
    b.branch(
        field(SEG, offset_of!(F, source_offset), 0),
        q!(source_offset),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(SEG, offset_of!(F, incarnation), 0),
        q!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(SEG, offset_of!(F, access), 1),
        n(1 << 63),
        C::NotEqual,
        invalid,
    );
    for value in [q!(observed), q!(destination)] {
        b.branch(value.clone(), n(0), C::Equal, invalid);
        super::barrier::reference(&mut b, value.clone(), invalid);
        b.branch(shr(value, 16), q!(source_offset), C::Equal, invalid);
    }
    b.value(add(q!(destination), n(2)));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.value(add(q!(observed), n(2)));
    b.a.load_rbp_slot(R::V11, SEG);
    b.a.add_reg_imm32(R::V11, offset_of!(F, word) as i32);
    b.a.atomic_compare_exchange(R::V11, R::V2);
    b.a.jcc_label(C::Equal, status[0]);
    b.a.jmp_label(status[3]);
    for (i, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, i as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(gate);
    super::relocation_gate::emit_relocation_gate(b.a);
    b.a.ret();
}
