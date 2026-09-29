//! Serial-bound table load/release; gc_weak_table_step_locked.
use super::*;
use crate::gc_abi::{
    WeakEntry, WeakHandle, WeakLoadRequest as Load, WeakTable as Table, WeakTableRequest as Request,
};

#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let load = a.create_text_label();
    let done = a.create_text_label();
    let loader = a.create_text_label();
    let release = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    for _ in 0..2 {
        a.sub_reg_imm8(R::V4, 96);
    }
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    a.mov_reg_reg(R::V0, R::V5);
    a.sub_reg_imm32(R::V0, 176);
    a.store_rbp_slot(NEXT, R::V0);
    let mut b = Emit { a };
    b.pointer(local(Q), invalid);
    b.branch(local(OP), n(1), C::Above, invalid);
    super::weak_follow::validate_table(&mut b, invalid);
    b.branch(
        field(
            Q,
            offset_of!(Request, handle) + offset_of!(WeakHandle, table),
            0,
        ),
        local(G),
        C::NotEqual,
        invalid,
    );
    super::weak_follow::validate_entry(
        &mut b,
        field(
            Q,
            offset_of!(Request, handle) + offset_of!(WeakHandle, index),
            0,
        ),
        field(
            Q,
            offset_of!(Request, handle) + offset_of!(WeakHandle, serial),
            0,
        ),
        invalid,
    );
    b.branch(local(OP), n(0), C::Equal, load);
    b.branch(
        field(G, offset_of!(Table, active), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.a.load_rbp_slot(R::V7, REG);
    b.a.load_rbp_slot(R::V6, CTX);
    b.value(field(
        Q,
        offset_of!(Request, handle) + offset_of!(WeakHandle, serial),
        0,
    ));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(release);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.set(
        field(CTX, offset_of!(WeakEntry, next_free), 0),
        field(G, offset_of!(Table, free_head), 0),
    );
    b.set(
        field(G, offset_of!(Table, free_head), 0),
        field(
            Q,
            offset_of!(Request, handle) + offset_of!(WeakHandle, index),
            0,
        ),
    );
    b.set(
        field(G, offset_of!(Table, active), 0),
        sub(field(G, offset_of!(Table, active), 0), n(1)),
    );
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(load);
    for (to, from) in [
        (offset_of!(Load, admission), offset_of!(Request, admission)),
        (offset_of!(Load, context), offset_of!(Request, context)),
        (
            offset_of!(Load, serial),
            offset_of!(Request, handle) + offset_of!(WeakHandle, serial),
        ),
        (
            offset_of!(Load, output_root),
            offset_of!(Request, output_root),
        ),
    ] {
        b.set(field(NEXT, to, 0), field(Q, from, 0));
    }
    b.set(field(NEXT, offset_of!(Load, slot), 0), local(CTX));
    b.a.load_rbp_slot(R::V7, REG);
    b.a.load_rbp_slot(R::V6, NEXT);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(loader);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 3);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(loader);
    super::weak_load::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(release);
    super::weak_release::emit(b.a);
    b.a.ret();
}
