//! Rooted scalar owner; mirrors gc_rooted_scalar.h without callbacks.
use super::*;
use crate::gc_abi::{GuardedScalar as L, RootAccess as A, RootFollow as F, RootedScalar as T};
use crate::gc_abi::{MetadataLeaseRequest, ResolveRequest};
const FIELD: i32 = 160;
fn call<B: PortableAsm>(b: &mut Emit<'_, B>, label: B::TextLabel, pool: E, op: u64) {
    b.value(pool);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(local(Q));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
}
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let returns: Vec<_> = (0..12).map(|_| a.create_text_label()).collect();
    let finish = a.create_text_label();
    let dynamic = a.create_text_label();
    let enter = a.create_text_label();
    let observe = a.create_text_label();
    let exit = a.create_text_label();
    let release = a.create_text_label();
    let abort = a.create_text_label();
    let acquire = a.create_text_label();
    let reader = a.create_text_label();
    let cleanup = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 48);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(T, $f), 0)
        };
    }

    b.pointer(local(Q), returns[1]);
    b.branch(local(OP), n(4), C::Above, returns[1]);
    b.branch(q!(stage), n(4), C::Above, returns[1]);
    b.branch(q!(operation), n(1), C::Above, returns[1]);
    b.branch(local(OP), n(4), C::Equal, abort);
    b.branch(local(OP), n(0), C::Equal, enter);
    b.branch(local(OP), n(1), C::Equal, observe);
    b.branch(local(OP), n(2), C::Equal, exit);
    b.branch(q!(stage), n(1), C::Equal, returns[8]);
    b.branch(q!(stage), n(2), C::Equal, returns[10]);
    b.branch(local(OP), n(3), C::Equal, release);
    b.a.bind_text_label(abort);
    for (stage, result) in [(2, 10), (3, 9), (4, 11)] {
        b.branch(q!(stage), n(stage), C::Equal, returns[result]);
    }
    let abort_guard = b.a.create_text_label();
    b.branch(q!(stage), n(1), C::Equal, abort_guard);
    for off in [0, 8, 16] {
        b.branch(
            field(Q, offset_of!(F, cleanup) + off, 0),
            n(0),
            C::NotEqual,
            returns[7],
        );
    }
    b.set(q!(result), n(2));
    b.set(q!(stage), n(4));
    b.a.jmp_label(returns[11]);
    b.a.bind_text_label(abort_guard);
    b.set(q!(result), n(2));
    b.set(q!(stage), n(2));
    b.a.jmp_label(returns[10]);
    b.a.bind_text_label(enter);
    for (stage, result) in [(1, 8), (2, 10), (3, 9), (4, 11)] {
        b.branch(q!(stage), n(stage), C::Equal, returns[result]);
    }
    call(&mut b, acquire, local(G), 0);
    b.branch(local(STATE), n(8), C::NotEqual, dynamic);
    b.set(q!(stage), n(1));
    b.a.jmp_label(dynamic);
    b.a.bind_text_label(observe);
    b.branch(q!(stage), n(2), C::Equal, returns[10]);
    b.branch(q!(stage), n(1), C::NotEqual, returns[1]);
    b.set(local(CTX), field(Q, offset_of!(F, context), 0));
    for (offset, value) in [
        (
            offset_of!(L, scope),
            add(
                local(Q),
                n((offset_of!(A, access) + offset_of!(ResolveRequest, scope)) as u64),
            ),
        ),
        (
            offset_of!(L, page),
            field(
                Q,
                offset_of!(F, cleanup) + offset_of!(MetadataLeaseRequest, page),
                0,
            ),
        ),
        (offset_of!(L, registry), q!(registry)),
        (offset_of!(L, byte_offset), q!(byte_offset)),
        (offset_of!(L, value), q!(value)),
        (
            offset_of!(L, phase),
            field(CTX, offset_of!(MutatorContext, adopted_phase), 8),
        ),
    ] {
        b.set(local(FIELD - offset as i32), value);
    }
    b.value(q!(operation));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.mov_reg_reg(R::V7, R::V5);
    b.a.sub_reg_imm32(R::V7, FIELD);
    b.a.call_label(reader);
    b.a.store_rbp_slot(STATE, R::V0);
    b.set(q!(result), local(STATE));
    b.set(q!(value), local(FIELD - offset_of!(L, value) as i32));
    b.set(q!(stage), n(2));
    b.a.jmp_label(returns[10]);
    b.a.bind_text_label(exit);
    b.branch(q!(stage), n(2), C::NotEqual, returns[1]);
    call(&mut b, cleanup, n(0), 0);
    b.branch(local(STATE), n(0), C::NotEqual, returns[10]);
    b.set(q!(stage), n(3));
    b.a.jmp_label(returns[9]);
    b.a.bind_text_label(release);
    b.branch(q!(stage), n(4), C::Equal, returns[11]);
    call(&mut b, cleanup, local(G), 1);
    b.branch(local(STATE), n(0), C::NotEqual, dynamic);
    b.branch(q!(stage), n(0), C::Equal, returns[0]);
    b.set(q!(stage), n(4));
    b.a.jmp_label(returns[11]);
    b.a.bind_text_label(dynamic);
    b.value(local(STATE));
    b.a.jmp_label(finish);
    for (i, label) in returns.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, i as u64);
        b.a.jmp_label(finish);
    }
    b.a.bind_text_label(finish);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(acquire);
    super::root_access::emit_root_access(b.a);
    b.a.ret();
    b.a.bind_text_label(reader);
    super::barrier::emit_guarded_scalar(b.a);
    b.a.ret();
    b.a.bind_text_label(cleanup);
    super::root_access::emit_cleanup(b.a);
    b.a.ret();
}
