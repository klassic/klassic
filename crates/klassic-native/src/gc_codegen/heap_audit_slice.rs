//! Finite resolve/scan/exit owner, matching gc_heap_audit_slice.h.
use super::*;
use crate::gc_abi::{HeapAuditSlice as T, HeapIncoming as I, ResolveRequest as A};
const SCAN: i32 = 176;
#[allow(dead_code)]
pub(crate) fn emit<P: PortableAsm>(a: &mut P) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let exit = a.create_text_label();
    let clear = a.create_text_label();
    let commit = a.create_text_label();
    let invalid_result = a.create_text_label();
    let cleanup = a.create_text_label();
    let entered = a.create_text_label();
    let forwarded = a.create_text_label();
    let wait = a.create_text_label();
    let exhausted = a.create_text_label();
    let early_clear = a.create_text_label();
    let resolve = a.create_text_label();
    let incoming = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 80);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(T, $f), 0)
        };
    }
    b.pointer(local(Q), invalid);
    b.branch(q!(stage), n(1), C::Above, invalid);
    b.branch(q!(stage), n(1), C::Equal, exit);
    b.pointer(q!(page), invalid);
    for v in [
        q!(observed),
        q!(pending_cursor),
        q!(pending_observed),
        q!(pending_result),
    ] {
        b.branch(v, n(0), C::NotEqual, invalid);
    }
    for i in 0..size_of::<A>() / 8 {
        b.branch(
            field(Q, offset_of!(T, access) + i * 8, 0),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.set(field(Q, offset_of!(T, access), 0), q!(reference));
    b.set(
        field(Q, offset_of!(T, access) + offset_of!(A, scope), 0),
        q!(context),
    );
    b.value(q!(page));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(add(local(Q), n(offset_of!(T, access) as u64)));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(resolve);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::Equal, entered);
    b.a.cmp_reg_imm8(R::V0, 3);
    b.a.jcc_label(C::Equal, forwarded);
    b.a.cmp_reg_imm8(R::V0, 2);
    b.a.jcc_label(C::Equal, wait);
    b.a.cmp_reg_imm8(R::V0, 4);
    b.a.jcc_label(C::Equal, exhausted);
    b.set(local(STATE), n(2));
    b.a.jmp_label(early_clear);
    b.a.bind_text_label(forwarded);
    b.set(local(STATE), n(4));
    b.a.jmp_label(early_clear);
    b.a.bind_text_label(wait);
    b.set(local(STATE), n(5));
    b.a.jmp_label(early_clear);
    b.a.bind_text_label(exhausted);
    b.set(local(STATE), n(6));
    b.a.bind_text_label(early_clear);
    for i in 0..size_of::<A>() / 8 {
        b.set(field(Q, offset_of!(T, access) + i * 8, 0), n(0));
    }
    b.value(local(STATE));
    b.a.jmp_label(done);
    b.a.bind_text_label(entered);
    b.set(q!(stage), n(1));
    b.set(
        local(SCAN),
        add(
            local(Q),
            n((offset_of!(T, access) + offset_of!(A, scope)) as u64),
        ),
    );
    for (offset, value) in [
        (offset_of!(I, page), q!(page)),
        (offset_of!(I, registry), q!(registry)),
        (offset_of!(I, phase), q!(phase)),
        (offset_of!(I, target_start), q!(target_start)),
        (offset_of!(I, target_limit), q!(target_limit)),
        (offset_of!(I, cursor), q!(cursor)),
        (offset_of!(I, observed), n(0)),
    ] {
        b.set(local(SCAN - offset as i32), value);
    }
    b.a.mov_reg_reg(R::V7, R::V5);
    b.a.sub_reg_imm32(R::V7, SCAN);
    b.a.call_label(incoming);
    b.a.store_rbp_slot(STATE, R::V0);
    b.set(q!(pending_result), local(STATE));
    b.set(
        q!(pending_cursor),
        local(SCAN - offset_of!(I, cursor) as i32),
    );
    b.set(
        q!(pending_observed),
        local(SCAN - offset_of!(I, observed) as i32),
    );
    b.a.bind_text_label(exit);
    b.pointer(q!(page), cleanup);
    b.value(q!(page));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(add(local(Q), n(offset_of!(T, access) as u64)));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.mov_imm64(R::V2, 1);
    b.a.call_label(resolve);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, cleanup);
    b.set(local(STATE), q!(pending_result));
    b.branch(local(STATE), n(3), C::Above, invalid_result);
    b.branch(local(STATE), n(2), C::NotEqual, commit);
    b.a.bind_text_label(invalid_result);
    b.set(local(STATE), n(2));
    b.a.jmp_label(clear);
    b.a.bind_text_label(commit);
    b.set(q!(cursor), q!(pending_cursor));
    b.set(q!(observed), q!(pending_observed));
    b.a.bind_text_label(clear);
    for i in 0..size_of::<A>() / 8 {
        b.set(field(Q, offset_of!(T, access) + i * 8, 0), n(0));
    }
    for off in [
        offset_of!(T, stage),
        offset_of!(T, pending_cursor),
        offset_of!(T, pending_observed),
        offset_of!(T, pending_result),
    ] {
        b.set(field(Q, off, 0), n(0));
    }
    b.value(local(STATE));
    b.a.jmp_label(done);
    b.a.bind_text_label(cleanup);
    b.a.mov_imm64(R::V0, 7);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 2);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(resolve);
    super::resolve::emit_resolve_access(b.a);
    b.a.ret();
    b.a.bind_text_label(incoming);
    super::heap_incoming::emit(b.a);
    b.a.ret();
}
