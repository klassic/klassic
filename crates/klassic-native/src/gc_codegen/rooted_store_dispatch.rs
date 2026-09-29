//! Shared native-lock dispatch for bounded rooted load/store controllers.
use super::*;
use crate::gc_abi::{
    RootedLoad as V, RootedMarkSet as M, RootedStore as T, RootedStoreRuntime as D,
};
fn call<B: PortableAsm>(b: &mut Emit<'_, B>, target: B::TextLabel, first: E, op: E) {
    b.value(first);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(local(Q));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(op);
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(target);
    b.a.store_rbp_slot(STATE, R::V0);
}
fn native<B: PortableAsm>(b: &mut Emit<'_, B>, off: usize) {
    b.value(field(G, off, 0));
    b.a.mov_reg_reg(R::V10, R::V0);
    b.value(local(LEASE));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.call_native_unary(R::V10);
}
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    emit_dispatch(a, false);
}
#[allow(dead_code)]
pub(crate) fn emit_load<B: PortableAsm>(a: &mut B) {
    emit_dispatch(a, true);
}
fn emit_dispatch<B: PortableAsm>(a: &mut B, load: bool) {
    let ret: Vec<_> = (0..45).map(|_| a.create_text_label()).collect();
    let done = a.create_text_label();
    let dynamic = a.create_text_label();
    let next = a.create_text_label();
    let store = a.create_text_label();
    let admission = a.create_text_label();
    let cancel = a.create_text_label();
    let rearm = a.create_text_label();
    let direct = a.create_text_label();
    let locked = a.create_text_label();
    let choose = a.create_text_label();
    let admit = a.create_text_label();
    let heap = a.create_text_label();
    let selected = a.create_text_label();
    let execute = a.create_text_label();
    let release = a.create_text_label();
    let admitted = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! d {
        ($f:ident) => {
            field(G, offset_of!(D, $f), 0)
        };
    }
    macro_rules! q {
        ($f:ident) => {
            field(
                Q,
                if load {
                    offset_of!(V, $f)
                } else {
                    offset_of!(T, $f)
                },
                0,
            )
        };
    }
    b.pointer(local(G), ret[44]);
    b.branch(local(OP), n(2), C::Above, ret[44]);
    call(&mut b, next, n(0), n(0));
    b.branch(local(STATE), n(44), C::Equal, dynamic);
    b.set(local(SEG), local(STATE));
    b.branch(local(OP), n(1), C::Equal, cancel);
    b.branch(local(OP), n(2), C::Equal, rearm);
    for r in [35, 36, 42, 43, 33] {
        b.branch(local(SEG), n(r), C::Equal, direct);
    }
    b.a.jmp_label(locked);
    b.a.bind_text_label(cancel);
    b.set(local(SEG), n(45));
    b.a.jmp_label(direct);
    b.a.bind_text_label(rearm);
    let needs_heap = b.a.create_text_label();
    b.branch(
        q!(stage),
        n(if load { 10 } else { 6 }),
        C::Equal,
        needs_heap,
    );
    b.set(local(SEG), n(47));
    b.a.jmp_label(direct);
    b.a.bind_text_label(needs_heap);
    b.set(local(SEG), n(32));
    b.a.bind_text_label(locked);
    b.set(local(CTX), field(Q, 0, 0));
    b.pointer(local(CTX), ret[44]);
    b.branch(d!(lock), n(0), C::Equal, ret[44]);
    b.branch(d!(unlock), n(0), C::Equal, ret[44]);
    for (off, width, value) in [
        (
            offset_of!(MutatorContext, abi_version),
            4,
            crate::gc_abi::ABI_VERSION as u64,
        ),
        (offset_of!(MutatorContext, state), 4, 0),
        (offset_of!(MutatorContext, access_epoch), 8, 0),
    ] {
        b.branch(field(CTX, off, width), n(value), C::NotEqual, ret[44]);
    }
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        ret[44],
    );
    b.set(local(KIND), n(0));
    b.branch(local(OP), n(0), C::NotEqual, choose);
    if !load {
        b.branch(q!(stage), n(0), C::NotEqual, choose);
    }
    b.branch(q!(cancel), n(0), C::NotEqual, choose);
    b.branch(local(SEG), n(37), C::Equal, admit);
    b.branch(q!(admitted), n(0), C::NotEqual, choose);
    if load {
        b.branch(q!(stage), n(0), C::NotEqual, choose);
    } else {
        b.branch(field(Q, offset_of!(V, stage), 0), n(0), C::NotEqual, choose);
        b.branch(
            field(Q, offset_of!(V, admitted), 0),
            n(0),
            C::NotEqual,
            choose,
        );
    }
    b.a.jmp_label(admit);
    b.a.bind_text_label(choose);
    b.branch(local(OP), n(2), C::Equal, heap);
    b.branch(local(SEG), n(32), C::Equal, heap);
    b.branch(local(SEG), n(34), C::NotEqual, ret[44]);
    b.set(
        local(USED),
        field(Q, offset_of!(V, marks) + offset_of!(M, current), 0),
    );
    b.branch(local(USED), n(1), C::Above, ret[44]);
    b.set(local(LEASE), d!(queue_locks));
    b.branch(local(USED), n(0), C::Equal, selected);
    b.set(local(LEASE), field(G, offset_of!(D, queue_locks) + 8, 0));
    b.a.jmp_label(selected);
    b.a.bind_text_label(heap);
    b.pointer(d!(heap_pool), ret[44]);
    b.set(local(LEASE), d!(heap_lock));
    b.a.jmp_label(selected);
    b.a.bind_text_label(admit);
    b.pointer(d!(admission), ret[44]);
    b.set(local(KIND), n(1));
    b.set(local(LEASE), d!(registry_lock));
    b.a.bind_text_label(selected);
    b.branch(local(LEASE), n(0), C::Equal, ret[44]);
    native(&mut b, offset_of!(D, lock));
    b.branch(local(KIND), n(0), C::Equal, execute);
    call(&mut b, admission, d!(admission), n(0));
    b.branch(local(STATE), n(0), C::Equal, admitted);
    b.set(local(NEXT), n(44));
    let wait = b.a.create_text_label();
    let exhausted = b.a.create_text_label();
    b.branch(local(STATE), n(1), C::Equal, wait);
    b.branch(local(STATE), n(3), C::Equal, exhausted);
    b.a.jmp_label(release);
    b.a.bind_text_label(wait);
    b.set(local(NEXT), n(38));
    b.a.jmp_label(release);
    b.a.bind_text_label(exhausted);
    b.set(local(NEXT), n(41));
    b.a.jmp_label(release);
    b.a.bind_text_label(admitted);
    b.set(local(NEXT), n(32));
    b.branch(local(SEG), n(37), C::NotEqual, release);
    call(&mut b, store, n(0), n(46));
    b.set(local(NEXT), local(STATE));
    b.a.jmp_label(release);
    b.a.bind_text_label(execute);
    b.set(local(REG), d!(heap_pool));
    let invoke = b.a.create_text_label();
    b.branch(local(SEG), n(34), C::NotEqual, invoke);
    b.set(local(REG), n(0));
    b.a.bind_text_label(invoke);
    let step = b.a.create_text_label();
    b.branch(local(OP), n(2), C::NotEqual, step);
    b.set(local(SEG), n(47));
    b.a.bind_text_label(step);
    call(&mut b, store, local(REG), local(SEG));
    b.set(local(NEXT), local(STATE));
    b.a.bind_text_label(release);
    native(&mut b, offset_of!(D, unlock));
    b.set(local(STATE), local(NEXT));
    b.a.jmp_label(dynamic);
    b.a.bind_text_label(direct);
    call(&mut b, store, n(0), local(SEG));
    b.a.jmp_label(dynamic);
    for (i, l) in ret.into_iter().enumerate() {
        b.a.bind_text_label(l);
        b.set(local(STATE), n(i as u64));
        b.a.jmp_label(dynamic);
    }
    b.a.bind_text_label(dynamic);
    b.value(local(STATE));
    b.a.jmp_label(done);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(next);
    if load {
        super::rooted_load::emit_next(b.a);
    } else {
        super::rooted_store::emit_next(b.a);
    }
    b.a.ret();
    b.a.bind_text_label(store);
    if load {
        super::rooted_load::emit(b.a);
    } else {
        super::rooted_store::emit(b.a);
    }
    b.a.ret();
    b.a.bind_text_label(admission);
    super::rooted_load_admit::emit(b.a);
    b.a.ret();
}
