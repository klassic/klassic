//! Bounded preparation -> observation -> retrying store owner.
use super::*;
use crate::gc_abi::{
    GuardedStore, MutatorContext, RootAccess, RootFollow, RootRequest, RootedLoad as V,
    RootedMarkSet as M, RootedObservation as O, RootedStore as T, RootedStoreAttempt as A,
};
const VALUE: usize = offset_of!(T, value);
const OBS: usize = offset_of!(T, observation);
const ATT: usize = offset_of!(T, attempt);
fn q(off: usize) -> E {
    field(Q, off, 0)
}
fn ptr(off: usize) -> E {
    add(local(Q), n(off as u64))
}
fn stage() -> E {
    q(offset_of!(T, stage))
}
fn call<B: PortableAsm>(b: &mut Emit<'_, B>, target: B::TextLabel, off: usize, op: E) {
    b.value(local(G));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(ptr(off));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(op);
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(target);
    b.a.store_rbp_slot(STATE, R::V0);
}
fn frame<B: PortableAsm>(a: &mut B) {
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
}
fn zero<B: PortableAsm>(b: &mut Emit<'_, B>, off: usize, bytes: usize) {
    for i in (off..off + bytes).step_by(8) {
        b.set(q(i), n(0));
    }
}
fn copy<B: PortableAsm>(b: &mut Emit<'_, B>, dst: usize, src: usize, bytes: usize) {
    for i in (0..bytes).step_by(8) {
        b.set(q(dst + i), q(src + i));
    }
}
fn lease<B: PortableAsm>(b: &mut Emit<'_, B>, off: usize, yes: B::TextLabel) {
    for i in [0, 8, 16] {
        b.branch(
            q(off + offset_of!(RootAccess, follow) + offset_of!(RootFollow, cleanup) + i),
            n(0),
            C::NotEqual,
            yes,
        );
    }
}
fn retry<B: PortableAsm>(b: &mut Emit<'_, B>, ret: &[B::TextLabel]) {
    for (child, result) in [(2, 38), (4, 39), (5, 40), (6, 41), (3, 32)] {
        b.branch(local(STATE), n(child), C::Equal, ret[result]);
    }
}
pub(super) fn emit_next<B: PortableAsm>(a: &mut B) {
    let ret: Vec<_> = (0..45).map(|_| a.create_text_label()).collect();
    let done = a.create_text_label();
    let value = a.create_text_label();
    let child = a.create_text_label();
    let obs = a.create_text_label();
    let attempt = a.create_text_label();
    frame(a);
    let mut b = Emit { a };
    b.pointer(local(Q), ret[44]);
    b.branch(stage(), n(8), C::Above, ret[44]);
    for off in [offset_of!(T, cancel), offset_of!(T, admitted)] {
        b.branch(q(off), n(1), C::Above, ret[44]);
    }
    for (s, r) in [(5, 35), (6, 36), (7, 42), (8, 43)] {
        b.branch(stage(), n(s), C::Equal, ret[r]);
    }
    b.branch(stage(), n(0), C::Equal, value);
    b.branch(stage(), n(1), C::Equal, obs);
    b.branch(stage(), n(3), C::Equal, attempt);
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(obs);
    b.branch(q(OBS + offset_of!(O, stage)), n(4), C::Above, ret[44]);
    for s in [1, 2] {
        b.branch(q(OBS + offset_of!(O, stage)), n(s), C::Equal, ret[33]);
    }
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(attempt);
    b.branch(q(ATT + offset_of!(A, stage)), n(5), C::Above, ret[44]);
    for s in [1, 2] {
        b.branch(q(ATT + offset_of!(A, stage)), n(s), C::Equal, ret[33]);
    }
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(value);
    call(&mut b, child, VALUE, n(0));
    b.value(local(STATE));
    b.a.jmp_label(done);
    for (i, l) in ret.into_iter().enumerate() {
        b.a.bind_text_label(l);
        b.a.mov_imm64(R::V0, i as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(child);
    super::rooted_load::emit_next(b.a);
    b.a.ret();
}
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let ret: Vec<_> = (0..48).map(|_| a.create_text_label()).collect();
    let done = a.create_text_label();
    let finish = a.create_text_label();
    let next = a.create_text_label();
    let next_fn = a.create_text_label();
    let value_fn = a.create_text_label();
    let obs_fn = a.create_text_label();
    let att_fn = a.create_text_label();
    let cleanup = a.create_text_label();
    let cancel = a.create_text_label();
    let resume = a.create_text_label();
    let admitted = a.create_text_label();
    let value = a.create_text_label();
    let obs = a.create_text_label();
    let init = a.create_text_label();
    let attempt = a.create_text_label();
    let again = a.create_text_label();
    let rearm = a.create_text_label();
    let aborted = a.create_text_label();
    let failed = a.create_text_label();
    let null = a.create_text_label();
    frame(a);
    let mut b = Emit { a };
    macro_rules! t {
        ($f:ident) => {
            q(offset_of!(T, $f))
        };
    }
    macro_rules! v {
        ($f:ident) => {
            q(VALUE + offset_of!(V, $f))
        };
    }
    macro_rules! o {
        ($f:ident) => {
            q(OBS + offset_of!(O, $f))
        };
    }
    macro_rules! at {
        ($f:ident) => {
            q(ATT + offset_of!(A, $f))
        };
    }
    const CONTEXT: usize = offset_of!(V, observation)
        + offset_of!(O, receiver)
        + offset_of!(RootAccess, follow)
        + offset_of!(RootFollow, context);
    const ROOT: usize =
        offset_of!(O, receiver) + offset_of!(RootAccess, follow) + offset_of!(RootFollow, root);
    const CELL: usize = offset_of!(RootRequest, handle); // cell is first handle word
    call(&mut b, next_fn, 0, n(0));
    b.branch(local(STATE), n(44), C::Equal, finish);
    b.set(local(SEG), local(STATE));
    let ordinary = b.a.create_text_label();
    b.branch(local(OP), n(47), C::NotEqual, ordinary);
    b.branch(stage(), n(6), C::Equal, rearm);
    b.a.bind_text_label(ordinary);
    b.branch(stage(), n(5), C::AboveOrEqual, finish);
    b.branch(local(OP), n(45), C::Equal, cancel);
    b.branch(local(OP), n(46), C::Equal, resume);
    b.branch(local(OP), local(SEG), C::NotEqual, ret[44]);
    b.branch(local(OP), n(37), C::Equal, ret[44]);
    b.branch(t!(admitted), n(0), C::NotEqual, admitted);
    b.branch(t!(cancel), n(0), C::NotEqual, admitted);
    b.branch(stage(), n(0), C::NotEqual, ret[44]);
    b.branch(local(OP), n(32), C::NotEqual, ret[44]);
    b.branch(v!(stage), n(0), C::NotEqual, ret[44]);
    b.branch(v!(admitted), n(0), C::Equal, ret[44]);
    b.set(local(REG), q(CONTEXT));
    b.pointer(local(REG), ret[44]);
    b.branch(
        q(OBS + offset_of!(O, receiver)),
        local(REG),
        C::NotEqual,
        ret[44],
    );
    b.branch(
        q(OBS + offset_of!(O, output) + CELL),
        q(OBS + ROOT + CELL),
        C::Equal,
        ret[44],
    );
    b.branch(
        q(OBS + offset_of!(O, output) + CELL),
        q(VALUE + offset_of!(V, observation) + offset_of!(O, output) + CELL),
        C::Equal,
        ret[44],
    );
    b.branch(
        field(REG, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        ret[44],
    );
    b.branch(
        field(REG, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        ret[44],
    );
    b.branch(
        field(REG, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        ret[44],
    );
    b.branch(
        field(REG, offset_of!(MutatorContext, access_epoch), 1),
        n(0),
        C::NotEqual,
        ret[44],
    );
    b.set(
        local(USED),
        field(REG, offset_of!(MutatorContext, private_producers), 1),
    );
    b.branch(local(USED), n(0), C::Equal, ret[44]);
    b.branch(local(USED), n(u64::MAX), C::Equal, ret[41]);
    b.set(
        field(REG, offset_of!(MutatorContext, private_producers), 1),
        add(local(USED), n(1)),
    );
    b.set(t!(admitted), n(1));
    b.a.bind_text_label(admitted);
    for (s, l) in [(0, value), (1, obs), (2, init), (3, attempt)] {
        b.branch(stage(), n(s), C::Equal, l);
    }
    b.a.jmp_label(again);
    b.a.bind_text_label(cancel);
    b.set(t!(cancel), n(1));
    b.branch(stage(), n(0), C::NotEqual, next);
    call(&mut b, value_fn, VALUE, n(45));
    b.a.jmp_label(next);
    b.a.bind_text_label(resume);
    b.branch(stage(), n(0), C::NotEqual, ret[44]);
    b.branch(t!(cancel), n(0), C::NotEqual, ret[44]);
    call(&mut b, value_fn, VALUE, local(OP));
    b.a.jmp_label(finish);
    b.a.bind_text_label(value);
    call(&mut b, value_fn, VALUE, local(OP));
    b.set(t!(last), local(STATE));
    let prepared = b.a.create_text_label();
    b.branch(local(STATE), n(35), C::Equal, prepared);
    b.branch(local(STATE), n(36), C::Equal, aborted);
    for r in [42, 43] {
        b.branch(local(STATE), n(r), C::Equal, failed);
    }
    b.a.jmp_label(finish);
    b.a.bind_text_label(prepared);
    b.branch(t!(cancel), n(0), C::NotEqual, aborted);
    b.set(stage(), n(1));
    b.a.jmp_label(next);
    b.a.bind_text_label(obs);
    let obs_cancel = b.a.create_text_label();
    let obs_clean = b.a.create_text_label();
    let obs_release = b.a.create_text_label();
    let obs_run = b.a.create_text_label();
    b.branch(t!(cancel), n(0), C::Equal, obs_clean);
    b.branch(local(OP), n(32), C::Equal, obs_cancel);
    b.a.bind_text_label(obs_clean);
    b.branch(o!(stage), n(0), C::NotEqual, obs_run);
    lease(&mut b, OBS, obs_release);
    b.a.jmp_label(obs_run);
    b.a.bind_text_label(obs_cancel);
    let obs_cancel_release = b.a.create_text_label();
    lease(&mut b, OBS, obs_cancel_release);
    b.a.jmp_label(aborted);
    b.a.bind_text_label(obs_cancel_release);
    call(&mut b, cleanup, OBS, n(1));
    b.branch(local(STATE), n(0), C::NotEqual, ret[32]);
    b.a.jmp_label(aborted);
    b.a.bind_text_label(obs_release);
    call(&mut b, cleanup, OBS, n(1));
    b.branch(local(STATE), n(0), C::NotEqual, ret[32]);
    b.a.bind_text_label(obs_run);
    b.set(local(LEASE), o!(stage));
    b.set(local(NEXT), n(3));
    let obs_call = b.a.create_text_label();
    b.branch(local(LEASE), n(3), C::AboveOrEqual, obs_call);
    b.set(local(NEXT), local(LEASE));
    b.a.bind_text_label(obs_call);
    call(&mut b, obs_fn, OBS, local(NEXT));
    b.set(t!(last), local(STATE));
    let observed = b.a.create_text_label();
    b.branch(local(STATE), n(11), C::Equal, observed);
    b.branch(local(LEASE), n(0), C::NotEqual, next);
    b.branch(local(STATE), n(0), C::Equal, null);
    retry(&mut b, &ret);
    for r in [8, 7] {
        b.branch(local(STATE), n(r), C::Equal, next);
    }
    b.a.jmp_label(failed);
    b.a.bind_text_label(observed);
    let obs_ok = b.a.create_text_label();
    b.branch(o!(result), n(0), C::Equal, obs_ok);
    b.branch(o!(result), n(3), C::NotEqual, failed);
    call(&mut b, obs_fn, OBS, n(4));
    b.branch(local(STATE), n(0), C::Equal, ret[39]);
    b.a.jmp_label(failed);
    b.a.bind_text_label(obs_ok);
    b.set(stage(), n(2));
    b.a.jmp_label(next);
    b.a.bind_text_label(init);
    b.branch(t!(cancel), n(0), C::NotEqual, aborted);
    zero(&mut b, ATT, size_of::<A>());
    b.set(q(ATT), q(OBS));
    copy(&mut b, ATT + ROOT, OBS + ROOT, size_of::<RootRequest>());
    b.set(at!(marks), ptr(VALUE + offset_of!(V, marks)));
    for (dst, src) in [
        (offset_of!(GuardedStore, registry), offset_of!(O, registry)),
        (
            offset_of!(GuardedStore, byte_offset),
            offset_of!(O, byte_offset),
        ),
        (
            offset_of!(GuardedStore, expected),
            offset_of!(O, output) + offset_of!(RootRequest, value),
        ),
    ] {
        b.set(q(ATT + offset_of!(A, commit) + dst), q(OBS + src));
    }
    b.set(stage(), n(3));
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(attempt);
    b.set(local(LEASE), at!(stage));
    let att_release = b.a.create_text_label();
    let att_run = b.a.create_text_label();
    b.branch(local(LEASE), n(0), C::NotEqual, att_run);
    lease(&mut b, ATT, att_release);
    b.a.jmp_label(att_run);
    b.a.bind_text_label(att_release);
    call(&mut b, att_fn, ATT, n(3));
    b.branch(local(STATE), n(0), C::NotEqual, ret[32]);
    b.a.bind_text_label(att_run);
    b.set(local(NEXT), n(3));
    let att_call = b.a.create_text_label();
    let att_normal = b.a.create_text_label();
    b.branch(t!(cancel), n(0), C::Equal, att_normal);
    b.branch(local(LEASE), n(2), C::AboveOrEqual, att_normal);
    b.set(local(NEXT), n(4));
    b.a.jmp_label(att_call);
    b.a.bind_text_label(att_normal);
    b.branch(local(LEASE), n(3), C::AboveOrEqual, att_call);
    b.set(local(NEXT), local(LEASE));
    b.a.bind_text_label(att_call);
    call(&mut b, att_fn, ATT, local(NEXT));
    b.set(t!(last), local(STATE));
    let att_complete = b.a.create_text_label();
    b.branch(local(STATE), n(11), C::Equal, att_complete);
    b.branch(local(LEASE), n(0), C::NotEqual, next);
    b.branch(local(STATE), n(0), C::Equal, null);
    retry(&mut b, &ret);
    for r in [8, 10, 7] {
        b.branch(local(STATE), n(r), C::Equal, next);
    }
    b.a.jmp_label(failed);
    b.a.bind_text_label(att_complete);
    b.set(t!(result), at!(result));
    let success = b.a.create_text_label();
    let att_retry = b.a.create_text_label();
    b.branch(t!(result), n(0), C::Equal, success);
    b.branch(t!(cancel), n(0), C::NotEqual, aborted);
    b.branch(t!(result), n(7), C::Equal, aborted);
    for r in [2, 3, 4, 6, 8, 9] {
        b.branch(t!(result), n(r), C::Equal, att_retry);
    }
    b.a.jmp_label(failed);
    b.a.bind_text_label(success);
    b.set(stage(), n(5));
    b.a.jmp_label(next);
    b.a.bind_text_label(att_retry);
    b.set(stage(), n(4));
    for (r, out) in [(2, 39), (8, 40), (3, 41), (9, 41)] {
        b.branch(t!(result), n(r), C::Equal, ret[out]);
    }
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(again);
    b.branch(t!(cancel), n(0), C::NotEqual, aborted);
    call(&mut b, obs_fn, OBS, n(4));
    b.branch(local(STATE), n(0), C::NotEqual, ret[44]);
    let reprepare = b.a.create_text_label();
    b.branch(t!(result), n(6), C::Equal, reprepare);
    b.set(stage(), n(1));
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(reprepare);
    // Preserve only existing-root bindings and admitted queue/snapshot inputs.
    for off in (0..size_of::<V>()).step_by(8) {
        let keep = off == CONTEXT
            || (ROOT..ROOT + size_of::<RootRequest>()).contains(&off)
            || (offset_of!(O, output)..offset_of!(O, output) + size_of::<RootRequest>())
                .contains(&off)
            || (offset_of!(V, marks) + offset_of!(M, queues)
                ..offset_of!(V, marks) + offset_of!(M, phase) + 8)
                .contains(&off);
        if !keep {
            b.set(q(off), n(0));
        }
    }
    copy(
        &mut b,
        ROOT,
        offset_of!(O, output),
        size_of::<RootRequest>(),
    );
    b.set(stage(), n(0));
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(rearm);
    b.branch(t!(cancel), n(1), C::NotEqual, ret[44]);
    for off in [
        offset_of!(T, admitted),
        offset_of!(V, admitted),
        offset_of!(V, fault),
    ] {
        b.branch(q(off), n(0), C::NotEqual, ret[44]);
    }
    let value_terminal = b.a.create_text_label();
    b.branch(v!(stage), n(9), C::Equal, value_terminal);
    b.branch(v!(stage), n(10), C::NotEqual, ret[44]);
    b.a.bind_text_label(value_terminal);
    let marks_terminal = b.a.create_text_label();
    for state in [0, 2, 3] {
        b.branch(
            q(offset_of!(V, marks) + offset_of!(M, state)),
            n(state),
            C::Equal,
            marks_terminal,
        );
    }
    b.a.jmp_label(ret[44]);
    b.a.bind_text_label(marks_terminal);
    let attempt_clean = b.a.create_text_label();
    b.branch(at!(stage), n(0), C::Equal, attempt_clean);
    b.branch(at!(stage), n(5), C::NotEqual, ret[44]);
    b.branch(at!(result), n(0), C::Equal, ret[44]);
    b.a.bind_text_label(attempt_clean);
    b.set(local(REG), q(CONTEXT));
    b.pointer(local(REG), ret[44]);
    b.branch(q(OBS), local(REG), C::NotEqual, ret[44]);
    b.branch(
        field(REG, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        ret[44],
    );
    b.branch(
        field(REG, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        ret[44],
    );
    b.branch(
        field(REG, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        ret[44],
    );
    b.branch(
        field(REG, offset_of!(MutatorContext, access_epoch), 1),
        n(0),
        C::NotEqual,
        ret[44],
    );
    for off in [
        VALUE,
        offset_of!(V, publication),
        offset_of!(V, marks) + offset_of!(M, jobs),
        offset_of!(V, marks) + offset_of!(M, jobs) + size_of::<crate::gc_abi::RootedMark>(),
        OBS,
        ATT,
    ] {
        // Follow cleanup and the complete ResolveRequest must be empty.
        for i in (48..size_of::<RootAccess>()).step_by(8) {
            b.branch(q(off + i), n(0), C::NotEqual, ret[44]);
        }
    }
    for off in (ATT + offset_of!(A, entries)..ATT + offset_of!(A, entries) + 48).step_by(8) {
        b.branch(q(off), n(0), C::NotEqual, ret[44]);
    }
    for off in (0..size_of::<T>()).step_by(8) {
        let keep = off == CONTEXT
            || off == OBS
            || (offset_of!(O, output)..offset_of!(O, output) + 32).contains(&off)
            || (OBS + ROOT..OBS + ROOT + 32).contains(&off)
            || (OBS + offset_of!(O, registry)..OBS + offset_of!(O, output) + 32).contains(&off);
        if !keep {
            b.set(q(off), n(0));
        }
    }
    copy(
        &mut b,
        ROOT,
        offset_of!(O, output),
        size_of::<RootRequest>(),
    );
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(aborted);
    b.set(stage(), n(6));
    b.a.jmp_label(next);
    b.a.bind_text_label(failed);
    b.set(stage(), n(7));
    b.a.jmp_label(next);
    b.a.bind_text_label(null);
    b.set(stage(), n(8));
    b.a.jmp_label(next);
    b.a.bind_text_label(next);
    call(&mut b, next_fn, 0, n(0));
    b.a.jmp_label(finish);
    for (i, l) in ret.into_iter().enumerate() {
        b.a.bind_text_label(l);
        b.set(local(STATE), n(i as u64));
        b.a.jmp_label(finish);
    }
    b.a.bind_text_label(finish);
    let result = b.a.create_text_label();
    let terminal = b.a.create_text_label();
    for r in [35, 36, 42, 43] {
        b.branch(local(STATE), n(r), C::Equal, terminal);
    }
    b.a.jmp_label(result);
    b.a.bind_text_label(terminal);
    b.pointer(local(Q), result);
    b.branch(stage(), n(5), C::Below, result);
    b.branch(stage(), n(8), C::Above, result);
    b.branch(t!(admitted), n(1), C::NotEqual, result);
    b.set(local(REG), q(CONTEXT));
    b.set(
        local(USED),
        field(REG, offset_of!(MutatorContext, private_producers), 1),
    );
    let account = b.a.create_text_label();
    b.branch(local(USED), n(0), C::NotEqual, account);
    b.set(local(STATE), n(44));
    b.a.jmp_label(result);
    b.a.bind_text_label(account);
    b.set(
        field(REG, offset_of!(MutatorContext, private_producers), 1),
        sub(local(USED), n(1)),
    );
    b.set(t!(admitted), n(0));
    b.a.bind_text_label(result);
    b.value(local(STATE));
    b.a.jmp_label(done);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(next_fn);
    emit_next(b.a);
    b.a.ret();
    b.a.bind_text_label(value_fn);
    super::rooted_load::emit_value(b.a);
    b.a.ret();
    b.a.bind_text_label(obs_fn);
    super::rooted_observation::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(att_fn);
    super::rooted_store_attempt::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(cleanup);
    super::root_access::emit_cleanup(b.a);
    b.a.ret();
}
