//! Bounded rooted-load owner; mirrors runtime/gc/gc_rooted_load.h.
use super::*;
use crate::gc_abi::{
    RootAccess as A, RootFollow as F, RootRequest, RootedLoad as T, RootedMark as J,
    RootedMarkSet as M, RootedObservation as O,
};
const OBS: usize = offset_of!(T, observation);
const MARK: usize = offset_of!(T, marks);
const PUB: usize = offset_of!(T, publication);
fn q(off: usize) -> E {
    field(Q, off, 0)
}
fn stage() -> E {
    q(offset_of!(T, stage))
}
fn last() -> E {
    q(offset_of!(T, last))
}
fn cancel() -> E {
    q(offset_of!(T, cancel))
}
fn mark(off: usize) -> E {
    q(MARK + off)
}
fn ptr(off: usize) -> E {
    add(local(Q), n(off as u64))
}
fn job<B: PortableAsm>(b: &mut Emit<'_, B>) {
    let selected = b.a.create_text_label();
    b.set(local(REG), ptr(MARK + offset_of!(M, jobs)));
    b.branch(mark(offset_of!(M, current)), n(0), C::Equal, selected);
    b.set(local(REG), add(local(REG), n(size_of::<J>() as u64)));
    b.a.bind_text_label(selected);
}
fn lease<B: PortableAsm>(b: &mut Emit<'_, B>, base: i32, present: B::TextLabel) {
    for off in [0, 8, 16] {
        b.branch(
            field(
                base,
                offset_of!(A, follow) + offset_of!(F, cleanup) + off,
                0,
            ),
            n(0),
            C::NotEqual,
            present,
        );
    }
}
fn call<B: PortableAsm>(b: &mut Emit<'_, B>, target: B::TextLabel, first: E, second: E, op: E) {
    b.value(first);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(second);
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(op);
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(target);
    b.a.store_rbp_slot(STATE, R::V0);
}
fn retry<B: PortableAsm>(b: &mut Emit<'_, B>, ret: &[B::TextLabel]) {
    for (child, event) in [(2, 38), (4, 39), (5, 40), (6, 41), (3, 32)] {
        b.branch(local(STATE), n(child), C::Equal, ret[event]);
    }
}
fn frame<B: PortableAsm>(a: &mut B) {
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
}
pub(super) fn emit_next<B: PortableAsm>(a: &mut B) {
    let ret: Vec<_> = (0..47).map(|_| a.create_text_label()).collect();
    let done = a.create_text_label();
    let action = a.create_text_label();
    let obs = a.create_text_label();
    let marking = a.create_text_label();
    let access = a.create_text_label();
    let queue = a.create_text_label();
    let recap = a.create_text_label();
    frame(a);
    let mut b = Emit { a };
    b.pointer(local(Q), ret[44]);
    b.branch(stage(), n(16), C::Above, ret[44]);
    for off in [
        offset_of!(T, cancel),
        offset_of!(T, fault),
        offset_of!(T, admitted),
    ] {
        b.branch(q(off), n(1), C::Above, ret[44]);
    }
    for (s, r) in [(9, 35), (10, 36), (11, 42), (12, 43)] {
        b.branch(stage(), n(s), C::Equal, ret[r]);
    }
    b.set(local(SEG), n(32));
    for (s, l) in [
        (0, obs),
        (2, marking),
        (4, access),
        (5, access),
        (14, access),
        (15, access),
        (8, recap),
    ] {
        b.branch(stage(), n(s), C::Equal, l);
    }
    b.a.jmp_label(action);
    b.a.bind_text_label(obs);
    b.branch(q(OBS + offset_of!(O, stage)), n(4), C::Above, ret[44]);
    for s in [1, 2] {
        b.branch(q(OBS + offset_of!(O, stage)), n(s), C::Equal, access);
    }
    b.a.jmp_label(action);
    b.a.bind_text_label(marking);
    b.branch(mark(offset_of!(M, current)), n(1), C::Above, ret[44]);
    job(&mut b);
    for s in [1, 2] {
        b.branch(field(REG, offset_of!(J, stage), 0), n(s), C::Equal, access);
    }
    b.branch(field(REG, offset_of!(J, stage), 0), n(3), C::Equal, queue);
    b.a.jmp_label(action);
    b.a.bind_text_label(recap);
    b.branch(cancel(), n(0), C::Equal, ret[37]);
    b.a.jmp_label(action);
    b.a.bind_text_label(access);
    b.a.jmp_label(ret[33]);
    b.a.bind_text_label(queue);
    b.set(local(SEG), n(34));
    b.a.bind_text_label(action);
    b.branch(cancel(), n(0), C::NotEqual, ret[32]);
    b.value(local(SEG));
    b.a.jmp_label(done);
    for (i, l) in ret.into_iter().enumerate() {
        b.a.bind_text_label(l);
        b.a.mov_imm64(R::V0, i as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
}
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    emit_mode(a, false);
}
#[allow(dead_code)]
pub(crate) fn emit_value<B: PortableAsm>(a: &mut B) {
    emit_mode(a, true);
}
fn emit_mode<B: PortableAsm>(a: &mut B, root_value: bool) {
    let ret: Vec<_> = (0..47).map(|_| a.create_text_label()).collect();
    let states: Vec<_> = (0..8).map(|_| a.create_text_label()).collect();
    let heal_states: Vec<_> = (0..4).map(|_| a.create_text_label()).collect();
    let heal = a.create_text_label();
    let heal_enter = a.create_text_label();
    let next = a.create_text_label();
    let observe = a.create_text_label();
    let marks = a.create_text_label();
    let enter = a.create_text_label();
    let publish = a.create_text_label();
    let cleanup = a.create_text_label();
    let next_return = a.create_text_label();
    let done = a.create_text_label();
    let request_cancel = a.create_text_label();
    let resume = a.create_text_label();
    let rearm = a.create_text_label();
    let cancelled = a.create_text_label();
    let cancel_heap = a.create_text_label();
    let restart = a.create_text_label();
    let failed = a.create_text_label();
    frame(a);
    let mut b = Emit { a };
    call(&mut b, next, n(0), local(Q), n(0));
    b.branch(local(STATE), n(44), C::Equal, ret[44]);
    if root_value {
        b.branch(stage(), n(13), C::AboveOrEqual, ret[44]);
        for off in [
            offset_of!(O, registry),
            offset_of!(O, byte_offset),
            offset_of!(O, result),
            offset_of!(O, output) + offset_of!(RootRequest, value),
            offset_of!(F, root) + offset_of!(RootRequest, value),
        ] {
            b.branch(q(OBS + off), n(0), C::NotEqual, ret[44]);
        }
        for off in (offset_of!(F, cleanup)..size_of::<A>()).step_by(8) {
            b.branch(q(OBS + off), n(0), C::NotEqual, ret[44]);
        }
        for off in (0..offset_of!(RootRequest, value)).step_by(8) {
            b.branch(
                q(OBS + offset_of!(F, root) + off),
                q(OBS + offset_of!(O, output) + off),
                C::NotEqual,
                ret[44],
            );
        }
        let bound = b.a.create_text_label();
        b.branch(q(OBS + offset_of!(O, stage)), n(0), C::Equal, bound);
        b.branch(q(OBS + offset_of!(O, stage)), n(4), C::NotEqual, ret[44]);
        b.a.bind_text_label(bound);
    }
    b.branch(local(OP), n(47), C::Equal, rearm);
    for r in [35, 36, 42, 43] {
        b.branch(local(STATE), n(r), C::Equal, ret[r as usize]);
    }
    b.branch(local(OP), n(45), C::Equal, request_cancel);
    b.branch(local(OP), n(46), C::Equal, resume);
    b.branch(local(OP), local(STATE), C::NotEqual, ret[44]);
    b.branch(local(OP), n(37), C::Equal, ret[44]);
    let admitted = b.a.create_text_label();
    let admit = b.a.create_text_label();
    b.branch(q(offset_of!(T, admitted)), n(0), C::NotEqual, admitted);
    b.branch(cancel(), n(0), C::NotEqual, admitted);
    b.branch(local(OP), n(32), C::NotEqual, admitted);
    b.branch(stage(), n(0), C::Equal, admit);
    b.branch(stage(), n(1), C::NotEqual, admitted);
    b.a.bind_text_label(admit);
    b.set(local(CTX), q(OBS + offset_of!(F, context)));
    b.pointer(local(CTX), ret[44]);
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        ret[44],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        ret[44],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        ret[44],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        ret[44],
    );
    let producers = field(CTX, offset_of!(MutatorContext, private_producers), 8);
    b.set(local(USED), producers.clone());
    b.branch(local(USED), n(u64::MAX), C::Equal, ret[41]);
    b.set(producers, add(local(USED), n(1)));
    b.set(q(offset_of!(T, admitted)), n(1));
    b.a.bind_text_label(admitted);
    let dispatch = b.a.create_text_label();
    b.branch(cancel(), n(0), C::Equal, dispatch);
    b.branch(local(OP), n(32), C::Equal, cancel_heap);
    b.a.bind_text_label(dispatch);
    for (s, l) in states.iter().enumerate() {
        b.branch(stage(), n(s as u64), C::Equal, *l);
    }
    for (s, l) in heal_states.iter().enumerate() {
        b.branch(stage(), n(13 + s as u64), C::Equal, *l);
    }
    b.a.jmp_label(ret[44]);
    b.a.bind_text_label(rearm);
    b.branch(stage(), n(10), C::NotEqual, ret[44]);
    b.branch(cancel(), n(1), C::NotEqual, ret[44]);
    b.branch(q(offset_of!(T, fault)), n(0), C::NotEqual, ret[44]);
    b.branch(q(offset_of!(T, admitted)), n(0), C::NotEqual, ret[44]);
    let obs_clean = b.a.create_text_label();
    for value in [0, 3, 4] {
        b.branch(q(OBS + offset_of!(O, stage)), n(value), C::Equal, obs_clean);
    }
    b.a.jmp_label(ret[44]);
    b.a.bind_text_label(obs_clean);
    let marks_clean = b.a.create_text_label();
    for value in [0, 3] {
        b.branch(mark(offset_of!(M, state)), n(value), C::Equal, marks_clean);
    }
    b.a.jmp_label(ret[44]);
    b.a.bind_text_label(marks_clean);
    b.set(local(CTX), q(OBS + offset_of!(F, context)));
    b.pointer(local(CTX), ret[44]);
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        ret[44],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        ret[44],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        ret[44],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        ret[44],
    );
    for base in [
        OBS,
        PUB,
        MARK + offset_of!(M, jobs),
        MARK + offset_of!(M, jobs) + size_of::<J>(),
    ] {
        // cleanup (24 bytes) and ResolveRequest (56 bytes) are contiguous.
        for off in (offset_of!(F, cleanup)..size_of::<A>()).step_by(8) {
            b.branch(q(base + off), n(0), C::NotEqual, ret[44]);
        }
    }
    let reset_done = b.a.create_text_label();
    b.branch(mark(offset_of!(M, state)), n(3), C::NotEqual, reset_done);
    call(&mut b, marks, local(G), ptr(MARK), n(9));
    b.branch(local(STATE), n(0), C::NotEqual, ret[44]);
    b.a.bind_text_label(reset_done);
    for off in (0..size_of::<A>()).step_by(8) {
        b.set(q(PUB + off), n(0));
    }
    for off in [
        offset_of!(T, cancel),
        offset_of!(T, last),
        offset_of!(T, retry_event),
    ] {
        b.set(q(off), n(0));
    }
    let fresh = b.a.create_text_label();
    b.branch(q(OBS + offset_of!(O, stage)), n(3), C::Below, fresh);
    b.branch(q(OBS + offset_of!(O, result)), n(0), C::NotEqual, fresh);
    b.set(q(OBS + offset_of!(O, stage)), n(4));
    b.set(stage(), n(8));
    b.a.jmp_label(ret[37]);
    b.a.bind_text_label(fresh);
    for off in [
        offset_of!(O, output) + offset_of!(RootRequest, value),
        offset_of!(O, stage),
        offset_of!(O, result),
    ] {
        b.set(q(OBS + off), n(0));
    }
    b.set(stage(), n(0));
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(request_cancel);
    b.set(cancel(), n(1));
    b.a.jmp_label(next_return);
    b.a.bind_text_label(resume);
    b.branch(stage(), n(8), C::NotEqual, ret[44]);
    b.branch(cancel(), n(0), C::NotEqual, ret[44]);
    b.set(stage(), n(1));
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(cancel_heap);
    let picked = b.a.create_text_label();
    let release = b.a.create_text_label();
    let cancel_marks = b.a.create_text_label();
    b.set(local(REG), ptr(PUB));
    let pick_receiver = b.a.create_text_label();
    b.branch(stage(), n(13), C::AboveOrEqual, pick_receiver);
    b.branch(stage(), n(0), C::NotEqual, picked);
    b.a.bind_text_label(pick_receiver);
    b.set(local(REG), ptr(OBS));
    b.a.bind_text_label(picked);
    lease(&mut b, REG, release);
    b.a.jmp_label(cancel_marks);
    b.a.bind_text_label(release);
    call(&mut b, cleanup, local(G), local(REG), n(1));
    b.branch(local(STATE), n(0), C::NotEqual, ret[32]);
    b.a.bind_text_label(cancel_marks);
    let cancel_call = b.a.create_text_label();
    for s in [1, 2] {
        b.branch(mark(offset_of!(M, state)), n(s), C::Equal, cancel_call);
    }
    b.a.jmp_label(cancelled);
    b.a.bind_text_label(cancel_call);
    call(&mut b, marks, local(G), ptr(MARK), n(8));
    b.set(last(), local(STATE));
    b.branch(local(STATE), n(22), C::NotEqual, ret[32]);
    b.a.bind_text_label(cancelled);
    b.branch(q(offset_of!(T, fault)), n(0), C::NotEqual, failed);
    b.set(stage(), n(10));
    b.a.jmp_label(next_return);
    // Observation: enter / read / exit / release, followed by clean result handling.
    b.a.bind_text_label(states[0]);
    if root_value {
        b.set(q(OBS + offset_of!(O, stage)), n(4));
        b.set(stage(), n(1));
        b.a.jmp_label(ret[32]);
    }
    b.set(local(NEXT), q(OBS + offset_of!(O, stage)));
    let obs_call = b.a.create_text_label();
    let obs_clean = b.a.create_text_label();
    b.branch(local(NEXT), n(0), C::NotEqual, obs_call);
    b.set(local(REG), ptr(OBS));
    lease(&mut b, REG, obs_clean);
    b.a.jmp_label(obs_call);
    b.a.bind_text_label(obs_clean);
    call(&mut b, cleanup, local(G), ptr(OBS), n(1));
    b.branch(local(STATE), n(0), C::NotEqual, ret[32]);
    b.a.bind_text_label(obs_call);
    let obs_invoke = b.a.create_text_label();
    b.set(local(USED), local(NEXT));
    b.branch(local(NEXT), n(3), C::Below, obs_invoke);
    b.set(local(USED), n(3));
    b.a.bind_text_label(obs_invoke);
    call(&mut b, observe, local(G), ptr(OBS), local(USED));
    b.set(last(), local(STATE));
    let obs_complete = b.a.create_text_label();
    let null_receiver = b.a.create_text_label();
    b.branch(local(STATE), n(11), C::Equal, obs_complete);
    b.branch(local(NEXT), n(0), C::NotEqual, next_return);
    b.branch(local(STATE), n(0), C::Equal, null_receiver);
    retry(&mut b, &ret);
    b.branch(local(STATE), n(7), C::Equal, ret[32]);
    b.branch(local(STATE), n(8), C::NotEqual, failed);
    b.a.jmp_label(next_return);
    b.a.bind_text_label(null_receiver);
    b.set(stage(), n(12));
    b.a.jmp_label(ret[43]);
    b.a.bind_text_label(obs_complete);
    let observed = b.a.create_text_label();
    b.branch(q(OBS + offset_of!(O, result)), n(0), C::Equal, observed);
    b.branch(q(OBS + offset_of!(O, result)), n(3), C::NotEqual, failed);
    call(&mut b, observe, local(G), ptr(OBS), n(4));
    b.set(last(), local(STATE));
    b.branch(local(STATE), n(0), C::Equal, ret[39]);
    b.a.jmp_label(failed);
    b.a.bind_text_label(observed);
    b.set(stage(), n(1));
    b.a.jmp_label(ret[32]);
    // Mark-set admission and one child action, under the action's lock domain.
    b.a.bind_text_label(states[1]);
    b.set(
        mark(offset_of!(M, context)),
        q(OBS + offset_of!(F, context)),
    );
    for i in (0..size_of::<RootRequest>()).step_by(8) {
        b.set(
            mark(offset_of!(M, root) + i),
            q(OBS + offset_of!(O, output) + i),
        );
    }
    b.set(
        mark(offset_of!(M, root) + offset_of!(RootRequest, value)),
        n(0),
    );
    call(&mut b, marks, local(G), ptr(MARK), n(7));
    b.set(last(), local(STATE));
    let marking = b.a.create_text_label();
    let root_enter = b.a.create_text_label();
    let recap = b.a.create_text_label();
    for (s, l) in [(18, marking), (17, root_enter), (19, recap)] {
        b.branch(local(STATE), n(s), C::Equal, l);
    }
    b.a.jmp_label(failed);
    b.a.bind_text_label(marking);
    b.set(stage(), n(2));
    b.a.jmp_label(next_return);
    b.a.bind_text_label(recap);
    b.set(stage(), n(8));
    b.a.jmp_label(next_return);
    b.a.bind_text_label(states[2]);
    job(&mut b);
    let mark_call = b.a.create_text_label();
    b.branch(
        field(REG, offset_of!(J, stage), 0),
        n(0),
        C::NotEqual,
        mark_call,
    );
    lease(&mut b, REG, restart);
    b.a.bind_text_label(mark_call);
    b.set(local(USED), field(REG, offset_of!(J, stage), 0));
    let mark_invoke = b.a.create_text_label();
    b.branch(local(USED), n(4), C::Below, mark_invoke);
    b.set(local(USED), n(4));
    b.a.bind_text_label(mark_invoke);
    call(&mut b, marks, local(G), ptr(MARK), local(USED));
    b.set(last(), local(STATE));
    b.branch(local(STATE), n(17), C::Equal, root_enter);
    for r in [19, 7] {
        b.branch(local(STATE), n(r), C::Equal, restart);
    }
    b.branch(local(STATE), n(20), C::Equal, ret[38]);
    let full = b.a.create_text_label();
    let exhausted = b.a.create_text_label();
    let fault = b.a.create_text_label();
    b.branch(local(STATE), n(15), C::Equal, full);
    b.branch(local(STATE), n(16), C::Equal, exhausted);
    for r in [21, 1] {
        b.branch(local(STATE), n(r), C::Equal, fault);
    }
    retry(&mut b, &ret);
    b.a.jmp_label(next_return);
    b.a.bind_text_label(full);
    b.set(q(offset_of!(T, retry_event)), n(38));
    b.a.jmp_label(restart);
    b.a.bind_text_label(exhausted);
    b.set(q(offset_of!(T, retry_event)), n(41));
    b.a.jmp_label(restart);
    b.a.bind_text_label(fault);
    b.set(q(offset_of!(T, fault)), n(1));
    b.set(cancel(), n(1));
    b.a.jmp_label(next_return);
    b.a.bind_text_label(root_enter);
    b.set(stage(), n(3));
    b.a.jmp_label(next_return);
    // Cancel unoffered work and reset the receipt before exposing RESTART.
    b.a.bind_text_label(states[7]);
    let restart_cancel = b.a.create_text_label();
    let restart_reset = b.a.create_text_label();
    let recaptured = b.a.create_text_label();
    for s in [1, 2] {
        b.branch(mark(offset_of!(M, state)), n(s), C::Equal, restart_cancel);
    }
    b.a.jmp_label(restart_reset);
    b.a.bind_text_label(restart_cancel);
    call(&mut b, marks, local(G), ptr(MARK), n(8));
    b.set(last(), local(STATE));
    b.branch(local(STATE), n(22), C::NotEqual, ret[32]);
    b.a.bind_text_label(restart_reset);
    b.branch(mark(offset_of!(M, state)), n(3), C::NotEqual, recaptured);
    call(&mut b, marks, local(G), ptr(MARK), n(9));
    b.set(last(), local(STATE));
    b.branch(local(STATE), n(0), C::NotEqual, ret[32]);
    b.a.bind_text_label(recaptured);
    b.set(stage(), n(8));
    b.set(local(STATE), q(offset_of!(T, retry_event)));
    b.set(q(offset_of!(T, retry_event)), n(0));
    b.branch(local(STATE), n(0), C::Equal, ret[37]);
    b.value(local(STATE));
    b.a.jmp_label(done);
    // Canonical result guard, root publication, EXIT, then exact lease release.
    b.a.bind_text_label(states[3]);
    let pub_clean = b.a.create_text_label();
    let pub_enter = b.a.create_text_label();
    b.set(local(REG), ptr(PUB));
    lease(&mut b, REG, pub_clean);
    b.a.jmp_label(pub_enter);
    b.a.bind_text_label(pub_clean);
    call(&mut b, cleanup, local(G), ptr(PUB), n(1));
    b.branch(local(STATE), n(0), C::NotEqual, ret[32]);
    b.a.bind_text_label(pub_enter);
    b.set(local(CTX), mark(offset_of!(M, context)));
    b.branch(
        field(CTX, offset_of!(MutatorContext, adopted_phase), 8),
        mark(offset_of!(M, phase)),
        C::NotEqual,
        restart,
    );
    b.set(
        q(PUB + offset_of!(F, context)),
        mark(offset_of!(M, context)),
    );
    for i in (0..size_of::<RootRequest>()).step_by(8) {
        b.set(
            q(PUB + offset_of!(F, root) + i),
            mark(offset_of!(M, root) + i),
        );
    }
    call(&mut b, enter, local(G), ptr(PUB), n(0));
    b.set(last(), local(STATE));
    let ready = b.a.create_text_label();
    let complete = b.a.create_text_label();
    b.branch(local(STATE), n(8), C::Equal, ready);
    b.branch(local(STATE), n(0), C::Equal, complete);
    b.branch(local(STATE), n(7), C::Equal, next_return);
    retry(&mut b, &ret);
    b.a.jmp_label(failed);
    b.a.bind_text_label(ready);
    b.set(stage(), n(4));
    b.a.jmp_label(next_return);
    b.a.bind_text_label(states[4]);
    let skip_publish = b.a.create_text_label();
    b.branch(cancel(), n(0), C::NotEqual, skip_publish);
    call(&mut b, publish, ptr(MARK), ptr(PUB), n(0));
    b.set(last(), local(STATE));
    b.a.bind_text_label(skip_publish);
    b.set(stage(), n(5));
    b.a.jmp_label(ret[33]);
    b.a.bind_text_label(states[5]);
    call(&mut b, cleanup, n(0), ptr(PUB), n(0));
    b.branch(local(STATE), n(0), C::NotEqual, ret[33]);
    b.set(stage(), n(6));
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(states[6]);
    call(&mut b, cleanup, local(G), ptr(PUB), n(1));
    b.branch(local(STATE), n(0), C::NotEqual, ret[32]);
    b.set(local(CTX), mark(offset_of!(M, context)));
    b.branch(
        field(CTX, offset_of!(MutatorContext, adopted_phase), 8),
        mark(offset_of!(M, phase)),
        C::NotEqual,
        restart,
    );
    b.branch(last(), n(0), C::Equal, heal_enter);
    b.branch(last(), n(2), C::Equal, root_enter);
    b.a.jmp_label(failed);
    b.a.bind_text_label(heal_enter);
    b.set(stage(), n(if root_value { 9 } else { 13 }));
    b.a.jmp_label(if root_value { ret[35] } else { ret[32] });
    b.a.bind_text_label(heal_states[0]);
    let clean_receiver = b.a.create_text_label();
    let enter_receiver = b.a.create_text_label();
    b.set(local(REG), ptr(OBS));
    lease(&mut b, REG, clean_receiver);
    b.a.jmp_label(enter_receiver);
    b.a.bind_text_label(clean_receiver);
    call(&mut b, cleanup, local(G), ptr(OBS), n(1));
    b.branch(local(STATE), n(0), C::NotEqual, ret[32]);
    b.a.bind_text_label(enter_receiver);
    b.set(local(CTX), mark(offset_of!(M, context)));
    b.branch(
        field(CTX, offset_of!(MutatorContext, adopted_phase), 8),
        mark(offset_of!(M, phase)),
        C::NotEqual,
        restart,
    );
    call(&mut b, enter, local(G), ptr(OBS), n(0));
    b.set(last(), local(STATE));
    let receiver_ready = b.a.create_text_label();
    b.branch(local(STATE), n(8), C::Equal, receiver_ready);
    b.branch(local(STATE), n(7), C::Equal, ret[32]);
    retry(&mut b, &ret);
    b.a.jmp_label(failed);
    b.a.bind_text_label(receiver_ready);
    b.set(stage(), n(14));
    b.a.jmp_label(ret[33]);
    b.a.bind_text_label(heal_states[1]);
    let skip_heal = b.a.create_text_label();
    b.branch(cancel(), n(0), C::NotEqual, skip_heal);
    call(&mut b, heal, local(Q), n(0), n(0));
    b.set(last(), local(STATE));
    b.a.bind_text_label(skip_heal);
    b.set(stage(), n(15));
    b.a.jmp_label(ret[33]);
    b.a.bind_text_label(heal_states[2]);
    call(&mut b, cleanup, n(0), ptr(OBS), n(0));
    b.branch(local(STATE), n(0), C::NotEqual, ret[33]);
    b.set(stage(), n(16));
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(heal_states[3]);
    call(&mut b, cleanup, local(G), ptr(OBS), n(1));
    b.branch(local(STATE), n(0), C::NotEqual, ret[32]);
    b.set(local(CTX), mark(offset_of!(M, context)));
    b.branch(
        field(CTX, offset_of!(MutatorContext, adopted_phase), 8),
        mark(offset_of!(M, phase)),
        C::NotEqual,
        restart,
    );
    for r in [0, 2] {
        b.branch(last(), n(r), C::Equal, complete);
    }
    b.branch(last(), n(3), C::Equal, root_enter);
    b.a.jmp_label(failed);
    b.a.bind_text_label(complete);
    b.set(stage(), n(9));
    b.a.jmp_label(next_return);
    b.a.bind_text_label(restart);
    b.set(stage(), n(7));
    b.a.jmp_label(ret[32]);
    b.a.bind_text_label(failed);
    b.set(stage(), n(11));
    b.a.jmp_label(next_return);
    b.a.bind_text_label(next_return);
    call(&mut b, next, n(0), local(Q), n(0));
    b.value(local(STATE));
    b.a.jmp_label(done);
    for (i, l) in ret.into_iter().enumerate() {
        b.a.bind_text_label(l);
        b.a.mov_imm64(R::V0, i as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.store_rbp_slot(STATE, R::V0);
    let release_admission = b.a.create_text_label();
    let finished = b.a.create_text_label();
    for status in [35, 36, 42, 43] {
        b.branch(local(STATE), n(status), C::Equal, release_admission);
    }
    b.a.jmp_label(finished);
    b.a.bind_text_label(release_admission);
    b.branch(q(offset_of!(T, admitted)), n(0), C::Equal, finished);
    b.set(local(CTX), q(OBS + offset_of!(F, context)));
    let producers = field(CTX, offset_of!(MutatorContext, private_producers), 8);
    b.set(local(USED), producers.clone());
    let consistent = b.a.create_text_label();
    b.branch(local(USED), n(0), C::NotEqual, consistent);
    b.set(local(STATE), n(44));
    b.a.jmp_label(finished);
    b.a.bind_text_label(consistent);
    b.set(producers, sub(local(USED), n(1)));
    b.set(q(offset_of!(T, admitted)), n(0));
    b.a.bind_text_label(finished);
    b.value(local(STATE));
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(next);
    emit_next(b.a);
    b.a.bind_text_label(observe);
    super::rooted_observation::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(marks);
    super::rooted_mark_set::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(enter);
    super::root_access::emit_root_access(b.a);
    b.a.ret();
    b.a.bind_text_label(publish);
    super::rooted_mark_publish::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(heal);
    super::rooted_load_heal::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(cleanup);
    super::root_access::emit_cleanup(b.a);
    b.a.ret();
}
