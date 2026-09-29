//! Counted pending mark ownership; see gc_pending_mark.h.
use super::*;
use crate::gc_abi::{
    GuardedMarkPrepare as P, GuardedMarkRoute as T, MarkQueue, MetadataLeaseRequest as L,
    PendingMark as Qry, ReferenceLease, ResolveRequest,
};
fn call<B: PortableAsm>(b: &mut Emit<'_, B>, label: B::TextLabel, first: E, second: E, op: u64) {
    b.value(first);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(second);
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
}
fn clear_route<B: PortableAsm>(b: &mut Emit<'_, B>) {
    for i in (0..size_of::<T>()).step_by(8) {
        b.set(field(REG, i, 0), n(0));
    }
}
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let ret: Vec<_> = (0..17).map(|_| a.create_text_label()).collect();
    let done = a.create_text_label();
    let dynamic = a.create_text_label();
    let enter = a.create_text_label();
    let route = a.create_text_label();
    let exit = a.create_text_label();
    let offer = a.create_text_label();
    let release = a.create_text_label();
    let reset = a.create_text_label();
    let phase_selected = a.create_text_label();
    let acquired = a.create_text_label();
    let null = a.create_text_label();
    let routed = a.create_text_label();
    let accepted = a.create_text_label();
    let duplicate = a.create_text_label();
    let release_check = a.create_text_label();
    let release_call = a.create_text_label();
    let release_clear = a.create_text_label();
    let cancelled = a.create_text_label();
    let acquire_fn = a.create_text_label();
    let route_fn = a.create_text_label();
    let cleanup_fn = a.create_text_label();
    let offer_fn = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(Qry, $f), 0)
        };
    }
    macro_rules! p {
        ($f:ident) => {
            field(REG, offset_of!(T, prepare) + offset_of!(P, $f), 0)
        };
    }
    macro_rules! lease {
        ($f:ident) => {
            field(
                Q,
                (offset_of!(Qry, current) + offset_of!(ReferenceLease, lease)) + offset_of!(L, $f),
                0,
            )
        };
    }
    b.pointer(local(Q), ret[1]);
    b.branch(local(OP), n(6), C::Above, ret[1]);
    b.branch(q!(stage), n(6), C::Above, ret[1]);
    b.set(local(REG), add(local(Q), n(offset_of!(Qry, route) as u64)));
    b.branch(local(OP), n(0), C::Equal, enter);
    b.branch(local(OP), n(1), C::Equal, route);
    b.branch(local(OP), n(2), C::Equal, exit);
    b.branch(q!(stage), n(1), C::Equal, ret[8]);
    b.branch(q!(stage), n(2), C::Equal, ret[11]);
    b.branch(local(OP), n(3), C::Equal, offer);
    b.branch(local(OP), n(6), C::Equal, reset);
    b.a.jmp_label(release);
    b.a.bind_text_label(enter);
    for (stage, result) in [(1, 8), (2, 11), (3, 9), (4, 10), (5, 12), (6, 13)] {
        b.branch(q!(stage), n(stage), C::Equal, ret[result]);
    }
    b.pointer(q!(queue), ret[1]);
    b.branch(q!(generation), n(1), C::Above, ret[1]);
    b.branch(q!(epoch), n(0), C::Equal, ret[1]);
    super::remembered::phase(&mut b, q!(phase), ret[1]);
    b.set(local(STATE), and(q!(phase), n(15)));
    b.branch(q!(generation), n(0), C::Equal, phase_selected);
    b.set(local(STATE), and(shr(q!(phase), 4), n(15)));
    b.a.bind_text_label(phase_selected);
    b.branch(local(STATE), n(1), C::Below, ret[1]);
    b.branch(local(STATE), n(3), C::Above, ret[1]);
    call(&mut b, acquire_fn, local(G), local(Q), 0);
    b.branch(local(STATE), n(8), C::Equal, acquired);
    b.branch(local(STATE), n(0), C::Equal, null);
    b.a.jmp_label(dynamic);
    b.a.bind_text_label(acquired);
    b.set(q!(stage), n(1));
    b.a.jmp_label(ret[8]);
    b.a.bind_text_label(null);
    b.set(q!(route_result), n(2));
    b.set(q!(stage), n(5));
    b.a.jmp_label(ret[12]);
    b.a.bind_text_label(route);
    b.branch(q!(stage), n(2), C::Equal, ret[11]);
    b.branch(q!(stage), n(1), C::NotEqual, ret[1]);
    clear_route(&mut b);
    b.set(field(REG, offset_of!(T, generation), 0), q!(generation));
    b.set(
        p!(scope),
        add(
            local(Q),
            n((offset_of!(Qry, access) + offset_of!(ResolveRequest, scope)) as u64),
        ),
    );
    b.set(p!(page), lease!(page));
    b.set(p!(phase), q!(phase));
    b.set(
        p!(reference),
        field(
            Q,
            offset_of!(Qry, current) + offset_of!(ReferenceLease, reference),
            0,
        ),
    );
    for i in (0..size_of::<L>()).step_by(8) {
        b.set(
            field(REG, offset_of!(T, prepare) + offset_of!(P, lease) + i, 0),
            field(
                Q,
                (offset_of!(Qry, current) + offset_of!(ReferenceLease, lease)) + i,
                0,
            ),
        );
    }
    call(&mut b, route_fn, local(REG), n(0), 0);
    b.set(q!(route_result), local(STATE));
    b.set(q!(stage), n(2));
    b.a.jmp_label(ret[11]);
    b.a.bind_text_label(exit);
    b.branch(q!(stage), n(2), C::NotEqual, ret[1]);
    call(&mut b, cleanup_fn, n(0), local(Q), 0);
    b.branch(local(STATE), n(0), C::NotEqual, ret[11]);
    b.branch(q!(route_result), n(0), C::Equal, routed);
    b.set(q!(stage), n(4));
    b.a.jmp_label(ret[10]);
    b.a.bind_text_label(routed);
    b.set(q!(stage), n(3));
    b.a.jmp_label(ret[9]);
    b.a.bind_text_label(offer);
    b.branch(q!(stage), n(3), C::NotEqual, ret[1]);
    b.set(local(SEG), q!(queue));
    b.branch(
        field(SEG, offset_of!(MarkQueue, generation), 0),
        q!(generation),
        C::NotEqual,
        ret[14],
    );
    b.branch(
        field(SEG, offset_of!(MarkQueue, epoch), 0),
        q!(epoch),
        C::NotEqual,
        ret[14],
    );
    call(
        &mut b,
        offer_fn,
        local(SEG),
        add(
            local(REG),
            n((offset_of!(T, prepare) + offset_of!(P, output)) as u64),
        ),
        0,
    );
    b.set(q!(offer_result), local(STATE));
    b.branch(local(STATE), n(0), C::Equal, accepted);
    b.branch(local(STATE), n(2), C::Equal, duplicate);
    b.branch(local(STATE), n(3), C::Equal, ret[15]);
    b.branch(local(STATE), n(6), C::Equal, ret[16]);
    b.a.jmp_label(ret[1]);
    b.a.bind_text_label(accepted);
    for v in [lease!(page), lease!(incarnation), lease!(token)] {
        b.set(v, n(0));
    }
    clear_route(&mut b);
    b.set(q!(stage), n(5));
    b.a.jmp_label(ret[12]);
    b.a.bind_text_label(duplicate);
    b.set(q!(stage), n(4));
    b.a.jmp_label(ret[10]);
    b.a.bind_text_label(release);
    b.branch(local(OP), n(5), C::Equal, release_check);
    b.branch(q!(stage), n(4), C::NotEqual, ret[1]);
    b.a.bind_text_label(release_check);
    b.branch(q!(stage), n(5), C::Equal, ret[12]);
    b.branch(q!(stage), n(6), C::Equal, ret[13]);
    for v in [lease!(page), lease!(incarnation), lease!(token)] {
        b.branch(v, n(0), C::NotEqual, release_call);
    }
    for off in [0, 8, 16] {
        b.branch(
            field(Q, offset_of!(Qry, retired) + off, 0),
            n(0),
            C::NotEqual,
            release_call,
        );
    }
    b.a.jmp_label(release_clear);
    b.a.bind_text_label(release_call);
    call(&mut b, cleanup_fn, local(G), local(Q), 1);
    b.branch(local(STATE), n(0), C::NotEqual, dynamic);
    b.a.bind_text_label(release_clear);
    clear_route(&mut b);
    b.branch(local(OP), n(5), C::Equal, cancelled);
    b.set(q!(stage), n(5));
    b.a.jmp_label(ret[12]);
    b.a.bind_text_label(cancelled);
    b.set(q!(stage), n(6));
    b.a.jmp_label(ret[13]);
    b.a.bind_text_label(reset);
    b.branch(q!(stage), n(5), C::Below, ret[1]);
    b.set(q!(stage), n(0));
    b.set(q!(route_result), n(0));
    b.set(q!(offer_result), n(0));
    b.a.jmp_label(ret[0]);
    b.a.bind_text_label(dynamic);
    b.value(local(STATE));
    b.a.jmp_label(done);
    for (i, label) in ret.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, i as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(acquire_fn);
    emit_enter(b.a);
    b.a.ret();
    b.a.bind_text_label(route_fn);
    super::mark_route::emit_route(b.a);
    b.a.ret();
    b.a.bind_text_label(cleanup_fn);
    emit_cleanup(b.a);
    b.a.ret();
    b.a.bind_text_label(offer_fn);
    super::mark_spill::emit_mark_spill(b.a);
    b.a.ret();
}

fn emit_enter<B: PortableAsm>(a: &mut B) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let follow = a.create_text_label();
    let access = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.set(local(CTX), field(Q, offset_of!(Qry, context), 0));
    b.pointer(local(CTX), invalid);
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    for off in (0..size_of::<ResolveRequest>()).step_by(8) {
        b.branch(
            field(Q, offset_of!(Qry, access) + off, 0),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.a.load_rbp_slot(R::V7, G);
    b.value(add(local(Q), n(offset_of!(Qry, current) as u64)));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(add(local(Q), n(offset_of!(Qry, retired) as u64)));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(follow);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, done);
    b.set(
        field(
            Q,
            offset_of!(Qry, access) + offset_of!(ResolveRequest, scope),
            0,
        ),
        local(CTX),
    );
    b.a.load_rbp_slot(R::V7, G);
    b.value(add(local(Q), n(offset_of!(Qry, current) as u64)));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(add(local(Q), n(offset_of!(Qry, access) as u64)));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(access);
    b.a.cmp_reg_imm8(R::V0, 8);
    b.a.jcc_label(C::Equal, done);
    b.a.store_rbp_slot(STATE, R::V0);
    for off in (0..size_of::<ResolveRequest>()).step_by(8) {
        b.set(field(Q, offset_of!(Qry, access) + off, 0), n(0));
    }
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(follow);
    super::pending_follow::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(access);
    super::pending_access::emit(b.a);
    b.a.ret();
}
fn emit_cleanup<B: PortableAsm>(a: &mut B) {
    use crate::gc_abi::SourceAccessRequest;
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let release = a.create_text_label();
    let failed = a.create_text_label();
    let resolve = a.create_text_label();
    let metadata = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    const CUR: usize = offset_of!(Qry, current) + offset_of!(ReferenceLease, lease);
    const ACC: usize = offset_of!(Qry, access);
    const SCP: usize = ACC + offset_of!(ResolveRequest, scope);
    b.branch(local(OP), n(1), C::Above, invalid);
    b.set(local(CTX), field(Q, offset_of!(Qry, context), 0));
    b.pointer(local(CTX), invalid);
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(local(OP), n(1), C::Equal, release);
    b.pointer(field(Q, CUR, 0), invalid);
    b.branch(
        field(Q, SCP + offset_of!(SourceAccessRequest, context), 0),
        local(CTX),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(Q, SCP + offset_of!(SourceAccessRequest, incarnation), 0),
        field(Q, CUR + 8, 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(Q, SCP + offset_of!(SourceAccessRequest, held), 0),
        n(0),
        C::Equal,
        invalid,
    );
    call(
        &mut b,
        resolve,
        field(Q, CUR, 0),
        add(local(Q), n(ACC as u64)),
        1,
    );
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, done);
    for off in (0..size_of::<ResolveRequest>()).step_by(8) {
        b.set(field(Q, ACC + off, 0), n(0));
    }
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(release);
    b.pointer(local(G), invalid);
    for off in (0..size_of::<ResolveRequest>()).step_by(8) {
        b.branch(field(Q, ACC + off, 0), n(0), C::NotEqual, invalid);
    }
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    for base in [offset_of!(Qry, retired), CUR] {
        let owned = b.a.create_text_label();
        let next = b.a.create_text_label();
        for off in [0, 8, 16] {
            b.branch(field(Q, base + off, 0), n(0), C::NotEqual, owned);
        }
        b.a.jmp_label(next);
        b.a.bind_text_label(owned);
        call(&mut b, metadata, local(G), add(local(Q), n(base as u64)), 1);
        b.branch(local(STATE), n(0), C::NotEqual, failed);
        b.a.bind_text_label(next);
    }
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.jmp_label(done);
    b.a.bind_text_label(failed);
    b.a.mov_imm64(R::V0, 7);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(resolve);
    super::resolve::emit_resolve_access(b.a);
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
}
