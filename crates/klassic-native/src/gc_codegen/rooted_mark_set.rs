//! Snapshot-bound generation completion; mirrors gc_rooted_mark_set.h.
use super::*;
use crate::gc_abi::{RootFollow, RootRequest, RootedMark as Job, RootedMarkSet as T};
fn call<B: PortableAsm>(b: &mut Emit<'_, B>, target: B::TextLabel, op: E) {
    b.value(local(G));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(local(REG));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(op);
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(target);
    b.a.store_rbp_slot(STATE, R::V0);
}
fn clear_jobs<B: PortableAsm>(b: &mut Emit<'_, B>) {
    for i in (0..size_of::<[Job; 2]>()).step_by(8) {
        b.set(field(Q, offset_of!(T, jobs) + i, 0), n(0));
    }
}
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let ret: Vec<_> = (0..23).map(|_| a.create_text_label()).collect();
    let done = a.create_text_label();
    let dynamic = a.create_text_label();
    let init = a.create_text_label();
    let reset = a.create_text_label();
    let terminal = a.create_text_label();
    let cancel = a.create_text_label();
    let cancelled = a.create_text_label();
    let job_selected = a.create_text_label();
    let invoke = a.create_text_label();
    let wait = a.create_text_label();
    let eligible = a.create_text_label();
    let offer_good = a.create_text_label();
    let complete = a.create_text_label();
    let required = a.create_text_label();
    let child = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(T, $f), 0)
        };
    }
    macro_rules! j {
        ($f:ident) => {
            field(REG, offset_of!(Job, $f), 0)
        };
    }
    let adopted = field(CTX, offset_of!(MutatorContext, adopted_phase), 8);
    b.pointer(local(Q), ret[21]);
    b.branch(local(OP), n(9), C::Above, ret[21]);
    for op in [5, 6] {
        b.branch(local(OP), n(op), C::Equal, ret[21]);
    }
    b.branch(q!(state), n(3), C::Above, ret[21]);
    b.branch(local(OP), n(9), C::Equal, reset);
    b.pointer(q!(context), ret[21]);
    b.set(local(CTX), q!(context));
    b.branch(local(OP), n(7), C::Equal, init);
    b.branch(q!(state), n(2), C::Equal, terminal);
    b.branch(q!(state), n(3), C::Equal, ret[22]);
    b.branch(q!(state), n(1), C::NotEqual, ret[21]);
    b.branch(q!(current), n(1), C::Above, ret[21]);
    b.set(local(REG), add(local(Q), n(offset_of!(T, jobs) as u64)));
    b.branch(q!(current), n(0), C::Equal, job_selected);
    b.set(local(REG), add(local(REG), n(size_of::<Job>() as u64)));
    b.a.bind_text_label(job_selected);
    b.branch(local(OP), n(8), C::Equal, cancel);
    for stage in [1, 2] {
        b.branch(j!(stage), n(stage), C::Equal, invoke);
    }
    for op in [2, 4] {
        b.branch(local(OP), n(op), C::Equal, invoke);
    }
    b.branch(adopted.clone(), q!(phase), C::NotEqual, ret[19]);
    b.a.bind_text_label(invoke);
    call(&mut b, child, local(OP));
    b.branch(local(STATE), n(14), C::Equal, ret[19]);
    b.branch(local(STATE), n(12), C::NotEqual, dynamic);
    b.branch(j!(route_result), n(3), C::Equal, wait);
    b.branch(j!(route_result), n(2), C::Equal, eligible);
    b.branch(j!(route_result), n(0), C::NotEqual, ret[21]);
    b.branch(j!(offer_result), n(0), C::Equal, offer_good);
    b.branch(j!(offer_result), n(2), C::NotEqual, ret[21]);
    b.a.bind_text_label(offer_good);
    b.a.bind_text_label(eligible);
    // current is 0/1; 1+current is the corresponding bit. Keep OR idempotent.
    b.set(local(USED), add(n(1), q!(current)));
    b.set(
        q!(completed),
        add(
            q!(completed),
            sub(local(USED), and(q!(completed), local(USED))),
        ),
    );
    b.branch(adopted.clone(), q!(phase), C::NotEqual, ret[19]);
    b.branch(q!(completed), q!(required), C::Equal, complete);
    b.set(
        q!(current),
        shr(
            and(and(q!(required), sub(n(3), and(q!(completed), n(3)))), n(2)),
            1,
        ),
    );
    b.a.jmp_label(ret[18]);
    b.a.bind_text_label(complete);
    b.set(q!(state), n(2));
    b.a.jmp_label(ret[17]);
    b.a.bind_text_label(wait);
    call(&mut b, child, n(6));
    b.a.jmp_label(ret[20]);
    b.a.bind_text_label(cancel);
    call(&mut b, child, n(5));
    b.branch(local(STATE), n(12), C::Equal, cancelled);
    b.branch(local(STATE), n(13), C::NotEqual, dynamic);
    b.a.bind_text_label(cancelled);
    b.set(q!(state), n(3));
    b.a.jmp_label(ret[22]);
    b.a.bind_text_label(terminal);
    b.branch(local(OP), n(8), C::Equal, cancelled);
    b.branch(adopted.clone(), q!(phase), C::NotEqual, ret[19]);
    b.a.jmp_label(ret[17]);
    b.a.bind_text_label(reset);
    b.branch(q!(state), n(2), C::Below, ret[21]);
    clear_jobs(&mut b);
    for v in [q!(state), q!(required), q!(completed), q!(current)] {
        b.set(v, n(0));
    }
    b.a.jmp_label(ret[0]);
    b.a.bind_text_label(init);
    b.branch(q!(state), n(0), C::NotEqual, ret[21]);
    b.branch(
        field(Q, offset_of!(T, root) + offset_of!(RootRequest, value), 0),
        n(0),
        C::NotEqual,
        ret[21],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        ret[21],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        ret[21],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        ret[21],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        ret[21],
    );
    super::remembered::phase(&mut b, q!(phase), ret[21]);
    b.branch(adopted, q!(phase), C::NotEqual, ret[19]);
    b.set(local(NEXT), n(0));
    for generation in 0..2 {
        let skip = b.a.create_text_label();
        b.set(local(USED), and(shr(q!(phase), generation * 4), n(15)));
        b.branch(local(USED), n(1), C::Below, skip);
        b.branch(local(USED), n(3), C::Above, skip);
        b.pointer(
            field(Q, offset_of!(T, queues) + generation as usize * 8, 0),
            ret[21],
        );
        b.branch(
            field(Q, offset_of!(T, epochs) + generation as usize * 8, 0),
            n(0),
            C::Equal,
            ret[21],
        );
        b.set(local(NEXT), add(local(NEXT), n(1 << generation)));
        b.a.bind_text_label(skip);
    }
    clear_jobs(&mut b);
    for generation in 0..2 {
        b.set(
            local(REG),
            add(
                local(Q),
                n((offset_of!(T, jobs) + generation * size_of::<Job>()) as u64),
            ),
        );
        b.set(field(REG, offset_of!(RootFollow, context), 0), q!(context));
        for i in (0..size_of::<RootRequest>()).step_by(8) {
            b.set(
                field(REG, offset_of!(RootFollow, root) + i, 0),
                field(Q, offset_of!(T, root) + i, 0),
            );
        }
        b.set(
            j!(queue),
            field(Q, offset_of!(T, queues) + generation * 8, 0),
        );
        b.set(j!(generation), n(generation as u64));
        b.set(
            j!(epoch),
            field(Q, offset_of!(T, epochs) + generation * 8, 0),
        );
        b.set(j!(phase), q!(phase));
    }
    b.set(q!(required), local(NEXT));
    b.set(q!(completed), n(0));
    b.set(q!(current), shr(and(local(NEXT), n(2)), 1));
    b.branch(local(NEXT), n(0), C::NotEqual, required);
    b.set(q!(state), n(2));
    b.a.jmp_label(ret[17]);
    b.a.bind_text_label(required);
    b.set(q!(state), n(1));
    b.a.jmp_label(ret[18]);
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
    b.a.bind_text_label(child);
    super::rooted_mark::emit(b.a, true);
    b.a.ret();
}
