//! Common-registry-lock snapshot admission; gc_rooted_load_admit.h.
use super::*;
use crate::gc_abi::{
    HandshakeState as H, MarkQueue, RootAccess, RootFollow, RootRequest, RootedLoad as T,
    RootedLoadAdmission as A, RootedMark, RootedMarkSet as M, RootedObservation as O,
    ThreadRegistration as P,
};
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let ret: Vec<_> = (0..4).map(|_| a.create_text_label()).collect();
    let done = a.create_text_label();
    let initial = a.create_text_label();
    let clean = a.create_text_label();
    let active = a.create_text_label();
    let phase = a.create_text_label();
    let already = a.create_text_label();
    let commit = a.create_text_label();
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
    macro_rules! obs {
        ($f:ident) => {
            field(Q, offset_of!(T, observation) + offset_of!(O, $f), 0)
        };
    }
    macro_rules! h {
        ($f:ident) => {
            field(SEG, offset_of!(H, $f), 0)
        };
    }
    macro_rules! p {
        ($f:ident) => {
            field(LEASE, offset_of!(P, $f), 0)
        };
    }
    macro_rules! ctx {
        ($f:ident,$w:expr) => {
            field(CTX, offset_of!(MutatorContext, $f), $w)
        };
    }
    b.pointer(local(G), ret[2]);
    b.pointer(local(Q), ret[2]);
    for value in [
        q!(cancel),
        q!(fault),
        field(Q, offset_of!(T, marks) + offset_of!(M, state), 0),
    ] {
        b.branch(value, n(0), C::NotEqual, ret[2]);
    }
    b.branch(q!(admitted), n(1), C::Above, ret[2]);
    b.branch(q!(stage), n(0), C::Equal, initial);
    b.branch(q!(stage), n(8), C::NotEqual, ret[2]);
    b.branch(obs!(stage), n(4), C::NotEqual, ret[2]);
    b.branch(obs!(result), n(0), C::NotEqual, ret[2]);
    b.a.jmp_label(clean);
    b.a.bind_text_label(initial);
    b.branch(obs!(stage), n(0), C::NotEqual, ret[2]);
    b.branch(
        field(
            Q,
            offset_of!(T, observation) + offset_of!(O, output) + offset_of!(RootRequest, value),
            0,
        ),
        n(0),
        C::NotEqual,
        ret[2],
    );
    b.a.bind_text_label(clean);
    b.branch(
        field(
            Q,
            offset_of!(RootFollow, root) + offset_of!(RootRequest, value),
            0,
        ),
        n(0),
        C::NotEqual,
        ret[2],
    );
    for base in [
        offset_of!(T, observation),
        offset_of!(T, publication),
        offset_of!(T, marks) + offset_of!(M, jobs),
        offset_of!(T, marks) + offset_of!(M, jobs) + size_of::<RootedMark>(),
    ] {
        for off in (offset_of!(RootFollow, cleanup)..size_of::<RootAccess>()).step_by(8) {
            b.branch(field(Q, base + off, 0), n(0), C::NotEqual, ret[2]);
        }
    }
    for (slot, value) in [
        (CTX, field(Q, offset_of!(RootFollow, context), 0)),
        (REG, field(G, offset_of!(A, registry), 0)),
        (SEG, field(G, offset_of!(A, handshake), 0)),
        (LEASE, field(G, offset_of!(A, participant), 0)),
    ] {
        b.set(local(slot), value);
        b.pointer(local(slot), ret[2]);
    }
    b.branch(
        ctx!(abi_version, 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        ret[2],
    );
    b.branch(ctx!(thread_id, 0), n(0), C::Equal, ret[2]);
    b.branch(ctx!(state, 4), n(0), C::NotEqual, ret[2]);
    b.branch(ctx!(access_epoch, 8), n(0), C::NotEqual, ret[2]);
    b.branch(p!(context), local(CTX), C::NotEqual, ret[2]);
    b.branch(p!(registry), local(SEG), C::NotEqual, ret[2]);
    b.branch(p!(registered), n(1), C::NotEqual, ret[2]);
    b.branch(p!(identity), ctx!(thread_id, 0), C::NotEqual, ret[2]);
    for value in [h!(members), h!(head)] {
        b.branch(value, n(0), C::Equal, ret[2]);
    }
    b.branch(h!(native), h!(members), C::Above, ret[2]);
    b.branch(h!(active), n(1), C::Above, ret[2]);
    b.branch(p!(identity), h!(last_identity), C::Above, ret[2]);
    b.branch(
        p!(acknowledged_generation),
        h!(generation),
        C::Above,
        ret[2],
    );
    b.branch(h!(active), n(0), C::NotEqual, active);
    for value in [h!(expected), h!(acknowledged)] {
        b.branch(value, n(0), C::NotEqual, ret[2]);
    }
    b.a.jmp_label(phase);
    b.a.bind_text_label(active);
    b.branch(h!(generation), n(0), C::Equal, ret[2]);
    b.branch(
        h!(expected),
        sub(h!(members), h!(native)),
        C::NotEqual,
        ret[2],
    );
    b.branch(h!(acknowledged), h!(expected), C::Above, ret[2]);
    b.a.bind_text_label(phase);
    let snapshot = field(REG, offset_of!(RootRegistry, phase), 0);
    super::remembered::phase(&mut b, snapshot.clone(), ret[2]);
    b.branch(h!(active), n(0), C::NotEqual, ret[1]);
    b.branch(
        p!(acknowledged_generation),
        h!(generation),
        C::NotEqual,
        ret[1],
    );
    b.branch(
        ctx!(adopted_phase, 8),
        snapshot.clone(),
        C::NotEqual,
        ret[1],
    );
    for (generation, (queue, epoch)) in [(OP, USED), (NEXT, STATE)].into_iter().enumerate() {
        let skip = b.a.create_text_label();
        b.set(local(queue), n(0));
        b.set(local(epoch), n(0));
        b.set(
            local(PREV),
            and(shr(snapshot.clone(), generation as u64 * 4), n(15)),
        );
        b.branch(local(PREV), n(1), C::Below, skip);
        b.branch(local(PREV), n(3), C::Above, skip);
        b.set(
            local(KIND),
            field(G, offset_of!(A, queues) + generation * 8, 0),
        );
        b.pointer(local(KIND), ret[2]);
        b.branch(
            field(KIND, offset_of!(MarkQueue, generation), 0),
            n(generation as u64),
            C::NotEqual,
            ret[2],
        );
        b.branch(
            field(KIND, offset_of!(MarkQueue, epoch), 0),
            n(0),
            C::Equal,
            ret[2],
        );
        b.set(local(queue), local(KIND));
        b.set(local(epoch), field(KIND, offset_of!(MarkQueue, epoch), 0));
        b.a.bind_text_label(skip);
    }
    b.set(local(PREV), ctx!(private_producers, 8));
    b.branch(q!(admitted), n(0), C::NotEqual, already);
    b.branch(local(PREV), n(u64::MAX), C::Equal, ret[3]);
    b.a.jmp_label(commit);
    b.a.bind_text_label(already);
    b.branch(local(PREV), n(0), C::Equal, ret[2]);
    b.a.bind_text_label(commit);
    for (generation, (queue, epoch)) in [(OP, USED), (NEXT, STATE)].into_iter().enumerate() {
        b.set(
            field(
                Q,
                offset_of!(T, marks) + offset_of!(M, queues) + generation * 8,
                0,
            ),
            local(queue),
        );
        b.set(
            field(
                Q,
                offset_of!(T, marks) + offset_of!(M, epochs) + generation * 8,
                0,
            ),
            local(epoch),
        );
    }
    b.set(
        field(Q, offset_of!(T, marks) + offset_of!(M, phase), 0),
        snapshot,
    );
    b.branch(q!(admitted), n(0), C::NotEqual, ret[0]);
    b.set(ctx!(private_producers, 8), add(local(PREV), n(1)));
    b.set(q!(admitted), n(1));
    b.a.jmp_label(ret[0]);
    for (status, label) in ret.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
}
