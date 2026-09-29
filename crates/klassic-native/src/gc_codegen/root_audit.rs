//! Serial-bound, generation-specific root inventory audit; gc_root_audit.h.
use super::*;
use crate::gc_abi::{BufferConsumer, BufferCursor, MarkQueue, RootAudit, RootWalk};
fn phase_valid<A: PortableAsm>(b: &mut Emit<'_, A>, phase: E, bad: A::TextLabel) {
    b.branch(and(phase.clone(), n(0xe000)), n(0), C::NotEqual, bad);
    b.branch(and(phase.clone(), n(15)), n(7), C::Above, bad);
    b.branch(and(shr(phase, 4), n(15)), n(7), C::Above, bad);
}
fn generation_mask<A: PortableAsm>(b: &mut Emit<'_, A>, generation: E) {
    let ready = b.a.create_text_label();
    b.set(local(KIND), n(1));
    b.branch(generation, n(0), C::Equal, ready);
    b.set(local(KIND), n(2));
    b.a.bind_text_label(ready);
}
#[allow(dead_code)]
pub(crate) fn emit_root_audit<A: PortableAsm>(a: &mut A) {
    emit_mode(a, false);
}
#[allow(dead_code)]
pub(crate) fn emit_root_retire_audit<A: PortableAsm>(a: &mut A) {
    emit_mode(a, true);
}
fn emit_mode<A: PortableAsm>(a: &mut A, retire: bool) {
    let status: [_; 6] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let begin = a.create_text_label();
    let check = a.create_text_label();
    let next_owner = a.create_text_label();
    let invoke = a.create_text_label();
    let commit = a.create_text_label();
    let sample = a.create_text_label();
    let result = a.create_text_label();
    let done = a.create_text_label();
    let core = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(RootAudit, $f), 0)
        };
    }
    macro_rules! w {
        ($f:ident) => {
            field(LEASE, offset_of!(RootWalk, $f), 0)
        };
    }
    macro_rules! wl {
        ($f:ident) => {
            field(
                LEASE,
                offset_of!(RootWalk, lease) + offset_of!(RootLease, $f),
                0,
            )
        };
    }
    macro_rules! g {
        ($f:ident) => {
            field(G, offset_of!(RootRegistry, $f), 0)
        };
    }
    macro_rules! r {
        ($f:ident) => {
            field(REG, offset_of!(RootRegistration, $f), 0)
        };
    }
    macro_rules! queue {
        ($f:ident) => {
            field(PREV, offset_of!(MarkQueue, $f), 0)
        };
    }
    b.branch(local(OP), n(3), C::Above, invalid);
    b.set(local(PREV), q!(queue));
    b.pointer(local(PREV), invalid);
    b.set(
        local(LEASE),
        add(local(Q), n(offset_of!(RootAudit, walk) as u64)),
    );
    b.branch(local(OP), n(2), C::AboveOrEqual, check);
    b.branch(local(OP), n(0), C::Equal, begin);
    b.branch(q!(state), n(1), C::NotEqual, invalid);
    b.branch(q!(serial), n(0), C::Equal, invalid);
    b.branch(q!(serial), g!(last_lease), C::Above, invalid);
    b.branch(q!(epoch), n(0), C::Equal, invalid);
    b.branch(q!(generation), n(1), C::Above, invalid);
    b.branch(w!(phase), q!(phase), C::NotEqual, invalid);
    b.branch(w!(registry), local(G), C::NotEqual, invalid);
    b.branch(w!(active), n(1), C::NotEqual, invalid);
    b.branch(wl!(kind), n(1), C::NotEqual, invalid);
    generation_mask(&mut b, q!(generation));
    b.branch(w!(mask), local(KIND), C::NotEqual, invalid);
    b.set(local(REG), wl!(registration));
    b.pointer(local(REG), invalid);
    b.branch(r!(registered), n(1), C::NotEqual, invalid);
    b.branch(r!(scope), wl!(scope), C::NotEqual, invalid);
    b.set(local(SEG), r!(segment));
    b.pointer(local(SEG), invalid);
    b.set(local(CTX), field(SEG, offset_of!(RootSegment, journal), 0));
    let no_journal = b.a.create_text_label();
    b.branch(local(CTX), n(0), C::Equal, no_journal);
    b.pointer(local(CTX), invalid);
    b.set(
        local(STATE),
        field(CTX, offset_of!(BufferCursor, consumed), 8),
    );
    b.set(
        local(USED),
        field(CTX, offset_of!(BufferCursor, published), 8),
    );
    b.branch(local(STATE), local(USED), C::Above, invalid);
    b.branch(local(STATE), local(USED), C::NotEqual, status[4]);
    b.a.bind_text_label(no_journal);
    b.branch(r!(journal_serial), n(0), C::NotEqual, status[4]);
    b.set(local(REG), r!(inventory_next));
    b.a.jmp_label(next_owner);
    b.a.bind_text_label(begin);
    let available = b.a.create_text_label();
    b.branch(q!(state), n(0), C::Equal, available);
    b.branch(q!(state), n(2), C::NotEqual, invalid);
    b.a.bind_text_label(available);
    b.branch(w!(active), n(0), C::NotEqual, invalid);
    b.branch(wl!(kind), n(0), C::NotEqual, invalid);
    phase_valid(&mut b, g!(phase), invalid);
    b.branch(queue!(epoch), n(0), C::Equal, invalid);
    b.branch(queue!(generation), n(1), C::Above, invalid);
    b.set(local(KIND), and(g!(phase), n(15)));
    let phase_ready = b.a.create_text_label();
    b.branch(queue!(generation), n(0), C::Equal, phase_ready);
    b.set(local(KIND), and(shr(g!(phase), 4), n(15)));
    b.a.bind_text_label(phase_ready);
    if retire {
        b.branch(local(KIND), n(7), C::NotEqual, invalid);
    } else {
        b.branch(local(KIND), n(1), C::Below, invalid);
        b.branch(local(KIND), n(3), C::Above, invalid);
    }
    b.set(local(REG), g!(inventory));
    b.branch(g!(last_lease), n(u64::MAX), C::Equal, status[3]);
    b.branch(local(REG), n(0), C::Equal, next_owner);
    b.branch(g!(last_lease), n(u64::MAX - 1), C::Equal, status[3]);
    b.a.bind_text_label(next_owner);
    b.set(local(CTX), n(0));
    b.branch(local(REG), n(0), C::Equal, invoke);
    b.pointer(local(REG), invalid);
    b.set(local(CTX), r!(context));
    b.pointer(local(CTX), invalid);
    b.set(local(SEG), r!(segment));
    b.pointer(local(SEG), invalid);
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
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        field(SEG, offset_of!(RootSegment, owner), 0),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(invoke);
    b.set(local(STATE), g!(last_lease));
    b.set(local(USED), q!(serial));
    let call = b.a.create_text_label();
    b.branch(local(OP), n(0), C::NotEqual, call);
    b.set(g!(last_lease), add(g!(last_lease), n(1)));
    b.set(local(USED), g!(last_lease));
    b.a.bind_text_label(call);
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, LEASE);
    b.a.load_rbp_slot(R::V2, OP);
    b.a.call_label(core);
    b.a.store_rbp_slot(NEXT, R::V0);
    b.branch(local(NEXT), n(0), C::Equal, commit);
    b.branch(local(NEXT), n(2), C::Equal, commit);
    b.set(g!(last_lease), local(STATE));
    b.a.jmp_label(result);
    b.a.bind_text_label(commit);
    let state = b.a.create_text_label();
    b.branch(local(OP), n(0), C::NotEqual, state);
    b.set(q!(serial), local(USED));
    b.set(q!(last_scope), g!(last_scope));
    b.set(q!(phase), g!(phase));
    b.set(q!(epoch), queue!(epoch));
    b.set(q!(generation), queue!(generation));
    generation_mask(&mut b, q!(generation));
    b.set(w!(mask), local(KIND));
    b.a.bind_text_label(state);
    b.set(q!(state), n(1));
    b.branch(local(NEXT), n(2), C::NotEqual, sample);
    b.set(q!(state), n(2));
    b.a.bind_text_label(sample);
    b.branch(local(CTX), n(0), C::Equal, result);
    b.set(local(REG), add(local(CTX), shl(q!(generation), 3)));
    b.branch(
        field(REG, offset_of!(MutatorContext, root_audit_serial), 0),
        q!(serial),
        C::Equal,
        result,
    );
    b.set(
        field(REG, offset_of!(MutatorContext, root_audit_serial), 0),
        q!(serial),
    );
    b.set(
        field(REG, offset_of!(MutatorContext, root_audit_identity), 0),
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
    );
    b.set(
        field(REG, offset_of!(MutatorContext, root_audit_revision), 0),
        field(CTX, offset_of!(MutatorContext, root_revision), 8),
    );
    b.a.bind_text_label(result);
    b.a.load_rbp_slot(R::V0, NEXT);
    b.a.jmp_label(done);
    b.a.bind_text_label(check);
    b.branch(q!(state), n(2), C::NotEqual, invalid);
    b.branch(q!(serial), n(0), C::Equal, invalid);
    b.branch(q!(serial), g!(last_lease), C::Above, invalid);
    b.branch(q!(epoch), n(0), C::Equal, invalid);
    b.branch(q!(generation), n(1), C::Above, invalid);
    b.branch(w!(active), n(0), C::NotEqual, invalid);
    b.branch(wl!(kind), n(0), C::NotEqual, invalid);
    b.branch(w!(registry), local(G), C::NotEqual, invalid);
    b.branch(w!(phase), q!(phase), C::NotEqual, invalid);
    generation_mask(&mut b, q!(generation));
    b.branch(w!(mask), local(KIND), C::NotEqual, invalid);
    phase_valid(&mut b, g!(phase), invalid);
    b.branch(q!(last_scope), g!(last_scope), C::NotEqual, status[5]);
    b.branch(queue!(epoch), q!(epoch), C::NotEqual, status[5]);
    b.branch(queue!(generation), q!(generation), C::NotEqual, status[5]);
    if retire {
        b.branch(q!(phase), g!(phase), C::NotEqual, status[5]);
        b.set(local(KIND), and(g!(phase), n(15)));
        let ready = b.a.create_text_label();
        b.branch(q!(generation), n(0), C::Equal, ready);
        b.set(local(KIND), and(shr(g!(phase), 4), n(15)));
        b.a.bind_text_label(ready);
        b.branch(local(KIND), n(7), C::NotEqual, status[5]);
    } else {
        let old = b.a.create_text_label();
        let phase_checked = b.a.create_text_label();
        b.branch(q!(generation), n(1), C::Equal, old);
        for generation in [0, 1] {
            if generation == 1 {
                b.a.bind_text_label(old);
            }
            b.set(local(KIND), and(shr(g!(phase), generation * 4), n(15)));
            b.branch(local(KIND), n(1), C::Below, status[5]);
            b.branch(local(KIND), n(3), C::Above, status[5]);
            b.branch(
                and(shr(g!(phase), 8 + generation), n(1)),
                and(shr(q!(phase), 8 + generation), n(1)),
                C::NotEqual,
                status[5],
            );
            b.a.jmp_label(phase_checked);
        }
        b.a.bind_text_label(phase_checked);
    }
    b.branch(local(OP), n(2), C::Equal, status[2]);
    b.set(local(CTX), q!(context));
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
    b.set(
        local(STATE),
        field(CTX, offset_of!(MutatorContext, state), 4),
    );
    b.branch(local(STATE), n(1), C::Below, invalid);
    b.branch(local(STATE), n(3), C::Above, invalid);
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    if retire {
        b.branch(
            field(CTX, offset_of!(MutatorContext, private_producers), 8),
            n(0),
            C::NotEqual,
            status[4],
        );
        for consumer in [
            offset_of!(MutatorContext, satb_consumer),
            offset_of!(MutatorContext, remembered_consumer),
        ] {
            for member in [
                offset_of!(BufferConsumer, serial),
                offset_of!(BufferConsumer, end),
                offset_of!(BufferConsumer, cursor),
            ] {
                b.branch(
                    field(CTX, consumer + member, 0),
                    n(0),
                    C::NotEqual,
                    status[4],
                );
            }
        }
        for buffer in [
            offset_of!(MutatorContext, satb),
            offset_of!(MutatorContext, remembered),
        ] {
            b.set(
                local(STATE),
                field(CTX, buffer + offset_of!(BufferCursor, consumed), 8),
            );
            b.set(
                local(USED),
                field(CTX, buffer + offset_of!(BufferCursor, published), 8),
            );
            b.branch(local(STATE), local(USED), C::Above, invalid);
            b.branch(
                local(USED),
                field(CTX, buffer + offset_of!(BufferCursor, top), 0),
                C::Above,
                invalid,
            );
            b.branch(local(STATE), local(USED), C::NotEqual, status[4]);
            b.branch(
                local(USED),
                field(CTX, buffer + offset_of!(BufferCursor, top), 0),
                C::NotEqual,
                status[4],
            );
        }
    }
    b.branch(
        field(CTX, offset_of!(MutatorContext, retiring_roots), 0),
        n(0),
        C::NotEqual,
        status[4],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, root_segments), 0),
        n(0),
        C::Equal,
        status[2],
    );
    b.set(local(REG), add(local(CTX), shl(q!(generation), 3)));
    b.branch(
        field(REG, offset_of!(MutatorContext, root_audit_serial), 0),
        q!(serial),
        C::NotEqual,
        status[5],
    );
    b.branch(
        field(REG, offset_of!(MutatorContext, root_audit_identity), 0),
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        C::NotEqual,
        status[5],
    );
    b.branch(
        field(REG, offset_of!(MutatorContext, root_audit_revision), 0),
        field(CTX, offset_of!(MutatorContext, root_revision), 8),
        C::NotEqual,
        status[5],
    );
    b.a.jmp_label(status[2]);
    for (code, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, code as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(core);
    crate::gc_codegen::root_lifetime::walk::emit_walk_locked(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
