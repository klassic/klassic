//! One rooted store attempt with explicit guard and entry-lease ownership.
use super::*;
use crate::gc_abi::{
    BarrierEntry, BufferedSlot, GuardedStore as S, MetadataLeaseRequest as L, ResolveRequest,
    RootAccess as A, RootFollow as F, RootedStoreAttempt as T,
};
fn call<B: PortableAsm>(b: &mut Emit<'_, B>, label: B::TextLabel, pool: E, request: E, op: u64) {
    b.value(pool);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(request);
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
}
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let ret: Vec<_> = (0..12).map(|_| a.create_text_label()).collect();
    let done = a.create_text_label();
    let dynamic = a.create_text_label();
    let enter = a.create_text_label();
    let abort = a.create_text_label();
    let access = a.create_text_label();
    let exit = a.create_text_label();
    let release = a.create_text_label();
    let release_entry = a.create_text_label();
    let acquire = a.create_text_label();
    let metadata = a.create_text_label();
    let commit = a.create_text_label();
    let cleanup = a.create_text_label();
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
    macro_rules! s {
        ($f:ident) => {
            field(Q, offset_of!(T, commit) + offset_of!(S, $f), 0)
        };
    }
    b.pointer(local(Q), ret[1]);
    b.branch(local(OP), n(4), C::Above, ret[1]);
    b.branch(q!(stage), n(5), C::Above, ret[1]);
    b.branch(q!(release_index), n(2), C::Above, ret[1]);
    b.branch(q!(stage), n(5), C::Equal, ret[11]);
    b.branch(local(OP), n(0), C::Equal, enter);
    b.branch(local(OP), n(4), C::Equal, abort);
    b.branch(local(OP), n(1), C::Equal, access);
    b.branch(local(OP), n(2), C::Equal, exit);
    b.a.jmp_label(release);
    b.a.bind_text_label(abort);
    b.branch(q!(stage), n(3), C::AboveOrEqual, ret[9]);
    b.branch(q!(stage), n(2), C::Equal, ret[10]);
    b.set(q!(result), n(7));
    let abort_guard = b.a.create_text_label();
    let abort_lease = b.a.create_text_label();
    b.branch(q!(stage), n(1), C::Equal, abort_guard);
    for off in [0, 8, 16] {
        b.branch(
            field(Q, offset_of!(F, cleanup) + off, 0),
            n(0),
            C::NotEqual,
            abort_lease,
        );
    }
    b.set(q!(stage), n(5));
    b.a.jmp_label(ret[11]);
    b.a.bind_text_label(abort_guard);
    b.set(q!(stage), n(2));
    b.a.jmp_label(ret[10]);
    b.a.bind_text_label(abort_lease);
    b.set(q!(stage), n(3));
    b.a.jmp_label(ret[9]);
    b.a.bind_text_label(enter);
    b.branch(q!(stage), n(3), C::AboveOrEqual, ret[9]);
    b.branch(q!(stage), n(2), C::Equal, ret[10]);
    b.branch(q!(stage), n(1), C::Equal, ret[8]);
    for value in [
        q!(result),
        q!(release_index),
        s!(scope),
        s!(page),
        s!(phase),
        s!(desired),
        s!(transferred),
    ] {
        b.branch(value, n(0), C::NotEqual, ret[1]);
    }
    for off in (0..2 * size_of::<L>()).step_by(8) {
        b.branch(
            field(Q, offset_of!(T, entries) + off, 0),
            n(0),
            C::NotEqual,
            ret[1],
        );
    }
    call(&mut b, acquire, local(G), local(Q), 0);
    b.branch(local(STATE), n(8), C::NotEqual, dynamic);
    for i in 0..2 {
        let entry = offset_of!(T, entries) + i * size_of::<L>();
        for off in [0, 8] {
            b.set(
                field(Q, entry + off, 0),
                field(Q, offset_of!(F, cleanup) + off, 0),
            );
        }
        b.set(field(Q, entry + offset_of!(L, token), 0), n(0));
        call(
            &mut b,
            metadata,
            local(G),
            add(local(Q), n(entry as u64)),
            0,
        );
        let acquired = b.a.create_text_label();
        let full = b.a.create_text_label();
        let exhausted = b.a.create_text_label();
        b.branch(local(STATE), n(0), C::Equal, acquired);
        b.set(q!(stage), n(2));
        b.branch(local(STATE), n(2), C::Equal, full);
        b.branch(local(STATE), n(3), C::Equal, exhausted);
        b.set(q!(result), n(10));
        b.a.jmp_label(ret[10]);
        b.a.bind_text_label(full);
        b.set(q!(result), n(8));
        b.a.jmp_label(ret[10]);
        b.a.bind_text_label(exhausted);
        b.set(q!(result), n(9));
        b.a.jmp_label(ret[10]);
        b.a.bind_text_label(acquired);
    }
    b.set(local(CTX), field(Q, offset_of!(F, context), 0));
    b.set(
        s!(scope),
        add(
            local(Q),
            n((offset_of!(A, access) + offset_of!(ResolveRequest, scope)) as u64),
        ),
    );
    b.set(
        s!(page),
        field(Q, offset_of!(F, cleanup) + offset_of!(L, page), 0),
    );
    b.set(
        s!(phase),
        field(CTX, offset_of!(MutatorContext, adopted_phase), 8),
    );
    for (i, start) in [offset_of!(S, satb), offset_of!(S, remembered)]
        .into_iter()
        .enumerate()
    {
        let entry = offset_of!(T, commit) + start;
        for (off, value) in [
            (offset_of!(BarrierEntry, previous), s!(expected)),
            (
                offset_of!(BarrierEntry, slot) + offset_of!(BufferedSlot, owner),
                field(
                    Q,
                    offset_of!(A, access) + offset_of!(ResolveRequest, reference),
                    0,
                ),
            ),
            (
                offset_of!(BarrierEntry, slot) + offset_of!(BufferedSlot, byte_offset),
                s!(byte_offset),
            ),
            (
                offset_of!(BarrierEntry, slot) + offset_of!(BufferedSlot, page_incarnation),
                field(Q, offset_of!(F, cleanup) + offset_of!(L, incarnation), 0),
            ),
            (
                offset_of!(BarrierEntry, slot) + offset_of!(BufferedSlot, record_epoch),
                s!(phase),
            ),
            (
                offset_of!(BarrierEntry, slot) + offset_of!(BufferedSlot, metadata_lease),
                field(
                    Q,
                    offset_of!(T, entries) + i * size_of::<L>() + offset_of!(L, token),
                    0,
                ),
            ),
        ] {
            b.set(field(Q, entry + off, 0), value);
        }
    }
    b.set(q!(stage), n(1));
    b.a.jmp_label(ret[8]);
    b.a.bind_text_label(access);
    b.branch(q!(stage), n(2), C::Equal, ret[10]);
    b.branch(q!(stage), n(1), C::NotEqual, ret[1]);
    call(
        &mut b,
        commit,
        q!(marks),
        add(local(Q), n(offset_of!(T, commit) as u64)),
        0,
    );
    b.set(q!(result), local(STATE));
    for i in 0..2 {
        let kept = b.a.create_text_label();
        b.branch(and(s!(transferred), n(1 << i)), n(0), C::Equal, kept);
        for off in [0, 8, 16] {
            b.set(
                field(Q, offset_of!(T, entries) + i * size_of::<L>() + off, 0),
                n(0),
            );
        }
        b.a.bind_text_label(kept);
    }
    b.set(q!(stage), n(2));
    b.a.jmp_label(ret[10]);
    b.a.bind_text_label(exit);
    b.branch(q!(stage), n(2), C::NotEqual, ret[1]);
    call(&mut b, cleanup, n(0), local(Q), 0);
    b.branch(local(STATE), n(0), C::NotEqual, ret[10]);
    b.set(q!(stage), n(3));
    b.a.jmp_label(ret[9]);
    b.a.bind_text_label(release);
    b.branch(q!(stage), n(1), C::Equal, ret[8]);
    b.branch(q!(stage), n(2), C::Equal, ret[10]);
    b.branch(q!(stage), n(4), C::Equal, release_entry);
    call(&mut b, cleanup, local(G), local(Q), 1);
    b.branch(q!(stage), n(0), C::Equal, dynamic);
    b.branch(local(STATE), n(0), C::NotEqual, ret[9]);
    b.set(q!(stage), n(4));
    b.a.jmp_label(ret[9]);
    b.a.bind_text_label(release_entry);
    b.branch(q!(release_index), n(2), C::Equal, ret[1]);
    b.set(
        local(REG),
        add(
            add(local(Q), n(offset_of!(T, entries) as u64)),
            add(shl(q!(release_index), 4), shl(q!(release_index), 3)),
        ),
    );
    let empty = b.a.create_text_label();
    b.branch(field(REG, offset_of!(L, token), 0), n(0), C::Equal, empty);
    call(&mut b, metadata, local(G), local(REG), 1);
    b.branch(local(STATE), n(0), C::NotEqual, ret[9]);
    b.a.bind_text_label(empty);
    for off in [0, 8, 16] {
        b.set(field(REG, off, 0), n(0));
    }
    b.set(q!(release_index), add(q!(release_index), n(1)));
    b.branch(q!(release_index), n(2), C::Below, ret[9]);
    b.set(q!(stage), n(5));
    b.a.jmp_label(ret[11]);
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
    b.a.bind_text_label(acquire);
    super::root_access::emit_root_access(b.a);
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
    b.a.bind_text_label(commit);
    super::rooted_store_commit::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(cleanup);
    super::root_access::emit_cleanup(b.a);
    b.a.ret();
}
