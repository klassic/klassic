//! Cooperative heap audit state owner, matching gc_heap_audit.h.
use super::*;
use crate::gc_abi::{
    AllocationPage as P, ForwardTable as T, HeapAudit as H, HeapAuditSlice as S, HeapWalk as W,
    MetadataLeaseRequest as M, ObjectWalk as O, RelocationGate as Gate, ResolveRequest as A,
    SourceAccessRequest as Access,
};
fn clear<B: PortableAsm>(b: &mut Emit<'_, B>, base: i32, size: usize) {
    for i in 0..size / 8 {
        b.set(field(base, i * 8, 0), n(0));
    }
}
fn call<B: PortableAsm>(b: &mut Emit<'_, B>, label: B::TextLabel, first: E, second: E, op: u64) {
    b.value(first);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(second);
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
}
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let returns: Vec<_> = (0..13).map(|_| a.create_text_label()).collect();
    let done = a.create_text_label();
    let scan = a.create_text_label();
    let consume = a.create_text_label();
    let cancel = a.create_text_label();
    let cancel_clear = a.create_text_label();
    let scan_done = a.create_text_label();
    let found = a.create_text_label();
    let finish = a.create_text_label();
    let page = a.create_text_label();
    let object = a.create_text_label();
    let release = a.create_text_label();
    let completed = a.create_text_label();
    let walker = a.create_text_label();
    let objects = a.create_text_label();
    let slicer = a.create_text_label();
    let metadata = a.create_text_label();
    let gate = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(H, $f), 0)
        };
    }
    macro_rules! w {
        ($f:ident) => {
            field(SEG, offset_of!(W, $f), 0)
        };
    }
    macro_rules! o {
        ($f:ident) => {
            field(REG, offset_of!(O, $f), 0)
        };
    }
    macro_rules! s {
        ($f:ident) => {
            field(LEASE, offset_of!(S, $f), 0)
        };
    }
    macro_rules! l {
        ($f:ident) => {
            field(SEG, offset_of!(W, lease) + offset_of!(M, $f), 0)
        };
    }
    let held = field(
        LEASE,
        offset_of!(S, access) + offset_of!(A, scope) + offset_of!(Access, held),
        0,
    );
    b.pointer(local(Q), returns[4]);
    b.branch(local(OP), n(3), C::Above, returns[4]);
    b.branch(q!(stage), n(4), C::Above, returns[4]);
    b.set(local(SEG), add(local(Q), n(offset_of!(H, walk) as u64)));
    b.set(local(REG), add(local(Q), n(offset_of!(H, object) as u64)));
    b.set(local(LEASE), add(local(Q), n(offset_of!(H, slice) as u64)));
    b.branch(local(OP), n(1), C::Equal, scan);
    b.branch(s!(stage), n(0), C::NotEqual, returns[8]);
    b.branch(held.clone(), n(0), C::NotEqual, returns[8]);
    b.branch(local(OP), n(2), C::Equal, consume);
    b.pointer(local(G), returns[4]);
    b.branch(w!(pool), local(G), C::NotEqual, returns[4]);
    b.pointer(q!(context), returns[4]);
    b.set(local(CTX), q!(context));
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        returns[4],
    );
    b.branch(local(OP), n(3), C::Equal, cancel);
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        returns[4],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        returns[4],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        returns[4],
    );
    b.pointer(q!(registry), returns[4]);
    b.pointer(q!(gate), returns[4]);
    b.branch(q!(epoch), n(0), C::Equal, returns[4]);
    b.branch(and(q!(phase), n(0xe000)), n(0), C::NotEqual, returns[4]);
    b.branch(and(q!(phase), n(15)), n(7), C::Above, returns[4]);
    b.branch(and(shr(q!(phase), 4), n(15)), n(7), C::Above, returns[4]);
    b.branch(q!(target_start), n(0), C::Equal, returns[4]);
    b.branch(
        q!(target_start),
        q!(target_limit),
        C::AboveOrEqual,
        returns[4],
    );
    b.branch(
        q!(target_limit),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        returns[4],
    );
    for v in [q!(target_start), q!(target_limit)] {
        b.branch(and(v, n(15)), n(0), C::NotEqual, returns[4]);
    }
    b.set(local(NEXT), q!(gate));
    b.branch(
        field(NEXT, offset_of!(Gate, epoch), 0),
        q!(epoch),
        C::NotEqual,
        returns[7],
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, adopted_phase), 8),
        q!(phase),
        C::NotEqual,
        returns[7],
    );
    call(&mut b, gate, q!(gate), n(0), 3);
    b.branch(local(STATE), n(2), C::Equal, returns[5]);
    b.branch(local(STATE), n(0), C::NotEqual, returns[4]);
    b.branch(q!(stage), n(4), C::Equal, finish);
    b.branch(q!(stage), n(3), C::Equal, returns[3]);
    b.branch(q!(stage), n(2), C::Equal, returns[2]);
    b.branch(q!(stage), n(1), C::Equal, object);
    call(&mut b, walker, local(G), local(SEG), 0);
    b.branch(local(STATE), n(2), C::Equal, completed);
    b.branch(local(STATE), n(0), C::Equal, page);
    for (status, result) in [(1, 1), (4, 12), (5, 6), (6, 5)] {
        b.branch(local(STATE), n(status), C::Equal, returns[result]);
    }
    b.a.jmp_label(returns[4]);
    b.a.bind_text_label(page);
    b.set(local(NEXT), l!(page));
    b.set(local(USED), field(NEXT, offset_of!(P, forward_table), 0));
    b.pointer(local(USED), returns[10]);
    clear(&mut b, REG, size_of::<O>());
    b.set(o!(lease), add(local(SEG), n(offset_of!(W, lease) as u64)));
    b.set(o!(page), l!(page));
    b.set(o!(incarnation), l!(incarnation));
    b.set(o!(table), local(USED));
    b.set(o!(used), field(USED, offset_of!(T, used), 8));
    b.set(q!(stage), n(1));
    b.a.jmp_label(returns[1]);
    b.a.bind_text_label(completed);
    b.set(q!(stage), n(4));
    b.a.bind_text_label(finish);
    b.branch(q!(findings), n(0), C::NotEqual, returns[9]);
    b.a.jmp_label(returns[0]);
    b.a.bind_text_label(object);
    call(&mut b, objects, local(G), local(REG), 0);
    b.branch(local(STATE), n(3), C::Equal, returns[7]);
    b.branch(local(STATE), n(2), C::Equal, returns[4]);
    b.branch(local(STATE), n(1), C::Equal, release);
    clear(&mut b, LEASE, size_of::<S>());
    b.set(s!(page), l!(page));
    b.set(s!(context), q!(context));
    b.set(s!(registry), q!(registry));
    b.set(s!(phase), q!(phase));
    b.set(s!(target_start), q!(target_start));
    b.set(s!(target_limit), q!(target_limit));
    b.set(local(KIND), n(0));
    for (shift, mask, base) in [(12, 1, 4), (8, 1, 8), (9, 1, 10), (10, 3, 12)] {
        b.value(and(shr(q!(phase), shift), n(mask)));
        b.a.mov_reg_reg(R::V1, R::V0);
        b.a.add_reg_imm32(R::V1, base);
        b.a.mov_imm64(R::V0, 1);
        b.a.shl_reg_count(R::V0);
        b.a.store_rbp_slot(USED, R::V0);
        b.set(local(KIND), add(local(KIND), local(USED)));
    }
    b.set(s!(reference), add(shl(o!(source_offset), 16), local(KIND)));
    b.set(q!(stage), n(2));
    b.a.jmp_label(returns[2]);
    b.a.bind_text_label(release);
    call(
        &mut b,
        metadata,
        local(G),
        add(local(SEG), n(offset_of!(W, lease) as u64)),
        1,
    );
    b.branch(local(STATE), n(0), C::NotEqual, returns[10]);
    clear(&mut b, REG, size_of::<O>());
    clear(&mut b, LEASE, size_of::<S>());
    b.set(q!(stage), n(0));
    b.a.jmp_label(returns[1]);
    b.a.bind_text_label(scan);
    b.branch(q!(stage), n(2), C::NotEqual, returns[4]);
    let invoke_slice = b.a.create_text_label();
    b.branch(s!(stage), n(0), C::NotEqual, invoke_slice);
    b.branch(held.clone(), n(0), C::NotEqual, invoke_slice);
    for v in [l!(page), l!(incarnation), l!(token), o!(source_offset)] {
        b.branch(v, n(0), C::Equal, returns[4]);
    }
    b.branch(
        o!(lease),
        add(local(SEG), n(offset_of!(W, lease) as u64)),
        C::NotEqual,
        returns[4],
    );
    for (left, right) in [
        (o!(page), l!(page)),
        (o!(incarnation), l!(incarnation)),
        (s!(page), l!(page)),
        (shr(s!(reference), 16), o!(source_offset)),
        (s!(context), q!(context)),
        (s!(registry), q!(registry)),
        (s!(phase), q!(phase)),
        (s!(target_start), q!(target_start)),
        (s!(target_limit), q!(target_limit)),
    ] {
        b.branch(left, right, C::NotEqual, returns[4]);
    }
    b.a.bind_text_label(invoke_slice);
    call(&mut b, slicer, local(LEASE), n(0), 0);
    b.branch(s!(stage), n(0), C::NotEqual, returns[8]);
    b.branch(held, n(0), C::NotEqual, returns[8]);
    for status in [0, 4] {
        b.branch(local(STATE), n(status), C::Equal, scan_done);
    }
    b.branch(local(STATE), n(1), C::Equal, returns[2]);
    b.branch(local(STATE), n(3), C::Equal, found);
    for status in [5, 6] {
        b.branch(local(STATE), n(status), C::Equal, returns[status as usize]);
    }
    b.a.jmp_label(returns[4]);
    b.a.bind_text_label(scan_done);
    b.set(o!(source_offset), n(0));
    clear(&mut b, LEASE, size_of::<S>());
    b.set(q!(stage), n(1));
    b.a.jmp_label(returns[1]);
    b.a.bind_text_label(found);
    b.set(q!(stage), n(3));
    b.a.jmp_label(returns[3]);
    b.a.bind_text_label(consume);
    b.branch(q!(stage), n(3), C::NotEqual, returns[4]);
    b.branch(s!(observed), n(0), C::Equal, returns[4]);
    b.branch(s!(cursor), n(u64::MAX - 8), C::Above, returns[4]);
    b.branch(q!(findings), n(u64::MAX), C::Equal, returns[6]);
    b.set(q!(findings), add(q!(findings), n(1)));
    b.set(s!(cursor), add(s!(cursor), n(8)));
    b.set(s!(observed), n(0));
    b.set(q!(stage), n(2));
    b.a.jmp_label(returns[2]);
    b.a.bind_text_label(cancel);
    let release_cancel = b.a.create_text_label();
    for v in [l!(page), l!(incarnation), l!(token)] {
        b.branch(v, n(0), C::NotEqual, release_cancel);
    }
    b.a.jmp_label(cancel_clear);
    b.a.bind_text_label(release_cancel);
    call(
        &mut b,
        metadata,
        local(G),
        add(local(SEG), n(offset_of!(W, lease) as u64)),
        1,
    );
    b.branch(local(STATE), n(0), C::NotEqual, returns[10]);
    b.a.bind_text_label(cancel_clear);
    b.set(w!(cursor), n(0));
    clear(&mut b, REG, size_of::<O>());
    clear(&mut b, LEASE, size_of::<S>());
    b.set(q!(stage), n(0));
    b.set(q!(findings), n(0));
    b.a.jmp_label(returns[11]);
    for (status, label) in returns.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(walker);
    super::heap_walk::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(objects);
    super::object_walk::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(slicer);
    super::heap_audit_slice::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
    b.a.bind_text_label(gate);
    super::relocation_gate::emit_relocation_gate(b.a);
    b.a.ret();
}
