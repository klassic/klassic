//! Bounded inventory discovery with a leased cursor. See gc_root_walk.h.
use super::*;
use crate::gc_abi::{RootWalk, RootWalkHeal, RootWalkItem, RootWalkScan};
const WALK: i32 = Q;
macro_rules! w {
    ($f:ident) => {
        field(WALK, offset_of!(RootWalk, $f), 0)
    };
}
macro_rules! wl {
    ($f:ident) => {
        field(
            WALK,
            offset_of!(RootWalk, lease) + offset_of!(RootLease, $f),
            0,
        )
    };
}
macro_rules! next {
    ($f:ident) => {
        field(NEXT, offset_of!(RootRegistration, $f), 0)
    };
}
macro_rules! prev {
    ($f:ident) => {
        field(PREV, offset_of!(RootRegistration, $f), 0)
    };
}

fn geometry<A: PortableAsm>(b: &mut Emit<'_, A>, invalid: A::TextLabel) {
    b.branch(s!(owner), n(0), C::Equal, invalid);
    b.branch(s!(scope), n(0), C::Equal, invalid);
    b.pointer(s!(cells), invalid);
    b.branch(s!(capacity), n(0), C::Equal, invalid);
    b.branch(s!(capacity), n(u32::MAX as u64), C::Above, invalid);
    b.branch(
        add(s!(cells), shl(s!(capacity), 5)),
        s!(cells),
        C::Below,
        invalid,
    );
}

/// V7=registry, V6=walk, V2=BEGIN/NEXT; returns ITEM/INVALID/DONE/EXHAUSTED/WAIT.
#[allow(dead_code)]
pub(crate) fn emit_walk_locked<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let exhausted = a.create_text_label();
    let wait = a.create_text_label();
    let done = a.create_text_label();
    let no_more = a.create_text_label();
    let advance = a.create_text_label();
    let validate = a.create_text_label();
    let commit = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(WALK, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(1), C::Above, invalid);
    b.set(local(PREV), n(0));
    b.branch(local(OP), n(1), C::Equal, advance);
    b.branch(w!(active), n(0), C::NotEqual, invalid);
    b.branch(wl!(kind), n(0), C::NotEqual, invalid);
    for value in [
        w!(pending),
        w!(pending_reference),
        w!(pending_observed),
        w!(pending_index),
    ] {
        b.branch(value, n(0), C::NotEqual, invalid);
    }
    b.branch(and(g!(phase), n(15)), n(7), C::Above, invalid);
    b.branch(and(shr(g!(phase), 4), n(15)), n(7), C::Above, invalid);
    b.set(local(NEXT), g!(inventory));
    b.a.jmp_label(validate);
    b.a.bind_text_label(advance);
    b.branch(w!(active), n(1), C::NotEqual, invalid);
    b.branch(w!(registry), local(G), C::NotEqual, invalid);
    b.branch(wl!(kind), n(1), C::NotEqual, invalid);
    b.pointer(wl!(registration), invalid);
    b.branch(wl!(serial), n(0), C::Equal, invalid);
    b.branch(g!(walkers), n(0), C::Equal, invalid);
    b.branch(g!(leases), n(0), C::Equal, invalid);
    b.set(local(PREV), wl!(registration));
    b.branch(prev!(registered), n(1), C::NotEqual, invalid);
    b.branch(prev!(scope), wl!(scope), C::NotEqual, invalid);
    b.branch(prev!(leases), n(0), C::Equal, invalid);
    b.branch(prev!(leases), g!(leases), C::Above, invalid);
    b.branch(w!(cursor), w!(limit), C::Above, invalid);
    b.branch(w!(cursor), w!(limit), C::NotEqual, wait);
    b.branch(w!(completed_mask), w!(mask), C::NotEqual, wait);
    for value in [
        w!(pending),
        w!(pending_reference),
        w!(pending_observed),
        w!(pending_index),
    ] {
        b.branch(value, n(0), C::NotEqual, wait);
    }
    b.set(local(NEXT), prev!(inventory_next));
    b.a.bind_text_label(validate);
    b.set(local(USED), n(0));
    b.branch(local(NEXT), n(0), C::Equal, commit);
    b.pointer(local(NEXT), invalid);
    b.branch(next!(registered), n(1), C::NotEqual, invalid);
    b.branch(next!(scope), n(0), C::Equal, invalid);
    let initial = a_label(&mut b);
    let links = a_label(&mut b);
    b.branch(local(PREV), n(0), C::Equal, initial);
    b.branch(next!(scope), prev!(scope), C::AboveOrEqual, invalid);
    b.branch(next!(inventory_previous), local(PREV), C::NotEqual, invalid);
    b.a.jmp_label(links);
    b.a.bind_text_label(initial);
    b.branch(next!(inventory_previous), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(links);
    b.pointer(next!(segment), invalid);
    b.branch(next!(leases), g!(leases), C::Above, invalid);
    b.set(local(SEG), next!(segment));
    b.set(local(STATE), s!(state));
    b.set(local(USED), s!(used));
    b.branch(local(STATE), n(1), C::Below, invalid);
    b.branch(local(STATE), n(2), C::Above, invalid);
    b.branch(s!(registration), local(NEXT), C::NotEqual, invalid);
    b.branch(s!(scope), next!(scope), C::NotEqual, invalid);
    geometry(&mut b, invalid);
    b.branch(local(USED), s!(capacity), C::Above, invalid);
    b.branch(g!(last_lease), n(u64::MAX), C::Equal, exhausted);
    b.branch(next!(leases), n(u64::MAX), C::Equal, exhausted);
    b.branch(local(PREV), n(0), C::NotEqual, commit);
    b.branch(g!(leases), n(u64::MAX), C::Equal, exhausted);
    b.branch(g!(walkers), n(u64::MAX), C::Equal, exhausted);
    b.a.bind_text_label(commit);
    let begin = a_label(&mut b);
    let state = a_label(&mut b);
    b.branch(local(PREV), n(0), C::Equal, begin);
    b.set(prev!(leases), sub(prev!(leases), n(1)));
    b.branch(local(NEXT), n(0), C::NotEqual, state);
    b.set(g!(leases), sub(g!(leases), n(1)));
    b.set(g!(walkers), sub(g!(walkers), n(1)));
    b.a.jmp_label(state);
    b.a.bind_text_label(begin);
    b.set(w!(registry), local(G));
    b.set(w!(phase), g!(phase));
    b.set(w!(mask), n(0));
    for (shift, bit) in [(0, 1), (4, 2)] {
        let skip = a_label(&mut b);
        let phase = and(shr(g!(phase), shift), n(15));
        b.branch(phase.clone(), n(1), C::Below, skip);
        b.branch(phase, n(3), C::Above, skip);
        b.set(w!(mask), add(w!(mask), n(bit)));
        b.a.bind_text_label(skip);
    }
    b.branch(local(NEXT), n(0), C::Equal, state);
    b.set(g!(leases), add(g!(leases), n(1)));
    b.set(g!(walkers), add(g!(walkers), n(1)));
    b.a.bind_text_label(state);
    b.set(w!(cursor), n(0));
    b.set(w!(completed_mask), n(0));
    b.set(w!(limit), local(USED));
    b.branch(local(NEXT), n(0), C::Equal, no_more);
    b.set(w!(active), n(1));
    b.set(next!(leases), add(next!(leases), n(1)));
    b.set(g!(last_lease), add(g!(last_lease), n(1)));
    b.set(wl!(registration), local(NEXT));
    b.set(wl!(scope), next!(scope));
    b.set(wl!(serial), g!(last_lease));
    b.set(wl!(kind), n(1));
    b.set(wl!(cursor), n(0));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(no_more);
    b.set(w!(active), n(0));
    b.set(wl!(kind), n(0));
    b.a.mov_imm64(R::V0, 2);
    b.a.jmp_label(done);
    for (label, status) in [(invalid, 1), (exhausted, 3), (wait, 4)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
fn a_label<A: PortableAsm>(b: &mut Emit<'_, A>) -> A::TextLabel {
    b.a.create_text_label()
}

/// V7=RootWalkScan, no registry lock, single owner of walk/control storage.
#[allow(dead_code)]
pub(crate) fn emit_walk_scan<A: PortableAsm>(a: &mut A) {
    const REQUEST: i32 = OP;
    macro_rules! request {
        ($f:ident) => {
            field(REQUEST, offset_of!(RootWalkScan, $f), 0)
        };
    }
    let invalid = a.create_text_label();
    let more = a.create_text_label();
    let done = a.create_text_label();
    let count_ready = a.create_text_label();
    let copy = a.create_text_label();
    let finish = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(REQUEST, R::V7);
    let mut b = Emit { a };
    b.pointer(request!(walk), invalid);
    b.pointer(request!(output), invalid);
    b.branch(request!(capacity), n(0), C::Equal, invalid);
    b.branch(request!(capacity), n(256), C::Above, invalid);
    b.branch(
        add(request!(output), shl(request!(capacity), 3)),
        request!(output),
        C::Below,
        invalid,
    );
    b.set(local(WALK), request!(walk));
    for value in [
        w!(pending),
        w!(pending_reference),
        w!(pending_observed),
        w!(pending_index),
    ] {
        b.branch(value, n(0), C::NotEqual, invalid);
    }
    b.branch(w!(active), n(1), C::NotEqual, invalid);
    b.branch(wl!(kind), n(1), C::NotEqual, invalid);
    b.branch(request!(serial), n(0), C::Equal, invalid);
    b.branch(request!(serial), wl!(serial), C::NotEqual, invalid);
    b.pointer(wl!(registration), invalid);
    b.branch(w!(cursor), w!(limit), C::Above, invalid);
    b.set(local(REG), wl!(registration));
    b.branch(r!(registered), n(1), C::NotEqual, invalid);
    b.branch(r!(scope), wl!(scope), C::NotEqual, invalid);
    b.pointer(r!(segment), invalid);
    b.set(local(SEG), r!(segment));
    b.set(local(STATE), s!(state));
    b.set(local(USED), s!(used));
    geometry(&mut b, invalid);
    b.branch(local(STATE), n(1), C::Below, invalid);
    b.branch(local(STATE), n(2), C::Above, invalid);
    b.branch(s!(scope), r!(scope), C::NotEqual, invalid);
    b.branch(s!(registration), local(REG), C::NotEqual, invalid);
    b.branch(local(USED), s!(capacity), C::Above, invalid);
    b.branch(w!(limit), local(USED), C::Above, invalid);
    b.set(local(NEXT), sub(w!(limit), w!(cursor)));
    b.branch(local(NEXT), request!(capacity), C::Below, count_ready);
    b.set(local(NEXT), request!(capacity));
    b.a.bind_text_label(count_ready);
    b.set(local(USED), n(0));
    b.set(local(PREV), request!(output));
    b.a.bind_text_label(copy);
    b.branch(local(USED), local(NEXT), C::Equal, finish);
    let address = add(s!(cells), shl(add(w!(cursor), local(USED)), 5));
    b.set(field(PREV, 0, 0), E::Mem(Box::new(address), 0, 8));
    b.set(local(PREV), add(local(PREV), n(8)));
    b.set(local(USED), add(local(USED), n(1)));
    b.a.jmp_label(copy);
    b.a.bind_text_label(finish);
    b.set(w!(cursor), add(w!(cursor), local(NEXT)));
    b.set(request!(written), local(NEXT));
    b.branch(w!(cursor), w!(limit), C::NotEqual, more);
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(more);
    b.a.mov_imm64(R::V0, 2);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
}

/// Exact-observation physical root repair under the scanner's native lease.
#[allow(dead_code)]
pub(crate) fn emit_walk_heal<A: PortableAsm>(a: &mut A) {
    const REQUEST: i32 = OP;
    macro_rules! request {
        ($f:ident) => {
            field(REQUEST, offset_of!(RootWalkHeal, $f), 0)
        };
    }
    let invalid = a.create_text_label();
    let changed = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(REQUEST, R::V7);
    let mut b = Emit { a };
    b.pointer(request!(walk), invalid);
    for value in [request!(expected), request!(desired)] {
        b.branch(value.clone(), n(0), C::Equal, invalid);
        super::barrier::reference(&mut b, value, invalid);
    }
    b.branch(
        and(request!(expected), n(0xffff)),
        and(request!(desired), n(0xffff)),
        C::NotEqual,
        invalid,
    );
    b.set(local(WALK), request!(walk));
    b.branch(w!(active), n(1), C::NotEqual, invalid);
    b.branch(wl!(kind), n(1), C::NotEqual, invalid);
    b.branch(request!(serial), n(0), C::Equal, invalid);
    b.branch(request!(serial), wl!(serial), C::NotEqual, invalid);
    b.pointer(wl!(registration), invalid);
    b.branch(w!(cursor), w!(limit), C::Above, invalid);
    b.set(local(REG), wl!(registration));
    b.branch(r!(registered), n(1), C::NotEqual, invalid);
    b.branch(r!(scope), wl!(scope), C::NotEqual, invalid);
    b.pointer(r!(segment), invalid);
    b.set(local(SEG), r!(segment));
    b.set(local(STATE), s!(state));
    b.set(local(USED), s!(used));
    geometry(&mut b, invalid);
    b.branch(local(STATE), n(1), C::Below, invalid);
    b.branch(local(STATE), n(2), C::Above, invalid);
    b.branch(s!(scope), r!(scope), C::NotEqual, invalid);
    b.branch(s!(registration), local(REG), C::NotEqual, invalid);
    b.branch(local(USED), s!(capacity), C::Above, invalid);
    b.branch(w!(limit), local(USED), C::Above, invalid);

    b.branch(request!(index), w!(cursor), C::AboveOrEqual, invalid);
    b.set(local(NEXT), add(s!(cells), shl(request!(index), 5)));
    b.value(request!(desired));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.value(request!(expected));
    b.a.load_rbp_slot(R::V11, NEXT);
    b.a.atomic_compare_exchange(R::V11, R::V2);
    b.a.store_rbp_slot(KIND, R::V0);
    b.branch(local(KIND), request!(expected), C::NotEqual, changed);
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(changed);
    b.a.mov_imm64(R::V0, 2);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
}

/// Capture/ACK one counted root snapshot. V7=request, V6=operation.
#[allow(dead_code)]
pub(crate) fn emit_walk_item<A: PortableAsm>(a: &mut A) {
    const REQUEST: i32 = OP;
    macro_rules! request {
        ($f:ident) => {
            field(REQUEST, offset_of!(RootWalkItem, $f), 0)
        };
    }
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let ack = a.create_text_label();
    let wait = a.create_text_label();
    let empty = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(REQUEST, R::V7);
    a.store_rbp_slot(CTX, R::V6);
    let mut b = Emit { a };
    b.branch(local(CTX), n(1), C::Above, invalid);
    b.pointer(request!(walk), invalid);
    b.set(local(WALK), request!(walk));
    b.branch(w!(active), n(1), C::NotEqual, invalid);
    b.branch(wl!(kind), n(1), C::NotEqual, invalid);
    b.branch(request!(serial), n(0), C::Equal, invalid);
    b.branch(request!(serial), wl!(serial), C::NotEqual, invalid);
    b.pointer(wl!(registration), invalid);
    b.branch(w!(cursor), w!(limit), C::Above, invalid);
    b.set(local(REG), wl!(registration));
    b.branch(r!(registered), n(1), C::NotEqual, invalid);
    b.branch(r!(scope), wl!(scope), C::NotEqual, invalid);
    b.pointer(r!(segment), invalid);
    b.set(local(SEG), r!(segment));
    b.set(local(STATE), s!(state));
    b.set(local(USED), s!(used));
    geometry(&mut b, invalid);
    b.branch(local(STATE), n(1), C::Below, invalid);
    b.branch(local(STATE), n(2), C::Above, invalid);
    b.branch(s!(scope), r!(scope), C::NotEqual, invalid);
    b.branch(s!(registration), local(REG), C::NotEqual, invalid);
    b.branch(local(USED), s!(capacity), C::Above, invalid);
    b.branch(w!(limit), local(USED), C::Above, invalid);

    b.branch(w!(pending), n(1), C::Above, invalid);
    b.branch(local(CTX), n(1), C::Equal, ack);
    b.branch(w!(pending), n(0), C::NotEqual, wait);
    for value in [
        w!(pending_reference),
        w!(pending_observed),
        w!(pending_index),
    ] {
        b.branch(value, n(0), C::NotEqual, invalid);
    }
    b.branch(w!(cursor), w!(limit), C::Equal, empty);
    b.set(local(NEXT), add(s!(cells), shl(w!(cursor), 5)));
    b.set(local(PREV), field(NEXT, 0, 8));
    b.set(w!(pending_reference), local(PREV));
    b.set(w!(pending_observed), local(PREV));
    b.set(request!(index), w!(cursor));
    b.set(w!(pending_index), w!(cursor));
    b.set(w!(pending), n(1));
    b.set(w!(cursor), add(w!(cursor), n(1)));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(ack);
    b.branch(w!(pending), n(1), C::NotEqual, invalid);
    b.branch(request!(index), w!(pending_index), C::NotEqual, invalid);
    b.branch(w!(cursor), n(0), C::Equal, invalid);
    b.branch(
        w!(pending_index),
        sub(w!(cursor), n(1)),
        C::NotEqual,
        invalid,
    );
    for value in [
        w!(pending),
        w!(pending_reference),
        w!(pending_observed),
        w!(pending_index),
    ] {
        b.set(value, n(0));
    }
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    for (label, status) in [(invalid, 1), (empty, 2), (wait, 3)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
