//! Heap-lock protected page metadata leases. Matching contract: gc_metadata.h.
use super::*;
use crate::gc_abi::{
    AllocationPage, HeapPool, MetadataLeasePool, MetadataLeaseRequest, MetadataLeaseSlot,
};
macro_rules! pool {
    ($f:ident) => {
        field(G, offset_of!(MetadataLeasePool, $f), 0)
    };
}
macro_rules! request {
    ($f:ident) => {
        field(Q, offset_of!(MetadataLeaseRequest, $f), 0)
    };
}
macro_rules! slot {
    ($f:ident) => {
        field(REG, offset_of!(MetadataLeaseSlot, $f), 0)
    };
}
macro_rules! heap {
    ($f:ident) => {
        field(LEASE, offset_of!(HeapPool, $f), 0)
    };
}
macro_rules! page {
    (state) => {
        field(SEG, offset_of!(AllocationPage, state), 8)
    };
    ($f:ident) => {
        field(SEG, offset_of!(AllocationPage, $f), 0)
    };
}
const _: () = assert!(std::mem::size_of::<AllocationPage>() == 112);
fn page_bytes(value: E) -> E {
    add(
        add(shl(value.clone(), 6), shl(value.clone(), 5)),
        shl(value, 4),
    )
}
fn select_slot<A: PortableAsm>(b: &mut Emit<'_, A>) {
    b.set(
        local(REG),
        add(pool!(slots), shl(sub(local(STATE), n(1)), 5)),
    );
}

/// V7=pool,V6=request,V2=acquire/release/resolve. No scans, allocation or waits.
#[allow(dead_code)]
pub(crate) fn emit_metadata_step<A: PortableAsm>(a: &mut A) {
    emit_metadata_step_tracked(a, &mut |_| {}, &mut |_| {});
}
/// Optional production hooks run under the same heap ticket. `changed` sees
/// the actual mutated slot in V8; `full` sees an unchanged failed request.
/// Hooks preserve all registers. Standalone leaf behavior remains unchanged.
pub(crate) fn emit_metadata_step_tracked<A: PortableAsm>(
    a: &mut A,
    changed: &mut impl FnMut(&mut A),
    full: &mut impl FnMut(&mut A),
) {
    let status: [_; 4] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let held = a.create_text_label();
    let validate = a.create_text_label();
    let release = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(2), C::Above, invalid);
    b.pointer(pool!(heap), invalid);
    b.pointer(pool!(slots), invalid);
    b.branch(pool!(capacity), n(0), C::Equal, invalid);
    b.branch(pool!(capacity), n(u32::MAX as u64), C::Above, invalid);
    b.branch(
        add(pool!(slots), shl(pool!(capacity), 5)),
        pool!(slots),
        C::Below,
        invalid,
    );
    b.branch(pool!(active), pool!(capacity), C::Above, invalid);
    b.branch(pool!(free_head), pool!(capacity), C::Above, invalid);
    let not_full = b.a.create_text_label();
    let head_ok = b.a.create_text_label();
    b.branch(pool!(active), pool!(capacity), C::Below, not_full);
    b.branch(pool!(free_head), n(0), C::NotEqual, invalid);
    b.a.jmp_label(head_ok);
    b.a.bind_text_label(not_full);
    b.branch(pool!(free_head), n(0), C::Equal, invalid);
    b.a.bind_text_label(head_ok);
    b.set(local(LEASE), pool!(heap));
    b.pointer(heap!(pages), invalid);
    b.branch(heap!(page_count), n(0), C::Equal, invalid);
    b.branch(heap!(page_count), n(u64::MAX / 112), C::Above, invalid);
    b.branch(
        add(heap!(pages), page_bytes(heap!(page_count))),
        heap!(pages),
        C::Below,
        invalid,
    );
    b.branch(local(OP), n(0), C::NotEqual, held);
    b.branch(request!(token), n(0), C::NotEqual, invalid);
    b.branch(pool!(free_head), n(0), C::Equal, status[2]);
    b.set(local(STATE), pool!(free_head));
    select_slot(&mut b);
    for v in [slot!(page), slot!(incarnation)] {
        b.branch(v, n(0), C::NotEqual, invalid);
    }
    b.branch(slot!(generation), n(u32::MAX as u64), C::Above, invalid);
    b.branch(slot!(next_free), pool!(capacity), C::Above, invalid);
    b.branch(slot!(next_free), local(STATE), C::Equal, invalid);
    let more_free = b.a.create_text_label();
    let free_ok = b.a.create_text_label();
    b.branch(
        add(pool!(active), n(1)),
        pool!(capacity),
        C::Below,
        more_free,
    );
    b.branch(slot!(next_free), n(0), C::NotEqual, invalid);
    b.a.jmp_label(free_ok);
    b.a.bind_text_label(more_free);
    b.branch(slot!(next_free), n(0), C::Equal, invalid);
    b.a.bind_text_label(free_ok);
    b.branch(slot!(generation), n(u32::MAX as u64), C::Equal, status[3]);
    b.set(local(SEG), request!(page));
    b.set(local(PREV), request!(incarnation));
    b.a.jmp_label(validate);
    b.a.bind_text_label(held);
    b.set(local(STATE), and(request!(token), n(u32::MAX as u64)));
    b.set(local(USED), shr(request!(token), 32));
    b.branch(local(STATE), n(0), C::Equal, invalid);
    b.branch(local(STATE), pool!(capacity), C::Above, invalid);
    b.branch(local(USED), n(0), C::Equal, invalid);
    b.branch(pool!(active), n(0), C::Equal, invalid);
    select_slot(&mut b);
    b.branch(slot!(generation), local(USED), C::NotEqual, invalid);
    b.branch(slot!(page), n(0), C::Equal, invalid);
    b.branch(slot!(incarnation), n(0), C::Equal, invalid);
    b.branch(slot!(next_free), n(0), C::NotEqual, invalid);
    let token_ok = b.a.create_text_label();
    b.branch(local(OP), n(1), C::NotEqual, token_ok);
    b.branch(request!(page), slot!(page), C::NotEqual, invalid);
    b.branch(
        request!(incarnation),
        slot!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(token_ok);
    b.set(local(SEG), slot!(page));
    b.set(local(PREV), slot!(incarnation));
    b.a.bind_text_label(validate);
    b.branch(local(PREV), n(0), C::Equal, invalid);
    b.branch(local(SEG), heap!(pages), C::Below, invalid);
    b.branch(
        sub(local(SEG), heap!(pages)),
        page_bytes(heap!(page_count)),
        C::AboveOrEqual,
        invalid,
    );
    b.value(sub(local(SEG), heap!(pages)));
    b.a.mov_imm64(R::V1, 112);
    b.a.div_unsigned(R::V1);
    b.a.store_rbp_slot(USED, R::V0);
    b.a.cmp_reg_imm32(R::V2, 0);
    b.a.jcc_label(C::NotEqual, invalid);
    b.branch(page!(incarnation), local(PREV), C::NotEqual, invalid);
    b.branch(local(OP), n(0), C::NotEqual, release);
    let live = b.a.create_text_label();
    b.branch(page!(state), n(1), C::Equal, live);
    b.branch(page!(state), n(5), C::Equal, live);
    b.branch(page!(state), n(7), C::Equal, live);
    b.branch(page!(state), n(8), C::NotEqual, invalid);
    b.a.bind_text_label(live);
    b.branch(page!(run_head), local(USED), C::NotEqual, invalid);
    b.branch(page!(metadata_leases), n(u64::MAX), C::Equal, status[3]);
    b.set(pool!(free_head), slot!(next_free));
    b.set(pool!(active), add(pool!(active), n(1)));
    b.set(page!(metadata_leases), add(page!(metadata_leases), n(1)));
    b.set(slot!(generation), add(slot!(generation), n(1)));
    b.set(slot!(page), local(SEG));
    b.set(slot!(incarnation), local(PREV));
    b.set(slot!(next_free), n(0));
    b.set(
        request!(token),
        add(shl(slot!(generation), 32), local(STATE)),
    );
    b.a.push_reg(R::V8);
    b.a.load_rbp_slot(R::V8, REG);
    changed(b.a);
    b.a.pop_reg(R::V8);
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(release);
    b.branch(page!(metadata_leases), n(0), C::Equal, invalid);
    let free = b.a.create_text_label();
    b.branch(local(OP), n(2), C::NotEqual, free);
    b.set(request!(page), local(SEG));
    b.set(request!(incarnation), local(PREV));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(free);
    b.set(page!(metadata_leases), sub(page!(metadata_leases), n(1)));
    b.set(pool!(active), sub(pool!(active), n(1)));
    b.set(slot!(page), n(0));
    b.set(slot!(incarnation), n(0));
    b.set(slot!(next_free), pool!(free_head));
    b.set(pool!(free_head), local(STATE));
    for off in [0, 8, 16] {
        b.set(field(Q, off, 0), n(0));
    }
    b.a.push_reg(R::V8);
    b.a.load_rbp_slot(R::V8, REG);
    changed(b.a);
    b.a.pop_reg(R::V8);
    b.a.jmp_label(status[0]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        if value == 2 {
            full(b.a);
        }
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
