//! Allocation counter observation; the caller supplies time and retained context.
use super::*;
use crate::gc_abi::{ABI_VERSION, AllocationSample, MutatorContext};

#[allow(dead_code)]
pub(crate) fn emit_sample<A: PortableAsm>(a: &mut A) {
    let statuses: [_; 5] = std::array::from_fn(|_| a.create_text_label());
    let invalid = statuses[3];
    let active = a.create_text_label();
    let read = a.create_text_label();
    let baseline = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.branch(
        field(G, offset_of!(MutatorContext, abi_version), 4),
        n(ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(G, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(Q, offset_of!(AllocationSample, context), 0),
        n(0),
        C::NotEqual,
        active,
    );
    for off in [
        offset_of!(AllocationSample, identity),
        offset_of!(AllocationSample, timestamp_ns),
        offset_of!(AllocationSample, total_bytes),
        offset_of!(AllocationSample, elapsed_ns),
        offset_of!(AllocationSample, delta_bytes),
    ] {
        b.branch(field(Q, off, 0), n(0), C::NotEqual, invalid);
    }
    b.a.jmp_label(read);
    b.a.bind_text_label(active);
    b.branch(
        field(Q, offset_of!(AllocationSample, context), 0),
        local(G),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(Q, offset_of!(AllocationSample, identity), 0),
        field(G, offset_of!(MutatorContext, thread_id), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(Q, offset_of!(AllocationSample, total_bytes), 0),
        n(u64::MAX),
        C::Equal,
        invalid,
    );
    b.branch(
        local(OP),
        field(Q, offset_of!(AllocationSample, timestamp_ns), 0),
        C::Below,
        invalid,
    );
    b.branch(
        local(OP),
        field(Q, offset_of!(AllocationSample, timestamp_ns), 0),
        C::Equal,
        statuses[2],
    );
    b.a.bind_text_label(read);
    b.set(
        local(REG),
        field(G, offset_of!(MutatorContext, allocated_bytes), 8),
    );
    b.branch(local(REG), n(u64::MAX), C::Equal, statuses[4]);
    b.branch(
        field(Q, offset_of!(AllocationSample, context), 0),
        n(0),
        C::Equal,
        baseline,
    );
    b.branch(
        local(REG),
        field(Q, offset_of!(AllocationSample, total_bytes), 0),
        C::Below,
        invalid,
    );
    b.set(
        field(Q, offset_of!(AllocationSample, elapsed_ns), 0),
        sub(
            local(OP),
            field(Q, offset_of!(AllocationSample, timestamp_ns), 0),
        ),
    );
    b.set(
        field(Q, offset_of!(AllocationSample, delta_bytes), 0),
        sub(
            local(REG),
            field(Q, offset_of!(AllocationSample, total_bytes), 0),
        ),
    );
    b.set(
        field(Q, offset_of!(AllocationSample, timestamp_ns), 0),
        local(OP),
    );
    b.set(
        field(Q, offset_of!(AllocationSample, total_bytes), 0),
        local(REG),
    );
    b.a.jmp_label(statuses[0]);
    b.a.bind_text_label(baseline);
    b.set(field(Q, offset_of!(AllocationSample, context), 0), local(G));
    b.set(
        field(Q, offset_of!(AllocationSample, identity), 0),
        field(G, offset_of!(MutatorContext, thread_id), 0),
    );
    b.set(
        field(Q, offset_of!(AllocationSample, timestamp_ns), 0),
        local(OP),
    );
    b.set(
        field(Q, offset_of!(AllocationSample, total_bytes), 0),
        local(REG),
    );
    b.a.jmp_label(statuses[1]);
    for (value, label) in statuses.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}

/// V7=sample,V6=usable headroom,V2=horizon ns. Exact full-width products.
#[allow(dead_code)]
pub(crate) fn emit_pressure<A: PortableAsm>(a: &mut A) {
    let quiet = a.create_text_label();
    let request = a.create_text_label();
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    b.pointer(field(G, offset_of!(AllocationSample, context), 0), invalid);
    b.branch(
        field(G, offset_of!(AllocationSample, identity), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(G, offset_of!(AllocationSample, elapsed_ns), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(local(OP), n(0), C::Equal, invalid);
    b.branch(
        field(G, offset_of!(AllocationSample, total_bytes), 0),
        n(u64::MAX),
        C::Equal,
        invalid,
    );
    b.branch(
        field(G, offset_of!(AllocationSample, delta_bytes), 0),
        field(G, offset_of!(AllocationSample, total_bytes), 0),
        C::Above,
        invalid,
    );
    b.branch(
        field(G, offset_of!(AllocationSample, elapsed_ns), 0),
        field(G, offset_of!(AllocationSample, timestamp_ns), 0),
        C::Above,
        invalid,
    );
    b.value(field(G, offset_of!(AllocationSample, delta_bytes), 0));
    b.a.load_rbp_slot(R::V1, OP);
    b.a.mul_unsigned_wide(R::V1);
    b.a.store_rbp_slot(STATE, R::V2);
    b.a.store_rbp_slot(USED, R::V0);
    b.value(field(G, offset_of!(AllocationSample, elapsed_ns), 0));
    b.a.load_rbp_slot(R::V1, Q);
    b.a.mul_unsigned_wide(R::V1);
    b.a.load_rbp_slot(R::V1, STATE);
    b.a.cmp_reg_reg(R::V1, R::V2);
    b.a.jcc_label(C::Above, request);
    b.a.jcc_label(C::Below, quiet);
    b.a.load_rbp_slot(R::V1, USED);
    b.a.cmp_reg_reg(R::V1, R::V0);
    b.a.jcc_label(C::AboveOrEqual, request);
    b.a.jmp_label(quiet);
    for (label, value) in [(quiet, 0), (request, 1), (invalid, 2)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
