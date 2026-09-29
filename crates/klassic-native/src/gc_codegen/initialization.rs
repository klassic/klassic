//! Guarded readiness publication; matches gc_initialization.h.
use super::*;
use crate::gc_abi::{AllocationPage, ForwardCell, ObjectHeader, SourceAccessRequest};
#[allow(dead_code)]
pub(crate) fn emit_publish<A: PortableAsm>(a: &mut A) {
    let bad = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    validate_owned(&mut b, bad);
    b.set(field(REG, offset_of!(ObjectHeader, state), 8), n(2));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
}

fn validate_owned<A: PortableAsm>(b: &mut Emit<'_, A>, bad: A::TextLabel) {
    b.pointer(local(Q), bad);
    super::mark_prepare::validate_state(b, bad, true);
    b.branch(local(KIND), n(1), C::NotEqual, bad);
    b.branch(
        field(SEG, offset_of!(AllocationPage, generation), 0),
        n(0),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(SEG, offset_of!(AllocationPage, owner), 0),
        field(LEASE, offset_of!(MutatorContext, thread_id), 0),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(SEG, offset_of!(AllocationPage, state), 8),
        n(1),
        C::NotEqual,
        bad,
    );
    b.set(
        local(NEXT),
        field(G, offset_of!(SourceAccessRequest, held), 0),
    );
    b.branch(
        field(NEXT, offset_of!(ForwardCell, word), 8),
        n(0),
        C::NotEqual,
        bad,
    );
    b.branch(
        and(
            field(NEXT, offset_of!(ForwardCell, access), 8),
            n(1u64 << 63),
        ),
        n(0),
        C::NotEqual,
        bad,
    );
}

/// V7=GuardedMarkPrepare, V6=InitializationField; one guarded private word.
#[allow(dead_code)]
pub(crate) fn emit_store<A: PortableAsm>(a: &mut A) {
    use crate::gc_abi::{GuardedMarkPrepare, InitializationField as F};
    let bad = a.create_text_label();
    let done = a.create_text_label();
    let scalar = a.create_text_label();
    let store = a.create_text_label();
    let scan = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 96);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(OP, R::V6);
    let mut b = Emit { a };
    b.pointer(local(OP), bad);
    b.pointer(field(OP, offset_of!(F, registry), 0), bad);
    b.branch(
        and(field(OP, offset_of!(F, byte_offset), 0), n(7)),
        n(0),
        C::NotEqual,
        bad,
    );
    b.branch(
        field(OP, offset_of!(F, is_reference), 0),
        n(1),
        C::Above,
        bad,
    );
    validate_owned(&mut b, bad);
    b.branch(
        field(OP, offset_of!(F, byte_offset), 0),
        field(REG, offset_of!(ObjectHeader, payload_bytes), 0),
        C::Above,
        bad,
    );
    b.branch(
        sub(
            field(REG, offset_of!(ObjectHeader, payload_bytes), 0),
            field(OP, offset_of!(F, byte_offset), 0),
        ),
        n(8),
        C::Below,
        bad,
    );
    // Bounded descriptor scan at FP-192; its single output at FP-200.
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 192);
    b.a.store_rbp_slot(CTX, R::V0);
    b.set(
        field(CTX, 0, 0),
        field(REG, offset_of!(ObjectHeader, descriptor_id), 0),
    );
    b.set(
        field(CTX, 8, 0),
        field(REG, offset_of!(ObjectHeader, payload_bytes), 0),
    );
    b.set(field(CTX, 16, 0), field(OP, offset_of!(F, byte_offset), 0));
    b.set(field(CTX, 24, 0), n(1));
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 200);
    b.a.store_rbp_slot(KIND, R::V0);
    b.set(field(CTX, 32, 0), local(KIND));
    b.set(field(CTX, 40, 0), n(1));
    b.set(field(CTX, 48, 0), n(0));
    b.set(field(CTX, 56, 0), n(0));
    b.set(local(200), n(u64::MAX));
    b.value(field(OP, offset_of!(F, registry), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, CTX);
    b.a.call_label(scan);
    b.a.cmp_reg_imm8(R::V0, 2);
    b.a.jcc_label(C::Equal, bad);
    b.set(
        local(NEXT),
        add(
            add(local(REG), n(32)),
            field(OP, offset_of!(F, byte_offset), 0),
        ),
    );
    b.branch(
        field(OP, offset_of!(F, is_reference), 0),
        n(0),
        C::Equal,
        scalar,
    );
    b.branch(field(CTX, 48, 0), n(1), C::NotEqual, bad);
    b.branch(
        local(200),
        field(OP, offset_of!(F, byte_offset), 0),
        C::NotEqual,
        bad,
    );
    b.branch(field(NEXT, 0, 8), n(0), C::NotEqual, bad);
    super::barrier::reference(&mut b, field(OP, offset_of!(F, value), 0), bad);
    b.branch(field(OP, offset_of!(F, value), 0), n(0), C::Equal, store);
    b.set(local(STATE), n(0));
    for (shift, mask, bias) in [(8, 1, 8), (9, 1, 10), (10, 3, 12), (12, 1, 4)] {
        b.value(add(
            and(
                shr(field(Q, offset_of!(GuardedMarkPrepare, phase), 0), shift),
                n(mask),
            ),
            n(bias),
        ));
        b.a.mov_reg_reg(R::V1, R::V0);
        b.a.mov_imm64(R::V2, 1);
        b.a.shl_reg_count(R::V2);
        b.a.store_rbp_slot(KIND, R::V2);
        b.set(local(STATE), add(local(STATE), local(KIND)));
    }
    b.branch(
        and(field(OP, offset_of!(F, value), 0), n(0xffff)),
        local(STATE),
        C::NotEqual,
        bad,
    );
    b.a.jmp_label(store);
    b.a.bind_text_label(scalar);
    b.branch(
        local(200),
        field(OP, offset_of!(F, byte_offset), 0),
        C::Equal,
        bad,
    );
    b.a.bind_text_label(store);
    b.set(field(NEXT, 0, 8), field(OP, offset_of!(F, value), 0));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(scan);
    crate::gc_codegen::descriptors::emit_scan(b.a);
    b.a.ret();
}

/// Bounded copy from immutable native bytes into a scalar-only receiver.
#[allow(dead_code)]
pub(crate) fn emit_copy<A: PortableAsm>(a: &mut A) {
    use crate::gc_abi::{InitializationBytes as F, TypeDescriptor};
    let bad = a.create_text_label();
    let empty = a.create_text_label();
    let done = a.create_text_label();
    let shape = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 96);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(OP, R::V6);
    let mut b = Emit { a };
    b.pointer(local(OP), bad);
    b.branch(field(OP, offset_of!(F, bytes), 0), n(256), C::Above, bad);
    b.branch(field(OP, offset_of!(F, bytes), 0), n(0), C::Equal, empty);
    b.branch(field(OP, offset_of!(F, source), 0), n(0), C::Equal, bad);
    b.a.bind_text_label(empty);
    b.branch(
        add(
            field(OP, offset_of!(F, source), 0),
            field(OP, offset_of!(F, bytes), 0),
        ),
        field(OP, offset_of!(F, source), 0),
        C::Below,
        bad,
    );
    validate_owned(&mut b, bad);
    b.branch(
        field(OP, offset_of!(F, byte_offset), 0),
        field(REG, offset_of!(ObjectHeader, payload_bytes), 0),
        C::Above,
        bad,
    );
    b.branch(
        field(OP, offset_of!(F, bytes), 0),
        sub(
            field(REG, offset_of!(ObjectHeader, payload_bytes), 0),
            field(OP, offset_of!(F, byte_offset), 0),
        ),
        C::Above,
        bad,
    );
    // Private AllocationRequest at FP-160; AllocationShape at FP-192.
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 160);
    b.a.store_rbp_slot(CTX, R::V0);
    for off in (0..40).step_by(8) {
        b.set(field(CTX, off, 0), n(0));
    }
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 192);
    b.a.store_rbp_slot(STATE, R::V0);
    b.set(
        field(STATE, 0, 0),
        field(REG, offset_of!(ObjectHeader, descriptor_id), 0),
    );
    b.set(
        field(STATE, 8, 0),
        field(REG, offset_of!(ObjectHeader, payload_bytes), 0),
    );
    b.set(field(STATE, 16, 0), local(CTX));
    b.value(field(OP, offset_of!(F, registry), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, STATE);
    b.a.call_label(shape);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, bad);
    b.set(local(NEXT), field(CTX, 8, 0));
    for off in [
        offset_of!(TypeDescriptor, fixed_bitmap),
        offset_of!(TypeDescriptor, element_bitmap),
        offset_of!(TypeDescriptor, fixed_bitmap_words),
        offset_of!(TypeDescriptor, element_bitmap_words),
    ] {
        b.branch(field(NEXT, off, 0), n(0), C::NotEqual, bad);
    }
    b.value(add(
        add(local(REG), n(32)),
        field(OP, offset_of!(F, byte_offset), 0),
    ));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(field(OP, offset_of!(F, source), 0));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(field(OP, offset_of!(F, bytes), 0));
    b.a.mov_reg_reg(R::V1, R::V0);
    b.a.rep_movsb();
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(shape);
    crate::gc_codegen::descriptors::emit_allocation_shape(b.a);
    b.a.ret();
}
