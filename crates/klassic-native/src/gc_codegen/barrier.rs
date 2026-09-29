//! Guarded physical pre-barrier/CAS commit, matching gc_barrier.h.
use super::*;
use crate::gc_abi::{
    AllocationPage as P, BarrierEntry, BufferCursor as B, ForwardCell as F, GuardedStore as T,
    ObjectHeader as H, SourceAccessRequest as S,
};
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(T, $f), 0)
    };
}
macro_rules! scope {
    ($f:ident) => {
        field(G, offset_of!(S, $f), 0)
    };
}
macro_rules! page {
    ($f:ident) => {
        field(SEG, offset_of!(P, $f), 0)
    };
}
const SCAN: i32 = 192;
const SELECTED: i32 = 200;
const ENTRY: i32 = 208;
pub(super) fn reference<A: PortableAsm>(b: &mut Emit<'_, A>, v: E, invalid: A::TextLabel) {
    let done = b.a.create_text_label();
    b.branch(v.clone(), n(0), C::Equal, done);
    b.branch(shr(v.clone(), 16), n(0), C::Equal, invalid);
    b.branch(and(v.clone(), n(0xf00cf)), n(0), C::NotEqual, invalid);
    b.branch(and(v.clone(), n(0x30)), n(0), C::Equal, invalid);
    for mask in [0x300, 0xc00, 0xf000] {
        b.set(local(KIND), and(v.clone(), n(mask)));
        b.branch(local(KIND), n(0), C::Equal, invalid);
        b.branch(
            and(local(KIND), sub(local(KIND), n(1))),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.a.bind_text_label(done);
}
fn buffer_bytes(v: E) -> E {
    add(shl(v.clone(), 5), shl(v, 4))
}
// Leaves scope/page/context/header in G/SEG/LEASE/REG. No address escapes.
pub(super) fn guarded_object<A: PortableAsm>(
    b: &mut Emit<'_, A>,
    scope_arg: E,
    page_arg: E,
    phase: E,
    invalid: A::TextLabel,
) {
    guarded_object_state(b, scope_arg, page_arg, phase, invalid, false);
}
pub(super) fn guarded_object_state<A: PortableAsm>(
    b: &mut Emit<'_, A>,
    scope_arg: E,
    page_arg: E,
    phase: E,
    invalid: A::TextLabel,
    allow_initializing: bool,
) {
    let word_ok = b.a.create_text_label();
    let page_ok = b.a.create_text_label();
    for v in [scope_arg.clone(), page_arg.clone()] {
        b.pointer(v, invalid);
    }
    b.branch(and(phase.clone(), n(0xe000)), n(0), C::NotEqual, invalid);
    b.branch(and(phase.clone(), n(15)), n(7), C::Above, invalid);
    b.branch(and(shr(phase.clone(), 4), n(15)), n(7), C::Above, invalid);
    b.set(local(G), scope_arg);
    b.pointer(scope!(context), invalid);
    b.pointer(scope!(held), invalid);
    b.branch(scope!(ticket), n(0), C::NotEqual, invalid);
    b.branch(scope!(incarnation), n(0), C::Equal, invalid);
    b.set(local(LEASE), scope!(context));
    b.branch(
        field(LEASE, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(LEASE, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(LEASE, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(LEASE, offset_of!(MutatorContext, access_epoch), 8),
        scope!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(LEASE, offset_of!(MutatorContext, adopted_phase), 8),
        phase.clone(),
        C::NotEqual,
        invalid,
    );
    b.set(local(REG), scope!(held));
    b.branch(
        field(REG, offset_of!(F, source_offset), 0),
        scope!(source_offset),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(REG, offset_of!(F, incarnation), 0),
        scope!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.branch(
        and(
            field(REG, offset_of!(F, access), 8),
            n(!crate::gc_abi::SOURCE_CLOSED),
        ),
        n(0),
        C::Equal,
        invalid,
    );
    b.set(local(STATE), field(REG, offset_of!(F, word), 8));
    b.branch(local(STATE), n(0), C::Equal, word_ok);
    b.branch(local(STATE), n(3), C::Equal, word_ok);
    b.branch(and(local(STATE), n(3)), n(1), C::NotEqual, invalid);
    b.branch(shr(local(STATE), 2), n(0), C::Equal, invalid);
    b.a.bind_text_label(word_ok);
    b.set(local(SEG), page_arg);
    b.set(local(STATE), field(SEG, offset_of!(P, state), 8));
    b.set(local(USED), field(SEG, offset_of!(P, allocated_top), 8));
    b.branch(local(STATE), n(1), C::Equal, page_ok);
    b.branch(local(STATE), n(5), C::NotEqual, invalid);
    b.a.bind_text_label(page_ok);
    b.branch(
        page!(incarnation),
        scope!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.branch(page!(generation), n(1), C::Above, invalid);
    b.branch(page!(start), n(0), C::Equal, invalid);
    b.branch(page!(start), page!(limit), C::AboveOrEqual, invalid);
    b.branch(
        page!(limit),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    for v in [
        page!(heap_base),
        page!(start),
        page!(limit),
        local(USED),
        scope!(source_offset),
    ] {
        b.branch(and(v, n(15)), n(0), C::NotEqual, invalid);
    }
    b.branch(
        page!(heap_base),
        sub(n(u64::MAX), page!(limit)),
        C::Above,
        invalid,
    );
    b.branch(local(USED), page!(start), C::Below, invalid);
    b.branch(local(USED), page!(limit), C::Above, invalid);
    b.branch(
        scope!(source_offset),
        add(page!(start), n(32)),
        C::Below,
        invalid,
    );
    b.branch(scope!(source_offset), local(USED), C::AboveOrEqual, invalid);
    b.set(
        local(REG),
        sub(add(page!(heap_base), scope!(source_offset)), n(32)),
    );
    if allow_initializing {
        b.set(local(KIND), field(REG, offset_of!(H, state), 8));
        b.branch(local(KIND), n(1), C::Below, invalid);
        b.branch(local(KIND), n(2), C::Above, invalid);
    } else {
        b.branch(
            field(REG, offset_of!(H, state), 8),
            n(2),
            C::NotEqual,
            invalid,
        );
    }

    b.branch(
        field(REG, offset_of!(H, payload_bytes), 0),
        sub(local(USED), scope!(source_offset)),
        C::Above,
        invalid,
    );
}
// The first four request fields have the same offsets for loads and stores.
fn guarded_slot<A: PortableAsm>(
    b: &mut Emit<'_, A>,
    phase: E,
    invalid: A::TextLabel,
    scan: A::TextLabel,
    reference_slot: bool,
) {
    use crate::gc_abi::GuardedLoad as L;
    const {
        assert!(offset_of!(L, scope) == offset_of!(T, scope));
        assert!(offset_of!(L, page) == offset_of!(T, page));
        assert!(offset_of!(L, registry) == offset_of!(T, registry));
        assert!(offset_of!(L, byte_offset) == offset_of!(T, byte_offset));
    }
    b.pointer(req!(registry), invalid);
    b.branch(and(req!(byte_offset), n(7)), n(0), C::NotEqual, invalid);
    guarded_object(b, req!(scope), req!(page), phase, invalid);
    b.branch(
        req!(byte_offset),
        field(REG, offset_of!(H, payload_bytes), 0),
        C::Above,
        invalid,
    );
    b.branch(
        sub(
            field(REG, offset_of!(H, payload_bytes), 0),
            req!(byte_offset),
        ),
        n(8),
        C::Below,
        invalid,
    );
    // DescriptorScan at FP-192 and its single output at FP-200.
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, SCAN);
    b.a.store_rbp_slot(CTX, R::V0);
    b.set(
        field(CTX, 0, 0),
        field(REG, offset_of!(H, descriptor_id), 0),
    );
    b.set(
        field(CTX, 8, 0),
        field(REG, offset_of!(H, payload_bytes), 0),
    );
    b.set(field(CTX, 16, 0), req!(byte_offset));
    b.set(field(CTX, 24, 0), n(1));
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, SELECTED);
    b.a.store_rbp_slot(KIND, R::V0);
    b.set(field(CTX, 32, 0), local(KIND));
    b.set(field(CTX, 40, 0), n(1));
    b.set(field(CTX, 48, 0), n(0));
    b.set(field(CTX, 56, 0), n(0));
    b.set(local(SELECTED), n(u64::MAX));
    b.value(req!(registry));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, CTX);
    b.a.call_label(scan);
    b.a.cmp_reg_imm32(R::V0, 2);
    b.a.jcc_label(C::Equal, invalid);
    if reference_slot {
        b.branch(field(CTX, 48, 0), n(1), C::NotEqual, invalid);
        b.branch(local(SELECTED), req!(byte_offset), C::NotEqual, invalid);
    } else {
        b.branch(local(SELECTED), req!(byte_offset), C::Equal, invalid);
    }
    b.set(
        local(NEXT),
        add(
            add(page!(heap_base), scope!(source_offset)),
            req!(byte_offset),
        ),
    );
}

#[allow(dead_code)]
pub(crate) fn emit_guarded_store<A: PortableAsm>(a: &mut A) {
    emit_store_impl(a, false);
}
#[allow(dead_code)]
pub(crate) fn emit_logical_cas<A: PortableAsm>(a: &mut A) {
    emit_store_impl(a, true);
}
fn emit_store_impl<A: PortableAsm>(a: &mut A, logical: bool) {
    let statuses: [_; 6] = std::array::from_fn(|_| a.create_text_label());
    let invalid = statuses[1];
    let color_done = a.create_text_label();
    let marking = a.create_text_label();
    let mark_done = a.create_text_label();
    let mask_done = a.create_text_label();
    let leases_ok = a.create_text_label();
    let scan = a.create_text_label();
    let after = a.create_text_label();
    let done = a.create_text_label();
    // Pair full fences around logical comparison, including the MISMATCH
    // acquire-only path. The shared CAS primitive is otherwise only AcqRel.
    if logical {
        a.atomic_fence();
    }
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    if logical {
        a.sub_reg_imm8(R::V4, 32);
        a.store_rbp_slot(232, R::V6);
        a.store_rbp_slot(240, R::V2);
    }
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    if logical {
        b.pointer(local(Q), invalid);
    }
    for v in [req!(scope), req!(page), req!(registry)] {
        b.pointer(v, invalid);
    }
    b.branch(req!(transferred), n(0), C::NotEqual, invalid);
    b.branch(and(req!(byte_offset), n(7)), n(0), C::NotEqual, invalid);
    reference(&mut b, req!(expected), invalid);
    reference(&mut b, req!(desired), invalid);
    b.branch(and(req!(phase), n(0xe000)), n(0), C::NotEqual, invalid);
    b.branch(and(req!(phase), n(15)), n(7), C::Above, invalid);
    b.branch(and(shr(req!(phase), 4), n(15)), n(7), C::Above, invalid);
    b.branch(req!(desired), n(0), C::Equal, color_done);
    b.set(local(STATE), n(0));
    for (shift, mask, bias) in [(8, 1, 8), (9, 1, 10), (10, 3, 12), (12, 1, 4)] {
        b.value(add(and(shr(req!(phase), shift), n(mask)), n(bias)));
        b.a.mov_reg_reg(R::V1, R::V0);
        b.a.mov_imm64(R::V2, 1);
        b.a.shl_reg_count(R::V2);
        b.a.store_rbp_slot(KIND, R::V2);
        b.set(local(STATE), add(local(STATE), local(KIND)));
    }
    b.branch(
        and(req!(desired), n(0xffff)),
        local(STATE),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(color_done);
    guarded_slot(&mut b, req!(phase), invalid, scan, true);
    if logical {
        for slot in [232, 240] {
            let valid = b.a.create_text_label();
            b.branch(local(slot), n(0), C::Equal, valid);
            b.pointer(local(slot), invalid);
            b.branch(
                field(slot, offset_of!(crate::gc_abi::MarkRecord, object_id), 0),
                n(0),
                C::Equal,
                invalid,
            );
            b.a.bind_text_label(valid);
        }
        let null_observation = b.a.create_text_label();
        let observation_valid = b.a.create_text_label();
        b.branch(local(240), n(0), C::Equal, null_observation);
        b.branch(req!(expected), n(0), C::Equal, invalid);
        b.a.jmp_label(observation_valid);
        b.a.bind_text_label(null_observation);
        b.branch(req!(expected), n(0), C::NotEqual, invalid);
        b.a.bind_text_label(observation_valid);
        b.set(
            local(NEXT),
            add(
                add(page!(heap_base), scope!(source_offset)),
                req!(byte_offset),
            ),
        );
        b.branch(field(NEXT, 0, 8), req!(expected), C::NotEqual, statuses[4]);
        b.branch(local(232), local(240), C::NotEqual, statuses[5]);
    }
    b.set(local(PREV), n(0));
    b.branch(req!(expected), n(0), C::Equal, mark_done);
    for shift in [0, 4] {
        let next = b.a.create_text_label();
        b.set(local(KIND), and(shr(req!(phase), shift), n(15)));
        b.branch(local(KIND), n(1), C::Below, next);
        b.branch(local(KIND), n(3), C::Above, next);
        b.a.jmp_label(marking);
        b.a.bind_text_label(next);
    }
    b.a.jmp_label(mark_done);
    b.a.bind_text_label(marking);
    b.set(local(PREV), n(1));
    b.a.bind_text_label(mark_done);
    b.branch(page!(generation), n(1), C::NotEqual, mask_done);
    b.set(local(PREV), add(local(PREV), n(2)));
    b.a.bind_text_label(mask_done);
    b.branch(local(PREV), n(3), C::NotEqual, leases_ok);
    b.branch(
        field(Q, offset_of!(T, satb) + 40, 0),
        field(Q, offset_of!(T, remembered) + 40, 0),
        C::Equal,
        invalid,
    );
    b.a.bind_text_label(leases_ok);
    for (bit, entry_off, buffer_off) in [
        (1, offset_of!(T, satb), offset_of!(MutatorContext, satb)),
        (
            2,
            offset_of!(T, remembered),
            offset_of!(MutatorContext, remembered),
        ),
    ] {
        let skip = b.a.create_text_label();
        b.branch(and(local(PREV), n(bit)), n(0), C::Equal, skip);
        b.set(local(ENTRY), add(local(Q), n(entry_off as u64)));
        b.set(local(CTX), add(local(LEASE), n(buffer_off as u64)));
        b.branch(field(ENTRY, 0, 0), req!(expected), C::NotEqual, invalid);
        b.branch(field(ENTRY, 8, 0), n(0), C::Equal, invalid);
        reference(&mut b, field(ENTRY, 8, 0), invalid);
        b.branch(
            shr(field(ENTRY, 8, 0), 16),
            scope!(source_offset),
            C::NotEqual,
            invalid,
        );
        b.branch(field(ENTRY, 16, 0), req!(byte_offset), C::NotEqual, invalid);
        b.branch(
            field(ENTRY, 24, 0),
            page!(incarnation),
            C::NotEqual,
            invalid,
        );
        b.branch(field(ENTRY, 32, 0), req!(phase), C::NotEqual, invalid);
        b.branch(field(ENTRY, 40, 0), n(0), C::Equal, invalid);
        b.pointer(field(CTX, offset_of!(B, entries), 0), invalid);
        b.branch(
            field(CTX, offset_of!(B, capacity), 0),
            n(0),
            C::Equal,
            invalid,
        );
        b.branch(
            field(CTX, offset_of!(B, capacity), 0),
            n(u32::MAX as u64),
            C::Above,
            invalid,
        );
        b.branch(
            field(CTX, offset_of!(B, entries), 0),
            sub(
                n(u64::MAX),
                buffer_bytes(field(CTX, offset_of!(B, capacity), 0)),
            ),
            C::Above,
            invalid,
        );
        b.set(local(STATE), field(CTX, offset_of!(B, published), 8));
        b.set(local(USED), field(CTX, offset_of!(B, consumed), 8));
        b.branch(
            local(STATE),
            field(CTX, offset_of!(B, top), 0),
            C::Above,
            invalid,
        );
        b.branch(local(USED), local(STATE), C::Above, invalid);
        b.branch(
            sub(field(CTX, offset_of!(B, top), 0), local(USED)),
            field(CTX, offset_of!(B, capacity), 0),
            C::Above,
            invalid,
        );
        b.branch(
            field(CTX, offset_of!(B, top), 0),
            n(u64::MAX),
            C::Equal,
            statuses[3],
        );
        b.branch(
            sub(field(CTX, offset_of!(B, top), 0), local(USED)),
            field(CTX, offset_of!(B, capacity), 0),
            C::Equal,
            statuses[2],
        );
        b.a.bind_text_label(skip);
    }
    b.set(
        local(NEXT),
        add(
            add(page!(heap_base), scope!(source_offset)),
            req!(byte_offset),
        ),
    );
    b.branch(field(NEXT, 0, 8), req!(expected), C::NotEqual, statuses[4]);
    for (bit, entry_off, buffer_off) in [
        (1, offset_of!(T, satb), offset_of!(MutatorContext, satb)),
        (
            2,
            offset_of!(T, remembered),
            offset_of!(MutatorContext, remembered),
        ),
    ] {
        let skip = b.a.create_text_label();
        b.branch(and(local(PREV), n(bit)), n(0), C::Equal, skip);
        b.set(local(CTX), add(local(LEASE), n(buffer_off as u64)));
        b.set(local(USED), field(CTX, offset_of!(B, top), 0));
        b.value(field(CTX, offset_of!(B, capacity), 0));
        b.a.mov_reg_reg(R::V1, R::V0);
        b.a.load_rbp_slot(R::V0, USED);
        b.a.div_unsigned(R::V1);
        b.a.store_rbp_slot(KIND, R::V2);
        b.set(
            local(REG),
            add(
                field(CTX, offset_of!(B, entries), 0),
                buffer_bytes(local(KIND)),
            ),
        );
        for off in (0..std::mem::size_of::<BarrierEntry>()).step_by(8) {
            b.set(field(REG, off, 0), field(Q, entry_off + off, 0));
        }
        b.set(field(CTX, offset_of!(B, top), 0), add(local(USED), n(1)));
        b.set(
            field(CTX, offset_of!(B, published), 8),
            add(local(USED), n(1)),
        );
        b.a.bind_text_label(skip);
    }
    b.set(req!(transferred), local(PREV));
    b.value(req!(desired));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.value(req!(expected));
    b.a.load_rbp_slot(R::V11, NEXT);
    b.a.atomic_compare_exchange(R::V11, R::V2);
    b.a.store_rbp_slot(KIND, R::V0);
    b.branch(local(KIND), req!(expected), C::NotEqual, statuses[4]);
    b.a.jmp_label(statuses[0]);
    for (value, label) in statuses.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    if logical {
        b.a.atomic_fence();
    }
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(scan);
    crate::gc_codegen::descriptors::emit_scan(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}

/// Atomically acquire a guarded reference slot into a prepared output root.
#[allow(dead_code)]
pub(crate) fn emit_guarded_load<A: PortableAsm>(a: &mut A) {
    emit_load_impl(a, false);
}
#[allow(dead_code)]
pub(crate) fn emit_guarded_observe<A: PortableAsm>(a: &mut A) {
    emit_load_impl(a, true);
}
fn emit_load_impl<A: PortableAsm>(a: &mut A, observe: bool) {
    use crate::gc_abi::{GuardedLoad as L, RootRequest};
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let scan = a.create_text_label();
    let root = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    if observe {
        b.pointer(local(Q), invalid);
    }
    b.branch(
        field(Q, offset_of!(L, output) + offset_of!(RootRequest, value), 0),
        n(0),
        C::NotEqual,
        invalid,
    );
    guarded_slot(
        &mut b,
        field(Q, offset_of!(L, phase), 0),
        invalid,
        scan,
        true,
    );
    b.set(local(STATE), field(NEXT, 0, 8));
    reference(&mut b, local(STATE), invalid);
    // Reuse the descriptor request scratch, after its scan has completed.
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, SCAN);
    b.a.store_rbp_slot(REG, R::V0);
    for off in (0..std::mem::size_of::<RootRequest>()).step_by(8) {
        b.set(field(REG, off, 0), field(Q, offset_of!(L, output) + off, 0));
    }
    b.set(field(REG, offset_of!(RootRequest, value), 0), local(STATE));
    b.a.load_rbp_slot(R::V7, LEASE);
    b.a.load_rbp_slot(R::V6, REG);
    b.a.call_label(root);
    if observe {
        b.a.cmp_reg_imm8(R::V0, 0);
        b.a.jcc_label(C::NotEqual, done);
        b.set(
            field(Q, offset_of!(L, output) + offset_of!(RootRequest, value), 0),
            local(STATE),
        );
        b.a.mov_imm64(R::V0, 0);
    }
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(scan);
    crate::gc_codegen::descriptors::emit_scan(b.a);
    b.a.ret();
    b.a.bind_text_label(root);
    crate::gc_codegen::roots::emit_guarded_store(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}

/// Exact-observation repair; logical identity/protection are caller obligations.
#[allow(dead_code)]
pub(crate) fn emit_guarded_heal<A: PortableAsm>(a: &mut A) {
    use crate::gc_abi::GuardedHeal as Heal;
    const {
        assert!(offset_of!(Heal, scope) == offset_of!(T, scope));
        assert!(offset_of!(Heal, page) == offset_of!(T, page));
        assert!(offset_of!(Heal, registry) == offset_of!(T, registry));
        assert!(offset_of!(Heal, byte_offset) == offset_of!(T, byte_offset));
        assert!(offset_of!(Heal, expected) == offset_of!(T, expected));
        assert!(offset_of!(Heal, desired) == offset_of!(T, desired));
        assert!(offset_of!(Heal, phase) == offset_of!(T, phase));
    }
    let statuses = [
        a.create_text_label(),
        a.create_text_label(),
        a.create_text_label(),
    ];
    let invalid = statuses[1];
    let done = a.create_text_label();
    let scan = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    for v in [req!(expected), req!(desired)] {
        b.branch(v.clone(), n(0), C::Equal, invalid);
        reference(&mut b, v, invalid);
    }
    guarded_slot(&mut b, req!(phase), invalid, scan, true);
    b.set(local(STATE), n(0));
    for (shift, mask, bias) in [(8, 1, 8), (9, 1, 10), (10, 3, 12), (12, 1, 4)] {
        b.value(add(and(shr(req!(phase), shift), n(mask)), n(bias)));
        b.a.mov_reg_reg(R::V1, R::V0);
        b.a.mov_imm64(R::V2, 1);
        b.a.shl_reg_count(R::V2);
        b.a.store_rbp_slot(KIND, R::V2);
        b.set(local(STATE), add(local(STATE), local(KIND)));
    }
    // Repair must not manufacture the current remembered-set stamp.
    b.set(
        local(STATE),
        add(and(local(STATE), n(!0x30u64)), and(req!(expected), n(0x30))),
    );
    b.branch(
        and(req!(desired), n(0xffff)),
        local(STATE),
        C::NotEqual,
        invalid,
    );
    b.value(req!(desired));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.value(req!(expected));
    b.a.load_rbp_slot(R::V11, NEXT);
    b.a.atomic_compare_exchange(R::V11, R::V2);
    b.a.store_rbp_slot(KIND, R::V0);
    b.branch(local(KIND), req!(expected), C::NotEqual, statuses[2]);
    b.a.jmp_label(statuses[0]);
    for (value, label) in statuses.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(scan);
    crate::gc_codegen::descriptors::emit_scan(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}

/// V7=GuardedScalar, V6=LOAD/STORE. Scalar bits never enter GC reference paths.
#[allow(dead_code)]
pub(crate) fn emit_guarded_scalar<A: PortableAsm>(a: &mut A) {
    use crate::gc_abi::GuardedScalar as S;
    const {
        assert!(offset_of!(S, scope) == offset_of!(T, scope));
        assert!(offset_of!(S, page) == offset_of!(T, page));
        assert!(offset_of!(S, registry) == offset_of!(T, registry));
        assert!(offset_of!(S, byte_offset) == offset_of!(T, byte_offset));
    }
    let bad = a.create_text_label();
    let store = a.create_text_label();
    let success = a.create_text_label();
    let done = a.create_text_label();
    let scan = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(OP, R::V6);
    let mut b = Emit { a };
    b.pointer(local(Q), bad);
    b.branch(local(OP), n(1), C::Above, bad);
    guarded_slot(&mut b, field(Q, offset_of!(S, phase), 0), bad, scan, false);
    b.branch(local(OP), n(1), C::Equal, store);
    b.set(field(Q, offset_of!(S, value), 0), field(NEXT, 0, 8));
    b.a.jmp_label(success);
    b.a.bind_text_label(store);
    b.set(field(NEXT, 0, 8), field(Q, offset_of!(S, value), 0));
    b.a.bind_text_label(success);
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

/// V7=scope, V6=page, V2=adopted phase; returns borrowed record or zero.
#[allow(dead_code)] // Ordinary compiler activation shares the pending GC ABI migration.
pub(crate) fn emit_guarded_identity<A: PortableAsm>(a: &mut A) {
    let bad = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(OP, R::V6);
    a.store_rbp_slot(PREV, R::V2);
    let mut b = Emit { a };
    guarded_object(&mut b, local(Q), local(OP), local(PREV), bad);
    b.set(local(NEXT), field(REG, offset_of!(H, mark_record), 0));
    b.pointer(local(NEXT), bad);
    b.branch(
        field(NEXT, offset_of!(crate::gc_abi::MarkRecord, object_id), 0),
        n(0),
        C::Equal,
        bad,
    );
    b.a.load_rbp_slot(R::V0, NEXT);
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 0);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
}

/// Return allocator-compatible physical bytes only while guarded.
#[allow(dead_code)]
pub(crate) fn emit_guarded_block_size<A: PortableAsm>(a: &mut A) {
    let bad = a.create_text_label();
    let nonzero = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(OP, R::V6);
    a.store_rbp_slot(PREV, R::V2);
    let mut b = Emit { a };
    guarded_object(&mut b, local(Q), local(OP), local(PREV), bad);
    b.set(
        local(NEXT),
        and(
            add(field(REG, offset_of!(H, payload_bytes), 0), n(15)),
            n(!15),
        ),
    );
    b.branch(local(NEXT), n(0), C::NotEqual, nonzero);
    b.set(local(NEXT), n(16));
    b.a.bind_text_label(nonzero);
    b.value(add(local(NEXT), n(std::mem::size_of::<H>() as u64)));
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 0);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
}
