//! Paired private startup admission; no native mapping or publication.
use super::*;
use crate::gc_abi::{
    DescriptorRegistry, MetadataCharge, NativeStartupRequest, RootCell, RootChange, RootStorage,
};
use std::mem::size_of;

#[allow(dead_code)]
pub(crate) fn emit_reserve<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let budget = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 48);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.set(
        local(REG),
        field(Q, offset_of!(NativeStartupRequest, roots), 0),
    );
    b.set(
        local(SEG),
        field(Q, offset_of!(NativeStartupRequest, descriptors), 0),
    );
    b.pointer(local(REG), invalid);
    b.pointer(local(SEG), invalid);
    b.branch(local(REG), local(SEG), C::Equal, invalid);
    for base in [REG, SEG] {
        for off in [
            offset_of!(MetadataCharge, bytes),
            offset_of!(MetadataCharge, owner),
            offset_of!(MetadataCharge, allocation),
            offset_of!(MetadataCharge, stage),
        ] {
            b.branch(field(base, off, 0), n(0), C::NotEqual, invalid);
        }
    }
    for off in [
        offset_of!(NativeStartupRequest, cells),
        offset_of!(NativeStartupRequest, entries),
    ] {
        b.branch(field(Q, off, 0), n(0), C::Equal, invalid);
        b.branch(field(Q, off, 0), n(u32::MAX as u64), C::Above, invalid);
    }
    let capacity = field(Q, offset_of!(NativeStartupRequest, capacity), 0);
    b.branch(capacity.clone(), n(5), C::Below, invalid);
    b.branch(capacity.clone(), n(0x1_0000_0000), C::Above, invalid);
    b.set(
        local(LEASE),
        field(Q, offset_of!(NativeStartupRequest, quantum), 0),
    );
    b.branch(local(LEASE), n(8), C::Below, invalid);
    b.set(local(STATE), sub(local(LEASE), n(1)));
    b.branch(and(local(LEASE), local(STATE)), n(0), C::NotEqual, invalid);
    assert_eq!(size_of::<RootCell>(), 32);
    assert_eq!(size_of::<RootChange>(), 32);
    b.set(
        local(USED),
        add(
            n(size_of::<RootStorage>() as u64),
            shl(
                add(
                    field(Q, offset_of!(NativeStartupRequest, cells), 0),
                    field(Q, offset_of!(NativeStartupRequest, entries), 0),
                ),
                5,
            ),
        ),
    );
    // NativeBuiltinRegistry prefix = DescriptorRegistry + BuiltinDescriptors.
    b.set(
        local(CTX),
        add(
            n(
                (size_of::<DescriptorRegistry>() + size_of::<super::super::descriptors::Builtins>())
                    as u64,
            ),
            shl(capacity, 3),
        ),
    );
    for slot in [USED, CTX] {
        b.branch(
            local(slot),
            sub(n(u64::MAX), local(STATE)),
            C::Above,
            invalid,
        );
        b.set(
            local(slot),
            and(
                add(local(slot), local(STATE)),
                sub(n(u64::MAX), local(STATE)),
            ),
        );
    }
    b.branch(local(USED), sub(n(u64::MAX), local(CTX)), C::Above, invalid);
    b.set(local(144), add(local(USED), local(CTX)));
    for slot in [136, 128, 120] {
        b.set(local(slot), n(0));
    }
    b.a.load_rbp_slot(R::V7, G);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, 144);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(budget);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, done);
    for (base, bytes) in [(REG, USED), (SEG, CTX)] {
        b.set(
            field(base, offset_of!(MetadataCharge, bytes), 0),
            local(bytes),
        );
        b.set(field(base, offset_of!(MetadataCharge, owner), 0), local(G));
        b.set(field(base, offset_of!(MetadataCharge, stage), 0), n(1));
    }
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(budget);
    super::budget::emit_budget_step(b.a);
    b.a.ret();
}

/// V7=immutable NativeStartupRequest, V6=mutator owner ID. Native mapping runs
/// outside locks/guards. Partial mappings remain owned on any failure.
#[allow(dead_code)]
pub(crate) fn emit_prepare<A: PortableAsm>(a: &mut A, quantum: u32, map: A::TextLabel) {
    assert!(quantum.is_power_of_two() && quantum >= 8);
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let roots = a.create_text_label();
    let descriptors = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 48);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(OP, R::V6);
    let mut b = Emit { a };
    b.pointer(local(Q), invalid);
    b.branch(local(OP), n(0), C::Equal, invalid);
    b.branch(
        field(Q, offset_of!(NativeStartupRequest, quantum), 0),
        n(quantum as u64),
        C::NotEqual,
        invalid,
    );
    b.set(
        local(REG),
        field(Q, offset_of!(NativeStartupRequest, roots), 0),
    );
    b.set(
        local(SEG),
        field(Q, offset_of!(NativeStartupRequest, descriptors), 0),
    );
    b.pointer(local(REG), invalid);
    b.pointer(local(SEG), invalid);
    b.branch(local(REG), local(SEG), C::Equal, invalid);
    for base in [REG, SEG] {
        b.pointer(field(base, offset_of!(MetadataCharge, owner), 0), invalid);
        b.branch(
            field(base, offset_of!(MetadataCharge, stage), 0),
            n(1),
            C::NotEqual,
            invalid,
        );
        b.branch(
            field(base, offset_of!(MetadataCharge, allocation), 0),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.branch(
        field(REG, offset_of!(MetadataCharge, owner), 0),
        field(SEG, offset_of!(MetadataCharge, owner), 0),
        C::NotEqual,
        invalid,
    );
    for off in [
        offset_of!(NativeStartupRequest, cells),
        offset_of!(NativeStartupRequest, entries),
    ] {
        b.branch(field(Q, off, 0), n(0), C::Equal, invalid);
        b.branch(field(Q, off, 0), n(u32::MAX as u64), C::Above, invalid);
    }
    let capacity = field(Q, offset_of!(NativeStartupRequest, capacity), 0);
    b.branch(capacity.clone(), n(5), C::Below, invalid);
    b.branch(capacity.clone(), n(0x1_0000_0000), C::Above, invalid);
    let round = |value: E| and(add(value, n(quantum as u64 - 1)), n(!(quantum as u64 - 1)));
    // Bounded u32 counts leave ample headroom for this target's quantum.
    b.set(
        local(USED),
        round(add(
            n(size_of::<RootStorage>() as u64),
            shl(
                add(
                    field(Q, offset_of!(NativeStartupRequest, cells), 0),
                    field(Q, offset_of!(NativeStartupRequest, entries), 0),
                ),
                5,
            ),
        )),
    );
    b.set(
        local(CTX),
        round(add(
            n(
                (size_of::<DescriptorRegistry>() + size_of::<super::super::descriptors::Builtins>())
                    as u64,
            ),
            shl(capacity, 3),
        )),
    );
    b.branch(
        field(REG, offset_of!(MetadataCharge, bytes), 0),
        local(USED),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(SEG, offset_of!(MetadataCharge, bytes), 0),
        local(CTX),
        C::NotEqual,
        invalid,
    );
    // Private RootStorageSpec at FP-144.
    b.set(local(144), local(OP));
    b.set(
        local(136),
        field(Q, offset_of!(NativeStartupRequest, cells), 0),
    );
    b.set(
        local(128),
        field(Q, offset_of!(NativeStartupRequest, entries), 0),
    );
    b.set(local(120), local(USED));
    b.a.load_rbp_slot(R::V7, REG);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, 144);
    b.a.call_label(roots);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, done);
    b.value(field(Q, offset_of!(NativeStartupRequest, capacity), 0));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.load_rbp_slot(R::V7, SEG);
    b.a.call_label(descriptors);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(roots);
    super::super::native_roots::emit_prepare(b.a, quantum, map);
    b.a.bind_text_label(descriptors);
    super::super::native_builtin_registry::emit_prepare(b.a, quantum, map);
}

/// Heap-lock protected paired commit; no mapping or publication. Only charge
/// pointers from the request are consumed, after successful private preparation.
#[allow(dead_code)]
pub(crate) fn emit_commit<A: PortableAsm>(a: &mut A) {
    emit_finish(a, true);
}
#[allow(dead_code)]
pub(crate) fn emit_release<A: PortableAsm>(a: &mut A) {
    emit_finish(a, false);
}
fn emit_finish<A: PortableAsm>(a: &mut A, commit: bool) {
    use crate::gc_abi::{AllocationPage, HeapPool};
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.set(
        local(REG),
        field(Q, offset_of!(NativeStartupRequest, roots), 0),
    );
    b.set(
        local(SEG),
        field(Q, offset_of!(NativeStartupRequest, descriptors), 0),
    );
    b.pointer(local(REG), invalid);
    b.pointer(local(SEG), invalid);
    b.branch(local(REG), local(SEG), C::Equal, invalid);
    let pages = field(G, offset_of!(HeapPool, page_count), 0);
    let committed = field(G, offset_of!(HeapPool, metadata_committed), 0);
    let pending = field(G, offset_of!(HeapPool, metadata_pending), 0);
    let maximum = field(G, offset_of!(HeapPool, metadata_max), 0);
    b.branch(pages.clone(), n(0), C::Equal, invalid);
    b.branch(
        pages.clone(),
        n(u64::MAX / size_of::<AllocationPage>() as u64),
        C::Above,
        invalid,
    );
    b.value(pages);
    b.a.mov_imm64(R::V1, size_of::<AllocationPage>() as u64);
    b.a.mul_unsigned_wide(R::V1);
    b.a.store_rbp_slot(CTX, R::V0);
    b.branch(committed.clone(), local(CTX), C::Below, invalid);
    b.branch(committed.clone(), maximum.clone(), C::Above, invalid);
    b.branch(
        pending.clone(),
        sub(maximum, committed.clone()),
        C::Above,
        invalid,
    );
    b.set(local(USED), pending.clone());
    if !commit {
        b.set(local(PREV), sub(committed.clone(), local(CTX)));
    }
    for base in [REG, SEG] {
        let bytes = field(base, offset_of!(MetadataCharge, bytes), 0);
        let allocation = field(base, offset_of!(MetadataCharge, allocation), 0);
        b.branch(bytes.clone(), n(0), C::Equal, invalid);
        b.branch(
            field(base, offset_of!(MetadataCharge, owner), 0),
            local(G),
            C::NotEqual,
            invalid,
        );
        if commit {
            b.branch(
                field(base, offset_of!(MetadataCharge, stage), 0),
                n(1),
                C::NotEqual,
                invalid,
            );
            b.pointer(allocation.clone(), invalid);
            b.branch(
                allocation,
                sub(n(u64::MAX), bytes.clone()),
                C::Above,
                invalid,
            );
            b.branch(bytes.clone(), local(USED), C::Above, invalid);
            b.set(local(USED), sub(local(USED), bytes));
        } else {
            let reserved = b.a.create_text_label();
            let next = b.a.create_text_label();
            b.branch(allocation, n(0), C::NotEqual, invalid);
            b.branch(
                field(base, offset_of!(MetadataCharge, stage), 0),
                n(1),
                C::Equal,
                reserved,
            );
            b.branch(
                field(base, offset_of!(MetadataCharge, stage), 0),
                n(2),
                C::NotEqual,
                invalid,
            );
            b.branch(bytes.clone(), local(PREV), C::Above, invalid);
            b.set(local(PREV), sub(local(PREV), bytes.clone()));
            b.a.jmp_label(next);
            b.a.bind_text_label(reserved);
            b.branch(bytes.clone(), local(USED), C::Above, invalid);
            b.set(local(USED), sub(local(USED), bytes));
            b.a.bind_text_label(next);
        }
    }
    if commit {
        // Both mapped range ends were checked above. Independent owners may be
        // adjacent in either order, but must never own overlapping backing bytes.
        let disjoint = b.a.create_text_label();
        let roots_begin = field(REG, offset_of!(MetadataCharge, allocation), 0);
        let descriptors_begin = field(SEG, offset_of!(MetadataCharge, allocation), 0);
        b.branch(
            roots_begin.clone(),
            add(
                descriptors_begin.clone(),
                field(SEG, offset_of!(MetadataCharge, bytes), 0),
            ),
            C::AboveOrEqual,
            disjoint,
        );
        b.branch(
            descriptors_begin,
            add(
                roots_begin,
                field(REG, offset_of!(MetadataCharge, bytes), 0),
            ),
            C::Below,
            invalid,
        );
        b.a.bind_text_label(disjoint);
        b.set(
            committed.clone(),
            add(committed, sub(pending.clone(), local(USED))),
        );
    } else {
        b.set(committed, add(local(CTX), local(PREV)));
    }
    b.set(pending, local(USED));
    for base in [REG, SEG] {
        b.set(
            field(base, offset_of!(MetadataCharge, stage), 0),
            n(if commit { 2 } else { 3 }),
        );
    }
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
}
