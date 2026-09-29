//! Bounded exclusive-source/private-destination copy; gc_relocate.h contract.
use super::*;
use crate::gc_abi::{ForwardCell as F, ObjectHeader as H, RelocationCopy as T};
macro_rules! cell {
    ($f:ident) => {
        field(G, offset_of!(F, $f), 0)
    };
}
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(T, $f), 0)
    };
}
macro_rules! src {
    ($f:ident) => {
        field(REG, offset_of!(H, $f), 0)
    };
}
macro_rules! dst {
    ($f:ident) => {
        field(SEG, offset_of!(H, $f), 0)
    };
}
#[allow(dead_code)]
pub(crate) fn emit_relocate_copy<A: PortableAsm>(a: &mut A) {
    const {
        assert!(std::mem::size_of::<H>() == 32);
    }
    let status: [_; 4] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let rounded = a.create_text_label();
    let separate = a.create_text_label();
    let initial = a.create_text_label();
    let copying = a.create_text_label();
    let terminal = a.create_text_label();
    let count = a.create_text_label();
    let copied = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.branch(cell!(incarnation), n(0), C::Equal, invalid);
    b.branch(
        req!(source_incarnation),
        cell!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.branch(req!(ticket), n(0), C::Equal, invalid);
    b.branch(req!(ticket), n(u64::MAX >> 2), C::Above, invalid);
    b.branch(
        field(G, offset_of!(F, word), 8),
        add(shl(req!(ticket), 2), n(1)),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(G, offset_of!(F, access), 8),
        n(crate::gc_abi::SOURCE_CLOSED),
        C::NotEqual,
        status[3],
    );
    b.branch(cell!(source_offset), n(48), C::Below, invalid);
    b.branch(req!(destination), n(48), C::Below, invalid);
    for v in [cell!(source_offset), req!(destination), req!(heap_base)] {
        b.branch(and(v, n(15)), n(0), C::NotEqual, invalid);
    }
    for v in [req!(source_limit), req!(destination_limit)] {
        b.branch(v.clone(), n(crate::gc_abi::MAX_OFFSET), C::Above, invalid);
        b.branch(req!(heap_base), sub(n(u64::MAX), v), C::Above, invalid);
    }
    b.branch(
        cell!(source_offset),
        req!(source_limit),
        C::AboveOrEqual,
        invalid,
    );
    b.branch(
        req!(destination),
        req!(destination_limit),
        C::AboveOrEqual,
        invalid,
    );
    b.branch(req!(budget), n(0), C::Equal, invalid);
    b.branch(
        req!(budget),
        n(crate::gc_abi::COPY_MAX_BYTES),
        C::Above,
        invalid,
    );
    b.branch(req!(stage), n(2), C::Above, invalid);
    b.set(
        local(REG),
        sub(add(req!(heap_base), cell!(source_offset)), n(32)),
    );
    b.branch(
        field(REG, offset_of!(H, state), 8),
        n(2),
        C::NotEqual,
        invalid,
    );
    b.set(local(LEASE), src!(payload_bytes));
    b.branch(
        local(LEASE),
        sub(req!(source_limit), cell!(source_offset)),
        C::Above,
        invalid,
    );
    b.branch(src!(descriptor_id), n(0), C::Equal, invalid);
    b.branch(src!(descriptor_id), n(u32::MAX as u64), C::Above, invalid);
    b.pointer(src!(mark_record), invalid);
    b.set(local(STATE), and(add(local(LEASE), n(15)), n(!15)));
    b.branch(local(STATE), n(0), C::NotEqual, rounded);
    b.set(local(STATE), n(16));
    b.a.bind_text_label(rounded);
    b.branch(
        local(STATE),
        sub(req!(source_limit), cell!(source_offset)),
        C::Above,
        invalid,
    );
    b.branch(
        local(STATE),
        sub(req!(destination_limit), req!(destination)),
        C::Above,
        invalid,
    );
    b.set(local(USED), add(cell!(source_offset), local(STATE)));
    b.set(local(CTX), add(req!(destination), local(STATE)));
    b.branch(
        local(USED),
        sub(req!(destination), n(32)),
        C::Below,
        separate,
    );
    b.branch(
        local(USED),
        sub(req!(destination), n(32)),
        C::Equal,
        separate,
    );
    b.branch(
        local(CTX),
        sub(cell!(source_offset), n(32)),
        C::Above,
        invalid,
    );
    b.a.bind_text_label(separate);
    b.set(
        local(SEG),
        sub(add(req!(heap_base), req!(destination)), n(32)),
    );
    b.set(local(KIND), field(SEG, offset_of!(H, state), 8));
    b.branch(req!(stage), n(0), C::Equal, initial);
    for off in [
        offset_of!(H, payload_bytes),
        offset_of!(H, descriptor_id),
        offset_of!(H, mark_record),
    ] {
        b.branch(field(SEG, off, 0), field(REG, off, 0), C::NotEqual, invalid);
    }
    b.branch(req!(stage), n(2), C::Equal, terminal);
    b.branch(req!(cursor), n(0), C::Equal, invalid);
    b.branch(req!(cursor), local(LEASE), C::AboveOrEqual, invalid);
    b.branch(local(KIND), n(1), C::NotEqual, invalid);
    b.a.jmp_label(copying);
    b.a.bind_text_label(terminal);
    b.branch(req!(cursor), local(LEASE), C::NotEqual, invalid);
    b.branch(local(KIND), n(2), C::NotEqual, invalid);
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(initial);
    b.branch(req!(cursor), n(0), C::NotEqual, invalid);
    b.branch(local(KIND), n(0), C::NotEqual, invalid);
    for v in [dst!(payload_bytes), dst!(descriptor_id), dst!(mark_record)] {
        b.branch(v, n(0), C::NotEqual, invalid);
    }
    for off in [
        offset_of!(H, payload_bytes),
        offset_of!(H, descriptor_id),
        offset_of!(H, mark_record),
    ] {
        b.set(field(SEG, off, 0), field(REG, off, 0));
    }
    b.set(field(SEG, offset_of!(H, state), 8), n(1));
    b.a.bind_text_label(copying);
    b.set(local(PREV), sub(local(LEASE), req!(cursor)));
    b.branch(local(PREV), req!(budget), C::Below, count);
    b.set(local(PREV), req!(budget));
    b.a.bind_text_label(count);
    b.set(local(NEXT), add(req!(cursor), local(PREV)));
    b.value(add(
        add(req!(heap_base), cell!(source_offset)),
        req!(cursor),
    ));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(add(add(req!(heap_base), req!(destination)), req!(cursor)));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V1, PREV);
    b.a.rep_movsb();
    b.set(req!(cursor), local(NEXT));
    b.branch(local(NEXT), local(LEASE), C::Equal, copied);
    b.set(req!(stage), n(1));
    b.a.jmp_label(status[2]);
    b.a.bind_text_label(copied);
    b.set(req!(stage), n(2));
    b.set(field(SEG, offset_of!(H, state), 8), n(2));
    b.a.jmp_label(status[0]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}

/// Only complete, still-owned copies can publish their forwarding destination.
#[allow(dead_code)]
pub(crate) fn emit_relocate_publish<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let copy = a.create_text_label();
    let publish = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 48);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(req!(stage), n(2), C::NotEqual, invalid);
    b.branch(local(OP), n(u16::MAX as u64), C::Above, invalid);
    b.branch(req!(destination), n(0), C::Equal, invalid);
    b.branch(
        req!(destination),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    b.branch(and(req!(destination), n(15)), n(0), C::NotEqual, invalid);
    b.branch(and(local(OP), n(0xcf)), n(0), C::NotEqual, invalid);
    b.branch(and(local(OP), n(0x30)), n(0), C::Equal, invalid);
    for mask in [0x300, 0xc00, 0xf000] {
        b.set(local(PREV), and(local(OP), n(mask)));
        b.branch(local(PREV), n(0), C::Equal, invalid);
        b.branch(
            and(local(PREV), sub(local(PREV), n(1))),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, Q);
    b.a.call_label(copy);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, done);
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 144);
    b.a.store_rbp_slot(REG, R::V0);
    b.set(field(REG, 0, 0), cell!(source_offset));
    b.set(field(REG, 8, 0), req!(source_incarnation));
    b.set(field(REG, 16, 0), req!(ticket));
    b.set(
        field(REG, 24, 0),
        add(shl(req!(destination), 16), local(OP)),
    );
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, REG);
    b.a.mov_imm64(R::V2, 2);
    b.a.call_label(publish);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(copy);
    emit_relocate_copy(b.a);
    b.a.ret();
    b.a.bind_text_label(publish);
    super::forward::emit_forward_step(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}

/// Heap-lock protected private target selection and completed metadata install.
#[allow(dead_code)]
pub(crate) fn emit_relocate_target<A: PortableAsm>(a: &mut A) {
    use crate::gc_abi::{AllocationPage as P, ForwardTable as FT};
    macro_rules! page {
        ($f:ident) => {
            field(G, offset_of!(P, $f), 0)
        };
    }
    macro_rules! table {
        ($f:ident) => {
            field(REG, offset_of!(FT, $f), 0)
        };
    }
    let status: [_; 4] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let valid_state = a.create_text_label();
    let check_request = a.create_text_label();
    let install_request = a.create_text_label();
    let capacity = a.create_text_label();
    let free_cell = a.create_text_label();
    let install = a.create_text_label();
    let rounded = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.set(local(STATE), field(G, offset_of!(P, state), 8));
    b.set(local(LEASE), field(G, offset_of!(P, allocated_top), 8));
    b.branch(local(OP), n(1), C::Above, invalid);
    b.branch(local(STATE), n(3), C::Equal, valid_state);
    b.branch(local(STATE), n(5), C::NotEqual, invalid);
    b.a.bind_text_label(valid_state);
    b.branch(page!(owner), n(0), C::NotEqual, invalid);
    for v in [page!(incarnation), page!(identity_base), page!(start)] {
        b.branch(v, n(0), C::Equal, invalid);
    }
    b.branch(page!(generation), n(1), C::Above, invalid);
    b.branch(page!(start), page!(limit), C::AboveOrEqual, invalid);
    b.branch(
        page!(limit),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    for v in [page!(heap_base), page!(start), page!(limit), local(LEASE)] {
        b.branch(and(v, n(15)), n(0), C::NotEqual, invalid);
    }
    b.branch(
        page!(heap_base),
        sub(n(u64::MAX), page!(limit)),
        C::Above,
        invalid,
    );
    b.branch(req!(heap_base), page!(heap_base), C::NotEqual, invalid);
    b.branch(local(LEASE), page!(start), C::Below, invalid);
    b.branch(local(LEASE), page!(limit), C::Above, invalid);
    b.branch(local(STATE), n(3), C::NotEqual, check_request);
    b.branch(local(LEASE), page!(start), C::NotEqual, invalid);
    b.a.bind_text_label(check_request);
    b.pointer(page!(starts), invalid);
    b.branch(
        page!(starts_words),
        shr(add(sub(page!(limit), page!(start)), n(1023)), 10),
        C::Below,
        invalid,
    );
    b.branch(page!(starts_words), n(u64::MAX / 8), C::Above, invalid);
    b.branch(
        page!(starts),
        sub(n(u64::MAX), shl(page!(starts_words), 3)),
        C::Above,
        invalid,
    );
    b.branch(local(OP), n(0), C::NotEqual, install_request);
    for v in [
        req!(stage),
        req!(cursor),
        req!(destination),
        req!(destination_limit),
    ] {
        b.branch(v, n(0), C::NotEqual, invalid);
    }
    b.a.jmp_label(capacity);
    b.a.bind_text_label(install_request);
    b.branch(req!(stage), n(2), C::NotEqual, invalid);
    b.branch(
        req!(destination),
        add(local(LEASE), n(32)),
        C::NotEqual,
        invalid,
    );
    b.branch(req!(destination_limit), page!(limit), C::NotEqual, invalid);
    b.a.bind_text_label(capacity);
    b.branch(sub(page!(limit), local(LEASE)), n(48), C::Below, status[2]);
    b.set(
        local(KIND),
        shr(sub(add(local(LEASE), n(32)), page!(start)), 4),
    );
    b.set(local(CTX), add(page!(starts), shl(shr(local(KIND), 6), 3)));
    b.value(and(local(KIND), n(63)));
    b.a.mov_reg_reg(R::V1, R::V0);
    b.a.mov_imm64(R::V2, 1);
    b.a.shl_reg_count(R::V2);
    b.a.store_rbp_slot(NEXT, R::V2);
    b.set(local(PREV), field(CTX, 0, 8));
    b.branch(and(local(PREV), local(NEXT)), n(0), C::NotEqual, invalid);
    b.branch(page!(forward_table), n(0), C::Equal, status[3]);
    b.pointer(page!(forward_table), invalid);
    b.set(local(REG), page!(forward_table));
    b.set(local(USED), field(REG, offset_of!(FT, used), 8));
    b.pointer(table!(cells), invalid);
    b.branch(table!(capacity), n(0), C::Equal, invalid);
    b.branch(table!(capacity), n(u32::MAX as u64), C::Above, invalid);
    b.branch(
        table!(cells),
        sub(n(u64::MAX), shl(table!(capacity), 5)),
        C::Above,
        invalid,
    );
    b.branch(local(USED), table!(capacity), C::Above, invalid);
    b.branch(local(USED), table!(capacity), C::Equal, status[3]);
    b.set(local(SEG), add(table!(cells), shl(local(USED), 5)));
    b.branch(local(USED), n(0), C::Equal, free_cell);
    b.set(local(KIND), sub(local(SEG), n(32)));
    b.branch(
        field(KIND, offset_of!(F, source_offset), 0),
        add(local(LEASE), n(32)),
        C::AboveOrEqual,
        invalid,
    );
    b.branch(
        field(KIND, offset_of!(F, incarnation), 0),
        page!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(free_cell);
    for off in [
        offset_of!(F, source_offset),
        offset_of!(F, incarnation),
        offset_of!(F, word),
        offset_of!(F, access),
    ] {
        b.branch(field(SEG, off, 0), n(0), C::NotEqual, invalid);
    }
    b.branch(local(OP), n(0), C::NotEqual, install);
    b.set(req!(destination), add(local(LEASE), n(32)));
    b.set(req!(destination_limit), page!(limit));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(install);
    b.set(local(KIND), add(page!(heap_base), local(LEASE)));
    b.branch(
        field(KIND, offset_of!(H, state), 8),
        n(2),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(KIND, offset_of!(H, payload_bytes), 0),
        req!(cursor),
        C::NotEqual,
        invalid,
    );
    b.branch(
        req!(cursor),
        sub(page!(limit), req!(destination)),
        C::Above,
        invalid,
    );
    b.branch(
        field(KIND, offset_of!(H, descriptor_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(KIND, offset_of!(H, descriptor_id), 0),
        n(u32::MAX as u64),
        C::Above,
        invalid,
    );
    b.pointer(field(KIND, offset_of!(H, mark_record), 0), invalid);
    b.set(local(KIND), and(add(req!(cursor), n(15)), n(!15)));
    b.branch(local(KIND), n(0), C::NotEqual, rounded);
    b.set(local(KIND), n(16));
    b.a.bind_text_label(rounded);
    b.branch(
        local(KIND),
        sub(page!(limit), req!(destination)),
        C::Above,
        invalid,
    );
    b.set(
        field(SEG, offset_of!(F, source_offset), 0),
        req!(destination),
    );
    b.set(
        field(SEG, offset_of!(F, incarnation), 0),
        page!(incarnation),
    );
    b.set(field(REG, offset_of!(FT, used), 8), add(local(USED), n(1)));
    // The selected bit is absent; adding it implements the bitmap OR.
    b.set(field(CTX, 0, 8), add(local(PREV), local(NEXT)));
    b.set(
        field(G, offset_of!(P, allocated_top), 8),
        add(req!(destination), local(KIND)),
    );
    b.set(field(G, offset_of!(P, state), 8), n(5));
    b.a.jmp_label(status[0]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
