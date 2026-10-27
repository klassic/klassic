//! Registry-lock protected root lifetime steps. A small expression emitter keeps
//! the many metadata checks explicit without assigning persistent scratch GPRs.
//! Matching ownership and completion contract: runtime/gc/gc_root_lifetime.h.
use crate::gc_abi::{
    MutatorContext, RootLease, RootLifeRequest, RootRegistration, RootRegistry, RootSegment,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

#[derive(Clone)]
enum E {
    Imm(u64),
    Slot(i32),
    Mem(Box<E>, i32, u8),
    Bin(Box<E>, Box<E>, u8),
}
fn n(v: u64) -> E {
    E::Imm(v)
}
fn local(v: i32) -> E {
    E::Slot(v)
}
fn field(base: i32, off: usize, atomic: u8) -> E {
    E::Mem(Box::new(local(base)), off as i32, atomic)
}
fn add(a: E, b: E) -> E {
    E::Bin(Box::new(a), Box::new(b), 0)
}
fn sub(a: E, b: E) -> E {
    E::Bin(Box::new(a), Box::new(b), 1)
}
fn and(a: E, b: E) -> E {
    E::Bin(Box::new(a), Box::new(b), 2)
}
fn shr(a: E, bits: u64) -> E {
    E::Bin(Box::new(a), Box::new(n(bits)), 3)
}
fn shl(a: E, bits: u64) -> E {
    E::Bin(Box::new(a), Box::new(n(bits)), 4)
}
struct Emit<'a, A: PortableAsm> {
    a: &'a mut A,
}
impl<A: PortableAsm> Emit<'_, A> {
    fn value(&mut self, e: E) {
        match e {
            E::Imm(v) => self.a.mov_imm64(R::V0, v),
            E::Slot(v) => self.a.load_rbp_slot(R::V0, v),
            E::Mem(base, off, atomic) => {
                self.value(*base);
                if atomic != 0 {
                    self.a.add_reg_imm32(R::V0, off);
                    if atomic == 4 {
                        self.a.atomic_load_acquire_u32(R::V0, R::V0);
                    } else {
                        self.a.atomic_load_acquire(R::V0, R::V0);
                    }
                } else {
                    self.a.load_ptr_disp32(R::V0, R::V0, off);
                }
            }
            E::Bin(left, right, op) => {
                if op >= 3 {
                    let E::Imm(bits) = *right else { unreachable!() };
                    self.value(*left);
                    if op == 3 {
                        self.a.shr_reg_imm8(R::V0, bits as u8);
                    } else {
                        self.a.shl_reg_imm8(R::V0, bits as u8);
                    }
                } else {
                    self.value(*left);
                    self.a.push_reg(R::V0);
                    self.value(*right);
                    self.a.mov_reg_reg(R::V1, R::V0);
                    self.a.pop_reg(R::V0);
                    match op {
                        0 => self.a.add_reg_reg(R::V0, R::V1),
                        1 => self.a.sub_reg_reg(R::V0, R::V1),
                        2 => self.a.and_reg_reg(R::V0, R::V1),
                        _ => unreachable!(),
                    }
                }
            }
        }
    }
    fn set(&mut self, dst: E, value: E) {
        self.value(value);
        match dst {
            E::Slot(off) => self.a.store_rbp_slot(off, R::V0),
            E::Mem(base, off, atomic) => {
                self.a.push_reg(R::V0);
                self.value(*base);
                self.a.pop_reg(R::V1);
                if atomic != 0 {
                    self.a.add_reg_imm32(R::V0, off);
                    self.a.atomic_store_release(R::V0, R::V1);
                } else {
                    self.a.store_ptr_disp32(R::V0, off, R::V1);
                }
            }
            _ => unreachable!(),
        }
    }
    fn branch(&mut self, left: E, right: E, cond: C, label: A::TextLabel) {
        self.value(left);
        self.a.push_reg(R::V0);
        self.value(right);
        self.a.mov_reg_reg(R::V1, R::V0);
        self.a.pop_reg(R::V0);
        self.a.cmp_reg_reg(R::V0, R::V1);
        self.a.jcc_label(cond, label);
    }
    fn pointer(&mut self, value: E, invalid: A::TextLabel) {
        self.branch(value.clone(), n(0), C::Equal, invalid);
        self.branch(and(value, n(7)), n(0), C::NotEqual, invalid);
    }
}
const G: i32 = 8;
const Q: i32 = 16;
const OP: i32 = 24;
const REG: i32 = 32;
const SEG: i32 = 40;
const LEASE: i32 = 48;
const STATE: i32 = 56;
const USED: i32 = 64;
const CTX: i32 = 72;
const PREV: i32 = 80;
const NEXT: i32 = 88;
const KIND: i32 = 96;

macro_rules! g {
    ($f:ident) => {
        field(G, offset_of!(RootRegistry, $f), 0)
    };
}
macro_rules! q {
    ($f:ident) => {
        field(Q, offset_of!(RootLifeRequest, $f), 0)
    };
}
macro_rules! r {
    (cleanup_cursor) => {
        field(REG, offset_of!(RootRegistration, cleanup_cursor), 8)
    };
    ($f:ident) => {
        field(REG, offset_of!(RootRegistration, $f), 0)
    };
}
macro_rules! s {
    (state) => {
        field(SEG, offset_of!(RootSegment, state), 8)
    };
    (used) => {
        field(SEG, offset_of!(RootSegment, used), 8)
    };
    ($f:ident) => {
        field(SEG, offset_of!(RootSegment, $f), 0)
    };
}
macro_rules! l {
    ($f:ident) => {
        field(LEASE, offset_of!(RootLease, $f), 0)
    };
}
macro_rules! ctx {
    (abi_version) => {
        field(CTX, offset_of!(MutatorContext, abi_version), 4)
    };
    (state) => {
        field(CTX, offset_of!(MutatorContext, state), 4)
    };
    (access_epoch) => {
        field(CTX, offset_of!(MutatorContext, access_epoch), 8)
    };
    (adopted_phase) => {
        field(CTX, offset_of!(MutatorContext, adopted_phase), 8)
    };
    ($f:ident) => {
        field(CTX, offset_of!(MutatorContext, $f), 0)
    };
}

#[allow(dead_code)]
pub(crate) fn emit_life_locked<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let wait = a.create_text_label();
    let exhausted = a.create_text_label();
    let success = a.create_text_label();
    let done = a.create_text_label();
    let ops: [_; 8] = std::array::from_fn(|_| a.create_text_label());
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(7), C::Above, invalid);
    b.pointer(q!(registration), invalid);
    b.set(local(REG), q!(registration));
    let nonempty = b.a.create_text_label();
    let head_valid = b.a.create_text_label();
    b.branch(g!(head), n(0), C::NotEqual, nonempty);
    b.branch(g!(retiring), n(0), C::NotEqual, invalid);
    b.a.jmp_label(head_valid);
    b.a.bind_text_label(nonempty);
    b.branch(g!(retiring), n(0), C::Equal, invalid);
    b.a.bind_text_label(head_valid);
    b.pointer(r!(segment), invalid);
    b.set(local(SEG), r!(segment));
    b.branch(r!(leases), g!(leases), C::Above, invalid);
    b.set(local(STATE), s!(state));
    b.set(local(USED), s!(used));
    b.branch(s!(owner), n(0), C::Equal, invalid);
    b.pointer(s!(cells), invalid);
    b.branch(s!(capacity), n(0), C::Equal, invalid);
    b.branch(s!(capacity), n(u32::MAX as u64), C::Above, invalid);
    b.branch(
        add(s!(cells), shl(s!(capacity), 5)),
        s!(cells),
        C::Below,
        invalid,
    );
    b.branch(local(USED), s!(capacity), C::Above, invalid);
    let owner = b.a.create_text_label();
    let owner_done = b.a.create_text_label();
    b.branch(local(OP), n(0), C::Equal, owner);
    b.branch(local(OP), n(3), C::NotEqual, owner_done);
    b.a.bind_text_label(owner);
    b.pointer(q!(context), invalid);
    b.set(local(CTX), q!(context));
    b.branch(
        ctx!(abi_version),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(ctx!(thread_id), s!(owner), C::NotEqual, invalid);
    b.branch(ctx!(access_epoch), n(0), C::NotEqual, invalid);
    let running = b.a.create_text_label();
    b.branch(ctx!(state), n(0), C::Equal, running);
    b.branch(local(OP), n(3), C::NotEqual, invalid);
    b.branch(ctx!(state), n(3), C::NotEqual, invalid);
    b.a.bind_text_label(running);
    b.branch(ctx!(adopted_phase), g!(phase), C::NotEqual, invalid);
    b.a.bind_text_label(owner_done);
    b.branch(local(OP), n(0), C::Equal, ops[0]);
    b.branch(r!(registered), n(1), C::NotEqual, invalid);
    b.branch(r!(scope), n(0), C::Equal, invalid);
    b.branch(q!(scope), r!(scope), C::NotEqual, invalid);
    b.branch(s!(scope), r!(scope), C::NotEqual, invalid);
    b.branch(s!(registration), local(REG), C::NotEqual, invalid);
    b.branch(local(STATE), n(1), C::Below, invalid);
    b.branch(local(STATE), n(3), C::Above, invalid);
    b.branch(local(OP), n(3), C::Equal, ops[3]);
    b.pointer(q!(lease), invalid);
    b.set(local(LEASE), q!(lease));
    for op in [1, 4, 6] {
        b.branch(local(OP), n(op), C::Equal, ops[op as usize]);
    }
    b.set(local(KIND), n(3));
    let kind_ready = b.a.create_text_label();
    let not_release = b.a.create_text_label();
    b.branch(local(OP), n(2), C::NotEqual, not_release);
    b.set(local(KIND), n(1));
    b.a.jmp_label(kind_ready);
    b.a.bind_text_label(not_release);
    b.branch(local(OP), n(5), C::NotEqual, kind_ready);
    b.set(local(KIND), n(2));
    b.a.bind_text_label(kind_ready);
    b.branch(l!(kind), local(KIND), C::NotEqual, invalid);
    b.branch(q!(serial), n(0), C::Equal, invalid);
    b.branch(q!(serial), l!(serial), C::NotEqual, invalid);
    b.branch(l!(registration), local(REG), C::NotEqual, invalid);
    b.branch(l!(scope), r!(scope), C::NotEqual, invalid);
    b.branch(r!(leases), n(0), C::Equal, invalid);
    b.branch(g!(leases), n(0), C::Equal, invalid);
    b.branch(local(OP), n(2), C::Equal, ops[2]);
    b.branch(local(OP), n(5), C::Equal, ops[5]);
    b.a.jmp_label(ops[7]);
    b.a.bind_text_label(ops[0]);
    for value in [
        r!(registered),
        r!(scope),
        r!(leases),
        local(STATE),
        local(USED),
        s!(scope),
        s!(registration),
        r!(limit),
        r!(cursor),
        r!(phase),
        r!(required_mask),
        r!(claim_serial),
        r!(claim_end),
        r!(previous),
        r!(next),
        r!(cleanup_cursor),
        r!(inventory_previous),
        r!(inventory_next),
        r!(context),
        r!(owner_previous),
        r!(owner_next),
        r!(journal_serial),
        r!(journal_end),
    ] {
        b.branch(value, n(0), C::NotEqual, invalid);
    }
    b.branch(g!(last_scope), n(u64::MAX), C::Equal, exhausted);
    b.branch(g!(active), n(u64::MAX), C::Equal, exhausted);
    b.set(local(NEXT), g!(inventory));
    let empty_inventory = b.a.create_text_label();
    b.branch(local(NEXT), n(0), C::Equal, empty_inventory);
    b.pointer(local(NEXT), invalid);
    b.branch(local(NEXT), local(REG), C::Equal, invalid);
    b.branch(
        field(NEXT, offset_of!(RootRegistration, inventory_previous), 0),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(empty_inventory);
    b.set(local(PREV), ctx!(root_segments));
    let empty_owner = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, empty_owner);
    b.pointer(local(PREV), invalid);
    b.branch(local(PREV), local(REG), C::Equal, invalid);
    b.branch(
        field(PREV, offset_of!(RootRegistration, owner_previous), 0),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(PREV, offset_of!(RootRegistration, context), 0),
        local(CTX),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(PREV, offset_of!(RootRegistration, registered), 0),
        n(1),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(empty_owner);
    b.set(g!(last_scope), add(g!(last_scope), n(1)));
    b.set(r!(scope), g!(last_scope));
    b.set(s!(scope), g!(last_scope));
    b.set(q!(scope), g!(last_scope));
    b.set(r!(registered), n(1));
    b.set(s!(registration), local(REG));
    b.set(g!(active), add(g!(active), n(1)));
    b.set(r!(inventory_next), local(NEXT));
    let no_previous_inventory = b.a.create_text_label();
    b.branch(local(NEXT), n(0), C::Equal, no_previous_inventory);
    b.set(
        field(NEXT, offset_of!(RootRegistration, inventory_previous), 0),
        local(REG),
    );
    b.a.bind_text_label(no_previous_inventory);
    b.set(g!(inventory), local(REG));
    b.set(r!(context), local(CTX));
    b.set(r!(owner_next), local(PREV));
    let no_owner_previous = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, no_owner_previous);
    b.set(
        field(PREV, offset_of!(RootRegistration, owner_previous), 0),
        local(REG),
    );
    b.a.bind_text_label(no_owner_previous);
    b.set(ctx!(root_segments), local(REG));
    b.set(s!(state), n(1));
    b.a.jmp_label(success);
    b.a.bind_text_label(ops[3]);
    b.branch(local(STATE), n(1), C::NotEqual, invalid);
    b.branch(g!(active), n(0), C::Equal, invalid);
    for value in [r!(claim_serial), r!(previous), r!(next)] {
        b.branch(value, n(0), C::NotEqual, invalid);
    }
    b.branch(g!(retiring), n(u64::MAX), C::Equal, exhausted);
    b.set(local(NEXT), g!(head));
    let no_head = b.a.create_text_label();
    b.branch(local(NEXT), n(0), C::Equal, no_head);
    b.pointer(local(NEXT), invalid);
    b.branch(local(NEXT), local(REG), C::Equal, invalid);
    b.branch(
        field(NEXT, offset_of!(RootRegistration, previous), 0),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(no_head);
    b.branch(and(g!(phase), n(15)), n(7), C::Above, invalid);
    b.branch(and(shr(g!(phase), 4), n(15)), n(7), C::Above, invalid);
    b.branch(r!(context), local(CTX), C::NotEqual, invalid);
    b.branch(ctx!(retiring_roots), n(u64::MAX), C::Equal, exhausted);
    unlink_owner(&mut b, invalid);
    b.set(ctx!(retiring_roots), add(ctx!(retiring_roots), n(1)));
    b.set(local(NEXT), g!(head));
    b.set(r!(limit), local(USED));
    b.set(r!(cursor), n(0));
    b.set(r!(phase), g!(phase));
    b.set(r!(required_mask), n(0));
    for (shift, bit) in [(0, 1), (4, 2)] {
        let skip = b.a.create_text_label();
        let phase = and(shr(g!(phase), shift), n(15));
        b.branch(phase.clone(), n(1), C::Below, skip);
        b.branch(phase, n(3), C::Above, skip);
        b.set(r!(required_mask), add(r!(required_mask), n(bit)));
        b.a.bind_text_label(skip);
    }
    b.set(r!(next), local(NEXT));
    let no_head_write = b.a.create_text_label();
    b.branch(local(NEXT), n(0), C::Equal, no_head_write);
    b.set(
        field(NEXT, offset_of!(RootRegistration, previous), 0),
        local(REG),
    );
    b.a.bind_text_label(no_head_write);
    b.set(g!(head), local(REG));
    b.set(g!(active), sub(g!(active), n(1)));
    b.set(g!(retiring), add(g!(retiring), n(1)));
    b.set(s!(state), n(2));
    b.a.jmp_label(success);
    let issue = b.a.create_text_label();
    b.a.bind_text_label(ops[1]);
    b.branch(l!(kind), n(0), C::NotEqual, invalid);
    b.branch(local(STATE), n(1), C::NotEqual, invalid);
    b.set(local(KIND), n(1));
    b.a.jmp_label(issue);
    for (op, kind) in [(4, 2), (6, 3)] {
        b.a.bind_text_label(ops[op]);
        b.branch(l!(kind), n(0), C::NotEqual, invalid);
        b.branch(local(STATE), n(2), C::NotEqual, invalid);
        b.branch(g!(retiring), n(0), C::Equal, invalid);
        b.branch(r!(limit), local(USED), C::NotEqual, invalid);
        b.branch(r!(cursor), r!(limit), C::Above, invalid);
        b.branch(r!(claim_serial), n(0), C::NotEqual, wait);
        if op == 4 {
            b.branch(r!(cursor), r!(limit), C::Equal, wait);
        } else {
            b.branch(r!(cursor), r!(limit), C::NotEqual, wait);
            b.branch(r!(leases), n(0), C::NotEqual, wait);
        }
        b.set(local(KIND), n(kind));
        b.a.jmp_label(issue);
    }
    b.a.bind_text_label(issue);
    b.branch(l!(kind), n(0), C::NotEqual, invalid);
    for value in [g!(last_lease), g!(leases), r!(leases)] {
        b.branch(value, n(u64::MAX), C::Equal, exhausted);
    }
    let keep_inventory = b.a.create_text_label();
    b.branch(local(KIND), n(3), C::NotEqual, keep_inventory);
    unlink_inventory(&mut b, invalid);
    b.a.bind_text_label(keep_inventory);
    b.set(g!(last_lease), add(g!(last_lease), n(1)));
    b.set(l!(registration), local(REG));
    b.set(l!(scope), r!(scope));
    b.set(l!(serial), g!(last_lease));
    b.set(l!(kind), local(KIND));
    b.set(l!(cursor), n(0));
    b.set(q!(serial), g!(last_lease));
    b.set(r!(leases), add(r!(leases), n(1)));
    b.set(g!(leases), add(g!(leases), n(1)));
    b.branch(local(KIND), n(1), C::Equal, success);
    b.set(r!(claim_serial), g!(last_lease));
    let clean = b.a.create_text_label();
    b.branch(local(KIND), n(3), C::Equal, clean);
    b.set(l!(cursor), r!(cursor));
    b.set(r!(claim_end), r!(limit));
    let end_ready = b.a.create_text_label();
    b.branch(sub(r!(limit), r!(cursor)), n(256), C::Below, end_ready);
    b.set(r!(claim_end), add(r!(cursor), n(256)));
    b.a.bind_text_label(end_ready);
    b.set(q!(begin), r!(cursor));
    b.set(q!(end), r!(claim_end));
    b.set(q!(mask), r!(required_mask));
    b.set(q!(phase), r!(phase));
    b.a.jmp_label(success);
    b.a.bind_text_label(clean);
    b.set(r!(cleanup_cursor), n(0));
    b.set(s!(state), n(3));
    b.a.jmp_label(success);
    let release = b.a.create_text_label();
    b.a.bind_text_label(ops[2]);
    b.branch(local(STATE), n(3), C::Equal, invalid);
    b.a.jmp_label(release);
    b.a.bind_text_label(ops[5]);
    b.branch(local(STATE), n(2), C::NotEqual, invalid);
    b.branch(r!(claim_serial), l!(serial), C::NotEqual, invalid);
    b.branch(q!(begin), r!(cursor), C::NotEqual, invalid);
    b.branch(q!(end), r!(claim_end), C::NotEqual, invalid);
    b.branch(q!(phase), r!(phase), C::NotEqual, invalid);
    b.branch(q!(end), r!(limit), C::Above, invalid);
    b.branch(q!(begin), q!(end), C::AboveOrEqual, invalid);
    b.branch(q!(mask), r!(required_mask), C::NotEqual, wait);
    b.branch(l!(cursor), r!(claim_end), C::NotEqual, wait);
    b.set(r!(cursor), r!(claim_end));
    b.set(r!(claim_serial), n(0));
    b.set(r!(claim_end), n(0));
    b.a.jmp_label(release);
    b.a.bind_text_label(ops[7]);
    b.branch(local(STATE), n(3), C::NotEqual, invalid);
    b.branch(r!(claim_serial), l!(serial), C::NotEqual, invalid);
    b.branch(r!(leases), n(1), C::NotEqual, invalid);
    b.branch(g!(retiring), n(0), C::Equal, invalid);
    b.branch(r!(cleanup_cursor), r!(limit), C::NotEqual, wait);
    b.pointer(r!(context), invalid);
    b.set(local(CTX), r!(context));
    b.branch(
        ctx!(abi_version),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(ctx!(thread_id), s!(owner), C::NotEqual, invalid);
    b.branch(ctx!(retiring_roots), n(0), C::Equal, invalid);
    let no_journal = b.a.create_text_label();
    b.branch(s!(journal), n(0), C::Equal, no_journal);
    b.pointer(s!(journal), invalid);
    b.set(local(KIND), s!(journal));
    b.set(
        local(USED),
        field(KIND, offset_of!(crate::gc_abi::BufferCursor, consumed), 8),
    );
    b.set(
        local(STATE),
        field(KIND, offset_of!(crate::gc_abi::BufferCursor, published), 8),
    );
    b.branch(local(USED), local(STATE), C::Above, invalid);
    b.branch(local(USED), local(STATE), C::NotEqual, wait);
    b.a.bind_text_label(no_journal);
    b.set(local(PREV), r!(previous));
    b.set(local(NEXT), r!(next));
    let head = b.a.create_text_label();
    let prev_ok = b.a.create_text_label();
    let next_ok = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, head);
    b.pointer(local(PREV), invalid);
    b.branch(
        field(PREV, offset_of!(RootRegistration, next), 0),
        local(REG),
        C::NotEqual,
        invalid,
    );
    b.a.jmp_label(prev_ok);
    b.a.bind_text_label(head);
    b.branch(g!(head), local(REG), C::NotEqual, invalid);
    b.a.bind_text_label(prev_ok);
    b.branch(local(NEXT), n(0), C::Equal, next_ok);
    b.pointer(local(NEXT), invalid);
    b.branch(
        field(NEXT, offset_of!(RootRegistration, previous), 0),
        local(REG),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(next_ok);
    let write_head = b.a.create_text_label();
    let unlink_next = b.a.create_text_label();
    let unlink_done = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, write_head);
    b.set(
        field(PREV, offset_of!(RootRegistration, next), 0),
        local(NEXT),
    );
    b.a.jmp_label(unlink_next);
    b.a.bind_text_label(write_head);
    b.set(g!(head), local(NEXT));
    b.a.bind_text_label(unlink_next);
    b.branch(local(NEXT), n(0), C::Equal, unlink_done);
    b.set(
        field(NEXT, offset_of!(RootRegistration, previous), 0),
        local(PREV),
    );
    b.a.bind_text_label(unlink_done);
    b.set(g!(retiring), sub(g!(retiring), n(1)));
    b.set(g!(leases), sub(g!(leases), n(1)));
    b.set(l!(kind), n(0));
    b.set(ctx!(retiring_roots), sub(ctx!(retiring_roots), n(1)));
    for off in (8..std::mem::size_of::<RootRegistration>()).step_by(8) {
        b.set(field(REG, off, 0), n(0));
    }
    b.set(s!(scope), n(0));
    b.set(s!(registration), n(0));
    b.set(s!(used), n(0));
    b.set(s!(state), n(0));
    b.a.jmp_label(success);
    b.a.bind_text_label(release);
    b.set(r!(leases), sub(r!(leases), n(1)));
    b.set(g!(leases), sub(g!(leases), n(1)));
    b.set(l!(kind), n(0));
    b.a.bind_text_label(success);
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    for (label, code) in [(invalid, 1), (wait, 2), (exhausted, 3)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, code);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}

#[allow(dead_code)]
pub(crate) fn emit_clear_chunk<A: PortableAsm>(a: &mut A) {
    emit_clear_chunk_bounded(a, 256);
}

/// Same exclusive-cleaner checks, with a smaller backend work quantum.
pub(crate) fn emit_clear_chunk_bounded<A: PortableAsm>(a: &mut A, cells: u64) {
    assert!((1..=256).contains(&cells));
    use crate::gc_abi::RootCell;
    let invalid = a.create_text_label();
    let more = a.create_text_label();
    let done = a.create_text_label();
    let copy = a.create_text_label();
    let finish = a.create_text_label();
    let end_ready = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    b.pointer(q!(registration), invalid);
    b.pointer(q!(lease), invalid);
    b.set(local(REG), q!(registration));
    b.set(local(LEASE), q!(lease));
    b.branch(l!(kind), n(3), C::NotEqual, invalid);
    b.branch(q!(serial), n(0), C::Equal, invalid);
    b.branch(l!(serial), q!(serial), C::NotEqual, invalid);
    b.branch(l!(registration), local(REG), C::NotEqual, invalid);
    b.branch(l!(scope), q!(scope), C::NotEqual, invalid);
    b.branch(r!(registered), n(0), C::Equal, invalid);
    b.branch(r!(scope), q!(scope), C::NotEqual, invalid);
    b.branch(r!(claim_serial), q!(serial), C::NotEqual, invalid);
    b.branch(r!(leases), n(1), C::NotEqual, invalid);
    b.branch(r!(segment), n(0), C::Equal, invalid);
    b.set(local(SEG), r!(segment));
    b.set(local(USED), r!(cleanup_cursor));
    b.branch(s!(state), n(3), C::NotEqual, invalid);
    b.branch(local(USED), r!(limit), C::Above, invalid);
    b.branch(r!(limit), s!(capacity), C::Above, invalid);
    b.set(local(NEXT), r!(limit));
    b.branch(sub(r!(limit), local(USED)), n(cells), C::Below, end_ready);
    b.set(local(NEXT), add(local(USED), n(cells)));
    b.a.bind_text_label(end_ready);
    b.a.bind_text_label(copy);
    b.branch(local(USED), local(NEXT), C::Equal, finish);
    b.set(local(PREV), add(s!(cells), shl(local(USED), 5)));
    b.set(field(PREV, offset_of!(RootCell, value), 8), n(0));
    b.set(field(PREV, offset_of!(RootCell, active), 0), n(0));
    b.set(local(USED), add(local(USED), n(1)));
    b.a.jmp_label(copy);
    b.a.bind_text_label(finish);
    b.set(r!(cleanup_cursor), local(NEXT));
    b.branch(local(NEXT), r!(limit), C::NotEqual, more);
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

#[allow(dead_code)]
pub(crate) fn emit_scan_leased<A: PortableAsm>(a: &mut A) {
    use crate::gc_abi::{RootScan, RootScanJob};
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    let lease = field(Q, offset_of!(RootScanJob, lease), 0);
    let serial = field(Q, offset_of!(RootScanJob, serial), 0);
    b.pointer(lease.clone(), invalid);
    b.set(local(LEASE), lease);
    b.branch(serial.clone(), n(0), C::Equal, invalid);
    b.branch(serial, l!(serial), C::NotEqual, invalid);
    b.branch(l!(kind), n(1), C::Below, invalid);
    b.branch(l!(kind), n(2), C::Above, invalid);
    b.branch(l!(registration), n(0), C::Equal, invalid);
    b.set(local(REG), l!(registration));
    b.branch(r!(registered), n(1), C::NotEqual, invalid);
    b.branch(r!(scope), l!(scope), C::NotEqual, invalid);
    b.branch(
        r!(segment),
        field(Q, offset_of!(RootScan, segment), 0),
        C::NotEqual,
        invalid,
    );
    let reader = b.a.create_text_label();
    b.branch(l!(kind), n(2), C::NotEqual, reader);
    b.branch(r!(claim_serial), l!(serial), C::NotEqual, invalid);
    b.branch(
        l!(cursor),
        field(Q, offset_of!(RootScan, cursor), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(l!(cursor), r!(claim_end), C::AboveOrEqual, invalid);
    b.branch(
        field(Q, offset_of!(RootScan, capacity), 0),
        sub(r!(claim_end), l!(cursor)),
        C::Above,
        invalid,
    );
    b.a.bind_text_label(reader);
    b.a.leave();
    crate::gc_codegen::roots::emit_transfer(b.a, false);
    b.a.cmp_reg_imm32(R::V0, 1);
    b.a.jcc_label(C::Equal, done);
    b.a.load_ptr_disp32(R::V8, R::V7, offset_of!(RootScanJob, lease) as i32);
    b.a.load_ptr_disp32(R::V9, R::V8, offset_of!(RootLease, kind) as i32);
    b.a.cmp_reg_imm32(R::V9, 2);
    b.a.jcc_label(C::NotEqual, done);
    b.a.load_ptr_disp32(R::V9, R::V7, offset_of!(RootScan, cursor) as i32);
    b.a.store_ptr_disp32(R::V8, offset_of!(RootLease, cursor) as i32, R::V9);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.leave();
    b.a.bind_text_label(done);
}

fn unlink_inventory<A: PortableAsm>(b: &mut Emit<'_, A>, invalid: A::TextLabel) {
    b.set(local(PREV), r!(inventory_previous));
    b.set(local(NEXT), r!(inventory_next));
    let head = b.a.create_text_label();
    let previous_ok = b.a.create_text_label();
    let next_ok = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, head);
    b.pointer(local(PREV), invalid);
    b.branch(
        field(PREV, offset_of!(RootRegistration, inventory_next), 0),
        local(REG),
        C::NotEqual,
        invalid,
    );
    b.a.jmp_label(previous_ok);
    b.a.bind_text_label(head);
    b.branch(g!(inventory), local(REG), C::NotEqual, invalid);
    b.a.bind_text_label(previous_ok);
    b.branch(local(NEXT), n(0), C::Equal, next_ok);
    b.pointer(local(NEXT), invalid);
    b.branch(
        field(NEXT, offset_of!(RootRegistration, inventory_previous), 0),
        local(REG),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(next_ok);
    let write_head = b.a.create_text_label();
    let write_next = b.a.create_text_label();
    let done = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, write_head);
    b.set(
        field(PREV, offset_of!(RootRegistration, inventory_next), 0),
        local(NEXT),
    );
    b.a.jmp_label(write_next);
    b.a.bind_text_label(write_head);
    b.set(g!(inventory), local(NEXT));
    b.a.bind_text_label(write_next);
    b.branch(local(NEXT), n(0), C::Equal, done);
    b.set(
        field(NEXT, offset_of!(RootRegistration, inventory_previous), 0),
        local(PREV),
    );
    b.a.bind_text_label(done);
    b.set(r!(inventory_previous), n(0));
    b.set(r!(inventory_next), n(0));
}

#[path = "allocation_sample.rs"]
pub(crate) mod allocation_sample;
#[path = "budget.rs"]
pub(crate) mod budget;
#[path = "buffers.rs"]
pub(crate) mod buffers;
#[path = "detach.rs"]
pub(crate) mod detach;
#[path = "forward.rs"]
pub(crate) mod forward;
#[path = "mark.rs"]
pub(crate) mod mark;
#[path = "mark_scan.rs"]
pub(crate) mod mark_scan;
#[path = "mark_spill.rs"]
pub(crate) mod mark_spill;
#[path = "metadata.rs"]
pub(crate) mod metadata;
#[path = "native_startup.rs"]
pub(crate) mod native_startup;
#[path = "reference_lease.rs"]
pub(crate) mod reference_lease;
#[path = "source_access.rs"]
pub(crate) mod source_access;
#[path = "root_walk.rs"]
pub(crate) mod walk;

fn unlink_owner<A: PortableAsm>(b: &mut Emit<'_, A>, invalid: A::TextLabel) {
    b.set(local(PREV), r!(owner_previous));
    b.set(local(NEXT), r!(owner_next));
    let head = b.a.create_text_label();
    let prev_ok = b.a.create_text_label();
    let next_ok = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, head);
    b.pointer(local(PREV), invalid);
    b.branch(
        field(PREV, offset_of!(RootRegistration, owner_next), 0),
        local(REG),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(PREV, offset_of!(RootRegistration, context), 0),
        local(CTX),
        C::NotEqual,
        invalid,
    );
    b.a.jmp_label(prev_ok);
    b.a.bind_text_label(head);
    b.branch(ctx!(root_segments), local(REG), C::NotEqual, invalid);
    b.a.bind_text_label(prev_ok);
    b.branch(local(NEXT), n(0), C::Equal, next_ok);
    b.pointer(local(NEXT), invalid);
    b.branch(
        field(NEXT, offset_of!(RootRegistration, owner_previous), 0),
        local(REG),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(NEXT, offset_of!(RootRegistration, context), 0),
        local(CTX),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(next_ok);
    let write_head = b.a.create_text_label();
    let write_next = b.a.create_text_label();
    let done = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, write_head);
    b.set(
        field(PREV, offset_of!(RootRegistration, owner_next), 0),
        local(NEXT),
    );
    b.a.jmp_label(write_next);
    b.a.bind_text_label(write_head);
    b.set(ctx!(root_segments), local(NEXT));
    b.a.bind_text_label(write_next);
    b.branch(local(NEXT), n(0), C::Equal, done);
    b.set(
        field(NEXT, offset_of!(RootRegistration, owner_previous), 0),
        local(PREV),
    );
    b.a.bind_text_label(done);
    b.set(r!(owner_previous), n(0));
    b.set(r!(owner_next), n(0));
}

#[path = "forward_table.rs"]
pub(crate) mod forward_table;

#[path = "resolve.rs"]
pub(crate) mod resolve;

#[path = "relocate.rs"]
pub(crate) mod relocate;

#[path = "page_release.rs"]
pub(crate) mod page_release;

#[path = "barrier.rs"]
pub(crate) mod barrier;

#[path = "root_follow.rs"]
pub(crate) mod root_follow;

#[path = "mark_prepare.rs"]
pub(crate) mod mark_prepare;

#[path = "mark_retarget.rs"]
pub(crate) mod mark_retarget;

#[path = "worker.rs"]
pub(crate) mod worker;

#[path = "root_journal.rs"]
pub(crate) mod root_journal;

#[path = "buffer_job.rs"]
pub(crate) mod buffer_job;

#[path = "root_audit.rs"]
pub(crate) mod root_audit;

#[path = "mark_end.rs"]
pub(crate) mod mark_end;

#[path = "mark_phase.rs"]
pub(crate) mod mark_phase;

#[path = "relocation_phase.rs"]
pub(crate) mod relocation_phase;

#[path = "cycle.rs"]
pub(crate) mod cycle;

#[path = "remembered.rs"]
pub(crate) mod remembered;

#[path = "remembered_inventory.rs"]
pub(crate) mod remembered_inventory;

#[path = "remembered_load.rs"]
pub(crate) mod remembered_load;

#[path = "mark_route.rs"]
pub(crate) mod mark_route;

#[path = "remembered_transfer.rs"]
pub(crate) mod remembered_transfer;

#[path = "remembered_owner.rs"]
pub(crate) mod remembered_owner;

#[path = "remembered_walk.rs"]
pub(crate) mod remembered_walk;

#[path = "remembered_lease.rs"]
pub(crate) mod remembered_lease;

#[path = "root_access.rs"]
pub(crate) mod root_access;

#[path = "source_drain.rs"]
pub(crate) mod source_drain;

#[path = "source_uncommit.rs"]
pub(crate) mod source_uncommit;

#[path = "forward_incoming.rs"]
pub(crate) mod forward_incoming;

#[path = "relocation_gate.rs"]
pub(crate) mod relocation_gate;

#[path = "relocation_job.rs"]
pub(crate) mod relocation_job;

#[path = "relocation_complete.rs"]
pub(crate) mod relocation_complete;

#[path = "relocation_claim.rs"]
pub(crate) mod relocation_claim;

#[path = "relocate_discard.rs"]
pub(crate) mod relocate_discard;

#[path = "relocation_cancel.rs"]
pub(crate) mod relocation_cancel;

#[path = "alloc_into.rs"]
pub(crate) mod alloc_into;

#[path = "initialization.rs"]
pub(crate) mod initialization;

#[path = "metadata_grow.rs"]
pub(crate) mod metadata_grow;

#[path = "mark_scan_commit.rs"]
pub(crate) mod mark_scan_commit;

#[path = "root_copy.rs"]
pub(crate) mod root_copy;

#[path = "heap_pressure.rs"]
pub(crate) mod heap_pressure;

#[path = "survivor_age.rs"]
pub(crate) mod survivor_age;

#[path = "promotion_remember.rs"]
pub(crate) mod promotion_remember;

#[path = "relocation_turn.rs"]
pub(crate) mod relocation_turn;

#[path = "forward_compress.rs"]
pub(crate) mod forward_compress;

#[path = "heap_incoming.rs"]
pub(crate) mod heap_incoming;

#[path = "heap_walk.rs"]
pub(crate) mod heap_walk;

#[path = "object_walk.rs"]
pub(crate) mod object_walk;

#[path = "heap_audit_slice.rs"]
pub(crate) mod heap_audit_slice;

#[path = "heap_audit.rs"]
pub(crate) mod heap_audit;

#[path = "rooted_observation.rs"]
pub(crate) mod rooted_observation;

#[path = "rooted_mark.rs"]
pub(crate) mod rooted_mark;

#[path = "rooted_mark_set.rs"]
pub(crate) mod rooted_mark_set;

#[path = "rooted_mark_publish.rs"]
pub(crate) mod rooted_mark_publish;

#[path = "rooted_load.rs"]
pub(crate) mod rooted_load;

#[path = "rooted_load_admit.rs"]
pub(crate) mod rooted_load_admit;

#[path = "rooted_load_heal.rs"]
pub(crate) mod rooted_load_heal;

#[path = "cycle_pressure.rs"]
pub(crate) mod cycle_pressure;

#[path = "cycle_mark_start.rs"]
pub(crate) mod cycle_mark_start;

#[path = "source_retire.rs"]
pub(crate) mod source_retire;

#[path = "rooted_scalar.rs"]
pub(crate) mod rooted_scalar;

#[path = "rooted_store_commit.rs"]
pub(crate) mod rooted_store_commit;

#[path = "rooted_store_attempt.rs"]
pub(crate) mod rooted_store_attempt;

#[path = "rooted_store.rs"]
pub(crate) mod rooted_store;

#[path = "rooted_store_dispatch.rs"]
pub(crate) mod rooted_store_dispatch;

#[path = "barrier_poll.rs"]
pub(crate) mod barrier_poll;

#[path = "barrier_assist.rs"]
pub(crate) mod barrier_assist;

#[path = "native_root_publish.rs"]
pub(crate) mod native_root_publish;

#[path = "pending_follow.rs"]
pub(crate) mod pending_follow;

#[path = "pending_access.rs"]
pub(crate) mod pending_access;

#[path = "pending_mark.rs"]
pub(crate) mod pending_mark;

#[path = "weak_load.rs"]
pub(crate) mod weak_load;

#[path = "weak_release.rs"]
pub(crate) mod weak_release;

#[path = "weak_liveness.rs"]
pub(crate) mod weak_liveness;

#[path = "weak_follow.rs"]
pub(crate) mod weak_follow;

#[path = "weak_incoming.rs"]
pub(crate) mod weak_incoming;

#[path = "weak_table_access.rs"]
pub(crate) mod weak_table_access;

#[path = "weak_create.rs"]
pub(crate) mod weak_create;

#[path = "weak_mark.rs"]
pub(crate) mod weak_mark;

#[path = "weak_capture.rs"]
pub(crate) mod weak_capture;

#[path = "weak_access.rs"]
pub(crate) mod weak_access;

#[path = "weak_finish.rs"]
pub(crate) mod weak_finish;

#[path = "weak_scan.rs"]
pub(crate) mod weak_scan;

#[path = "weak_relocation.rs"]
pub(crate) mod weak_relocation;

#[path = "pin_registry.rs"]
pub(crate) mod pin_registry;

#[path = "pin_relocation.rs"]
pub(crate) mod pin_relocation;

#[path = "pin_source_release.rs"]
pub(crate) mod pin_source_release;

#[path = "root_pin.rs"]
pub(crate) mod root_pin;

#[path = "pin_dispatch.rs"]
pub(crate) mod pin_dispatch;

#[path = "native_pin.rs"]
pub(crate) mod native_pin;

#[path = "native_pin_close.rs"]
pub(crate) mod native_pin_close;

#[path = "native_pin_progress.rs"]
pub(crate) mod native_pin_progress;

#[path = "native_pin_wait.rs"]
pub(crate) mod native_pin_wait;

#[path = "root_finish_notify.rs"]
pub(crate) mod root_finish_notify;

#[path = "native_source_uncommit.rs"]
pub(crate) mod native_source_uncommit;
