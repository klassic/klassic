//! Covered weak scan -> relocation adoption under the common lock.
use super::*;
use crate::gc_abi::{
    RelocationPhaseControl as Relocation, WeakLoadGate as Gate, WeakMarkControl as Owner,
    WeakProcess, WeakScan as Scan,
};
use std::mem::size_of;
// Match weak_scan::validate's private frame contract. It preserves the input
// registry and relocation pointers while validating the captured scan owner.
const SCAN: i32 = 104;
const REGISTRY: i32 = 112;
const OWNER: i32 = 120;
const WORK: i32 = 144;
const RELOCATION: i32 = 152;
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let wait = a.create_text_label();
    let done = a.create_text_label();
    let phase = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 48);
    a.store_rbp_slot(SCAN, R::V7);
    a.store_rbp_slot(REGISTRY, R::V6);
    a.store_rbp_slot(RELOCATION, R::V2);
    let mut b = Emit { a };
    for slot in [SCAN, REGISTRY, RELOCATION] {
        b.pointer(local(slot), invalid);
    }
    b.set(
        local(WORK),
        add(local(SCAN), n(offset_of!(Scan, work) as u64)),
    );
    super::weak_scan::validate(&mut b, invalid);
    for (left, right) in [
        (local(REGISTRY), field(REG, offset_of!(Gate, registry), 0)),
        (
            field(RELOCATION, offset_of!(Relocation, handshake), 0),
            field(REG, offset_of!(Gate, handshake), 0),
        ),
        (
            field(RELOCATION, offset_of!(Relocation, generation), 0),
            field(SCAN, offset_of!(Scan, generation), 0),
        ),
    ] {
        b.branch(left, right, C::NotEqual, invalid);
    }
    b.branch(
        field(SCAN, offset_of!(Scan, stage), 0),
        n(1),
        C::Equal,
        wait,
    );
    b.branch(
        field(SCAN, offset_of!(Scan, stage), 0),
        n(2),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(SCAN, offset_of!(Scan, cursor), 0),
        field(SCAN, offset_of!(Scan, capacity), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(OWNER, offset_of!(Owner, processor), 0),
        n(0),
        C::NotEqual,
        wait,
    );
    for off in (0..size_of::<WeakProcess>()).step_by(8) {
        b.branch(field(WORK, off, 0), n(0), C::NotEqual, invalid);
    }
    b.a.load_rbp_slot(R::V7, REGISTRY);
    b.a.load_rbp_slot(R::V6, RELOCATION);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(phase);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, done);
    b.set(field(OWNER, offset_of!(Owner, scan), 0), n(0));
    b.set(field(OWNER, offset_of!(Owner, stage), 0), n(0));
    b.set(field(REG, offset_of!(Gate, closed), 0), n(0));
    b.set(field(SCAN, offset_of!(Scan, stage), 0), n(5));
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    for (label, status) in [(invalid, 2), (wait, 1)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(phase);
    super::relocation_phase::emit(b.a);
    b.a.ret();
}
