//! Bounded membership-retaining page walk; gc_remembered_walk.h.
use super::*;
use crate::gc_abi::{
    RememberedInventory as I, RememberedInventoryRequest as Request, RememberedJob,
    RememberedRegistration as Node, RememberedWalk as Walk,
};
fn validate<A: PortableAsm>(b: &mut Emit<'_, A>, core: A::TextLabel, bad: A::TextLabel) {
    b.set(field(CTX, offset_of!(Request, registration), 0), local(REG));
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, CTX);
    b.a.mov_imm64(R::V2, 8);
    b.a.call_label(core);
    b.a.store_rbp_slot(KIND, R::V0);
    b.branch(local(KIND), n(0), C::NotEqual, bad);
}
#[allow(dead_code)]
pub(crate) fn emit_walk<A: PortableAsm>(a: &mut A) {
    let status: [_; 4] = std::array::from_fn(|_| a.create_text_label());
    let bad = status[1];
    let begin = a.create_text_label();
    let release = a.create_text_label();
    let done = a.create_text_label();
    let core = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 96);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! g {
        ($f:ident) => {
            field(G, offset_of!(I, $f), 0)
        };
    }
    macro_rules! w {
        ($f:ident) => {
            field(Q, offset_of!(Walk, $f), 0)
        };
    }
    macro_rules! r {
        ($f:ident) => {
            field(REG, offset_of!(Node, $f), 0)
        };
    }
    b.branch(local(OP), n(2), C::Above, bad);
    super::remembered_inventory::valid(&mut b, G, bad);
    b.pointer(local(Q), bad);
    for (slot, displacement) in [(LEASE, 160), (CTX, 192)] {
        b.a.mov_reg_reg(R::V0, R::V5);
        b.a.sub_reg_imm32(R::V0, displacement);
        b.a.store_rbp_slot(slot, R::V0);
    }
    for off in (0..std::mem::size_of::<RememberedJob>()).step_by(8) {
        b.set(field(LEASE, off, 0), n(0));
    }
    b.set(field(CTX, offset_of!(Request, job), 0), local(LEASE));
    b.set(field(CTX, offset_of!(Request, phase), 0), n(0));
    b.branch(local(OP), n(0), C::Equal, begin);
    b.branch(w!(registry), local(G), C::NotEqual, bad);
    b.branch(g!(walkers), n(0), C::Equal, bad);
    b.branch(w!(epoch), g!(epoch), C::NotEqual, bad);
    b.branch(w!(identity), n(0), C::Equal, bad);
    b.branch(w!(identity), w!(limit), C::Above, bad);
    b.branch(w!(limit), g!(last_identity), C::Above, bad);
    b.set(local(REG), w!(registration));
    validate(&mut b, core, bad);
    b.branch(r!(identity), w!(identity), C::NotEqual, bad);
    b.branch(local(OP), n(2), C::Equal, release);
    b.branch(r!(next), n(0), C::Equal, release);
    b.set(local(REG), r!(next));
    validate(&mut b, core, bad);
    b.set(w!(registration), local(REG));
    b.set(w!(identity), r!(identity));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(release);
    b.set(g!(walkers), sub(g!(walkers), n(1)));
    for off in (0..std::mem::size_of::<Walk>()).step_by(8) {
        b.set(field(Q, off, 0), n(0));
    }
    b.a.jmp_label(status[2]);
    b.a.bind_text_label(begin);
    for off in (0..std::mem::size_of::<Walk>()).step_by(8) {
        b.branch(field(Q, off, 0), n(0), C::NotEqual, bad);
    }
    b.branch(g!(head), n(0), C::Equal, status[2]);
    b.branch(g!(walkers), n(u64::MAX), C::Equal, status[3]);
    b.set(local(REG), g!(head));
    validate(&mut b, core, bad);
    b.set(w!(registry), local(G));
    b.set(w!(registration), local(REG));
    b.set(w!(identity), r!(identity));
    b.set(w!(limit), g!(last_identity));
    b.set(w!(epoch), g!(epoch));
    b.set(g!(walkers), add(g!(walkers), n(1)));
    b.a.jmp_label(status[0]);
    for (code, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.set(local(KIND), n(code as u64));
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.load_rbp_slot(R::V0, KIND);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(core);
    super::remembered_inventory::emit_inventory(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
