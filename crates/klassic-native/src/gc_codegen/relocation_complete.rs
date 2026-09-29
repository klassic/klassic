//! Resumable installed-relocation completion using shared runtime operations.
use super::*;
use crate::gc_abi::{ForwardCell, RelocationCompletion, RelocationCopy, RelocationJob};
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(RelocationCompletion, $f), 0)
    };
}
macro_rules! job {
    ($f:ident) => {
        field(REG, offset_of!(RelocationJob, $f), 0)
    };
}
macro_rules! copy {
    ($f:ident) => {
        field(SEG, offset_of!(RelocationCopy, $f), 0)
    };
}
fn job_call<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel, op: u64) {
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, REG);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
}
#[allow(dead_code)]
pub(crate) fn emit_relocation_complete<A: PortableAsm>(a: &mut A) {
    let status: [_; 4] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let inspect = a.create_text_label();
    let finish = a.create_text_label();
    let release = a.create_text_label();
    let checked = a.create_text_label();
    let resumed = a.create_text_label();
    let ready = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let job_step = a.create_text_label();
    let publish = a.create_text_label();
    let gate = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 32);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.branch(req!(stage), n(3), C::Above, invalid);
    b.branch(req!(stage), n(0), C::NotEqual, resumed);
    b.branch(req!(owner), n(0), C::NotEqual, invalid);
    b.a.jmp_label(ready);
    b.a.bind_text_label(resumed);
    b.branch(req!(owner), local(Q), C::NotEqual, invalid);
    b.a.bind_text_label(ready);
    b.pointer(req!(job), invalid);
    b.pointer(req!(copy), invalid);
    b.set(local(REG), req!(job));
    b.set(local(SEG), req!(copy));
    b.pointer(job!(source), invalid);
    b.set(local(CTX), job!(source));
    b.branch(job!(ticket), n(0), C::Equal, invalid);
    b.branch(copy!(ticket), job!(ticket), C::NotEqual, invalid);
    b.branch(copy!(stage), n(2), C::NotEqual, invalid);
    b.branch(
        copy!(destination),
        shr(job!(destination), 16),
        C::NotEqual,
        invalid,
    );
    b.branch(
        copy!(source_incarnation),
        field(CTX, offset_of!(ForwardCell, incarnation), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(req!(stage), n(1), C::Equal, inspect);
    b.branch(req!(stage), n(2), C::Equal, finish);
    b.branch(req!(stage), n(3), C::Equal, release);
    job_call(&mut b, job_step, 0);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.set(req!(owner), local(Q));
    b.set(req!(stage), n(1));
    b.a.jmp_label(status[2]);
    b.a.bind_text_label(inspect);
    job_call(&mut b, job_step, 1);
    b.branch(local(STATE), n(2), C::NotEqual, checked);
    b.value(and(job!(destination), n(0xffff)));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.load_rbp_slot(R::V7, CTX);
    b.a.load_rbp_slot(R::V6, SEG);
    b.a.call_label(publish);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(3), C::Equal, status[3]);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    job_call(&mut b, job_step, 1);
    b.a.bind_text_label(checked);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.set(req!(stage), n(2));
    b.a.jmp_label(status[2]);
    b.a.bind_text_label(finish);
    job_call(&mut b, job_step, 2);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.set(req!(stage), n(3));
    b.a.jmp_label(status[2]);
    b.a.bind_text_label(release);
    b.branch(job!(owner), local(REG), C::NotEqual, invalid);
    b.branch(job!(gate), local(G), C::NotEqual, invalid);
    b.branch(job!(state), n(3), C::NotEqual, invalid);
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 144);
    b.a.store_rbp_slot(NEXT, R::V0);
    for (offset, value) in [(0, job!(permit)), (8, job!(serial)), (16, job!(epoch))] {
        b.set(field(NEXT, offset, 0), value);
    }
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, NEXT);
    b.a.mov_imm64(R::V2, 1);
    b.a.call_label(gate);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.set(req!(stage), n(4));
    b.a.jmp_label(status[0]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(job_step);
    super::relocation_job::emit_relocation_job(b.a);
    b.a.ret();
    b.a.bind_text_label(publish);
    super::relocate::emit_relocate_publish(b.a);
    b.a.ret();
    b.a.bind_text_label(gate);
    super::relocation_gate::emit_relocation_gate(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
