//! Observations and invalid caller probes of the actual participant registry.
use super::super::*;
use crate::gc_abi::{HandshakeState as H, ThreadRegistration as T};
use crate::gc_codegen::production::{lea_rt, load_rt};

#[derive(Clone, Copy)]
pub(crate) enum ParticipantMetric {
    Members,
    Native,
    Generation,
    Expected,
    Acknowledged,
    Active,
    OwnerGeneration,
    Registered,
    Identity,
}
fn field(metric: ParticipantMetric) -> usize {
    use ParticipantMetric::*;
    match metric {
        Members => participants::HANDSHAKE + offset_of!(H, members),
        Native => participants::HANDSHAKE + offset_of!(H, native),
        Generation => participants::HANDSHAKE + offset_of!(H, generation),
        Expected => participants::HANDSHAKE + offset_of!(H, expected),
        Acknowledged => participants::HANDSHAKE + offset_of!(H, acknowledged),
        Active => participants::HANDSHAKE + offset_of!(H, active),
        OwnerGeneration => participants::OWNER + offset_of!(T, acknowledged_generation),
        Registered => participants::OWNER + offset_of!(T, registered),
        Identity => participants::OWNER + offset_of!(T, identity),
    }
}
pub(crate) fn participant_metric<E: PortableAsm>(a: &mut E, metric: ParticipantMetric) {
    load_rt(a, R::V0, field(metric));
}
/// Test-invalid metadata, followed only by a rejection probe.
pub(crate) fn participant_set<E: PortableAsm>(a: &mut E, metric: ParticipantMetric, value: u64) {
    store_rt_imm(a, field(metric), value);
}
/// The actual finite registry operation, returning its status to the harness.
/// The caller takes the same collector lock as the production wrappers.
pub(crate) fn participant_step(g: &mut crate::NativeCodeGenerator, operation: u64) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    lea_rt(g, R::V7, participants::HANDSHAKE);
    lea_rt(g, R::V6, participants::OWNER);
    g.mov_imm64(R::V2, operation);
    g.call_label(runtime.labels.participant_step);
    g.push_reg(R::V0);
    g.push_reg(R::V0);
    worker::unlock(g, &runtime.labels);
    g.pop_reg(R::V0);
    g.pop_reg(R::V0);
    g.gc_gen = Some(runtime);
}

/// Register additional real native context/entry storage, without pretending
/// they allocate or run as mutators. Their missing acknowledgement must stop
/// the current sole-owner phase coordinator before it publishes a new phase.
pub(crate) fn participant_add_unacknowledged(g: &mut crate::NativeCodeGenerator, total: u64) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    for _ in 1..total {
        g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
        let ctx = g
            .asm
            .data_label_with_i64s(&vec![0; size_of::<MutatorContext>() / 8]);
        let reg = g.asm.data_label_with_i64s(&vec![0; size_of::<T>() / 8]);
        g.mov_data_addr(R::V0, ctx);
        g.mov_imm64(R::V1, ABI_VERSION as u64);
        g.store_ptr_disp32(R::V0, 0, R::V1);
        g.mov_data_addr(R::V6, reg);
        g.store_ptr_disp32(R::V6, offset_of!(T, context) as i32, R::V0);
        lea_rt(g, R::V7, participants::HANDSHAKE);
        g.mov_imm64(R::V2, 0);
        g.call_label(runtime.labels.participant_step);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, runtime.labels.fail);
    }
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

/// Exercise the same checked atomic publication used by production phases.
/// Rejection is caught for observation; it must leave the snapshot untouched.
pub(crate) fn participant_adopt(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().expect("generational runtime");
    worker::lock(g, &runtime.labels);
    let rejected = g.create_text_label();
    let done = g.create_text_label();
    lea_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    g.load_ptr_disp32(R::V0, R::V1, 0);
    g.add_reg_imm32(R::V0, 0x10000);
    participants::adopt(g, rejected);
    g.mov_imm64(R::V0, 1);
    g.jmp_label(done);
    g.bind_text_label(rejected);
    g.mov_imm64(R::V0, 0);
    g.bind_text_label(done);
    g.push_reg(R::V0);
    g.push_reg(R::V0);
    worker::unlock(g, &runtime.labels);
    g.pop_reg(R::V0);
    g.pop_reg(R::V0);
    g.gc_gen = Some(runtime);
}
