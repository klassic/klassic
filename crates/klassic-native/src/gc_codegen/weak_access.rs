//! Guarded weak processing ENTER/OBSERVE/EXIT; gc_weak_process_access_step.
use super::*;
use crate::gc_abi::{
    GuardedMarkPrepare as Probe, MarkPhaseControl as Mark, MetadataLeaseRequest, ReferenceLease,
    ResolveRequest, SourceAccessRequest, WeakLoadGate as Gate, WeakMarkControl as Owner,
    WeakProcess as Process, WeakTable as Table,
};
use std::mem::size_of;
const PROCESS: i32 = 104;
const CONTEXT: i32 = 112;
const OWNER: i32 = 120;
const MARK: i32 = 128;
const ACCESS: i32 = 136;
const CURRENT: i32 = 144;
const RETIRED: i32 = 152;
const PROBE: i32 = 160;

fn process(f: usize) -> E {
    field(PROCESS, f, 0)
}
fn clear_access<A: PortableAsm>(b: &mut Emit<'_, A>) {
    for off in (0..size_of::<ResolveRequest>()).step_by(8) {
        b.set(field(ACCESS, off, 0), n(0));
    }
}

#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let result: [_; 11] = std::array::from_fn(|_| a.create_text_label());
    let invalid = result[2];
    let exit = a.create_text_label();
    let exit_stage = a.create_text_label();
    let exited = a.create_text_label();
    let enter = a.create_text_label();
    let observe = a.create_text_label();
    let check_owner = a.create_text_label();
    let followed = a.create_text_label();
    let access_ready = a.create_text_label();
    let live = a.create_text_label();
    let observed = a.create_text_label();
    let pending = a.create_text_label();
    let done = a.create_text_label();
    let metadata = a.create_text_label();
    let follow = a.create_text_label();
    let access = a.create_text_label();
    let resolve = a.create_text_label();
    let liveness = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    for bytes in [112, 112, 48] {
        a.sub_reg_imm8(R::V4, bytes);
    }
    a.store_rbp_slot(PROCESS, R::V7);
    a.store_rbp_slot(OP, R::V6);
    a.store_rbp_slot(STATE, R::V2);
    a.mov_reg_reg(R::V0, R::V5);
    a.sub_reg_imm32(R::V0, 272);
    a.store_rbp_slot(PROBE, R::V0);
    let mut b = Emit { a };
    b.pointer(local(PROCESS), invalid);
    b.branch(local(OP), n(2), C::Above, invalid);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    for (slot, off) in [
        (ACCESS, offset_of!(Process, access)),
        (CURRENT, offset_of!(Process, current)),
        (RETIRED, offset_of!(Process, retired)),
    ] {
        b.set(local(slot), add(local(PROCESS), n(off as u64)));
    }
    // EXIT intentionally does not inspect the frozen owner or current phase.
    b.branch(local(OP), n(2), C::Equal, exit);
    b.branch(local(OP), n(0), C::Equal, enter);
    b.branch(
        process(offset_of!(Process, stage)),
        n(2),
        C::NotEqual,
        invalid,
    );
    b.a.jmp_label(check_owner);
    b.a.bind_text_label(enter);
    b.branch(
        process(offset_of!(Process, stage)),
        n(1),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(check_owner);
    b.set(local(CONTEXT), process(offset_of!(Process, context)));
    b.pointer(local(CONTEXT), invalid);
    b.branch(
        field(CONTEXT, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CONTEXT, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(CONTEXT, offset_of!(MutatorContext, state), 4),
        n(0),
        C::NotEqual,
        invalid,
    );
    let context_ready = b.a.create_text_label();
    b.branch(local(OP), n(1), C::Equal, context_ready);
    b.branch(
        field(CONTEXT, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(context_ready);
    validate_process_owner(&mut b, PROCESS, CONTEXT, OWNER, MARK, invalid);
    b.branch(local(OP), n(1), C::Equal, observe);
    b.branch(
        field(RETIRED, offset_of!(MetadataLeaseRequest, token), 0),
        n(0),
        C::Equal,
        pending,
    );
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, RETIRED);
    b.a.mov_imm64(R::V2, 1);
    b.a.call_label(metadata);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, result[10]);
    b.a.bind_text_label(pending);
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, CURRENT);
    b.a.load_rbp_slot(R::V2, RETIRED);
    b.a.call_label(follow);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::Equal, followed);
    for (status, mapped) in [(3, 5), (2, 1), (5, 3), (6, 4), (7, 10)] {
        b.branch(local(STATE), n(status), C::Equal, result[mapped]);
    }
    b.a.jmp_label(invalid);
    b.a.bind_text_label(followed);
    clear_access(&mut b);
    b.set(
        field(
            ACCESS,
            offset_of!(ResolveRequest, scope) + offset_of!(SourceAccessRequest, context),
            0,
        ),
        local(CONTEXT),
    );
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, CURRENT);
    b.a.load_rbp_slot(R::V2, ACCESS);
    b.a.call_label(access);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(8), C::Equal, access_ready);
    clear_access(&mut b);
    for (status, mapped) in [(3, 5), (2, 1), (6, 4)] {
        b.branch(local(STATE), n(status), C::Equal, result[mapped]);
    }
    b.a.jmp_label(invalid);
    b.a.bind_text_label(access_ready);
    b.set(process(offset_of!(Process, stage)), n(2));
    b.a.jmp_label(result[0]);

    b.a.bind_text_label(observe);
    for off in (0..size_of::<Probe>()).step_by(8) {
        b.set(field(PROBE, off, 0), n(0));
    }
    for (off, value) in [
        (
            offset_of!(Probe, scope),
            add(local(ACCESS), n(offset_of!(ResolveRequest, scope) as u64)),
        ),
        (
            offset_of!(Probe, page),
            field(CURRENT, offset_of!(ReferenceLease, lease), 0),
        ),
        (
            offset_of!(Probe, phase),
            process(offset_of!(Process, phase)),
        ),
        (
            offset_of!(Probe, reference),
            field(CURRENT, offset_of!(ReferenceLease, reference), 0),
        ),
    ] {
        b.set(field(PROBE, off, 0), value);
    }
    for off in (0..size_of::<MetadataLeaseRequest>()).step_by(8) {
        b.set(
            field(PROBE, offset_of!(Probe, lease) + off, 0),
            field(CURRENT, offset_of!(ReferenceLease, lease) + off, 0),
        );
    }
    b.value(process(offset_of!(Process, generation)));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(process(offset_of!(Process, epoch)));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.load_rbp_slot(R::V7, PROBE);
    b.a.call_label(liveness);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(3), C::Equal, result[1]);
    b.branch(local(STATE), n(0), C::Equal, live);
    b.branch(local(STATE), n(1), C::NotEqual, invalid);
    b.set(process(offset_of!(Process, result)), n(6));
    b.a.jmp_label(observed);
    b.a.bind_text_label(live);
    b.set(process(offset_of!(Process, result)), n(7));
    b.a.bind_text_label(observed);
    b.set(process(offset_of!(Process, stage)), n(3));
    b.value(process(offset_of!(Process, result)));
    b.a.jmp_label(done);

    b.a.bind_text_label(exit);
    b.branch(
        process(offset_of!(Process, stage)),
        n(2),
        C::Equal,
        exit_stage,
    );
    b.branch(
        process(offset_of!(Process, stage)),
        n(3),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(exit_stage);
    b.value(field(CURRENT, offset_of!(ReferenceLease, lease), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, ACCESS);
    b.a.mov_imm64(R::V2, 1);
    b.a.call_label(resolve);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    clear_access(&mut b);
    b.branch(process(offset_of!(Process, stage)), n(3), C::Equal, exited);
    b.set(process(offset_of!(Process, stage)), n(1));
    b.a.jmp_label(result[0]);
    b.a.bind_text_label(exited);
    b.set(process(offset_of!(Process, stage)), n(4));
    b.a.jmp_label(result[0]);
    for (value, label) in result.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
    b.a.bind_text_label(follow);
    super::pending_follow::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(access);
    super::pending_access::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(resolve);
    super::resolve::emit_resolve_access(b.a);
    b.a.ret();
    b.a.bind_text_label(liveness);
    super::weak_liveness::emit(b.a);
    b.a.ret();
}

// Caller has checked the process context. G receives its metadata pool;
// REG is the completed owner's gate, SEG its table. Other locals are retained.
pub(super) fn validate_process_owner<A: PortableAsm>(
    b: &mut Emit<'_, A>,
    process: i32,
    context: i32,
    owner: i32,
    mark: i32,
    invalid: A::TextLabel,
) {
    b.set(local(owner), field(process, offset_of!(Process, owner), 0));
    b.pointer(local(owner), invalid);
    b.branch(
        field(owner, offset_of!(Owner, processor), 0),
        local(process),
        C::NotEqual,
        invalid,
    );
    b.set(local(REG), field(owner, offset_of!(Owner, gate), 0));
    super::weak_capture::validate_owner(
        b,
        owner,
        REG,
        mark,
        field(process, offset_of!(Process, phase), 0),
        invalid,
    );
    for (mo, po) in [
        (offset_of!(Mark, epoch), offset_of!(Process, epoch)),
        (
            offset_of!(Mark, generation),
            offset_of!(Process, generation),
        ),
        (
            offset_of!(Mark, rendezvous),
            offset_of!(Process, rendezvous),
        ),
    ] {
        b.branch(
            field(mark, mo, 0),
            field(process, po, 0),
            C::NotEqual,
            invalid,
        );
    }
    b.set(local(SEG), field(process, offset_of!(Process, table), 0));
    b.pointer(local(SEG), invalid);
    b.branch(
        field(SEG, offset_of!(Table, gate), 0),
        local(REG),
        C::NotEqual,
        invalid,
    );
    b.set(local(G), field(process, offset_of!(Process, pool), 0));
    b.branch(
        field(REG, offset_of!(Gate, pool), 0),
        local(G),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(context, offset_of!(MutatorContext, adopted_phase), 8),
        field(process, offset_of!(Process, phase), 0),
        C::NotEqual,
        invalid,
    );
}
