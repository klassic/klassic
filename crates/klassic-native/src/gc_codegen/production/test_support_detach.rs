//! Actual production owner detach/disposal entry calls, never manufactured state.
use super::super::*;
use super::TestActor;
use crate::gc_abi::ThreadRegistration;

fn select(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
}
pub(crate) fn actor_detach_call(g: &mut crate::NativeCodeGenerator, actor: TestActor, begin: bool) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = if begin {
        l.detach_begin_actor
    } else {
        l.detach_step_actor
    };
    select(g, actor);
    g.call_label(entry);
    g.pop_reg(R::OwnerContext);
}
pub(crate) fn actor_detach_metric(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    field: usize,
) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    // Observe under the caller's shared ticket; never borrow another live
    // thread's owner-lock flag or claim its private execution identity.
    select(g, actor);
    match field {
        0 => {
            lea_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, state));
            g.atomic_load_acquire_u32(R::V0, R::V1);
        }
        1 => load_rt(
            g,
            R::V0,
            participants::OWNER + offset_of!(ThreadRegistration, registered),
        ),
        2 => load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, root_segments)),
        3 => load_rt(
            g,
            R::V0,
            RT_CTX + offset_of!(MutatorContext, retiring_roots),
        ),
        4 => load_rt(
            g,
            R::V0,
            owner_state::offset(
                offset_of!(OwnerState, detach) + offset_of!(detach::Owner, submitted),
            ),
        ),
        5 => load_rt(
            g,
            R::V0,
            owner_state::offset(
                offset_of!(OwnerState, detach) + offset_of!(detach::Owner, finished),
            ),
        ),
        6 => load_rt(g, R::V0, detach::HANDLES),
        _ => unreachable!(),
    }
    g.pop_reg(R::OwnerContext);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}
pub(crate) fn actor_detach_wait(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 20_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1976, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    actor_detach_call(g, actor, false);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, fail);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1976);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
pub(crate) fn actor_detach_dispose(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let entry = g.gc_gen.as_ref().unwrap().labels.detach_dispose_actor;
    g.mov_data_addr(R::V7, actor.pointer);
    g.load_ptr_disp32(R::V7, R::V7, 0);
    g.call_label(entry);
}
/// Drive the real resumable disposal contract. Another OS thread can retain
/// a native lookup cursor even after this actor has cleared TLS and exited.
pub(crate) fn actor_detach_dispose_wait(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, poll, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.poll_actor,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 20_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1976, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    actor_detach_dispose(g, actor);
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, done);
    g.call_label(poll);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1976);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
pub(crate) fn actor_detach_seed(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    extra: usize,
) -> crate::DataLabel {
    let (alloc, adopt, publish, load) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (
            l.alloc_actor,
            l.root_adopt_actor,
            l.publish_pending,
            l.root_load_actor,
        )
    };
    select(g, actor);
    g.mov_imm64(R::V7, 24);
    g.mov_imm64(R::V6, 1);
    g.call_label(alloc);
    g.mov_imm64(R::V1, 84);
    g.store_ptr_disp32(R::V0, 0, R::V1);
    let source = g.asm.data_label_with_i64s(&[0]);
    g.mov_data_addr(R::V1, source);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.mov_reg_reg(R::V0, R::V1);
    g.call_label(adopt);
    g.call_label(publish);
    for _ in 0..extra {
        let region = super::journal_region_new(g, 8);
        g.mov_data_addr(R::V0, source);
        g.load_ptr_disp32(R::V0, R::V0, 0);
        g.call_label(load);
        super::journal_region_store(g, region, false);
    }
    g.pop_reg(R::OwnerContext);
    source
}
pub(crate) fn actor_native_release(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    handle: crate::DataLabel,
) {
    select(g, actor);
    let (poll, relax) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (
            l.owner_checkpoint_poll,
            l.worker.as_ref().unwrap().hooks.relax,
        )
    };
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    super::native_root_call(g, handle, 2);
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, done);
    g.call_label(poll);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    g.pop_reg(R::OwnerContext);
}

pub(crate) fn detach_wait_metric(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    field: usize,
    expected: u64,
) {
    let (clock, relax, fail) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (
            l.worker.as_ref().unwrap().hooks.clock,
            l.worker.as_ref().unwrap().hooks.relax,
            l.fail,
        )
    };
    g.call_label(clock);
    g.mov_imm64(R::V1, 30_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1976, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    actor_detach_metric(g, actor, field);
    g.cmp_reg_imm32(R::V0, expected as i32);
    g.jcc_label(C::Equal, done);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1976);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
pub(crate) fn actor_frame_reader(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
) -> crate::DataLabel {
    use crate::gc_abi::{RootLease, RootLifeRequest, RootRegistration};
    let q = g.asm.data_label_with_i64s(&vec![
        0;
        (size_of::<RootLifeRequest>()
            + size_of::<RootLease>())
            / 8
    ]);
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    g.add_reg_imm32(R::V0, d(offset_of!(OwnerState, roots)));
    g.mov_data_addr(R::V6, q);
    g.store_ptr_disp32(R::V6, d(offset_of!(RootLifeRequest, registration)), R::V0);
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootRegistration, scope)));
    g.store_ptr_disp32(R::V6, d(offset_of!(RootLifeRequest, scope)), R::V0);
    g.mov_reg_reg(R::V0, R::V6);
    g.add_reg_imm32(R::V0, size_of::<RootLifeRequest>() as i32);
    g.store_ptr_disp32(R::V6, d(offset_of!(RootLifeRequest, lease)), R::V0);
    lea_rt(g, R::V7, registered_roots::REGISTRY);
    g.mov_imm64(R::V2, 1);
    g.call_label(rt.labels.root_life_step);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, rt.labels.fail);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
    q
}
/// The test holds a real reader over this cell's storage; inspect it through
/// the actual primary load barrier after each completed physical move.
pub(crate) fn detach_retained_value(g: &mut crate::NativeCodeGenerator, source: crate::DataLabel) {
    let entry = g.gc_gen.as_ref().unwrap().labels.field_load_actor;
    g.mov_data_addr(R::V10, source);
    g.load_ptr_disp32(R::V10, R::V10, 0);
    g.load_ptr_disp32(R::V0, R::V10, 0);
    g.call_label(entry);
    g.load_ptr_disp32(R::V0, R::V0, 0);
}

pub(crate) fn detach_request_full_move(g: &mut crate::NativeCodeGenerator) {
    // Same actual public capacity request as the moving actor regression suite.
    store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
    let start = g.gc_gen.as_ref().unwrap().labels.moving_start_actor;
    super::call_owner(g, start);
}

pub(crate) fn actor_dispose_while_bound(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    select(g, actor);
    actor_detach_dispose(g, actor);
    g.pop_reg(R::OwnerContext);
}
