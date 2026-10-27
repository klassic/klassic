//! Fresh real Linux threads enter the production attach path while another
//! owner drives a real rendezvous/relocation. No registry or phase is forged.
use super::super::{bind_primary_owner, emit_unbounded_budget, request_full_collection};
use super::*;
use crate::gc_codegen::threads;
use crate::{NativeCodeGenerator, TextLabel};
const STACK_BYTES: u64 = 65536;
const TID: i32 = 24;
const STACK: i32 = 32;
const HEAP: i32 = 40;
const DONE: i32 = 48;
const ROOT: i32 = 64;
const OS_TID: i32 = 72;

pub(crate) struct AttachThread {
    pub(crate) actor: TestActor,
    pub(crate) root: crate::DataLabel,
    pub(super) control: crate::DataLabel,
    join: TextLabel,
    unmap: TextLabel,
}

pub(crate) fn start_attach_thread(g: &mut NativeCodeGenerator) -> AttachThread {
    start_thread(g, false, false, None)
}
pub(crate) fn start_detach_thread(g: &mut NativeCodeGenerator) -> AttachThread {
    start_thread(g, true, false, None)
}
pub(crate) fn start_tls_detach_thread(g: &mut NativeCodeGenerator) -> AttachThread {
    start_thread(g, true, true, None)
}
pub(crate) fn start_inherited_tls_clear_thread(
    g: &mut NativeCodeGenerator,
    target: TestActor,
) -> AttachThread {
    start_thread(g, false, false, Some(target))
}
fn start_thread(
    g: &mut NativeCodeGenerator,
    detaching: bool,
    tls: bool,
    inherited: Option<TestActor>,
) -> AttachThread {
    let actor = prepare_actor(g, 16, 8);
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let root = g.asm.data_label_with_i64s(&[0]);
    let control = g.asm.data_label_with_i64s(&[0; 11]);
    let (attach, alloc, adopt, publish, enter, fail) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (
            l.attach_actor,
            l.alloc_actor,
            l.root_adopt_actor,
            l.publish_pending,
            l.native_enter_actor,
            l.fail,
        )
    };
    let start = g.create_text_label();
    let join = g.create_text_label();
    let map = g.create_text_label();
    let unmap = g.create_text_label();
    let entry = g.create_text_label();
    let body = g.create_text_label();
    g.jmp_label(body);
    g.bind_text_label(start);
    threads::emit_linux_worker_start(g);
    g.bind_text_label(join);
    threads::emit_linux_worker_join(g);
    g.bind_text_label(map);
    threads::emit_linux_stack_alloc(g);
    g.bind_text_label(unmap);
    threads::emit_linux_stack_free(g);
    g.bind_text_label(entry);
    g.push_reg(R::V5);
    g.mov_reg_reg(R::V5, R::V4);
    g.sub_reg_imm32(R::V4, if detaching { 2048 } else { 128 });
    g.store_rbp_slot(8, R::V7);
    threads::emit_linux_thread_context(g, crate::Reg::R12);
    g.load_ptr_disp32(R::V3, R::V7, HEAP);
    g.asm.mov_imm64(crate::Reg::Rax, 186); // actual Linux gettid
    g.asm.syscall();
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, OS_TID, R::V0);
    let child_done = g.create_text_label();
    if let Some(target) = inherited {
        let (query, clear, set) = {
            let l = &g.gc_gen.as_ref().unwrap().labels;
            (l.tls_query, l.tls_clear_actor, l.tls_os.set)
        };
        // clone inherits GS even though CLONE_SETTLS installs a fresh FS.
        // Confirm that the victim pointer really is present on this new thread.
        g.mov_reg_reg(R::V7, R::V5);
        g.sub_reg_imm32(R::V7, 16);
        g.call_label(query);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
        g.load_rbp_slot(R::V0, 16);
        g.mov_data_addr(R::V1, target.pointer);
        g.load_ptr_disp32(R::V1, R::V1, 0);
        g.cmp_reg_reg(R::V0, R::V1);
        g.jcc_label(C::NotEqual, fail);
        let current = g.gc_gen.as_ref().unwrap().labels.tls_current;
        g.mov_reg_reg(R::V7, R::V5);
        g.sub_reg_imm32(R::V7, 16);
        g.call_label(current);
        g.load_rbp_slot(R::V1, 8);
        g.store_ptr_disp32(R::V1, 80, R::V0);
        g.cmp_reg_imm32(R::V0, 3);
        g.jcc_label(C::NotEqual, fail);
        g.load_rbp_slot(R::V0, 16);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
        g.mov_data_addr(R::V1, target.pointer);
        g.load_ptr_disp32(R::V1, R::V1, 0);
        g.mov_reg_reg(R::V7, R::V1);
        g.call_label(clear);
        g.load_rbp_slot(R::V1, 8);
        g.store_ptr_disp32(R::V1, 56, R::V0);
        // This child never owned an association. Drop only its inherited GS;
        // no collector user/owner state belongs to this native probe thread.
        g.mov_imm64(R::V0, 0);
        for slot in [48, 40, 32] {
            g.store_rbp_slot(slot, R::V0);
        }
        g.mov_reg_reg(R::V7, R::V5);
        g.sub_reg_imm32(R::V7, 48);
        g.mov_text_addr(R::V11, set);
        g.call_native_unary(R::V11);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
        threads::emit_linux_thread_unbind(g);
        g.jmp_label(child_done);
    }
    g.mov_reg_reg(R::V7, R::OwnerContext);
    g.call_label(attach);
    if tls {
        let install = g.gc_gen.as_ref().unwrap().labels.tls_install_actor;
        g.call_label(install);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
        let rt = g.gc_gen.take().unwrap();
        assert_tls_current(g, &rt.labels);
        g.gc_gen = Some(rt);
    }
    // Prove that the actual admitted owner can allocate, root and publish.
    g.mov_imm64(R::V7, 24);
    g.mov_imm64(R::V6, 1);
    g.call_label(alloc);
    g.mov_imm64(R::V1, 84);
    g.store_ptr_disp32(R::V0, 0, R::V1);
    g.load_rbp_slot(R::V1, 8);
    g.load_ptr_disp32(R::V1, R::V1, ROOT);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.mov_reg_reg(R::V0, R::V1);
    g.call_label(adopt);
    g.call_label(publish);
    if detaching {
        let (begin, step, poll, relax) = {
            let l = &g.gc_gen.as_ref().unwrap().labels;
            (
                l.detach_begin_actor,
                l.detach_step_actor,
                l.owner_checkpoint_poll,
                l.worker.as_ref().unwrap().hooks.relax,
            )
        };
        g.call_label(begin);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
        g.load_rbp_slot(R::V1, 8);
        g.add_reg_imm32(R::V1, 56); // actual Detaching publication receipt
        g.mov_imm64(R::V0, 1);
        g.atomic_store_release(R::V1, R::V0);
        let wait = g.create_text_label();
        let retire = g.create_text_label();
        let done = g.create_text_label();
        g.bind_text_label(wait);
        g.call_label(poll); // real ACK/resume while retaining actual owner roots
        g.load_rbp_slot(R::V1, 8);
        g.atomic_load_acquire(R::V0, R::V1); // GO at control word zero
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, retire);
        g.call_label(relax);
        g.jmp_label(wait);
        g.bind_text_label(retire);
        g.call_label(step);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::Equal, done);
        g.cmp_reg_imm32(R::V0, 2);
        g.jcc_label(C::NotEqual, fail);
        g.call_label(relax);
        g.jmp_label(retire);
        g.bind_text_label(done);
        if tls {
            // Keep the actual Detached context/TLS association until the parent
            // proves that a different OS thread cannot clear it or delete key.
            let allow = g.create_text_label();
            let held = g.create_text_label();
            g.bind_text_label(held);
            g.load_rbp_slot(R::V1, 8);
            g.atomic_load_acquire(R::V0, R::V1);
            g.cmp_reg_imm32(R::V0, 2);
            g.jcc_label(C::Equal, allow);
            g.call_label(relax);
            g.jmp_label(held);
            g.bind_text_label(allow);
            let rt = g.gc_gen.take().unwrap();
            assert_tls_current(g, &rt.labels);
            g.gc_gen = Some(rt);
            let (clear, query) = {
                let l = &g.gc_gen.as_ref().unwrap().labels;
                (l.tls_clear_actor, l.tls_query)
            };
            g.mov_reg_reg(R::V7, R::OwnerContext);
            g.call_label(clear);
            g.test_reg_reg(R::V0, R::V0);
            g.jcc_label(C::NotEqual, fail);
            g.mov_reg_reg(R::V7, R::V5);
            g.sub_reg_imm32(R::V7, 16);
            g.call_label(query);
            g.test_reg_reg(R::V0, R::V0);
            g.jcc_label(C::NotEqual, fail);
            g.load_rbp_slot(R::V0, 16);
            g.test_reg_reg(R::V0, R::V0);
            g.jcc_label(C::NotEqual, fail);
        }
        threads::emit_linux_thread_unbind(g);
    } else {
        g.call_label(enter); // root/owner retained; the primary can collect again
    }
    g.bind_text_label(child_done);
    g.load_rbp_slot(R::V1, 8);
    g.add_reg_imm32(R::V1, DONE);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
    g.leave();
    g.ret();
    g.bind_text_label(body);
    g.mov_imm64(R::V7, STACK_BYTES);
    g.call_label(map);
    g.cmp_reg_imm32(R::V0, -4095);
    g.jcc_label(C::AboveOrEqual, fail);
    g.mov_data_addr(R::V9, control);
    g.store_ptr_disp32(R::V9, STACK, R::V0);
    g.store_ptr_disp32(R::V9, HEAP, R::V3);
    g.mov_data_addr(R::V1, actor.pointer);
    g.load_ptr_disp32(R::V1, R::V1, 0);
    g.store_ptr_disp32(R::V9, 16, R::V1);
    g.mov_reg_reg(R::V1, R::V9);
    g.add_reg_imm32(R::V1, 8);
    g.store_ptr_disp32(R::V9, 8, R::V1);
    g.mov_data_addr(R::V1, root);
    g.store_ptr_disp32(R::V9, ROOT, R::V1);
    g.asm.mov_reg_reg(crate::Reg::Rdi, crate::Reg::Rax);
    g.asm
        .add_reg_imm32(crate::Reg::Rdi, STACK_BYTES as i32 + 4096);
    g.asm.mov_reg_reg(crate::Reg::Rsi, crate::Reg::R9);
    g.asm.add_reg_imm32(crate::Reg::Rsi, 8);
    g.asm.lea_text_addr(crate::Reg::Rdx, entry);
    g.asm.mov_reg_reg(crate::Reg::Rcx, crate::Reg::R9);
    g.asm.mov_reg_reg(crate::Reg::R8, crate::Reg::R9);
    g.asm.add_reg_imm32(crate::Reg::R8, TID);
    g.call_label(start);
    g.cmp_reg_imm32(R::V0, 0);
    g.jcc_label(C::LessEqual, fail);
    AttachThread {
        actor,
        root,
        control,
        join,
        unmap,
    }
}

/// Observe only. The live worker/coordinator must make the progress itself.
pub(crate) fn wait_attach_thread(g: &mut NativeCodeGenerator, t: &AttachThread, done: bool) {
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
    g.store_rbp_slot(1984, R::V0);
    let again = g.create_text_label();
    let ready = g.create_text_label();
    g.bind_text_label(again);
    if done {
        g.mov_data_addr(R::V1, t.control);
        g.add_reg_imm32(R::V1, DONE);
        g.atomic_load_acquire(R::V0, R::V1);
    } else {
        actor_metric(g, t.actor, ActorMetric::AttachWaits);
    }
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, ready);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1984);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(ready);
}

pub(crate) fn join_attach_thread(g: &mut NativeCodeGenerator, t: &AttachThread) {
    wait_attach_thread(g, t, true);
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.mov_data_addr(R::V7, t.control);
    g.add_reg_imm32(R::V7, TID);
    g.call_label(t.join);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.mov_data_addr(R::V7, t.control);
    g.load_ptr_disp32(R::V7, R::V7, STACK);
    g.mov_imm64(R::V6, STACK_BYTES + 8192);
    g.call_label(t.unmap);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
}

pub(crate) fn attach_thread_tid(g: &mut NativeCodeGenerator, t: &AttachThread) {
    g.mov_data_addr(R::V1, t.control);
    g.load_ptr_disp32(R::V0, R::V1, OS_TID);
}

pub(crate) fn detach_thread_flag(g: &mut NativeCodeGenerator, t: &AttachThread, field: usize) {
    g.mov_data_addr(R::V1, t.control);
    g.add_reg_imm32(R::V1, if field == 0 { 56 } else { 16 });
    g.atomic_load_acquire(R::V0, R::V1);
}
pub(crate) fn detach_thread_allow_tls_clear(g: &mut NativeCodeGenerator, t: &AttachThread) {
    g.mov_data_addr(R::V1, t.control);
    g.mov_imm64(R::V0, 2);
    g.atomic_store_release(R::V1, R::V0);
}
pub(crate) fn inherited_tls_clear_result(g: &mut NativeCodeGenerator, t: &AttachThread) {
    g.mov_data_addr(R::V1, t.control);
    g.load_ptr_disp32(R::V0, R::V1, 56);
}
pub(crate) fn inherited_tls_current_result(g: &mut NativeCodeGenerator, t: &AttachThread) {
    g.mov_data_addr(R::V1, t.control);
    g.load_ptr_disp32(R::V0, R::V1, 80);
}

fn assert_tls_current<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.mov_reg_reg(R::V7, R::V5);
    a.sub_reg_imm32(R::V7, 16);
    a.call_label(l.tls_current);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_rbp_slot(R::V0, 16);
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, l.fail);
}
pub(crate) fn detach_thread_retire(g: &mut NativeCodeGenerator, t: &AttachThread) {
    g.mov_data_addr(R::V1, t.control);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
}

/// Cross-assembler probe of real mark-time attach and post-Full native return.
/// This uses the production leaves and actual registered cells throughout.
pub(crate) fn emit_mark_attach_probe<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    emit_mark_attach_impl(a, l, false, false);
}
pub(crate) fn emit_mark_attach_detach_probe<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    emit_mark_attach_impl(a, l, true, false);
}
pub(crate) fn emit_mark_attach_tls_probe<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    emit_mark_attach_impl(a, l, true, true);
}
fn emit_mark_attach_impl<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    detaching: bool,
    tls: bool,
) {
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 64);
    a.push_reg(R::OwnerContext);
    if tls {
        assert_keyless_identity(a, l, true);
        a.call_label(l.tls_initialize);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    a.call_label(l.worker.as_ref().unwrap().mark_start);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.push_reg(R::V0);
    let roots_bytes = size_of::<RootStorage>() as u64 + (16 + 8) * 32;
    a.mov_imm64(R::V7, offset_of!(OwnerState, roots) as u64 + roots_bytes);
    a.call_label(l.metadata_map);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.native_oom);
    a.mov_reg_reg(R::V7, R::V0);
    for (field, value) in [
        (offset_of!(RootStorageSpec, cells), 16),
        (offset_of!(RootStorageSpec, entries), 8),
        (offset_of!(RootStorageSpec, bytes), roots_bytes),
    ] {
        a.mov_imm64(R::V0, value);
        a.store_ptr_disp32(R::V7, d(offset_of!(OwnerState, spec) + field), R::V0);
    }
    a.call_label(l.attach_actor);
    a.mov_reg_reg(R::OwnerContext, R::V0);
    if tls {
        a.call_label(l.tls_install_actor);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
        assert_tls_current(a, l);
        a.call_label(l.tls_install_actor); // real same-thread idempotence
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
        a.call_label(l.tls_destroy);
        a.cmp_reg_imm32(R::V0, 2);
        a.jcc_label(C::NotEqual, l.fail);
    }
    a.pop_reg(R::V1);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_imm64(R::V7, 24);
    a.mov_imm64(R::V6, 1);
    a.call_label(l.alloc_actor);
    a.mov_imm64(R::V1, 84);
    a.store_ptr_disp32(R::V0, 0, R::V1);
    a.store_rbp_slot(8, R::V0);
    a.mov_reg_reg(R::V0, R::V5);
    a.sub_reg_imm8(R::V0, 8);
    a.call_label(l.root_adopt_actor);
    a.call_label(l.publish_pending);
    a.call_label(l.native_enter_actor);
    a.load_rbp_slot(R::V6, 8); // actual stable RootCell pointer
    a.push_reg(R::OwnerContext);
    a.push_reg(R::V6);
    bind_primary_owner(a);
    worker::lock(a, l);
    emit_unbounded_budget(a);
    a.call_label(l.worker.as_ref().unwrap().mark_loop);
    a.call_label(l.mark_end);
    worker::unlock(a, l);
    // Request a real Full through its ordinary capacity policy input.
    request_full_collection(a);
    a.call_label(l.collect);
    worker::lock(a, l);
    a.call_label(l.finish);
    worker::unlock(a, l);
    load_rt(a, R::V0, RT_FULL_COLLECTIONS);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.pop_reg(R::V0);
    a.pop_reg(R::OwnerContext);
    a.call_label(l.native_exit_actor);
    a.call_label(l.root_load_actor);
    a.load_ptr_disp32(R::V0, R::V0, 0);
    a.cmp_reg_imm32(R::V0, 84);
    a.jcc_label(C::NotEqual, l.fail);
    if detaching {
        a.call_label(l.detach_begin_actor);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
        let again = a.create_text_label();
        let done = a.create_text_label();
        a.bind_text_label(again);
        a.call_label(l.detach_step_actor);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, done);
        a.cmp_reg_imm32(R::V0, 2);
        a.jcc_label(C::NotEqual, l.fail);
        a.call_label(l.worker.as_ref().unwrap().hooks.relax);
        a.jmp_label(again);
        a.bind_text_label(done);
        if tls {
            assert_tls_current(a, l);
            a.mov_reg_reg(R::V7, R::V5);
            a.sub_reg_imm32(R::V7, 16);
            a.call_label(l.tls_query);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, l.fail);
            a.load_rbp_slot(R::V0, 16);
            a.cmp_reg_reg(R::V0, R::OwnerContext);
            a.jcc_label(C::NotEqual, l.fail);
            a.call_label(l.tls_destroy); // Detached still owns an OS association
            a.cmp_reg_imm32(R::V0, 2);
            a.jcc_label(C::NotEqual, l.fail);
            a.mov_reg_reg(R::V7, R::OwnerContext);
            a.call_label(l.tls_clear_actor);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, l.fail);
            a.mov_reg_reg(R::V7, R::V5);
            a.sub_reg_imm32(R::V7, 16);
            a.call_label(l.tls_query);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, l.fail);
            a.load_rbp_slot(R::V0, 16);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, l.fail);
            a.call_label(l.tls_destroy);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, l.fail);
            assert_keyless_identity(a, l, false);
        }
        a.mov_reg_reg(R::V7, R::OwnerContext);
        a.pop_reg(R::OwnerContext);
        a.call_label(l.detach_dispose_actor);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    } else {
        a.call_label(l.native_enter_actor);
        a.pop_reg(R::OwnerContext);
    }
    a.leave();
}

// The probe owns frame words -16 (output) and -24 (saved physical OS identity).
// Both calls occur while there is no OS TLS key; an owner/context identity is
// insufficient because the actual physical thread persists across attach/GC.
fn assert_keyless_identity<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, save: bool) {
    a.mov_reg_reg(R::V7, R::V5);
    a.sub_reg_imm32(R::V7, 16);
    a.call_label(l.tls_query_thread);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_rbp_slot(R::V0, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    if save {
        a.store_rbp_slot(24, R::V0);
    } else {
        a.load_rbp_slot(R::V1, 24);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, l.fail);
    }
}
