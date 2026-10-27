//! Real native return / Old mark observations, with no invented worker credit.
use super::*;
pub(crate) fn native_actor(g: &mut crate::NativeCodeGenerator, actor: TestActor, enter: bool) {
    let label = if enter {
        g.gc_gen.as_ref().unwrap().labels.native_enter_actor
    } else {
        g.gc_gen.as_ref().unwrap().labels.native_exit_actor
    };
    select(g, actor);
    call_owner(g, label);
    restore(g);
}
pub(crate) fn old_mark_shared_metric(g: &mut crate::NativeCodeGenerator, metric: u64) {
    match metric {
        0 => load_rt(g, R::V0, old_footprint::BLACK),
        1 => {
            load_rt(g, R::V0, RT_DM_WORKER);
            g.load_rbp_slot(R::V1, 1528);
            g.sub_reg_reg(R::V0, R::V1);
        }
        2 => load_rt(g, R::V0, allocation_records::REVISION),
        _ => unreachable!(),
    }
}
pub(crate) fn old_epoch_of(g: &mut crate::NativeCodeGenerator) {
    g.load_ptr_disp32(
        R::V0,
        R::V0,
        offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
    );
    g.add_reg_imm32(R::V0, offset_of!(MarkRecord, old_epoch) as i32);
    g.atomic_load_acquire(R::V0, R::V0);
}
pub(crate) fn native_return_fault(g: &mut crate::NativeCodeGenerator, actor: TestActor, mode: u64) {
    select(g, actor);
    match mode {
        0 => {
            load_rt(g, R::V0, allocation_records::REVISION);
            g.add_reg_imm32(R::V0, 2);
            store_rt(g, allocation_records::REVISION, R::V0);
        }
        1 => {
            load_rt(
                g,
                R::V0,
                registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
            );
            store_rt(g, RT_CTX + offset_of!(MutatorContext, adopted_phase), R::V0);
        }
        2 | 3 | 4 | 6 => {
            load_rt(
                g,
                R::V0,
                registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
            );
            match mode {
                2 => {
                    g.mov_imm64(R::V1, 0x400);
                    g.xor_reg_reg(R::V0, R::V1);
                }
                3 => {
                    g.mov_imm64(R::V1, 2);
                    g.or_reg_reg(R::V0, R::V1);
                }
                4 => {
                    g.shl_reg_imm8(R::V0, 48);
                    g.shr_reg_imm8(R::V0, 48);
                }
                6 => {
                    g.mov_imm64(R::V1, 0x100);
                    g.xor_reg_reg(R::V0, R::V1);
                }
                _ => unreachable!(),
            }
            store_rt(
                g,
                registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
                R::V0,
            );
        }
        7 => store_rt_imm(g, RT_MARK_ACTIVE, 0),
        5 => store_rt_imm(g, RT_CTX + offset_of!(MutatorContext, private_producers), 1),
        _ => unreachable!(),
    }
    let exit = g.gc_gen.as_ref().unwrap().labels.native_exit_actor;
    call_owner(g, exit);
    restore(g);
}

/// Test-only metadata observation: this case never changes the record bank.
/// This is not a managed pointer/root and supplies no reachability to marking.
pub(crate) fn remember_mark_record(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let slot = g.asm.data_label_with_i64s(&[0]);
    g.load_ptr_disp32(
        R::V0,
        R::V0,
        offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
    );
    g.mov_data_addr(R::V1, slot);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    slot
}
pub(crate) fn remembered_old_epoch(g: &mut crate::NativeCodeGenerator, slot: crate::DataLabel) {
    g.mov_data_addr(R::V1, slot);
    g.load_ptr_disp32(R::V0, R::V1, 0);
    g.add_reg_imm32(R::V0, offset_of!(MarkRecord, old_epoch) as i32);
    g.atomic_load_acquire(R::V0, R::V0);
}
pub(crate) fn wait_old_worker(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().unwrap();
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.mov_imm64(R::V0, 100000);
    g.store_rbp_slot(1512, R::V0);
    g.bind_text_label(again);
    worker::lock(g, &runtime.labels);
    load_rt(g, R::V0, RT_MARK_DONE);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    worker::request_work(g);
    worker::unlock(g, &runtime.labels);
    worker::wake_if_pending(g, &runtime.labels);
    g.call_label(runtime.labels.worker.as_ref().unwrap().hooks.relax);
    g.load_rbp_slot(R::V0, 1512);
    g.sub_reg_imm8(R::V0, 1);
    g.store_rbp_slot(1512, R::V0);
    g.jcc_label(C::Equal, runtime.labels.fail);
    g.jmp_label(again);
    g.bind_text_label(done);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

pub(crate) fn foreign_native_journal_fault(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    mode: u64,
) {
    native_actor(g, actor, true);
    if mode == 0 {
        native_actor(g, actor, false);
    }
    select(g, actor);
    match mode {
        0 => {} // A truly Running owner, with an already empty journal.
        1 => {
            load_rt(g, R::V1, RT_JOURNAL);
            g.load_ptr_disp32(
                R::V0,
                R::V1,
                offset_of!(crate::gc_abi::BufferCursor, consumed) as i32,
            );
            g.sub_reg_imm8(R::V0, 1);
            g.store_ptr_disp32(
                R::V1,
                offset_of!(crate::gc_abi::BufferCursor, consumed) as i32,
                R::V0,
            );
        }
        2 => store_rt_imm(
            g,
            registered_roots::registration(g) + offset_of!(RootRegistration, journal_serial),
            1,
        ),
        3 => store_rt_imm(g, RT_CTX + offset_of!(MutatorContext, private_producers), 1),
        4 => store_rt_imm(g, RT_CTX + offset_of!(MutatorContext, tlab_top), 1),
        _ => unreachable!(),
    }
    restore(g);
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    g.call_label(runtime.labels.root_journal_seal);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
