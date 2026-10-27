//! Actual queued retirement and shared lifetime observations.
use super::super::*;
use super::JournalRegion;
use crate::gc_abi::{RootLease, RootLifeRequest, RootRegistration};
use root_retirement::Node;

#[derive(Clone, Copy)]
pub(crate) struct RetiringRegion {
    node: crate::DataLabel,
    region: JournalRegion,
}
pub(crate) fn retire_region(
    g: &mut crate::NativeCodeGenerator,
    region: JournalRegion,
) -> RetiringRegion {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let node = g.asm.data_label_with_i64s(&vec![0; size_of::<Node>() / 8]);
    g.mov_data_addr(R::V7, node);
    g.mov_data_addr(R::V0, region.storage);
    g.store_ptr_disp32(
        R::V7,
        d(offset_of!(Node, life) + offset_of!(RootLifeRequest, registration)),
        R::V0,
    );
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootRegistration, scope)));
    g.store_ptr_disp32(
        R::V7,
        d(offset_of!(Node, life) + offset_of!(RootLifeRequest, scope)),
        R::V0,
    );
    let entry = g.gc_gen.as_ref().unwrap().labels.root_retire_actor;
    g.call_label(entry);
    RetiringRegion { node, region }
}
pub(crate) fn retirement_step(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    g.call_label(rt.labels.root_retirement_step);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}
pub(crate) fn retirement_metric(
    g: &mut crate::NativeCodeGenerator,
    r: RetiringRegion,
    field: usize,
) {
    match field {
        0 => {
            g.mov_data_addr(R::V1, r.node);
            g.add_reg_imm32(R::V1, d(offset_of!(Node, completed)));
            g.atomic_load_acquire(R::V0, R::V1);
        }
        1 => {
            g.mov_data_addr(R::V1, r.region.storage);
            g.load_ptr_disp32(R::V0, R::V1, d(offset_of!(RootRegistration, registered)));
        }
        2 => load_rt(g, R::V0, root_work::PENDING),
        3 => load_rt(
            g,
            R::V0,
            RT_CTX + offset_of!(MutatorContext, retiring_roots),
        ),
        4 => {
            g.mov_data_addr(R::V1, r.region.storage);
            g.load_ptr_disp32(R::V0, R::V1, d(offset_of!(RootRegistration, leases)));
        }
        5 => {
            g.mov_data_addr(R::V1, r.region.storage);
            g.load_ptr_disp32(R::V0, R::V1, d(offset_of!(RootRegistration, cursor)));
        }
        6 => load_rt(
            g,
            R::V0,
            offset_of!(Runtime, root_retirement) + offset_of!(root_retirement::State, debt),
        ),
        7 => load_rt(
            g,
            R::V0,
            offset_of!(Runtime, root_retirement) + offset_of!(root_retirement::State, transport),
        ),
        8 => load_rt(
            g,
            R::V0,
            offset_of!(Runtime, root_retirement) + offset_of!(root_retirement::State, cleared),
        ),
        _ => unreachable!(),
    }
}

/// Hold a real counted reader lease across RETIRE; the consumer may scan but
/// must keep the storage/context until the actual reader releases its lease.
pub(crate) fn retirement_reader(
    g: &mut crate::NativeCodeGenerator,
    region: JournalRegion,
) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let request = g.asm.data_label_with_i64s(&vec![
        0;
        (size_of::<RootLifeRequest>()
            + size_of::<RootLease>())
            / 8
    ]);
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    g.mov_data_addr(R::V6, request);
    g.mov_data_addr(R::V0, region.storage);
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
    request
}
pub(crate) fn retirement_reader_release(
    g: &mut crate::NativeCodeGenerator,
    request: crate::DataLabel,
) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    lea_rt(g, R::V7, registered_roots::REGISTRY);
    g.mov_data_addr(R::V6, request);
    g.mov_imm64(R::V2, 2);
    g.call_label(rt.labels.root_life_step);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, rt.labels.fail);
    worker::request_work(g);
    worker::unlock(g, &rt.labels);
    worker::wake_if_pending(g, &rt.labels);
    g.gc_gen = Some(rt);
}
pub(crate) fn retirement_disposable(g: &mut crate::NativeCodeGenerator, r: RetiringRegion) {
    g.mov_data_addr(R::V7, r.region.storage);
    crate::gc_codegen::root_storage::emit_can_dispose(g);
}
pub(crate) fn retirement_wait(g: &mut crate::NativeCodeGenerator, r: RetiringRegion, assist: bool) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1984, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    retirement_metric(g, r, 0);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    if assist {
        retirement_step(g);
    }
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1984);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
