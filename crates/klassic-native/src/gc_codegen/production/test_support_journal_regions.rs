//! Distinct actual prepared storages and real producer retries. No synthetic
//! journal entries or mark queue certificates are used by these fixtures.
use super::super::*;
use crate::gc_abi::{BufferCursor, RootLifeRequest, RootRegistration, RootRegistry};
use crate::gc_codegen::production::{OwnerState, lea_rt, load_rt, owner_state, store_rt};

#[derive(Clone, Copy)]
pub(crate) struct JournalRegion {
    pub storage: crate::DataLabel,
    request: crate::DataLabel,
}
fn words(g: &mut crate::NativeCodeGenerator, bytes: usize) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    g.asm.data_label_with_i64s(&vec![0; bytes / 8])
}
pub(crate) fn journal_region_new(
    g: &mut crate::NativeCodeGenerator,
    capacity: u64,
) -> JournalRegion {
    journal_region_sized(g, 1, capacity)
}
pub(crate) fn journal_region_sized(
    g: &mut crate::NativeCodeGenerator,
    cells: u64,
    capacity: u64,
) -> JournalRegion {
    let runtime = g.gc_gen.take().unwrap();
    let bytes = size_of::<RootStorage>() + cells as usize * 32 + capacity as usize * 32;
    let storage = words(g, bytes);
    let spec = words(g, size_of::<RootStorageSpec>());
    let life = words(g, size_of::<RootLifeRequest>());
    let request = words(g, size_of::<RootRequest>());
    g.mov_data_addr(R::V6, spec);
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.store_ptr_disp32(R::V6, d(offset_of!(RootStorageSpec, owner)), R::V0);
    for (field, value) in [
        (offset_of!(RootStorageSpec, cells), cells),
        (offset_of!(RootStorageSpec, entries), capacity),
        (offset_of!(RootStorageSpec, bytes), bytes as u64),
    ] {
        g.mov_imm64(R::V0, value);
        g.store_ptr_disp32(R::V6, d(field), R::V0);
    }
    g.mov_data_addr(R::V7, storage);
    g.call_label(runtime.labels.root_storage_prepare);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, runtime.labels.fail);
    worker::lock(g, &runtime.labels);
    g.mov_data_addr(R::V6, life);
    g.mov_data_addr(R::V0, storage);
    g.store_ptr_disp32(R::V6, d(offset_of!(RootLifeRequest, registration)), R::V0);
    lea_rt(g, R::V0, RT_CTX);
    g.store_ptr_disp32(R::V6, d(offset_of!(RootLifeRequest, context)), R::V0);
    lea_rt(g, R::V7, registered_roots::REGISTRY);
    g.mov_imm64(R::V2, 0);
    g.call_label(runtime.labels.root_life_step);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, runtime.labels.fail);
    worker::unlock(g, &runtime.labels);
    g.mov_data_addr(R::V6, request);
    g.mov_data_addr(R::V0, storage);
    g.add_reg_imm32(R::V0, d(offset_of!(RootStorage, segment)));
    g.store_ptr_disp32(R::V6, d(offset_of!(RootRequest, segment)), R::V0);
    lea_rt(g, R::V7, RT_CTX);
    g.mov_imm64(R::V2, ROOT_RESERVE);
    g.call_label(runtime.labels.step);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, runtime.labels.fail);
    g.gc_gen = Some(runtime);
    JournalRegion { storage, request }
}

pub(crate) fn journal_region_reserve(g: &mut crate::NativeCodeGenerator, region: JournalRegion) {
    let step = g.gc_gen.as_ref().unwrap().labels.step;
    lea_rt(g, R::V7, RT_CTX);
    g.mov_data_addr(R::V6, region.request);
    g.mov_imm64(R::V2, ROOT_RESERVE);
    g.call_label(step);
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
}
/// V0 is a raw payload address, encoded here. Route the actual request through production
/// root_step's scratch-store path, including its real FLUSH target selection.
/// Restore the ordinary scratch request afterward; no managed allocation/poll.
pub(crate) fn journal_region_store(
    g: &mut crate::NativeCodeGenerator,
    region: JournalRegion,
    locked: bool,
) {
    let runtime = g.gc_gen.take().unwrap();
    encode(g, R::V0, R::V2);
    g.mov_data_addr(R::V1, region.request);
    g.store_ptr_disp32(R::V1, d(offset_of!(RootRequest, value)), R::V0);
    g.push_reg(R::V0); // keep call-stack alignment with five saved words
    for field in (0..size_of::<RootRequest>()).step_by(8) {
        load_rt(g, R::V0, RT_ALLOC_ROOT + field);
        g.push_reg(R::V0);
        g.load_ptr_disp32(R::V0, R::V1, d(field));
        store_rt(g, RT_ALLOC_ROOT + field, R::V0);
    }
    if locked {
        worker::lock(g, &runtime.labels);
    }
    root_step(g, &runtime.labels, RT_ALLOC_ROOT, ROOT_STORE);
    if locked {
        worker::unlock(g, &runtime.labels);
    }
    g.mov_data_addr(R::V1, region.request);
    for field in (0..size_of::<RootRequest>()).step_by(8).rev() {
        load_rt(g, R::V0, RT_ALLOC_ROOT + field);
        g.store_ptr_disp32(R::V1, d(field), R::V0);
        g.pop_reg(R::V0);
        store_rt(g, RT_ALLOC_ROOT + field, R::V0);
    }
    g.pop_reg(R::V0);
    g.gc_gen = Some(runtime);
}
pub(crate) fn journal_region_cursor(
    g: &mut crate::NativeCodeGenerator,
    region: JournalRegion,
    consumed: bool,
) {
    g.mov_data_addr(R::V1, region.storage);
    g.load_ptr_disp32(
        R::V0,
        R::V1,
        d(offset_of!(RootStorage, journal)
            + if consumed {
                offset_of!(BufferCursor, consumed)
            } else {
                offset_of!(BufferCursor, published)
            }),
    );
}
pub(crate) fn journal_region_load(g: &mut crate::NativeCodeGenerator, region: JournalRegion) {
    let l = g.gc_gen.as_ref().unwrap().labels.field_load;
    g.mov_data_addr(R::V10, region.storage);
    g.add_reg_imm32(R::V10, size_of::<RootStorage>() as i32);
    g.load_ptr_disp32(R::V0, R::V10, 0);
    g.call_label(l);
    g.load_ptr_disp32(R::V0, R::V0, 0);
}
pub(crate) fn journal_regions_visited(g: &mut crate::NativeCodeGenerator) {
    load_rt(
        g,
        R::V0,
        owner_state::offset(offset_of!(OwnerState, root_journal))
            + offset_of!(root_journal::State, regions),
    );
}
pub(crate) fn journal_region_corrupt(
    g: &mut crate::NativeCodeGenerator,
    region: JournalRegion,
    mode: u64,
) {
    g.mov_data_addr(R::V1, region.storage);
    if mode == 2 {
        g.mov_imm64(R::V0, 0);
        g.store_ptr_disp32(R::V1, d(offset_of!(RootRegistration, context)), R::V0);
    } else {
        g.load_ptr_disp32(
            R::V1,
            R::V1,
            d(offset_of!(RootStorage, journal) + offset_of!(BufferCursor, entries)),
        );
        g.mov_imm64(R::V0, u64::MAX);
        g.store_ptr_disp32(
            R::V1,
            d(if mode == 0 {
                offset_of!(RootChange, phase)
            } else {
                offset_of!(RootChange, scope)
            }),
            R::V0,
        );
    }
}
pub(crate) fn journal_registry_walkers(g: &mut crate::NativeCodeGenerator) {
    load_rt(
        g,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, walkers),
    );
}
