//! M-G startup: map the runtime block and heap, prepare pages, descriptors
//! and root storage.
use super::*;
use crate::gc_codegen::production::{lea_rt, load_rt, owner_state, store_rt};

pub(super) fn emit_startup<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
    a.bind_text_label(l.startup);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    // Metadata mapping; the runtime block sits at its start.
    if super::metadata::initial_bytes(a) > super::metadata::limit(a) {
        a.jmp_label(l.metadata_budget_oom);
    }
    a.mov_imm64(R::V7, super::metadata::initial_bytes(a));
    a.call_label(l.map);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.native_oom);
    a.mov_reg_reg(R::V3, R::V0);
    a.mov_reg_reg(R::OwnerContext, R::V3);
    a.add_reg_imm32(R::OwnerContext, imm(super::layout(a).owner));
    store_rt(a, offset_of!(Runtime, primary_owner), R::OwnerContext);
    store_rt(
        a,
        owner_state::offset(offset_of!(super::OwnerState, shared_runtime)),
        R::V3,
    );
    a.mov_data_addr(R::V1, runtime);
    a.store_ptr_disp32(R::V1, 0, R::V3);
    super::metadata::startup(a);
    super::trace::startup(a);
    super::large_pages::startup(a);
    // Heap mapping: offset 0 is never a page, so page i starts at (i+1) pages.
    a.mov_imm64(R::V7, super::geometry(a).heap_bytes);
    a.call_label(l.memory.reserve);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.native_oom);
    store_rt(a, RT_HEAP_BASE, R::V0);
    super::large_pages::reservation(a);
    store_rt_imm(a, RT_HEAP_CAPACITY_BYTES, a.gc_heap_capacity());
    store_rt_imm(
        a,
        offset_of!(Runtime, heap_soft_maximum),
        a.gc_heap_soft_maximum(),
    );
    store_rt_imm(a, RT_OLD_LIMIT_BYTES, super::geometry(a).old_bytes);
    store_rt_imm(a, RT_YOUNG_LIMIT_PAGES, super::geometry(a).young_pages);

    commit::startup(a, l);

    // A 256-byte immutable color table avoids recomputing the four one-hot
    // fields at every root/heap store. The snapshot still comes from one load.
    for epochs in 0..32 {
        let phase = crate::gc_abi::PhaseSnapshot::new(
            crate::gc_abi::Phase::Idle,
            crate::gc_abi::Phase::Idle,
            epochs,
            0,
        )
        .expect("five valid epoch bits");
        store_rt_imm(
            a,
            super::layout(a).colours as usize + usize::from(epochs) * 8,
            u64::from(phase.color()),
        );
    }
    // Fresh unpublished Running context; registration assigns its identity.
    store_rt_imm(a, RT_CTX, u64::from(ABI_VERSION));
    participants::startup(a, l);
    store_rt_imm(
        a,
        capacity::REQUESTED,
        super::geometry::policy_geometry(a).young_pages,
    );
    store_rt_imm(
        a,
        capacity::ACTIVE,
        super::geometry::policy_geometry(a).young_pages,
    );
    store_rt_imm(
        a,
        start_policy::TARGET,
        (super::geometry::policy_geometry(a).young_pages / 4)
            .max(start_policy::MIN)
            .min((super::geometry::policy_geometry(a).young_pages * 3 / 4).max(1)),
    );
    store_rt_imm(a, tenuring::THRESHOLD, tenuring::MIN);
    store_rt_imm(a, tenuring::FROZEN, tenuring::MIN);
    store_rt_imm(
        a,
        old_policy::TARGET,
        super::geometry::policy_geometry(a).old_min,
    );
    store_rt_imm(
        a,
        old_policy::BUDGET,
        super::geometry::policy_geometry(a).young_bytes,
    );
    // Page table: every page READY, owning its start bitmap and forward table.
    let page = |field: usize| d(field);
    load_rt(a, R::V1, RT_HEAP_BASE);
    lea_rt(a, R::V8, super::layout(a).pages as usize);
    lea_rt(a, R::V10, super::layout(a).starts as usize);
    lea_rt(a, R::V11, super::layout(a).tables as usize);
    lea_rt(a, R::V2, super::layout(a).cells as usize);
    a.mov_imm64(R::V6, HEAP_PAGE_BYTES);
    a.mov_imm64(R::V7, 1);
    a.mov_imm64(R::V9, 0);
    let fill = a.create_text_label();
    a.bind_text_label(fill);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, heap_base)), R::V1);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, start)), R::V6);
    a.store_ptr_disp32(
        R::V8,
        page(offset_of!(AllocationPage, allocated_top)),
        R::V6,
    );
    a.mov_reg_reg(R::V0, R::V6);
    a.add_reg_imm32(R::V0, imm(HEAP_PAGE_BYTES));
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, limit)), R::V0);
    a.mov_reg_reg(R::V0, R::V9);
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, incarnation)), R::V0);
    a.store_ptr_disp32(
        R::V8,
        page(offset_of!(AllocationPage, identity_base)),
        R::V7,
    );
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, run_head)), R::V9);
    a.mov_imm64(R::V0, PAGE_READY);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, state)), R::V0);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, starts)), R::V10);
    a.mov_imm64(R::V0, STARTS_WORDS);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, starts_words)), R::V0);
    a.store_ptr_disp32(
        R::V8,
        page(offset_of!(AllocationPage, forward_table)),
        R::V11,
    );
    a.store_ptr_disp32(R::V11, d(offset_of!(ForwardTable, cells)), R::V2);
    a.mov_imm64(R::V0, CELLS_PER_PAGE);
    a.store_ptr_disp32(R::V11, d(offset_of!(ForwardTable, capacity)), R::V0);
    a.add_reg_imm32(R::V8, d(size_of::<AllocationPage>()));
    a.add_reg_imm32(R::V10, imm(STARTS_WORDS * 8));
    a.add_reg_imm32(R::V11, d(size_of::<ForwardTable>()));
    a.add_reg_imm32(R::V2, imm(CELLS_PER_PAGE * size_of::<ForwardCell>() as u64));
    a.add_reg_imm32(R::V6, imm(HEAP_PAGE_BYTES));
    a.add_reg_imm32(R::V7, imm(IDENTITY_STRIDE));
    a.add_reg_imm32(R::V9, 1);
    a.cmp_reg_imm32(R::V9, imm(super::geometry(a).pages));
    a.jcc_label(C::Below, fill);
    store_rt_addr(a, RT_PAGES, super::layout(a).pages as usize);
    super::metadata_leases::startup(a);
    super::weak::startup(a);
    super::mark_queue::startup(a);
    super::pin_abi::startup(a, l);
    super::thread_api::startup(a, l);
    // Mark-record arena covering every possible object.
    store_rt_addr(
        a,
        RT_MARKS + offset_of!(MarkArena, records),
        super::layout(a).marks as usize,
    );
    store_rt_imm(
        a,
        RT_MARKS + offset_of!(MarkArena, capacity),
        super::geometry::mark_records(a.gc_heap_capacity()),
    );
    store_rt_imm(a, allocation_records::REVISION, 1);
    a.mov_imm64(R::V7, super::geometry::policy_geometry(a).young_pages);
    a.mov_imm64(R::V6, generation::YOUNG);
    pool::claim(a, l);
    store_rt(a, RT_SPACE_BASE, R::V0);
    store_rt(a, RT_NEXT_PAGE, R::V0);
    store_rt_imm(
        a,
        RT_SPACE_END,
        super::geometry::policy_geometry(a).young_pages,
    );
    store_rt_imm(
        a,
        RT_COPY_LOW,
        super::geometry::policy_geometry(a).young_pages,
    );
    store_rt_imm(
        a,
        RT_BOUNDS,
        super::geometry::policy_geometry(a).young_pages << 32,
    );
    store_rt_imm(
        a,
        pool::OLD_PAGES,
        super::geometry(a).old_bytes / HEAP_PAGE_BYTES,
    );
    a.mov_imm64(R::V7, super::geometry(a).old_bytes / HEAP_PAGE_BYTES);
    a.mov_imm64(R::V6, generation::OLD);
    pool::claim(a, l);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    store_rt(a, RT_OLD_TOP, R::V0);
    store_rt(a, RT_OLD_BASE, R::V0);
    old_runs::startup(a);
    store_rt_imm(a, RT_REMSET_BASE, super::layout(a).remset);
    // Builtin descriptors; raw bytes (id 1) is the only one M-G1 allocates.
    lea_rt(a, R::V7, RT_BUILTINS);
    a.call_label(l.builtin_prepare);
    store_rt_imm(a, RT_SPEC + offset_of!(RootStorageSpec, cells), ROOT_CELLS);
    store_rt_imm(
        a,
        RT_SPEC + offset_of!(RootStorageSpec, entries),
        JOURNAL_ENTRIES,
    );
    store_rt_imm(
        a,
        RT_SPEC + offset_of!(RootStorageSpec, bytes),
        super::layout(a).roots_bytes,
    );
    owner_state::initialize(a, l);
    super::worker::emit_spawn_call(a, l);
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();
}
