//! M-G startup: map the runtime block and heap, prepare pages, descriptors
//! and root storage.
use super::*;

pub(super) fn emit_startup<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
    a.bind_text_label(l.startup);
    a.push_reg(R::V3);
    // Metadata mapping; the runtime block sits at its start.
    a.mov_imm64(R::V7, L.total);
    a.call_label(l.map);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.oom);
    a.mov_reg_reg(R::V3, R::V0);
    a.mov_data_addr(R::V1, runtime);
    a.store_ptr_disp32(R::V1, 0, R::V3);
    // Heap mapping: offset 0 is never a page, so page i starts at (i+1) pages.
    a.mov_imm64(R::V7, HEAP_BYTES);
    a.call_label(l.map);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.oom);
    a.store_ptr_disp32(R::V3, d(RT_HEAP_BASE), R::V0);
    // The single mutator: ABI version, Running (0), thread 1, initial phase.
    store_rt_imm(a, RT_CTX, u64::from(ABI_VERSION));
    store_rt_imm(a, RT_CTX + offset_of!(MutatorContext, thread_id), 1);
    // Page table: every page READY, owning its start bitmap and forward table.
    let page = |field: usize| d(field);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    lea_rt(a, R::V8, L.pages as usize);
    lea_rt(a, R::V10, L.starts as usize);
    lea_rt(a, R::V11, L.tables as usize);
    lea_rt(a, R::V2, L.cells as usize);
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
    a.cmp_reg_imm32(R::V9, imm(PAGES));
    a.jcc_label(C::Below, fill);
    store_rt_addr(a, RT_PAGES, L.pages as usize);
    // Mark-record arena covering every possible object.
    store_rt_addr(
        a,
        RT_MARKS + offset_of!(MarkArena, records),
        L.marks as usize,
    );
    store_rt_imm(
        a,
        RT_MARKS + offset_of!(MarkArena, capacity),
        HALF * CELLS_PER_PAGE,
    );
    store_rt_imm(a, RT_SPACE_END, HALF);
    store_rt_imm(a, RT_OLD_TOP, YOUNG_END);
    store_rt_imm(a, RT_OLD_BASE, YOUNG_END);
    // Builtin descriptors; raw bytes (id 1) is the only one M-G1 allocates.
    lea_rt(a, R::V7, RT_BUILTINS);
    a.call_label(l.builtin_prepare);
    // Root storage: one unregistered segment owned by thread 1, scope 1.
    store_rt_imm(a, RT_SPEC + offset_of!(RootStorageSpec, owner), 1);
    store_rt_imm(a, RT_SPEC + offset_of!(RootStorageSpec, cells), ROOT_CELLS);
    store_rt_imm(
        a,
        RT_SPEC + offset_of!(RootStorageSpec, entries),
        JOURNAL_ENTRIES,
    );
    store_rt_imm(
        a,
        RT_SPEC + offset_of!(RootStorageSpec, bytes),
        L.roots_bytes,
    );
    lea_rt(a, R::V7, L.roots as usize);
    lea_rt(a, R::V6, RT_SPEC);
    a.call_label(l.root_storage_prepare);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    lea_rt(
        a,
        R::V1,
        L.roots as usize + offset_of!(RootStorage, segment),
    );
    a.store_ptr_disp32(R::V3, d(RT_SEGMENT), R::V1);
    a.mov_imm64(R::V0, SEGMENT_SCOPE);
    a.store_ptr_disp32(R::V1, d(offset_of!(RootSegment, scope)), R::V0);
    a.mov_imm64(R::V0, ROOT_SEGMENT_ACTIVE);
    a.store_ptr_disp32(R::V1, d(offset_of!(RootSegment, state)), R::V0);
    a.store_ptr_disp32(
        R::V3,
        d(RT_ALLOC_ROOT + offset_of!(RootRequest, segment)),
        R::V1,
    );
    a.store_ptr_disp32(
        R::V3,
        d(RT_FRAME_ROOT + offset_of!(RootRequest, segment)),
        R::V1,
    );
    store_rt_addr(
        a,
        RT_JOURNAL,
        L.roots as usize + offset_of!(RootStorage, journal),
    );
    // Allocation requests: raw-bytes descriptor, result, mark arena.
    store_rt_addr(
        a,
        RT_REQUEST + offset_of!(AllocationRequest, descriptor),
        RT_BUILTINS + 8,
    );
    store_rt_addr(
        a,
        RT_REQUEST + offset_of!(AllocationRequest, output),
        RT_RESULT,
    );
    store_rt_addr(
        a,
        RT_REQUEST + offset_of!(AllocationRequest, marks),
        RT_MARKS,
    );
    store_rt_addr(
        a,
        RT_INTO + offset_of!(AllocationInto, allocation),
        RT_REQUEST,
    );
    store_rt_addr(a, RT_INTO + offset_of!(AllocationInto, root), RT_ALLOC_ROOT);
    // Cell 0 is the allocation scratch root; frame roots use cells 1...
    root_step(a, l, RT_ALLOC_ROOT, ROOT_RESERVE);
    a.pop_reg(R::V3);
    a.ret();
}
