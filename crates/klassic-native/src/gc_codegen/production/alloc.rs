//! The M-G1/M-G2 allocator: the legacy `gc_alloc` ABI over `alloc_into`,
//! with close/bind refill, journal drains, evacuation on exhaustion and an
//! honest OOM.
use super::*;

pub(super) fn emit_alloc<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
    stress: bool,
) {
    a.bind_text_label(l.alloc);
    a.push_reg(R::V3);
    load_runtime(a, runtime);
    // Advance an open relocation phase by one bounded quantum (or, with a
    // worker, finish one it scanned); maybe start a mark cycle.
    a.push_reg(R::V7);
    a.push_reg(R::V6);
    a.call_label(l.quantum);
    mark::trigger(a, l);
    a.pop_reg(R::V6);
    a.pop_reg(R::V7);
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_SIZE), R::V7);
    // Legacy shape tags equal the builtin descriptor ids (1, 2, 4).
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_TAG), R::V6);
    if stress {
        // --gc-stress: evacuate before every allocation, so any reference
        // left outside a RootCell across an allocation is caught at once.
        a.call_label(l.collect);
    }
    a.call_label(l.alloc_raw);
    worker::wake_if_pending(a, l);
    a.pop_reg(R::V3);
    a.ret();

    // alloc_raw: V3 = runtime, size in alloc_size -> V0 = payload address.
    a.bind_text_label(l.alloc_raw);
    // Publish the previous object before its page can close, then empty the
    // scratch root that alloc_into requires.
    a.call_label(l.publish_pending);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_ROOT + REQUEST_VALUE), R::V0);
    root_step(a, l, RT_ALLOC_ROOT, ROOT_STORE);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_ALLOC_SIZE));
    a.mov_imm64(R::V1, MAX_PAYLOAD);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.too_large);
    // Descriptor and element count from the tag: raw bytes count bytes,
    // records count reference words, lists add one fixed length word.
    let record = a.create_text_label();
    let list = a.create_text_label();
    let chosen = a.create_text_label();
    a.load_ptr_disp32(R::V1, R::V3, d(RT_ALLOC_TAG));
    a.cmp_reg_imm8(R::V1, 2);
    a.jcc_label(C::Equal, record);
    a.cmp_reg_imm8(R::V1, 4);
    a.jcc_label(C::Equal, list);
    a.cmp_reg_imm8(R::V1, 1);
    a.jcc_label(C::NotEqual, l.fail);
    lea_rt(a, R::V2, RT_BUILTINS + 8);
    a.jmp_label(chosen);
    a.bind_text_label(record);
    lea_rt(a, R::V2, RT_BUILTINS + 72);
    a.shr_reg_imm8(R::V0, 3);
    a.jmp_label(chosen);
    a.bind_text_label(list);
    lea_rt(a, R::V2, RT_BUILTINS + 136);
    a.sub_reg_imm8(R::V0, 8);
    a.shr_reg_imm8(R::V0, 3);
    a.bind_text_label(chosen);
    a.store_ptr_disp32(
        R::V3,
        d(RT_REQUEST + offset_of!(AllocationRequest, descriptor)),
        R::V2,
    );
    a.store_ptr_disp32(
        R::V3,
        d(RT_REQUEST + offset_of!(AllocationRequest, elements)),
        R::V0,
    );
    let attempt = a.create_text_label();
    let done = a.create_text_label();
    let refill = a.create_text_label();
    let not_refill = a.create_text_label();
    let bind = a.create_text_label();
    a.bind_text_label(attempt);
    lea_rt(a, R::V7, RT_CTX);
    lea_rt(a, R::V6, RT_INTO);
    a.call_label(l.alloc_into);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm8(R::V0, ALLOC_REFILL as i8);
    a.jcc_label(C::Equal, refill);
    a.bind_text_label(not_refill);
    a.cmp_reg_imm8(R::V0, ALLOC_METADATA_REFILL as i8);
    a.jcc_label(C::Equal, l.oom);
    a.cmp_reg_imm8(R::V0, ALLOC_ROOT_FLUSH as i8);
    a.jcc_label(C::NotEqual, l.fail);
    a.call_label(l.drain);
    a.jmp_label(attempt);
    // Refill: close the current page (if any) and bind the next prepared one.
    a.bind_text_label(refill);
    a.load_ptr_disp32(
        R::V6,
        R::V3,
        d(RT_REQUEST + offset_of!(AllocationRequest, page)),
    );
    a.test_reg_reg(R::V6, R::V6);
    a.jcc_label(C::Equal, bind);
    lea_rt(a, R::V7, RT_CTX);
    a.mov_imm64(R::V2, 1);
    a.call_label(l.tlab_page);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(
        R::V3,
        d(RT_REQUEST + offset_of!(AllocationRequest, page)),
        R::V0,
    );
    a.bind_text_label(bind);
    let have_page = a.create_text_label();
    if l.worker.is_some() {
        // The worker takes pages from the top: claim next_page by a CAS on
        // the packed bounds (next_page | copy_low << 32).
        let retry = a.create_text_label();
        let full = a.create_text_label();
        a.bind_text_label(retry);
        a.load_ptr_disp32(R::V0, R::V3, d(RT_BOUNDS));
        a.mov_reg_reg(R::V1, R::V0);
        a.shl_reg_imm8(R::V1, 32);
        a.shr_reg_imm8(R::V1, 32);
        a.mov_reg_reg(R::V2, R::V0);
        a.shr_reg_imm8(R::V2, 32);
        a.cmp_reg_reg(R::V1, R::V2);
        a.jcc_label(C::AboveOrEqual, full);
        a.mov_reg_reg(R::V8, R::V0);
        a.mov_reg_reg(R::V6, R::V0);
        a.add_reg_imm32(R::V6, 1);
        a.mov_reg_reg(R::V7, R::V3);
        a.add_reg_imm32(R::V7, d(RT_BOUNDS));
        a.atomic_compare_exchange(R::V7, R::V6);
        a.cmp_reg_reg(R::V0, R::V8);
        a.jcc_label(C::NotEqual, retry);
        a.mov_reg_reg(R::V0, R::V1);
        a.jmp_label(have_page);
        a.bind_text_label(full);
    } else {
        a.load_ptr_disp32(R::V0, R::V3, d(RT_NEXT_PAGE));
        a.load_ptr_disp32(R::V1, R::V3, d(RT_SPACE_END));
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::Below, have_page);
    }
    // The semispace is full: evacuate once; a second exhaustion (or one
    // while evacuating) means the live data does not fit.
    a.load_ptr_disp32(R::V0, R::V3, d(RT_COLLECTING));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.oom);
    a.call_label(l.collect);
    // Evacuation may have bound a to-space page into the TLAB already, so
    // retry the allocation instead of binding another page here.
    a.load_ptr_disp32(R::V0, R::V3, d(RT_NEXT_PAGE));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_SPACE_END));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, l.oom);
    a.jmp_label(attempt);
    a.bind_text_label(have_page);
    // page = pages + index * 112 (= index*128 - index*16).
    a.mov_reg_reg(R::V6, R::V0);
    a.shl_reg_imm8(R::V6, 7);
    a.mov_reg_reg(R::V1, R::V0);
    a.shl_reg_imm8(R::V1, 4);
    a.sub_reg_reg(R::V6, R::V1);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_PAGES));
    a.add_reg_reg(R::V6, R::V1);
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_NEXT_PAGE), R::V0);
    a.store_ptr_disp32(R::V3, d(RT_BIND_PAGE), R::V6);
    lea_rt(a, R::V7, RT_CTX);
    a.mov_imm64(R::V2, 0);
    a.call_label(l.tlab_page);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_BIND_PAGE));
    a.store_ptr_disp32(
        R::V3,
        d(RT_REQUEST + offset_of!(AllocationRequest, page)),
        R::V0,
    );
    a.jmp_label(attempt);
    // Success: remember the object for publication and answer its address.
    a.bind_text_label(done);
    a.load_ptr_disp32(
        R::V0,
        R::V3,
        d(RT_RESULT + offset_of!(AllocationResult, reference)),
    );
    a.store_ptr_disp32(R::V3, d(RT_PENDING_REF), R::V0);
    a.load_ptr_disp32(
        R::V1,
        R::V3,
        d(RT_REQUEST + offset_of!(AllocationRequest, page)),
    );
    a.store_ptr_disp32(R::V3, d(RT_PENDING_PAGE), R::V1);
    // The object's forward cell is the table's last used cell.
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(AllocationPage, forward_table)));
    a.load_ptr_disp32(R::V2, R::V1, d(offset_of!(ForwardTable, used)));
    a.sub_reg_imm8(R::V2, 1);
    a.shl_reg_imm8(R::V2, 5);
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(ForwardTable, cells)));
    a.add_reg_reg(R::V1, R::V2);
    a.store_ptr_disp32(R::V3, d(RT_PENDING_CELL), R::V1);
    mark::alloc_black(a, l);
    decode(a, R::V0, R::V1);
    a.ret();
}

fn source_access<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    a.load_ptr_disp32(R::V7, R::V3, d(RT_PENDING_CELL));
    lea_rt(a, R::V6, RT_ACCESS);
    a.mov_imm64(R::V2, op);
    a.call_label(l.source_access);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}

/// ENTER the pending object's source guard, publish readiness, EXIT.
pub(super) fn emit_publish_pending<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    a.bind_text_label(l.publish_pending);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_PENDING_REF));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    let access = |field: usize| d(RT_ACCESS + field);
    let publish = |field: usize| d(RT_PUBLISH + field);
    let lease = |field: usize| d(RT_PUBLISH + offset_of!(GuardedMarkPrepare, lease) + field);
    lea_rt(a, R::V1, RT_CTX);
    a.store_ptr_disp32(
        R::V3,
        access(offset_of!(SourceAccessRequest, context)),
        R::V1,
    );
    a.store_ptr_disp32(
        R::V3,
        publish(offset_of!(GuardedMarkPrepare, reference)),
        R::V0,
    );
    a.shr_reg_imm8(R::V0, 16);
    a.store_ptr_disp32(
        R::V3,
        access(offset_of!(SourceAccessRequest, source_offset)),
        R::V0,
    );
    a.load_ptr_disp32(R::V1, R::V3, d(RT_PENDING_PAGE));
    a.store_ptr_disp32(R::V3, publish(offset_of!(GuardedMarkPrepare, page)), R::V1);
    a.store_ptr_disp32(R::V3, lease(offset_of!(MetadataLeaseRequest, page)), R::V1);
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(AllocationPage, incarnation)));
    a.store_ptr_disp32(
        R::V3,
        access(offset_of!(SourceAccessRequest, incarnation)),
        R::V1,
    );
    a.store_ptr_disp32(
        R::V3,
        lease(offset_of!(MetadataLeaseRequest, incarnation)),
        R::V1,
    );
    a.mov_imm64(R::V1, PUBLISH_LEASE_TOKEN);
    a.store_ptr_disp32(R::V3, lease(offset_of!(MetadataLeaseRequest, token)), R::V1);
    lea_rt(a, R::V1, RT_ACCESS);
    a.store_ptr_disp32(R::V3, publish(offset_of!(GuardedMarkPrepare, scope)), R::V1);
    source_access(a, l, SOURCE_ENTER);
    lea_rt(a, R::V7, RT_PUBLISH);
    a.call_label(l.publish);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    source_access(a, l, SOURCE_EXIT);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_PENDING_REF), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_PUBLISHED));
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_PUBLISHED), R::V0);
    a.bind_text_label(done);
    a.ret();
}
