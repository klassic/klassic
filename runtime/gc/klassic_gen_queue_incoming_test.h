/* Compare the linked normal pass with its retained array fallback on the same
 * actual runtime queues; genuine TAKE/YIELD and scan/REFILL exercise stale
 * snapshots and slot reuse without fabricating liveness/metadata counts. */
static uint64_t incoming_until_result(NativeQueueIncoming *q){
    uint64_t status,turns=0;do{assert(++turns<100000);status=gc_native_queue_incoming_next_locked(q);}while(status==GC_NSI_MORE);
    return status;
}
static void incoming_differential_and_races(KgRuntime *r,KgDriver *d){
    kg_lock(r);NativeWorkStorage *work=gc_native_runtime_work(&r->native);MarkQueue *old=&work->queues[GC_OLD];
    uint64_t active=kg_leases(r)->active;MarkTask *tasks=(void *)(uintptr_t)old->tasks;
    uint64_t normal=tasks[old->head-1].reference>>16,spill=gc_mark_spill_cell(old,old->overflow_head)->reference>>16;
    uint64_t starts[]={normal,spill,(work->leases.heap? r->native.backing.heap.page_count+1:0)*GC_HEAP_PAGE_BYTES};
    for(unsigned i=0;i<3;i++){
        NativeQueueIncoming fast={0},legacy={0};
        assert(gc_native_queue_incoming_begin_locked(&r->native,starts[i],starts[i]+16,&fast)==GC_NSI_MORE);
        assert(gc_native_queue_incoming_prefer_fifo_locked(&fast)==GC_NSI_MORE);
        uint64_t status=incoming_until_result(&fast),observed=fast.observed;
        assert(status==(i==2?GC_NSI_DONE:GC_NSI_FOUND));
        assert(gc_native_queue_incoming_cancel_locked(&fast)==GC_NSI_DONE);
        assert(gc_native_queue_incoming_begin_locked(&r->native,starts[i],starts[i]+16,&legacy)==GC_NSI_MORE);
        legacy.linked[0]=legacy.linked[1]=0;legacy.cursor=0; /* Same receipt, original physical array algorithm. */
        assert(incoming_until_result(&legacy)==status&&legacy.observed==observed);
        assert(gc_native_queue_incoming_cancel_locked(&legacy)==GC_NSI_DONE&&kg_leases(r)->active==active);
    }
    NativeQueueIncoming claimed={0};
    assert(gc_native_queue_incoming_begin_locked(&r->native,normal,normal+16,&claimed)==GC_NSI_MORE);
    assert(gc_native_queue_incoming_prefer_fifo_locked(&claimed)==GC_NSI_MORE&&claimed.linked[1]);
    uint64_t serial=old->last_serial;MarkRequest owner={0};
    assert(gc_mark_step_locked(old,&owner,GC_MARK_TAKE)==GC_MARK_OK&&old->in_flight==1&&old->last_serial==serial);
    assert(gc_native_queue_incoming_next_locked(&claimed)==GC_NSI_RESTART);
    assert(gc_native_queue_incoming_cancel_locked(&claimed)==GC_NSI_DONE);
    NativeQueueIncoming fallback={0};
    assert(gc_native_queue_incoming_begin_locked(&r->native,normal,normal+16,&fallback)==GC_NSI_MORE);
    assert(gc_native_queue_incoming_prefer_fifo_locked(&fallback)==GC_NSI_MORE&&!fallback.linked[1]);
    assert(incoming_until_result(&fallback)==GC_NSI_FOUND&&fallback.observed==owner.reference);
    assert(gc_native_queue_incoming_cancel_locked(&fallback)==GC_NSI_DONE);
    MarkRequest stale=owner;
    assert(gc_mark_step_locked(old,&owner,GC_MARK_YIELD)==GC_MARK_OK&&old->last_serial>serial);
    MarkRequest before=stale;
    assert(gc_mark_step_locked(old,&stale,GC_MARK_FINISH)==GC_MARK_INVALID&&!memcmp(&stale,&before,sizeof(stale)));
    NativeQueueIncoming recycled={0};uint64_t slot=old->head;
    assert(gc_native_queue_incoming_begin_locked(&r->native,normal,normal+16,&recycled)==GC_NSI_MORE);
    assert(gc_native_queue_incoming_prefer_fifo_locked(&recycled)==GC_NSI_MORE);
    uint64_t original_record=tasks[slot-1].record,original_serial=tasks[slot-1].serial;serial=old->last_serial;
    kg_unlock(r);kgd_scan(d,GC_OLD);kg_lock(r);
    assert(!tasks[slot-1].state&&old->last_serial==serial);
    assert(gc_mark_spill_locked(old,NULL,GC_MARK_REFILL)==GC_MARK_OK);
    assert(tasks[slot-1].state==1&&tasks[slot-1].record!=original_record&&tasks[slot-1].serial!=original_serial&&old->last_serial==serial);
    assert(gc_native_queue_incoming_next_locked(&recycled)==GC_NSI_RESTART);
    assert(gc_native_queue_incoming_cancel_locked(&recycled)==GC_NSI_DONE&&kg_leases(r)->active==active-1);
    kg_unlock(r);
}
