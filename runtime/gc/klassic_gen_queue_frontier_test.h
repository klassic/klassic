/* Real yielded receiver + source-prepared rooted producer, appended exactly
 * after a captured rebind tail and before that pass's terminal transition. */
static KgLeaseOps queue_real_lease_ops;
static NativeMetadataOps queue_real_metadata_ops;
static unsigned queue_fail_map,queue_fail_commit;
static void *queue_map_fault(size_t bytes){
    if(queue_fail_map){queue_fail_map--;return NULL;}return queue_real_metadata_ops.map(bytes);
}
static bool queue_commit_fault(void *address,size_t bytes){
    assert(kg_owned_driver&&atomic_load_explicit(&kg_owned_driver->actor.state,memory_order_acquire)==GC_BLOCKED_NATIVE);
    if(queue_fail_commit){queue_fail_commit--;return false;}return queue_real_lease_ops.commit(address,bytes);
}
static void partial_and_late(bool full,bool moving){
    kg_configure(UINT64_C(16)*1024*1024,0,UINT64_C(8)*1024*1024,0);kg_init();
    KgRuntime *r=&kg_runtime;RootCell *prefix=kg_root_new(),*graph=kg_root_new(),*child=kg_root_new(),*observed=kg_root_new();
    kg_alloc_into(8,1,prefix);kg_store_i64(prefix,0,1717);
    kg_alloc_into(40*8,2,graph);
    if(!moving)for(unsigned i=0;i<2;i++)kg_driver_pressure(r,KG_PRESSURE_METADATA);
    for(unsigned i=0;i<40;i++){kg_alloc_into(8,1,child);kg_store_i64(child,0,3000+i);kg_store_ref(graph,i*8,child);}
    kg_root_clear(child);kg_enter_native(r);
    uint64_t owned=kg_driver_cleanup_enter(r,NULL);assert(!owned);KgDriver *d=r->driver;uint64_t turns=0;
    /* The delayed producer must own the first mark of its target. Earlier
     * child values can remain in root journals after their roots are cleared.
     * Acknowledge those real entries while the driver is idle. */
    kg_lock(r);assert(!d->state&&!d->background_old&&!(r->native.roots.phase&255));kg_unlock(r);
    for(;;){
        uint64_t pending=0;kg_lock(r);
        for(KgRootBlock *b=kg_current_mutator(r)->root_blocks;b;b=b->next){
            BufferCursor *j=(void *)(uintptr_t)b->roots->segment.journal;
            pending+=atomic_load_explicit(&j->published,memory_order_acquire)-atomic_load_explicit(&j->consumed,memory_order_acquire);
        }
        kg_unlock(r);if(!pending)break;kg_drain_journals(r);assert(++turns<300000);
    }
    kg_lock(r);d->requested[0]=d->requested[1]=1;kg_unlock(r);
    while(d->state!=3||d->background_old!=3){assert(++turns<300000);quantum(r,d);}
    while(d->root_started!=2){assert(++turns<300000);(void)kgd_roots(d,false);}
    while(d->old_root_started!=2){uint64_t gen=d->generation,start=d->root_started;
        d->generation=GC_OLD;d->root_started=d->old_root_started;(void)kgd_roots(d,false);
        d->old_root_started=d->root_started;d->generation=gen;d->root_started=start;}
    /* Authenticate the requested receiver. Other genuine roots/journals may
     * be ahead of it; a single FIFO TAKE does not identify this graph. The
     * explicit prefix root makes that schedule part of every regression. */
    PendingMark identity={.context=(uintptr_t)&d->actor,
        .current={.reference=atomic_load_explicit(&graph->value,memory_order_acquire)}};
    assert(kgd_enter(d,&identity)==GC_RAC_READY);uint64_t bytes,descriptor,expected_record;
    kgd_shape(d,&identity,&bytes,&descriptor,&expected_record);assert(bytes==320&&descriptor==2);
    kgd_exit(&identity);kgd_release(d,&identity);
    MarkQueue *old=&gc_native_runtime_work(&r->native)->queues[GC_OLD];MarkTask *tasks;MarkTask partial={0};unsigned receiver_slices=0;
    do{
        kg_lock(r);assert(gc_mark_step_locked(old,NULL,GC_MARK_CHECK)==GC_MARK_BUSY);kg_unlock(r);
        assert(++receiver_slices<128);kgd_scan(d,GC_OLD);kg_lock(r);tasks=(void *)(uintptr_t)old->tasks;
        for(uint64_t i=0;i<old->capacity;i++)if(tasks[i].state==1&&tasks[i].record==expected_record&&tasks[i].cursor)partial=tasks[i];
        kg_unlock(r);
    }while(!partial.record);
    kg_lock(r);assert(partial.record==expected_record&&partial.cursor==16*8&&old->overflow_count>=15);
    uint64_t epoch=old->epoch;kg_unlock(r);
    assert(receiver_slices>=2);fprintf(stderr,"actual receiver selected after %u real FIFO scans, record=%llx cursor=%llu\n",
        receiver_slices,(unsigned long long)partial.record,(unsigned long long)partial.cursor);
    while(d->state!=9||d->copy_stage){assert(++turns<300000);quantum(r,d);}
    if(moving){
        bool rebound=false;
        while(d->state||d->background_old||d->requested[0]||d->requested[1]){
            assert(++turns<300000);quantum(r,d);
            if(d->generation==GC_YOUNG&&d->state==12){
                kg_lock(r);assert(old->epoch==epoch);tasks=(void *)(uintptr_t)old->tasks;
                for(uint64_t i=0;i<old->capacity;i++)if(tasks[i].record==partial.record){
                    assert(tasks[i].state==1&&tasks[i].cursor==partial.cursor&&tasks[i].reference!=partial.reference&&
                        tasks[i].metadata_lease!=partial.metadata_lease);
                    MetadataLeaseRequest gone={.token=partial.metadata_lease};
                    assert(gc_metadata_step_locked(kg_leases(r),&gone,GC_ML_RESOLVE)==GC_ML_INVALID);
                    PendingMark probe={.context=(uintptr_t)&d->actor,.current={.reference=tasks[i].reference}};kg_unlock(r);
                    assert(kgd_enter(d,&probe)==GC_RAC_READY);uint64_t bytes,descriptor,record;
                    kgd_shape(d,&probe,&bytes,&descriptor,&record);assert(bytes==320&&descriptor==2&&record==partial.record);
                    kgd_exit(&probe);kgd_release(d,&probe);kg_lock(r);rebound=true;
                }
                kg_unlock(r);
            }
        }
        assert(rebound);kgd_native(d,true);kg_driver_cleanup_leave(r,owned);kg_leave_native(r);
        for(unsigned i=0;i<40;i++){kg_load_ref_into(graph,i*8,child);assert(kg_load_i64(child,0)==3000+i);}
        kg_root_release(observed);kg_root_release(child);kg_root_release(graph);kg_root_release(prefix);kg_shutdown();
        assert(!r->native.allocations&&!r->native.backing.heap.metadata_committed&&!r->native.backing.heap.metadata_pending);
        puts("C task capacity2: moving Young receiver keeps real Old cursor128, destination native arena/token, stale source token rejection and budgets0 PASS");return;
    }
    kgd_native(d,true);kg_leave_native(r);kg_root_publication_begin(r);
    RootedLoad q={.observation={.registry=(uintptr_t)kg_descriptors(r),.byte_offset=39*8}};
    kg_lock(r);q.observation.receiver.follow.context=(uintptr_t)kg_current_context(r);
    q.observation.receiver.follow.root=kg_root_request_locked(r,graph);
    q.observation.output=kg_root_request_locked(r,observed);kg_unlock(r);
    RootedLoadAdmission admission;RootedStoreRuntime binding=kg_dispatch(r,&admission);
    while(q.stage!=GC_RLOAD_MARKING||q.marks.current!=GC_OLD||q.marks.jobs[GC_OLD].stage!=3){
        uint64_t status=gc_rooted_load_dispatch(&binding,&q,GC_RLD_STEP);
        assert(status!=GC_RLOAD_INVALID&&status!=GC_RLOAD_ERROR&&status!=GC_RLOAD_EXHAUSTED&&status!=GC_RLOAD_WAIT);
        assert(++turns<300000);
    }
    assert(!atomic_load_explicit(&kg_current_context(r)->access_epoch,memory_order_acquire));
    assert(kg_current_context(r)->private_producers==1);
    uint64_t late_record=q.marks.jobs[GC_OLD].route.prepare.output.record;
    uint64_t late_source_token=q.marks.jobs[GC_OLD].route.prepare.output.metadata_lease;
    assert(late_record&&late_source_token);
    /* Keep the real source-prepared producer counted while copies finish.
     * No phase transition/owner poll occurs with that private producer live. */
    kg_lock(r);kgd_adopt(d);kg_unlock(r);
    while(!d->rebind_stage){assert(++turns<300000);assert(!kgd_copy(d));}
    assert(d->rebind_stage==1&&d->old_frozen&&d->cursor==d->rebind_source);
    KgRun *source=d->cursor;uint64_t captured_tail=d->rebind_tail;
    NativeQueueIncoming late_incoming={0};
    kg_lock(r);uint64_t before=old->last_serial;
    assert(((MarkRecord *)(uintptr_t)late_record)->old_epoch!=old->epoch);
    assert(gc_native_queue_incoming_begin_locked(&r->native,3*GC_HEAP_PAGE_BYTES,4*GC_HEAP_PAGE_BYTES,&late_incoming)==GC_NSI_MORE);
    assert(gc_native_queue_incoming_prefer_fifo_locked(&late_incoming)==GC_NSI_MORE);kg_unlock(r);
    kgd_native(d,true);
    for(;;){uint64_t status=gc_rooted_load_dispatch(&binding,&q,GC_RLD_STEP);
        if(status==GC_RLOAD_DONE)break;
        assert(status!=GC_RLOAD_INVALID&&status!=GC_RLOAD_ERROR&&status!=GC_RLOAD_EXHAUSTED&&status!=GC_RLOAD_WAIT);
        assert(++turns<300000);}
    kg_root_publication_end(r);kg_enter_native(r);
    kg_lock(r);assert(gc_native_queue_incoming_next_locked(&late_incoming)==GC_NSI_RESTART);
    assert(gc_native_queue_incoming_cancel_locked(&late_incoming)==GC_NSI_DONE);fprintf(stderr,"late before=%llu after=%llu captured=%llx tail=%llx record=%llx\n",(unsigned long long)before,(unsigned long long)old->last_serial,(unsigned long long)captured_tail,(unsigned long long)old->overflow_tail,(unsigned long long)late_record);assert(old->overflow_tail==late_record&&late_record!=captured_tail&&old->last_serial>before);
    MarkTask *late=gc_mark_spill_cell(old,late_record);
    assert(late->metadata_lease==late_source_token);MetadataLeaseRequest lease={.token=late_source_token};
    assert(gc_metadata_step_locked(kg_leases(r),&lease,GC_ML_RESOLVE)==GC_ML_OK);
    assert(lease.page==(uintptr_t)gc_native_allocation_page(&source->native));kg_unlock(r);
    if(full){
        MetadataLeaseRequest held[2048]={0};uint64_t count=0;
        kg_lock(r);ReferenceLease graph_lease={.reference=atomic_load_explicit(&graph->value,memory_order_acquire)};
        assert(gc_reference_lease_locked(kg_leases(r),&graph_lease)==GC_ML_OK);
        AllocationPage *page=(void *)(uintptr_t)graph_lease.lease.page;held[count++]=graph_lease.lease;
        uint64_t base=kg_leases(r)->slots,capacity=kg_leases(r)->capacity;
        while(kg_leases(r)->free_head){assert(count<2048);held[count]=(MetadataLeaseRequest){.page=(uintptr_t)page,.incarnation=page->incarnation};
            assert(gc_metadata_step_locked(kg_leases(r),&held[count++],GC_ML_ACQUIRE)==GC_ML_OK);}
        KgLeaseBacking *owner=r->lease_backing;queue_real_lease_ops=owner->ops;queue_real_metadata_ops=r->os.metadata;
        owner->ops.commit=queue_commit_fault;r->os.metadata.map=queue_map_fault;
        queue_fail_map=queue_fail_commit=1;kgd_adopt(d);kg_unlock(r);
        for(unsigned i=0;kg_leases(r)->capacity==capacity;i++){assert(i<8);assert(!kgd_old_rebind(d,source));}
        kg_lock(r);assert(kg_leases(r)->slots==base&&kg_leases(r)->capacity>capacity);
        assert(!queue_fail_map&&!queue_fail_commit);
        for(uint64_t i=0;i<count;i++){uint64_t token=held[i].token;
            assert(gc_metadata_step_locked(kg_leases(r),&held[i],GC_ML_RESOLVE)==GC_ML_OK&&held[i].token==token);
            assert(gc_metadata_step_locked(kg_leases(r),&held[i],GC_ML_RELEASE)==GC_ML_OK);}
        owner->ops=queue_real_lease_ops;r->os.metadata=queue_real_metadata_ops;kg_unlock(r);
    }
    bool rebound=false,yield_preserved=false;uint64_t restarts=0;
    while(d->state||d->background_old||d->requested[0]||d->requested[1]){
        uint64_t prior=d->rebind_stage;
        if(++turns>=300000){fprintf(stderr,"stalled state=%llu rebind=%llu expected=%llu serial=%llu sourceleases=%llu\n",
            (unsigned long long)d->state,(unsigned long long)d->rebind_stage,(unsigned long long)d->rebind_revision,
            (unsigned long long)old->last_serial,(unsigned long long)gc_native_allocation_page(&source->native)->metadata_leases);assert(false);}
        quantum(r,d);
        if(prior==3&&!d->rebind_stage)restarts++;
        if(d->generation==GC_YOUNG&&d->state==12){kg_lock(r);
            assert(old->epoch==epoch);late=gc_mark_spill_cell(old,late_record);
            assert(late->record==late_record&&late->state==1&&late->metadata_lease!=late_source_token);
            lease=(MetadataLeaseRequest){.token=late->metadata_lease};
            assert(gc_metadata_step_locked(kg_leases(r),&lease,GC_ML_RESOLVE)==GC_ML_OK);
            assert(lease.page!=(uintptr_t)gc_native_allocation_page(&source->native));rebound=true;
            tasks=(void *)(uintptr_t)old->tasks;
            for(uint64_t i=0;i<old->capacity;i++)if(tasks[i].record==partial.record){
                assert(tasks[i].state==1&&tasks[i].cursor==partial.cursor);yield_preserved=true;}
            kg_unlock(r);
        }
    }
    assert(rebound&&yield_preserved&&restarts);
    kgd_native(d,true);kg_driver_cleanup_leave(r,owned);kg_leave_native(r);
    for(unsigned i=0;i<40;i++){kg_load_ref_into(graph,i*8,child);assert(kg_load_i64(child,0)==3000+i);}
    assert(kg_load_i64(observed,0)==3039);
    kg_root_release(observed);kg_root_release(child);kg_root_release(graph);kg_root_release(prefix);kg_shutdown();
    assert(!r->native.allocations&&!r->native.backing.heap.metadata_committed&&!r->native.backing.heap.metadata_pending);
    puts("C task capacity2: real yielded cursor, actual late prepared producer, revision restart, canonical token handoff and budgets0 PASS");
}
