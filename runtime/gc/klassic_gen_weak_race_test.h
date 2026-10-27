/* Actual owner registration and shutdown/processor metadata ownership. */
#include <pthread.h>
#include <sched.h>
static _Atomic bool weak_shutdown_hold;
static _Atomic unsigned weak_stop_checked,weak_stop_release,weak_stop_late_step;
static _Atomic uint64_t weak_stop_phase;
static uint64_t weak_shutdown_ownership;
static _Atomic unsigned weak_shutdown_captured,weak_shutdown_stale;
static void weak_shutdown_after_drop(void *raw){
    if(!weak_shutdown_hold)return;
    KgRuntime *r=raw;kg_lock(r);KgWeakInventory *w=kgw_owner(r);
    assert(w->table.active==0&&w->mark.processor==(uintptr_t)&w->scan.work&&w->scan.work.stage==1);
    MetadataLeaseRequest private=w->scan.work.current.lease;
    assert(private.token&&gc_metadata_step_locked(kg_leases(r),&private,GC_ML_RESOLVE)==GC_ML_OK);
    assert(!gc_weak_table_entry_locked(&w->table,w->scan.work.handle));
    atomic_store_explicit(&weak_shutdown_captured,1,memory_order_release);kg_unlock(r);
    KgDriver *d=r->driver;assert(kg_owned_driver==d);kgd_native(d,true);kg_driver_cleanup_leave(r,weak_shutdown_ownership);
}
static void weak_commit_observed(void *raw,void *work){
    (void)raw;WeakProcess *q=work;
    if(weak_shutdown_hold&&q->stage==6&&q->result==GC_WP_STALE){
        assert(!q->current.lease.token&&!q->retired.token&&!q->owner->processor);
        atomic_store_explicit(&weak_shutdown_stale,1,memory_order_release);
    }
}
static void weak_shutdown_scan(void){
    kg_configure(UINT64_C(16)*1024*1024,0,UINT64_C(8)*1024*1024,4);kg_init();
    KgRuntime *r=&kg_runtime;RootCell *root=kg_root_new();kg_alloc_into(8,1,root);kg_store_i64(root,0,9911);
    KgWeakHandle handle=kg_weak_new(root);(void)handle;kg_root_release(root);
    kg_enter_native(r);weak_shutdown_ownership=kg_driver_cleanup_enter(r,NULL);assert(!weak_shutdown_ownership);
    KgDriver *d=r->driver;kg_lock(r);d->requested[0]=d->requested[1]=1;kg_unlock(r);uint64_t turns=0;
    while(kgw_owner(r)->scan.work.stage!=1){
        assert(++turns<2000000);kg_lock(r);kgd_adopt(d);kg_unlock(r);kgd_step(d);
        assert(!atomic_load_explicit(&d->actor.access_epoch,memory_order_acquire));
    }
    weak_shutdown_hold=true;kgd_native(d,true);kg_leave_native(r);
    kg_shutdown();
    assert(atomic_load_explicit(&weak_shutdown_captured,memory_order_acquire)&&atomic_load_explicit(&weak_shutdown_stale,memory_order_acquire));
    assert(atomic_load_explicit(&weak_stop_late_step,memory_order_acquire));
    assert(!kg_weak_inventory&&!r->native.allocations&&!r->native.backing.heap.metadata_committed&&!r->native.backing.heap.metadata_pending);
    puts("C weak shutdown: actual captured processor retains private lease across handle drop, stale cleanup and final OS budgets0 PASS");
}
typedef struct {
    KgWeakHandle shared,returned;
    _Atomic unsigned *ready,*go,*released,*loads;
    unsigned count;
} WeakPeer;
static void *weak_peer(void *raw){
    WeakPeer *p=raw;kg_thread_attach();RootCell *out=kg_root_new();
    assert(kg_weak_load_into(p->shared,out)&&kg_load_i64(out,0)==2026);
    kg_native_enter();atomic_fetch_add_explicit(p->ready,1,memory_order_release);
    while(!atomic_load_explicit(p->go,memory_order_acquire))sched_yield();kg_native_exit();
    assert(kg_load_i64(out,0)==2026);KgWeakHandle held[320];
    for(unsigned i=0;i<320;i++)held[i]=kg_weak_new(out);
    for(unsigned i=0;i<320;i++)assert(kg_weak_release(held[i]));
    for(unsigned i=0;i<48;i++){
        if(kg_weak_load_into(p->shared,out)){assert(kg_load_i64(out,0)==2026);atomic_fetch_add_explicit(p->loads,1,memory_order_relaxed);}
        if(!(i%13))weak_major();
        if(!(i%7)){kg_native_enter();kg_native_exit();}
    }
    if(kg_weak_release(p->shared))atomic_fetch_add_explicit(p->released,1,memory_order_relaxed);
    assert(!kg_weak_load_into(p->shared,out));
    kg_alloc_into(8,1,out);kg_store_i64(out,0,9000+p->count);p->returned=kg_weak_new(out);
    kg_root_release(out);kg_thread_detach();return NULL;
}
static void weak_nowners(unsigned count){
    kg_configure(UINT64_C(32)*1024*1024,0,UINT64_C(16)*1024*1024,4);kg_init();
    RootCell *root=kg_root_new();kg_alloc_into(8,1,root);kg_store_i64(root,0,2026);KgWeakHandle shared=kg_weak_new(root);
    _Atomic unsigned ready=0,go=0,released=0,loads=0;WeakPeer peers[8];pthread_t threads[8];
    kg_native_enter();for(unsigned i=0;i<count;i++){peers[i]=(WeakPeer){shared,{0},&ready,&go,&released,&loads,i};assert(!pthread_create(&threads[i],NULL,weak_peer,&peers[i]));}
    while(atomic_load_explicit(&ready,memory_order_acquire)!=count)sched_yield();kg_native_exit();kg_root_clear(root);
    weak_major();weak_major(); /* Every actual Native owner's output remains a strong root. */
    atomic_store_explicit(&go,1,memory_order_release);kg_native_enter();
    for(unsigned i=0;i<count;i++)assert(!pthread_join(threads[i],NULL));kg_native_exit();
    assert(atomic_load_explicit(&released,memory_order_acquire)==1&&atomic_load_explicit(&loads,memory_order_acquire));
    for(unsigned i=0;i<count;i++)assert(kg_weak_release(peers[i].returned)); /* Creator detach does not destroy global handles. */
    assert(!kg_weak_release(shared));kg_verify();kg_root_release(root);kg_shutdown();
    assert(!kg_weak_inventory&&!kg_runtime.native.allocations&&!kg_runtime.native.backing.heap.metadata_committed&&!kg_runtime.native.backing.heap.metadata_pending);
    printf("C weak runtime: %u real owners, rooted keepalive across Native GC, growth/load/release races, creator detach and budgets0 PASS\n",count);
}
static KgLeaseOps weak_real_ops;
static NativeMetadataOps weak_real_metadata;
static unsigned weak_fail_map,weak_fail_commit,weak_fail_decommit,weak_fail_release,weak_fail_unmap;
static uintptr_t weak_unmap_target;
static WeakEntry weak_before_growth[128];
static void weak_fault_prefix_preserved(void){
    KgRuntime *r=&kg_runtime;MutatorContext *ctx=kg_current_context(r);
    assert(ctx&&atomic_load_explicit(&ctx->state,memory_order_acquire)==GC_BLOCKED_NATIVE);
    kg_lock(r);KgWeakInventory *w=kgw_owner(r);assert(w->table.capacity==128);
    assert(!memcmp(w->table.entries,weak_before_growth,sizeof(weak_before_growth)));
    for(unsigned i=0;i<128;i++){MetadataLeaseRequest lease=w->table.entries[i].slot.lease;
        assert(gc_metadata_step_locked(kg_leases(r),&lease,GC_ML_RESOLVE)==GC_ML_OK&&lease.token==weak_before_growth[i].slot.lease.token);}
    kg_unlock(r);
}
static void *weak_map_fault(size_t bytes){
    if(weak_fail_map){weak_fail_map--;weak_fault_prefix_preserved();return NULL;}return weak_real_metadata.map(bytes);
}
static bool weak_commit_fault(void *address,size_t bytes){
    if(weak_fail_commit){weak_fail_commit--;weak_fault_prefix_preserved();assert(weak_real_ops.commit(address,bytes));return false;}
    return weak_real_ops.commit(address,bytes);
}
static bool weak_decommit_fault(void *address,size_t bytes){
    if(weak_fail_decommit){weak_fail_decommit--;assert(!mprotect(address,bytes,PROT_NONE));return false;}return weak_real_ops.decommit(address,bytes);
}
static bool weak_release_fault(void *address,size_t bytes){
    if(weak_fail_release){weak_fail_release--;return false;}return weak_real_ops.release(address,bytes);
}
static bool weak_unmap_fault(void *address,size_t bytes){
    if((uintptr_t)address==weak_unmap_target&&weak_fail_unmap){weak_fail_unmap--;return false;}return weak_real_metadata.unmap(address,bytes);
}
static void weak_faults(void){
    kg_configure(UINT64_C(16)*1024*1024,0,UINT64_C(8)*1024*1024,4);kg_init();
    RootCell *root=kg_root_new(),*out=kg_root_new();kg_alloc_into(8,1,root);kg_store_i64(root,0,1212);KgWeakHandle held[129];
    for(unsigned i=0;i<128;i++)held[i]=kg_weak_new(root);
    KgRuntime *r=&kg_runtime;kg_lock(r);KgWeakInventory *w=kgw_owner(r);weak_real_ops=w->ops;weak_real_metadata=r->os.metadata;
    memcpy(weak_before_growth,w->table.entries,sizeof(weak_before_growth));WeakEntry *base=w->table.entries;
    w->ops.commit=weak_commit_fault;w->ops.decommit=weak_decommit_fault;w->ops.release=weak_release_fault;
    r->os.metadata.map=weak_map_fault;r->os.metadata.unmap=weak_unmap_fault;
    weak_fail_map=weak_fail_commit=weak_fail_decommit=weak_fail_release=weak_fail_unmap=1;kg_unlock(r);
    held[128]=kg_weak_new(root);kg_lock(r);assert(w->table.entries==base&&w->table.capacity==384&&w->table.active==129);
    assert(!weak_fail_map&&!weak_fail_commit);weak_unmap_target=w->chunks->control.allocation;assert(weak_unmap_target);kg_unlock(r);
    for(unsigned i=0;i<129;i++)assert(kg_weak_load_into(held[i],out)&&kg_load_i64(out,0)==1212);
    kg_root_release(out);kg_root_release(root);kg_shutdown();
    assert(!weak_fail_decommit&&!weak_fail_release&&!weak_fail_unmap&&!kg_weak_inventory);
    assert(!r->native.allocations&&!r->native.backing.heap.metadata_committed&&!r->native.backing.heap.metadata_pending);
    puts("C weak backing: real map/partial commit/decommit/control unmap/reservation failures, unchanged old tokens and final budgets0 PASS");
}
typedef struct {
    KgWeakHandle handle;RootCell *output;uint64_t identity;
    _Atomic unsigned ready,go,waiting,returned,moving_pause,retire_wait;
} WeakGatePeer;
static WeakGatePeer *weak_gate_peer;
static void weak_load_wait_observed(void *raw){
    if(!weak_gate_peer)return;
    KgRuntime *r=raw;atomic_store_explicit(&weak_gate_peer->waiting,1,memory_order_release);
    kg_lock(r);bool retiring=(r->native.roots.phase&15)==GC_RETIRE;kg_unlock(r);
    if(retiring)atomic_store_explicit(&weak_gate_peer->retire_wait,1,memory_order_release);
}
/* Delay only the caller, outside locks and guards. Both GC handshakes
 * still require the real owner's ACK. This fixes the previously rare case
 * where the open relocation interval is missed before Retire closes again. */
static void weak_before_load(void *raw){
    WeakGatePeer *p=weak_gate_peer;if(!p)return;KgRuntime *r=raw;
    kg_lock(r);bool moving=(r->native.roots.phase&15)==GC_CONCURRENT_RELOCATE&&!kgw_owner(r)->gate.closed;kg_unlock(r);
    if(!moving||atomic_exchange_explicit(&p->moving_pause,1,memory_order_acq_rel))return;
    for(;;){kg_lock(r);bool retiring=(r->native.roots.phase&15)==GC_RETIRE;kg_unlock(r);if(retiring)break;sched_yield();}
}
static void *weak_gate_waiter(void *raw){
    WeakGatePeer *p=raw;kg_thread_attach();p->output=kg_root_new();p->identity=kg_context(&kg_runtime)->thread_id;kg_native_enter();
    atomic_store_explicit(&p->ready,1,memory_order_release);
    while(!atomic_load_explicit(&p->go,memory_order_acquire))sched_yield();kg_native_exit();
    assert(!kg_weak_load_into(p->handle,p->output));
    atomic_store_explicit(&p->returned,1,memory_order_release);kg_root_release(p->output);kg_thread_detach();return NULL;
}
static void weak_closed_gate(void){
    kg_configure(UINT64_C(16)*1024*1024,0,UINT64_C(8)*1024*1024,4);kg_init();
    KgRuntime *r=&kg_runtime;RootCell *root=kg_root_new();kg_alloc_into(8,1,root);kg_store_i64(root,0,4141);
    WeakGatePeer peer={.handle=kg_weak_new(root)};weak_gate_peer=&peer;kg_root_release(root);pthread_t thread;
    kg_native_enter();assert(!pthread_create(&thread,NULL,weak_gate_waiter,&peer));
    while(!atomic_load_explicit(&peer.ready,memory_order_acquire))sched_yield();
    uint64_t ownership=kg_driver_cleanup_enter(r,NULL);assert(!ownership);KgDriver *d=r->driver;
    kg_lock(r);d->requested[GC_YOUNG]=1;kg_unlock(r);uint64_t turns=0;
    while(d->state!=6){assert(++turns<2000000);kg_lock(r);kgd_adopt(d);kg_unlock(r);kgd_step(d);}
    kg_lock(r);KgWeakInventory *w=kgw_owner(r);assert(w->gate.closed&&w->mark.stage==3&&!w->scan.stage);
    WeakHandle handle;assert(kgw_lookup(w,peer.handle,&handle));assert(gc_weak_table_entry_locked(&w->table,handle)->slot.reference);kg_unlock(r);
    atomic_store_explicit(&peer.go,1,memory_order_release);
    while(!atomic_load_explicit(&peer.waiting,memory_order_acquire)){assert(++turns<2000000);sched_yield();}
    assert(!atomic_load_explicit(&peer.returned,memory_order_acquire));
    assert(!atomic_load_explicit(&peer.output->value,memory_order_acquire));
    /* Observe the actual bad handoff condition before fixing the harness:
     * retained busy prevents this Running weak waiter from going Native. */
    /* BEGIN's nonempty cursor is not yet the intended owner handoff. The
     * actual peer can still be AT_SAFEPOINT after its relocation ACK while
     * the handshake has already released. Let its real POLL resume Running,
     * and let the finite audit pass any earlier Native registrations. Never
     * change a context state or accept another owner's cursor as this proof. */
    for(;;){
        kg_lock(r);NativeBufferIncoming *buffers=&d->retirement.buffers;
        if(buffers->self&&buffers->remaining&&buffers->cursor){
            ThreadRegistration *candidate=(void *)(uintptr_t)buffers->cursor;
            MutatorContext *ctx=(void *)(uintptr_t)candidate->context;
            if(candidate->identity==peer.identity&&atomic_load_explicit(&peer.retire_wait,memory_order_acquire)&&
               atomic_load_explicit(&ctx->state,memory_order_acquire)==GC_RUNNING&&
               gc_native_buffer_quiescence_locked(&r->native.handshake,candidate)==GC_NBQ_WAIT)break;
        }
        kg_unlock(r);assert(++turns<2000000);kg_lock(r);kgd_adopt(d);kg_unlock(r);kgd_step(d);kg_yield();
        assert(!atomic_load_explicit(&d->actor.access_epoch,memory_order_acquire));
    }
    NativeBufferIncoming before=d->retirement.buffers;
    ThreadRegistration *registration=(void *)(uintptr_t)before.cursor;MutatorContext *waiter=(void *)(uintptr_t)registration->context;
    assert(registration->identity==peer.identity);
    assert(d->state==12&&d->busy&&kgw_owner(r)->gate.closed&&kgw_owner(r)->retiring==1);
    assert(atomic_load_explicit(&waiter->state,memory_order_acquire)==GC_RUNNING&&
        gc_native_buffer_quiescence_locked(&r->native.handshake,registration)==GC_NBQ_WAIT);
    assert(atomic_load_explicit(&peer.retire_wait,memory_order_acquire)&&!atomic_load_explicit(&peer.returned,memory_order_acquire));
    fprintf(stderr,"actual retained busy: state12 gateclosed1 waiter%llu Running, BufferIncoming WAIT remaining%llu cursor%llx\n",
        (unsigned long long)waiter->thread_id,(unsigned long long)before.remaining,(unsigned long long)before.cursor);
    CycleTicket ticket=r->native.cycle[GC_YOUNG].ticket;kg_unlock(r);
    for(unsigned i=0;i<64;i++){kg_lock(r);kgd_adopt(d);kg_unlock(r);kgd_step(d);}
    kg_lock(r);assert(d->state==12&&d->retirement.buffers.cursor==before.cursor&&d->retirement.buffers.remaining==before.remaining);kg_unlock(r);
    kgd_native(d,true);kg_driver_cleanup_leave(r,ownership);
    for(;;){kg_lock(r);uint64_t status=gc_cycle_step_locked(&r->native.cycles,&ticket,GC_CY_POLL);kg_unlock(r);
        if(status==GC_CY_OK)break;assert(status==GC_CY_WAIT);assert(++turns<2000000);kg_driver_step(r);kg_yield();}
    assert(!pthread_join(thread,NULL));kg_native_exit();assert(atomic_load_explicit(&peer.returned,memory_order_acquire));
    weak_gate_peer=NULL;assert(kg_weak_release(peer.handle));kg_shutdown();
    assert(!kg_weak_inventory&&!r->native.allocations&&!r->native.backing.heap.metadata_committed&&!r->native.backing.heap.metadata_pending);
    puts("C weak gate: real post-strong-end load waits without root publication, actual dead clear/move adoption returns empty and budgets0 PASS");
}
static _Atomic uint64_t weak_diag_serial,weak_diag_epoch[2],weak_diag_stamp[2],weak_diag_live[2];
static void weak_observe_trace(const void *raw,uint64_t gen,uint64_t epoch,uint64_t status){
    const GuardedMarkPrepare *access=raw;KgWeakInventory *w=kg_weak_inventory;
    if(!w||w->scan.work.handle.serial!=atomic_load_explicit(&weak_diag_serial,memory_order_acquire))return;
    if(status==GC_WEAK_LIVE||status==GC_WEAK_DEAD){MarkRecord *record;
        assert(gc_mark_prepare_record(access,&record));
        atomic_store_explicit(&weak_diag_stamp[gen],gen?record->old_epoch:record->young_epoch,memory_order_relaxed);
        atomic_store_explicit(&weak_diag_live[gen],status,memory_order_relaxed);
        atomic_store_explicit(&weak_diag_epoch[gen],epoch,memory_order_release);}
}
static void weak_epoch_trace(const char *label,RootCell *source,RootCell *out){
    KgRuntime *r=&kg_runtime;kg_lock(r);KgDriver *d=r->driver;uint64_t pending=0;
    for(KgRootBlock *b=kg_current_mutator(r)->root_blocks;b;b=b->next){BufferCursor *j=(void *)(uintptr_t)b->roots->segment.journal;
        pending+=j->top-atomic_load_explicit(&j->consumed,memory_order_acquire);}
    fprintf(stderr,"weak trace %s phase=%llx driver=%llu/%llu bg=%llu ledger Y%llu/%llu O%llu/%llu roots=%llx/%llx pending=%llu probe Yepoch%llu/stamp%llu/status%llu Oepoch%llu/stamp%llu/status%llu\n",label,
        (unsigned long long)r->native.roots.phase,(unsigned long long)d->generation,(unsigned long long)d->state,(unsigned long long)d->background_old,
        (unsigned long long)r->native.cycles.started[0],(unsigned long long)r->native.cycles.completed[0],
        (unsigned long long)r->native.cycles.started[1],(unsigned long long)r->native.cycles.completed[1],
        (unsigned long long)atomic_load_explicit(&source->value,memory_order_acquire),(unsigned long long)atomic_load_explicit(&out->value,memory_order_acquire),
        (unsigned long long)pending,(unsigned long long)atomic_load_explicit(&weak_diag_epoch[0],memory_order_acquire),
        (unsigned long long)atomic_load_explicit(&weak_diag_stamp[0],memory_order_relaxed),(unsigned long long)atomic_load_explicit(&weak_diag_live[0],memory_order_relaxed),
        (unsigned long long)atomic_load_explicit(&weak_diag_epoch[1],memory_order_acquire),(unsigned long long)atomic_load_explicit(&weak_diag_stamp[1],memory_order_relaxed),
        (unsigned long long)atomic_load_explicit(&weak_diag_live[1],memory_order_relaxed));kg_unlock(r);
}
static uint64_t weak_pending_root_transport_locked(KgRuntime *r){
    uint64_t pending=0;for(KgRootBlock *b=kg_current_mutator(r)->root_blocks;b;b=b->next){
        BufferCursor *j=(void *)(uintptr_t)b->roots->segment.journal;
        pending+=atomic_load_explicit(&j->published,memory_order_acquire)-atomic_load_explicit(&j->consumed,memory_order_acquire);}
    return pending;
}
/* This proves weak-only input for the NEXT requested cycle: merely clearing
 * cells is insufficient while real strong journal prefixes remain published. */
static void weak_drain_idle_transport(void){
    KgRuntime *r=&kg_runtime;kg_enter_native(r);uint64_t owned=kg_driver_cleanup_enter(r,NULL);assert(!owned);
    KgDriver *d=r->driver;kg_lock(r);assert(!d->state&&!d->background_old&&!(r->native.roots.phase&255));
    uint64_t pending=weak_pending_root_transport_locked(r);kg_unlock(r);
    while(pending){kg_drain_journals(r);kg_lock(r);uint64_t next=weak_pending_root_transport_locked(r);assert(next<pending);pending=next;kg_unlock(r);}
    kg_lock(r);NativeWorkStorage *work=gc_native_runtime_work(&r->native);
    assert(gc_mark_step_locked(&work->queues[0],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY&&gc_mark_step_locked(&work->queues[1],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
    assert(!r->native.mark_scan_count&&!kg_context(r)->private_producers);kg_unlock(r);
    kgd_native(d,true);kg_driver_cleanup_leave(r,owned);kg_leave_native(r);
}
static void weak_transport_counterexample(void){
    kg_configure(UINT64_C(16)*1024*1024,0,UINT64_C(8)*1024*1024,4);kg_init();
    KgRuntime *r=&kg_runtime;RootCell *source=kg_root_new(),*out=kg_root_new();kg_alloc_into(8,1,source);kg_store_i64(source,0,3030);
    KgWeakHandle handle=kg_weak_new(source);atomic_store_explicit(&weak_diag_serial,handle.serial,memory_order_release);
    kg_enter_native(r);uint64_t owned=kg_driver_cleanup_enter(r,NULL);assert(!owned);KgDriver *d=r->driver;
    kgd_native(d,true);kg_leave_native(r);
    for(unsigned i=0;i<32;i++)assert(kg_weak_load_into(handle,out)&&kg_load_i64(out,0)==3030);
    kg_root_clear(source);kg_root_clear(out);kg_enter_native(r);
    kg_lock(r);assert(!d->state&&!(r->native.roots.phase&255));assert(weak_pending_root_transport_locked(r)>16);
    assert(!atomic_load_explicit(&source->value,memory_order_acquire)&&!atomic_load_explicit(&out->value,memory_order_acquire));d->requested[0]=1;kg_unlock(r);
    uint64_t turns=0;while(d->state||d->requested[0]){assert(++turns<2000000);kg_lock(r);kgd_adopt(d);kg_unlock(r);kgd_step(d);}
    kg_lock(r);KgWeakInventory *w=kgw_owner(r);WeakHandle internal;assert(kgw_lookup(w,handle,&internal));
    assert(gc_weak_table_entry_locked(&w->table,internal)->slot.reference&&weak_pending_root_transport_locked(r)==0);kg_unlock(r);
    uint64_t epoch=atomic_load_explicit(&weak_diag_epoch[0],memory_order_acquire);
    assert(epoch&&atomic_load_explicit(&weak_diag_stamp[0],memory_order_relaxed)==epoch&&atomic_load_explicit(&weak_diag_live[0],memory_order_relaxed)==GC_WEAK_LIVE);
    /* No new public root publication: the next exact fresh epoch has no
     * unconsumed strong work and must clear this same weak-only object. */
    kg_lock(r);d->requested[0]=1;kg_unlock(r);
    while(d->state||d->requested[0]){assert(++turns<2000000);kg_lock(r);kgd_adopt(d);kg_unlock(r);kgd_step(d);}
    assert(atomic_load_explicit(&weak_diag_epoch[0],memory_order_acquire)>epoch&&atomic_load_explicit(&weak_diag_live[0],memory_order_relaxed)==GC_WEAK_DEAD);
    kgd_native(d,true);kg_driver_cleanup_leave(r,owned);kg_native_exit();
    assert(!kg_weak_load_into(handle,out)&&kg_weak_release(handle));kg_root_release(out);kg_root_release(source);kg_shutdown();
    assert(!kg_weak_inventory&&!r->native.allocations&&!r->native.backing.heap.metadata_committed&&!r->native.backing.heap.metadata_pending);
    puts("C weak transport: empty root cells with actual pending strong journals float once; fresh quiescent epoch clears without new roots PASS");
}
/* The worker really reads stop1, then pauses outside locks/guards until the
 * owner publishes terminal stop2. Its stale decision must not admit an actor. */
static void weak_driver_stop_checked(void *raw){
    if(!weak_shutdown_hold||atomic_load_explicit(&weak_stop_checked,memory_order_acquire))return;
    KgDriver *d=raw;KgRuntime *r=d->runtime;kg_lock(r);
    bool pause=d->stop==1&&!d->busy&&!d->state&&!d->background_old&&!r->native.allocations;
    if(pause)atomic_store_explicit(&weak_stop_checked,1,memory_order_release);kg_unlock(r);
    if(pause)while(!atomic_load_explicit(&weak_stop_release,memory_order_acquire))sched_yield();
}
static void weak_driver_before_terminal(void *raw){
    if(!weak_shutdown_hold)return;KgDriver *d=raw;uint64_t steps=0;
    while(!atomic_load_explicit(&weak_stop_checked,memory_order_acquire)){assert(++steps<2000000);kg_poll();sched_yield();}
    kg_lock(d->runtime);assert(!atomic_load_explicit(&d->actor.access_epoch,memory_order_acquire));kg_unlock(d->runtime);
}
static void weak_driver_terminal(void *raw){
    if(!weak_shutdown_hold)return;KgDriver *d=raw;KgRuntime *r=d->runtime;
    kg_lock(r);assert(d->stop==2&&!d->busy&&!d->state&&!d->background_old&&!d->cleanup_waiters&&!r->native.handshake.active);
    atomic_store_explicit(&weak_stop_phase,r->native.roots.phase,memory_order_release);kg_unlock(r);
    assert(!kgd_acquire(d)&&!kgd_acquire_for(d,true));
    atomic_store_explicit(&weak_stop_release,1,memory_order_release);
}
static void weak_driver_worker_stepped(void *raw){
    if(!weak_shutdown_hold||!atomic_load_explicit(&weak_stop_release,memory_order_acquire))return;
    KgDriver *d=raw;KgRuntime *r=d->runtime;kg_lock(r);
    assert(d->stop==2&&!d->busy&&!d->state&&!d->background_old&&!r->native.handshake.active&&
        r->native.roots.phase==atomic_load_explicit(&weak_stop_phase,memory_order_acquire));kg_unlock(r);
    atomic_store_explicit(&weak_stop_late_step,1,memory_order_release);
}
