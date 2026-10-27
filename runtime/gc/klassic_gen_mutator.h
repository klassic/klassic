/* Runtime implementation include: unique OS-owner attach/detach and root import.
 * Included once by klassic_gen_runtime.c after its private rooted services. */
#ifndef KLASSIC_GEN_MUTATOR_H
#define KLASSIC_GEN_MUTATOR_H
static void *kg_unattached_map(KgRuntime *r,uint64_t bytes,MetadataCharge *charge){
    if(kg_current_mutator(r))kg_fail("unattached metadata ownership",0);
    if(gc_native_metadata_size(bytes,r->os.metadata.quantum,&charge->bytes))kg_fail("mutator control size",bytes);
    kg_lock(r);uint64_t s=gc_budget_step_locked(&r->native.backing.heap,charge,GC_BUDGET_RESERVE);kg_unlock(r);
    if(s)kg_fail("mutator metadata admission",s);
    kg_metadata_prepare_retry(r,charge);
    kg_lock(r);s=gc_budget_step_locked(&r->native.backing.heap,charge,GC_BUDGET_COMMIT);kg_unlock(r);
    if(s)kg_fail("mutator metadata commit",s);return (void *)(uintptr_t)charge->allocation;
}
bool kg_try_thread_attach(void){
    KgRuntime *r=&kg_runtime;if(kg_current_mutator(r)){kg_require_owner(r);return true;}
    if(!kg_admission_acquire())return false;
#ifdef KG_ATTACH_PRELOCK_HOOK
    KG_ATTACH_PRELOCK_HOOK(r);
#endif
    kg_lock(r);
    if(r->closing||r->admission_closed){kg_unlock(r);kg_admission_release();return false;}
    if(r->attaching==UINT64_MAX)kg_fail("mutator attach count overflow",0);
    r->attaching++;kg_unlock(r);kg_admission_release();
    MetadataCharge control={0};KgMutator *m=kg_unattached_map(r,sizeof(*m),&control);
    m->control=control;m->runtime=r;m->context=&m->local_context;m->participant=&m->local_participant;m->worker=&m->local_worker;
#ifdef _WIN32
    m->owner=GetCurrentThreadId();
#else
    m->owner=pthread_self();
#endif
    BarrierEntry *rings=kg_unattached_map(r,2*r->journal_capacity*sizeof(BarrierEntry),&m->rings);
    m->context->abi_version=GC_ABI_VERSION;
    atomic_init(&m->context->state,GC_RUNNING);atomic_init(&m->context->adopted_phase,0);
    atomic_init(&m->context->access_epoch,0);atomic_init(&m->context->root_revision,0);atomic_init(&m->context->private_producers,0);
    atomic_init(&m->context->satb.published,0);atomic_init(&m->context->satb.consumed,0);
    atomic_init(&m->context->remembered.published,0);atomic_init(&m->context->remembered.consumed,0);
    m->context->satb.entries=(uintptr_t)rings;m->context->remembered.entries=(uintptr_t)(rings+r->journal_capacity);
    m->context->satb.capacity=m->context->remembered.capacity=r->journal_capacity;
    m->participant->context=(uintptr_t)m->context;
    *m->worker=(WorkerRequest){(uintptr_t)&r->native.handshake,(uintptr_t)m->participant};
    /* Ring backing is complete before REGISTER makes the context observable.
     * Identity-bound roots can be initialized afterwards while really Native. */
    for(;;){
        kg_lock(r);uint64_t s=gc_worker_step_locked(&r->native.roots,m->worker,GC_WK_ATTACH);
        if(s==GC_WK_OK){
            m->state=KG_MUTATOR_REGISTERED;m->next=r->mutators;if(m->next)m->next->previous=m;
            if(r->mutator_count==UINT64_MAX||r->mutator_generation==UINT64_MAX)kg_fail("mutator inventory overflow",0);
            r->mutators=m;r->mutator_count++;r->mutator_generation++;kg_mutator=m;kg_unlock(r);break;
        }
        kg_unlock(r);if(s!=GC_WK_RETRY)kg_fail("mutator registration",s);kg_driver_step(r);kg_yield();
    }
    (void)kg_grow_roots(r);m->store_observed=kg_root_new();
    kg_lock(r);if(!r->attaching)kg_fail("mutator attach receipt",0);r->attaching--;kg_notify(r);kg_unlock(r);return true;
}
void kg_thread_attach(void){if(!kg_try_thread_attach())kg_fail("mutator admission closed",0);}
static void kg_dispose_owner_roots_native(KgRuntime *r,KgMutator *m){
    while(m->root_blocks){
        KgRootBlock *b=m->root_blocks;KgRootBlock *next=b->next;
        if(b->initial)kg_fail("bootstrap roots require global shutdown",0);
        /* Lookup controls disappear under the common lock before OS disposal.
         * FINISH already excluded every real RootLease and journal reader. */
        kg_lock(r);m->root_blocks=next;kg_unlock(r);uint64_t s,failures=0;
        while((s=gc_native_roots_dispose(&b->storage,&r->os.metadata,gc_root_storage_can_dispose))==GC_NATIVE_METADATA_OS_ERROR)
            kg_os_retry(&failures,"detached root backing OS retry limit");
        if(s)kg_fail("detached root backing",s);
        kg_lock(r);s=gc_budget_step_locked(&r->native.backing.heap,&b->storage,GC_BUDGET_RELEASE);kg_unlock(r);
        if(s)kg_fail("detached root backing budget",s);
        KgMetadataRetire job={0};
        do{s=kg_metadata_retire_step_native(r,job.charge.stage?NULL:&b->control,&job,m->context);}while(s==GC_NATIVE_METADATA_OS_ERROR);
        if(s)kg_fail("detached root control",s);
    }
}
void kg_thread_detach(void){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);KgMutator *m=kg_current_mutator(r);
    if(m->bootstrap)kg_fail("bootstrap owner uses shutdown",0);
    if(kg_managed_callback_active())kg_fail("mutator detach inside managed callback",0);
    kg_lock(r);kg_pin_owner_can_detach_locked(r,m->context);kg_unlock(r);
    kg_lock(r);if(r->detaching==UINT64_MAX)kg_fail("mutator detach count overflow",0);r->detaching++;kg_unlock(r);
    kg_root_release(m->store_observed);m->store_observed=NULL;
    for(KgRootBlock *b=m->root_blocks;b;b=b->next)if(b->live)kg_fail("live roots at mutator detach",b->live);
    for(KgRootBlock *b=m->root_blocks;b;b=b->next)kg_retire_roots(r,b);
    kg_enter_native(r);uint64_t ownership;
    for(;;){
        kg_lock(r);MutatorContext *ctx=m->context;
        bool clean=!m->pins&&!ctx->satb_consumer.serial&&!ctx->remembered_consumer.serial&&
            atomic_load_explicit(&ctx->satb.published,memory_order_acquire)==atomic_load_explicit(&ctx->satb.consumed,memory_order_acquire)&&
            atomic_load_explicit(&ctx->remembered.published,memory_order_acquire)==atomic_load_explicit(&ctx->remembered.consumed,memory_order_acquire);
        kg_unlock(r);if(!clean){kg_driver_step(r);kg_yield();continue;}
        ownership=kg_driver_cleanup_enter(r,NULL);kg_lock(r);clean=!m->pins;kg_unlock(r);
        if(clean)break;kg_driver_cleanup_leave(r,ownership);kg_yield();
    }
    kg_dispose_owner_roots_native(r,m);kg_lock(r);
    if(m->pins||r->native.handshake.active||!m->participant->registered)kg_fail("mutator detach still borrowed",0);
    uint64_t s=gc_worker_step_locked(&r->native.roots,m->worker,GC_WK_LEAVE_NATIVE);
    if(s)kg_fail("mutator detach native leave",s);
    atomic_store_explicit(&m->context->state,GC_DETACHING,memory_order_release);m->state=KG_MUTATOR_DETACHING;
    DetachRequest detach={(uintptr_t)&r->native.roots,(uintptr_t)&r->native.handshake,(uintptr_t)m->participant};
    s=gc_detach_step_locked(&detach);if(s)kg_fail("mutator detach completion",s);
    if(m->previous)m->previous->next=m->next;else r->mutators=m->next;
    if(m->next)m->next->previous=m->previous;
    if(!r->mutator_count||r->mutator_generation==UINT64_MAX)kg_fail("mutator inventory completion",0);
    m->state=KG_MUTATOR_DETACHED;r->mutator_count--;r->mutator_generation++;kg_mutator=NULL;kg_notify(r);kg_unlock(r);
    /* The child is now unregistered; the exclusively owned collector Native
     * actor outlives both mappings and supplies actual OS-native ownership. */
    const MutatorContext *actor=kg_driver_cleanup_actor(r);KgMetadataRetire job={0};
    do{s=kg_metadata_retire_step_native(r,job.charge.stage?NULL:&m->rings,&job,actor);}while(s==GC_NATIVE_METADATA_OS_ERROR);
    if(s)kg_fail("mutator ring retirement",s);job=(KgMetadataRetire){0};
    do{s=kg_metadata_retire_step_native(r,job.charge.stage?NULL:&m->control,&job,actor);}while(s==GC_NATIVE_METADATA_OS_ERROR);
    if(s)kg_fail("mutator control retirement",s);kg_driver_cleanup_leave(r,ownership);
    kg_lock(r);if(!r->detaching)kg_fail("mutator detach receipt",0);r->detaching--;kg_notify(r);kg_unlock(r);
}
void kg_root_import(RootCell *out,const RootCell *source){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);if(out==source){kg_root_copy(out,source);return;}
    for(;;){
        kg_lock_current(r);RootRequest output=kg_root_request_locked(r,out);KgRootBlock *found=NULL;uint64_t address=(uintptr_t)source;
        for(KgMutator *m=r->mutators;m&&!found;m=m->next)for(KgRootBlock *b=m->root_blocks;b;b=b->next){
            RootSegment *segment=&b->roots->segment;
            if(address>=segment->cells&&address-segment->cells<segment->capacity*sizeof(RootCell)&&!((address-segment->cells)%sizeof(RootCell))){found=b;break;}
        }
        if(!found||!source->active||atomic_load_explicit(&found->roots->segment.state,memory_order_acquire)!=GC_ROOT_SEGMENT_ACTIVE)
            kg_fail("foreign root import lifetime",address);
        RootLease lease={0};RootLifeRequest reader={.registration=(uintptr_t)&found->roots->registration,
            .scope=found->roots->segment.scope,.lease=(uintptr_t)&lease};
        uint64_t s=gc_root_life_locked(&r->native.roots,&reader,GC_RL_ACQUIRE);if(s)kg_fail("foreign root reader admission",s);
        output.value=atomic_load_explicit(&source->value,memory_order_acquire);
        if(!gc_store_ref_valid(output.value))kg_fail("foreign root reference encoding",0);
        s=gc_root_step(kg_context(r),&output,GC_ROOT_STORE);output.value=0;
        reader.serial=lease.serial;uint64_t released=gc_root_life_locked(&r->native.roots,&reader,GC_RL_RELEASE);
        kg_unlock(r);if(released)kg_fail("foreign root reader release",released);
        /* The captured value has an actual owned RootCell and published
         * journal before its foreign reader lease is released. No raw key is
         * kept on the stack across a poll. Ordinary root follow canonicalizes. */
        if(s==GC_ROOT_OK){kg_root_copy(out,out);return;}
        if(s!=GC_ROOT_FLUSH)kg_fail("foreign root owned publication",s);kg_poll();
    }
}
#endif
