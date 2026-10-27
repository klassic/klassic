#include "klassic_gen_policy.h"
#include "gc_native_source_uncommit.h"
#include <stddef.h>

enum { KG_BACKED=1,KG_CACHED=2,KG_DROPPING=4,KG_OBSERVED=8,KG_SWEEPING=16 };
typedef struct {
    uint64_t self,pages,committed,dropping,floor,initial_cursor,closing;
    uint64_t verified_roots,verified_objects,verified_fields,verified_forwarding,verifications;
    uint64_t scan_cursor,observations,clock_resets,idle_uncommits;
    unsigned char states[];
} KgPolicy;
typedef struct { uint64_t at,incarnation; } KgFreeObservation;
static KgFreeObservation *kg_policy_times(KgPolicy *p){
    uintptr_t address=(uintptr_t)(p->states+p->pages);
    return (void *)((address+7)&~(uintptr_t)7);
}
static _Thread_local KgRuntime *kg_policy_owner;
static _Thread_local const MutatorContext *kg_policy_actor;
static KgPolicy *kg_policy(KgRuntime *r){return (KgPolicy *)r->policy;}
static bool kg_policy_native(const MutatorContext *actor){
    return actor&&actor->abi_version==GC_ABI_VERSION&&actor->thread_id&&
        atomic_load_explicit(&actor->state,memory_order_acquire)==GC_BLOCKED_NATIVE&&
        !atomic_load_explicit(&actor->access_epoch,memory_order_acquire);
}
static void *kg_policy_address(KgRuntime *r,uint64_t index){
    return (void *)(uintptr_t)(r->native.backing.heap.heap_base+((index+1)<<GC_HEAP_PAGE_SHIFT));
}
static bool kg_policy_protect(void *address,size_t bytes,bool readable){
#ifdef _WIN32
    DWORD previous;return VirtualProtect(address,bytes,readable?PAGE_READWRITE:PAGE_NOACCESS,&previous)!=0;
#else
    return !mprotect(address,bytes,readable?(PROT_READ|PROT_WRITE):PROT_NONE);
#endif
}
static bool kg_policy_commit_page(void *address){
#ifdef _WIN32
    if(VirtualAlloc(address,GC_HEAP_PAGE_BYTES,MEM_COMMIT,PAGE_READWRITE)!=address)return false;
#endif
    if(!kg_policy_protect(address,GC_HEAP_PAGE_BYTES,true))return false;
    /* Establish real zero backing now, rather than counting a merely reserved
     * PROT_NONE range as a minimum-heap commitment. */
    memset(address,0,GC_HEAP_PAGE_BYTES);return true;
}
uint64_t kg_policy_init(KgRuntime *r){
    if(!gc_native_runtime_valid(&r->native)||r->native.stage!=2||!kg_policy_native(&r->native.context))return GC_NATIVE_METADATA_INVALID;
    HeapPool *heap=&r->native.backing.heap;
    if(r->min_heap>r->hard_max||r->min_heap>UINT64_MAX-(GC_HEAP_PAGE_BYTES-1))return GC_NATIVE_METADATA_INVALID;
    if(!r->policy){
        uint64_t bytes=0;
        if(heap->page_count>(UINT64_MAX-sizeof(KgPolicy)-7)/(1+sizeof(KgFreeObservation)))return GC_NATIVE_METADATA_INVALID;
        uint64_t storage=((sizeof(KgPolicy)+heap->page_count+7)&~UINT64_C(7))+heap->page_count*sizeof(KgFreeObservation);
        if(gc_native_metadata_size(storage,r->os.metadata.quantum,&bytes))return GC_NATIVE_METADATA_INVALID;
        r->policy_charge.bytes=bytes;kg_lock(r);
        uint64_t status=gc_budget_step_locked(heap,&r->policy_charge,GC_BUDGET_RESERVE);kg_unlock(r);
        if(status)return status==GC_BUDGET_LIMIT?GC_NATIVE_RUNTIME_LIMIT:GC_NATIVE_METADATA_INVALID;
        kg_metadata_prepare_retry(r,&r->policy_charge);
        KgPolicy *p=(void *)(uintptr_t)r->policy_charge.allocation;
        p->self=(uintptr_t)p;p->pages=heap->page_count;
        p->floor=(r->min_heap+GC_HEAP_PAGE_BYTES-1)&~(GC_HEAP_PAGE_BYTES-1);
        kg_lock(r);status=gc_budget_step_locked(heap,&r->policy_charge,GC_BUDGET_COMMIT);kg_unlock(r);
        if(status)return GC_NATIVE_METADATA_INVALID;
        r->policy=p;
    }
    KgPolicy *p=kg_policy(r);
    while(p->initial_cursor<p->floor/GC_HEAP_PAGE_BYTES){
        uint64_t index=p->initial_cursor;void *address=kg_policy_address(r,index);
        if(!(p->states[index]&KG_BACKED)){
            if(!kg_policy_commit_page(address))return GC_NATIVE_METADATA_OS_ERROR;
            p->states[index]=KG_BACKED;p->committed+=GC_HEAP_PAGE_BYTES;
        }
        if(!kg_policy_protect(address,GC_HEAP_PAGE_BYTES,false))return GC_NATIVE_METADATA_OS_ERROR;
        p->states[index]|=KG_CACHED;p->initial_cursor++;
    }
    return GC_NATIVE_METADATA_OK;
}
void kg_policy_os_enter(KgRuntime *r,const MutatorContext *actor){
    if(!kg_policy_native(actor)||!r->policy||kg_policy_owner)kg_fail("backing policy native actor",0);
    kg_policy_owner=r;kg_policy_actor=actor;
}
void kg_policy_os_leave(KgRuntime *r){
    if(kg_policy_owner!=r||!kg_policy_native(kg_policy_actor))kg_fail("backing policy native handoff",0);
    kg_policy_actor=NULL;kg_policy_owner=NULL;
}
void kg_policy_before_prepare(KgRuntime *r,KgRun *run,const MutatorContext *actor){
    if(!gc_native_allocation_actor_native(&run->native,actor))kg_fail("backing preparation owner",0);
    kg_policy_os_enter(r,actor);
    KgPolicy *p=kg_policy(r);
    for(uint64_t i=run->native.lease.head_index;i<run->native.lease.head_index+run->native.lease.page_count;i++){
        /* The native page-pool lease may reserve a FREE address while an idle
         * sweep owns its backing. No prepare/zero/map can pass this handoff. */
        for(;;){kg_lock(r);bool sweeping=(p->states[i]&KG_SWEEPING)!=0;kg_unlock(r);if(!sweeping)break;kg_yield();}
        kg_lock(r);bool cached=(p->states[i]&KG_CACHED)!=0;
        p->states[i]&=(unsigned char)~KG_OBSERVED;kg_policy_times(p)[i]=(KgFreeObservation){0};kg_unlock(r);
        if(cached){uint64_t failures=0;
            while(!kg_policy_protect(kg_policy_address(r,i),GC_HEAP_PAGE_BYTES,true))kg_os_retry(&failures,"cached page activation OS retry limit");
            kg_lock(r);p->states[i]&=(unsigned char)~KG_CACHED;kg_unlock(r);
        }
    }
}
void kg_policy_after_prepare(KgRuntime *r,KgRun *run){
    if(run->native.lease.committed_mapping){
        KgPolicy *p=kg_policy(r);kg_lock(r);
        for(uint64_t i=run->native.lease.head_index;i<run->native.lease.head_index+run->native.lease.page_count;i++){
            if(!(p->states[i]&KG_BACKED))p->committed+=GC_HEAP_PAGE_BYTES;
            p->states[i]=KG_BACKED;
        }
        kg_unlock(r);
    }
    kg_policy_os_leave(r);
}
uint64_t kg_policy_commit(NativeHeapOwner *owner,PageLease *lease){
    KgRuntime *r=kg_policy_owner;
    if(!r||&r->native.backing!=owner||!kg_policy_native(kg_policy_actor))return GC_NATIVE_METADATA_INVALID;
    return gc_native_heap_commit_lease(owner,lease);
}
bool kg_policy_decommit(void *address,size_t bytes){
    KgRuntime *r=kg_policy_owner;if(!r||!kg_policy_native(kg_policy_actor))return false;
    KgPolicy *p=kg_policy(r);HeapPool *heap=&r->native.backing.heap;
    uint64_t start=(uintptr_t)address-heap->heap_base;
    if((uintptr_t)address<heap->heap_base||start<GC_HEAP_PAGE_BYTES||
       (start&(GC_HEAP_PAGE_BYTES-1))||!bytes||(bytes&(GC_HEAP_PAGE_BYTES-1))||
       bytes>heap->hard_max||start-GC_HEAP_PAGE_BYTES>heap->hard_max-bytes)return false;
    uint64_t first=(start>>GC_HEAP_PAGE_SHIFT)-1,count=bytes>>GC_HEAP_PAGE_SHIFT;
    for(uint64_t i=first;i<first+count;i++){
        kg_lock(r);unsigned char state=p->states[i];
        if(!(state&KG_BACKED)){kg_unlock(r);continue;}
        bool dropping=(state&KG_DROPPING)!=0;
        bool retain=!dropping&&!p->closing&&(r->uncommit_delay_ns||p->committed-p->dropping-GC_HEAP_PAGE_BYTES<p->floor);
        if(!retain&&!dropping){p->states[i]|=KG_DROPPING;p->dropping+=GC_HEAP_PAGE_BYTES;}
        kg_unlock(r);void *page=kg_policy_address(r,i);
        if(state&KG_SWEEPING)kg_fail("overlapping idle backing owner",i);
        if(retain){
            kg_lock(r);p->states[i]&=(unsigned char)~KG_OBSERVED;kg_policy_times(p)[i]=(KgFreeObservation){0};kg_unlock(r);
            if(!(state&KG_CACHED)){
                if(!kg_policy_protect(page,GC_HEAP_PAGE_BYTES,true))return false;
                /* The authenticated source/private rollback has excluded all
                 * payload actors. Discard old bytes before retaining backing. */
                memset(page,0,GC_HEAP_PAGE_BYTES);
                if(!kg_policy_protect(page,GC_HEAP_PAGE_BYTES,false))return false;
                kg_lock(r);p->states[i]=KG_BACKED|KG_CACHED;kg_unlock(r);
            }
        }else{
            if(!gc_native_heap_decommit_range(page,GC_HEAP_PAGE_BYTES))return false;
            kg_lock(r);p->states[i]=0;kg_policy_times(p)[i]=(KgFreeObservation){0};p->committed-=GC_HEAP_PAGE_BYTES;p->dropping-=GC_HEAP_PAGE_BYTES;kg_unlock(r);
        }
    }
    return true;
}
uint64_t kg_policy_source_uncommit(KgRuntime *r,SourceUncommit *q,const MutatorContext *actor){
    kg_policy_os_enter(r,actor);
    uint64_t status=gc_native_source_uncommit_payload(&r->native.backing.heap,q,r->payload_decommit?r->payload_decommit:kg_policy_decommit);
    kg_policy_os_leave(r);return status;
}
void kg_policy_begin_shutdown(KgRuntime *r){kg_lock(r);if(r->policy)kg_policy(r)->closing=1;kg_unlock(r);}
uint64_t kg_policy_committed_locked(KgRuntime *r){return r->policy?kg_policy(r)->committed:0;}
uint64_t kg_policy_verifications_locked(KgRuntime *r){return r->policy?kg_policy(r)->verifications:0;}
uint64_t kg_policy_check_locked(KgRuntime *r){
    KgPolicy *p=kg_policy(r);if(!p||p->self!=(uintptr_t)p||p->pages!=r->native.backing.heap.page_count)return KG_VERIFY_INVALID;
    uint64_t backed=0,dropping=0;
    for(uint64_t i=0;i<p->pages;i++){
        unsigned char state=p->states[i];if(state&~(KG_BACKED|KG_CACHED|KG_DROPPING|KG_OBSERVED|KG_SWEEPING))return KG_VERIFY_INVALID;
        if((state&(KG_CACHED|KG_DROPPING))&&!(state&KG_BACKED))return KG_VERIFY_INVALID;
        if((state&KG_CACHED)&&(state&KG_DROPPING))return KG_VERIFY_INVALID;
        if((state&KG_OBSERVED)&&(!(state&KG_CACHED)||!kg_policy_times(p)[i].at))return KG_VERIFY_INVALID;
        if((state&KG_SWEEPING)&&((state&(KG_BACKED|KG_DROPPING))!=(KG_BACKED|KG_DROPPING)))return KG_VERIFY_INVALID;
        backed+=!!(state&KG_BACKED)*GC_HEAP_PAGE_BYTES;dropping+=!!(state&KG_DROPPING)*GC_HEAP_PAGE_BYTES;
    }
    if(backed!=p->committed||dropping!=p->dropping||backed>r->hard_max||dropping>backed||
       (!p->closing&&backed-dropping<p->floor))return KG_VERIFY_INVALID;
    return KG_VERIFY_OK;
}
void kg_policy_verified(KgRuntime *r,uint64_t roots,uint64_t objects,uint64_t fields,uint64_t forwarding){
    kg_lock(r);KgPolicy *p=kg_policy(r);
    p->verified_roots+=roots;p->verified_objects+=objects;p->verified_fields+=fields;
    p->verified_forwarding+=forwarding;p->verifications++;
    uint64_t committed=p->committed,active=r->native.backing.heap.committed;kg_unlock(r);
    if(r->flags&2)fprintf(stderr,"klassic GC verify: roots=%llu objects=%llu refs=%llu forwarding=%llu committed=%llu active=%llu\n",
        (unsigned long long)roots,(unsigned long long)objects,(unsigned long long)fields,(unsigned long long)forwarding,
        (unsigned long long)committed,(unsigned long long)active);
}
uint64_t kg_policy_dispose_native(KgRuntime *r){
    if(!kg_policy_native(&r->native.context)||r->native.allocations||r->native.backing.heap.committed)return GC_NATIVE_METADATA_INVALID;
    KgPolicy *p=kg_policy(r);if(!p)return GC_NATIVE_METADATA_OK;
    kg_policy_begin_shutdown(r);kg_policy_os_enter(r,&r->native.context);
    for(uint64_t i=0;i<p->pages;i++)if(p->states[i]&KG_BACKED){
        if(!kg_policy_decommit(kg_policy_address(r,i),GC_HEAP_PAGE_BYTES)){kg_policy_os_leave(r);return GC_NATIVE_METADATA_OS_ERROR;}
    }
    kg_policy_os_leave(r);
    if(p->committed||p->dropping)return GC_NATIVE_METADATA_INVALID;
    uint64_t status=gc_native_metadata_dispose(&r->policy_charge,&r->os.metadata);
    if(status)return status;
    r->policy=NULL;kg_lock(r);status=gc_budget_step_locked(&r->native.backing.heap,&r->policy_charge,GC_BUDGET_RELEASE);kg_unlock(r);
    return status?GC_NATIVE_METADATA_INVALID:GC_NATIVE_METADATA_OK;
}

#include "klassic_gen_policy_delay.h"
