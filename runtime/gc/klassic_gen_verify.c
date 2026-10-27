/* Debug verifier for the real rooted C heap. The single collector phase owner
 * excludes relocation/retirement while observing weakly-consistent mutator
 * updates; it never marks an object or grants mark/retirement credit. Native
 * metadata leases and per-object guards protect every managed read. */
#include "klassic_gen_policy.h"
#include "gc_pending_mark.h"
#include "gc_root_walk.h"

typedef struct {uint64_t roots,objects,fields,forwarding;} KgVerified;
/* Captured while ENTER already holds the heap lock; shape never reacquires a
 * lock with a live payload guard. Record arenas remain pinned by page imports. */
typedef struct {PendingMark mark;uint64_t records,used,valid_origin;} KgGuard;
static KgRun *kgv_run_locked(KgRuntime *r,const AllocationPage *page){
    for(NativeAllocationRun *n=(void *)(uintptr_t)r->native.allocations;n;n=(void *)(uintptr_t)n->next)
        if(n->lease.head_index==page->run_head&&n->lease.incarnation==page->incarnation)return (KgRun *)n;
    return NULL;
}
/* Validate native pointer provenance BEFORE any shared leaf dereferences an
 * object's starts/table/record metadata. Those pointers are not heap values. */
static bool kgv_metadata_locked(KgRuntime *r,KgRun *owner,AllocationPage *page){
    NativeAllocationRun *n=&owner->native;HeapPool *heap=&r->native.backing.heap;
    uint64_t stage=atomic_load_explicit(&n->stage,memory_order_acquire);
    if(stage<3||stage>8||!gc_native_allocation_valid(n)||n->runtime!=&r->native||
       page->run_head!=n->lease.head_index||page->incarnation!=n->lease.incarnation||
       page->heap_base!=heap->heap_base||page->start!=((n->lease.head_index+1)<<GC_HEAP_PAGE_SHIFT)||
       !n->lease.page_count||n->lease.page_count>heap->page_count-n->lease.head_index||
       page->limit!=((n->lease.head_index+n->lease.page_count+1)<<GC_HEAP_PAGE_SHIFT)||
       page->starts!=n->bitmap.mapping||n->bitmap.stage!=3||!page->starts||
       page->starts_words!=n->lease.page_count*(GC_HEAP_PAGE_BYTES>>7)/8||
       page->starts_words>n->bitmap.bytes/8||n->forwarding.stage!=2||
       n->forwarding.bytes<sizeof(ForwardTable)||!n->forwarding.allocation||page->forward_table!=n->forwarding.allocation)return false;
    ForwardTable *table=(void *)(uintptr_t)n->forwarding.allocation;
    if(!n->marks.capacity||n->marks.capacity>UINT32_MAX||
       table->capacity!=n->marks.capacity||table->cells!=(uintptr_t)(table+1)||
       table->capacity>(n->forwarding.bytes-sizeof(*table))/(sizeof(ForwardCell)+sizeof(uint64_t))||
       n->imports!=table->cells+table->capacity*sizeof(ForwardCell)||
       atomic_load_explicit(&table->used,memory_order_acquire)>table->capacity)return false;
    return true;
}
static uint64_t kgv_close(KgRuntime *r,PendingMark *p){
    if(p->access.scope.held){
        /* SourceAccess EXIT is one CAS and can legitimately lose to another
         * mutator's guard counter update. Retain the live guard and retry;
         * taking a lock, yielding or polling here would violate its scope. */
        uint64_t s;do{s=gc_pending_mark_cleanup(NULL,p,GC_RAC_EXIT);}while(s==GC_RESOLVE_WAIT);
        if(s!=GC_RF_DONE)return KG_VERIFY_INVALID;
    }
    kg_lock(r);uint64_t s=gc_pending_mark_cleanup(kg_leases(r),p,GC_RAC_RELEASE);kg_unlock(r);
    return s==GC_RF_DONE?KG_VERIFY_OK:KG_VERIFY_INVALID;
}
static uint64_t kgv_open(KgRuntime *r,MutatorContext *actor,uint64_t reference,KgGuard *guard){
    PendingMark *p=&guard->mark;
    *p=(PendingMark){.context=(uintptr_t)actor,.current={.reference=reference}};
    if(!reference)return KG_VERIFY_OK;
    /* Every valid forwarding edge names a distinct aligned object in this
     * finite reservation. Bound cycle diagnosis by that actual geometry,
     * without introducing a fixed valid-chain length limit. */
    uint64_t edges=0,limit=r->native.backing.heap.hard_max/16;
    for(;;){
        uint64_t value=p->current.reference,offset=value>>16;
        if(!gc_store_ref_valid(value)){(void)kgv_close(r,p);return KG_VERIFY_INVALID;}
        if(edges>limit){(void)kgv_close(r,p);return KG_VERIFY_INVALID;}
        kg_lock(r);HeapPool *heap=&r->native.backing.heap;
        uint64_t index=offset>>GC_HEAP_PAGE_SHIFT;
        if(!index||index>heap->page_count){kg_unlock(r);(void)kgv_close(r,p);return KG_VERIFY_INVALID;}
        AllocationPage *pages=(void *)(uintptr_t)heap->pages,*page=&pages[index-1];
        if(page->run_head>index-1){kg_unlock(r);(void)kgv_close(r,p);return KG_VERIFY_INVALID;}
        page=&pages[page->run_head];KgRun *run=kgv_run_locked(r,page);
        if(!run||!kgv_metadata_locked(r,run,page)){kg_unlock(r);(void)kgv_close(r,p);return KG_VERIFY_INVALID;}
        uint64_t s=gc_pending_mark_enter(kg_leases(r),p);
        if(s==GC_RAC_READY){
            ForwardTable *table=(void *)(uintptr_t)run->native.forwarding.allocation;
            uint64_t cell=(p->access.scope.held-table->cells)/sizeof(ForwardCell);
            guard->valid_origin=0;
            if(p->access.scope.held>=table->cells&&cell<atomic_load_explicit(&table->used,memory_order_acquire)){
                uint64_t imported=((uint64_t *)(uintptr_t)run->native.imports)[cell];
                NativeAllocationRun *origin=imported?(void *)(uintptr_t)imported:&run->native;
                if(gc_native_allocation_valid(origin)&&origin->runtime==&r->native&&origin->mark_records.stage==2&&
                   origin->marks.records&&origin->marks.records==origin->mark_records.allocation&&
                   origin->marks.used<=origin->marks.capacity){
                    guard->records=origin->marks.records;guard->used=origin->marks.used;guard->valid_origin=1;
                }
            }
        }
        kg_unlock(r);
        if(s==GC_RAC_READY)return KG_VERIFY_OK;
        if(s==GC_RF_ADVANCED){if(p->current.reference!=value)edges++;continue;}
        if(s==GC_RF_METADATA_FULL||s==GC_RF_WAIT){(void)kgv_close(r,p);return KG_VERIFY_WAIT;}
        (void)kgv_close(r,p);return KG_VERIFY_INVALID;
    }
}
static uint64_t kgv_shape(KgRuntime *r,KgGuard *guard,uint64_t *payload,uint64_t *descriptor){
    PendingMark *p=&guard->mark;
    MutatorContext *ctx;ObjectHeader *h;uint64_t state;
    uint64_t phase=atomic_load_explicit(&((MutatorContext *)(uintptr_t)p->context)->adopted_phase,memory_order_acquire);
    if(!gc_guarded_object_state((uintptr_t)&p->access.scope,p->current.lease.page,phase,true,&ctx,&h,&state))return KG_VERIFY_INVALID;
    if(!guard->valid_origin||h->mark_record<guard->records||
       h->mark_record-guard->records>=guard->used*sizeof(MarkRecord)||
       (h->mark_record-guard->records)%sizeof(MarkRecord))return KG_VERIFY_INVALID;
    MarkRecord *record=(void *)(uintptr_t)h->mark_record;if(!record->object_id)return KG_VERIFY_INVALID;
    *payload=h->payload_bytes;*descriptor=h->descriptor_id;
    DescriptorScan scan={h->descriptor_id,h->payload_bytes,0,1,0,0,0,0};uint64_t output;
    scan.output=(uintptr_t)&output;scan.output_capacity=1;
    if(gc_descriptor_scan(kg_descriptors(r),&scan)==GC_SCAN_INVALID)return KG_VERIFY_INVALID;
    return state==GC_OBJECT_INITIALIZING?KG_VERIFY_WAIT:KG_VERIFY_OK;
}
static uint64_t kgv_reference(KgRuntime *r,MutatorContext *actor,uint64_t value){
    if(!value)return KG_VERIFY_OK;KgGuard p={0};uint64_t s=kgv_open(r,actor,value,&p);
    if(s!=KG_VERIFY_OK)return s;uint64_t payload,descriptor;
    s=kgv_shape(r,&p,&payload,&descriptor);uint64_t clean=kgv_close(r,&p.mark);
    /* A rooted initializing allocation is valid. Its shape/record has already
     * been checked, and its zero-black publication is a separate producer. */
    return clean!=KG_VERIFY_OK?clean:s==KG_VERIFY_WAIT?KG_VERIFY_OK:s;
}
static void kgv_end_walk(KgRuntime *r,RootWalk *walk){
    while(walk->active){
        walk->cursor=walk->limit;walk->mask=walk->completed_mask=0;
        kg_lock(r);uint64_t s=gc_root_walk_locked(&r->native.roots,walk,GC_RW_NEXT);kg_unlock(r);
        if(s!=GC_RW_ITEM&&s!=GC_RW_DONE)kg_fail("verification root-reader release",s);
    }
}
uint64_t kg_verify_runtime(KgRuntime *r,MutatorContext *actor){
    if(!actor||actor->abi_version!=GC_ABI_VERSION||!actor->thread_id||
       atomic_load_explicit(&actor->state,memory_order_acquire)!=GC_RUNNING||
       atomic_load_explicit(&actor->access_epoch,memory_order_acquire))return KG_VERIFY_INVALID;
    kg_lock(r);uint64_t s=kg_policy_check_locked(r),phase=r->native.roots.phase;
    bool stopped=r->native.handshake.active!=0;
    if(!stopped)atomic_store_explicit(&actor->adopted_phase,phase,memory_order_release);kg_unlock(r);
    if(s!=KG_VERIFY_OK)return s;if(stopped)return KG_VERIFY_WAIT;
    KgVerified counts={0};RootWalk walk={0};kg_lock(r);s=gc_root_walk_locked(&r->native.roots,&walk,GC_RW_BEGIN);kg_unlock(r);
    while(s==GC_RW_ITEM){
        while(walk.cursor<walk.limit){
            uint64_t values[16];RootWalkScan scan={(uintptr_t)&walk,walk.lease.serial,(uintptr_t)values,16,0};
            uint64_t status=gc_root_walk_scan(&scan);
            if(status!=GC_ROOT_OK&&status!=2){kgv_end_walk(r,&walk);return KG_VERIFY_INVALID;}
            for(uint64_t i=0;i<scan.written;i++){
                status=kgv_reference(r,actor,values[i]);if(status){kgv_end_walk(r,&walk);return status;}counts.roots++;
            }
        }
        walk.mask=walk.completed_mask=0;kg_lock(r);s=gc_root_walk_locked(&r->native.roots,&walk,GC_RW_NEXT);kg_unlock(r);
    }
    if(s!=GC_RW_DONE)return KG_VERIFY_INVALID;
    kg_lock(r);NativeAllocationRun *native=(void *)(uintptr_t)r->native.allocations;kg_unlock(r);
    while(native){
        kg_lock(r);NativeAllocationRun *next=(void *)(uintptr_t)native->next;
        uint64_t stage=atomic_load_explicit(&native->stage,memory_order_acquire);
        if(stage<3||stage>8){kg_unlock(r);native=next;continue;}
        AllocationPage *page=gc_native_allocation_page(native);KgRun *run=(KgRun *)native;
        if(!kgv_metadata_locked(r,run,page)){kg_unlock(r);return KG_VERIFY_INVALID;}
        uint64_t state=atomic_load_explicit(&page->state,memory_order_acquire);
        ForwardTable *table=(void *)(uintptr_t)native->forwarding.allocation;
        uint64_t top=atomic_load_explicit(&page->allocated_top,memory_order_acquire);
        uint64_t used=atomic_load_explicit(&table->used,memory_order_acquire);kg_unlock(r);
        ForwardCell *cells=(void *)(uintptr_t)table->cells;uint64_t previous=0;
        for(uint64_t i=0;i<used;i++){
            ForwardCell *cell=&cells[i];uint64_t offset=cell->source_offset,word=atomic_load_explicit(&cell->word,memory_order_acquire);
            if(offset<=previous||offset<page->start+sizeof(ObjectHeader)||offset>=page->limit||offset>=top||
               (offset&15)||cell->incarnation!=page->incarnation)return KG_VERIFY_INVALID;
            previous=offset;counts.forwarding++;
            if(word&&word!=3){
                if((word&3)==1)return KG_VERIFY_WAIT;
                if((word&3)!=2||!gc_forward_destination(word&~UINT64_C(3),offset))return KG_VERIFY_INVALID;
                s=kgv_reference(r,actor,word&~UINT64_C(3));if(s)return s;continue;
            }
            if(state!=GC_PAGE_ALLOCATING&&state!=GC_PAGE_CLOSED)continue;
            if(word==3&&atomic_load_explicit(&cell->access,memory_order_acquire)==GC_SOURCE_CLOSED)continue;
            KgGuard base={0};uint16_t bad=gc_abi_color((PhaseSnapshot){phase})^GC_YOUNG_MARK_MASK^GC_OLD_MARK_MASK;
            s=kgv_open(r,actor,(offset<<16)|bad,&base);if(s)return s;
            uint64_t payload,descriptor;s=kgv_shape(r,&base,&payload,&descriptor);
            if(s==KG_VERIFY_WAIT){(void)kgv_close(r,&base.mark);continue;}
            if(s){(void)kgv_close(r,&base.mark);return s;}
            /* This exact object is guarded; capture one reference slot before
             * EXIT, then validate it under the same single phase-owner lease.
             * No poll or relocation/retirement can intervene in this observer. */
            uint64_t cursor=0;
            for(;;){
                uint64_t slot=0;DescriptorScan scan={descriptor,payload,cursor,1,(uintptr_t)&slot,1,0,0};
                uint64_t result=gc_descriptor_scan(kg_descriptors(r),&scan);
                if(result==GC_SCAN_INVALID){(void)kgv_close(r,&base.mark);return KG_VERIFY_INVALID;}
                uint64_t value=0;
                if(scan.written){
                    MutatorContext *ctx;_Atomic uint64_t *address;
                    if(!gc_guarded_slot((uintptr_t)&base.mark.access.scope,base.mark.current.lease.page,(uintptr_t)kg_descriptors(r),slot,phase,&ctx,&address)){
                        (void)kgv_close(r,&base.mark);return KG_VERIFY_INVALID;
                    }
                    value=atomic_load_explicit(address,memory_order_acquire);counts.fields++;
                }
                s=kgv_close(r,&base.mark);if(s)return s;
                if(scan.written){s=kgv_reference(r,actor,value);if(s)return s;}
                if(result==GC_SCAN_DONE)break;
                cursor=scan.next_offset;s=kgv_open(r,actor,(offset<<16)|bad,&base);if(s)return s;
            }
            counts.objects++;
        }
        native=next;
    }
    kg_policy_verified(r,counts.roots,counts.objects,counts.fields,counts.forwarding);return KG_VERIFY_OK;
}
