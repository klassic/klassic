/* Runtime-private receipt for a selected run with live copies and dead slots.
 * This deliberately does not broaden wholly-dead seal or SourceDrain. The
 * cycle owner captured each record under its real source guard. CLAIM validates
 * that record's arena/import and mark epoch under the common queue/heap lock;
 * COMMIT consumes the same protected slot only after actual forwarding or a
 * dead-only closed RETAIN. Each slot is consumed exactly once, in order.
 * SEAL rechecks the complete immutable table in bounded slices. No caller may
 * append into this selected run, introduce references to its dead objects, or
 * reuse its controls before full incoming/source/color retirement. */
#ifndef KLASSIC_GC_NATIVE_SELECTED_SOURCE_H
#define KLASSIC_GC_NATIVE_SELECTED_SOURCE_H
#include "gc_native_allocation.h"
#include "gc_relocation_claim.h"
#include "gc_source_drain.h"
typedef struct {
    uint64_t self,generation,cycle,epoch,gate_epoch,phase,incarnation,table,top,used;
    uint64_t cursor,seal_cursor,live_count,dead_count,active,record,ticket,live;
    NativeAllocationRun *run;
} NativeSelectedSource;
static inline bool gc_native_selected_source_valid(const NativeSelectedSource *q){
    if(!q||q->self!=(uintptr_t)q||!gc_native_allocation_valid(q->run)||q->run->stage!=8||q->generation>1)return false;
    NativeRuntimeOwner *r=q->run->runtime;AllocationPage *page=gc_native_allocation_page(q->run);
    uint64_t phase=r->roots.phase,mask=(UINT64_C(1)<<(8+q->generation))|(UINT64_C(1)<<(10+q->generation));
    if(((phase>>(4*q->generation))&15)!=GC_CONCURRENT_RELOCATE||((phase^q->phase)&mask)||
       r->cycle[q->generation].ticket.cycle!=q->cycle||gc_native_runtime_work(r)->queues[q->generation].epoch!=q->epoch||
       r->cycle[q->generation].gate.epoch!=q->gate_epoch||page->incarnation!=q->incarnation||
       page->owner||page->forward_table!=q->table||page->generation!=q->generation||
       atomic_load_explicit(&page->state,memory_order_acquire)!=GC_PAGE_CLOSED||
       atomic_load_explicit(&page->allocated_top,memory_order_acquire)!=q->top||q->cursor>q->used||
       q->seal_cursor>q->used||q->active>1)return false;
    ForwardTable *table=(void *)(uintptr_t)q->table;
    return table&&atomic_load_explicit(&table->used,memory_order_acquire)==q->used;
}
static inline uint64_t gc_native_selected_source_begin_locked(NativeAllocationRun *run,unsigned gen,NativeSelectedSource *q){
    if(!q||q->self||!gc_native_allocation_valid(run)||run->stage!=8||gen>1)return GC_SD_INVALID;
    NativeRuntimeOwner *r=run->runtime;AllocationPage *page=gc_native_allocation_page(run);
    if(page->generation!=gen||page->owner||atomic_load(&page->state)!=GC_PAGE_CLOSED||
       ((r->roots.phase>>(4*gen))&15)!=GC_CONCURRENT_RELOCATE||
       r->cycle[gen].marking.outcome!=GC_SELECT||r->cycle[gen].moving.state!=3||!page->forward_table)return GC_SD_INVALID;
    ForwardTable *table=(void *)(uintptr_t)page->forward_table;
    *q=(NativeSelectedSource){.self=(uintptr_t)q,.generation=gen,.cycle=r->cycle[gen].ticket.cycle,
        .epoch=gc_native_runtime_work(r)->queues[gen].epoch,.gate_epoch=r->cycle[gen].gate.epoch,
        .phase=r->roots.phase,.incarnation=page->incarnation,.table=page->forward_table,
        .top=atomic_load(&page->allocated_top),.used=atomic_load(&table->used),.run=run};
    return gc_native_selected_source_valid(q)?GC_SD_DONE:GC_SD_INVALID;
}
static inline uint64_t gc_native_selected_source_claim_locked(NativeSelectedSource *q,
        const MetadataLeaseRequest *borrow,uint64_t record_address,RelocationClaim *claim){
    if(!gc_native_selected_source_valid(q)||q->active||q->cursor>=q->used||!borrow||!record_address||
       !claim||claim->ticket||claim->epoch)return GC_RCL_INVALID;
    NativeAllocationRun *run=q->run;NativeRuntimeOwner *r=run->runtime;
    MetadataLeaseRequest checked={.token=borrow->token};
    if(gc_metadata_step_locked(&gc_native_runtime_work(r)->leases,&checked,GC_ML_RESOLVE)!=GC_ML_OK||
       checked.page!=borrow->page||checked.incarnation!=q->incarnation||checked.page!=(uintptr_t)gc_native_allocation_page(run))return GC_RCL_INVALID;
    ForwardTable *table=(void *)(uintptr_t)q->table;ForwardCell *cell=(ForwardCell *)(uintptr_t)table->cells+q->cursor;
    if(claim->source!=(uintptr_t)cell||claim->source_offset!=cell->source_offset||claim->incarnation!=q->incarnation)return GC_RCL_INVALID;
    NativeAllocationRun *origin=(void *)(uintptr_t)((uint64_t *)(uintptr_t)run->imports)[q->cursor];
    bool imported=origin!=NULL;if(!origin)origin=run;
    if(!gc_native_allocation_valid(origin)||origin->runtime!=r||origin->mark_records.stage!=2||
       !origin->marks.records||origin->marks.records!=origin->mark_records.allocation||
       !origin->marks.used||origin->marks.used>origin->marks.capacity||record_address<origin->marks.records||
       record_address-origin->marks.records>=origin->marks.used*sizeof(MarkRecord)||
       (record_address-origin->marks.records)%sizeof(MarkRecord)||(imported&&!origin->import_users)||
       (!imported&&record_address!=origin->marks.records+q->cursor*sizeof(MarkRecord)))return GC_RCL_INVALID;
    MarkRecord *record=(void *)(uintptr_t)record_address;
    if(!record->object_id)return GC_RCL_INVALID;
    uint64_t stamp=q->generation?record->old_epoch:record->young_epoch;if(stamp>q->epoch)return GC_RCL_STALE;
    uint64_t status=gc_relocation_claim_locked(&r->cycle[q->generation].gate,claim);
    if(status==GC_RCL_OWNED){q->active=1;q->record=record_address;q->ticket=claim->ticket;q->live=stamp==q->epoch;}
    return status;
}
static inline uint64_t gc_native_selected_source_commit_locked(NativeSelectedSource *q,const MetadataLeaseRequest *borrow,
        const RelocationClaim *claim){
    if(!gc_native_selected_source_valid(q)||!q->active||!borrow||!claim||claim->ticket!=q->ticket||claim->epoch!=q->gate_epoch)return GC_SD_INVALID;
    NativeRuntimeOwner *r=q->run->runtime;MetadataLeaseRequest checked={.token=borrow->token};
    if(gc_metadata_step_locked(&gc_native_runtime_work(r)->leases,&checked,GC_ML_RESOLVE)!=GC_ML_OK||
       checked.page!=(uintptr_t)gc_native_allocation_page(q->run)||checked.incarnation!=q->incarnation)return GC_SD_INVALID;
    ForwardTable *table=(void *)(uintptr_t)q->table;ForwardCell *cell=(ForwardCell *)(uintptr_t)table->cells+q->cursor;
    uint64_t word=atomic_load_explicit(&cell->word,memory_order_acquire);
    MarkRecord *record=(void *)(uintptr_t)q->record;uint64_t stamp=q->generation?record->old_epoch:record->young_epoch;
    if(claim->source!=(uintptr_t)cell||cell->incarnation!=q->incarnation||
       atomic_load_explicit(&cell->access,memory_order_acquire)!=GC_SOURCE_CLOSED||
       (q->live?stamp!=q->epoch||((word&3)!=2)||!gc_forward_destination(word&~UINT64_C(3),cell->source_offset):stamp==q->epoch||word!=3))return GC_SD_INVALID;
    if(q->live)q->live_count++;else q->dead_count++;
    q->cursor++;q->active=q->record=q->ticket=q->live=0;return GC_SD_DONE;
}
static inline uint64_t gc_native_selected_source_seal_locked(NativeSelectedSource *q,uint64_t budget){
    if(!gc_native_selected_source_valid(q)||q->active||q->cursor!=q->used||
       q->live_count+q->dead_count!=q->used||!budget||budget>256)return GC_SD_INVALID;
    ForwardTable *table=(void *)(uintptr_t)q->table;ForwardCell *cells=(void *)(uintptr_t)table->cells;
    uint64_t end=q->used-q->seal_cursor>budget?q->seal_cursor+budget:q->used;
    for(;q->seal_cursor<end;q->seal_cursor++){
        ForwardCell *cell=&cells[q->seal_cursor];uint64_t word=atomic_load_explicit(&cell->word,memory_order_acquire);
        if(cell->incarnation!=q->incarnation||atomic_load_explicit(&cell->access,memory_order_acquire)!=GC_SOURCE_CLOSED||
           (word!=3&&((word&3)!=2||!gc_forward_destination(word&~UINT64_C(3),cell->source_offset))))return GC_SD_WAIT;
    }
    if(q->seal_cursor<q->used)return GC_SD_MORE;
    AllocationPage *page=gc_native_allocation_page(q->run);if(page->metadata_leases)return GC_SD_WAIT;
    atomic_store_explicit(&page->state,GC_PAGE_EVACUATED,memory_order_release);return GC_SD_DONE;
}
#endif
