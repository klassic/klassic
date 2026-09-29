/* Bounded incoming-reference audit of the entire published weak table.
 * BEGIN/NEXT hold the common weak/heap lock outside access guards. The owning
 * cycle closes admission BEFORE BEGIN and retains table/storage/range lifetime,
 * completed liveness, and exclusion of new references into the target range
 * through consumption of DONE. This is the sole published table for this gate.
 * Storage replacement/growth and serial changes invalidate a captured scan.
 * Explicit release and forwarding AWAY from the target are permitted; no other
 * writer may introduce a target reference behind the cursor. Closing weak
 * admission alone does not freeze forwarding or establish these cycle proofs.
 *
 * NEXT examines at most one slot, without payload access or lease acquisition.
 * FOUND leaves its cursor in place and reports a serial-bound handle and bits.
 * The caller can follow/dispose that handle, then NEXT rechecks the same slot.
 * There is no operation that skips a finding. An unmoved retained referent
 * remains FOUND and prevents this range from being certified absent.
 * DONE covers weak SLOT references only; independent cleanup leases and all
 * roots/heap/jobs/buffers/incoming-forwarding/color proofs are still required.
 * The audit never reopens admission or authorizes source reuse on its own.
 */
#ifndef KLASSIC_GC_WEAK_INCOMING_H
#define KLASSIC_GC_WEAK_INCOMING_H
#include "gc_weak_table.h"
#include <string.h>
enum { GC_WI_DONE=0,GC_WI_MORE=1,GC_WI_FOUND=2,GC_WI_INVALID=3,GC_WI_RESTART=4 };
typedef uint64_t (*WeakIncomingMetadata)(MetadataLeasePool *,MetadataLeaseRequest *,uint64_t);
static inline uint64_t gc_weak_incoming_begin_locked(WeakTable *table,uint64_t start,uint64_t limit,WeakIncoming *q){
    WeakIncoming zero={0};
    if(!q||((uintptr_t)q&7)||memcmp(q,&zero,sizeof(zero))||!gc_weak_table_valid(table)||
        !start||start>=limit||limit>GC_MAX_OFFSET||((start|limit)&15))return GC_WI_INVALID;
    if(!table->gate->closed)return GC_WI_RESTART;
    *q=(WeakIncoming){.table=table,.entries=table->entries,.gate=table->gate,
        .pool=(void *)(uintptr_t)table->gate->pool,.capacity=table->capacity,.last_serial=table->last_serial,
        .target_start=start,.target_limit=limit,.stage=1};
    return GC_WI_MORE;
}
static inline uint64_t gc_weak_incoming_next_locked(WeakIncoming *q,WeakIncomingMetadata metadata){
    if(!q||((uintptr_t)q&7)||!metadata||(q->stage!=1&&q->stage!=2)||
        !gc_weak_table_valid(q->table)||q->cursor>q->capacity||
        !q->target_start||q->target_start>=q->target_limit||q->target_limit>GC_MAX_OFFSET||
        ((q->target_start|q->target_limit)&15))return GC_WI_INVALID;
    WeakTable *table=q->table;
    if(table->entries!=q->entries||table->gate!=q->gate||table->capacity!=q->capacity||
        table->last_serial!=q->last_serial||!table->gate->closed||
        table->gate->pool!=(uintptr_t)q->pool)return GC_WI_RESTART;
    if(q->stage==2)return q->cursor==q->capacity?GC_WI_DONE:GC_WI_INVALID;
    if(q->cursor==q->capacity){q->stage=2;q->handle=(WeakHandle){0};q->observed=0;return GC_WI_DONE;}
    WeakEntry *entry=&q->entries[q->cursor];WeakSlot *slot=&entry->slot;
    WeakHandle handle={(uintptr_t)table,q->cursor+1,slot->serial};
    if(!slot->serial){
        WeakSlot zero={0};
        if(memcmp(slot,&zero,sizeof(zero))||entry->next_free>q->capacity)return GC_WI_INVALID;
    }else{
        if(gc_weak_table_entry_locked(table,handle)!=entry)return GC_WI_INVALID;
        if(!slot->reference){
            if(slot->lease.page||slot->lease.incarnation||slot->lease.token)return GC_WI_INVALID;
        }else{
            MetadataLeaseRequest checked=slot->lease;GcRef encoded;
            uint64_t offset=slot->reference>>16;
            if(!gc_abi_encode_ref(offset,(uint16_t)slot->reference,&encoded)||
                metadata(q->pool,&checked,GC_ML_RESOLVE)!=GC_ML_OK||
                checked.page!=slot->lease.page||checked.incarnation!=slot->lease.incarnation)return GC_WI_INVALID;
            AllocationPage *page=(void *)(uintptr_t)checked.page;
            uint64_t top=atomic_load_explicit(&page->allocated_top,memory_order_acquire);
            if(page->start>=page->limit||page->limit>GC_MAX_OFFSET||top<page->start||top>page->limit||
                offset<page->start+sizeof(ObjectHeader)||offset>=top)return GC_WI_INVALID;
            if(offset>=q->target_start&&offset<q->target_limit){
                q->handle=handle;q->observed=slot->reference;return GC_WI_FOUND;
            }
        }
    }
    q->cursor++;q->handle=(WeakHandle){0};q->observed=0;return GC_WI_MORE;
}
/* Fixed entry: BEGIN(0) accepts only target_start/target_limit initialized;
 * NEXT(1) uses the captured table. Errors preserve the original request. */
typedef uint64_t (*WeakIncomingStepper)(WeakTable *,WeakIncoming *,uint64_t);
static inline uint64_t gc_weak_incoming_step_locked(WeakTable *table,WeakIncoming *q,uint64_t op){
    if(op>1||!q||((uintptr_t)q&7))return GC_WI_INVALID;
    if(op){
        if(q->table!=table)return GC_WI_INVALID;
        return gc_weak_incoming_next_locked(q,gc_metadata_step_locked);
    }
    WeakIncoming fresh=*q,zero={0};fresh.target_start=fresh.target_limit=0;
    if(memcmp(&fresh,&zero,sizeof(zero)))return GC_WI_INVALID;
    uint64_t result=gc_weak_incoming_begin_locked(table,q->target_start,q->target_limit,&fresh);
    if(result==GC_WI_MORE)*q=fresh;
    return result;
}
#endif
