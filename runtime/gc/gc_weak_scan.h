/* Bounded scan of the runtime's entire published weak table. There is ONE
 * table for this admission gate; publication/growth and every table alias are
 * excluded until FINISH or atomic relocation handoff. This is a caller-owned runtime inventory, not a proof
 * that arbitrary unregistered native tables do not exist. Entries/table/gate
 * live for the runtime lifetime. All calls hold the common weak/heap lock,
 * outside guards; the per-entry OBSERVE/EXIT execute separately outside locks.
 *
 * NEXT examines at most one slot or consumes one completed entry. A captured
 * item is driven with weak_process, then NEXT advances after its cleanup.
 * Explicit release is allowed while admission is closed; creation is not.
 * Cancellation of an extant item is retried, not counted as processed. FINISH
 * requires coverage and zero pending processor/leases before reopening. ABORT
 * cleans private work but keeps admission closed for a fresh full rescan.
 * Scheduler reclamation exclusion remains required through FINISH or transfer
 * to the relocation owner via gc_weak_scan_relocate_begin_locked.
 * States: 0 fresh,1 scanning,2 covered,3 finished,4 aborted,5 handed to relocation.
 */
#ifndef KLASSIC_GC_WEAK_SCAN_H
#define KLASSIC_GC_WEAK_SCAN_H
#include "gc_weak_process.h"
#define GC_WS_DONE 11u
#define GC_WS_ABORTED 12u

static inline bool gc_weak_scan_valid(const WeakScan *q){
    return q&&!((uintptr_t)q&7)&&gc_weak_table_valid(q->table)&&gc_weak_process_context(q->context)&&
        !atomic_load_explicit(&q->context->access_epoch,memory_order_acquire)&&
        gc_weak_mark_owner_valid(q->owner,q->phase)&&q->owner->scan==(uintptr_t)&q->work&&
        q->table->gate==q->owner->gate&&q->table->entries==q->entries&&q->table->capacity==q->capacity&&
        q->table->last_serial==q->last_serial&&q->cursor<=q->capacity&&
        q->owner->mark->epoch==q->epoch&&q->owner->mark->generation==q->generation&&
        q->owner->mark->rendezvous==q->rendezvous&&atomic_load_explicit(&q->context->adopted_phase,memory_order_acquire)==q->phase;
}
static inline uint64_t gc_weak_scan_begin_locked(WeakTable *table,WeakMarkControl *owner,
        MutatorContext *ctx,WeakScan *q){
    WeakScan zero={0};
    if(!q||((uintptr_t)q&7)||memcmp(q,&zero,sizeof(zero))||!gc_weak_table_valid(table)||
        !gc_weak_process_context(ctx)||atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)||
        !gc_weak_mark_owner_valid(owner,atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire))||
        table->gate!=owner->gate)return GC_WP_INVALID;
    if(owner->processor||owner->scan)return GC_WP_WAIT;
    *q=(WeakScan){.table=table,.owner=owner,.context=ctx,.entries=table->entries,
        .capacity=table->capacity,.last_serial=table->last_serial,.phase=owner->mark->phase,
        .epoch=owner->mark->epoch,.generation=owner->mark->generation,.rendezvous=owner->mark->rendezvous,.stage=1};
    owner->scan=(uintptr_t)&q->work;return GC_WP_OK;
}
static inline uint64_t gc_weak_scan_next_locked(WeakScan *q,const WeakProcessOps *ops){
    if(!gc_weak_scan_valid(q)||q->stage!=1||!gc_weak_process_ops(ops))return GC_WP_INVALID;
    if(q->work.stage){
        if(q->work.stage!=6||q->owner->processor)return GC_WP_WAIT;
        if(q->work.table!=q->table||q->work.owner!=q->owner||q->work.handle.index!=q->cursor+1||
            q->work.current.lease.token||q->work.retired.token)return GC_WP_INVALID;
        bool advance=true;
        if(q->work.result==GC_WP_STALE){
            WeakEntry *e=gc_weak_table_entry_locked(q->table,q->work.handle);
            if(e&&e->slot.reference)advance=false;
            if(e&&!e->slot.reference&&(e->slot.lease.page||e->slot.lease.incarnation||e->slot.lease.token))return GC_WP_INVALID;
        }else if(q->work.result!=GC_WP_LIVE&&q->work.result!=GC_WP_DEAD)return GC_WP_INVALID;
        q->work=(WeakProcess){0};if(advance)q->cursor++;
        return GC_WP_ADVANCED;
    }
    if(q->owner->processor)return GC_WP_INVALID;
    if(q->cursor==q->capacity){q->stage=2;return GC_WS_DONE;}
    WeakEntry *e=&q->entries[q->cursor];
    if(!e->slot.serial){
        if(e->slot.gate||e->slot.reference||e->slot.lease.page||e->slot.lease.incarnation||e->slot.lease.token||
            e->next_free>q->capacity)return GC_WP_INVALID;
        q->cursor++;return GC_WP_ADVANCED;
    }
    WeakHandle handle={(uintptr_t)q->table,q->cursor+1,e->slot.serial};
    uint64_t status=gc_weak_process_begin_locked(q->table,q->owner,q->context,handle,&q->work,ops);
    if(status==GC_WP_EMPTY){q->cursor++;return GC_WP_ADVANCED;}
    return status;
}
static inline uint64_t gc_weak_scan_finish_locked(WeakScan *q){
    WeakProcess zero={0};
    if(!gc_weak_scan_valid(q)||q->stage!=2||q->cursor!=q->capacity||q->owner->processor||
        memcmp(&q->work,&zero,sizeof(zero)))return GC_WP_INVALID;
    q->owner->scan=0;q->owner->stage=0;q->owner->gate->closed=0;q->stage=3;return GC_WS_DONE;
}
static inline uint64_t gc_weak_scan_abort_locked(WeakScan *q,const WeakProcessOps *ops){
    if(!q||((uintptr_t)q&7)||(q->stage!=1&&q->stage!=2)||!q->owner||
        q->owner->scan!=(uintptr_t)&q->work||!gc_weak_process_ops(ops)||!gc_weak_process_context(q->context)||
        atomic_load_explicit(&q->context->access_epoch,memory_order_acquire))return GC_WP_INVALID;
    if(q->work.stage&&q->work.stage!=6){
        uint64_t result=gc_weak_process_cancel_locked(&q->work,ops);
        if(q->work.stage!=6)return result;
    }
    if(q->owner->processor)return GC_WP_INVALID;
    q->owner->scan=0;q->stage=4;return GC_WS_ABORTED;
}
/* Fixed entry: start is borrowed for BEGIN and ignored by other operations. */
enum { GC_WS_BEGIN=0,GC_WS_NEXT=1,GC_WS_FINISH=2,GC_WS_ABORT=3 };
typedef uint64_t (*WeakScanStepper)(WeakScan *,const WeakScanStart *,uint64_t);
static inline uint64_t gc_weak_scan_step_locked(WeakScan *q,const WeakScanStart *start,uint64_t op){
    if(op>GC_WS_ABORT)return GC_WP_INVALID;
    if(op==GC_WS_BEGIN){
        if(!start||((uintptr_t)start&7))return GC_WP_INVALID;
        return gc_weak_scan_begin_locked(start->table,start->owner,start->context,q);
    }
    const WeakProcessOps ops={gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,
        gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked};
    if(op==GC_WS_NEXT)return gc_weak_scan_next_locked(q,&ops);
    if(op==GC_WS_FINISH)return gc_weak_scan_finish_locked(q);
    return gc_weak_scan_abort_locked(q,&ops);
}
#endif
