/* Bounded mark scheduling. One lock/queue per generation protects record stamps
 * and all task transitions. Prepared stable native arrays, valid free/FIFO
 * lists, and unique logical MarkRecords are caller prerequisites. Never hold
 * a raw access guard while acquiring this lock. No allocation/scan/wait here.
 * OFFER transfers reference/metadata protection only on OK; DUPLICATE/FULL
 * retain it. Mark stamp and enqueue commit together. TAKE retains an in-flight
 * slot; YIELD returns it to the queue without needing a free slot. FINISH
 * returns protection to the request for release, after all children have been
 * protected/enqueued. EMPTY means only this queue is empty, never termination
 * of private producers, roots, buffers or the whole collector. */
#ifndef KLASSIC_GC_MARK_H
#define KLASSIC_GC_MARK_H
#include "gc_abi.h"
#define GC_MARK_OFFER 0u
#define GC_MARK_TAKE 1u
#define GC_MARK_FINISH 2u
#define GC_MARK_YIELD 3u
#define GC_MARK_CHECK 4u
#define GC_MARK_OK 0u
#define GC_MARK_INVALID 1u
#define GC_MARK_DUPLICATE 2u
#define GC_MARK_FULL 3u
#define GC_MARK_EMPTY 4u
#define GC_MARK_BUSY 5u
#define GC_MARK_EXHAUSTED 6u
static inline uint64_t gc_mark_step_locked(MarkQueue *g,MarkRequest *q,uint64_t op) {
    if(op>4 || !g->tasks || (g->tasks&7) || !g->capacity || g->capacity>UINT32_MAX ||
        g->tasks>UINT64_MAX-g->capacity*sizeof(MarkTask) || !g->epoch || g->generation>1 ||
        g->queued>g->capacity || g->in_flight>g->capacity-g->queued ||
        g->free_head>g->capacity || g->head>g->capacity || g->tail>g->capacity ||
        (!!g->free_head!=(g->queued+g->in_flight<g->capacity)) ||
        (!!g->head!=!!g->queued) || (!!g->tail!=!!g->queued) ||
        ((g->overflow_head|g->overflow_tail)&7) ||
        (!!g->overflow_head!=!!g->overflow_count) || (!!g->overflow_tail!=!!g->overflow_count) ||
        g->overflow_count>UINT64_MAX-g->queued-g->in_flight) return GC_MARK_INVALID;
    if(op==GC_MARK_CHECK)return g->queued||g->in_flight||g->overflow_count?GC_MARK_BUSY:GC_MARK_EMPTY;
    if(!q || ((uintptr_t)q&7))return GC_MARK_INVALID;
    MarkTask *tasks=(MarkTask *)(uintptr_t)g->tasks,*t,*tail=NULL;
    uint64_t index;
    // Before either append, validate the current tail without traversing it.
    if((op==GC_MARK_OFFER||op==GC_MARK_YIELD)&&g->tail) {
        tail=&tasks[g->tail-1];
        if(tail->state!=1 || tail->next)return GC_MARK_INVALID;
    }
    if(op==GC_MARK_OFFER) {
        if(q->index||q->serial||q->cursor||!q->record||(q->record&7)||
            !q->incarnation||!q->metadata_lease)return GC_MARK_INVALID;
        GcRef encoded;
        if(!gc_abi_encode_ref(q->reference>>16,(uint16_t)q->reference,&encoded))return GC_MARK_INVALID;
        MarkRecord *record=(MarkRecord *)(uintptr_t)q->record;
        uint64_t *stamp=g->generation?&record->old_epoch:&record->young_epoch;
        if(!record->object_id||*stamp>g->epoch)return GC_MARK_INVALID;
        if(*stamp==g->epoch)return GC_MARK_DUPLICATE;
        if(!g->free_head||g->overflow_count)return GC_MARK_FULL;
        if(g->last_serial==UINT64_MAX)return GC_MARK_EXHAUSTED;
        index=g->free_head;t=&tasks[index-1];
        if(t->state||t->serial>g->last_serial||t->record||t->reference||t->incarnation||t->metadata_lease||t->cursor||
            t->next>g->capacity||t->next==index||
            (!!t->next!=(g->queued+g->in_flight+1<g->capacity)))return GC_MARK_INVALID;
        g->free_head=t->next;t->next=0;t->state=1;t->serial=++g->last_serial;
        t->record=q->record;t->reference=q->reference;t->incarnation=q->incarnation;
        t->metadata_lease=q->metadata_lease;t->cursor=0;
        if(tail)tail->next=index;else g->head=index;
        g->tail=index;g->queued++;*stamp=g->epoch;*q=(MarkRequest){0};
        return GC_MARK_OK;
    }
    if(op==GC_MARK_TAKE) {
        if(q->record||q->reference||q->incarnation||q->metadata_lease||q->index||q->serial||q->cursor)return GC_MARK_INVALID;
        if(!g->head)return GC_MARK_EMPTY;
        index=g->head;t=&tasks[index-1];
        if(t->state!=1||!t->serial||t->serial>g->last_serial||!t->record||!t->reference||!t->incarnation||!t->metadata_lease||
            t->next>g->capacity||t->next==index|| (!!t->next!=(g->queued>1)) ||
            ((index==g->tail)!=(g->queued==1)))return GC_MARK_INVALID;
        g->head=t->next;if(!g->head)g->tail=0;g->queued--;g->in_flight++;t->next=0;t->state=2;
        *q=(MarkRequest){t->record,t->reference,t->incarnation,t->metadata_lease,index,t->serial,t->cursor};
        return GC_MARK_OK;
    }
    index=q->index;
    if(!index||index>g->capacity||!q->serial||!g->in_flight)return GC_MARK_INVALID;
    t=&tasks[index-1];
    if(t->state!=2||t->next||t->serial!=q->serial||q->serial>g->last_serial||
        t->record!=q->record||t->reference!=q->reference||t->incarnation!=q->incarnation||
        t->metadata_lease!=q->metadata_lease||q->cursor<t->cursor||(op==GC_MARK_YIELD&&(q->cursor&7)))return GC_MARK_INVALID;
    if(op==GC_MARK_YIELD) {
        // Invalidate copied claims on every re-claim, not only slot reuse.
        if(g->last_serial==UINT64_MAX)return GC_MARK_EXHAUSTED;
        t->cursor=q->cursor;t->state=1;t->serial=++g->last_serial;
        if(tail)tail->next=index;else g->head=index;
        g->tail=index;g->queued++;g->in_flight--;*q=(MarkRequest){0};
    } else {
        g->in_flight--;t->state=0;t->next=g->free_head;g->free_head=index;
        t->record=t->reference=t->incarnation=t->metadata_lease=t->cursor=0;
        q->index=q->serial=0; // completed protection returns to the caller
    }
    return GC_MARK_OK;
}
#endif
