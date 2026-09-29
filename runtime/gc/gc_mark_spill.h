/* Progress under bounded task-slot saturation, under the SAME generation lock
 * as gc_mark_step_locked. Each logical MarkRecord has two prepared, zeroed
 * overflow cells which outlive all jobs. No allocation, traversal or waits.
 * Admission/budgeting for these cells belongs before object publication.
 * Entry references and page leases transfer exactly as for ordinary OFFER.
 * Overflow work stays counted, including when every ordinary slot is claimed.
 * REFILL moves at most one deferred job to a free ordinary slot. Call it before
 * TAKE; FULL with claimed jobs does not mean the holders should wait for space.
 * They may record children here, advance their finite scan and yield/finish.
 * Metadata-lease exhaustion is a separate admission/progress obligation. */
#ifndef KLASSIC_GC_MARK_SPILL_H
#define KLASSIC_GC_MARK_SPILL_H
#include "gc_mark.h"
#define GC_MARK_OFFER_OR_SPILL 0u
#define GC_MARK_REFILL 1u
static inline MarkTask *gc_mark_spill_cell(MarkQueue *g,uint64_t address) {
    MarkRecord *r=(MarkRecord *)(uintptr_t)address;
    return g->generation?&r->old_spill:&r->young_spill;
}
static inline uint64_t gc_mark_spill_locked(MarkQueue *g,MarkRequest *q,uint64_t op) {
    if(op>1)return GC_MARK_INVALID;
    uint64_t result=gc_mark_step_locked(g,q,op==0?GC_MARK_OFFER:GC_MARK_CHECK);
    if(op==0) {
        if(result!=GC_MARK_FULL)return result; // failures/duplicate preserve input
        if(g->last_serial==UINT64_MAX || g->overflow_count==UINT64_MAX-g->queued-g->in_flight)return GC_MARK_EXHAUSTED;
        MarkTask *s=gc_mark_spill_cell(g,q->record),*tail=NULL;
        if(s->next||s->serial||s->state||s->record||s->reference||s->incarnation||s->metadata_lease||s->cursor)return GC_MARK_INVALID;
        if(g->overflow_tail){tail=gc_mark_spill_cell(g,g->overflow_tail);
            if(tail->state!=1||tail->next||tail->record!=g->overflow_tail)return GC_MARK_INVALID;}
        s->serial=++g->last_serial;s->state=1;s->record=q->record;s->reference=q->reference;
        s->incarnation=q->incarnation;s->metadata_lease=q->metadata_lease;
        if(tail)tail->next=q->record;else g->overflow_head=q->record;
        g->overflow_tail=q->record;g->overflow_count++;
        MarkRecord *r=(MarkRecord *)(uintptr_t)q->record;
        if(g->generation)r->old_epoch=g->epoch;else r->young_epoch=g->epoch;
        *q=(MarkRequest){0};return GC_MARK_OK;
    }
    if(result==GC_MARK_INVALID)return result;
    if(!g->overflow_count)return GC_MARK_EMPTY;
    if(!g->free_head)return GC_MARK_FULL;
    MarkTask *tasks=(MarkTask *)(uintptr_t)g->tasks,*t=&tasks[g->free_head-1],*tail=NULL;
    uint64_t index=g->free_head;
    if(t->state||t->serial>g->last_serial||t->record||t->reference||t->incarnation||t->metadata_lease||t->cursor||
        t->next>g->capacity||t->next==index|| (!!t->next!=(g->queued+g->in_flight+1<g->capacity)))return GC_MARK_INVALID;
    if(g->tail){tail=&tasks[g->tail-1];if(tail->state!=1||tail->next)return GC_MARK_INVALID;}
    MarkTask *s=gc_mark_spill_cell(g,g->overflow_head);
    MarkRecord *r=(MarkRecord *)(uintptr_t)g->overflow_head;
    if(s->state!=1||!s->serial||s->serial>g->last_serial||s->record!=g->overflow_head||
        !s->reference||!s->incarnation||!s->metadata_lease||s->cursor||
        (s->next&7)||s->next==g->overflow_head|| (!!s->next!=(g->overflow_count>1))||
        ((g->overflow_head==g->overflow_tail)!=(g->overflow_count==1))||
        (g->generation?r->old_epoch:r->young_epoch)!=g->epoch)return GC_MARK_INVALID;
    g->free_head=t->next;g->overflow_head=s->next;if(!g->overflow_head)g->overflow_tail=0;
    g->overflow_count--;*t=*s;t->next=0;*s=(MarkTask){0};
    if(tail)tail->next=index;else g->head=index;
    g->tail=index;g->queued++;return GC_MARK_OK;
}
#endif
