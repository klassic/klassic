/* Replace the physical receiver of one exclusive in-flight mark claim under
 * its generation queue lock. No source guard may be held while acquiring it.
 * The caller has followed the source's strong worker/root reference and used
 * guarded mark preparation to obtain a destination with the SAME MarkRecord.
 * Destination and original leases belong to the same metadata pool and are
 * distinct unique obligations. All native controls/records remain live and
 * disjoint; no heap or descriptor is accessed here.
 *
 * On OK the destination obligation transfers into both the authenticated task
 * and its claim. The processed, aligned cursor and in-flight count persist;
 * serial rotation rejects stale copied claims. The old obligation transfers to
 * retired (initially empty), which must be resolved/released outside this queue
 * lock. Source metadata cannot retire before that release. No stamp is changed.
 * On rejection/exhaustion both input protections remain owned and all state is
 * preserved. This is neither promotion nor a phase/termination/retirement policy.
 */
#ifndef KLASSIC_GC_MARK_RETARGET_H
#define KLASSIC_GC_MARK_RETARGET_H
#include "gc_mark.h"
static inline uint64_t gc_mark_retarget_locked(MarkQueue *g,MarkRetarget *q){
    if(gc_mark_step_locked(g,NULL,GC_MARK_CHECK)!=GC_MARK_BUSY||!q->job||(q->job&7))return GC_MARK_INVALID;
    MarkRequest *j=(MarkRequest *)(uintptr_t)q->job,*d=&q->destination,*old=&q->retired;
    if(old->record||old->reference||old->incarnation||old->metadata_lease||old->index||old->serial||old->cursor||
        !j->index||j->index>g->capacity||!j->serial||!g->in_flight||!j->record||(j->record&7)||
        !j->incarnation||!j->metadata_lease||(j->cursor&7)||d->record!=j->record||
        !d->incarnation||!d->metadata_lease||d->metadata_lease==j->metadata_lease||d->index||d->serial||d->cursor)return GC_MARK_INVALID;
    GcRef checked;
    if(!gc_abi_encode_ref(j->reference>>16,(uint16_t)j->reference,&checked)||
        !gc_abi_encode_ref(d->reference>>16,(uint16_t)d->reference,&checked))return GC_MARK_INVALID;
    MarkTask *t=(MarkTask *)(uintptr_t)g->tasks+j->index-1;
    if(t->state!=2||t->next||t->serial!=j->serial||j->serial>g->last_serial||t->record!=j->record||
        t->reference!=j->reference||t->incarnation!=j->incarnation||t->metadata_lease!=j->metadata_lease||j->cursor<t->cursor)return GC_MARK_INVALID;
    MarkRecord *record=(MarkRecord *)(uintptr_t)j->record;
    if(!record->object_id||(g->generation?record->old_epoch:record->young_epoch)!=g->epoch)return GC_MARK_INVALID;
    if(g->last_serial==UINT64_MAX)return GC_MARK_EXHAUSTED;
    *old=(MarkRequest){.record=j->record,.reference=j->reference,.incarnation=j->incarnation,.metadata_lease=j->metadata_lease};
    j->reference=t->reference=d->reference;j->incarnation=t->incarnation=d->incarnation;
    j->metadata_lease=t->metadata_lease=d->metadata_lease;t->cursor=j->cursor;
    j->serial=t->serial=++g->last_serial;*d=(MarkRequest){0};return GC_MARK_OK;
}
#endif
