/* Bounded installed-relocation completion funnel, heap lock held. The unique
 * stable control, job, complete copy, source/destination metadata and mappings
 * remain live until DONE. Target INSTALL and generation handoff precede stage 0.
 * Inputs are immutable; callers may poll/reschedule between MORE results only
 * while retaining all ownership. No allocation, OS call or payload copying.
 * Stages: 0 bind, 1 publish/check, 2 finish, 3 release, 4 terminal.
 * Interrupted publication resumes by reading the bound exact receipt. Invalid
 * requests retain ownership; they never discard an installed destination.
 */
#ifndef KLASSIC_GC_RELOCATION_COMPLETE_H
#define KLASSIC_GC_RELOCATION_COMPLETE_H
#include "gc_relocation_job.h"
#include "gc_relocate.h"
enum { GC_RC_DONE=0,GC_RC_INVALID=1,GC_RC_MORE=2,GC_RC_WAIT=3 };
static inline uint64_t gc_relocation_complete_locked(RelocationGate *g,RelocationCompletion *q){
    if(q->stage>3||(q->stage?q->owner!=(uintptr_t)q:q->owner!=0)||
        !q->job||(q->job&7)||!q->copy||(q->copy&7))return GC_RC_INVALID;
    RelocationJob *j=(RelocationJob *)(uintptr_t)q->job;
    RelocationCopy *copy=(RelocationCopy *)(uintptr_t)q->copy;
    if(!j->source||(j->source&7)||!j->ticket||copy->ticket!=j->ticket||copy->stage!=2||
        copy->destination!=(j->destination>>16))return GC_RC_INVALID;
    ForwardCell *source=(ForwardCell *)(uintptr_t)j->source;
    if(copy->source_incarnation!=source->incarnation)return GC_RC_INVALID;
    if(!q->stage){
        if(gc_relocation_job_locked(g,j,GC_RLJ_BIND)!=GC_RLJ_OK)return GC_RC_INVALID;
        q->owner=(uintptr_t)q;q->stage=1;return GC_RC_MORE;
    }
    if(q->stage==1){
        uint64_t status=gc_relocation_job_locked(g,j,GC_RLJ_CHECK);
        if(status==GC_RLJ_WAIT){
            status=gc_relocate_publish(source,copy,j->destination&UINT16_MAX);
            if(status==GC_COPY_WAIT)return GC_RC_WAIT;
            if(status!=GC_COPY_DONE)return GC_RC_INVALID;
            status=gc_relocation_job_locked(g,j,GC_RLJ_CHECK);
        }
        if(status!=GC_RLJ_OK)return GC_RC_INVALID;
        q->stage=2;return GC_RC_MORE;
    }
    if(q->stage==2){
        if(gc_relocation_job_locked(g,j,GC_RLJ_FINISH)!=GC_RLJ_OK)return GC_RC_INVALID;
        q->stage=3;return GC_RC_MORE;
    }
    if(j->owner!=(uintptr_t)j||j->gate!=(uintptr_t)g||j->state!=3)return GC_RC_INVALID;
    RelocationGateRequest release={j->permit,j->serial,j->epoch};
    if(gc_relocation_gate_locked(g,&release,GC_RG_RELEASE)!=GC_RG_OK)return GC_RC_INVALID;
    q->stage=4;return GC_RC_DONE;
}
#endif
