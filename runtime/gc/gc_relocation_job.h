/* Heap-lock protected installed-job receipt. The scheduler supplies a unique,
 * stable job control and live source/permit/gate metadata. BIND follows target
 * INSTALL and generation handoff, before forwarding; the complete copy and both
 * mappings remain owned by this job until publication or explicit recovery.
 * Claim tickets come from this heap gate's permit serial via relocation CLAIM;
 * BIND/CHECK/FINISH require that exact serial, preventing another permit from
 * adopting the claim. Bound inputs are immutable until disposal.
 * This records ownership, not proof of INSTALL, private copy or handoff itself.
 * Do not copy/zero/reuse a bound control until its permit has been released and
 * no scheduler can reference it. CHECK authenticates the permit incarnation and
 * original claim, then records the exact immutable publication receipt. WAIT
 * retains every resource. FINISH requires CHECK-completed state and the same
 * exact publication, unbinds the permit, and makes this job terminal. Only then
 * can gate RELEASE succeed. One permit cannot bind two live job controls.
 * This leaf never releases the permit, allocates, waits or reads payload.
 */
#ifndef KLASSIC_GC_RELOCATION_JOB_H
#define KLASSIC_GC_RELOCATION_JOB_H
#include "gc_relocation_gate.h"
#include "gc_forward.h"
enum { GC_RLJ_BIND=0,GC_RLJ_CHECK=1,GC_RLJ_FINISH=2 };
enum { GC_RLJ_OK=0,GC_RLJ_INVALID=1,GC_RLJ_WAIT=2 };
static inline uint64_t gc_relocation_job_locked(RelocationGate *g,RelocationJob *j,uint64_t op){
    if(op>2||!g->epoch||g->closed>1||!g->active||g->active>g->last_serial||!g->head||(g->head&7)||
        !j->permit||(j->permit&7)||!j->source||(j->source&7)||!j->ticket||j->ticket>(UINT64_MAX>>2))return GC_RLJ_INVALID;
    RelocationPermit *p=(RelocationPermit *)(uintptr_t)j->permit;
    if(p->gate!=(uintptr_t)g||!p->serial||p->serial!=j->ticket||p->serial>g->last_serial||p->epoch!=g->epoch||
        (p->previous&7)||(p->next&7)||(g->active==1&&(p->previous||p->next))||
        (g->active>1&&!p->previous&&!p->next))return GC_RLJ_INVALID;
    RelocationPermit *previous=(RelocationPermit *)(uintptr_t)p->previous,*next=(RelocationPermit *)(uintptr_t)p->next;
    if(previous){if(previous->gate!=(uintptr_t)g||previous->epoch!=g->epoch||previous->serial<=p->serial||
        previous->serial>g->last_serial||previous->next!=(uintptr_t)p)return GC_RLJ_INVALID;}
    else if(g->head!=(uintptr_t)p)return GC_RLJ_INVALID;
    if(next&&(next->gate!=(uintptr_t)g||next->epoch!=g->epoch||!next->serial||next->serial>=p->serial||next->previous!=(uintptr_t)p))return GC_RLJ_INVALID;
    ForwardCell *source=(ForwardCell *)(uintptr_t)j->source;
    if(!source->source_offset||(source->source_offset&15)||source->source_offset>GC_MAX_OFFSET||!source->incarnation||
        !gc_forward_destination(j->destination,source->source_offset)||
        atomic_load_explicit(&source->access,memory_order_acquire)!=GC_SOURCE_CLOSED)return GC_RLJ_INVALID;
    uint64_t word=atomic_load_explicit(&source->word,memory_order_acquire),claim=(j->ticket<<2)|1;
    if(!op){
        if(p->job||j->owner||j->gate||j->serial||j->epoch||j->source_offset||j->incarnation||j->state||word!=claim)return GC_RLJ_INVALID;
        j->owner=(uintptr_t)j;j->gate=(uintptr_t)g;j->serial=p->serial;j->epoch=p->epoch;
        j->source_offset=source->source_offset;j->incarnation=source->incarnation;j->state=1;p->job=(uintptr_t)j;return GC_RLJ_OK;
    }
    if(p->job!=(uintptr_t)j||j->owner!=(uintptr_t)j||j->gate!=(uintptr_t)g||j->serial!=p->serial||j->epoch!=p->epoch||
        j->source_offset!=source->source_offset||j->incarnation!=source->incarnation||!j->state||j->state>2)return GC_RLJ_INVALID;
    if(op==GC_RLJ_FINISH){
        if(j->state!=2||word!=(j->destination|2))return GC_RLJ_INVALID;
        j->state=3;p->job=0;return GC_RLJ_OK;
    }
    if(word==claim)return j->state==1?GC_RLJ_WAIT:GC_RLJ_INVALID;
    if(word!=(j->destination|2))return GC_RLJ_INVALID;
    j->state=2;return GC_RLJ_OK;
}
#endif
