/* Heap-lock protected relocation admission. Every mover registers BEFORE
 * claiming a source or preparing a private target and retains its permit until
 * publication, or completed private cleanup + source retention. One permit is
 * one live job, not proof of its copy/cleanup correctness. Native controls remain
 * live and disjoint; all list edits use this same lock.
 * Bound installed jobs must FINISH before RELEASE; the job pointer prevents
 * bypassing their publication receipt. Before binding, cleanup remains a caller obligation.
 * INSPECT validates a live unbound permit and its nonce/links without changing
 * any state, for private cleanup while admission remains held. It does not
 * authorize payload access or authenticate an installed job.
 * CLOSE denies new movers immediately; existing jobs may finish and RELEASE.
 * CHECK succeeds only closed + empty. Keep admission closed through the entire
 * incoming-reference audit/seal, and capture epoch to reject a reopened audit.
 * REOPEN advances epoch without wrap. No traversal, payload access, OS work,
 * polling or waiting here. This gate is not a whole-source retirement proof.
 */
#ifndef KLASSIC_GC_RELOCATION_GATE_H
#define KLASSIC_GC_RELOCATION_GATE_H
#include "gc_abi.h"
enum { GC_RG_ACQUIRE=0,GC_RG_RELEASE=1,GC_RG_CLOSE=2,GC_RG_CHECK=3,GC_RG_REOPEN=4,GC_RG_INSPECT=5 };
enum { GC_RG_OK=0,GC_RG_INVALID=1,GC_RG_WAIT=2,GC_RG_EXHAUSTED=3 };
static inline uint64_t gc_relocation_gate_locked(RelocationGate *g,RelocationGateRequest *q,uint64_t op){
    if(op>GC_RG_INSPECT||!g->epoch||g->closed>1||g->active>g->last_serial||!!g->active!=!!g->head||(g->head&7))return GC_RG_INVALID;
    if(op==GC_RG_CLOSE){g->closed=1;return g->active?GC_RG_WAIT:GC_RG_OK;}
    if(op==GC_RG_CHECK)return g->closed&&!g->active?GC_RG_OK:GC_RG_WAIT;
    if(op==GC_RG_REOPEN){
        if(!g->closed)return GC_RG_INVALID;if(g->active)return GC_RG_WAIT;
        if(g->epoch==UINT64_MAX)return GC_RG_EXHAUSTED;
        g->epoch++;g->closed=0;return GC_RG_OK;
    }
    if(!q||((uintptr_t)q&7)||!q->permit||(q->permit&7))return GC_RG_INVALID;
    RelocationPermit *p=(RelocationPermit *)(uintptr_t)q->permit;
    if(op==GC_RG_ACQUIRE){
        if(q->serial||q->epoch||p->gate||p->serial||p->epoch||p->previous||p->next||p->job)return GC_RG_INVALID;
        if(g->closed)return GC_RG_WAIT;
        if(g->last_serial==UINT64_MAX)return GC_RG_EXHAUSTED;
        RelocationPermit *head=(RelocationPermit *)(uintptr_t)g->head;
        if(head&&(head->gate!=(uintptr_t)g||head->epoch!=g->epoch||head->previous||!head->serial||head->serial>g->last_serial))return GC_RG_INVALID;
        *p=(RelocationPermit){(uintptr_t)g,++g->last_serial,g->epoch,0,g->head,0};
        if(head)head->previous=(uintptr_t)p;
        g->head=(uintptr_t)p;g->active++;q->serial=p->serial;q->epoch=p->epoch;return GC_RG_OK;
    }
    if(p->job||!g->active||p->gate!=(uintptr_t)g||!p->serial||q->serial!=p->serial||p->serial>g->last_serial||
        p->epoch!=g->epoch||q->epoch!=p->epoch||(p->previous&7)||(p->next&7)||
        (g->active==1&&(p->previous||p->next))||(g->active>1&&!p->previous&&!p->next))return GC_RG_INVALID;
    RelocationPermit *previous=(RelocationPermit *)(uintptr_t)p->previous,*next=(RelocationPermit *)(uintptr_t)p->next;
    if(previous){if(previous->gate!=(uintptr_t)g||previous->epoch!=g->epoch||previous->serial<=p->serial||
        previous->serial>g->last_serial||previous->next!=(uintptr_t)p)return GC_RG_INVALID;}
    else if(g->head!=(uintptr_t)p)return GC_RG_INVALID;
    if(next&&(next->gate!=(uintptr_t)g||next->epoch!=g->epoch||!next->serial||next->serial>=p->serial||next->previous!=(uintptr_t)p))return GC_RG_INVALID;
    if(op==GC_RG_INSPECT)return GC_RG_OK;
    if(previous)previous->next=p->next;else g->head=p->next;
    if(next)next->previous=p->previous;
    g->active--;*p=(RelocationPermit){0};return GC_RG_OK;
}
#endif
