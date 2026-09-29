/* Collector-side FINISH and owner notification under the registry lock.
 * The unique notification record, callback and event are live/disjoint from
 * root backing. Bind the correct owner's private event before calling. Callback
 * sets the sticky event and wakes without blocking or managed reentry.
 * Stage 0: cleanup unfinished; 1: FINISH succeeded, notification owes a retry;
 * 2: notification succeeded. RETRY is NOT a failed FINISH: never replay FINISH.
 * After stage 1 no root/registry/request/context memory is read; backing may
 * already be unmapped. Keep notification/event storage alive until stage 2,
 * independently of backing and thread detachment. Retry under the same lock.
 */
#ifndef KLASSIC_GC_ROOT_FINISH_NOTIFY_H
#define KLASSIC_GC_ROOT_FINISH_NOTIFY_H
#include "gc_root_lifetime.h"
#define GC_RL_NOTIFY_RETRY 4u
typedef int64_t (*RootCompletionNotify)(_Atomic uint32_t *);
static inline uint64_t gc_root_finish_notify_locked(RootRegistry *roots,RootLifeRequest *q,RootFinishNotification *n){
    if(!n||((uintptr_t)n&7)||n->stage>2)return GC_RL_INVALID;
    if(n->stage==2)return GC_RL_OK;
    if(!n->context||(n->context&7)||!n->event||(n->event&3)||!n->notify)return GC_RL_INVALID;
    if(!n->stage){
        if(!roots||((uintptr_t)roots&7)||!q||((uintptr_t)q&7)||
           !q->registration||(q->registration&7))return GC_RL_INVALID;
        const RootRegistration *r=(void *)(uintptr_t)q->registration;
        if(r->context!=n->context)return GC_RL_INVALID;
        uint64_t result=gc_root_life_locked(roots,q,GC_RL_FINISH);
        if(result!=GC_RL_OK)return result;
        n->stage=1;
    }
    int64_t result=((RootCompletionNotify)(uintptr_t)n->notify)((void *)(uintptr_t)n->event);
    n->last_error=(uint64_t)result;
    if(result)return GC_RL_NOTIFY_RETRY;
    n->stage=2;return GC_RL_OK;
}
#endif
