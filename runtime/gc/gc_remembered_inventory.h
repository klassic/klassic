/* Global page inventory, using the SAME registry lock as producers, mark end
 * and local bitmap operations. Register every live Old-page set before it can
 * receive entries; after registration never call gc_remembered_step_locked
 * directly. The wrapper maintains counts of sets with pending bits/claims and
 * sets still needing the new flip, without traversing pages during mark end.
 * BEGIN publishes a prepared Young MarkStart epoch in O(1); each page FLIP is
 * bounded work outside the global stopped interval. New-epoch RECORD/TAKE on
 * an unflipped page returns WAIT without consuming the producer's entry.
 * ATTACH requires zeroed maps; DETACH requires empty maps and caller proof of
 * no remaining page/entry/native borrow. Active bounded walks block DETACH
 * and epoch BEGIN; ATTACH may still prepend new registrations. Link storage
 * stays live while listed. Walkers never borrow managed object liveness.
 * DETACH_PAGE additionally binds the exact Old-page incarnation, requires a
 * closed/unowned page and rejects outstanding metadata leases, including
 * buffered source records. For this op only, q.phase is the native page pointer.
 * Use the SAME heap/registry lock as lease acquisition/release. Native page
 * storage remains live throughout. This guard does not prove absence of future
 * source producers or permit metadata/color/address reclamation: caller still
 * establishes canonical redirection and the outer retirement boundary.
 * The scheduler alone calls BEGIN, after prior global termination/retirement;
 * all older buffers/jobs must be acknowledged, not merely locally empty.
 * Request, job, registration, inventory, sets and bitmaps are native/disjoint.
 */
#ifndef KLASSIC_GC_REMEMBERED_INVENTORY_H
#define KLASSIC_GC_REMEMBERED_INVENTORY_H
#include "gc_remembered.h"
#define GC_RI_ATTACH 0u
#define GC_RI_DETACH 1u
#define GC_RI_BEGIN 2u
#define GC_RI_RECORD 3u
#define GC_RI_FLIP 4u
#define GC_RI_TAKE 5u
#define GC_RI_ACK 6u
#define GC_RI_CHECK 7u
#define GC_RI_VALIDATE 8u /* read-only membership/local-state validation */
#define GC_RI_DETACH_PAGE 9u /* q.phase holds a live AllocationPage pointer */
static inline bool gc_remembered_inventory_valid(const RememberedInventory *g) {
    return g&& !((uintptr_t)g&7)&&!!g->head==!!g->members&&g->members<=g->last_identity&&
        g->pending<=g->members&&g->claims<=g->members&&g->unflipped<=g->members&&
        gc_abi_valid_phase((PhaseSnapshot){g->epoch})&&(g->epoch>>16)&&
        gc_abi_valid_phase((PhaseSnapshot){g->last_phase})&&g->last_phase>=g->epoch&&
        (!g->previous_epoch||(gc_abi_valid_phase((PhaseSnapshot){g->previous_epoch})&&
        (g->previous_epoch>>16)&&(g->previous_epoch>>16)<(g->epoch>>16)&&((g->previous_epoch^g->epoch)&0x1000)));
}
static inline uint64_t gc_remembered_inventory_step_locked(RememberedInventory *g,RememberedInventoryRequest *q,uint64_t op) {
    if(op>9||!gc_remembered_inventory_valid(g)||!q||((uintptr_t)q&7))return GC_RS_INVALID;
    if(op==GC_RI_CHECK)return g->unflipped||g->pending||g->claims||g->walkers?GC_RS_WAIT:GC_RS_EMPTY;
    if(op==GC_RI_BEGIN) {
        if(!gc_abi_valid_phase((PhaseSnapshot){q->phase})||(q->phase&15)!=GC_MARK_START||
           (q->phase>>16)<=(g->last_phase>>16)||!((q->phase^g->epoch)&0x1000))return GC_RS_INVALID;
        if(g->unflipped||g->pending||g->claims||g->walkers)return GC_RS_WAIT;
        g->previous_epoch=g->epoch;g->epoch=g->last_phase=q->phase;g->unflipped=g->members;return GC_RS_OK;
    }
    if(!q->registration||(q->registration&7)||!q->job||(q->job&7))return GC_RS_INVALID;
    RememberedRegistration *r=(RememberedRegistration *)(uintptr_t)q->registration;
    if(!r->set||(r->set&7))return GC_RS_INVALID;
    RememberedSet *s=(RememberedSet *)(uintptr_t)r->set;RememberedJob *job=(RememberedJob *)(uintptr_t)q->job;
    uint64_t status=gc_remembered_step_locked(s,job,GC_RS_CHECK);
    if(status!=GC_RS_EMPTY&&status!=GC_RS_WAIT)return status;
    if(op==GC_RI_ATTACH) {
        if(r->registry||r->identity||r->previous||r->next||r->epoch||s->epoch!=g->epoch||
           s->previous_epoch!=g->previous_epoch||s->last_phase!=g->epoch||s->current_count||s->pending||s->active_serial)return GC_RS_INVALID;
        if(g->last_identity==UINT64_MAX)return GC_RS_EXHAUSTED;
        RememberedRegistration *head=(RememberedRegistration *)(uintptr_t)g->head;
        if(head&&((g->head&7)||head->registry!=(uintptr_t)g||head->previous||!head->identity||head->identity>g->last_identity))return GC_RS_INVALID;
        r->registry=(uintptr_t)g;r->identity=++g->last_identity;r->epoch=g->epoch;r->next=g->head;
        if(head)head->previous=(uintptr_t)r;g->head=(uintptr_t)r;g->members++;return GC_RS_OK;
    }
    if(r->registry!=(uintptr_t)g||!r->identity||r->identity>g->last_identity||!g->members||r->epoch!=s->epoch||
       (r->epoch!=g->epoch&&(!g->previous_epoch||r->epoch!=g->previous_epoch))||
       (r->epoch!=g->epoch&&!g->unflipped)||(r->epoch==g->epoch&&g->unflipped==g->members)||((r->previous|r->next)&7))return GC_RS_INVALID;
    RememberedRegistration *prev=(RememberedRegistration *)(uintptr_t)r->previous,*next=(RememberedRegistration *)(uintptr_t)r->next;
    if(prev?(prev->registry!=(uintptr_t)g||prev->next!=(uintptr_t)r||prev->identity<=r->identity||prev->identity>g->last_identity):g->head!=(uintptr_t)r)return GC_RS_INVALID;
    if(next&&(next->registry!=(uintptr_t)g||next->previous!=(uintptr_t)r||!next->identity||next->identity>=r->identity))return GC_RS_INVALID;
    uint64_t pending=!!s->pending,active=!!s->active_serial;
    if(g->pending<pending||g->claims<active||(!pending&&g->pending==g->members)||(!active&&g->claims==g->members))return GC_RS_INVALID;
    if(op==GC_RI_VALIDATE)return GC_RS_OK;
    if(op==GC_RI_DETACH||op==GC_RI_DETACH_PAGE) {
        if(op==GC_RI_DETACH_PAGE){
            if(!q->phase||(q->phase&7))return GC_RS_INVALID;
            AllocationPage *page=(AllocationPage *)(uintptr_t)q->phase;
            if(page->start!=s->start||page->limit!=s->limit||page->incarnation!=s->incarnation||page->generation!=GC_OLD)return GC_RS_INVALID;
            uint64_t state=atomic_load_explicit(&page->state,memory_order_acquire);
            if(state!=GC_PAGE_ALLOCATING&&state!=GC_PAGE_CLOSED&&state!=GC_PAGE_EVACUATED&&state!=GC_PAGE_UNCOMMITTED)return GC_RS_INVALID;
            if(state==GC_PAGE_ALLOCATING||page->owner||page->metadata_leases)return GC_RS_WAIT;
        }
        if(g->walkers||r->epoch!=g->epoch||s->current_count||pending||active)return GC_RS_WAIT;
        if(prev)prev->next=r->next;else g->head=r->next;if(next)next->previous=r->previous;
        g->members--;r->registry=r->identity=r->previous=r->next=r->epoch=0;return GC_RS_OK;
    }
    if(op==GC_RI_FLIP) {
        if(r->epoch==g->epoch||job->phase!=g->epoch)return GC_RS_INVALID;
    }else if(r->epoch!=g->epoch&&(op!=GC_RI_RECORD||job->phase>=g->epoch))return GC_RS_WAIT;
    status=gc_remembered_step_locked(s,job,op-GC_RI_RECORD);
    if(status==GC_RS_OK||status==GC_RS_MORE||status==GC_RS_EMPTY||status==GC_RS_WAIT) {
        g->pending=g->pending-pending+!!s->pending;g->claims=g->claims-active+!!s->active_serial;
        if(op==GC_RI_RECORD&&status==GC_RS_OK&&job->phase>g->last_phase)g->last_phase=job->phase;
        if(op==GC_RI_FLIP&&status==GC_RS_OK){r->epoch=g->epoch;g->unflipped--;}
    }
    return status;
}
#endif
