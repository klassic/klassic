/* Bounded Old-page inventory traversal under the same registry lock as
 * ATTACH/DETACH, flip, claims and mark end. Each call visits at most the current
 * and next registration; no bitmap or heap traversal occurs here. The unique
 * native cursor is live/disjoint and must not be copied, modified or replayed.
 * BEGIN requires a zero cursor. NEXT yields one older registration or releases
 * the walk at the tail. CANCEL releases early. ITEM borrows registration/set
 * storage only, not page mappings, object liveness or a remembered slot claim.
 *
 * The inventory's walker count prevents ALL unlink/free and epoch BEGIN while
 * a cursor is active. ATTACH can prepend newer nodes; this finite pass excludes
 * them. Pending/unflipped/claim counts and subsequent passes handle new work;
 * DONE alone is never termination. Release/cancel the cursor before waiting
 * for mark end or next-epoch adoption. TAKE/FLIP/ACK use the inventory wrapper,
 * and any returned claim has its own lifetime after NEXT/CANCEL. No polling,
 * allocation or OS operation is performed with this lock held.
 */
#ifndef KLASSIC_GC_REMEMBERED_WALK_H
#define KLASSIC_GC_REMEMBERED_WALK_H
#include "gc_remembered_inventory.h"
enum { GC_RMW_BEGIN=0,GC_RMW_NEXT=1,GC_RMW_CANCEL=2 };
enum { GC_RMW_ITEM=0,GC_RMW_INVALID=1,GC_RMW_DONE=2,GC_RMW_EXHAUSTED=3 };
static inline uint64_t gc_remembered_walk_locked(RememberedInventory *g,RememberedWalk *w,uint64_t op){
    if(op>GC_RMW_CANCEL||!gc_remembered_inventory_valid(g)||!w||((uintptr_t)w&7))return GC_RMW_INVALID;
    RememberedRegistration *r;
    RememberedJob job={0};RememberedInventoryRequest q={.job=(uintptr_t)&job};
    if(op==GC_RMW_BEGIN){
        if(w->registry||w->registration||w->identity||w->limit||w->epoch)return GC_RMW_INVALID;
        if(!g->head)return GC_RMW_DONE;
        if(g->walkers==UINT64_MAX)return GC_RMW_EXHAUSTED;
        q.registration=g->head;
        if(gc_remembered_inventory_step_locked(g,&q,GC_RI_VALIDATE)!=GC_RS_OK)return GC_RMW_INVALID;
        r=(RememberedRegistration *)(uintptr_t)g->head;
        *w=(RememberedWalk){(uintptr_t)g,(uintptr_t)r,r->identity,g->last_identity,g->epoch};
        g->walkers++;return GC_RMW_ITEM;
    }
    if(w->registry!=(uintptr_t)g||!g->walkers||w->epoch!=g->epoch||!w->identity||
       w->identity>w->limit||w->limit>g->last_identity)return GC_RMW_INVALID;
    q.registration=w->registration;
    if(gc_remembered_inventory_step_locked(g,&q,GC_RI_VALIDATE)!=GC_RS_OK)return GC_RMW_INVALID;
    r=(RememberedRegistration *)(uintptr_t)w->registration;
    if(r->identity!=w->identity)return GC_RMW_INVALID;
    if(op==GC_RMW_NEXT&&r->next){
        q.registration=r->next;
        if(gc_remembered_inventory_step_locked(g,&q,GC_RI_VALIDATE)!=GC_RS_OK)return GC_RMW_INVALID;
        r=(RememberedRegistration *)(uintptr_t)r->next;
        w->registration=(uintptr_t)r;w->identity=r->identity;return GC_RMW_ITEM;
    }
    g->walkers--;*w=(RememberedWalk){0};return GC_RMW_DONE;
}
#endif
