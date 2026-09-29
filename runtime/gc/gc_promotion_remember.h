/* Bounded prepublication promotion bookkeeping under the remembered registry
 * lock. Caller owns an installed but unpublished Old destination, retains its
 * metadata and immutable descriptor, and supplies that exact payload shape.
 * No managed address or field value is read: descriptor reference slots are
 * recorded even when null. Native arguments and metadata are live/disjoint.
 * One descriptor candidate is examined per call. Cursor commits only after its
 * slot record succeeds; retries cannot skip a slot. DONE permits only remset
 * handoff, NOT forwarding publication until Old marking also permits it.
 * The owner retains its phase/producer obligation until completion/publication.
 * WAIT means inventory/page epoch changed or is unflipped. To adopt a new phase,
 * restart cursor at zero while the destination remains private. Never continue
 * a partially scanned cursor with a new phase. Earlier records remain valid
 * conservative entries. No heap access, allocation, polling or OS call occurs.
 */
#ifndef KLASSIC_GC_PROMOTION_REMEMBER_H
#define KLASSIC_GC_PROMOTION_REMEMBER_H
#include "gc_descriptors.h"
#include "gc_remembered_inventory.h"
enum { GC_PR_DONE=0, GC_PR_MORE=1, GC_PR_INVALID=2, GC_PR_WAIT=3 };
typedef uint64_t (*PromotionRememberStep)(RememberedInventory *,PromotionRemember *);
static inline uint64_t gc_promotion_remember_locked(RememberedInventory *g,PromotionRemember *q){
    if(!q||((uintptr_t)q&7)||!q->registry||(q->registry&7))return GC_PR_INVALID;
    RememberedJob job={0};RememberedInventoryRequest request={q->registration,(uintptr_t)&job,0};
    if(gc_remembered_inventory_step_locked(g,&request,GC_RI_VALIDATE)!=GC_RS_OK)return GC_PR_INVALID;
    RememberedSet *set=(RememberedSet *)(uintptr_t)((RememberedRegistration *)(uintptr_t)q->registration)->set;
    if(q->owner<set->start+sizeof(ObjectHeader)||q->owner>set->limit||(q->owner&15)||
       q->payload_bytes>set->limit-q->owner)return GC_PR_INVALID;
    if(q->phase!=g->epoch||set->epoch!=g->epoch)return GC_PR_WAIT;
    uint64_t offset=0;
    DescriptorScan scan={q->descriptor_id,q->payload_bytes,q->cursor,1,(uintptr_t)&offset,1,0,0};
    uint64_t result=gc_descriptor_scan((DescriptorRegistry *)(uintptr_t)q->registry,&scan);
    if(result==GC_SCAN_INVALID)return GC_PR_INVALID;
    if(scan.written){
        job=(RememberedJob){.offset=q->owner+offset,.incarnation=set->incarnation,.phase=q->phase};
        if(gc_remembered_inventory_step_locked(g,&request,GC_RI_RECORD)!=GC_RS_OK)return GC_PR_INVALID;
    }
    q->cursor=scan.next_offset;return result==GC_SCAN_DONE?GC_PR_DONE:GC_PR_MORE;
}
#endif
