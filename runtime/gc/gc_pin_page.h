/* Page pin admission, serialized by the heap/selection lock. Each page
 * incarnation owns a dedicated gate (epoch initially 1), never a mover gate.
 * Permits are stable native controls, one per pin, and may survive native
 * blocking. They carry no source-access guard. The page and permit lifetimes
 * must outlive registration. The owner must independently retain a strong root
 * and metadata lease; this admission primitive does not return an address.
 *
 * Every selection of any object on this page must acquire SELECT first and
 * hold it through forwarding publication/private cleanup. A selected page
 * denies pins: callers follow/assist using their root outside the lock, then
 * retry at the canonical destination's page. A pinned page is skipped without
 * closing admission or waiting for native code. REOPEN is allowed only after
 * the owner proves all selected work is finished. Retirement also requires
 * exclusive selection plus the separate reachability/metadata proofs.
 *
 * Pin requests use the existing nonce-authenticated intrusive permit ABI.
 * Selection requests are unique owner-private controls (not public handles),
 * have permit=serial=0 and receive the exclusive epoch. Do not recycle a page
 * control until all callers and requests for its old incarnation are gone.
 * No allocation, polling, payload access or traversal occurs here.
 */
#ifndef KLASSIC_GC_PIN_PAGE_H
#define KLASSIC_GC_PIN_PAGE_H
#include "gc_relocation_gate.h"
enum { GC_PP_PIN=0,GC_PP_UNPIN=1,GC_PP_SELECT=2,GC_PP_REOPEN=3 };
static inline uint64_t gc_pin_page_locked(RelocationGate *page,
                                         RelocationGateRequest *q,uint64_t op){
    if(!page||((uintptr_t)page&7)||!q||((uintptr_t)q&7)||op>GC_PP_REOPEN)
        return GC_RG_INVALID;
    if(op==GC_PP_PIN||op==GC_PP_UNPIN)
        return gc_relocation_gate_locked(page,q,op==GC_PP_PIN?GC_RG_ACQUIRE:GC_RG_RELEASE);
    if(q->permit||q->serial)return GC_RG_INVALID;
    if(op==GC_PP_SELECT){
        if(q->epoch)return GC_RG_INVALID;
        /* Validate using the common gate contract without closing a pinned
         * page. CLOSE does not dereference intrusive links. */
        RelocationGate candidate=*page;
        uint64_t status=gc_relocation_gate_locked(&candidate,NULL,GC_RG_CLOSE);
        if(status!=GC_RG_OK)return status;
        if(page->closed)return GC_RG_WAIT;
        page->closed=1;q->epoch=page->epoch;return GC_RG_OK;
    }
    if(!q->epoch||q->epoch!=page->epoch)return GC_RG_INVALID;
    uint64_t status=gc_relocation_gate_locked(page,NULL,GC_RG_REOPEN);
    if(status==GC_RG_OK)q->epoch=0;
    return status;
}
#endif
