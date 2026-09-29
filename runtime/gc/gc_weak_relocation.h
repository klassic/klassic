/* Atomic covered weak scan -> relocation-start adoption, common phase/weak/
 * heap lock held. Selection, relocation-turn ownership, drained mover gate and
 * remap-color reuse proofs are STILL prerequisites of the relocation helper.
 * This adds weak coverage/cleanup; it does not infer those independent proofs.
 *
 * Do not FINISH/reopen the scan first. A waiting/failed relocation BEGIN leaves
 * the covered scan and weak admission closed, so creation cannot invalidate
 * coverage while the scheduler retries outside the lock. Success starts the
 * handshake before clearing weak claims/reopening. Loads then wait for adoption.
 * Phase and cross-generation reclamation exclusion remain owned until transfer.
 * No scan, allocation, payload access or wait happens inside this bridge.
 */
#ifndef KLASSIC_GC_WEAK_RELOCATION_H
#define KLASSIC_GC_WEAK_RELOCATION_H
#include "gc_weak_scan.h"
#include "gc_relocation_phase.h"
static inline uint64_t gc_weak_scan_relocate_begin_locked(WeakScan *scan,RootRegistry *registry,
        RelocationPhaseControl *relocation,RelocationPhaseStep step){
    if(!scan||((uintptr_t)scan&7)||!registry||((uintptr_t)registry&7)||!relocation||((uintptr_t)relocation&7)||
        !step||!gc_weak_scan_valid(scan)||registry!=(void *)(uintptr_t)scan->owner->gate->registry||
        relocation->handshake!=scan->owner->gate->handshake||relocation->generation!=scan->generation)
        return GC_RP_INVALID;
    if(scan->stage==1)return GC_RP_WAIT;
    if(scan->stage!=2||scan->cursor!=scan->capacity)return GC_RP_INVALID;
    if(scan->owner->processor)return GC_RP_WAIT;
    WeakProcess zero={0};if(memcmp(&scan->work,&zero,sizeof(zero)))return GC_RP_INVALID;
    uint64_t status=step(registry,relocation,GC_RP_BEGIN);
    if(status==GC_RP_ADVANCED){
        scan->owner->scan=0;scan->owner->stage=0;scan->owner->gate->closed=0;scan->stage=5;
    }
    return status;
}
typedef uint64_t (*WeakRelocationBegin)(WeakScan *,RootRegistry *,RelocationPhaseControl *);
static inline uint64_t gc_weak_relocation_begin_locked(WeakScan *scan,RootRegistry *registry,
        RelocationPhaseControl *relocation){
    return gc_weak_scan_relocate_begin_locked(scan,registry,relocation,gc_relocation_phase_step_locked);
}
#endif
