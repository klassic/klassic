/* Application collector remembered transport. Storage is budgeted and attached
 * to actual Old runs. All registry steps share KgRuntime.lock. No raw managed
 * address escapes; the driver's visitor resolves the retained logical owner and
 * completes rooted Young handoff before returning a keep/transfer receipt. */
#ifndef KLASSIC_GEN_REMEMBERED_H
#define KLASSIC_GEN_REMEMBERED_H
#include "klassic_gen_runtime_internal.h"
#include "gc_remembered_inventory.h"
typedef struct {
    uint64_t keep;
    KgRun *target;
    uint64_t target_offset;
    const MarkRecord *dead_record;
    uint64_t dead_epoch;
} KgRememberedResult;
typedef void (*KgRememberedVisit)(KgRuntime *,const RememberedRegistration *,
    const RememberedJob *,const RememberedOwner *,const MetadataLeaseRequest *,
    KgRememberedResult *,void *);
/* Returns GC_BUDGET_*. Caller owns this private Old run and a real Native actor;
 * no source guard, runtime lock, or published consumer may cross preparation. */
uint64_t kg_remembered_attach(KgRuntime *,KgRun *,const MutatorContext *);
/* Driver already called native_cycle_begin's RI_BEGIN. This merely flips live
 * page registrations to that exact prepared Young MarkStart, outside the global
 * stopped interval. The function never begins a second inventory epoch. */
void kg_remembered_begin(KgRuntime *,uint64_t phase);
void kg_remembered_drain_context(KgRuntime *,MutatorContext *);
/* Single driver owner; callback completes handoff without ACKing the claim or
 * releasing the borrowed source metadata lease. target means transfer, and must
 * denote the SAME logical Old owner/relative field after canonical following.
 * keep is permitted only after actual rooted Young mark handoff has completed. */
void kg_remembered_scan(KgRuntime *,KgRememberedVisit,void *);
/* After the actual source-drain seal has made an Old run EVACUATED, transfer
 * all of its current AND previous slots. Every visit must return its canonical
 * distinct Old target, or the selected Old cycle's exact dead record/epoch.
 * Death requires the real selected-run stamp and retained closed source cell;
 * neither empty queues nor zero metadata leases authorize dropping a slot. */
void kg_remembered_migrate_run(KgRuntime *,KgRun *,KgRememberedVisit,void *);
void kg_remembered_promote(KgRuntime *,KgRun *,uint64_t owner,
    uint64_t descriptor,uint64_t payload,uint64_t phase);
/* Returns GC_RS_WAIT with backing retained until all current/previous bits,
 * jobs, walks and source leases have really drained. Caller separately proves
 * no future source producers or references can be introduced after detachment. */
uint64_t kg_remembered_detach(KgRuntime *,KgRun *,const MutatorContext *);
uint64_t kg_remembered_detach_with_pin(KgRuntime *,KgRun *,const MutatorContext *,PinPageRegistry *,const RelocationGateRequest *);
#endif
