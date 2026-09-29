/* Connect runtime-owned requests/queues/registries to shared cycle boundaries.
 * Hold the request + registry + selected generation locks for every call;
 * phase snapshots and remset inventory use this same serialization. Notify,
 * poll, scan, assist and wait only after unlocking. No call scans roots/heap.
 *
 * Before BEGIN, the scheduler proves prior parity/color lifetime and all old
 * remset producer obligations finished. First startup is trivially fresh.
 * Before relocation/Retire and COMPLETE, the scheduler still supplies complete
 * selection, copy/publication, weak/native/heap/incoming/source/color retirement
 * proofs. These controls DO NOT replace those services or infer them from empty
 * queues. A Major ticket still owes the full major cycle.
 */
#ifndef KLASSIC_GC_NATIVE_CYCLE_H
#define KLASSIC_GC_NATIVE_CYCLE_H
#include "gc_native_runtime.h"
#include "gc_cycle_mark_start.h"
#include "gc_relocation_phase.h"
static inline bool gc_native_cycle_valid(NativeRuntimeOwner *runtime,uint64_t generation){
    return gc_native_runtime_valid(runtime)&&runtime->stage==3&&generation<2&&gc_native_runtime_work(runtime);
}
static inline uint64_t gc_native_cycle_request_locked(NativeRuntimeOwner *runtime,CycleTicket *ticket){
    if(!ticket||((uintptr_t)ticket&7)||!gc_native_cycle_valid(runtime,ticket->kind))return GC_CY_INVALID;
    return gc_cycle_step_locked(&runtime->cycles,ticket,GC_CY_REQUEST);
}
/* Preview only metadata controls: shared START touches neither linked owners
 * nor roots. Commit the ledger, epoch, remset and prepared phase together only
 * after its last fallible admission. Failure preserves all runtime state. */
static inline uint64_t gc_native_cycle_begin_locked(NativeRuntimeOwner *runtime,uint64_t generation,CycleMarkStarter start){
    if(!gc_native_cycle_valid(runtime,generation)||!start)return GC_CY_INVALID;
    NativeCycleState *cycle=&runtime->cycle[generation];
    if(((runtime->roots.phase>>(generation*4))&15)!=GC_IDLE||runtime->handshake.active)return GC_CY_WAIT;
    if(cycle->marking_audit.walk.active||cycle->marking_audit.walk.lease.kind||
       cycle->retirement_audit.walk.active||cycle->retirement_audit.walk.lease.kind||
       cycle->gate.active||cycle->gate.head||!cycle->gate.closed||cycle->completing)return GC_CY_INVALID;
    NativeWorkStorage *work=gc_native_runtime_work(runtime);MarkQueue queue=work->queues[generation];
    uint64_t checked=gc_mark_step_locked(&queue,NULL,GC_MARK_CHECK);
    if(checked!=GC_MARK_EMPTY)return checked==GC_MARK_BUSY?GC_CY_WAIT:GC_CY_INVALID;
    if(queue.epoch==UINT64_MAX||(runtime->roots.phase>>16)>(UINT64_MAX>>16)-2)return GC_CY_EXHAUSTED;
    queue.epoch++;
    RootRegistry roots=runtime->roots;HandshakeState handshake=runtime->handshake;
    CycleLedger ledger=runtime->cycles;RememberedInventory remembered=runtime->remembered;
    roots.phase=(roots.phase+UINT64_C(0x10000))|(UINT64_C(1)<<(generation*4));
    roots.phase^=UINT64_C(1)<<(8+generation);
    if(!generation){
        roots.phase^=UINT64_C(0x1000);
        RememberedInventoryRequest flip={.phase=roots.phase};
        uint64_t result=gc_remembered_inventory_step_locked(&remembered,&flip,GC_RI_BEGIN);
        if(result!=GC_RS_OK)return result==GC_RS_WAIT?GC_CY_WAIT:GC_CY_INVALID;
    }
    RootAudit audit={.queue=(uintptr_t)&queue};
    MarkPhaseControl control={.handshake=(uintptr_t)&handshake,.audit=(uintptr_t)&audit,
        .generation=generation,.remembered=(uintptr_t)&remembered};
    CycleTicket ticket={.kind=generation};CycleMarkStart request={(uintptr_t)&roots,(uintptr_t)&control};
    uint64_t result=start(&ledger,&ticket,&request);if(result!=GC_CY_OK)return result;
    runtime->cycles=ledger;runtime->roots.phase=roots.phase;runtime->handshake=handshake;
    runtime->remembered=remembered;work->queues[generation].epoch=queue.epoch;
    ticket.ledger=(uintptr_t)&runtime->cycles;cycle->ticket=ticket;
    cycle->marking_audit=(RootAudit){.queue=(uintptr_t)&work->queues[generation]};
    cycle->retirement_audit=(RootAudit){.queue=(uintptr_t)&work->queues[generation]};
    control.handshake=(uintptr_t)&runtime->handshake;control.audit=(uintptr_t)&cycle->marking_audit;
    control.remembered=(uintptr_t)&runtime->remembered;cycle->marking=control;
    cycle->moving=cycle->retiring=(RelocationPhaseControl){.handshake=(uintptr_t)&runtime->handshake,
        .gate=(uintptr_t)&cycle->gate,.generation=generation};
    return GC_CY_OK;
}
static inline uint64_t gc_native_cycle_mark_locked(NativeRuntimeOwner *runtime,uint64_t generation,uint64_t op){
    if(!gc_native_cycle_valid(runtime,generation)||op==GC_MP_START)return GC_MP_INVALID;
    return gc_mark_phase_step_locked(&runtime->roots,&runtime->cycle[generation].marking,op);
}
static inline uint64_t gc_native_cycle_move_locked(NativeRuntimeOwner *runtime,uint64_t generation,uint64_t op){
    if(!gc_native_cycle_valid(runtime,generation))return GC_RP_INVALID;
    return gc_relocation_phase_step_locked(&runtime->roots,&runtime->cycle[generation].moving,op);
}
static inline uint64_t gc_native_cycle_retire_locked(NativeRuntimeOwner *runtime,uint64_t generation,uint64_t op){
    if(!gc_native_cycle_valid(runtime,generation))return GC_RP_INVALID;
    return gc_relocation_retire_step_locked(&runtime->roots,&runtime->cycle[generation].retiring,op);
}
enum { GC_NC_BEGIN=0,GC_NC_DECIDE=1,GC_NC_RELEASE=2 };
enum { GC_NC_ADVANCED=0,GC_NC_WAIT=1,GC_NC_INVALID=2,GC_NC_EXHAUSTED=3,GC_NC_REAUDIT=4,GC_NC_COMPLETE=5 };
/* The caller's full-cycle proof precedes BEGIN. Concurrent retirement root
 * audit precedes DECIDE; it is checked again with all owners acknowledged.
 * Invalidated/incomplete work releases the pause and requests re-audit, never
 * drains a graph while stopped. Success changes Retire -> Idle then requires
 * a second adoption rendezvous before satisfying tickets. */
static inline uint64_t gc_native_cycle_complete_locked(NativeRuntimeOwner *runtime,uint64_t generation,uint64_t op){
    if(!gc_native_cycle_valid(runtime,generation)||op>GC_NC_RELEASE)return GC_NC_INVALID;
    NativeCycleState *cycle=&runtime->cycle[generation];HandshakeState *handshake=&runtime->handshake;
    if(cycle->retiring.state!=2||gc_relocation_gate_locked(&cycle->gate,NULL,GC_RG_CHECK)!=GC_RG_OK)return GC_NC_INVALID;
    CycleLedger ledger=runtime->cycles;CycleTicket ticket=cycle->ticket;
    if(ticket.ledger!=(uintptr_t)&runtime->cycles)return GC_NC_INVALID;
    ticket.ledger=(uintptr_t)&ledger;
    if(gc_cycle_step_locked(&ledger,&ticket,GC_CY_FINISH)!=GC_CY_OK)return GC_NC_INVALID;
    uint64_t phase=(runtime->roots.phase>>(generation*4))&15;
    if(op==GC_NC_BEGIN){
        if(cycle->completing||phase!=GC_RETIRE)return GC_NC_INVALID;
        if(handshake->active)return GC_NC_WAIT;
        if(handshake->generation>UINT64_MAX-2||(runtime->roots.phase>>16)==(UINT64_MAX>>16))return GC_NC_EXHAUSTED;
        if(gc_handshake_step_locked(handshake,NULL,GC_HS_BEGIN)!=GC_HS_OK)return GC_NC_INVALID;
        cycle->completing=1;cycle->finish_phase=runtime->roots.phase;cycle->finish_rendezvous=handshake->generation;
        return GC_NC_ADVANCED;
    }
    if(cycle->completing!=(op==GC_NC_DECIDE?1u:2u)||!handshake->active||
       cycle->finish_phase!=runtime->roots.phase||cycle->finish_rendezvous!=handshake->generation||
       phase!=(op==GC_NC_DECIDE?GC_RETIRE:GC_IDLE))return GC_NC_INVALID;
    if(handshake->acknowledged!=handshake->expected)return GC_NC_WAIT;
    if(op==GC_NC_RELEASE){
        if(gc_handshake_step_locked(handshake,NULL,GC_HS_END)!=GC_HS_OK)return GC_NC_INVALID;
        runtime->cycles=ledger;cycle->completing=cycle->finish_phase=cycle->finish_rendezvous=0;
        return GC_NC_COMPLETE;
    }
    MarkEndRequest check={(uintptr_t)handshake,(uintptr_t)&cycle->retirement_audit,handshake->generation,(uintptr_t)&runtime->remembered};
    uint64_t status=gc_retire_inventory_check_locked(&runtime->roots,&check,0);
    if(status==GC_ME_INVALID)return GC_NC_INVALID;
    if(status!=GC_ME_READY){
        (void)gc_handshake_step_locked(handshake,NULL,GC_HS_END);
        cycle->completing=cycle->finish_phase=cycle->finish_rendezvous=0;return GC_NC_REAUDIT;
    }
    (void)gc_handshake_step_locked(handshake,NULL,GC_HS_END);
    (void)gc_handshake_step_locked(handshake,NULL,GC_HS_BEGIN);
    runtime->roots.phase=(runtime->roots.phase+UINT64_C(0x10000))&~(UINT64_C(15)<<(generation*4));
    cycle->completing=2;cycle->finish_phase=runtime->roots.phase;cycle->finish_rendezvous=handshake->generation;
    return GC_NC_ADVANCED;
}
#endif
