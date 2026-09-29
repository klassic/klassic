#ifndef KLASSIC_GC_NATIVE_CYCLE_CASES_H
#define KLASSIC_GC_NATIVE_CYCLE_CASES_H
#include "gc_native_cycle.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void gc_nc_poll(NativeRuntimeOwner *r,bool parked){
    assert(gc_worker_step_locked(&r->roots,&r->worker,GC_WK_POLL)==(parked?GC_WK_PARK:GC_WK_OK));
}
/* An actual attached, empty root segment. No objects, weak/native handles,
 * incoming references or relocation candidates exist in this fixture. Empty
 * graph proofs here do not stand in for production graph service. */
static void gc_nc_audit(NativeRuntimeOwner *r,unsigned gen,bool retire){
    RootAudit *a=retire?&r->cycle[gen].retirement_audit:&r->cycle[gen].marking_audit;
    uint64_t (*step)(RootRegistry *,RootAudit *,uint64_t)=retire?gc_root_retire_audit_step_locked:gc_root_audit_step_locked;
    uint64_t status=step(&r->roots,a,GC_RA_BEGIN);
    assert(status==GC_RA_ITEM&&a->walk.limit==0);
    uint64_t value=UINT64_MAX;
    RootWalkScan scan={(uintptr_t)&a->walk,a->walk.lease.serial,(uintptr_t)&value,1,0};
    assert(gc_root_walk_scan(&scan)==GC_ROOT_OK&&scan.written==0);
    a->walk.completed_mask=a->walk.mask;
    assert(step(&r->roots,a,GC_RA_NEXT)==GC_RA_DONE);
    assert(!r->roots.leases&&!r->roots.walkers);
}
static void gc_nc_start(NativeRuntimeOwner *r,unsigned gen,CycleMarkStarter start){
    assert(gc_native_cycle_begin_locked(r,gen,start)==GC_CY_OK);
    assert(gc_native_cycle_mark_locked(r,gen,GC_MP_ACTIVATE)==GC_MP_WAIT);
    gc_nc_poll(r,true);
    assert(gc_native_cycle_mark_locked(r,gen,GC_MP_ACTIVATE)==GC_MP_ADVANCED);
    assert(gc_native_cycle_mark_locked(r,gen,GC_MP_RELEASE)==GC_MP_WAIT);
    gc_nc_poll(r,true);
    assert(gc_native_cycle_mark_locked(r,gen,GC_MP_RELEASE)==GC_MP_MARKING);
    gc_nc_poll(r,false);
}
static void gc_nc_finish(NativeRuntimeOwner *r,unsigned gen,CycleTicket *ticket,bool stale){
    gc_nc_audit(r,gen,false);
    assert(gc_native_cycle_mark_locked(r,gen,GC_MP_BEGIN)==GC_MP_ADVANCED);
    gc_nc_poll(r,true);
    assert(gc_native_cycle_mark_locked(r,gen,GC_MP_DECIDE)==GC_MP_ADVANCED);
    gc_nc_poll(r,true);
    assert(gc_native_cycle_mark_locked(r,gen,GC_MP_RELEASE)==GC_MP_SELECT);
    gc_nc_poll(r,false);
    assert(gc_cycle_step_locked(&r->cycles,ticket,GC_CY_POLL)==GC_CY_WAIT);
    assert(gc_native_cycle_move_locked(r,gen,GC_RP_BEGIN)==GC_RP_ADVANCED);
    gc_nc_poll(r,true);
    assert(gc_native_cycle_move_locked(r,gen,GC_RP_ACTIVATE)==GC_RP_ADVANCED);
    gc_nc_poll(r,true);
    assert(gc_native_cycle_move_locked(r,gen,GC_RP_RELEASE)==GC_RP_RELOCATING);
    gc_nc_poll(r,false);
    assert(gc_relocation_gate_locked(&r->cycle[gen].gate,NULL,GC_RG_CLOSE)==GC_RG_OK);
    assert(gc_native_cycle_retire_locked(r,gen,GC_RR_BEGIN)==GC_RP_ADVANCED);
    gc_nc_poll(r,true);
    assert(gc_native_cycle_retire_locked(r,gen,GC_RR_RELEASE)==GC_RP_RETIRING);
    gc_nc_poll(r,false);
    gc_nc_audit(r,gen,true);
    if(stale){
        /* A newer audit invalidates the captured owner certificate. */
        RootAudit replacement={.queue=r->cycle[gen].retirement_audit.queue};
        assert(gc_root_retire_audit_step_locked(&r->roots,&replacement,GC_RA_BEGIN)==GC_RA_ITEM);
        replacement.walk.completed_mask=replacement.walk.mask;
        assert(gc_root_retire_audit_step_locked(&r->roots,&replacement,GC_RA_NEXT)==GC_RA_DONE);
        assert(gc_native_cycle_complete_locked(r,gen,GC_NC_BEGIN)==GC_NC_ADVANCED);
        gc_nc_poll(r,true);
        assert(gc_native_cycle_complete_locked(r,gen,GC_NC_DECIDE)==GC_NC_REAUDIT);
        assert(!r->handshake.active&&!r->cycle[gen].completing);
        gc_nc_poll(r,false);
        assert(gc_cycle_step_locked(&r->cycles,ticket,GC_CY_POLL)==GC_CY_WAIT);
        gc_nc_audit(r,gen,true);
    }
    assert(gc_native_cycle_complete_locked(r,gen,GC_NC_BEGIN)==GC_NC_ADVANCED);
    assert(gc_native_cycle_complete_locked(r,gen,GC_NC_DECIDE)==GC_NC_WAIT);
    gc_nc_poll(r,true);
    assert(gc_native_cycle_complete_locked(r,gen,GC_NC_DECIDE)==GC_NC_ADVANCED);
    assert(gc_cycle_step_locked(&r->cycles,ticket,GC_CY_POLL)==GC_CY_WAIT);
    assert(gc_native_cycle_complete_locked(r,gen,GC_NC_RELEASE)==GC_NC_WAIT);
    gc_nc_poll(r,true);
    assert(gc_native_cycle_complete_locked(r,gen,GC_NC_RELEASE)==GC_NC_COMPLETE);
    gc_nc_poll(r,false);
    assert(gc_cycle_step_locked(&r->cycles,ticket,GC_CY_POLL)==GC_CY_OK);
}
static void gc_run_native_cycle_cases(CycleMarkStarter start){
    NativeRuntimeOwner r={0};NativeHeapOps ops=gc_native_heap_ops();
    NativeRuntimeConfig config={{4*GC_HEAP_PAGE_BYTES,2*GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES,64*ops.metadata.quantum},4,4,64,{8,8,8}};
    assert(gc_native_runtime_prepare(&r,&config,&ops)==GC_NATIVE_METADATA_OK);
    assert(gc_native_runtime_activate(&r)==GC_NATIVE_METADATA_OK);
    NativeWorkStorage *work=gc_native_runtime_work(&r);
    for(unsigned gen=0;gen<2;gen++){
        NativeRuntimeOwner before=r;NativeWorkStorage wb=*work;
        assert(gc_native_cycle_begin_locked(&r,gen,start)==GC_CY_EMPTY);
        assert(!memcmp(&before,&r,sizeof(r))&&!memcmp(&wb,work,sizeof(wb)));
    }
    CycleTicket old={.kind=1},young={0},next={0};
    assert(gc_native_cycle_request_locked(&r,&old)==GC_CY_OK);
    assert(gc_native_cycle_request_locked(&r,&young)==GC_CY_OK);
    for(unsigned fault=0;fault<5;fault++){
        uint64_t epoch=work->queues[0].epoch,phase=r.roots.phase,hgen=r.handshake.generation;
        if(fault==0)work->queues[0].epoch=UINT64_MAX;
        if(fault==1)r.roots.phase=UINT64_MAX&~UINT64_C(0xffff);
        if(fault==2)r.handshake.generation=UINT64_MAX;
        if(fault==3)r.remembered.walkers=1;
        if(fault==4)r.cycle[0].gate.closed=0;
        NativeRuntimeOwner before=r;NativeWorkStorage wb=*work;
        assert(gc_native_cycle_begin_locked(&r,0,start)==(fault<3?GC_CY_EXHAUSTED:fault==3?GC_CY_WAIT:GC_CY_INVALID));
        assert(!memcmp(&before,&r,sizeof(r))&&!memcmp(&wb,work,sizeof(wb)));
        work->queues[0].epoch=epoch;r.roots.phase=phase;r.handshake.generation=hgen;
        r.remembered.walkers=0;r.cycle[0].gate.closed=1;
    }
    gc_nc_start(&r,1,start);
    gc_nc_start(&r,0,start);
    assert((r.roots.phase&255)==0x22);
    assert(gc_native_cycle_request_locked(&r,&next)==GC_CY_OK&&next.cycle==2);
    assert(gc_native_runtime_dispose(&r,&ops)==GC_NATIVE_METADATA_INVALID&&r.stage==3);
    gc_nc_finish(&r,0,&young,true);
    assert((r.roots.phase&255)==0x20);
    assert(gc_cycle_step_locked(&r.cycles,&next,GC_CY_POLL)==GC_CY_WAIT);
    gc_nc_start(&r,0,start);gc_nc_finish(&r,0,&next,false);
    gc_nc_finish(&r,1,&old,true);
    assert(!(r.roots.phase&255)&&r.cycles.completed[0]==2&&r.cycles.completed[1]==1);
    RootStorage *storage=(void *)(uintptr_t)r.root_charge.allocation;
    RootLease lease={0};
    RootLifeRequest q={.registration=(uintptr_t)&storage->registration,.scope=storage->segment.scope,.context=(uintptr_t)&r.context,.lease=(uintptr_t)&lease};
    assert(gc_root_life_locked(&r.roots,&q,GC_RL_RETIRE)==GC_RL_OK);
    assert(gc_root_life_locked(&r.roots,&q,GC_RL_CLEAN)==GC_RL_OK);
    assert(gc_root_clear_chunk(&q)==GC_RL_OK);
    assert(gc_root_life_locked(&r.roots,&q,GC_RL_FINISH)==GC_RL_OK);
    assert(gc_native_runtime_dispose(&r,&ops)==GC_NATIVE_METADATA_OK);
    puts("native cycle controls: overlapping mark lanes, deferred tickets and retirement re-audit through final adoption PASS");
}
#endif
