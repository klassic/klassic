#ifndef KLASSIC_GC_NATIVE_RUNTIME_CASES_H
#define KLASSIC_GC_NATIVE_RUNTIME_CASES_H
#include "gc_native_runtime.h"
#include "gc_buffer_job.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static unsigned gc_runtime_maps,gc_runtime_unmaps,gc_runtime_fail_map,gc_runtime_fail_unmap;
static void *gc_runtime_map(size_t bytes){
    if(++gc_runtime_maps==gc_runtime_fail_map)return NULL;
    return gc_native_metadata_map(bytes);
}
static bool gc_runtime_unmap(void *address,size_t bytes){
    if(++gc_runtime_unmaps==gc_runtime_fail_unmap)return false;
    return gc_native_metadata_unmap(address,bytes);
}
static void *gc_runtime_reserve_fail(size_t bytes){(void)bytes;return NULL;}
static bool gc_runtime_release_fail(void *p,size_t bytes){(void)p;(void)bytes;return false;}
static void gc_run_native_runtime_cases(void){
    NativeWorkConfig invalid[]={{0,1,1},{1,0,1},{1,1,0},{UINT64_MAX,1,1},{1,UINT64_MAX,1},{1,1,UINT64_MAX}};
    for(unsigned i=0;i<sizeof(invalid)/sizeof(*invalid);i++){
        uint64_t bytes=123;
        assert(gc_native_work_size(&invalid[i],4096,&bytes)==GC_NATIVE_METADATA_INVALID&&bytes==123);
    }
    for(unsigned mode=0;mode<13;mode++){
        NativeRuntimeOwner owner={0};NativeHeapOps ops=gc_native_heap_ops(),injected=ops;
        injected.metadata.map=gc_runtime_map;injected.metadata.unmap=gc_runtime_unmap;
        NativeRuntimeConfig config={{4*GC_HEAP_PAGE_BYTES,2*GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES,32*ops.metadata.quantum},3,4,64,{8,8,8}};
        gc_runtime_maps=gc_runtime_unmaps=gc_runtime_fail_map=gc_runtime_fail_unmap=0;
        if(mode==0)injected.reserve=gc_runtime_reserve_fail;
        if(mode>=1&&mode<=3)gc_runtime_fail_map=mode;
        if(mode==4)config.heap.metadata_max=ops.metadata.quantum;
        if(mode==10)gc_runtime_fail_map=4;
        if(mode==11)config.heap.metadata_max=3*ops.metadata.quantum;
        uint64_t status=gc_native_runtime_prepare(&owner,&config,&injected);
        assert(status==(mode<4||mode==10?GC_NATIVE_METADATA_OS_ERROR:mode==4||mode==11?GC_NATIVE_RUNTIME_LIMIT:GC_NATIVE_METADATA_OK));
        if(mode==11)assert(gc_runtime_maps==1); /* all work admission precedes package mapping */
        NativeRuntimeOwner copy=owner;assert(!gc_native_runtime_valid(&copy));
        assert(gc_native_runtime_prepare(&owner,&config,&injected)==GC_NATIVE_METADATA_INVALID);
        if((mode>=5&&mode<=9)||mode==12){
            assert(owner.stage==2&&owner.participant.registered&&owner.context.thread_id);
            assert(atomic_load(&owner.context.state)==GC_BLOCKED_NATIVE&&!owner.context.root_segments&&!owner.roots.active);
            assert(owner.root_charge.stage==2&&owner.descriptor_charge.stage==2&&owner.work_charge.stage==2);
            assert(owner.backing.heap.metadata_committed==owner.backing.table_bytes+owner.root_charge.bytes+owner.descriptor_charge.bytes+owner.work_charge.bytes);
            NativeWorkStorage *work=gc_native_runtime_work(&owner);assert(work);
            assert(work->leases.capacity==8&&work->leases.free_head==1&&!work->leases.active);
            assert(owner.context.satb.entries==work->satb&&owner.context.remembered.entries==work->remembered);
            assert(gc_native_work_quiescent(work));
            work->leases.active=1;
            assert(gc_native_runtime_dispose(&owner,&injected)==GC_NATIVE_METADATA_INVALID&&owner.stage==2);
            work->leases.active=0;
            owner.backing.heap.metadata_pending++;
            assert(gc_native_runtime_dispose(&owner,&injected)==GC_NATIVE_METADATA_INVALID&&owner.stage==2);
            owner.backing.heap.metadata_pending--;
            owner.context.satb.top=1;atomic_store(&owner.context.satb.published,1);
            assert(gc_native_runtime_dispose(&owner,&injected)==GC_NATIVE_METADATA_INVALID&&owner.stage==2);
            owner.context.satb.top=0;atomic_store(&owner.context.satb.published,0);
            owner.context.remembered_consumer.end=1;
            assert(gc_native_runtime_dispose(&owner,&injected)==GC_NATIVE_METADATA_INVALID&&owner.stage==2);
            owner.context.remembered_consumer.end=0;
            if(mode==9){
                assert(gc_native_runtime_activate(&owner)==GC_NATIVE_METADATA_OK);
                assert(owner.stage==3&&atomic_load(&owner.context.state)==GC_RUNNING&&owner.roots.active==1);
                assert(gc_native_runtime_activate(&owner)==GC_NATIVE_METADATA_INVALID);
                unsigned unmaps=gc_runtime_unmaps;
                assert(gc_native_runtime_dispose(&owner,&injected)==GC_NATIVE_METADATA_INVALID&&gc_runtime_unmaps==unmaps);
                RootStorage *storage=(void *)(uintptr_t)owner.root_charge.allocation;
                RootRequest root={.segment=(uintptr_t)&storage->segment};
                assert(gc_root_step(&owner.context,&root,GC_ROOT_RESERVE)==GC_ROOT_OK);
                RootLifeRequest retire={.registration=(uintptr_t)&storage->registration,.scope=storage->segment.scope,.context=(uintptr_t)&owner.context};
                assert(gc_root_life_locked(&owner.roots,&retire,GC_RL_RETIRE)==GC_RL_OK);
                RootLease lease={0};RootLifeRequest clean={.registration=retire.registration,.scope=retire.scope,.lease=(uintptr_t)&lease};
                assert(gc_root_life_locked(&owner.roots,&clean,GC_RL_CLEAN)==GC_RL_WAIT);
                assert(gc_root_life_locked(&owner.roots,&clean,GC_RL_CLAIM)==GC_RL_OK);
                assert(clean.mask==0&&clean.begin==0&&clean.end==1);
                uint64_t value=UINT64_MAX;
                RootScanJob scan={.scan={.segment=root.segment,.cursor=clean.begin,.capacity=1,.output=(uintptr_t)&value},
                    .lease=(uintptr_t)&lease,.serial=clean.serial};
                assert(gc_root_scan_leased(&scan)==GC_ROOT_OK);
                assert(value==0);
                assert(gc_root_life_locked(&owner.roots,&clean,GC_RL_ACK)==GC_RL_OK);
                assert(gc_root_life_locked(&owner.roots,&clean,GC_RL_CLEAN)==GC_RL_OK);
                assert(gc_root_clear_chunk(&clean)==GC_RL_OK);
                assert(gc_root_life_locked(&owner.roots,&clean,GC_RL_FINISH)==GC_RL_OK);
                /* Transport boundary checks, with empty entries carrying no
                 * managed references or lease obligations. Both real rings
                 * fill, publish, claim and ACK before their backing can die. */
                for(unsigned selector=0;selector<2;selector++){
                    BarrierEntry empty={0},out[8];
                    for(unsigned i=0;i<8;i++)assert(gc_barrier_buffer_step(&owner.context,&empty,selector)==GC_BB_OK);
                    assert(gc_barrier_buffer_step(&owner.context,&empty,selector)==GC_BB_FULL);
                    assert(gc_native_runtime_dispose(&owner,&injected)==GC_NATIVE_METADATA_INVALID&&owner.stage==3);
                    assert(gc_barrier_buffer_step(&owner.context,NULL,selector+2)==GC_BB_OK);
                    BarrierJob job={.identity=owner.context.thread_id,.selector=selector,.output=(uintptr_t)out,.capacity=8};
                    assert(gc_buffer_job_step(&owner.context,&job,GC_BJ_CLAIM)==GC_BJ_OK);
                    assert(gc_buffer_job_step(&owner.context,&job,GC_BJ_ACK)==GC_BJ_WAIT);
                    assert(gc_buffer_job_step(&owner.context,&job,GC_BJ_COPY)==GC_BJ_OK&&job.written==8);
                    assert(gc_native_runtime_dispose(&owner,&injected)==GC_NATIVE_METADATA_INVALID&&owner.stage==3);
                    assert(gc_buffer_job_step(&owner.context,&job,GC_BJ_ACK)==GC_BJ_OK);
                }
            }
        }
        if(mode>=5&&mode<=7)gc_runtime_fail_unmap=mode==7?4:mode-4;
        if(mode==12)gc_runtime_fail_unmap=3;
        if(mode==8)injected.release=gc_runtime_release_fail;
        status=gc_native_runtime_dispose(&owner,&injected);
        if((mode>=5&&mode<=8)||mode==12){
            assert(status==GC_NATIVE_METADATA_OS_ERROR&&owner.stage==4);
            assert(gc_native_runtime_activate(&owner)==GC_NATIVE_METADATA_INVALID);
            assert(!gc_native_runtime_work(&owner));
            gc_runtime_fail_unmap=0;injected.release=ops.release;
            assert(gc_native_runtime_dispose(&owner,&injected)==GC_NATIVE_METADATA_OK);
        }else assert(status==GC_NATIVE_METADATA_OK);
        assert(owner.stage==5&&!owner.root_charge.allocation&&!owner.descriptor_charge.allocation&&!owner.work_charge.allocation);
        assert(!owner.context.satb.entries&&!owner.context.remembered.entries);
        assert(!owner.participant.registered&&!owner.handshake.members&&!owner.handshake.native);
        assert(!owner.backing.reservation&&!owner.backing.table&&!owner.backing.heap.metadata_committed);
        assert(gc_native_runtime_dispose(&owner,&injected)==GC_NATIVE_METADATA_INVALID);
    }
    puts("native runtime bootstrap: owned heap/context/packages, delayed activation and retryable quiescent disposal PASS");
    puts("native work bootstrap: charged SATB/remset rings, lease pool and two mark queues retain pending work through disposal PASS");
}
#endif
