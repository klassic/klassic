#ifndef KLASSIC_GC_NATIVE_ROOT_PUBLISH_CASES_H
#define KLASSIC_GC_NATIVE_ROOT_PUBLISH_CASES_H
#include "gc_native_roots.h"
#include "gc_worker.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
static void gc_run_native_root_publish_cases(NativeRootPublisher candidate,const NativeMetadataOps *ops){
    for(unsigned impl=0;impl<2;impl++)for(unsigned fault=0;fault<36;fault++){
        NativeRootPublisher publish=impl?candidate:gc_native_roots_publish_locked;
        RootRegistry registry={.phase=0x10000};HandshakeState hs={.last_identity=16};
        MutatorContext ctx={.abi_version=GC_ABI_VERSION};ThreadRegistration participant={.context=(uintptr_t)&ctx};WorkerRequest worker={(uintptr_t)&hs,(uintptr_t)&participant};
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_ATTACH)==GC_WK_OK&&ctx.thread_id==17);
        uint64_t bytes=0;assert(gc_native_roots_size(3,7,ops->quantum,&bytes)==GC_NATIVE_METADATA_OK);
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage),.metadata_max=sizeof(AllocationPage)+bytes};
        MetadataCharge charge={.bytes=bytes};assert(gc_budget_step_locked(&heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
        assert(gc_native_roots_prepare(&charge,17,3,7,ops,gc_root_storage_prepare)==GC_NATIVE_METADATA_OK);
        assert(gc_budget_step_locked(&heap,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
        RootStorage *s=(void *)(uintptr_t)charge.allocation;void *original=malloc(bytes),*before=malloc(bytes);assert(original&&before);memcpy(original,s,bytes);
        MetadataCharge committed=charge;MutatorContext initial_ctx=ctx;RootRegistry initial_registry=registry;
        const MetadataCharge *q=&charge;MutatorContext *owner=&ctx;RootRegistry *g=&registry;
        switch(fault){
        case 0:break;case 1:charge.stage=0;break;case 2:charge.stage=1;break;case 3:charge.stage=3;break;
        case 4:charge.owner=0;break;case 5:charge.owner++;break;case 6:charge.allocation=0;break;case 7:charge.allocation++;break;
        case 8:charge.allocation=UINT64_MAX-7;break;case 9:charge.bytes=sizeof(*s)-1;break;case 10:charge.bytes=sizeof(*s)+3*sizeof(RootCell)+7*sizeof(RootChange)-1;break;
        case 11:s->registration.segment=0;break;case 12:s->segment.cells+=8;break;case 13:s->segment.journal=0;break;
        case 14:s->segment.capacity=0;break;case 15:s->segment.capacity=UINT64_C(1)<<32;break;case 16:s->journal.capacity=0;break;
        case 17:s->journal.capacity=UINT64_C(1)<<32;break;case 18:s->journal.entries+=8;break;case 19:s->journal.top=1;break;
        case 20:atomic_store(&s->journal.published,1);break;case 21:atomic_store(&s->journal.consumed,1);break;
        case 22:s->segment.owner++;break;case 23:atomic_store(&s->segment.used,1);break;case 24:atomic_store(&s->segment.state,GC_ROOT_SEGMENT_ACTIVE);break;
        case 25:ctx.abi_version++;break;case 26:ctx.thread_id++;break;case 27:atomic_store(&ctx.state,GC_BLOCKED_NATIVE);break;
        case 28:atomic_store(&ctx.access_epoch,27);break;case 29:atomic_store(&ctx.adopted_phase,0x20000);break;
        case 30:q=NULL;break;case 31:g=(void *)((char *)&registry+1);break;case 32:registry.last_scope=UINT64_MAX;break;
        case 33:registry.active=UINT64_MAX;break;case 34:ctx.root_segments=1;break;case 35:owner=NULL;break;
        }
        MetadataCharge saved_charge=charge;MutatorContext saved_ctx=ctx;RootRegistry saved_registry=registry;HeapPool saved_heap=heap;memcpy(before,s,bytes);
        assert(publish(g,owner,q)==(fault==0?GC_RL_OK:fault==32||fault==33?GC_RL_EXHAUSTED:GC_RL_INVALID));
        assert(!memcmp(&charge,&saved_charge,sizeof(charge))&&!memcmp(&heap,&saved_heap,sizeof(heap)));
        if(fault){
            assert(!memcmp(s,before,bytes)&&!memcmp(&ctx,&saved_ctx,sizeof(ctx))&&!memcmp(&registry,&saved_registry,sizeof(registry)));
            charge=committed;ctx=initial_ctx;registry=initial_registry;memcpy(s,original,bytes);
        }else{
            assert(registry.active==1&&registry.inventory==(uintptr_t)&s->registration&&ctx.root_segments==(uintptr_t)&s->registration);
            assert(s->segment.scope==1&&s->segment.registration==(uintptr_t)&s->registration);
            memcpy(before,s,bytes);saved_ctx=ctx;saved_registry=registry;
            assert(publish(&registry,&ctx,&charge)==GC_RL_INVALID&&!memcmp(s,before,bytes)&&!memcmp(&ctx,&saved_ctx,sizeof(ctx))&&!memcmp(&registry,&saved_registry,sizeof(registry)));
            assert(gc_worker_step_locked(&registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
            assert(gc_native_roots_dispose(&charge,ops,gc_root_storage_can_dispose)==GC_NATIVE_METADATA_INVALID&&charge.allocation==committed.allocation);
            assert(gc_worker_step_locked(&registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
            RootLease reader={0};RootLifeRequest life={.registration=(uintptr_t)&s->registration,.lease=(uintptr_t)&reader,.scope=s->segment.scope,.context=(uintptr_t)&ctx};
            assert(gc_root_life_locked(&registry,&life,GC_RL_ACQUIRE)==GC_RL_OK);
            assert(gc_root_life_locked(&registry,&life,GC_RL_RETIRE)==GC_RL_OK);
            RootLease cleaner={0};RootLifeRequest clean={.registration=life.registration,.lease=(uintptr_t)&cleaner,.scope=life.scope};
            assert(gc_root_life_locked(&registry,&clean,GC_RL_CLEAN)==GC_RL_WAIT);
            assert(gc_root_life_locked(&registry,&life,GC_RL_RELEASE)==GC_RL_OK);
            assert(gc_root_life_locked(&registry,&clean,GC_RL_CLEAN)==GC_RL_OK&&gc_root_clear_chunk(&clean)==GC_RL_OK);
            assert(gc_root_life_locked(&registry,&clean,GC_RL_FINISH)==GC_RL_OK);
            assert(!registry.active&&!registry.retiring&&!registry.leases&&!registry.inventory&&!ctx.root_segments&&!ctx.retiring_roots);
        }
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
        assert(gc_native_roots_dispose(&charge,ops,gc_root_storage_can_dispose)==GC_NATIVE_METADATA_OK);
        assert(gc_budget_step_locked(&heap,&charge,GC_BUDGET_RELEASE)==GC_BUDGET_OK&&heap.metadata_committed==sizeof(AllocationPage));
        assert(gc_worker_step_locked(&registry,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
        atomic_store(&ctx.state,GC_DETACHING);assert(gc_handshake_step_locked(&hs,&participant,GC_HS_DETACH)==GC_HS_OK&&!hs.members);
        free(original);free(before);
    }
    puts("native root publication: committed mapping to registered scope, rejection preservation and leased retirement PASS");
}
#endif
