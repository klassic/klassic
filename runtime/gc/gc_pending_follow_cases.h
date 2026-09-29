#ifndef KLASSIC_GC_PENDING_FOLLOW_CASES_H
#define KLASSIC_GC_PENDING_FOLLOW_CASES_H
#include "gc_pending_access.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void gc_run_pending_follow_cases(PendingFollower candidate,PendingAccessor accessor){
    for(unsigned impl=0;impl<2;impl++)for(unsigned fault=0;fault<18;fault++){
        PendingFollower follow=impl?candidate:gc_pending_follow_locked;
        PendingAccessor enter=impl?accessor:gc_pending_access_locked;
        AllocationPage pages[3]={0};ForwardTable tables[3]={0};ForwardCell cells[3]={0};
        _Atomic uint64_t starts[3][128]={0};MetadataLeaseSlot slots[2]={{.next_free=2},{0}};
        /* Unmapped managed base: every step must operate only on metadata. */
        HeapPool heap={.heap_base=UINT64_C(0x100000000),.pages=(uintptr_t)pages,.page_count=3};
        MetadataLeasePool pool={(uintptr_t)&heap,(uintptr_t)slots,2,1,0};
        uint64_t refs[3];
        for(unsigned i=0;i<3;i++){
            uint64_t start=(i+1)*GC_HEAP_PAGE_BYTES;refs[i]=((start+32)<<16)|0x1510;
            pages[i]=(AllocationPage){.heap_base=heap.heap_base,.start=start,.limit=start+GC_HEAP_PAGE_BYTES,
                .incarnation=27+i,.identity_base=1024,.state=GC_PAGE_CLOSED,.starts=(uintptr_t)starts[i],
                .starts_words=128,.allocated_top=start+48,.run_head=i,.forward_table=(uintptr_t)&tables[i]};
            atomic_store(&starts[i][0],4);tables[i]=(ForwardTable){(uintptr_t)&cells[i],1,1};
            cells[i]=(ForwardCell){.source_offset=start+32,.incarnation=27+i};
        }
        atomic_store(&cells[0].word,refs[1]|2);atomic_store(&cells[1].word,refs[2]|2);
        ReferenceLease current={.reference=refs[0]},*q=&current;MetadataLeaseRequest retired={0};
        uint64_t expected=GC_RF_ADVANCED;
        switch(fault){
        case 0:break;case 1:atomic_store(&cells[0].word,5);expected=GC_RF_WAIT;break;
        case 2:atomic_store(&cells[0].word,0);expected=GC_RF_DONE;break;
        case 3:atomic_store(&cells[0].word,3);expected=GC_RF_DONE;break;
        case 4:pool.capacity=1;slots[0].next_free=0;expected=GC_RF_METADATA_FULL;break;
        case 5:atomic_store(&starts[1][0],0);expected=GC_RF_INVALID;break;
        case 6:pages[1].incarnation=0;expected=GC_RF_INVALID;break;
        case 7:atomic_store(&pages[1].state,GC_PAGE_READY);expected=GC_RF_INVALID;break;
        case 8:pages[0].forward_table=0;expected=GC_RF_INVALID;break;
        case 9:assert(gc_reference_lease_locked(&pool,&current)==GC_ML_OK);current.lease.incarnation++;expected=GC_RF_INVALID;break;
        case 10:retired.token=1;expected=GC_RF_CLEANUP_REQUIRED;break;
        case 11:q=NULL;expected=GC_RF_INVALID;break;
        case 12:q=(void *)((char *)&current+1);expected=GC_RF_INVALID;break;
        case 13:current.reference=0;expected=GC_RF_DONE;break;
        case 14:current.reference|=1;expected=GC_RF_INVALID;break;
        case 15:slots[0].generation=UINT32_MAX;expected=GC_RF_EXHAUSTED;break;
        case 16:slots[1].generation=UINT32_MAX;expected=GC_RF_EXHAUSTED;break;
        case 17:pages[1].metadata_leases=UINT64_MAX;expected=GC_RF_EXHAUSTED;break;
        }
        uint64_t actual=follow(&pool,q,&retired);
        if(actual!=expected)fprintf(stderr,"pending follow impl=%u fault=%u expected=%llu actual=%llu\n",impl,fault,(unsigned long long)expected,(unsigned long long)actual);
        assert(actual==expected);
        if(!fault){
            assert(current.reference==refs[1]&&current.lease.page==(uintptr_t)&pages[1]&&pool.active==1);
            assert(!pages[0].metadata_leases&&pages[1].metadata_leases==1&&!retired.token);
            MutatorContext ctx={.abi_version=GC_ABI_VERSION,.thread_id=17};
            ResolveRequest access={.scope={.context=(uintptr_t)&ctx}};
            /* The middle copy starts moving AFTER a successful follow check. */
            atomic_store(&cells[1].word,0);
            assert(follow(&pool,&current,&retired)==GC_RF_DONE);
            ReferenceLease middle=current;MetadataLeasePool held=pool;
            atomic_store(&cells[1].word,refs[2]|2);
            assert(enter(&pool,&current,&access)==GC_RF_ADVANCED);
            ResolveRequest empty={.scope={.context=(uintptr_t)&ctx}};
            assert(!memcmp(&access,&empty,sizeof(access))&&!memcmp(&current,&middle,sizeof(current))&&!memcmp(&pool,&held,sizeof(pool)));
            for(unsigned bad=0;bad<6;bad++){
                if(bad==0)ctx.abi_version++;else if(bad==1)ctx.thread_id=0;
                else if(bad==2)atomic_store(&ctx.state,GC_BLOCKED_NATIVE);
                else if(bad==3)atomic_store(&ctx.access_epoch,1);
                else if(bad==4)access.reference=refs[1];else access.destination=refs[2];
                ResolveRequest before=access;
                assert(enter(&pool,&current,&access)==GC_RF_INVALID&&!memcmp(&access,&before,sizeof(access)));
                assert(!memcmp(&current,&middle,sizeof(current))&&!memcmp(&pool,&held,sizeof(pool)));
                ctx=(MutatorContext){.abi_version=GC_ABI_VERSION,.thread_id=17};access=empty;
            }
            assert(follow(&pool,&current,&retired)==GC_RF_ADVANCED&&current.reference==refs[2]);
            assert(!pages[1].metadata_leases&&pages[2].metadata_leases==1&&pool.active==1);
            ReferenceLease saved=current;
            assert(follow(&pool,&current,&retired)==GC_RF_DONE&&!memcmp(&saved,&current,sizeof(saved)));
            assert(enter(&pool,&current,&access)==GC_RAC_READY&&access.scope.held==(uintptr_t)&cells[2]);
            ResolveRequest guarded=access;
            assert(enter(&pool,&current,&access)==GC_RF_INVALID&&!memcmp(&guarded,&access,sizeof(access)));
            assert(gc_resolve_access(&pages[2],&access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK&&!atomic_load(&ctx.access_epoch));
        }else if(current.lease.token){
            assert(current.reference==refs[0]&&current.lease.page==(uintptr_t)&pages[0]);
            assert(pool.active==1&&pages[0].metadata_leases==1&&!retired.token);
            if(fault==9)current.lease.incarnation--;
            if(fault==4){
                /* Restore spare capacity; the source token stays owned through
                 * the failed destination acquisition and successful retry. */
                pool.capacity=2;pool.free_head=2;
                assert(follow(&pool,&current,&retired)==GC_RF_ADVANCED&&current.reference==refs[1]);
            }
        }else assert(!pool.active);
        if(current.lease.token)assert(gc_metadata_step_locked(&pool,&current.lease,GC_ML_RELEASE)==GC_ML_OK);
        assert(!pool.active&&!pages[0].metadata_leases);
    }
    puts("pending access: forwarding after follow, scratch discard, guard ownership and unchanged rejection PASS");
    puts("pending reference follow: bounded two-hop metadata handoff, retained pressure and balanced cleanup PASS");
}
#endif
