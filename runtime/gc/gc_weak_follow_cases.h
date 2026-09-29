#ifndef KLASSIC_GC_WEAK_FOLLOW_CASES_H
#define KLASSIC_GC_WEAK_FOLLOW_CASES_H
#include "gc_weak_follow.h"
#include "gc_weak_scan_cases.h"
static void gc_run_weak_follow_cases(WeakTableFollower candidate){
    for(unsigned impl=0;impl<2;impl++)for(unsigned generation=0;generation<2;generation++)for(unsigned mode=0;mode<16;mode++){
        WeakTableFollower step=impl?candidate:gc_weak_follow_step_locked;
        WeakScanFixture f;weak_scan_fixture(&f,generation,true);
        WeakFollowRequest q={.index=1,.serial=11};uint64_t expected=GC_RF_INVALID,op=0;
        MetadataLeasePool *pool=&f.r.weak.f.pool;ReferenceLease pressure[6]={0};
        WeakTableOps release={.release=gc_weak_release_locked};
        switch(mode){
        case 0:expected=GC_RF_ADVANCED;break;
        case 1:atomic_store(&f.forward[0].word,0);expected=GC_RF_DONE;break;
        case 2:atomic_store(&f.forward[0].word,3);expected=GC_RF_DONE;break;
        case 3:atomic_store(&f.forward[0].word,5);expected=GC_RF_WAIT;break;
        case 4:q.index=0;break;
        case 5:q.serial=10;break;
        case 6:f.entries[0].slot.gate+=8;break;
        case 7:f.entries[0].slot.lease.token+=UINT64_C(1)<<32;break;
        case 8:q.retired.token=1;expected=GC_RF_CLEANUP_REQUIRED;break;
        case 9:f.r.weak.gate.closed=2;break;
        case 10:op=1;break;
        case 11:q.index=3;q.serial=13;expected=GC_RF_DONE;break;
        case 12:q.index=3;q.serial=13;f.entries[2].slot.lease.token=1;break;
        case 13:
            for(unsigned i=0;i<6;i++){pressure[i].reference=f.live;assert(gc_reference_lease_locked(pool,&pressure[i])==GC_ML_OK);}
            expected=GC_RF_METADATA_FULL;break;
        case 14:f.leases[pool->free_head-1].generation=UINT32_MAX;expected=GC_RF_EXHAUSTED;break;
        case 15:{WeakHandle h={(uintptr_t)&f.table,1,11};assert(gc_weak_table_release_locked(&f.table,h,&release)==GC_WEAK_RELEASED);break;}
        }
        WeakScanFixture before;memcpy(&before,&f,sizeof(f));WeakFollowRequest old=q;
        assert(step(&f.table,&q,op)==expected);
        assert(!memcmp(&q,&old,sizeof(q)));
        if(expected!=GC_RF_ADVANCED)assert(!memcmp(&f,&before,sizeof(f)));
        else{
            assert(f.entries[0].slot.reference==f.live&&f.entries[0].slot.serial==11&&!q.retired.token);
            assert(f.entries[0].slot.lease.token!=before.entries[0].slot.lease.token&&pool->active==2);
            assert(!memcmp(&f.r.phase,&before.r.phase,sizeof(f.r.phase)));
        }
        if(mode==6)f.entries[0].slot.gate-=8;
        if(mode==7)f.entries[0].slot.lease.token-=UINT64_C(1)<<32;
        if(mode==9)f.r.weak.gate.closed=1;
        if(mode==12)f.entries[2].slot.lease.token=0;
        if(mode==13){
            for(unsigned i=0;i<6;i++)assert(gc_metadata_step_locked(pool,&pressure[i].lease,GC_ML_RELEASE)==GC_ML_OK);
            assert(step(&f.table,&q,0)==GC_RF_ADVANCED&&f.entries[0].slot.reference==f.live);
        }
        for(unsigned i=0;i<4;i++)if(f.entries[i].slot.serial){
            WeakHandle h={(uintptr_t)&f.table,i+1,f.entries[i].slot.serial};
            assert(gc_weak_table_release_locked(&f.table,h,&release)==GC_WEAK_RELEASED);
        }
        assert(!pool->active&&!f.r.weak.f.page.metadata_leases);free(f.heap);
    }
    puts("weak follow: 64 C/emitted slot-transfer, pressure, cleanup and failure-preservation cases PASS");
}
#endif
