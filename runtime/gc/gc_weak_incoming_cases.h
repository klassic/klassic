#ifndef KLASSIC_GC_WEAK_INCOMING_CASES_H
#define KLASSIC_GC_WEAK_INCOMING_CASES_H
#include "gc_weak_incoming.h"
#include "gc_weak_follow.h"
#include "gc_weak_scan_cases.h"
static uint64_t weak_incoming_begin_via(WeakTable *table,uint64_t start,uint64_t limit,WeakIncoming *q,WeakIncomingStepper step){
    q->target_start=start;q->target_limit=limit;return step(table,q,0);
}
static void gc_run_weak_incoming_cases(PendingFollower candidate,WeakIncomingMetadata metadata_candidate,WeakIncomingStepper scan_candidate){
    for(unsigned impl=0;impl<2;impl++)for(unsigned generation=0;generation<2;generation++)for(unsigned mode=0;mode<12;mode++){
        PendingFollower follow=impl?candidate:gc_pending_follow_locked;
        WeakIncomingStepper step=impl?scan_candidate:gc_weak_incoming_step_locked;
        WeakIncomingMetadata metadata=impl?metadata_candidate:gc_metadata_step_locked;
        WeakScanFixture f;weak_scan_fixture(&f,generation,mode!=0);
        WeakIncoming scan={0};uint64_t start=GC_HEAP_PAGE_BYTES+32;
        WeakTableOps ops={.release=gc_weak_release_locked};MetadataLeasePool *pool=&f.r.weak.f.pool;
        assert(weak_incoming_begin_via(&f.table,start,start+16,&scan,step)==GC_WI_MORE);
        WeakIncoming before=scan;
        if(mode==3){
            f.table.last_serial++;
            assert(step(&f.table,&scan,1)==GC_WI_RESTART&&!memcmp(&before,&scan,sizeof(scan)));
            f.table.last_serial--;
        }else if(mode==4){
            f.r.weak.gate.closed=0;
            assert(step(&f.table,&scan,1)==GC_WI_RESTART&&!memcmp(&before,&scan,sizeof(scan)));
            WeakIncoming fresh={0};assert(weak_incoming_begin_via(&f.table,start,start+16,&fresh,step)==GC_WI_RESTART&&!fresh.stage);
            f.r.weak.gate.closed=1;
        }else if(mode==5){
            f.table.entries++;
            assert(step(&f.table,&scan,1)==GC_WI_RESTART&&!memcmp(&before,&scan,sizeof(scan)));
            f.table.entries--;
        }else if(mode==6){
            f.table.capacity=3;f.table.free_head=0;
            assert(step(&f.table,&scan,1)==GC_WI_RESTART&&!memcmp(&before,&scan,sizeof(scan)));
            f.table.capacity=f.table.free_head=4;
        }else if(mode==7){
            f.entries[0].slot.lease.token+=UINT64_C(1)<<32;
            assert(step(&f.table,&scan,1)==GC_WI_INVALID&&!memcmp(&before,&scan,sizeof(scan)));
            f.entries[0].slot.lease.token-=UINT64_C(1)<<32;
        }
        WeakHandle first={(uintptr_t)&f.table,1,11};
        if(mode==10){
            assert(gc_weak_table_release_locked(&f.table,first,&ops)==GC_WEAK_RELEASED);
        }else{
            assert(step(&f.table,&scan,1)==GC_WI_FOUND&&!scan.cursor);
            assert(scan.handle.index==1&&scan.handle.serial==11&&scan.observed==f.entries[0].slot.reference);
            MetadataLeaseRequest retired={0};
            if(mode==2){
                ReferenceLease pressure[6]={0};
                for(unsigned i=0;i<6;i++){pressure[i].reference=f.live;assert(gc_reference_lease_locked(pool,&pressure[i])==GC_ML_OK);}
                assert(gc_weak_table_follow_locked(&f.table,scan.handle,&retired,follow)==GC_RF_METADATA_FULL);
                assert(step(&f.table,&scan,1)==GC_WI_FOUND&&!scan.cursor);
                for(unsigned i=0;i<6;i++)assert(metadata(pool,&pressure[i].lease,GC_ML_RELEASE)==GC_ML_OK);
            }
            assert(gc_weak_table_follow_locked(&f.table,scan.handle,&retired,follow)==(mode?GC_RF_ADVANCED:GC_RF_DONE));
            if(!mode){
                /* Retained/unmoved references cannot be skipped as "processed". */
                assert(step(&f.table,&scan,1)==GC_WI_FOUND&&!scan.cursor);
                assert(gc_weak_table_release_locked(&f.table,scan.handle,&ops)==GC_WEAK_RELEASED);
            }
        }
        assert(step(&f.table,&scan,1)==GC_WI_MORE&&scan.cursor==1&&!scan.observed);
        assert(step(&f.table,&scan,1)==GC_WI_MORE&&scan.cursor==2);
        if(mode==8){
            f.entries[2].slot.lease.token=1;before=scan;
            assert(step(&f.table,&scan,1)==GC_WI_INVALID&&!memcmp(&before,&scan,sizeof(scan)));
            f.entries[2].slot.lease.token=0;
        }
        assert(step(&f.table,&scan,1)==GC_WI_MORE&&scan.cursor==3);
        if(mode==9){
            f.entries[3].slot.reference=f.live;before=scan;
            assert(step(&f.table,&scan,1)==GC_WI_INVALID&&!memcmp(&before,&scan,sizeof(scan)));
            f.entries[3].slot.reference=0;
        }
        assert(step(&f.table,&scan,1)==GC_WI_MORE&&scan.cursor==4);
        assert(step(&f.table,&scan,1)==GC_WI_DONE&&scan.stage==2&&f.r.weak.gate.closed);
        if(mode==11){
            f.table.last_serial++;assert(step(&f.table,&scan,1)==GC_WI_RESTART);f.table.last_serial--;
        }
        assert(step(&f.table,&scan,1)==GC_WI_DONE);
        for(unsigned i=0;i<4;i++)if(f.entries[i].slot.serial){
            WeakHandle handle={(uintptr_t)&f.table,i+1,f.entries[i].slot.serial};
            assert(gc_weak_table_release_locked(&f.table,handle,&ops)==GC_WEAK_RELEASED);
        }
        assert(!pool->active&&!f.r.weak.f.page.metadata_leases);free(f.heap);
    }
    puts("weak incoming: 48 C/emitted bounded scan, remap, pressure and stale-capture lifecycles PASS");
}
#endif
