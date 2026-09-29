#ifndef KLASSIC_GC_HEAP_WALK_CASES_H
#define KLASSIC_GC_HEAP_WALK_CASES_H
#include "gc_heap_walk.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
typedef struct {MetadataLeasePool pool;HeapWalk walk;HeapPool heap;MetadataLeaseSlot slots[2];AllocationPage pages[9];} HeapWalkFixture;
static void heap_walk_fixture(HeapWalkFixture *f){
    memset(f,0,sizeof(*f));
    f->pool=(MetadataLeasePool){(uintptr_t)&f->heap,(uintptr_t)f->slots,1,1,0};
    f->heap.pages=(uintptr_t)f->pages;f->heap.page_count=9;
    f->walk=(HeapWalk){(uintptr_t)&f->pool,(uintptr_t)&f->heap,(uintptr_t)f->pages,9,0,{0}};
    for(unsigned i=0;i<9;i++){
        f->pages[i].incarnation=27+i;f->pages[i].run_head=i;
        atomic_store(&f->pages[i].state,i);
    }
    f->pages[4].run_head=1;f->pages[4].incarnation=f->pages[1].incarnation;
}
static void gc_run_heap_walk_cases(HeapWalker candidate){
    for(unsigned impl=0;impl<2;impl++){
        HeapWalker step=impl?candidate:gc_heap_walk_locked;HeapWalkFixture f,before;
        assert(step(NULL,NULL)==GC_HW_INVALID);heap_walk_fixture(&f);
        assert(step(&f.pool,NULL)==GC_HW_INVALID&&step(&f.pool,(void *)1)==GC_HW_INVALID);
        for(unsigned round=0;round<32;round++){
            heap_walk_fixture(&f);
            for(unsigned i=0;i<9;i++){
                assert(f.walk.cursor==i&&!f.pool.active&&!f.walk.lease.token);
                if(i==2||i==6){
                    memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.walk)==GC_HW_WAIT);
                    assert(!memcmp(&f,&before,sizeof(f)));atomic_store(&f.pages[i].state,GC_PAGE_FREE);
                }
                if(i==1||i==5||i==7||i==8){
                    MetadataLeaseRequest blocker={(uintptr_t)&f.pages[1],f.pages[1].incarnation,0};
                    assert(gc_metadata_step_locked(&f.pool,&blocker,GC_ML_ACQUIRE)==GC_ML_OK);
                    memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.walk)==GC_HW_FULL);
                    assert(!memcmp(&f,&before,sizeof(f)));
                    assert(gc_metadata_step_locked(&f.pool,&blocker,GC_ML_RELEASE)==GC_ML_OK);
                    assert(step(&f.pool,&f.walk)==GC_HW_PAGE&&f.walk.cursor==i+1);
                    assert(f.walk.lease.page==(uintptr_t)&f.pages[i]&&f.walk.lease.incarnation==27+i&&f.walk.lease.token);
                    assert(f.pool.active==1&&f.pages[i].metadata_leases==1);
                    memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.walk)==GC_HW_INVALID);
                    assert(!memcmp(&f,&before,sizeof(f)));
                    assert(gc_metadata_step_locked(&f.pool,&f.walk.lease,GC_ML_RELEASE)==GC_ML_OK);
                }else assert(step(&f.pool,&f.walk)==GC_HW_MORE&&f.walk.cursor==i+1);
            }
            memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.walk)==GC_HW_DONE);
            assert(!memcmp(&f,&before,sizeof(f))&&!f.pool.active);
        }
        for(unsigned fault=0;fault<24;fault++){
            heap_walk_fixture(&f);f.walk.cursor=1;uint64_t expected=GC_HW_INVALID;
            switch(fault){
            case 0:f.walk.pool=0;break;case 1:f.walk.heap++;break;case 2:f.walk.pages++;break;
            case 3:f.walk.page_count++;break;case 4:f.walk.cursor=10;break;
            case 5:f.walk.lease.page=1;break;case 6:f.walk.lease.incarnation=1;break;case 7:f.walk.lease.token=1;break;
            case 8:f.pool.heap=0;break;case 9:f.heap.pages=0;break;case 10:f.heap.page_count=0;break;
            case 11:f.heap.page_count=UINT64_MAX;break;case 12:atomic_store(&f.pages[1].state,9);break;
            case 13:f.pages[1].incarnation=0;break;case 14:f.pages[1].run_head=0;break;
            case 15:f.pool.slots=0;break;case 16:f.slots[0].generation=UINT32_MAX;expected=GC_HW_EXHAUSTED;break;
            case 17:f.pages[1].metadata_leases=UINT64_MAX;expected=GC_HW_EXHAUSTED;break;
            case 18:f.walk.cursor=4;f.pages[4].run_head=4;break;
            case 19:f.walk.cursor=4;f.pages[4].incarnation++;break;
            case 20:f.walk.cursor=4;atomic_store(&f.pages[1].state,GC_PAGE_FREE);break;
            case 21:f.walk.cursor=0;f.pages[0].metadata_leases=1;break;
            case 22:f.walk.cursor=3;atomic_store(&f.pages[3].allocated_top,16);expected=GC_HW_WAIT;break;
            case 23:f.walk.cursor=4;f.pages[1].run_head=0;break;
            }
            memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.walk)==expected);
            assert(!memcmp(&f,&before,sizeof(f)));
        }
    }
    puts("heap walk: 64 C/candidate mixed-page walks preserve lease ownership and 48 unchanged rejection cases PASS");
}
#endif
