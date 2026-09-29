#ifndef KLASSIC_GC_OBJECT_WALK_CASES_H
#define KLASSIC_GC_OBJECT_WALK_CASES_H
#include "gc_object_walk.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
typedef struct {MetadataLeasePool pool;HeapPool heap;AllocationPage page;MetadataLeaseSlot slot;MetadataLeaseRequest lease;ForwardTable table;ForwardCell cells[4];ObjectWalk walk;} ObjectWalkFixture;
static void object_walk_fixture(ObjectWalkFixture *f){
    memset(f,0,sizeof(*f));
    f->pool=(MetadataLeasePool){(uintptr_t)&f->heap,(uintptr_t)&f->slot,1,1,0};
    f->heap.pages=(uintptr_t)&f->page;f->heap.page_count=1;
    f->page=(AllocationPage){.heap_base=UINT64_MAX-15,.start=16,.limit=1024,.incarnation=27,
        .state=GC_PAGE_CLOSED,.allocated_top=256,.forward_table=(uintptr_t)&f->table};
    f->table=(ForwardTable){(uintptr_t)f->cells,4,3};
    for(unsigned i=0;i<4;i++)f->cells[i]=(ForwardCell){48+i*48,27,0,0};
    f->lease=(MetadataLeaseRequest){(uintptr_t)&f->page,27,0};
    assert(gc_metadata_step_locked(&f->pool,&f->lease,GC_ML_ACQUIRE)==GC_ML_OK);
    f->walk=(ObjectWalk){(uintptr_t)&f->lease,(uintptr_t)&f->page,27,(uintptr_t)&f->table,3,0,0};
}
static void gc_run_object_walk_cases(ObjectWalker candidate){
    const uint64_t states[]={GC_PAGE_ALLOCATING,GC_PAGE_CLOSED,GC_PAGE_EVACUATED,GC_PAGE_UNCOMMITTED};
    const uint64_t words[]={0,5,3,(UINT64_C(512)<<16)|0x1512};
    for(unsigned impl=0;impl<2;impl++){
        ObjectWalker step=impl?candidate:gc_object_walk_locked;ObjectWalkFixture f,before;
        assert(step(NULL,NULL)==GC_OW_INVALID);object_walk_fixture(&f);
        assert(step(&f.pool,NULL)==GC_OW_INVALID&&step(&f.pool,(void *)1)==GC_OW_INVALID);
        for(unsigned state=0;state<4;state++)for(unsigned word=0;word<4;word++){
            object_walk_fixture(&f);atomic_store(&f.page.state,states[state]);
            for(unsigned i=0;i<4;i++)atomic_store(&f.cells[i].word,words[word]);
            for(unsigned i=0;i<3;i++){
                memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.walk)==GC_OW_ITEM);
                before.walk.source_offset=48+i*48;before.walk.cursor=i+1;
                assert(!memcmp(&f,&before,sizeof(f)));
                assert(step(&f.pool,&f.walk)==GC_OW_INVALID&&!memcmp(&f,&before,sizeof(f)));
                f.walk.source_offset=0;
            }
            memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.walk)==GC_OW_DONE&&!memcmp(&f,&before,sizeof(f)));
            atomic_store(&f.table.used,4);memcpy(&before,&f,sizeof(f));
            assert(step(&f.pool,&f.walk)==GC_OW_WAIT&&!memcmp(&f,&before,sizeof(f)));
            f.walk.used=4;f.walk.cursor=0;
            for(unsigned i=0;i<4;i++){
                assert(step(&f.pool,&f.walk)==GC_OW_ITEM&&f.walk.source_offset==48+i*48&&f.walk.cursor==i+1);
                f.walk.source_offset=0;
            }
            assert(step(&f.pool,&f.walk)==GC_OW_DONE&&f.pool.active==1&&f.page.metadata_leases==1);
            assert(gc_metadata_step_locked(&f.pool,&f.lease,GC_ML_RELEASE)==GC_ML_OK);
            memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.walk)==GC_OW_INVALID&&!memcmp(&f,&before,sizeof(f)));
        }
        for(unsigned fault=0;fault<30;fault++){
            object_walk_fixture(&f);uint64_t expected=GC_OW_INVALID;
            switch(fault){
            case 0:f.walk.lease=0;break;case 1:f.walk.lease=1;break;case 2:f.walk.page++;break;
            case 3:f.walk.incarnation++;break;case 4:f.walk.source_offset=48;break;
            case 5:f.lease.token=0;break;case 6:f.lease.token+=UINT64_C(1)<<32;break;
            case 7:f.pool.heap=0;break;case 8:atomic_store(&f.page.state,GC_PAGE_FREE);break;
            case 9:f.page.start=0;break;case 10:f.page.limit=16;break;
            case 11:f.page.limit=GC_MAX_OFFSET+1;break;case 12:f.page.start++;break;
            case 13:f.walk.table=0;break;case 14:f.walk.table++;break;case 15:f.page.forward_table=0;break;
            case 16:f.table.cells=0;break;case 17:f.table.capacity=0;break;
            case 18:f.table.capacity=UINT64_C(1)<<32;break;case 19:f.table.cells=UINT64_MAX-7;break;
            case 20:f.walk.used=5;break;case 21:f.walk.cursor=4;break;case 22:atomic_store(&f.table.used,5);break;
            case 23:f.walk.used=2;expected=GC_OW_WAIT;break;
            case 24:atomic_store(&f.page.allocated_top,0);break;case 25:atomic_store(&f.page.allocated_top,1025);break;
            case 26:f.cells[0].source_offset=32;break;case 27:f.cells[0].source_offset=256;break;
            case 28:f.cells[0].incarnation++;break;case 29:f.walk.cursor=1;f.cells[1].source_offset=48;break;
            }
            memcpy(&before,&f,sizeof(f));assert(step(&f.pool,&f.walk)==expected&&!memcmp(&f,&before,sizeof(f)));
        }
    }
    puts("object walk: 32 C/candidate leased key walks, append restarts and 60 unchanged rejection cases PASS");
}
#endif
