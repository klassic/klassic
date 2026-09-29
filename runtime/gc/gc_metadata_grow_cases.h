#ifndef KLASSIC_GC_METADATA_GROW_CASES_H
#define KLASSIC_GC_METADATA_GROW_CASES_H
#include "gc_metadata_grow.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
typedef uint64_t (*MetadataGrower)(MetadataLeasePool *,uint64_t);
static void gc_run_metadata_grow_cases(MetadataGrower grow){
    unsigned count=0;
    const uint64_t additions[]={1,2,63,64,255,256};
    for(uint64_t old=1;old<=4;old++)for(uint64_t active=0;active<=old;active++)
    for(unsigned a=0;a<sizeof(additions)/sizeof(*additions);a++){
        MetadataLeaseSlot slots[260]={0},expected[260];
        uint64_t heap=0,new_capacity=old+additions[a];
        for(uint64_t i=0;i<old;i++){
            if(i<active)slots[i]=(MetadataLeaseSlot){.page=0x1000+8*i,.incarnation=17,.generation=23};
            else slots[i].next_free=i+1<old?i+2:0;
        }
        MetadataLeasePool pool={(uintptr_t)&heap,(uintptr_t)slots,old,active<old?active+1:0,active};
        memcpy(expected,slots,sizeof(slots));
        for(uint64_t i=old;i<new_capacity;i++)expected[i].next_free=i+1<new_capacity?i+2:pool.free_head;
        MetadataLeasePool result=pool;result.capacity=new_capacity;result.free_head=old+1;
        assert(grow(&pool,new_capacity)==GC_ML_OK);
        assert(!memcmp(&pool,&result,sizeof(pool))&&!memcmp(slots,expected,sizeof(slots)));count++;
        assert(grow(&pool,new_capacity)==GC_ML_INVALID);
        assert(!memcmp(&pool,&result,sizeof(pool))&&!memcmp(slots,expected,sizeof(slots)));count++;
    }
    for(unsigned fault=0;fault<12;fault++){
        uint64_t heap=0,n=3;MetadataLeaseSlot slots[260]={0},before[260];
        MetadataLeasePool pool={(uintptr_t)&heap,(uintptr_t)slots,2,1,0};slots[0].next_free=2;
        switch(fault){
        case 0:pool.heap=0;break;case 1:pool.slots=0;break;
        case 2:pool.capacity=0;break;case 3:n=1;break;case 4:n=259;break;
        case 5:pool.active=3;break;case 6:pool.free_head=3;break;case 7:pool.free_head=0;break;
        case 8:slots[2].page=8;break;case 9:slots[2].incarnation=1;break;
        case 10:slots[2].generation=1;break;case 11:slots[2].next_free=1;break;
        }
        MetadataLeasePool saved=pool;memcpy(before,slots,sizeof(slots));
        assert(grow(&pool,n)==GC_ML_INVALID);
        assert(!memcmp(&pool,&saved,sizeof(pool))&&!memcmp(slots,before,sizeof(slots)));count++;
    }
    assert(count==180);
    puts("metadata grow: 180 bounded admission and unchanged rejection cases PASS");
}
#endif
