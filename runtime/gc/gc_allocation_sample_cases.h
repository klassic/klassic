#ifndef KLASSIC_GC_ALLOCATION_SAMPLE_CASES_H
#define KLASSIC_GC_ALLOCATION_SAMPLE_CASES_H
#include "gc_allocation_sample.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void gc_run_allocation_sample_cases(AllocationSampler candidate){
    AllocationSampler implementations[]={gc_allocation_sample,candidate};
    for(unsigned implementation=0;implementation<2;implementation++)for(unsigned fault=0;fault<25;fault++){
        MutatorContext ctx={.abi_version=GC_ABI_VERSION,.thread_id=17,.allocated_bytes=200};
        AllocationSample q={(uintptr_t)&ctx,17,10,100,3,4};
        const MutatorContext *context=&ctx;AllocationSample *query=&q;
        uint64_t now=20,status=GC_SAMPLE_INVALID;
        switch(fault){
        case 0:status=GC_SAMPLE_OK;break;
        case 1:q=(AllocationSample){0};status=GC_SAMPLE_BASELINE;break;
        case 2:now=10;status=GC_SAMPLE_WAIT;break;
        case 3:now=9;break;
        case 4:atomic_store(&ctx.allocated_bytes,UINT64_MAX);status=GC_SAMPLE_SATURATED;break;
        case 5:q=(AllocationSample){0};atomic_store(&ctx.allocated_bytes,UINT64_MAX);status=GC_SAMPLE_SATURATED;break;
        case 6:q.context+=8;break;case 7:q.identity++;break;case 8:ctx.thread_id=0;break;
        case 9:ctx.abi_version--;break;case 10:q.total_bytes=UINT64_MAX;break;case 11:q.total_bytes=201;break;
        case 12:query=NULL;break;case 13:context=NULL;break;
        case 14:query=(void *)((unsigned char *)&q+1);break;case 15:context=(const void *)((const unsigned char *)&ctx+1);break;
        case 16:q=(AllocationSample){.total_bytes=1};break;case 17:q=(AllocationSample){.timestamp_ns=1};break;
        case 18:q=(AllocationSample){.identity=1};break;case 19:q=(AllocationSample){.delta_bytes=1};break;
        case 20:q=(AllocationSample){.elapsed_ns=1};break;
        case 21:atomic_store(&ctx.allocated_bytes,100);status=GC_SAMPLE_OK;break;
        case 22:now=UINT64_MAX;status=GC_SAMPLE_OK;break;
        case 23:now=0;q=(AllocationSample){0};status=GC_SAMPLE_BASELINE;break;
        case 24:now=UINT64_MAX;q.timestamp_ns=q.total_bytes=0;atomic_store(&ctx.allocated_bytes,UINT64_MAX-1);status=GC_SAMPLE_OK;break;
        }
        MutatorContext before;memcpy(&before,&ctx,sizeof(ctx));AllocationSample expected=q;
        if(status==GC_SAMPLE_BASELINE)expected=(AllocationSample){(uintptr_t)&ctx,17,now,200,0,0};
        if(status==GC_SAMPLE_OK)expected=(AllocationSample){q.context,q.identity,now,atomic_load(&ctx.allocated_bytes),
            now-q.timestamp_ns,atomic_load(&ctx.allocated_bytes)-q.total_bytes};
        assert(implementations[implementation](context,query,now)==status);
        assert(!memcmp(&ctx,&before,sizeof(ctx))&&!memcmp(&q,&expected,sizeof(q)));
        if(status==GC_SAMPLE_OK||status==GC_SAMPLE_BASELINE){
            assert(implementations[implementation](context,query,now)==GC_SAMPLE_WAIT);
            assert(!memcmp(&ctx,&before,sizeof(ctx))&&!memcmp(&q,&expected,sizeof(q)));
        }
    }
    puts("allocation samples: 25 C/candidate baseline, time, identity and saturation preservation cases PASS");
}
#endif
