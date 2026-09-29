#ifndef KLASSIC_GC_ALLOCATION_PRESSURE_CASES_H
#define KLASSIC_GC_ALLOCATION_PRESSURE_CASES_H
#include "gc_allocation_pressure.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void gc_run_allocation_pressure_cases(AllocationPressure candidate){
    const uint64_t bytes[]={0,1,UINT32_MAX,UINT64_C(1)<<32,UINT64_MAX-1};
    const uint64_t times[]={1,UINT64_C(1)<<32,UINT64_MAX};
    const uint64_t headrooms[]={0,1,UINT32_MAX,UINT64_C(1)<<32,UINT64_MAX};
    /* Unbounded-integer oracle: delta*horizon >= headroom*elapsed, in the
     * Cartesian-product loop order below. No native limb arithmetic oracle. */
    const unsigned char expected[]={
        1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
        1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
        1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
        1,1,1,1,1,1,0,1,1,0,1,1,0,0,1,
        1,1,1,0,1,1,0,0,1,0,0,0,0,0,0,
        1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,
        1,1,1,1,1,1,1,1,1,0,1,1,0,0,1,
        1,1,1,0,1,1,0,1,1,0,0,1,0,0,0,
        1,1,1,0,0,1,0,0,1,0,0,0,0,0,0,
        1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,
        1,1,1,1,1,1,0,1,1,0,1,1,0,0,1,
        1,1,1,0,1,1,0,0,1,0,0,1,0,0,0,
        1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,
        1,1,1,1,1,1,1,1,1,0,1,1,0,0,1,
        1,1,1,0,1,1,0,1,1,0,0,1,0,0,0,
    };
    unsigned cases=0;
    for(unsigned d=0;d<5;d++)for(unsigned t=0;t<3;t++)for(unsigned b=0;b<5;b++)for(unsigned h=0;h<3;h++){
        AllocationSample q={0x1000,17,times[t],UINT64_MAX-1,times[t],bytes[d]},before=q;
        assert(gc_allocation_pressure(&q,headrooms[b],times[h])==expected[cases]);
        assert(candidate(&q,headrooms[b],times[h])==expected[cases]);
        assert(!memcmp(&q,&before,sizeof(q)));cases++;
    }
    assert(cases==sizeof(expected));
    for(unsigned fault=0;fault<11;fault++){
        AllocationSample q={0x1000,17,20,100,10,40};const AllocationSample *query=&q;uint64_t horizon=10;
        switch(fault){
        case 0:query=NULL;break;case 1:query=(const void *)((const unsigned char *)&q+1);break;
        case 2:q.context=0;break;case 3:q.context++;break;case 4:q.identity=0;break;
        case 5:q.elapsed_ns=0;break;case 6:horizon=0;break;case 7:q.total_bytes=UINT64_MAX;break;
        case 8:q.delta_bytes=101;break;case 9:q.elapsed_ns=21;break;case 10:q=(AllocationSample){0};break;
        }
        AllocationSample before=q;
        assert(gc_allocation_pressure(query,100,horizon)==GC_PRESSURE_INVALID);
        assert(candidate(query,100,horizon)==GC_PRESSURE_INVALID&&!memcmp(&q,&before,sizeof(q)));cases++;
    }
    assert(cases==236);
    puts("allocation pressure: 236 C/candidate exact-product and invalid-sample cases PASS");
}
#endif
