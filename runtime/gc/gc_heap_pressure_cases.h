#ifndef KLASSIC_GC_HEAP_PRESSURE_CASES_H
#define KLASSIC_GC_HEAP_PRESSURE_CASES_H
#include "gc_heap_pressure.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
/* Expected results generated with unbounded integer arithmetic. */
static void gc_run_heap_pressure_cases(HeapAllocationPressure candidate){
    static const uint64_t rows[][8]={
        {UINT64_C(1000),UINT64_C(600),UINT64_C(200),UINT64_C(200),UINT64_C(100),UINT64_C(299),UINT64_C(1),UINT64_C(0)},
        {UINT64_C(1000),UINT64_C(600),UINT64_C(200),UINT64_C(200),UINT64_C(100),UINT64_C(300),UINT64_C(1),UINT64_C(1)},
        {UINT64_C(1000),UINT64_C(1000),UINT64_C(200),UINT64_C(500),UINT64_C(100),UINT64_C(199),UINT64_C(1),UINT64_C(0)},
        {UINT64_C(1000),UINT64_C(1000),UINT64_C(200),UINT64_C(500),UINT64_C(100),UINT64_C(200),UINT64_C(1),UINT64_C(1)},
        {UINT64_C(1000),UINT64_C(600),UINT64_C(700),UINT64_C(200),UINT64_C(0),UINT64_C(99),UINT64_C(1),UINT64_C(0)},
        {UINT64_C(1000),UINT64_C(600),UINT64_C(700),UINT64_C(200),UINT64_C(0),UINT64_C(100),UINT64_C(1),UINT64_C(1)},
        {UINT64_C(1000),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(1),UINT64_C(1)},
        {UINT64_C(1000),UINT64_C(600),UINT64_C(1000),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(1),UINT64_C(1)},
        {UINT64_C(1000),UINT64_C(600),UINT64_C(500),UINT64_C(800),UINT64_C(0),UINT64_C(0),UINT64_C(1),UINT64_C(1)},
        {UINT64_C(1000),UINT64_C(600),UINT64_C(0),UINT64_C(600),UINT64_C(0),UINT64_C(0),UINT64_C(1),UINT64_C(1)},
        {UINT64_C(1000),UINT64_C(600),UINT64_C(0),UINT64_C(0),UINT64_C(600),UINT64_C(0),UINT64_C(1),UINT64_C(1)},
        {UINT64_C(1000),UINT64_C(600),UINT64_C(0),UINT64_C(599),UINT64_C(0),UINT64_C(0),UINT64_C(1),UINT64_C(0)},
        {UINT64_C(18446744073709551615),UINT64_C(18446744073709551615),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(18446744073709551614),UINT64_C(1),UINT64_C(0)},
        {UINT64_C(18446744073709551615),UINT64_C(18446744073709551615),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(18446744073709551614),UINT64_C(2),UINT64_C(1)},
        {UINT64_C(18446744073709551615),UINT64_C(18446744073709551615),UINT64_C(1),UINT64_C(18446744073709551613),UINT64_C(1),UINT64_C(0),UINT64_C(1),UINT64_C(1)},
        {UINT64_C(18446744073709551615),UINT64_C(18446744073709551615),UINT64_C(0),UINT64_C(18446744073709551613),UINT64_C(1),UINT64_C(0),UINT64_C(1),UINT64_C(0)},
        {UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(1),UINT64_C(2)},
        {UINT64_C(1000),UINT64_C(1001),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(1),UINT64_C(2)},
        {UINT64_C(1000),UINT64_C(1000),UINT64_C(1001),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(1),UINT64_C(2)},
        {UINT64_C(1000),UINT64_C(1000),UINT64_C(0),UINT64_C(1001),UINT64_C(0),UINT64_C(0),UINT64_C(1),UINT64_C(2)},
        {UINT64_C(1000),UINT64_C(1000),UINT64_C(0),UINT64_C(999),UINT64_C(2),UINT64_C(0),UINT64_C(1),UINT64_C(2)},
        {UINT64_C(18446744073709551615),UINT64_C(18446744073709551615),UINT64_C(0),UINT64_C(18446744073709551615),UINT64_C(1),UINT64_C(0),UINT64_C(1),UINT64_C(2)},
        {UINT64_C(1000),UINT64_C(600),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(18446744073709551615),UINT64_C(1),UINT64_C(2)},
        {UINT64_C(1000),UINT64_C(600),UINT64_C(0),UINT64_C(0),UINT64_C(0),UINT64_C(1),UINT64_C(0),UINT64_C(2)}
    };
    unsigned cases=0;
    for(unsigned implementation=0;implementation<2;implementation++){
        HeapAllocationPressure forecast=implementation?candidate:gc_heap_allocation_pressure;
        for(unsigned i=0;i<sizeof(rows)/sizeof(rows[0]);i++){
            const uint64_t *r=rows[i];
            HeapPool heap={.hard_max=r[0],.soft_max=r[1],.relocation_reserve=r[2],.committed=r[3],.pending=r[4],.pressure=i&1};
            AllocationSample sample={0x1000,17,1,UINT64_MAX-1,1,r[5]};
            HeapPool before=heap;AllocationSample saved=sample;
            assert(forecast(&heap,&sample,r[6])==r[7]);
            assert(!memcmp(&heap,&before,sizeof(heap))&&!memcmp(&sample,&saved,sizeof(sample)));cases++;
        }
        for(unsigned fault=0;fault<4;fault++){
            HeapPool heap={.hard_max=1000,.soft_max=600};
            AllocationSample sample={0x1000,17,1,100,1,1};
            HeapPool before=heap;AllocationSample saved=sample;
            const HeapPool *h=fault==0?NULL:fault==1?(void *)((uintptr_t)&heap+1):&heap;
            const AllocationSample *s=fault==2?NULL:fault==3?(void *)((uintptr_t)&sample+1):&sample;
            assert(forecast(h,s,1)==GC_PRESSURE_INVALID);
            assert(!memcmp(&heap,&before,sizeof(heap))&&!memcmp(&sample,&saved,sizeof(sample)));cases++;
        }
    }
    assert(cases==56);
    puts("heap pressure: 56 C/candidate soft target, reserve, pending and rejection cases PASS");
}
#endif
