#ifndef KLASSIC_GC_SURVIVOR_AGE_CASES_H
#define KLASSIC_GC_SURVIVOR_AGE_CASES_H
#include "gc_survivor_age.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void gc_run_survivor_age_cases(SurvivorAge candidate){
    unsigned count=0;
    for(unsigned impl=0;impl<2;impl++){
        SurvivorAge step=impl?candidate:gc_survivor_age_locked;
        for(uint64_t age=0;age<=15;age++)for(uint64_t threshold=1;threshold<=15;threshold++)for(unsigned retry=0;retry<2;retry++){
            MarkRecord record={.object_id=17,.young_epoch=7,.old_epoch=11,.age_epoch=age?10:0,.age=age};
            record.young_spill.serial=19;record.old_spill.serial=23;
            uint64_t cycle=age&&retry?10:11,new_age=age&&retry?age:age<15?age+1:15;
            MarkRecord expected=record;expected.age_epoch=cycle;expected.age=new_age;
            assert(step(&record,cycle,threshold)==(new_age>=threshold?GC_AGE_PROMOTE:GC_AGE_YOUNG));
            assert(!memcmp(&record,&expected,sizeof(record)));count++;
        }
        for(unsigned fault=0;fault<10;fault++){
            MarkRecord record={.object_id=17,.age_epoch=10,.age=3};MarkRecord *r=&record;
            uint64_t cycle=11,threshold=4;
            switch(fault){
            case 0:r=NULL;break;case 1:r=(void *)((uintptr_t)r+1);break;case 2:record.object_id=0;break;
            case 3:cycle=0;break;case 4:cycle=9;break;case 5:threshold=0;break;case 6:threshold=16;break;
            case 7:record.age=16;break;case 8:record.age_epoch=0;break;case 9:record.age=0;break;
            }
            MarkRecord before=record;assert(step(r,cycle,threshold)==GC_AGE_INVALID);assert(!memcmp(&record,&before,sizeof(record)));count++;
        }
        MarkRecord record={.object_id=17,.age_epoch=UINT64_MAX-1,.age=15};
        assert(step(&record,UINT64_MAX,15)==GC_AGE_PROMOTE&&record.age==15&&record.age_epoch==UINT64_MAX);count++;
        MarkRecord before=record;assert(step(&record,UINT64_MAX,15)==GC_AGE_PROMOTE&&!memcmp(&record,&before,sizeof(record)));count++;
    }
    assert(count==984);
    puts("survivor age: 984 C/candidate cycle-idempotent, saturation and rejection cases PASS");
}
static void gc_run_marked_survivor_age_cases(MarkedSurvivorAge candidate){
    unsigned count=0;
    for(unsigned impl=0;impl<2;impl++){
        MarkedSurvivorAge step=impl?candidate:gc_marked_survivor_age_locked;
        for(unsigned gen=0;gen<2;gen++)for(unsigned stamp=4;stamp<=6;stamp++)for(unsigned aged=0;aged<2;aged++)for(unsigned threshold=1;threshold<=4;threshold+=3){
            MarkQueue queue={.epoch=5,.generation=gen},before=queue;
            MarkRecord record={.object_id=17,.young_epoch=stamp,.old_epoch=11,.age_epoch=aged?4:0,.age=aged?3:0},expected=record;
            uint64_t result=GC_AGE_INVALID;
            if(!gen&&stamp<5)result=GC_AGE_NOT_SURVIVED;
            if(!gen&&stamp==5){expected.age++;expected.age_epoch=5;result=expected.age>=threshold?GC_AGE_PROMOTE:GC_AGE_YOUNG;}
            assert(step(&queue,&record,threshold)==result&&!memcmp(&record,&expected,sizeof(record))&&!memcmp(&queue,&before,sizeof(queue)));count++;
            if(!gen&&stamp==5){assert(step(&queue,&record,threshold)==result&&!memcmp(&record,&expected,sizeof(record)));count++;}
        }
        for(unsigned fault=0;fault<10;fault++){
            MarkQueue queue={.epoch=5},before;MarkRecord record={.object_id=17,.young_epoch=5},saved;
            const MarkQueue *q=&queue;MarkRecord *r=&record;uint64_t threshold=1;
            switch(fault){case 0:q=NULL;break;case 1:q=(void *)((uintptr_t)q+1);break;case 2:r=NULL;break;
            case 3:r=(void *)((uintptr_t)r+1);break;case 4:record.object_id=0;break;case 5:queue.epoch=0;break;
            case 6:threshold=0;break;case 7:threshold=16;break;case 8:record.age=16;break;case 9:record.age_epoch=6;record.age=1;break;}
            before=queue;saved=record;assert(step(q,r,threshold)==GC_AGE_INVALID);
            assert(!memcmp(&queue,&before,sizeof(queue))&&!memcmp(&record,&saved,sizeof(record)));count++;
        }
    }
    assert(count==76);
    puts("marked survivor age: 76 C/candidate epoch, generation, retry and rejection cases PASS");
}
#endif
