#ifndef KLASSIC_GC_PROMOTION_REMEMBER_CASES_H
#define KLASSIC_GC_PROMOTION_REMEMBER_CASES_H
#include "gc_promotion_remember.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void gc_run_promotion_remember_cases(PromotionRememberStep candidate){
    unsigned cases=0;
    for(unsigned impl=0;impl<2;impl++)for(unsigned shape=0;shape<4;shape++){
        PromotionRememberStep step=impl?candidate:gc_promotion_remember_locked;
        uint64_t fixed=2,element=2,bits[2][16]={0};
        TypeDescriptor d={.id=1,.alignment=16};uint64_t bytes;
        if(shape==0){d.fixed_bytes=24;d.fixed_bitmap=(uintptr_t)&fixed;d.fixed_bitmap_words=1;bytes=24;}
        else if(shape==1){d.fixed_bytes=8000;bytes=8000;}
        else if(shape==2){d.fixed_bytes=8;d.element_bytes=16;d.element_bitmap=(uintptr_t)&element;d.element_bitmap_words=1;bytes=4808;}
        else bytes=0;
        _Atomic uint64_t descriptors[2]={0};DescriptorRegistry registry={(uintptr_t)descriptors,2};
        assert(gc_descriptor_register(&registry,&d)==GC_DESC_OK);
        RememberedInventory g={.epoch=0x10000,.last_phase=0x10000};
        RememberedSet set={.start=4096,.limit=12288,.incarnation=7,.bits={(uintptr_t)bits[0],(uintptr_t)bits[1]},.words=16,.epoch=g.epoch,.last_phase=g.epoch};
        RememberedRegistration r={.set=(uintptr_t)&set};RememberedJob job={0};
        RememberedInventoryRequest request={(uintptr_t)&r,(uintptr_t)&job,0};
        assert(gc_remembered_inventory_step_locked(&g,&request,GC_RI_ATTACH)==GC_RS_OK);
        PromotionRemember initial={(uintptr_t)&registry,(uintptr_t)&r,4128,1,bytes,g.epoch,0};
        for(unsigned fault=0;fault<9;fault++){
            PromotionRemember q=initial;
            if(fault==0)q.owner++;
            if(fault==1)q.owner=set.limit+16;
            if(fault==2)q.payload_bytes=UINT64_MAX;
            if(fault==3)q.cursor=bytes+8;
            if(fault==4)q.descriptor_id=2;
            if(fault==5)q.registry=0;
            if(fault==6)q.registration=0;
            if(fault==7)q.phase+=0x10000;
            if(fault==8)q.owner=set.start+16;
            PromotionRemember before=q;RememberedInventory gb=g;RememberedSet sb=set;
            uint64_t bb[2][16];memcpy(bb,bits,sizeof(bits));
            assert(step(&g,&q)==(fault==7?GC_PR_WAIT:GC_PR_INVALID));
            assert(!memcmp(&q,&before,sizeof(q))&&!memcmp(&g,&gb,sizeof(g))&&!memcmp(&set,&sb,sizeof(set))&&!memcmp(bb,bits,sizeof(bits)));cases++;
        }
        PromotionRemember q=initial;uint64_t status;unsigned calls=0;
        do{uint64_t cursor=q.cursor;status=step(&g,&q);calls++;assert(calls<=601);
            assert(status<=GC_PR_MORE&&q.cursor>=cursor);
            if(shape==0||shape==2)assert(q.cursor-cursor<=16);
        }while(status==GC_PR_MORE);
        assert(q.cursor==bytes&&set.current_count==(shape==0?1:shape==2?300:0));
        assert(calls==(shape==0?3:shape==2?600:1));
        for(unsigned bit=0;bit<1024;bit++){
            uint64_t slot=set.start+8*bit;
            bool expected=shape==0?slot==initial.owner+8:
                shape==2&&slot>=initial.owner+16&&slot<initial.owner+bytes&&(slot-initial.owner-16)%16==0;
            assert(!!(bits[0][bit/64]&(UINT64_C(1)<<(bit%64)))==expected&&!bits[1][bit/64]);
        }
        RememberedSet sb=set;assert(step(&g,&q)==GC_PR_DONE&&!memcmp(&set,&sb,sizeof(set)));cases++;
        if(shape==0){
            // Phase handoff after private-prefix work cannot silently advance
            // the old cursor. Restart re-records all fields in the new bank.
            q=initial;q.cursor=16;request.phase=0x21101;
            assert(gc_remembered_inventory_step_locked(&g,&request,GC_RI_BEGIN)==GC_RS_OK);
            job=(RememberedJob){.incarnation=set.incarnation,.phase=g.epoch};
            assert(gc_remembered_inventory_step_locked(&g,&request,GC_RI_FLIP)==GC_RS_OK);
            PromotionRemember before=q;assert(step(&g,&q)==GC_PR_WAIT&&!memcmp(&q,&before,sizeof(q)));
            q.phase=g.epoch;q.cursor=0;do{status=step(&g,&q);assert(status<=GC_PR_MORE);}while(status==GC_PR_MORE);
            assert(set.current_count==1&&set.pending==1&&g.pending==1);cases++;
        }
    }
    printf("promotion remember: %u C/candidate bounded shapes, unchanged rejection and phase restart cases PASS\n",cases);
}
#endif
