#ifndef KLASSIC_GC_FORWARD_COMPRESS_CASES_H
#define KLASSIC_GC_FORWARD_COMPRESS_CASES_H
#include "gc_forward_compress.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void gc_run_forward_compress_cases(ForwardCompressor candidate){
    const uint64_t colors[]={0x1510,0x2620,0x4930};unsigned cases=0;
    for(unsigned impl=0;impl<2;impl++)for(unsigned color=0;color<3;color++)for(unsigned fault=0;fault<26;fault++){
        ForwardCompressor step=impl?candidate:gc_forward_compress_locked;
        RelocationGate g={.epoch=11,.closed=1};RelocationPermit permit={0};
        ForwardCell cell={.source_offset=16,.incarnation=7,.word=(UINT64_C(128)<<16)|0x1512,.access=GC_SOURCE_CLOSED};
        ForwardCompress q={(uintptr_t)&cell,16,7,(UINT64_C(128)<<16)|0x1510,(UINT64_C(256)<<16)|colors[color],11};
        uint64_t result=GC_FC_INVALID;
        switch(fault){
            case 0:result=GC_FC_OK;break;
            case 1:q.source_offset=0;break;case 2:q.source_offset=17;break;case 3:q.incarnation=0;break;
            case 4:q.cell=0;break;case 5:q.cell++;break;case 6:q.epoch=0;break;
            case 7:q.epoch++;result=GC_FC_RETRY;break;case 8:g.closed=0;result=GC_FC_WAIT;break;
            case 9:g.epoch=0;break;case 10:g.active=g.last_serial=1;g.head=(uintptr_t)&permit;result=GC_FC_WAIT;break;
            case 11:cell.source_offset=32;break;case 12:cell.incarnation++;break;
            case 13:atomic_store(&cell.access,0);break;case 14:atomic_store(&cell.access,GC_SOURCE_CLOSED|1);break;
            case 15:q.observed=0;break;case 16:q.destination=0;break;
            case 17:q.observed=(UINT64_C(16)<<16)|0x1510;break;
            case 18:q.destination=(UINT64_C(16)<<16)|0x1510;break;
            case 19:q.destination|=1;break;case 20:q.observed|=1;break;
            case 21:atomic_store(&cell.word,q.destination|2);result=GC_FC_RETRY;break;
            case 22:atomic_store(&cell.word,5);result=GC_FC_RETRY;break;
            case 23:atomic_store(&cell.word,3);result=GC_FC_RETRY;break;
            case 24:q.destination=q.observed;result=GC_FC_OK;break;
            case 25:g.closed=2;break;
        }
        RelocationGate before=g;ForwardCompress request=q;ForwardCell expected;memcpy(&expected,&cell,sizeof(cell));
        if(result==GC_FC_OK)atomic_store(&expected.word,q.destination|2);
        assert(step(&g,&q)==result);
        assert(!memcmp(&g,&before,sizeof(g))&&!memcmp(&q,&request,sizeof(q))&&!memcmp(&cell,&expected,sizeof(cell)));
        cases++;
    }
    printf("forward compression: %u C/candidate exact-edge, gate and unchanged rejection cases PASS\n",cases);
}
#endif
