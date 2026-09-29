#ifndef KLASSIC_GC_RELOCATION_CANCEL_CASES_H
#define KLASSIC_GC_RELOCATION_CANCEL_CASES_H
#include "gc_relocate_discard_cases.h"
#include "gc_relocation_cancel.h"
typedef uint64_t (*RelocationCanceller)(RelocationGate *,RelocationCancellation *);
typedef struct {DiscardFixture d;RelocationGate g;RelocationPermit p;RelocationCancellation q;} CancelFixture;
#define CD(f) (offsetof(CancelFixture,d)+offsetof(DiscardFixture,f))
static const uint64_t cancel_pointers[]={CD(page.heap_base),CD(copy.heap_base),CD(q.owner),CD(q.source),CD(q.copy),
    CD(heap)+16+offsetof(ObjectHeader,mark_record),CD(heap)+2048+offsetof(ObjectHeader,mark_record),
    offsetof(CancelFixture,g.head),offsetof(CancelFixture,p.gate),offsetof(CancelFixture,p.previous),
    offsetof(CancelFixture,p.next),offsetof(CancelFixture,p.job),offsetof(CancelFixture,q.owner),
    offsetof(CancelFixture,q.permit),offsetof(CancelFixture,q.target),offsetof(CancelFixture,q.discard)};
#undef CD
static unsigned cancel_cases;static FILE *cancel_vectors;
static uint64_t cancel_normalize(CancelFixture *dst,const CancelFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(cancel_pointers)/8;i++){
        uint64_t *p=(uint64_t *)((unsigned char *)dst+cancel_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}}
    return mask;
}
static void cancel_fixture(CancelFixture *f,uint64_t payload,unsigned mode){
    memset(f,0,sizeof(*f));discard_fixture(&f->d,payload,mode);
    f->g=(RelocationGate){.head=(uintptr_t)&f->p,.active=1,.last_serial=1,.epoch=9};
    f->p=(RelocationPermit){.gate=(uintptr_t)&f->g,.serial=1,.epoch=9};
    f->q=(RelocationCancellation){.permit=(uintptr_t)&f->p,.serial=1,.epoch=9,
        .target=(uintptr_t)&f->d.page,.discard=(uintptr_t)&f->d.q};
}
static void cancel_compare(CancelFixture *a,RelocationCanceller step,uint64_t status){
    CancelFixture c,before,expected,x,y;memcpy(&c,a,sizeof(c));uint64_t mask=cancel_normalize(&c,a);
    for(unsigned i=0;i<sizeof(cancel_pointers)/8;i++)if(mask&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&c+cancel_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&expected,&c,sizeof(c));
    if(status!=GC_RCAN_INVALID){
        assert(status==(c.q.stage==3?GC_RCAN_DONE:GC_RCAN_MORE));
        if(!c.q.stage){
            uint64_t payload=((ObjectHeader *)(c.d.heap+16))->payload_bytes,rounded=(payload+15)&~UINT64_C(15);if(!rounded)rounded=16;
            uint64_t bytes=c.d.copy.stage?32+rounded:0,count=bytes-c.d.q.cursor;if(count>256)count=256;
            memset(expected.d.heap+2048+c.d.q.cursor,0,count);expected.d.q.owner=(uintptr_t)&c.d.q;
            expected.d.q.bytes=bytes;expected.d.q.cursor+=count;expected.q.owner=(uintptr_t)&c.q;
            if(expected.d.q.cursor==bytes)expected.q.stage=1;
        }else{
            expected.q.stage++;
            if(c.q.stage==1)atomic_store(&expected.d.source.word,3);
            if(c.q.stage==2)atomic_store(&expected.d.source.access,0);
            if(c.q.stage==3){expected.g.head=0;expected.g.active=0;expected.p=(RelocationPermit){0};}
        }
    }
    uint64_t actual=step(&a->g,&a->q),reference=gc_relocation_cancel_locked(&c.g,&c.q);
    if(actual!=status||reference!=status){fprintf(stderr,"relocation cancel case %u stage %llu got %llu C %llu expected %llu\n",cancel_cases,(unsigned long long)before.q.stage,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)status);abort();}
    assert(!memcmp(&expected,&c,sizeof(c)));memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(cancel_normalize(&x,a)==cancel_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(cancel_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t rec[]={0,reference,cancel_normalize(&x,&c),cancel_normalize(&y,&c)};
        assert(fwrite(rec,sizeof(rec),1,cancel_vectors)==1);assert(fwrite(&x,sizeof(x),1,cancel_vectors)==1);assert(fwrite(&y,sizeof(y),1,cancel_vectors)==1);}
    cancel_cases++;
}
static void gc_run_relocation_cancel_cases(RelocationCanceller step){
    CancelFixture f;cancel_cases=0;const char *path=getenv("KLASSIC_GC_RELOCATION_CANCEL_VECTORS");
    if(path){cancel_vectors=fopen(path,"wb");assert(cancel_vectors);assert(fwrite("KLRCAN01",8,1,cancel_vectors)==1);
        uint64_t h[]={sizeof(f),offsetof(CancelFixture,g),offsetof(CancelFixture,q),sizeof(cancel_pointers)/8};
        assert(fwrite(h,sizeof(h),1,cancel_vectors)==1);assert(fwrite(cancel_pointers,sizeof(cancel_pointers),1,cancel_vectors)==1);}
    const uint64_t sizes[]={0,1,7,16,17,223,224,225,257,1025};
    for(unsigned closed=0;closed<2;closed++)for(unsigned mode=0;mode<4;mode++)for(unsigned size=0;size<10;size++){
        uint64_t payload=sizes[size];if(mode==2&&payload<2)continue;
        cancel_fixture(&f,payload,mode);f.g.closed=closed;
        while(f.q.stage<4){
            uint64_t stage=f.q.stage,cursor=f.d.q.cursor;
            cancel_compare(&f,step,stage==3?GC_RCAN_DONE:GC_RCAN_MORE);
            assert(f.d.q.cursor-cursor<=256);
            if(f.q.stage<4){
                assert(f.g.active==1);f.q.owner=(uintptr_t)&f.g;
                cancel_compare(&f,step,GC_RCAN_INVALID);f.q.owner=(uintptr_t)&f.q;
            }
            /* Reopened source readers do not prevent releasing GC admission. */
            if(f.q.stage==3&&closed)atomic_store(&f.d.source.access,1);
        }
        assert(!f.g.active&&!f.p.gate&&atomic_load(&f.d.source.word)==3);
        cancel_compare(&f,step,GC_RCAN_INVALID);
    }
    for(unsigned stage=0;stage<4;stage++)for(unsigned fault=0;fault<28;fault++){
        cancel_fixture(&f,1025,3);
        while(f.q.stage<stage)assert(gc_relocation_cancel_locked(&f.g,&f.q)==GC_RCAN_MORE);
        switch(fault){
        case 0:f.q.owner=1;break;case 1:f.q.stage=4;break;case 2:f.q.target=0;break;case 3:f.q.target++;break;
        case 4:f.q.discard=0;break;case 5:f.q.discard++;break;case 6:f.d.q.source=0;break;case 7:f.d.q.source++;break;
        case 8:f.d.q.copy=0;break;case 9:f.d.q.copy++;break;case 10:f.q.serial=0;break;case 11:f.q.serial=UINT64_MAX;break;
        case 12:f.d.copy.ticket++;break;case 13:f.d.source.incarnation=0;break;case 14:f.d.copy.source_incarnation++;break;
        case 15:f.d.source.source_offset=16;break;case 16:f.d.source.source_offset++;break;case 17:f.d.source.source_offset=GC_MAX_OFFSET+1;break;
        case 18:f.q.permit=0;break;case 19:f.q.epoch++;break;case 20:f.p.job=1;break;case 21:f.p.gate=0;break;
        case 22:f.q.serial++;f.d.copy.ticket++;break;case 23:f.d.q.owner=1;break;case 24:f.d.q.cursor++;break;
        case 25:if(stage)f.q.owner=0;else f.d.q.bytes=16;break;
        case 26:atomic_store(&f.d.source.word,stage<2?((UINT64_C(2080)<<16)|0x1512):5);break;
        case 27:atomic_store(&f.d.source.access,stage<2?(GC_SOURCE_CLOSED|1):stage==2?0:GC_SOURCE_CLOSED);break;
        }
        cancel_compare(&f,step,GC_RCAN_INVALID);
    }
    cancel_fixture(&f,1025,3);atomic_store(&f.d.page.state,GC_PAGE_CLOSED);atomic_store(&f.d.page.allocated_top,3120);
    cancel_compare(&f,step,GC_RCAN_INVALID);
    assert(cancel_cases==769);
    if(cancel_vectors){assert(!fclose(cancel_vectors));cancel_vectors=NULL;}
    printf("relocation cancel %u C/candidate bounded cleanup, source reopening and permit release cases PASS\n",cancel_cases);
}
#endif
