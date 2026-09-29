#ifndef KLASSIC_GC_RELOCATE_DISCARD_CASES_H
#define KLASSIC_GC_RELOCATE_DISCARD_CASES_H
#include "gc_relocate_discard.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RelocationDiscarder)(AllocationPage *,RelocationDiscard *);
typedef struct {AllocationPage page;ForwardCell source;RelocationCopy copy;RelocationDiscard q;_Alignas(16) unsigned char heap[4096];} DiscardFixture;
static const uint64_t discard_pointers[]={offsetof(DiscardFixture,page.heap_base),offsetof(DiscardFixture,copy.heap_base),
    offsetof(DiscardFixture,q.owner),offsetof(DiscardFixture,q.source),offsetof(DiscardFixture,q.copy),
    offsetof(DiscardFixture,heap)+16+offsetof(ObjectHeader,mark_record),offsetof(DiscardFixture,heap)+2048+offsetof(ObjectHeader,mark_record)};
static unsigned discard_cases;static FILE *discard_vectors;
static uint64_t discard_normalize(DiscardFixture *dst,const DiscardFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(discard_pointers)/8;i++){
        uint64_t *p=(uint64_t *)((unsigned char *)dst+discard_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}}
    return mask;
}
static void discard_fixture(DiscardFixture *f,uint64_t payload,unsigned mode){
    memset(f,0,sizeof(*f));memset(f->heap,0x77,sizeof(f->heap));memset(f->heap+2048,0,2048);
    f->page=(AllocationPage){.heap_base=(uintptr_t)f->heap,.start=2048,.limit=4096,.incarnation=9,.state=GC_PAGE_READY,.allocated_top=2048};
    f->source=(ForwardCell){.source_offset=48,.incarnation=7,.word=5,.access=GC_SOURCE_CLOSED};
    f->copy=(RelocationCopy){(uintptr_t)f->heap,7,1,2048,mode?2080:0,mode?4096:0,256,mode==2?1:mode==3?payload:0,mode>=2?mode-1:0};
    f->q=(RelocationDiscard){.source=(uintptr_t)&f->source,.copy=(uintptr_t)&f->copy};
    ObjectHeader *h=(ObjectHeader *)(f->heap+16);*h=(ObjectHeader){.payload_bytes=payload,.descriptor_id=1,.mark_record=(uintptr_t)(f->heap+4080),.state=GC_OBJECT_PUBLISHED};
    if(mode>=2){uint64_t rounded=(payload+15)&~UINT64_C(15);if(!rounded)rounded=16;
        memset(f->heap+2048,0xa5,32+rounded);ObjectHeader *to=(ObjectHeader *)(f->heap+2048);*to=*h;
        atomic_store(&to->state,mode==2?GC_OBJECT_INITIALIZING:GC_OBJECT_PUBLISHED);}
}
static void discard_compare(DiscardFixture *a,RelocationDiscarder step,uint64_t status){
    DiscardFixture c,before,expected,x,y;memcpy(&c,a,sizeof(c));uint64_t mask=discard_normalize(&c,a);
    for(unsigned i=0;i<sizeof(discard_pointers)/8;i++)if(mask&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&c+discard_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&expected,&c,sizeof(c));
    if(status!=GC_DISCARD_INVALID){
        uint64_t payload=((ObjectHeader *)(c.heap+16))->payload_bytes,rounded=(payload+15)&~UINT64_C(15);if(!rounded)rounded=16;
        uint64_t bytes=c.copy.stage?32+rounded:0,count=bytes-c.q.cursor;if(count>256)count=256;
        memset(expected.heap+2048+c.q.cursor,0,count);expected.q.owner=(uintptr_t)&c.q;expected.q.bytes=bytes;expected.q.cursor+=count;
        assert(status==(expected.q.cursor==bytes?GC_DISCARD_DONE:GC_DISCARD_MORE));
    }
    uint64_t actual=step(&a->page,&a->q),reference=gc_relocate_discard_locked(&c.page,&c.q);
    if(actual!=status||reference!=status){fprintf(stderr,"relocation discard case %u got %llu C %llu expected %llu\n",discard_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)status);abort();}
    assert(!memcmp(&expected,&c,sizeof(c)));memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(discard_normalize(&x,a)==discard_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(discard_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t rec[]={0,reference,discard_normalize(&x,&c),discard_normalize(&y,&c)};
        assert(fwrite(rec,sizeof(rec),1,discard_vectors)==1);assert(fwrite(&x,sizeof(x),1,discard_vectors)==1);assert(fwrite(&y,sizeof(y),1,discard_vectors)==1);}
    discard_cases++;
}
static void gc_run_relocate_discard_cases(RelocationDiscarder step){
    DiscardFixture f;discard_cases=0;const char *path=getenv("KLASSIC_GC_RELOCATE_DISCARD_VECTORS");
    if(path){discard_vectors=fopen(path,"wb");assert(discard_vectors);assert(fwrite("KLRDSC01",8,1,discard_vectors)==1);
        uint64_t h[]={sizeof(f),offsetof(DiscardFixture,page),offsetof(DiscardFixture,q),sizeof(discard_pointers)/8};
        assert(fwrite(h,sizeof(h),1,discard_vectors)==1);assert(fwrite(discard_pointers,sizeof(discard_pointers),1,discard_vectors)==1);}
    const uint64_t sizes[]={0,1,7,16,17,223,224,225,257,1025};
    for(unsigned closed=0;closed<2;closed++)for(unsigned mode=0;mode<4;mode++)for(unsigned size=0;size<10;size++){
        uint64_t payload=sizes[size];if(mode==2&&payload<2)continue;
        discard_fixture(&f,payload,mode);if(closed)atomic_store(&f.page.state,GC_PAGE_CLOSED);
        uint64_t rounded=(payload+15)&~UINT64_C(15);if(!rounded)rounded=16;uint64_t bytes=mode>=2?32+rounded:0;
        do{discard_compare(&f,step,bytes-f.q.cursor>256?GC_DISCARD_MORE:GC_DISCARD_DONE);}while(f.q.cursor<bytes);
        discard_compare(&f,step,GC_DISCARD_DONE);
        f.q.owner=(uintptr_t)&f.source;discard_compare(&f,step,GC_DISCARD_INVALID);f.q.owner=(uintptr_t)&f.q;
        atomic_store(&f.source.word,3);discard_compare(&f,step,GC_DISCARD_INVALID);
    }
    for(unsigned fault=0;fault<58;fault++){
        discard_fixture(&f,1025,3);ObjectHeader *h=(ObjectHeader *)(f.heap+16);
        switch(fault){case 0:f.q.source=0;break;case 1:f.q.source++;break;case 2:f.q.copy=0;break;case 3:f.q.copy++;break;
        case 4:f.q.owner=1;break;case 5:f.q.cursor=16;break;case 6:f.q.bytes=16;break;case 7:f.copy.ticket=0;break;
        case 8:f.copy.ticket=UINT64_MAX;break;case 9:f.copy.source_incarnation++;break;case 10:f.source.incarnation=0;break;
        case 11:atomic_store(&f.source.word,0);break;case 12:atomic_store(&f.source.word,9);break;case 13:atomic_store(&f.source.word,(UINT64_C(2080)<<16)|0x1512);break;
        case 14:atomic_store(&f.source.access,0);break;case 15:atomic_store(&f.source.access,GC_SOURCE_CLOSED|1);break;case 16:f.copy.stage=3;break;
        case 17:atomic_store(&f.page.state,GC_PAGE_ALLOCATING);break;case 18:f.page.owner=1;break;case 19:f.page.incarnation=0;break;
        case 20:f.page.start=0;break;case 21:f.page.start=f.page.limit;break;case 22:f.page.limit=GC_MAX_OFFSET+1;break;
        case 23:f.page.heap_base++;break;case 24:f.page.start++;break;case 25:f.page.limit++;break;case 26:atomic_store(&f.page.allocated_top,2049);break;
        case 27:atomic_store(&f.page.allocated_top,2032);break;case 28:atomic_store(&f.page.allocated_top,4112);break;case 29:atomic_store(&f.page.allocated_top,2064);break;
        case 30:f.page.heap_base=f.copy.heap_base=UINT64_MAX&~UINT64_C(15);break;case 31:f.copy.heap_base+=16;break;
        case 32:f.copy.destination+=16;break;case 33:f.copy.destination_limit-=16;break;case 34:f.page.limit=f.copy.destination_limit=2080;break;
        case 35:f.copy.destination=0;break;case 36:f.source.source_offset=16;break;case 37:f.source.source_offset++;break;
        case 38:f.copy.source_limit=GC_MAX_OFFSET+1;break;case 39:f.source.source_offset=f.copy.source_limit;break;
        case 40:f.page.heap_base=f.copy.heap_base=(UINT64_MAX-8191)&~UINT64_C(15);f.copy.source_limit=GC_MAX_OFFSET;break;
        case 41:atomic_store(&h->state,0);break;case 42:h->payload_bytes=2048;break;case 43:h->descriptor_id=0;break;
        case 44:h->descriptor_id=UINT64_C(1)<<32;break;case 45:h->mark_record=0;break;case 46:h->mark_record++;break;
        case 47:h->payload_bytes=f.copy.cursor=1953;f.copy.source_limit=2001;break;
        case 48:f.page.start=64;atomic_store(&f.page.allocated_top,64);f.copy.destination=96;break;
        case 49:f.copy.stage=1;f.copy.cursor=0;break;case 50:f.copy.stage=1;break;case 51:f.copy.cursor--;break;
        case 52:f.q.owner=(uintptr_t)&f.q;f.q.bytes=48;break;case 53:f.q.owner=(uintptr_t)&f.q;f.q.bytes=1072;f.q.cursor=1;break;
        case 54:f.q.owner=(uintptr_t)&f.q;f.q.bytes=1072;f.q.cursor=1088;break;case 55:f.copy.stage=0;break;
        case 56:atomic_store(&f.page.state,GC_PAGE_CLOSED);atomic_store(&f.page.allocated_top,3120);break;
        case 57:atomic_store(&f.page.state,GC_PAGE_CLOSED);atomic_store(&f.page.allocated_top,3120);f.q.owner=(uintptr_t)&f.q;f.q.bytes=1072;f.q.cursor=256;break;}
        discard_compare(&f,step,GC_DISCARD_INVALID);
    }
    assert(discard_cases==386);if(discard_vectors){assert(!fclose(discard_vectors));discard_vectors=NULL;}
    puts("relocation discard 386 C/candidate bounded private cleanup and installed-target rejection cases PASS");
}
#endif
