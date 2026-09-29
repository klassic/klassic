#ifndef KLASSIC_GC_RELOCATE_CASES_H
#define KLASSIC_GC_RELOCATE_CASES_H
#include "gc_relocate.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RelocateCopy)(ForwardCell *,RelocationCopy *);
typedef struct {ForwardCell g;RelocationCopy q;MarkRecord record;_Alignas(16) unsigned char heap[8192];} CopyFixture;
static const uint64_t copy_pointers[]={offsetof(CopyFixture,q.heap_base),offsetof(CopyFixture,heap)+32,offsetof(CopyFixture,heap)+4096-16};
static FILE *copy_vectors;static unsigned copy_cases;
static void copy_fixture(CopyFixture *f,uint64_t bytes,unsigned reversed) {
    memset(f,0,sizeof(*f));memset(f->heap,0xa5,sizeof(f->heap));
    uint64_t source=reversed?4096:48,destination=reversed?48:4096;
    f->g=(ForwardCell){source,27,5,GC_SOURCE_CLOSED};
    f->q=(RelocationCopy){(uintptr_t)f->heap,27,1,reversed?8192:3072,destination,reversed?3072:8192,256,0,0};
    f->record.object_id=42;f->record.old_epoch=13;
    ObjectHeader *header=(ObjectHeader *)(f->heap+source-32);
    *header=(ObjectHeader){bytes,17,(uintptr_t)&f->record,GC_OBJECT_PUBLISHED};
    memset(f->heap+destination-32,0,32);
    for(uint64_t i=0;i<bytes;i++)f->heap[source+i]=(unsigned char)(i*17+5);
}
static uint64_t copy_normalize(CopyFixture *f,const CopyFixture *owner) {
    uint64_t mask=0;
    for(unsigned i=0;i<3;i++){
        unsigned char *p=(unsigned char *)f+copy_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)owner&&v-(uintptr_t)owner<sizeof(*owner)){v-=(uintptr_t)owner;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}
    }return mask;
}
static void copy_clone(CopyFixture *dst,const CopyFixture *src){
    memcpy(dst,src,sizeof(*dst));uint64_t mask=copy_normalize(dst,src);
    for(unsigned i=0;i<3;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+copy_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void copy_check(const CopyFixture *before,const CopyFixture *after,uint64_t status){
    if(status==GC_COPY_INVALID||status==GC_COPY_WAIT){assert(!memcmp(before,after,sizeof(*before)));return;}
    CopyFixture expected;memcpy(&expected,before,sizeof(expected));
    if(before->q.stage!=2){
        const ObjectHeader *src=(const ObjectHeader *)(before->heap+before->g.source_offset-32);
        ObjectHeader *dst=(ObjectHeader *)(expected.heap+before->q.destination-32);
        if(!before->q.stage){dst->payload_bytes=src->payload_bytes;dst->descriptor_id=src->descriptor_id;dst->mark_record=src->mark_record;atomic_store(&dst->state,GC_OBJECT_INITIALIZING);}
        uint64_t n=src->payload_bytes-before->q.cursor;if(n>before->q.budget)n=before->q.budget;
        assert(n<=GC_COPY_MAX_BYTES);
        memcpy(expected.heap+before->q.destination+before->q.cursor,before->heap+before->g.source_offset+before->q.cursor,(size_t)n);
        expected.q.cursor+=n;expected.q.stage=expected.q.cursor==src->payload_bytes?2:1;
        if(expected.q.stage==2)atomic_store(&dst->state,GC_OBJECT_PUBLISHED);
    }
    assert(status==(expected.q.stage==2?GC_COPY_DONE:GC_COPY_MORE));
    assert(!memcmp(&expected,after,sizeof(expected)));
}
static void copy_compare(CopyFixture *a,CopyFixture *c,RelocateCopy step,uint64_t expected){
    CopyFixture ba,bc,x,y;memcpy(&ba,a,sizeof(ba));memcpy(&bc,c,sizeof(bc));
    uint64_t actual=step(&a->g,&a->q),reference=gc_relocate_copy(&c->g,&c->q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"copy case %u got %llu C %llu expected %llu\n",copy_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    copy_check(&ba,a,actual);copy_check(&bc,c,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(copy_normalize(&x,a)==copy_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(copy_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t record[]={0,reference,copy_normalize(&x,c),copy_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,copy_vectors)==1);assert(fwrite(&x,sizeof(x),1,copy_vectors)==1);assert(fwrite(&y,sizeof(y),1,copy_vectors)==1);}
    copy_cases++;
}
static void gc_run_relocate_cases(RelocateCopy step){
    CopyFixture a,c;copy_cases=0;const char *path=getenv("KLASSIC_GC_COPY_VECTORS");
    if(path){copy_vectors=fopen(path,"wb");assert(copy_vectors);assert(fwrite("KLRCOP01",8,1,copy_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(CopyFixture,g),offsetof(CopyFixture,q),3};assert(fwrite(header,sizeof(header),1,copy_vectors)==1);assert(fwrite(copy_pointers,sizeof(copy_pointers),1,copy_vectors)==1);}
#define COPY(result) copy_compare(&a,&c,step,result)
    const uint64_t lengths[]={0,7,16,255,256,257,1025},budgets[]={1,7,128,256};
    for(unsigned reversed=0;reversed<2;reversed++)for(unsigned size=0;size<7;size++)for(unsigned budget=0;budget<4;budget++) {
        copy_fixture(&a,lengths[size],reversed);a.q.budget=budgets[budget];copy_clone(&c,&a);
        do {COPY(lengths[size]-a.q.cursor>a.q.budget?GC_COPY_MORE:GC_COPY_DONE);}while(a.q.stage!=2);
        COPY(GC_COPY_DONE); // completion is idempotent before forwarding publish
    }
    for(unsigned fault=0;fault<39;fault++) {
        copy_fixture(&a,17,0);uint64_t expected=GC_COPY_INVALID;
        ObjectHeader *from=(ObjectHeader *)(a.heap+16),*to=(ObjectHeader *)(a.heap+4096-32);
        switch(fault){case 0:a.g.incarnation=0;break;case 1:a.q.source_incarnation++;break;case 2:a.q.ticket=0;break;
        case 3:a.q.ticket=UINT64_MAX;break;case 4:a.q.ticket=2;break;case 5:atomic_store(&a.g.word,0);break;
        case 6:atomic_store(&a.g.access,0);expected=GC_COPY_WAIT;break;case 7:atomic_store(&a.g.access,GC_SOURCE_CLOSED+1);expected=GC_COPY_WAIT;break;
        case 8:a.g.source_offset=32;break;case 9:a.q.destination=32;break;case 10:a.g.source_offset++;break;
        case 11:a.q.destination++;break;case 12:a.q.heap_base++;break;case 13:a.q.source_limit=UINT64_MAX;break;case 14:a.q.destination_limit=UINT64_MAX;break;
        case 15:a.q.source_limit=48;break;case 16:a.q.destination_limit=4096;break;case 17:a.q.heap_base=UINT64_MAX-15;break;
        case 18:a.q.budget=0;break;case 19:a.q.budget=257;break;case 20:a.q.stage=3;break;case 21:atomic_store(&from->state,GC_OBJECT_INITIALIZING);break;
        case 22:from->payload_bytes=UINT64_MAX;break;case 23:from->descriptor_id=0;break;case 24:from->descriptor_id=UINT64_C(1)<<32;break;
        case 25:from->mark_record=0;break;case 26:from->mark_record++;break;
        case 27:a.q.source_limit=48+17;break;case 28:a.q.destination_limit=4096+17;break;
        case 29:a.q.destination=48;break;case 30:a.q.destination=64;break;case 31:a.g.source_offset=4096;a.q.source_limit=8192;*to=*from;break;
        case 32:a.q.cursor=1;break;case 33:atomic_store(&to->state,GC_OBJECT_PUBLISHED);break;
        case 34:to->payload_bytes=1;break;case 35:to->descriptor_id=1;break;case 36:to->mark_record=(uintptr_t)&a.record;break;
        case 37:atomic_store(&a.g.word,3);break;case 38:atomic_store(&a.g.word,(UINT64_C(4096)<<16)|0x1512);break;}
        copy_clone(&c,&a);COPY(expected);
    }
    for(unsigned fault=0;fault<9;fault++) {
        copy_fixture(&a,17,0);a.q.budget=1;copy_clone(&c,&a);COPY(GC_COPY_MORE);
        ObjectHeader *to=(ObjectHeader *)(a.heap+4096-32);
        switch(fault){case 0:a.q.cursor=0;break;case 1:a.q.cursor=17;break;case 2:a.q.cursor=UINT64_MAX;break;
        case 3:atomic_store(&to->state,GC_OBJECT_PUBLISHED);break;case 4:to->payload_bytes++;break;case 5:to->descriptor_id++;break;case 6:to->mark_record=0;break;
        case 7:a.q.stage=2;break;case 8:a.q.cursor=17;a.q.stage=2;break;}
        copy_clone(&c,&a);COPY(GC_COPY_INVALID);
    }
    if(copy_vectors){assert(fclose(copy_vectors)==0);copy_vectors=NULL;}
    printf("relocation copy %u C/candidate bounded payload and preservation cases PASS\n",copy_cases);
#undef COPY
}
typedef uint64_t (*RelocatePublish)(ForwardCell *,RelocationCopy *,uint64_t);
static void publish_compare(CopyFixture *a,CopyFixture *c,RelocatePublish publish,uint64_t color,uint64_t expected,FILE *vectors){
    CopyFixture ba,bc,x,y;memcpy(&ba,a,sizeof(ba));memcpy(&bc,c,sizeof(bc));
    uint64_t result=publish(&a->g,&a->q,color),reference=gc_relocate_publish(&c->g,&c->q,color);
    assert(result==expected&&reference==expected);
    if(!result){atomic_store(&ba.g.word,(ba.q.destination<<16)|color|2);atomic_store(&bc.g.word,(bc.q.destination<<16)|color|2);}
    assert(!memcmp(&ba,a,sizeof(ba))&&!memcmp(&bc,c,sizeof(bc)));
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(copy_normalize(&x,a)==copy_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(vectors){
        // Restore the pre-publication word for successful records.
        if(!result)atomic_store(&bc.g.word,(bc.q.ticket<<2)|1);
        memcpy(&x,&bc,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t record[]={color,reference,copy_normalize(&x,c),copy_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,vectors)==1);assert(fwrite(&x,sizeof(x),1,vectors)==1);assert(fwrite(&y,sizeof(y),1,vectors)==1);
    }
}
static void gc_run_relocate_publish_cases(RelocateCopy copy,RelocatePublish publish){
    CopyFixture a,c;unsigned cases=0;FILE *vectors=NULL;const char *path=getenv("KLASSIC_GC_COPY_PUBLISH_VECTORS");
    if(path){vectors=fopen(path,"wb");assert(vectors);assert(fwrite("KLRPUB01",8,1,vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(CopyFixture,g),offsetof(CopyFixture,q),3};assert(fwrite(header,sizeof(header),1,vectors)==1);assert(fwrite(copy_pointers,sizeof(copy_pointers),1,vectors)==1);}
#define PUBLISH(color,expected) do{publish_compare(&a,&c,publish,color,expected,vectors);cases++;}while(0)
    for(unsigned remembered=1;remembered<4;remembered++)for(unsigned young=0;young<2;young++)for(unsigned old=0;old<2;old++)for(unsigned remap=0;remap<4;remap++) {
        uint64_t color=(remembered<<4)|(1u<<(young+8))|(1u<<(old+10))|(1u<<(remap+12));
        copy_fixture(&a,257,0);copy_clone(&c,&a);PUBLISH(color,GC_COPY_INVALID);
        assert(copy(&a.g,&a.q)==GC_COPY_MORE&&gc_relocate_copy(&c.g,&c.q)==GC_COPY_MORE);PUBLISH(color,GC_COPY_INVALID);
        assert(copy(&a.g,&a.q)==GC_COPY_DONE&&gc_relocate_copy(&c.g,&c.q)==GC_COPY_DONE);PUBLISH(color,GC_COPY_DONE);
        PUBLISH(color,GC_COPY_INVALID); // cannot republish after losing claim ownership
    }
    for(unsigned fault=0;fault<25;fault++) {
        copy_fixture(&a,7,0);assert(copy(&a.g,&a.q)==GC_COPY_DONE);uint64_t color=0x1510,expected=GC_COPY_INVALID;
        if(fault<16)color^=UINT64_C(1)<<fault;
        else switch(fault){case 16:color=UINT64_C(1)<<32;break;case 17:color=0;break;case 18:a.q.cursor--;break;
        case 19:atomic_store(&((ObjectHeader *)(a.heap+4096-32))->state,GC_OBJECT_INITIALIZING);break;
        case 20:a.q.source_incarnation++;break;case 21:a.q.ticket++;break;case 22:atomic_store(&a.g.access,GC_SOURCE_CLOSED+1);expected=GC_COPY_WAIT;break;
        case 23:a.q.destination=GC_MAX_OFFSET+1;break;case 24:atomic_store(&a.g.word,3);break;}
        if(fault<16){GcRef ref;if(gc_abi_encode_ref(a.q.destination,(uint16_t)color,&ref))expected=GC_COPY_DONE;}
        copy_clone(&c,&a);PUBLISH(color,expected);
    }
    if(vectors)assert(fclose(vectors)==0);
    printf("relocation publish %u C/candidate complete-copy and ownership cases PASS\n",cases);
#undef PUBLISH
}
#endif
