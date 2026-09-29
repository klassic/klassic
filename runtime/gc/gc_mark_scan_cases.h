#ifndef KLASSIC_GC_MARK_SCAN_CASES_H
#define KLASSIC_GC_MARK_SCAN_CASES_H
#include "gc_mark_scan.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*MarkScanner)(MarkScan *);
typedef struct {
    MarkScan s;MarkRequest j;MarkRecord r,other;DescriptorRegistry registry;TypeDescriptor desc;
    _Atomic uint64_t descriptors[2];uint64_t bitmap;uint64_t output[257],locations[257];
    _Alignas(16) unsigned char heap[2112];
} MarkScanFixture;
static const uint64_t mark_scan_pointers[]={offsetof(MarkScanFixture,s.job),offsetof(MarkScanFixture,s.heap_base),offsetof(MarkScanFixture,s.registry),offsetof(MarkScanFixture,s.output),offsetof(MarkScanFixture,s.locations),offsetof(MarkScanFixture,j.record),offsetof(MarkScanFixture,registry.slots),offsetof(MarkScanFixture,desc.element_bitmap),offsetof(MarkScanFixture,descriptors[1]),offsetof(MarkScanFixture,heap)+16+offsetof(ObjectHeader,mark_record),offsetof(MarkScanFixture,heap)+1024+offsetof(ObjectHeader,mark_record)};
static void mark_scan_fixture(MarkScanFixture *f,unsigned length,unsigned capacity) {
    memset(f,0,sizeof(*f));f->r.object_id=17;f->bitmap=1;
    f->j=(MarkRequest){.record=(uintptr_t)&f->r,.reference=(UINT64_C(48)<<16)|0x1510,.incarnation=1,.metadata_lease=1,.index=1,.serial=1};
    f->registry=(DescriptorRegistry){(uintptr_t)f->descriptors,2};
    f->desc=(TypeDescriptor){.id=1,.alignment=8,.element_bytes=8,.element_bitmap=(uintptr_t)&f->bitmap,.element_bitmap_words=1};
    atomic_store(&f->descriptors[1],(uintptr_t)&f->desc);
    ObjectHeader *h=(ObjectHeader *)(f->heap+16);h->mark_record=(uintptr_t)&f->r;h->descriptor_id=1;h->payload_bytes=length*8;atomic_store(&h->state,GC_OBJECT_PUBLISHED);
    for(unsigned i=0;i<length;i++)atomic_store((_Atomic uint64_t *)(f->heap+48+i*8),i%3?((uint64_t)(i+1)<<20)|0x1510:0);
    memset(f->locations,0xcd,sizeof(f->locations));
    memset(f->output,0xab,sizeof(f->output));f->s=(MarkScan){(uintptr_t)&f->j,(uintptr_t)f->heap,(uintptr_t)&f->registry,(uintptr_t)f->output,capacity,55,66,77,0};
}
static uint64_t mark_scan_normalize(MarkScanFixture *dst,const MarkScanFixture *src) {
    uint64_t mask=0;for(unsigned i=0;i<sizeof(mark_scan_pointers)/8;i++){
        unsigned char *p=(unsigned char *)dst+mark_scan_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void mark_scan_copy(MarkScanFixture *dst,const MarkScanFixture *src) {
    memcpy(dst,src,sizeof(*dst));uint64_t mask=mark_scan_normalize(dst,src);
    for(unsigned i=0;i<sizeof(mark_scan_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+mark_scan_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static FILE *mark_scan_vectors;static unsigned mark_scan_cases;
static void mark_scan_compare(MarkScanFixture *a,MarkScanFixture *c,MarkScanner scan,uint64_t expected) {
    MarkScanFixture before,x,y;memcpy(&before,c,sizeof(before));
    uint64_t actual=scan(&a->s),reference=gc_mark_scan_slice(&c->s);
    if(actual!=expected||reference!=expected){fprintf(stderr,"mark scan %u got %llu C %llu expected %llu\n",mark_scan_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(expected==GC_SCAN_INVALID)assert(!memcmp(&before,c,sizeof(before)));
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(mark_scan_normalize(&x,a)==mark_scan_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(mark_scan_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t record[]={0,reference,mark_scan_normalize(&x,c),mark_scan_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,mark_scan_vectors)==1);assert(fwrite(&x,sizeof(x),1,mark_scan_vectors)==1);assert(fwrite(&y,sizeof(y),1,mark_scan_vectors)==1);}
    mark_scan_cases++;
}
static void gc_run_mark_scan_cases(MarkScanner scan) {
    MarkScanFixture a,c;mark_scan_cases=0;const char *path=getenv("KLASSIC_GC_MARK_SCAN_VECTORS");
    if(path){mark_scan_vectors=fopen(path,"wb");assert(mark_scan_vectors);assert(fwrite("KLMSCN01",8,1,mark_scan_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(MarkScanFixture,s),offsetof(MarkScanFixture,s),sizeof(mark_scan_pointers)/8};
        assert(fwrite(header,sizeof(header),1,mark_scan_vectors)==1);assert(fwrite(mark_scan_pointers,sizeof(mark_scan_pointers),1,mark_scan_vectors)==1);}
    for(unsigned length=0;length<=256;length++)for(unsigned mode=0;mode<3;mode++){
        unsigned capacity=mode==0?1:mode==1?7:256;mark_scan_fixture(&a,length,capacity);if(mode)a.s.locations=(uintptr_t)a.locations;mark_scan_copy(&c,&a);
        unsigned count=length<capacity?length:capacity;mark_scan_compare(&a,&c,scan,count==length?0:1);
        assert(a.s.written==count&&a.s.examined==count&&a.s.next_cursor==count*8&&!a.j.cursor);
        for(unsigned i=0;i<count;i++)assert(a.output[i]==(i%3?((uint64_t)(i+1)<<20)|0x1510:0));
        for(unsigned i=0;i<count;i++)assert(a.locations[i]==(mode?i*8:UINT64_C(0xcdcdcdcdcdcdcdcd)));
        assert(a.locations[count]==UINT64_C(0xcdcdcdcdcdcdcdcd));
        assert(a.output[count]==UINT64_C(0xabababababababab));
        a.j.cursor=c.j.cursor=length*8;mark_scan_compare(&a,&c,scan,0);assert(!a.s.written);
    }
    const unsigned scalar_lengths[]={1,7,9,255,2051};
    for(unsigned i=0;i<sizeof(scalar_lengths)/sizeof(scalar_lengths[0]);i++){
        mark_scan_fixture(&a,0,1);a.desc.element_bytes=1;a.desc.element_bitmap=a.desc.element_bitmap_words=0;
        ((ObjectHeader *)(a.heap+16))->payload_bytes=scalar_lengths[i];mark_scan_copy(&c,&a);
        mark_scan_compare(&a,&c,scan,GC_SCAN_DONE);assert(a.s.next_cursor==scalar_lengths[i]&&!a.s.written&&!a.s.examined&&!a.j.cursor);
    }
    for(unsigned fault=0;fault<20;fault++) {
        mark_scan_fixture(&a,16,4);ObjectHeader *h=(ObjectHeader *)(a.heap+16);
        switch(fault){case 0:a.s.job=0;break;case 1:a.s.job++;break;case 2:a.s.registry=0;break;case 3:a.s.heap_base++;break;
        case 4:a.j.index=0;break;case 5:a.j.serial=0;break;case 6:a.j.record=0;break;case 7:a.j.record++;break;
        case 8:a.j.incarnation=0;break;case 9:a.j.metadata_lease=0;break;case 10:a.j.reference=0;break;case 11:a.j.reference|=1;break;
        case 12:a.s.heap_base=UINT64_MAX-15;break;case 13:atomic_store(&h->state,GC_OBJECT_INITIALIZING);break;case 14:h->mark_record++;break;
        case 15:h->payload_bytes=UINT64_MAX;break;case 16:a.s.capacity=0;break;case 17:a.s.capacity=257;break;case 18:a.j.cursor=129;break;case 19:a.j.cursor=1;break;}
        mark_scan_copy(&c,&a);mark_scan_compare(&a,&c,scan,GC_SCAN_INVALID);
    }
    for(unsigned fault=0;fault<5;fault++){
        mark_scan_fixture(&a,16,4);a.s.locations=(uintptr_t)a.locations;
        switch(fault){case 0:a.s.locations++;break;case 1:a.s.locations=UINT64_MAX-7;break;
        case 2:a.s.locations=a.s.output;break;case 3:a.s.locations=a.s.output+8;break;case 4:a.s.locations=a.s.output-8;break;}
        mark_scan_copy(&c,&a);mark_scan_compare(&a,&c,scan,GC_SCAN_INVALID);
    }
    // Mixed layouts retain actual byte offsets, including null reference slots.
    for(unsigned part=0;part<3;part++){
        mark_scan_fixture(&a,3,part?1:3);a.desc.element_bytes=24;a.bitmap=5;
        a.s.locations=(uintptr_t)a.locations;a.j.cursor=part*8;
        mark_scan_copy(&c,&a);mark_scan_compare(&a,&c,scan,part==1?GC_SCAN_MORE:GC_SCAN_DONE);
        assert(a.s.written==(part==0?2:part==1?0:1));
        assert(a.s.next_cursor==(part==1?16:24));
        if(part==0){assert(a.locations[0]==0&&a.output[0]==0);assert(a.locations[1]==16&&a.output[1]==((UINT64_C(3)<<20)|0x1510));}
        if(part==2)assert(a.locations[0]==16&&a.output[0]==((UINT64_C(3)<<20)|0x1510));
        assert(a.locations[a.s.written]==UINT64_C(0xcdcdcdcdcdcdcdcd));
    }
    // Identity numbers alone cannot substitute a different logical record.
    mark_scan_fixture(&a,16,16);a.other.object_id=a.r.object_id;
    ((ObjectHeader *)(a.heap+16))->mark_record=(uintptr_t)&a.other;
    mark_scan_copy(&c,&a);mark_scan_compare(&a,&c,scan,GC_SCAN_INVALID);
    // Two physical copies retain exactly one stable mark record. This checks
    // scanner/header representation, not the future concurrent copy protocol.
    mark_scan_fixture(&a,16,16);a.r.young_epoch=9;a.r.old_epoch=12;
    memcpy(a.heap+1024,a.heap+16,sizeof(ObjectHeader)+128);
    mark_scan_copy(&c,&a);mark_scan_compare(&a,&c,scan,GC_SCAN_DONE);
    a.j.reference=c.j.reference=(UINT64_C(1056)<<16)|0x1510;
    mark_scan_compare(&a,&c,scan,GC_SCAN_DONE);
    assert(a.r.young_epoch==9&&a.r.old_epoch==12);
    if(mark_scan_vectors){assert(fclose(mark_scan_vectors)==0);mark_scan_vectors=NULL;}
    printf("mark slices %u C/candidate bounded atomic heap-scan cases PASS\n",mark_scan_cases);
}
#endif
