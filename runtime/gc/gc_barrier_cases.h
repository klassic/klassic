#ifndef KLASSIC_GC_BARRIER_CASES_H
#define KLASSIC_GC_BARRIER_CASES_H
#include "gc_barrier.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*GuardedStorer)(GuardedStore *);
typedef uint64_t (*GuardedHealer)(const GuardedHeal *);
typedef struct {
    GuardedStore q;SourceAccessRequest scope;MutatorContext ctx;AllocationPage page;ForwardCell cell;
    DescriptorRegistry registry;TypeDescriptor descriptor;_Atomic uint64_t slots[2];uint64_t bitmap;
    MarkRecord record;BarrierEntry satb[3],remembered[3];_Alignas(16) unsigned char heap[8192];
} StoreFixture;
static const uint64_t store_pointers[]={offsetof(StoreFixture,q.scope),offsetof(StoreFixture,q.page),offsetof(StoreFixture,q.registry),
    offsetof(StoreFixture,scope.context),offsetof(StoreFixture,scope.held),offsetof(StoreFixture,ctx.satb.entries),offsetof(StoreFixture,ctx.remembered.entries),
    offsetof(StoreFixture,page.heap_base),offsetof(StoreFixture,registry.slots),offsetof(StoreFixture,descriptor.fixed_bitmap),offsetof(StoreFixture,slots)+8,offsetof(StoreFixture,heap)+32};
static unsigned store_cases;static FILE *store_vectors;
static void store_fixture(StoreFixture *f,uint64_t phase,unsigned generation,uint64_t previous){
    memset(f,0,sizeof(*f));f->scope=(SourceAccessRequest){(uintptr_t)&f->ctx,48,27,(uintptr_t)&f->cell,0};
    f->ctx=(MutatorContext){.abi_version=GC_ABI_VERSION,.thread_id=17,.adopted_phase=phase,.access_epoch=27};
    f->ctx.satb=(BufferCursor){(uintptr_t)f->satb,3,0,0,0};f->ctx.remembered=(BufferCursor){(uintptr_t)f->remembered,3,0,0,0};
    f->page=(AllocationPage){.heap_base=(uintptr_t)f->heap,.start=16,.limit=8192,.incarnation=27,.generation=generation,.state=GC_PAGE_CLOSED,.allocated_top=80};
    f->cell=(ForwardCell){48,27,0,1};f->registry=(DescriptorRegistry){(uintptr_t)f->slots,2};
    f->bitmap=3;f->descriptor=(TypeDescriptor){.id=1,.alignment=16,.fixed_bytes=24,.fixed_bitmap=(uintptr_t)&f->bitmap,.fixed_bitmap_words=1};
    atomic_store(&f->slots[1],(uintptr_t)&f->descriptor);
    ObjectHeader *h=(ObjectHeader *)(f->heap+16);f->record.object_id=42;*h=(ObjectHeader){24,1,(uintptr_t)&f->record,GC_OBJECT_PUBLISHED};
    atomic_store((_Atomic uint64_t *)(f->heap+48),previous);
    f->q=(GuardedStore){.scope=(uintptr_t)&f->scope,.page=(uintptr_t)&f->page,.registry=(uintptr_t)&f->registry,
        .expected=previous,.desired=(UINT64_C(144)<<16)|gc_abi_color((PhaseSnapshot){phase}),.phase=phase};
    f->q.satb=(BarrierEntry){{previous},{{(UINT64_C(48)<<16)|0x1510},0,27,phase,11}};
    f->q.remembered=f->q.satb;f->q.remembered.slot.metadata_lease=12;
}
static uint64_t store_normalize(StoreFixture *f,const StoreFixture *owner){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(store_pointers)/sizeof(*store_pointers);i++){
        unsigned char *p=(unsigned char *)f+store_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)owner&&v-(uintptr_t)owner<sizeof(*owner)){v-=(uintptr_t)owner;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}
    }return mask;
}
static void store_clone(StoreFixture *d,const StoreFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=store_normalize(d,s);
    for(unsigned i=0;i<sizeof(store_pointers)/sizeof(*store_pointers);i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)d+store_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);
    }
}
static uint64_t store_mask(const StoreFixture *f){uint64_t y=f->q.phase&15,o=(f->q.phase>>4)&15;
    return (f->q.expected&&((y>=1&&y<=3)||(o>=1&&o<=3))?1:0)|(f->page.generation==GC_OLD?2:0);
}
static void store_check(const StoreFixture *before,const StoreFixture *after,uint64_t status){
    StoreFixture e;memcpy(&e,before,sizeof(e));
    if(status==GC_STORE_OK){uint64_t mask=store_mask(before);
        if(mask&1){e.satb[e.ctx.satb.top%3]=e.q.satb;e.ctx.satb.top++;atomic_store(&e.ctx.satb.published,e.ctx.satb.top);}
        if(mask&2){e.remembered[e.ctx.remembered.top%3]=e.q.remembered;e.ctx.remembered.top++;atomic_store(&e.ctx.remembered.published,e.ctx.remembered.top);}
        e.q.transferred=mask;atomic_store((_Atomic uint64_t *)(e.heap+48+e.q.byte_offset),e.q.desired);
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void store_compare(StoreFixture *a,GuardedStorer step,uint64_t expected){
    StoreFixture c,ba,bc,x,y;store_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->q),reference=gc_guarded_store(&c.q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"guarded store case %u got %llu C %llu expected %llu\n",store_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    store_check(&ba,a,actual);store_check(&bc,&c,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(store_normalize(&x,a)==store_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(store_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,store_normalize(&x,&c),store_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,store_vectors)==1);assert(fwrite(&x,sizeof(x),1,store_vectors)==1);assert(fwrite(&y,sizeof(y),1,store_vectors)==1);}
    store_cases++;
}
static void gc_run_guarded_store_cases(GuardedStorer step){
    StoreFixture a;store_cases=0;const char *path=getenv("KLASSIC_GC_STORE_VECTORS");
    if(path){store_vectors=fopen(path,"wb");assert(store_vectors);assert(fwrite("KLGSTR01",8,1,store_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(StoreFixture,q),offsetof(StoreFixture,q),sizeof(store_pointers)/sizeof(*store_pointers)};
        assert(fwrite(header,sizeof(header),1,store_vectors)==1);assert(fwrite(store_pointers,sizeof(store_pointers),1,store_vectors)==1);}
    uint64_t old=(UINT64_C(128)<<16)|0x1510;
    for(unsigned young=0;young<8;young++)for(unsigned older=0;older<8;older++)for(unsigned gen=0;gen<2;gen++)for(unsigned previous=0;previous<2;previous++){
        store_fixture(&a,young|(older<<4),gen,previous?old:0);store_compare(&a,step,GC_STORE_OK);
    }
    for(unsigned epochs=0;epochs<32;epochs++)for(unsigned kind=0;kind<3;kind++){
        store_fixture(&a,(UINT64_C(123)<<16)|(epochs<<8)|0x22,GC_OLD,old);
        if(kind==0)a.q.desired=0;
        if(kind==1){a.q.expected=a.q.desired;atomic_store((_Atomic uint64_t *)(a.heap+48),a.q.expected);a.q.satb.previous.bits=a.q.remembered.previous.bits=a.q.expected;}
        store_compare(&a,step,GC_STORE_OK);
    }
    for(unsigned pos=0;pos<9;pos++){
        store_fixture(&a,0x22,GC_OLD,old);a.ctx.satb.top=a.ctx.remembered.top=pos;
        atomic_store(&a.ctx.satb.published,pos);atomic_store(&a.ctx.satb.consumed,pos);
        atomic_store(&a.ctx.remembered.published,pos);atomic_store(&a.ctx.remembered.consumed,pos);
        store_compare(&a,step,GC_STORE_OK);
    }
    for(unsigned fault=0;fault<51;fault++){
        store_fixture(&a,0x22,GC_OLD,old);uint64_t result=GC_STORE_INVALID;
        switch(fault){case 0:a.q.scope=0;break;case 1:a.q.scope++;break;case 2:a.q.page=0;break;case 3:a.q.page++;break;
        case 4:a.q.registry=0;break;case 5:a.q.registry++;break;case 6:a.q.transferred=1;break;case 7:a.q.byte_offset=1;break;
        case 8:a.q.expected=1;break;case 9:a.q.desired=1;break;case 10:a.q.desired^=0x300;break;case 11:a.q.phase=8;break;
        case 12:a.q.phase=0xe000;break;case 13:a.scope.context=0;break;case 14:a.scope.context++;break;case 15:a.scope.held=0;break;
        case 16:a.scope.held++;break;case 17:a.scope.ticket=1;break;case 18:a.scope.incarnation=0;break;case 19:a.ctx.abi_version++;break;
        case 20:a.ctx.thread_id=0;break;case 21:atomic_store(&a.ctx.state,GC_BLOCKED_NATIVE);break;case 22:atomic_store(&a.ctx.access_epoch,0);break;
        case 23:atomic_store(&a.ctx.adopted_phase,0);break;case 24:a.cell.source_offset+=16;break;case 25:a.cell.incarnation++;break;
        case 26:atomic_store(&a.cell.access,GC_SOURCE_CLOSED);break;case 27:atomic_store(&a.cell.word,2);break;case 28:atomic_store(&a.cell.word,1);break;
        case 29:atomic_store(&a.page.state,GC_PAGE_READY);break;case 30:a.page.incarnation++;break;case 31:a.page.generation=2;break;
        case 32:a.page.start=0;break;case 33:a.page.limit=UINT64_MAX;break;case 34:a.page.heap_base++;break;case 35:a.page.heap_base=UINT64_MAX-15;break;
        case 36:atomic_store(&a.page.allocated_top,16);break;case 37:a.page.start=48;break;case 38:((ObjectHeader *)(a.heap+16))->payload_bytes=33;break;
        case 39:atomic_store(&((ObjectHeader *)(a.heap+16))->state,1);break;case 40:a.q.byte_offset=24;break;case 41:a.q.byte_offset=16;break;
        case 42:a.bitmap=0;break;case 43:atomic_store(&a.slots[1],0);break;case 44:a.descriptor.fixed_bytes=32;break;
        case 45:a.q.remembered.slot.metadata_lease=a.q.satb.slot.metadata_lease;break;
        case 46:atomic_store((_Atomic uint64_t *)(a.heap+48),a.q.desired);result=GC_STORE_RETRY;break;
        case 47:a.q.satb.previous.bits=0;break;case 48:a.q.remembered.previous.bits=0;break;
        case 49:a.q.phase|=0x80;break;case 50:((ObjectHeader *)(a.heap+16))->descriptor_id=2;break;
        }
        store_compare(&a,step,result);
    }
    for(unsigned buffer=0;buffer<2;buffer++)for(unsigned fault=0;fault<17;fault++){
        store_fixture(&a,0x22,GC_OLD,old);BufferCursor *b=buffer?&a.ctx.remembered:&a.ctx.satb;
        BarrierEntry *e=buffer?&a.q.remembered:&a.q.satb;uint64_t result=GC_STORE_INVALID;
        switch(fault){case 0:e->slot.owner.bits=0;break;case 1:e->slot.owner.bits=1;break;case 2:e->slot.owner.bits+=UINT64_C(16)<<16;break;
        case 3:e->slot.byte_offset=8;break;case 4:e->slot.page_incarnation++;break;case 5:e->slot.record_epoch++;break;case 6:e->slot.metadata_lease=0;break;
        case 7:b->entries=0;break;case 8:b->entries++;break;case 9:b->capacity=0;break;case 10:b->capacity=UINT64_MAX;break;
        case 11:b->entries=UINT64_MAX-7;break;case 12:atomic_store(&b->published,1);break;case 13:atomic_store(&b->consumed,1);break;
        case 14:b->top=4;break;case 15:b->top=3;atomic_store(&b->published,3);result=GC_STORE_FULL;break;
        case 16:b->top=UINT64_MAX;atomic_store(&b->published,UINT64_MAX);atomic_store(&b->consumed,UINT64_MAX);result=GC_STORE_EXHAUSTED;break;}
        store_compare(&a,step,result);
    }
    // Existing entrants may finish writes after a claimant closes admission.
    for(unsigned closed=0;closed<2;closed++){
        store_fixture(&a,0x22,GC_OLD,old);atomic_store(&a.cell.word,5);atomic_store(&a.cell.access,1|(closed?GC_SOURCE_CLOSED:0));store_compare(&a,step,GC_STORE_OK);
    }
    if(store_vectors){assert(!fclose(store_vectors));store_vectors=NULL;}
    printf("guarded store %u C/candidate pre-barrier, CAS and preservation cases PASS\n",store_cases);
}
#endif
