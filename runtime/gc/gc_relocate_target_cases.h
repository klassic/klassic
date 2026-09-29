#ifndef KLASSIC_GC_RELOCATE_TARGET_CASES_H
#define KLASSIC_GC_RELOCATE_TARGET_CASES_H
#include "gc_relocate_cases.h"
typedef uint64_t (*RelocateTarget)(AllocationPage *,RelocationCopy *,uint64_t);
typedef struct {
    AllocationPage p;RelocationCopy q;ForwardTable table;ForwardCell cells[4];
    _Atomic uint64_t starts[8];MarkRecord record;_Alignas(16) unsigned char heap[8192];
} TargetFixture;
static const uint64_t target_pointers[]={offsetof(TargetFixture,p.heap_base),offsetof(TargetFixture,q.heap_base),
    offsetof(TargetFixture,p.starts),offsetof(TargetFixture,p.forward_table),offsetof(TargetFixture,table.cells),
    offsetof(TargetFixture,heap)+32,offsetof(TargetFixture,heap)+96};
static FILE *target_vectors;static unsigned target_cases;
static void target_fixture(TargetFixture *f){
    memset(f,0,sizeof(*f));f->p=(AllocationPage){.heap_base=(uintptr_t)f->heap,.start=16,.limit=8192,.incarnation=27,
        .identity_base=1024,.state=GC_PAGE_READY,.starts=(uintptr_t)f->starts,.starts_words=8,.allocated_top=16,.forward_table=(uintptr_t)&f->table};
    f->q=(RelocationCopy){.heap_base=(uintptr_t)f->heap,.source_incarnation=17,.ticket=1,.source_limit=4096,.budget=256};
    f->table=(ForwardTable){(uintptr_t)f->cells,4,0};f->record=(MarkRecord){.object_id=42,.old_epoch=13};
}
static void target_complete(TargetFixture *f,uint64_t bytes){
    ObjectHeader *h=(ObjectHeader *)(f->heap+f->q.destination-32);
    *h=(ObjectHeader){bytes,17,(uintptr_t)&f->record,GC_OBJECT_PUBLISHED};f->q.stage=2;f->q.cursor=bytes;
}
static uint64_t target_normalize(TargetFixture *f,const TargetFixture *owner){
    uint64_t mask=0;
    for(unsigned i=0;i<sizeof(target_pointers)/sizeof(*target_pointers);i++){
        unsigned char *p=(unsigned char *)f+target_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)owner&&v-(uintptr_t)owner<sizeof(*owner)){v-=(uintptr_t)owner;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}
    }return mask;
}
static void target_clone(TargetFixture *dst,const TargetFixture *src){
    memcpy(dst,src,sizeof(*dst));uint64_t mask=target_normalize(dst,src);
    for(unsigned i=0;i<sizeof(target_pointers)/sizeof(*target_pointers);i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+target_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void target_check(const TargetFixture *before,const TargetFixture *after,uint64_t op,uint64_t status){
    TargetFixture e;memcpy(&e,before,sizeof(e));
    if(!status){
        if(!op){e.q.destination=atomic_load(&e.p.allocated_top)+32;e.q.destination_limit=e.p.limit;}
        else{
            uint64_t used=atomic_load(&e.table.used),granule=(e.q.destination-e.p.start)>>4;
            uint64_t rounded=(e.q.cursor+15)&~UINT64_C(15);if(!rounded)rounded=16;
            e.cells[used].source_offset=e.q.destination;e.cells[used].incarnation=e.p.incarnation;
            atomic_store(&e.table.used,used+1);atomic_fetch_or(&e.starts[granule>>6],UINT64_C(1)<<(granule&63));
            atomic_store(&e.p.allocated_top,e.q.destination+rounded);atomic_store(&e.p.state,GC_PAGE_CLOSED);
        }
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void target_compare(TargetFixture *a,RelocateTarget step,uint64_t op,uint64_t expected){
    TargetFixture c,ba,bc,x,y;target_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->p,&a->q,op),reference=gc_relocate_target_locked(&c.p,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"target case %u op %llu got %llu C %llu expected %llu\n",target_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    target_check(&ba,a,op,actual);target_check(&bc,&c,op,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(target_normalize(&x,a)==target_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(target_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,target_normalize(&x,&c),target_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,target_vectors)==1);assert(fwrite(&x,sizeof(x),1,target_vectors)==1);assert(fwrite(&y,sizeof(y),1,target_vectors)==1);}
    target_cases++;
}
static void gc_run_relocate_target_cases(RelocateTarget step){
    TargetFixture a;target_cases=0;const char *path=getenv("KLASSIC_GC_TARGET_VECTORS");
    if(path){target_vectors=fopen(path,"wb");assert(target_vectors);assert(fwrite("KLRTGT01",8,1,target_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(TargetFixture,p),offsetof(TargetFixture,q),sizeof(target_pointers)/sizeof(*target_pointers)};
        assert(fwrite(header,sizeof(header),1,target_vectors)==1);assert(fwrite(target_pointers,sizeof(target_pointers),1,target_vectors)==1);}
    const uint64_t lengths[]={0,7,16,17,255,256,257,4000};
    for(unsigned gen=0;gen<2;gen++)for(unsigned state=0;state<2;state++)for(unsigned size=0;size<8;size++){
        target_fixture(&a);a.p.generation=gen;atomic_store(&a.p.state,state?GC_PAGE_CLOSED:GC_PAGE_READY);
        target_compare(&a,step,0,GC_TARGET_OK);target_compare(&a,step,1,GC_TARGET_INVALID);
        target_complete(&a,lengths[size]);target_compare(&a,step,1,GC_TARGET_OK);target_compare(&a,step,1,GC_TARGET_INVALID);
    }
    for(unsigned op=0;op<2;op++)for(unsigned fault=0;fault<47;fault++){
        target_fixture(&a);if(op){assert(!gc_relocate_target_locked(&a.p,&a.q,0));target_complete(&a,17);}
        uint64_t expected=GC_TARGET_INVALID;
        switch(fault){
        case 0:atomic_store(&a.p.state,GC_PAGE_ALLOCATING);break;case 1:a.p.owner=1;break;
        case 2:a.p.incarnation=0;break;case 3:a.p.identity_base=0;break;case 4:a.p.generation=2;break;
        case 5:a.p.start=0;break;case 6:a.p.start=a.p.limit;break;case 7:a.p.limit=UINT64_MAX;break;
        case 8:a.p.heap_base++;break;case 9:a.p.start++;break;case 10:a.p.limit++;break;
        case 11:atomic_fetch_add(&a.p.allocated_top,1);break;case 12:a.p.heap_base=UINT64_MAX-15;break;
        case 13:a.q.heap_base++;break;case 14:atomic_store(&a.p.allocated_top,0);break;
        case 15:atomic_store(&a.p.allocated_top,a.p.limit+16);break;case 16:atomic_store(&a.p.allocated_top,32);break;
        case 17:a.p.starts=0;break;case 18:a.p.starts++;break;case 19:a.p.starts_words=7;break;
        case 20:a.p.starts_words=UINT64_MAX;break;case 21:a.p.starts=UINT64_MAX-7;break;
        case 22:a.q.stage=3;break;case 23:if(op)a.q.destination++;else a.q.cursor=1;break;
        case 24:a.q.destination_limit++;break;case 25:if(op)a.q.stage=1;else a.q.destination=48;break;
        case 26:atomic_store(&a.p.state,GC_PAGE_CLOSED);atomic_store(&a.p.allocated_top,8192);
            if(op)a.q.destination=8224;expected=GC_TARGET_REFILL;break;
        case 27:atomic_store(&a.starts[0],4);break;case 28:a.p.forward_table=0;expected=GC_TARGET_METADATA_REFILL;break;
        case 29:a.p.forward_table++;break;case 30:a.table.cells=0;break;case 31:a.table.cells++;break;
        case 32:a.table.capacity=0;break;case 33:a.table.capacity=UINT64_MAX;break;
        case 34:a.table.cells=UINT64_MAX-7;break;case 35:atomic_store(&a.table.used,5);break;
        case 36:atomic_store(&a.table.used,4);expected=GC_TARGET_METADATA_REFILL;break;
        case 37:atomic_store(&a.table.used,1);a.cells[0]=(ForwardCell){48,27,0,0};break;
        case 38:atomic_store(&a.table.used,1);a.cells[0]=(ForwardCell){16,28,0,0};break;
        case 39:a.cells[0].source_offset=1;break;case 40:a.cells[0].incarnation=1;break;
        case 41:atomic_store(&a.cells[0].word,1);break;case 42:atomic_store(&a.cells[0].access,1);break;
        case 43:a.q.destination_limit=1;break;case 44:a.q.stage=4;break;
        case 45:a.p.limit=32;if(op)a.q.destination_limit=32;expected=GC_TARGET_REFILL;break;
        case 46:atomic_store(&a.p.state,GC_PAGE_RUN_TAIL);break;
        }
        target_compare(&a,step,op,expected);
    }
    for(unsigned fault=0;fault<9;fault++){
        target_fixture(&a);assert(!gc_relocate_target_locked(&a.p,&a.q,0));target_complete(&a,17);
        ObjectHeader *h=(ObjectHeader *)(a.heap+16);
        switch(fault){case 0:atomic_store(&h->state,1);break;case 1:h->payload_bytes++;break;
        case 2:h->payload_bytes=a.q.cursor=UINT64_MAX;break;case 3:h->descriptor_id=0;break;
        case 4:h->descriptor_id=UINT64_C(1)<<32;break;case 5:h->mark_record=0;break;case 6:h->mark_record++;break;
        case 7:a.q.cursor=h->payload_bytes=17;a.p.limit=a.q.destination_limit=64;break;
        case 8:a.q.cursor=0;break;}
        target_compare(&a,step,1,GC_TARGET_INVALID);
    }
    target_fixture(&a);target_compare(&a,step,2,GC_TARGET_INVALID);
    for(unsigned gen=0;gen<2;gen++){
        target_fixture(&a);a.p.generation=gen;
        for(unsigned object=0;object<2;object++){
            a.q.destination=a.q.destination_limit=a.q.stage=a.q.cursor=0;
            target_compare(&a,step,0,GC_TARGET_OK);target_complete(&a,17);target_compare(&a,step,1,GC_TARGET_OK);
        }
        assert(atomic_load(&a.table.used)==2&&atomic_load(&a.p.allocated_top)==144);
    }
    if(target_vectors){assert(!fclose(target_vectors));target_vectors=NULL;}
    printf("relocation target %u C/candidate preparation and metadata installation cases PASS\n",target_cases);
}
#endif
