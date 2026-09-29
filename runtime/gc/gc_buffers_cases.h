#ifndef KLASSIC_GC_BUFFERS_CASES_H
#define KLASSIC_GC_BUFFERS_CASES_H
#include "gc_buffers.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*BarrierBufferStep)(MutatorContext *,const BarrierEntry *,uint64_t);
typedef uint64_t (*BarrierBufferDrain)(BarrierDrain *);
typedef struct {
    MutatorContext ctx; BarrierEntry input; BarrierDrain q;
    BarrierEntry output[257],entries[300];
} BufferFixture;
_Static_assert(sizeof(BufferFixture)<0x8000,"buffer ARM replay arena");
static const uint64_t buffer_pointers[]={offsetof(BufferFixture,ctx.satb.entries),offsetof(BufferFixture,ctx.remembered.entries),
    offsetof(BufferFixture,q.buffer),offsetof(BufferFixture,q.output)};
static FILE *buffer_vectors[2];static unsigned buffer_cases[2];
static BarrierEntry buffer_entry(uint64_t index) {
    return (BarrierEntry){.previous={index%4?((index+1)<<20)|0x1510:0},
        .slot={.owner={((index+100)<<20)|0x2510},.byte_offset=(index%13)*8,
            .page_incarnation=index+41,.record_epoch=index%2,.metadata_lease=index+791}};
}
static BufferCursor *buffer_selected(BufferFixture *f,unsigned selector){return selector?&f->ctx.remembered:&f->ctx.satb;}
static void buffer_fixture(BufferFixture *f,unsigned selector,uint64_t capacity,uint64_t start) {
    memset(f,0,sizeof(*f));f->ctx.abi_version=GC_ABI_VERSION;f->ctx.thread_id=17;
    BufferCursor *b=buffer_selected(f,selector);*b=(BufferCursor){.entries=(uintptr_t)f->entries,.capacity=capacity,.top=start};
    atomic_store(&b->published,start);atomic_store(&b->consumed,start);
    f->input=buffer_entry(0);f->q=(BarrierDrain){(uintptr_t)b,(uintptr_t)f->output,256,999};
    memset(f->output,0xab,sizeof(f->output));
}
static uint64_t buffer_normalize(BufferFixture *dst,const BufferFixture *src) {
    uint64_t mask=0;for(unsigned i=0;i<sizeof(buffer_pointers)/8;i++){
        unsigned char *p=(unsigned char *)dst+buffer_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void buffer_copy(BufferFixture *dst,const BufferFixture *src) {
    memcpy(dst,src,sizeof(*dst));uint64_t mask=buffer_normalize(dst,src);
    for(unsigned i=0;i<sizeof(buffer_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+buffer_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void buffer_compare(BufferFixture *a,BufferFixture *c,unsigned kind,unsigned op,BarrierBufferStep step,BarrierBufferDrain drain,uint64_t expected) {
    BufferFixture before,x,y;memcpy(&before,c,sizeof(before));
    uint64_t actual=kind?drain(&a->q):step(&a->ctx,&a->input,op);
    uint64_t reference=kind?gc_barrier_buffer_drain(&c->q):gc_barrier_buffer_step(&c->ctx,&c->input,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"buffer kind %u case %u op %u got %llu C %llu expected %llu\n",kind,buffer_cases[kind],op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if((!kind&&expected)||expected==GC_BB_INVALID)assert(!memcmp(c,&before,sizeof(before)));
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(buffer_normalize(&x,a)==buffer_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(buffer_vectors[kind]){memcpy(&x,&before,sizeof(x));memcpy(&y,c,sizeof(y));
        uint64_t record[]={op,reference,buffer_normalize(&x,c),buffer_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,buffer_vectors[kind])==1);assert(fwrite(&x,sizeof(x),1,buffer_vectors[kind])==1);assert(fwrite(&y,sizeof(y),1,buffer_vectors[kind])==1);}
    buffer_cases[kind]++;
}
static void gc_run_buffer_cases(BarrierBufferStep step,BarrierBufferDrain drain) {
    BufferFixture *a=malloc(sizeof(*a)),*c=malloc(sizeof(*c));assert(a&&c);
    const char *names[]={"KLASSIC_GC_BUFFER_STEP_VECTORS","KLASSIC_GC_BUFFER_DRAIN_VECTORS"};
    const char *magic[]={"KLBBST01","KLBBDR01"};
    for(unsigned k=0;k<2;k++){buffer_cases[k]=0;const char *path=getenv(names[k]);if(path){buffer_vectors[k]=fopen(path,"wb");assert(buffer_vectors[k]);
        assert(fwrite(magic[k],8,1,buffer_vectors[k])==1);uint64_t h[]={sizeof(*a),k?offsetof(BufferFixture,q):offsetof(BufferFixture,ctx),offsetof(BufferFixture,input),sizeof(buffer_pointers)/8};
        assert(fwrite(h,sizeof(h),1,buffer_vectors[k])==1);assert(fwrite(buffer_pointers,sizeof(buffer_pointers),1,buffer_vectors[k])==1);}}
#define BUFFER(k,op,result) buffer_compare(a,c,k,op,step,drain,result)
    const unsigned capacities[]={1,3,256,300},budgets[]={1,17,256};
    for(unsigned selector=0;selector<2;selector++)for(unsigned size=0;size<4;size++) {
        unsigned capacity=capacities[size];buffer_fixture(a,selector,capacity,capacity-1);buffer_copy(c,a);
        for(unsigned round=0;round<2;round++) {
            for(unsigned i=0;i<capacity;i++){a->input=c->input=buffer_entry(round*capacity+i);BUFFER(0,selector,0);}
            BUFFER(0,selector,GC_BB_FULL);BUFFER(1,0,0);assert(a->q.written==0); // private data stays invisible
            BUFFER(0,selector+2,0);
            a->q.capacity=c->q.capacity=budgets[size%3];
            unsigned read=0;do {
                unsigned n=capacity-read;if(n>a->q.capacity)n=(unsigned)a->q.capacity;
                BUFFER(1,0,read+n==capacity?0:2);assert(a->q.written==n);
                for(unsigned j=0;j<n;j++){BarrierEntry expected=buffer_entry(round*capacity+read+j);assert(!memcmp(&expected,&a->output[j],sizeof(expected)));}
                read+=n;
            }while(read<capacity);
            assert(atomic_load(&buffer_selected(a,selector)->consumed)==buffer_selected(a,selector)->top);
        }
    }
    for(unsigned op=0;op<4;op++)for(unsigned state=0;state<6;state++)for(unsigned guard=0;guard<2;guard++) {
        buffer_fixture(a,op&1,3,0);atomic_store(&a->ctx.state,state);atomic_store(&a->ctx.access_epoch,guard);buffer_copy(c,a);
        bool valid=op<2?state==0:(state==0||state==1||state==3);BUFFER(0,op,valid?0:GC_BB_INVALID);
    }
    for(unsigned selector=0;selector<2;selector++) {
        buffer_fixture(a,selector,3,UINT64_MAX-1);buffer_copy(c,a);BUFFER(0,selector,0);BUFFER(0,selector,GC_BB_EXHAUSTED);
        BUFFER(0,selector+2,0);BUFFER(1,0,0);assert(a->q.written==1);assert(atomic_load(&buffer_selected(a,selector)->consumed)==UINT64_MAX);
        BUFFER(0,selector,GC_BB_EXHAUSTED);
    }
    for(unsigned fault=0;fault<12;fault++) {
        buffer_fixture(a,0,3,0);BufferCursor *b=&a->ctx.satb;unsigned op=0;
        switch(fault){case 0:a->ctx.abi_version++;break;case 1:a->ctx.thread_id=0;break;
        case 2:b->entries=0;break;case 3:b->entries++;break;case 4:b->capacity=0;break;
        case 5:b->capacity=UINT64_MAX;break;case 6:b->entries=UINT64_MAX-7;break;
        case 7:atomic_store(&b->published,1);break;case 8:atomic_store(&b->consumed,1);break;
        case 9:b->top=4;break;case 10:op=4;break;case 11:atomic_store(&b->published,4);b->top=4;break;}
        buffer_copy(c,a);BUFFER(0,op,GC_BB_INVALID);
    }
    for(unsigned fault=0;fault<13;fault++) {
        buffer_fixture(a,0,3,0);BufferCursor *b=&a->ctx.satb;
        switch(fault){case 0:a->q.buffer=0;break;case 1:a->q.buffer++;break;case 2:a->q.output=0;break;case 3:a->q.output++;break;
        case 4:a->q.capacity=0;break;case 5:a->q.capacity=257;break;case 6:a->q.output=UINT64_MAX-7;break;
        case 7:b->entries=0;break;case 8:b->entries++;break;case 9:b->capacity=0;break;case 10:b->entries=UINT64_MAX-7;break;
        case 11:atomic_store(&b->consumed,1);break;case 12:atomic_store(&b->published,4);break;}
        buffer_copy(c,a);BUFFER(1,0,GC_BB_INVALID);
    }
    // Sole consumer must ignore the producer-private cursor, even if poisoned.
    buffer_fixture(a,0,3,0);a->entries[0]=buffer_entry(7);atomic_store(&a->ctx.satb.published,1);a->ctx.satb.top=UINT64_MAX;
    buffer_copy(c,a);BUFFER(1,0,0);assert(!memcmp(&a->output[0],&a->entries[0],sizeof(BarrierEntry)));
    for(unsigned k=0;k<2;k++)if(buffer_vectors[k]){assert(fclose(buffer_vectors[k])==0);buffer_vectors[k]=NULL;}
    printf("barrier buffers %u producer / %u drain cases PASS\n",buffer_cases[0],buffer_cases[1]);free(a);free(c);
#undef BUFFER
}
#endif
