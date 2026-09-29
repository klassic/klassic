#ifndef KLASSIC_GC_BUFFERS_CONCURRENCY_H
#define KLASSIC_GC_BUFFERS_CONCURRENCY_H
#include "gc_buffers_cases.h"
#include "gc_roots_concurrency.h"
typedef struct {
    MutatorContext ctx;BarrierEntry entries[3];
    BarrierBufferStep step;BarrierBufferDrain drain;
    unsigned selector;uint64_t base;
    _Atomic uint64_t ready,resume;
} BufferRace;
#ifdef _WIN32
static DWORD WINAPI buffer_reader(void *arg)
#else
static void *buffer_reader(void *arg)
#endif
{
    BufferRace *r=arg;BufferCursor *b=r->selector?&r->ctx.remembered:&r->ctx.satb;
    BarrierEntry out[3];memset(out,0xab,sizeof(out));BarrierDrain q={(uintptr_t)b,(uintptr_t)out,2,0};
    atomic_store_explicit(&r->ready,1,memory_order_release);
    while(atomic_load_explicit(&r->resume,memory_order_acquire)<1)root_yield();
    assert(r->drain(&q)==0&&q.written==0); // one complete but still private entry
    atomic_store_explicit(&r->ready,2,memory_order_release);
    while(atomic_load_explicit(&r->resume,memory_order_acquire)<2)root_yield();
    uint64_t read=0;
    while(read<512) {
        uint64_t result=r->drain(&q);assert(result==0||result==2);assert(q.written<=2&&read+q.written<=512);
        for(uint64_t i=0;i<q.written;i++){BarrierEntry expected=buffer_entry(r->base+read+i);assert(!memcmp(&expected,&out[i],sizeof(expected)));}
        for(unsigned i=0;i<sizeof(out[2]);i++)assert(((unsigned char *)&out[2])[i]==0xab);
        read+=q.written;if(!q.written)root_yield();
    }
    return 0;
}
static void gc_run_buffer_races(BarrierBufferStep step,BarrierBufferDrain drain) {
    BufferRace r={.step=step,.drain=drain};
    for(unsigned round=0;round<64;round++) {
        r.ctx=(MutatorContext){.abi_version=GC_ABI_VERSION,.thread_id=17};r.selector=round%2;r.base=round*512;
        BufferCursor *b=r.selector?&r.ctx.remembered:&r.ctx.satb;
        *b=(BufferCursor){.entries=(uintptr_t)r.entries,.capacity=round%3?3:1};
        atomic_store(&r.ready,0);atomic_store(&r.resume,0);
        RootThread reader;
#ifdef _WIN32
        reader=CreateThread(NULL,0,buffer_reader,&r,0,NULL);assert(reader);
#else
        assert(pthread_create(&reader,NULL,buffer_reader,&r)==0);
#endif
        while(atomic_load_explicit(&r.ready,memory_order_acquire)<1)root_yield();
        BarrierEntry entry=buffer_entry(r.base);assert(step(&r.ctx,&entry,r.selector)==0);
        atomic_store_explicit(&r.resume,1,memory_order_release);
        while(atomic_load_explicit(&r.ready,memory_order_acquire)<2)root_yield();
        assert(step(&r.ctx,NULL,r.selector+2)==0);atomic_store_explicit(&r.resume,2,memory_order_release);
        for(unsigned i=1;i<512;i++) {
            entry=buffer_entry(r.base+i);uint64_t result;
            do {result=step(&r.ctx,&entry,r.selector);assert(result==0||result==GC_BB_FULL);
                if(result){assert(step(&r.ctx,NULL,r.selector+2)==0);root_yield();}
            }while(result);
            if(i%3==0||i==511)assert(step(&r.ctx,NULL,r.selector+2)==0);
        }
#ifdef _WIN32
        assert(WaitForSingleObject(reader,30000)==WAIT_OBJECT_0);assert(CloseHandle(reader));
#else
        assert(pthread_join(reader,NULL)==0);
#endif
        assert(b->top==512&&atomic_load(&b->published)==512&&atomic_load(&b->consumed)==512);
    }
    puts("barrier buffers: 64 private-prefix, concurrent ring-wrap and complete-entry rounds PASS");
}
#endif
