#ifndef KLASSIC_GC_HEAP_INCOMING_CASES_H
#define KLASSIC_GC_HEAP_INCOMING_CASES_H
#include "gc_heap_incoming.h"
#include "gc_barrier_cases.h"
static void gc_run_heap_incoming_cases(HeapIncomingScanner candidate){
    for(unsigned impl=0;impl<2;impl++){
    HeapIncomingScanner scan=impl?candidate:gc_heap_incoming;
    StoreFixture f,before;HeapIncoming q,saved;
    const uint64_t ref=(UINT64_C(128)<<16)|0x1510;
    assert(scan(NULL)==GC_HI_INVALID);
    assert(scan((void *)1)==GC_HI_INVALID);
    for(unsigned fault=0;fault<18;fault++){
        store_fixture(&f,0x22,GC_OLD,ref);
        q=(HeapIncoming){(uintptr_t)&f.scope,(uintptr_t)&f.page,(uintptr_t)&f.registry,0x22,128,160,0,0};
        switch(fault){
        case 0:q.registry=0;break;case 1:q.registry++;break;
        case 2:q.target_start=0;break;case 3:q.target_limit=128;break;
        case 4:q.target_limit=GC_MAX_OFFSET+1;break;case 5:q.target_start++;break;
        case 6:q.cursor=1;break;case 7:q.cursor=32;break;case 8:q.observed=ref;break;
        case 9:q.phase=0;break;case 10:atomic_store(&f.ctx.access_epoch,0);break;
        case 11:atomic_store(&f.cell.access,0);break;case 12:atomic_store(&f.cell.word,ref|2);break;
        case 13:atomic_store(&f.slots[1],0);break;case 14:f.descriptor.fixed_bytes=32;break;
        case 15:atomic_store((_Atomic uint64_t *)(f.heap+48),1);break;
        case 16:atomic_store(&((ObjectHeader *)(f.heap+16))->state,GC_OBJECT_INITIALIZING);break;
        case 17:atomic_store(&f.page.state,GC_PAGE_UNCOMMITTED);break;
        }
        memcpy(&before,&f,sizeof(f));saved=q;
        assert(scan(&q)==GC_HI_INVALID);
        assert(!memcmp(&q,&saved,sizeof(q))&&!memcmp(&f,&before,sizeof(f)));
    }
    /* Range boundaries, null, malformed scalar bits and pointer-shaped scalars.
     * Only field +8 is a reference. FOUND must not skip it on retries. */
    const uint64_t values[]={0,(UINT64_C(112)<<16)|0x1510,ref,
        (UINT64_C(144)<<16)|0x2620,(UINT64_C(160)<<16)|0x4930};
    for(unsigned i=0;i<5;i++){
        store_fixture(&f,0x22,GC_OLD,ref);f.bitmap=2;
        atomic_store((_Atomic uint64_t *)(f.heap+56),values[i]);
        atomic_store((_Atomic uint64_t *)(f.heap+64),1);
        q=(HeapIncoming){(uintptr_t)&f.scope,(uintptr_t)&f.page,(uintptr_t)&f.registry,0x22,128,160,0,0};
        memcpy(&before,&f,sizeof(f));
        assert(scan(&q)==GC_HI_MORE&&q.cursor==8&&!q.observed);
        if(i==2||i==3){
            assert(scan(&q)==GC_HI_FOUND&&q.cursor==8&&q.observed==values[i]);
            saved=q;assert(scan(&q)==GC_HI_INVALID&&!memcmp(&q,&saved,sizeof(q)));
            q.observed=0;assert(scan(&q)==GC_HI_FOUND&&q.cursor==8);
            assert(!memcmp(&f,&before,sizeof(f)));
            /* Simulate a concurrent logical clear; the audit never writes it. */
            atomic_store((_Atomic uint64_t *)(f.heap+56),0);q.observed=0;
            memcpy(&before,&f,sizeof(f));
        }
        assert(scan(&q)==GC_HI_MORE&&q.cursor==16&&!q.observed);
        assert(scan(&q)==GC_HI_DONE&&q.cursor==24);
        assert(scan(&q)==GC_HI_DONE&&q.cursor==24);
        assert(!memcmp(&f,&before,sizeof(f)));
    }
    /* A 600-slot array requires 600 slices, not one unbounded guard interval. */
    store_fixture(&f,0x22,GC_OLD,0);f.bitmap=1;
    f.descriptor.fixed_bytes=0;f.descriptor.fixed_bitmap=0;f.descriptor.fixed_bitmap_words=0;
    f.descriptor.element_bytes=8;f.descriptor.element_bitmap=(uintptr_t)&f.bitmap;f.descriptor.element_bitmap_words=1;
    ((ObjectHeader *)(f.heap+16))->payload_bytes=4800;atomic_store(&f.page.allocated_top,4848);
    q=(HeapIncoming){(uintptr_t)&f.scope,(uintptr_t)&f.page,(uintptr_t)&f.registry,0x22,128,160,0,0};
    for(unsigned i=0;i<600;i++){
        assert(scan(&q)==(i==599?GC_HI_DONE:GC_HI_MORE));
        assert(q.cursor==(i+1)*8&&!q.observed);
    }
    /* Scalar-only bytes are skipped without treating arbitrary bits as roots.
     * Zero-byte objects still own a committed aligned allocation extent. */
    for(unsigned empty=0;empty<2;empty++){
        store_fixture(&f,0x22,GC_OLD,1);
        f.descriptor.fixed_bytes=empty?0:8000;f.descriptor.fixed_bitmap=0;f.descriptor.fixed_bitmap_words=0;
        ((ObjectHeader *)(f.heap+16))->payload_bytes=f.descriptor.fixed_bytes;
        atomic_store(&f.page.allocated_top,empty?64:8048);
        q=(HeapIncoming){(uintptr_t)&f.scope,(uintptr_t)&f.page,(uintptr_t)&f.registry,0x22,128,160,0,0};
        memcpy(&before,&f,sizeof(f));assert(scan(&q)==GC_HI_DONE);
        assert(q.cursor==f.descriptor.fixed_bytes&&!q.observed&&!memcmp(&f,&before,sizeof(f)));
    }
    /* A selection may close admission while this existing guard remains live. */
    store_fixture(&f,0x22,GC_OLD,ref);
    atomic_store(&f.cell.word,5);atomic_store(&f.cell.access,GC_SOURCE_CLOSED|1);
    q=(HeapIncoming){(uintptr_t)&f.scope,(uintptr_t)&f.page,(uintptr_t)&f.registry,0x22,128,160,0,0};
    assert(scan(&q)==GC_HI_FOUND&&q.observed==ref&&q.cursor==0);
    }
    puts("heap incoming C/candidate: bounded descriptor slots, range boundaries, retained findings and unchanged rejection PASS");
}
#endif
