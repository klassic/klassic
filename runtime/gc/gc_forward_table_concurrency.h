/* The fixture owns metadata/strong storage until join. Headers may be read
 * while INITIALIZING; no unpublished payload or production registry is used. */
#ifndef KLASSIC_GC_FORWARD_TABLE_CONCURRENCY_H
#define KLASSIC_GC_FORWARD_TABLE_CONCURRENCY_H
#include "gc_metadata_concurrency.h"
#include "gc_heap_cases.h"
#include "gc_forward_table_cases.h"
#include "gc_source_access_cases.h"
typedef struct {
    HeapFixture heap;
    _Atomic uint64_t finished;
    ForwardFinder lookup;
    SourceAccess access;
} TableRace;
#ifdef _WIN32
static DWORD WINAPI scan_allocated_headers(void *argument)
#else
static void *scan_allocated_headers(void *argument)
#endif
{
    TableRace *race=argument;HeapFixture *shared=&race->heap;
    uint64_t cursor = 16, scanned = 0;
    MutatorContext ctx={.abi_version=GC_ABI_VERSION,.thread_id=99};
    for (;;) {
        uint64_t top = atomic_load_explicit(&shared->page.allocated_top, memory_order_acquire);
        while (cursor < top) {
            ForwardLookup lookup={cursor+sizeof(ObjectHeader),shared->page.incarnation,0};
            assert(race->lookup(&shared->forwards,&lookup)==GC_FL_OK);
            SourceAccessRequest guard={(uintptr_t)&ctx,lookup.source_offset,lookup.incarnation,0,0};
            assert(race->access((ForwardCell *)(uintptr_t)lookup.cell,&guard,GC_SA_ENTER)==0);
            const ObjectHeader *h = (const ObjectHeader *)(shared->heap + cursor);
            assert(atomic_load_explicit(&h->state, memory_order_acquire) == GC_OBJECT_INITIALIZING);
            assert(h->payload_bytes == 8 && h->descriptor_id == 23);
            uint64_t granule = (cursor + sizeof(ObjectHeader) - 16) / 16;
            const MarkRecord *record=(const MarkRecord *)(uintptr_t)h->mark_record;
            assert(record->object_id == 1024 + granule);
            assert(atomic_load_explicit(&shared->starts[granule / 64], memory_order_acquire) &
                   (UINT64_C(1) << (granule & 63)));
            cursor += sizeof(ObjectHeader) + 16;
            assert(race->access((ForwardCell *)(uintptr_t)lookup.cell,&guard,GC_SA_EXIT)==0);
            scanned++;
        }
        assert(cursor == top);
        if (atomic_load_explicit(&race->finished, memory_order_acquire)) {
            if (cursor == atomic_load_explicit(&shared->page.allocated_top, memory_order_acquire)) break;
        }
        metadata_yield();
    }
    assert(scanned == (sizeof(shared->heap) - 16) / 48);
    return 0;
}
static void gc_run_forward_table_races(TlabAllocator allocate,ForwardFinder lookup,SourceAccess access) {
    TableRace *race=calloc(1,sizeof(*race));assert(race);
    race->lookup=lookup;race->access=access;HeapFixture *shared=&race->heap;
    for (unsigned round = 0; round < 32; round++) {
        heap_fixture_init(shared);
        shared->descriptor.fixed_bytes = 8;
        atomic_store(&race->finished, 0);
#ifdef _WIN32
        HANDLE reader=CreateThread(NULL,0,scan_allocated_headers,race,0,NULL);assert(reader);
#else
        pthread_t reader;assert(pthread_create(&reader,NULL,scan_allocated_headers,race)==0);
#endif
        for (unsigned i = 0; i < (sizeof(shared->heap) - 16) / 48; i++) {
            assert(allocate(&shared->ctx, &shared->request) == GC_ALLOC_OK);
            metadata_yield();
        }
        assert(allocate(&shared->ctx, &shared->request) == GC_ALLOC_REFILL);
        atomic_store_explicit(&race->finished, 1, memory_order_release);
#ifdef _WIN32
        assert(WaitForSingleObject(reader,INFINITE)==WAIT_OBJECT_0);assert(CloseHandle(reader));
#else
        assert(pthread_join(reader,NULL)==0);
#endif
    }
    free(race);
    puts("forward table: 32 concurrent append, lookup and guarded header rounds PASS");
    puts("TLAB header/bitmap concurrent publication: 32 rounds PASS");
}
#endif
