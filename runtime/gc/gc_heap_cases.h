/* Shared C/emitted conformance cases. Test code, never a runtime include. */
#ifndef KLASSIC_GC_HEAP_CASES_H
#define KLASSIC_GC_HEAP_CASES_H
#include "gc_heap.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*TlabAllocator)(MutatorContext *, const AllocationRequest *);
typedef struct {
    MutatorContext ctx;
    AllocationPage page;
    TypeDescriptor descriptor;
    AllocationRequest request;
    AllocationResult output;
    _Atomic uint64_t starts[8];
    MarkArena marks;
    MarkRecord records[171];
    ForwardTable forwards;
    ForwardCell cells[171];
    _Alignas(16) unsigned char heap[8192];
} HeapFixture;

static FILE *heap_vectors;
static const uint64_t heap_pointer_fields[] = {
    offsetof(HeapFixture, ctx.tlab_start), offsetof(HeapFixture, ctx.tlab_top),
    offsetof(HeapFixture, ctx.tlab_end), offsetof(HeapFixture, page.heap_base),
    offsetof(HeapFixture, page.starts), offsetof(HeapFixture, descriptor.fixed_bitmap),
    offsetof(HeapFixture, descriptor.element_bitmap), offsetof(HeapFixture, request.page),
    offsetof(HeapFixture, request.descriptor), offsetof(HeapFixture, request.output),
    offsetof(HeapFixture, request.marks), offsetof(HeapFixture, marks.records),
    offsetof(HeapFixture, page.forward_table), offsetof(HeapFixture, forwards.cells)
};
static uint64_t heap_vector_normalize(HeapFixture *copy, const HeapFixture *address_owner) {
    uint64_t mask = 0, base = (uintptr_t)address_owner;
    for (unsigned i = 0; i < sizeof(heap_pointer_fields)/sizeof(*heap_pointer_fields); i++) {
        unsigned char *field = (unsigned char *)copy + heap_pointer_fields[i];
        uint64_t pointer;
        memcpy(&pointer, field, 8);
        if (pointer >= base && pointer <= base + sizeof(*copy)) {
            pointer -= base;
            mask |= UINT64_C(1) << i;
            memcpy(field, &pointer, 8);
        }
    }
    // Object-start bits identify header pointers separately from the finite
    // fixture-pointer mask. Payload bytes are never interpreted as pointers.
    for (unsigned granule=0;granule<512;granule++) {
        if (!(atomic_load(&copy->starts[granule/64]) & (UINT64_C(1)<<(granule%64)))) continue;
        uint64_t user=16+granule*16;
        if(user<sizeof(ObjectHeader)||user>=sizeof(copy->heap))continue;
        unsigned char *field=copy->heap+user-sizeof(ObjectHeader)+offsetof(ObjectHeader,mark_record);
        uint64_t value;memcpy(&value,field,8);
        if(value>=(uintptr_t)address_owner->records&&value-(uintptr_t)address_owner->records<sizeof(copy->records)) {
            value-=base;memcpy(field,&value,8);
        }
    }
    return mask;
}

static void heap_fixture_init(HeapFixture *f) {
    memset(f, 0, sizeof(*f));
    f->ctx.abi_version = GC_ABI_VERSION;
    f->ctx.thread_id = 7;
    f->ctx.tlab_start = f->ctx.tlab_top = (uintptr_t)f->heap + 16;
    f->ctx.tlab_end = (uintptr_t)f->heap + sizeof(f->heap);
    f->page.heap_base = (uintptr_t)f->heap;
    f->page.start = 16;
    f->page.limit = sizeof(f->heap);
    f->page.incarnation = 17;
    f->page.identity_base = 1024;
    f->page.owner = 7;
    atomic_init(&f->page.state, GC_PAGE_ALLOCATING);
    f->page.starts = (uintptr_t)f->starts;
    f->page.starts_words = 8;
    atomic_init(&f->page.allocated_top, 16);
    f->descriptor.id = 23;
    f->descriptor.alignment = 16;
    f->request = (AllocationRequest){(uintptr_t)&f->page, (uintptr_t)&f->descriptor, 0, (uintptr_t)&f->output, (uintptr_t)&f->marks};
    f->marks=(MarkArena){(uintptr_t)f->records,171,0};
    f->page.forward_table=(uintptr_t)&f->forwards;
    f->forwards=(ForwardTable){(uintptr_t)f->cells,171,0};
    f->output = (AllocationResult){UINT64_MAX, UINT64_MAX};
    memset(f->heap, 0xa5, 16); // guard granule must never be allocated/written
}

static uint64_t heap_check_one(HeapFixture *f, TlabAllocator allocate, uint64_t expected) {
    HeapFixture before;
    memcpy(&before, f, sizeof(before));
    uint64_t status = allocate(&f->ctx, &f->request);
    assert(status == expected);
    if (status != GC_ALLOC_OK) {
        assert(memcmp(f, &before, sizeof(*f)) == 0);
        return status;
    }
    uint64_t payload = before.descriptor.fixed_bytes + before.request.elements * before.descriptor.element_bytes;
    uint64_t rounded = (payload + 15) & ~UINT64_C(15);
    if (!rounded) rounded = 16;
    uint64_t offset = before.ctx.tlab_top - (uintptr_t)f->heap;
    uint64_t user = offset + sizeof(ObjectHeader);
    uint64_t granule = (user - before.page.start) >> 4;
    assert(user < sizeof(f->heap));
    assert(f->output.reference == (user << 16 | gc_abi_color((PhaseSnapshot){atomic_load(&before.ctx.adopted_phase)})));
    assert(f->output.object_id == before.page.identity_base + granule);
    const ObjectHeader *header = (const ObjectHeader *)(f->heap + offset);
    assert(header->payload_bytes == payload && header->descriptor_id == before.descriptor.id);
    assert(header->mark_record == (uintptr_t)&f->records[before.marks.used] && atomic_load(&header->state) == GC_OBJECT_INITIALIZING);
    assert(f->marks.used==before.marks.used+1);
    uint64_t used=atomic_load(&before.forwards.used);
    assert(atomic_load(&f->forwards.used)==used+1);
    before.cells[used].source_offset=user;before.cells[used].incarnation=f->page.incarnation;
    assert(!memcmp(before.cells,f->cells,sizeof(f->cells)));
    assert(f->records[before.marks.used].object_id==f->output.object_id);
    before.records[before.marks.used].object_id=f->output.object_id;
    assert(!memcmp(before.records,f->records,sizeof(f->records)));
    assert(f->ctx.tlab_top == before.ctx.tlab_top + sizeof(ObjectHeader) + rounded);
    assert(atomic_load(&f->page.allocated_top) == f->ctx.tlab_top - (uintptr_t)f->heap);
    assert(memcmp(f->heap, before.heap, (size_t)offset) == 0);
    assert(memcmp(f->heap + user, before.heap + user, sizeof(f->heap) - (size_t)user) == 0);
    for (unsigned i = 0; i < 8; i++) {
        uint64_t bits = atomic_load(&before.starts[i]);
        if (i == granule / 64) bits |= UINT64_C(1) << (granule & 63);
        assert(atomic_load(&f->starts[i]) == bits);
    }
    uint64_t count=atomic_load(&before.ctx.allocated_bytes),bytes=sizeof(ObjectHeader)+rounded;
    uint64_t expected_count=count>UINT64_MAX-bytes?UINT64_MAX:count+bytes;
    assert(atomic_load(&f->ctx.allocated_bytes)==expected_count);
    atomic_store(&before.ctx.allocated_bytes,expected_count);
    before.ctx.tlab_top = f->ctx.tlab_top;
    atomic_store(&before.page.allocated_top, atomic_load(&f->page.allocated_top));
    assert(memcmp(&before.ctx, &f->ctx, sizeof(f->ctx)) == 0);
    assert(memcmp(&before.page, &f->page, sizeof(f->page)) == 0);
    assert(memcmp(&before.descriptor, &f->descriptor, sizeof(f->descriptor)) == 0);
    assert(memcmp(&before.request, &f->request, sizeof(f->request)) == 0);
    return status;
}

static unsigned heap_case_count;
static void heap_compare(HeapFixture *a, HeapFixture *c, TlabAllocator candidate, uint64_t expected) {
    HeapFixture before, after;
    if (heap_vectors) memcpy(&before, c, sizeof(before));
    assert(heap_check_one(c, gc_try_tlab_allocate, expected) == heap_check_one(a, candidate, expected));
    HeapFixture x,y;memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));
    heap_vector_normalize(&x,a);heap_vector_normalize(&y,c);
    assert(memcmp(x.heap, y.heap, sizeof(a->heap)) == 0);
    assert(memcmp(a->records,c->records,sizeof(a->records))==0&&a->marks.used==c->marks.used);
    assert(memcmp(a->cells,c->cells,sizeof(a->cells))==0&&atomic_load(&a->forwards.used)==atomic_load(&c->forwards.used));
    assert(memcmp(a->starts, c->starts, sizeof(a->starts)) == 0);
    assert(memcmp(&a->output, &c->output, sizeof(a->output)) == 0);
    if (heap_vectors) {
        memcpy(&after, c, sizeof(after));
        uint64_t record[] = {expected, heap_vector_normalize(&before, c), heap_vector_normalize(&after, c)};
        assert(fwrite(record, sizeof(record), 1, heap_vectors) == 1);
        assert(fwrite(&before, sizeof(before), 1, heap_vectors) == 1);
        assert(fwrite(&after, sizeof(after), 1, heap_vectors) == 1);
    }
    heap_case_count++;
}

static void heap_fault(HeapFixture *f, unsigned fault) {
    switch (fault) {
    case 0: f->ctx.abi_version = 0; break;
    case 1: f->ctx.thread_id = 0; break;
    case 2: atomic_store(&f->ctx.state, GC_BLOCKED_NATIVE); break;
    case 3: atomic_store(&f->ctx.access_epoch, 1); break;
    case 4: f->request.descriptor = 0; break;
    case 5: f->request.output = 0; break;
    case 6: f->descriptor.id = 0; break;
    case 7: f->descriptor.id = UINT64_C(1) << 32; break;
    case 8: f->request.elements = 1; break;
    case 9: atomic_store(&f->ctx.adopted_phase, 8); break;
    case 10: atomic_store(&f->ctx.adopted_phase, 0x80); break;
    case 11: atomic_store(&f->ctx.adopted_phase, 0x2000); break;
    case 12: atomic_store(&f->page.state, 0); break;
    case 13: f->page.generation = GC_OLD; break;
    case 14: f->page.owner = 8; break;
    case 15: f->page.incarnation = 0; break;
    case 16: f->page.identity_base = 0; break;
    case 17: f->page.starts = 0; break;
    case 18: f->page.start = 0; break;
    case 19: f->page.start = f->page.limit; break;
    case 20: f->page.limit = UINT64_C(1) << 48; break;
    case 21: f->page.heap_base++; break;
    case 22: f->page.start++; break;
    case 23: f->page.limit++; break;
    case 24: f->page.heap_base = UINT64_MAX - 15; break;
    case 25: f->ctx.tlab_start += 16; break;
    case 26: f->ctx.tlab_end -= 16; break;
    case 27: f->ctx.tlab_top = f->ctx.tlab_start - 16; break;
    case 28: f->ctx.tlab_top = f->ctx.tlab_end + 16; break;
    case 29: f->ctx.tlab_top++; break;
    case 30: atomic_fetch_add(&f->page.allocated_top, 16); break;
    case 31: f->page.starts_words = 0; break;
    case 32: atomic_store(&f->starts[0], 4); break; // payload granule 2 already allocated
    case 33: f->descriptor.alignment = 0; break;
    case 34: f->descriptor.alignment = 3; break;
    case 35: f->descriptor.alignment = 32; break;
    default: assert(0);
    }
}

static void gc_run_tlab_cases(TlabAllocator candidate) {
    // malloc storage supplies the effective types established by header writes;
    // it also lets ASan guard the complete fixture allocation.
    HeapFixture *a = malloc(sizeof(*a)), *c = malloc(sizeof(*c));
    assert(a && c);
    heap_case_count = 0;
    const char *vector_path = getenv("KLASSIC_GC_TLAB_VECTORS");
    if (vector_path) {
        heap_vectors = fopen(vector_path, "wb");
        assert(heap_vectors && fwrite("KLTLAB02", 8, 1, heap_vectors) == 1);
        uint64_t header[] = {sizeof(HeapFixture), offsetof(HeapFixture, ctx),
            offsetof(HeapFixture, request), sizeof(heap_pointer_fields)/sizeof(*heap_pointer_fields)};
        assert(fwrite(header, sizeof(header), 1, heap_vectors) == 1);
        assert(fwrite(heap_pointer_fields, sizeof(heap_pointer_fields), 1, heap_vectors) == 1);
        uint64_t geometry[]={offsetof(HeapFixture,heap),offsetof(HeapFixture,starts),16,offsetof(HeapFixture,records),sizeof(a->records),sizeof(a->heap)};
        assert(fwrite(geometry,sizeof(geometry),1,heap_vectors)==1);
    }
    const uint64_t sizes[] = {0, 1, 8, 16, 24, 63, 64, 255, 1024, 8128, 8144, 8192, UINT64_MAX - 48, UINT64_MAX - 31, UINT64_MAX - 15, UINT64_MAX};
    const uint64_t strides[] = {0, 1, 3, 8, 24, 512, UINT64_MAX};
    const uint64_t counts[] = {0, 1, 2, 1000, UINT64_MAX};
    for (unsigned s = 0; s < sizeof(sizes)/sizeof(*sizes); s++)
    for (unsigned t = 0; t < sizeof(strides)/sizeof(*strides); t++)
    for (unsigned n = 0; n < sizeof(counts)/sizeof(*counts); n++) {
        heap_fixture_init(a); heap_fixture_init(c);
        a->descriptor.fixed_bytes = c->descriptor.fixed_bytes = sizes[s];
        a->descriptor.element_bytes = c->descriptor.element_bytes = strides[t];
        a->request.elements = c->request.elements = counts[n];
        uint64_t expected;
        // 128-bit test arithmetic is independent of the runtime's checks.
        // Supported native validation compilers provide unsigned __int128.
        __uint128_t payload = (__uint128_t)sizes[s] + (__uint128_t)strides[t] * counts[n];
        __uint128_t rounded = (payload + 15) & ~(__uint128_t)15;
        if (!rounded) rounded = 16;
        if (!strides[t] && counts[n]) expected = GC_ALLOC_INVALID;
        else if (payload > UINT64_MAX || rounded > UINT64_MAX - sizeof(ObjectHeader)) expected = GC_ALLOC_OVERFLOW;
        else if (rounded + sizeof(ObjectHeader) > sizeof(a->heap) - 16) expected = GC_ALLOC_REFILL;
        else expected = GC_ALLOC_OK;
        heap_compare(a, c, candidate, expected);
    }
    for (uint64_t epochs = 0; epochs < 32; epochs++)
    for (uint64_t young = 0; young < 8; young++) for (uint64_t old = 0; old < 8; old++) {
        heap_fixture_init(a); heap_fixture_init(c);
        uint64_t phase = UINT64_C(0x123456789abc0000) | epochs << 8 | old << 4 | young;
        atomic_store(&a->ctx.adopted_phase, phase); atomic_store(&c->ctx.adopted_phase, phase);
        a->descriptor.fixed_bytes = c->descriptor.fixed_bytes = 24;
        heap_compare(a, c, candidate, GC_ALLOC_OK);
    }
    // Reject every misaligned native control before dereference or mutation.
    for (unsigned field = 0; field < 4; field++) for (unsigned byte = 1; byte < 8; byte++) {
        heap_fixture_init(a); heap_fixture_init(c);
        uint64_t *left[] = {&a->request.descriptor, &a->request.output, &a->request.page, &a->page.starts};
        uint64_t *right[] = {&c->request.descriptor, &c->request.output, &c->request.page, &c->page.starts};
        *left[field] += byte; *right[field] += byte;
        heap_compare(a, c, candidate, GC_ALLOC_INVALID);
    }
    for (unsigned fault = 0; fault < 36; fault++) {
        heap_fixture_init(a); heap_fixture_init(c);
        heap_fault(a, fault); heap_fault(c, fault);
        heap_compare(a, c, candidate, GC_ALLOC_INVALID);
    }
    // Exhaustion is a retryable, separate resource failure: no heap/record
    // publication occurs, and an enlarged prepared arena can satisfy retry.
    heap_fixture_init(a);heap_fixture_init(c);
    a->marks.capacity=c->marks.capacity=1;
    heap_compare(a,c,candidate,GC_ALLOC_OK);
    heap_compare(a,c,candidate,GC_ALLOC_METADATA_REFILL);
    a->marks.capacity=c->marks.capacity=2;
    heap_compare(a,c,candidate,GC_ALLOC_OK);
    assert(a->records[0].object_id!=a->records[1].object_id);
    heap_fixture_init(a);heap_fixture_init(c);a->request.marks=c->request.marks=0;
    heap_compare(a,c,candidate,GC_ALLOC_METADATA_REFILL);
    for(unsigned fault=0;fault<8+sizeof(MarkRecord)/8;fault++) {
        heap_fixture_init(a);heap_fixture_init(c);
        switch(fault){case 0:a->request.marks++;c->request.marks++;break;
        case 1:a->marks.records=c->marks.records=0;break;
        case 2:a->marks.records++;c->marks.records++;break;
        case 3:a->marks.capacity=c->marks.capacity=0;break;
        case 4:a->marks.capacity=c->marks.capacity=UINT64_MAX;break;
        case 5:a->marks.records=c->marks.records=UINT64_MAX-7;break;
        case 6:a->marks.used=c->marks.used=172;break;
        case 7:a->marks.used=c->marks.used=UINT64_MAX;break;
        default:{uint64_t dirty=1;memcpy((unsigned char *)&a->records[0]+(fault-8)*8,&dirty,8);memcpy((unsigned char *)&c->records[0]+(fault-8)*8,&dirty,8);break;}}
        heap_compare(a,c,candidate,GC_ALLOC_INVALID);
    }
    heap_fixture_init(a);heap_fixture_init(c);a->page.forward_table=c->page.forward_table=0;
    heap_compare(a,c,candidate,GC_ALLOC_METADATA_REFILL);
    heap_fixture_init(a);heap_fixture_init(c);a->forwards.capacity=c->forwards.capacity=1;
    heap_compare(a,c,candidate,GC_ALLOC_OK);heap_compare(a,c,candidate,GC_ALLOC_METADATA_REFILL);
    for(unsigned fault=0;fault<12;fault++) {
        heap_fixture_init(a);heap_fixture_init(c);
        switch(fault){case 0:a->page.forward_table++;c->page.forward_table++;break;
        case 1:a->forwards.cells=c->forwards.cells=0;break;case 2:a->forwards.cells++;c->forwards.cells++;break;
        case 3:a->forwards.capacity=c->forwards.capacity=0;break;case 4:a->forwards.capacity=c->forwards.capacity=UINT64_MAX;break;
        case 5:a->forwards.cells=c->forwards.cells=UINT64_MAX-7;break;
        case 6:atomic_store(&a->forwards.used,172);atomic_store(&c->forwards.used,172);break;
        case 7:atomic_store(&a->forwards.used,UINT64_MAX);atomic_store(&c->forwards.used,UINT64_MAX);break;
        case 8:a->cells[0].source_offset=c->cells[0].source_offset=16;break;
        case 9:a->cells[0].incarnation=c->cells[0].incarnation=1;break;
        case 10:atomic_store(&a->cells[0].word,1);atomic_store(&c->cells[0].word,1);break;
        case 11:atomic_store(&a->cells[0].access,1);atomic_store(&c->cells[0].access,1);break;}
        heap_compare(a,c,candidate,GC_ALLOC_INVALID);
    }
    for(unsigned fault=0;fault<4;fault++) {
        heap_fixture_init(a);heap_fixture_init(c);heap_compare(a,c,candidate,GC_ALLOC_OK);
        if(fault==0)a->cells[0].source_offset=c->cells[0].source_offset=96;
        else if(fault==1)a->cells[0].source_offset=c->cells[0].source_offset=112;
        else a->cells[0].incarnation=c->cells[0].incarnation=fault==2?0:18;
        heap_compare(a,c,candidate,GC_ALLOC_INVALID);
    }
    heap_fixture_init(a); heap_fixture_init(c);
    a->request.page = c->request.page = 0;
    heap_compare(a, c, candidate, GC_ALLOC_REFILL);
    heap_fixture_init(a); heap_fixture_init(c);
    a->page.identity_base = c->page.identity_base = UINT64_MAX - 1;
    heap_compare(a, c, candidate, GC_ALLOC_OVERFLOW);
    a->page.identity_base = c->page.identity_base = UINT64_MAX - 2;
    heap_compare(a, c, candidate, GC_ALLOC_OK);
    assert(a->output.object_id == UINT64_MAX);
    for (uint64_t alignment = 1; alignment <= 16; alignment *= 2) {
        heap_fixture_init(a); heap_fixture_init(c);
        a->descriptor.alignment = c->descriptor.alignment = alignment;
        uint64_t count=alignment==1?0:alignment==2?UINT64_MAX-49:alignment==4?UINT64_MAX-48:alignment==8?UINT64_MAX-47:UINT64_MAX;
        atomic_store(&a->ctx.allocated_bytes,count);atomic_store(&c->ctx.allocated_bytes,count);
        heap_compare(a, c, candidate, GC_ALLOC_OK);
    }
    heap_fixture_init(a); heap_fixture_init(c);
    a->descriptor.element_bytes = c->descriptor.element_bytes = 1;
    a->request.elements = c->request.elements = 944;
    heap_compare(a, c, candidate, GC_ALLOC_OK);
    a->request.elements = c->request.elements = 0;
    heap_compare(a, c, candidate, GC_ALLOC_OK);
    assert(atomic_load(&a->starts[0]) & (UINT64_C(1) << 63));
    heap_compare(a, c, candidate, GC_ALLOC_OK);
    // Fill to the region boundary. Exact-fit zero payload has a reserved user
    // granule inside the page. Rollover must preserve the last valid object.
    heap_fixture_init(a); heap_fixture_init(c);
    a->descriptor.element_bytes = c->descriptor.element_bytes = 1;
    a->request.elements = c->request.elements = 8096;
    heap_compare(a, c, candidate, GC_ALLOC_OK);
    a->request.elements = c->request.elements = 0;
    heap_compare(a, c, candidate, GC_ALLOC_OK);
    assert(a->ctx.tlab_top == a->ctx.tlab_end);
    assert((a->output.reference >> 16) == sizeof(a->heap) - 16);
    heap_compare(a, c, candidate, GC_ALLOC_REFILL);
    // Walk object starts across multiple bitmap words with varied-sized objects.
    heap_fixture_init(a); heap_fixture_init(c);
    a->descriptor.element_bytes = c->descriptor.element_bytes = 1;
    for (unsigned i = 0; i < 64; i++) {
        a->request.elements = c->request.elements = i % 4 * 8;
        heap_compare(a, c, candidate, GC_ALLOC_OK);
    }
    free(a); free(c);
    printf("%u C/candidate TLAB cases PASS\n", heap_case_count);
    puts("TLAB allocation: sizes, colors, object starts, rollover and failure preservation PASS");
    if (heap_vectors) { assert(fclose(heap_vectors) == 0); heap_vectors = NULL; }
}
#endif
