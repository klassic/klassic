#define _GNU_SOURCE
#include "gc_native_contract_code.h"
#include "gc_metadata.h"
#include "gc_root_lifetime.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

static void *executable(const unsigned char *bytes, size_t length) {
    long page = sysconf(_SC_PAGESIZE);
    assert(page > 0);
    size_t size = (length + (size_t)page - 1) / (size_t)page * (size_t)page;
    void *p = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    assert(p != MAP_FAILED);
    memcpy(p, bytes, length);
    assert(!mprotect(p, size, PROT_READ | PROT_EXEC));
    return p;
}
typedef uint64_t (*Metadata)(MetadataLeasePool *, MetadataLeaseRequest *, uint64_t, uint64_t *);
typedef uint64_t (*Clear)(RootLifeRequest *);

static void metadata_hooks(void) {
    Metadata step = (Metadata)executable(metadata_code, sizeof(metadata_code));
    AllocationPage page = {.incarnation=17,.run_head=0};
    atomic_store(&page.state, GC_PAGE_CLOSED);
    HeapPool heap = {.pages=(uintptr_t)&page,.page_count=1};
    MetadataLeaseSlot slot = {0};
    MetadataLeasePool pool = {(uintptr_t)&heap,(uintptr_t)&slot,1,1,0};
    MetadataLeaseRequest q = {(uintptr_t)&page,17,0};
    uint64_t events[3] = {0};
    assert(step(&pool,&q,GC_ML_ACQUIRE,events)==GC_ML_OK);
    assert(events[0]==1 && !events[1] && events[2]==(uintptr_t)&slot);
    uint64_t token=q.token;
    assert(token && pool.active==1 && page.metadata_leases==1);
    q.token=0;
    assert(step(&pool,&q,GC_ML_ACQUIRE,events)==GC_ML_FULL);
    assert(events[0]==1 && events[1]==1 && !q.token && pool.active==1);
    q.token=token;
    assert(step(&pool,&q,GC_ML_RESOLVE,events)==GC_ML_OK);
    assert(events[0]==1 && events[1]==1);
    assert(step(&pool,&q,GC_ML_RELEASE,events)==GC_ML_OK);
    assert(events[0]==2 && events[1]==1 && !pool.active && !page.metadata_leases);
    assert(step(&pool,&q,GC_ML_RELEASE,events)==GC_ML_INVALID);
    assert(events[0]==2 && events[1]==1);
}

static void bounded_cleanup(const unsigned char *code, size_t length, uint64_t quantum) {
    Clear clear = (Clear)executable(code,length);
    RootCell cells[19] = {0};
    RootSegment segment = {.cells=(uintptr_t)cells,.capacity=19,.state=3};
    RootRegistration registration = {.segment=(uintptr_t)&segment,.scope=5,.registered=1,
        .leases=1,.limit=19,.claim_serial=9};
    RootLease lease = {.registration=(uintptr_t)&registration,.serial=9,.scope=5,.kind=3};
    RootLifeRequest q = {.registration=(uintptr_t)&registration,.lease=(uintptr_t)&lease,
        .scope=5,.serial=9};
    for (unsigned i=0;i<19;i++) {
        atomic_store(&cells[i].value,0x1510+((uint64_t)(i+1)<<20));
        cells[i].active=1;
        cells[i].reuse_generation=i+3;
    }
    q.serial=8;
    assert(clear(&q)==GC_RL_INVALID && !registration.cleanup_cursor);
    assert(atomic_load(&cells[0].value) && cells[0].active);
    q.serial=9;
    unsigned calls=0;
    do {
        uint64_t before=registration.cleanup_cursor;
        uint64_t expected=19-before>quantum?quantum:19-before;
        uint64_t status=clear(&q);
        assert(registration.cleanup_cursor==before+expected);
        assert(status==(registration.cleanup_cursor==19?GC_RL_OK:GC_RL_WAIT));
        for (unsigned i=0;i<19;i++) {
            assert(cells[i].reuse_generation==i+3);
            assert(cells[i].active==(i>=registration.cleanup_cursor));
            assert((atomic_load(&cells[i].value)!=0)==(i>=registration.cleanup_cursor));
        }
        calls++;
    } while (registration.cleanup_cursor<19);
    assert(calls==(19+quantum-1)/quantum);
    assert(clear(&q)==GC_RL_OK);
    lease.kind=0;
    assert(clear(&q)==GC_RL_INVALID);
}

int main(void) {
    metadata_hooks();
    bounded_cleanup(clear_1_code,sizeof(clear_1_code),1);
    bounded_cleanup(clear_7_code,sizeof(clear_7_code),7);
    bounded_cleanup(clear_256_code,sizeof(clear_256_code),256);
    puts("shared native leaf hooks and finite cleanup: PASS");
}
