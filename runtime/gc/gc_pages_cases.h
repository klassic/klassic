#ifndef KLASSIC_GC_PAGES_CASES_H
#define KLASSIC_GC_PAGES_CASES_H
#include "gc_pages.h"
#include "gc_heap.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*PoolStep)(HeapPool *, PageLease *, uint64_t);
typedef uint64_t (*TlabPage)(MutatorContext *, AllocationPage *, uint64_t);
/* Metadata snapshots use synthetic mapping/bitmap addresses, never dereference
 * them. The separate pipeline establishes real OS preparation. */
typedef struct { HeapPool h; PageLease l; AllocationPage pages[8]; } PoolFixture;
static unsigned pool_cases;
static FILE *pool_vectors;
static void pool_fixture(PoolFixture *f, unsigned occupied) {
    memset(f, 0, sizeof(*f));
    f->h.heap_base = UINT64_C(0x100000000);
    f->h.pages = (uintptr_t)f->pages;
    f->h.page_count = 8;
    f->h.hard_max = 8 * GC_HEAP_PAGE_BYTES;
    f->h.soft_max = 2 * GC_HEAP_PAGE_BYTES;
    f->h.relocation_reserve = 2 * GC_HEAP_PAGE_BYTES;
    f->h.metadata_committed = sizeof(f->pages);
    f->h.last_identity = 100000;
    f->h.last_incarnation = 50;
    for (unsigned i = 0; i < 8; i++) if (occupied & (1u << i)) {
        atomic_store(&f->pages[i].state, GC_PAGE_CLOSED);
        f->h.committed += GC_HEAP_PAGE_BYTES;
        f->h.metadata_committed += GC_HEAP_PAGE_BYTES / 128;
    }
    f->h.metadata_max = f->h.metadata_committed + 8 * GC_HEAP_PAGE_BYTES / 128;
    f->h.pressure = f->h.committed > f->h.soft_max;
    f->l.page_count = 1;
}
static void pool_copy(PoolFixture *dst, const PoolFixture *src) {
    memcpy(dst, src, sizeof(*dst));
    if (dst->h.pages) dst->h.pages = (uintptr_t)dst->pages;
}
static void pool_compare(PoolFixture *a, PoolFixture *c, PoolStep candidate, uint64_t op, uint64_t expected) {
    PoolFixture before_a, before_c;
    memcpy(&before_a, a, sizeof(*a)); memcpy(&before_c, c, sizeof(*c));
    uint64_t result = candidate(&a->h, &a->l, op);
    uint64_t reference = gc_pool_step_locked(&c->h, &c->l, op);
    assert(result == expected && reference == expected);
    if (pool_vectors) {
        PoolFixture before, after;
        memcpy(&before, &before_c, sizeof(before)); memcpy(&after, c, sizeof(after));
        uint64_t record[] = {op, reference, before.h.pages != 0, after.h.pages != 0};
        if (before.h.pages) before.h.pages = offsetof(PoolFixture, pages);
        if (after.h.pages) after.h.pages = offsetof(PoolFixture, pages);
        assert(fwrite(record, sizeof(record), 1, pool_vectors) == 1);
        assert(fwrite(&before, sizeof(before), 1, pool_vectors) == 1);
        assert(fwrite(&after, sizeof(after), 1, pool_vectors) == 1);
    }
    if (result) {
        assert(memcmp(a, &before_a, sizeof(*a)) == 0);
        assert(memcmp(c, &before_c, sizeof(*c)) == 0);
    }
    assert(a->h.pages == before_a.h.pages && c->h.pages == before_c.h.pages);
    PoolFixture x, y;
    memcpy(&x, a, sizeof(x)); memcpy(&y, c, sizeof(y));
    x.h.pages = y.h.pages = 0;
    assert(memcmp(&x, &y, sizeof(x)) == 0);
    if (!result) {
        assert(a->h.committed + a->h.pending <= a->h.hard_max);
        assert(a->h.metadata_committed + a->h.metadata_pending <= a->h.metadata_max);
        assert(a->h.pressure == (a->h.committed + a->h.pending > a->h.soft_max));
        if (op == GC_POOL_RESERVE) {
            assert(a->l.stage == 1 && !a->l.starts && !a->l.committed_mapping);
            assert(a->l.identity_base == before_a.h.last_identity + 1);
            assert(a->l.incarnation == before_a.h.last_incarnation + 1);
            assert(a->h.pending == before_a.h.pending + a->l.page_count * GC_HEAP_PAGE_BYTES);
            for (unsigned i = 0; i < 8; i++) {
                if (i >= a->l.head_index && i < a->l.head_index + a->l.page_count) {
                    AllocationPage *p = &a->pages[i];
                    assert(atomic_load(&before_a.pages[i].state) == GC_PAGE_FREE);
                    assert(p->start == (i + 1) * GC_HEAP_PAGE_BYTES);
                    assert(p->limit == (a->l.head_index + a->l.page_count + 1) * GC_HEAP_PAGE_BYTES);
                    assert(p->run_head == a->l.head_index && p->owner == 0);
                    assert(p->incarnation == a->l.incarnation && p->identity_base == a->l.identity_base);
                    assert(atomic_load(&p->state) == (i == a->l.head_index ? GC_PAGE_PREPARING : GC_PAGE_RUN_TAIL));
                } else assert(memcmp(&a->pages[i], &before_a.pages[i], sizeof(AllocationPage)) == 0);
            }
        } else if (op == GC_POOL_ABORT) {
            assert(a->l.stage == 3 && !a->l.starts && !a->l.committed_mapping);
            assert(a->h.last_identity == before_a.h.last_identity);
            assert(a->h.last_incarnation == before_a.h.last_incarnation);
        } else {
            assert(a->l.stage == 2 && !a->l.starts && !a->l.committed_mapping);
            assert(a->pages[a->l.head_index].starts == before_a.l.starts);
            assert(atomic_load(&a->pages[a->l.head_index].state) == GC_PAGE_READY);
        }
    }
    pool_cases++;
}
static void gc_run_pool_cases(PoolStep candidate) {
    PoolFixture a, c;
    pool_cases = 0;
    const char *path = getenv("KLASSIC_GC_POOL_VECTORS");
    if (path) {
        pool_vectors = fopen(path, "wb");
        assert(pool_vectors && fwrite("KLPAGE01", 8, 1, pool_vectors) == 1);
        uint64_t header[] = {sizeof(PoolFixture), offsetof(PoolFixture, h), offsetof(PoolFixture, l), 1,
            offsetof(PoolFixture, h.pages)};
        assert(fwrite(header, sizeof(header), 1, pool_vectors) == 1);
    }
    for (unsigned mask = 0; mask < 256; mask++) for (unsigned n = 1; n <= 8; n++)
    for (unsigned worker = 0; worker < 2; worker++) for (unsigned metadata = 0; metadata < 2; metadata++) {
        pool_fixture(&a, mask);
        a.l.page_count = n; a.l.relocation = worker; a.l.generation = worker;
        if (!metadata) a.h.metadata_max = a.h.metadata_committed;
        pool_copy(&c, &a);
        unsigned occupied = 0, run = 0, best = 0;
        for (unsigned i = 0; i < 8; i++) {
            if (mask & (1u << i)) { occupied++; run = 0; } else run++;
            if (run > best) best = run;
        }
        uint64_t expected = occupied + n > (worker ? 8u : 6u) ? GC_POOL_HEAP_LIMIT :
                            !metadata ? GC_POOL_METADATA_LIMIT : best < n ? GC_POOL_FRAGMENTED : GC_POOL_OK;
        pool_compare(&a, &c, candidate, GC_POOL_RESERVE, expected);
        if (!expected) {
            uint64_t id = a.l.identity_base;
            pool_compare(&a, &c, candidate, GC_POOL_ABORT, GC_POOL_OK);
            assert(a.h.pending == 0 && a.h.metadata_pending == 0);
            a.l = c.l = (PageLease){.page_count = n, .relocation = worker, .generation = worker};
            pool_compare(&a, &c, candidate, GC_POOL_RESERVE, GC_POOL_OK);
            assert(a.l.identity_base > id);
        }
    }
    for (unsigned fault = 0; fault < 24; fault++) {
        pool_fixture(&a, 0);
        uint64_t op = GC_POOL_RESERVE, expected = GC_POOL_INVALID;
        switch (fault) {
        case 0: op = UINT64_MAX; break;
        case 1: a.h.pages = 0; break;
        case 2: a.h.page_count = 0; break;
        case 3: a.h.page_count = GC_MAX_OFFSET >> GC_HEAP_PAGE_SHIFT; break;
        case 4: a.h.heap_base++; break;
        case 5: a.h.heap_base = UINT64_MAX - 15; break;
        case 6: a.h.hard_max = 0; break;
        case 7: a.h.hard_max++; break;
        case 8: a.h.soft_max = a.h.hard_max + 1; break;
        case 9: a.h.relocation_reserve = a.h.hard_max + 1; break;
        case 10: a.h.committed = a.h.hard_max + 1; break;
        case 11: a.h.pending = a.h.hard_max + 1; break;
        case 12: a.h.metadata_committed--; break;
        case 13: a.h.metadata_committed = a.h.metadata_max + 1; break;
        case 14: a.h.metadata_pending = a.h.metadata_max; break;
        case 15: a.l.page_count = 0; break;
        case 16: a.l.page_count = 9; break;
        case 17: a.l.generation = 2; break;
        case 18: a.l.relocation = 2; break;
        case 19: a.l.stage = 1; break;
        case 20: a.l.starts = 8; break;
        case 21: a.l.committed_mapping = 1; break;
        case 22: a.h.last_identity = UINT64_MAX - 1; expected = GC_POOL_EXHAUSTED; break;
        case 23: a.h.last_incarnation = UINT64_MAX; expected = GC_POOL_EXHAUSTED; break;
        }
        pool_copy(&c, &a);
        pool_compare(&a, &c, candidate, op, expected);
    }
    pool_fixture(&a, 0); a.l.page_count = 3; pool_copy(&c, &a);
    pool_compare(&a, &c, candidate, GC_POOL_RESERVE, GC_POOL_OK);
    PageLease original = a.l;
    a.l.page_count = c.l.page_count = 2;
    pool_compare(&a, &c, candidate, GC_POOL_ABORT, GC_POOL_INVALID);
    a.l = c.l = original;
    a.l.committed_mapping = c.l.committed_mapping = 1;
    pool_compare(&a, &c, candidate, GC_POOL_ABORT, GC_POOL_INVALID);
    pool_compare(&a, &c, candidate, GC_POOL_COMMIT, GC_POOL_INVALID);
    a.l.starts = c.l.starts = 0x8001;
    pool_compare(&a, &c, candidate, GC_POOL_COMMIT, GC_POOL_INVALID);
    a.l.starts = c.l.starts = 0x8000;
    pool_compare(&a, &c, candidate, GC_POOL_COMMIT, GC_POOL_OK);
    pool_compare(&a, &c, candidate, GC_POOL_ABORT, GC_POOL_INVALID);
    a.l = c.l = original;
    pool_compare(&a, &c, candidate, GC_POOL_ABORT, GC_POOL_INVALID);
    // Two adjacent aborted preparations become one reusable large run.
    pool_fixture(&a, 0); a.h.relocation_reserve = 0; a.l.page_count = 3; pool_copy(&c, &a);
    pool_compare(&a, &c, candidate, GC_POOL_RESERVE, GC_POOL_OK);
    PageLease left = a.l;
    a.l = c.l = (PageLease){.page_count = 5};
    pool_compare(&a, &c, candidate, GC_POOL_RESERVE, GC_POOL_OK);
    assert(a.l.head_index == 3);
    pool_compare(&a, &c, candidate, GC_POOL_ABORT, GC_POOL_OK);
    a.l = c.l = left;
    pool_compare(&a, &c, candidate, GC_POOL_ABORT, GC_POOL_OK);
    a.l = c.l = (PageLease){.page_count = 8};
    pool_compare(&a, &c, candidate, GC_POOL_RESERVE, GC_POOL_OK);
    assert(a.l.head_index == 0 && a.l.incarnation > left.incarnation);
    a.l = c.l = left;
    pool_compare(&a, &c, candidate, GC_POOL_ABORT, GC_POOL_INVALID);
    // Even a prematurely FREE-marked slot cannot overwrite retained metadata.
    pool_fixture(&a,0);a.pages[0].metadata_leases=1;pool_copy(&c,&a);
    pool_compare(&a,&c,candidate,GC_POOL_RESERVE,GC_POOL_OK);assert(a.l.head_index==1&&a.pages[0].metadata_leases==1);
    pool_fixture(&a,0);a.l.page_count=6;a.pages[2].metadata_leases=1;pool_copy(&c,&a);
    pool_compare(&a,&c,candidate,GC_POOL_RESERVE,GC_POOL_FRAGMENTED);
    pool_fixture(&a,0);pool_copy(&c,&a);pool_compare(&a,&c,candidate,GC_POOL_RESERVE,GC_POOL_OK);
    a.pages[0].metadata_leases=c.pages[0].metadata_leases=1;
    pool_compare(&a,&c,candidate,GC_POOL_ABORT,GC_POOL_INVALID);
    printf("%u C/candidate page-pool cases PASS\n", pool_cases);
    puts("page pool: budgets, reserve, fragmentation, rollback and stale leases PASS");
    if (pool_vectors) { assert(fclose(pool_vectors) == 0); pool_vectors = NULL; }
}
#endif
