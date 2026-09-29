#ifndef KLASSIC_GC_ROOT_WALK_CONCURRENCY_H
#define KLASSIC_GC_ROOT_WALK_CONCURRENCY_H
#include "gc_root_walk_cases.h"
#include "gc_root_lifetime_concurrency.h"

typedef struct {
    WalkFixture f;
    RootLifetimeRace sync; /* Reuse the portable mutex and ready/resume pair. */
    RootWalker step;
    RootWalkScanner scan;
    RootLife life;
    RootClear clear;
    RootLeasedScan leased;
    unsigned removed, visited, deleted;
} RootWalkRace;

static uint64_t walk_race_step(RootWalkRace *r, RootWalk *w, uint64_t op) {
    life_lock(&r->sync);
    uint64_t result = r->step(&r->f.g, w, op);
    life_unlock(&r->sync);
    return result;
}
static uint64_t walk_race_life(RootWalkRace *r, uint64_t op) {
    life_lock(&r->sync);
    uint64_t result = r->life(&r->f.g, &r->f.life, op);
    life_unlock(&r->sync);
    return result;
}

#ifdef _WIN32
static DWORD WINAPI walk_race_reader(void *arg)
#else
static void *walk_race_reader(void *arg)
#endif
{
    RootWalkRace *r = arg;
    WalkFixture *f = &r->f;
    assert(walk_race_step(r, &f->w, GC_RW_BEGIN) == GC_RW_ITEM);
    assert(f->w.lease.registration == (uintptr_t)&f->r[2]);
    atomic_store_explicit(&r->sync.ready, 1, memory_order_release);
    while (!atomic_load_explicit(&r->sync.resume, memory_order_acquire)) root_yield();
    uint64_t values[1];
    do {
        unsigned i = (unsigned)((RootRegistration *)(uintptr_t)f->w.lease.registration - f->r);
        assert(i < 3 && (i != r->removed || i == 2));
        RootWalkScan q = {(uintptr_t)&f->w, f->w.lease.serial, (uintptr_t)values, 1, 0};
        uint64_t count = 0;
        do {
            uint64_t result = r->scan(&q);
            assert(result == (count + 1 == f->w.limit ? 0u : 2u));
            assert(q.written == 1 && values[0] == walk_value(i, count));
            count++;
        } while (f->w.cursor < f->w.limit);
        r->visited |= 1u << i;
        // Symbolic visitor only: real graph work must precede this certificate.
        f->w.completed_mask = f->w.mask;
        uint64_t result = walk_race_step(r, &f->w, GC_RW_NEXT);
        assert(result == (f->w.active ? GC_RW_ITEM : GC_RW_DONE));
        assert(r->scan(&q) == GC_ROOT_INVALID); // old serial, even with control reuse
    } while (f->w.active);
    return 0;
}

static void walk_race_delete(RootWalkRace *r, unsigned i) {
    WalkFixture *f = &r->f;
    walk_select(f, i);
    assert(walk_race_life(r, GC_RL_RETIRE) == GC_RL_OK);
    while (f->r[i].cursor < f->r[i].limit) {
        assert(walk_race_life(r, GC_RL_CLAIM) == GC_RL_OK);
        uint64_t values[256];
        RootScanJob job = {.scan = {f->r[i].segment, f->life.begin, (uintptr_t)values, f->life.end-f->life.begin, 0},
            .lease = (uintptr_t)&f->worker, .serial = f->life.serial};
        assert(r->leased(&job) == (f->life.end == f->r[i].limit ? 0u : 2u));
        for (uint64_t j = 0; j < job.scan.written; j++)
            assert(values[j] == walk_value(i, f->life.begin+j));
        assert(f->life.mask == 3);
        assert(walk_race_life(r, GC_RL_ACK) == GC_RL_OK);
    }
    r->deleted |= 1u << i;
}

static void walk_race_finish(RootWalkRace *r) {
    assert(walk_race_life(r, GC_RL_CLEAN) == GC_RL_OK);
    uint64_t status;
    do { status = r->clear(&r->f.life); } while (status == GC_RL_WAIT);
    assert(status == GC_RL_OK);
    assert(walk_race_life(r, GC_RL_FINISH) == GC_RL_OK);
}

static void gc_run_root_walk_races(RootWalker step, RootWalkScanner scan, RootLife life, RootClear clear, RootLeasedScan leased) {
    RootWalkRace *r = calloc(1, sizeof(*r)); assert(r);
    r->step=step; r->scan=scan; r->life=life; r->clear=clear; r->leased=leased;
#ifdef _WIN32
    InitializeCriticalSection(&r->sync.lock);
#else
    assert(pthread_mutex_init(&r->sync.lock, NULL) == 0);
#endif
    for (unsigned round=0; round<64; round++) {
        WalkFixture *f=&r->f;
        walk_fixture(f, 0x22); r->removed=round%3; r->visited=r->deleted=0;
        atomic_store(&r->sync.ready, 0); atomic_store(&r->sync.resume, 0);
        for (unsigned i=0; i<3; i++) walk_attach(f, i, i ? 2 : 300, life);
        RootThread reader;
#ifdef _WIN32
        reader=CreateThread(NULL, 0, walk_race_reader, r, 0, NULL); assert(reader);
#else
        assert(pthread_create(&reader, NULL, walk_race_reader, r) == 0);
#endif
        while (!atomic_load_explicit(&r->sync.ready, memory_order_acquire)) root_yield();
        walk_race_delete(r, r->removed);
        if (r->removed == 2) {
            assert(walk_race_life(r, GC_RL_CLEAN) == GC_RL_WAIT);
        } else {
            walk_race_finish(r);
            // Reuse the same registration/cells at a newer head scope.
            assert(walk_race_life(r, GC_RL_ATTACH) == GC_RL_OK);
        }
        walk_select(f, 3);
        assert(walk_race_life(r, GC_RL_ATTACH) == GC_RL_OK);
        RootRequest q = {.segment=(uintptr_t)&f->s[3]};
        assert(gc_root_step(&f->ctx, &q, 0) == 0);
        q.value=walk_value(3, 0); assert(gc_root_step(&f->ctx, &q, 1) == 0);
        // A second discovery starts after insertion and independently pins it.
        RootWalk second = {0};
        assert(walk_race_step(r, &second, GC_RW_BEGIN) == GC_RW_ITEM);
        assert(f->g.walkers == 2 && f->g.leases == 2);
        atomic_store_explicit(&r->sync.resume, 1, memory_order_release);
#ifdef _WIN32
        assert(WaitForSingleObject(reader, 30000) == WAIT_OBJECT_0); assert(CloseHandle(reader));
#else
        assert(pthread_join(reader, NULL) == 0);
#endif
        assert((r->visited | r->deleted) == 7);
        assert(f->g.walkers == 1 && f->g.leases == 1);
        RootChange changes[8]; RootDrain drain={(uintptr_t)&f->buffer, (uintptr_t)changes, 8, 0};
        assert(gc_root_drain(&drain)==0 && drain.written==1);
        assert(changes[0].previous==0 && changes[0].next==walk_value(3, 0));
        assert(changes[0].phase==0x22 && changes[0].scope==f->s[3].scope);
        if (r->removed == 2) { walk_select(f, 2); walk_race_finish(r); }
        // Complete the second walk with an independent control and serial.
        unsigned seen=0;
        do {
            unsigned i=(unsigned)((RootRegistration *)(uintptr_t)second.lease.registration-f->r);
            seen |= 1u<<i;
            uint64_t values[256]; RootWalkScan sq={(uintptr_t)&second, second.lease.serial, (uintptr_t)values, 256, 0};
            uint64_t count=0, result;
            do {
                result=scan(&sq); assert(result==0 || result==2);
                for(uint64_t j=0; j<sq.written; j++)assert(values[j]==walk_value(i, count+j));
                count+=sq.written;
            } while(result==2);
            assert(count==second.limit); second.completed_mask=second.mask;
            result=walk_race_step(r, &second, GC_RW_NEXT);
            assert(result==(second.active?GC_RW_ITEM:GC_RW_DONE));
        } while(second.active);
        assert((seen & 8) && !f->g.walkers && !f->g.leases);
    }
#ifdef _WIN32
    DeleteCriticalSection(&r->sync.lock);
#else
    assert(pthread_mutex_destroy(&r->sync.lock) == 0);
#endif
    free(r);
    puts("root inventory: 64 delayed-walker, unlink/reuse, publication-journal and two-walker rounds PASS");
}
#endif
