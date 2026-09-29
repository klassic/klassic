#ifndef KLASSIC_GC_ROOTS_CONCURRENCY_H
#define KLASSIC_GC_ROOTS_CONCURRENCY_H
#include "gc_roots_cases.h"
#ifdef _WIN32
#include <windows.h>
#define ROOT_THREAD_RESULT DWORD WINAPI
typedef HANDLE RootThread;
static void root_yield(void) { SwitchToThread(); }
#else
#include <pthread.h>
#include <sched.h>
#define ROOT_THREAD_RESULT void *
typedef pthread_t RootThread;
static void root_yield(void) { sched_yield(); }
#endif
/* Symbolic references test root/journal visibility and retention, not moving
 * graph semantics. Fixture storage is leased until both workers join. */
typedef struct {
    MutatorContext ctx;
    RootSegment segment;
    RootCell cells[32];
    BufferCursor buffer;
    RootChange entries[3];
    _Atomic uint64_t scanner_ready, finished, marks;
    RootStep step; RootScanner scan; RootDrainer drain;
} RootRace;
static void root_observe(RootRace *r, uint64_t value) {
    if (!value) return;
    uint64_t id = value >> 20;
    assert(id >= 1 && id <= 64 && (value & UINT64_C(0xfffff)) == 0x1510);
    atomic_fetch_or_explicit(&r->marks, UINT64_C(1) << (id - 1), memory_order_relaxed);
}
static ROOT_THREAD_RESULT root_snapshot_worker(void *argument) {
    RootRace *r = argument; uint64_t output[2] = {0};
    RootScan q = {(uintptr_t)&r->segment, 0, (uintptr_t)output, 1, 0};
    assert(r->scan(&q) == 2 && q.written == 1 && q.cursor == 1);
    root_observe(r, output[0]);
    atomic_store_explicit(&r->scanner_ready, 1, memory_order_release);
    // Force pop/overwrite before the remaining cells are visited. The old
    // referents must survive through the pre-mutation journal, not this scan.
    while (!atomic_load_explicit(&r->finished, memory_order_acquire)) root_yield();
    assert(atomic_load_explicit(&r->segment.state, memory_order_acquire) == GC_ROOT_SEGMENT_RETIRING);
    for (;;) {
        uint64_t status = r->scan(&q);
        assert(status != GC_ROOT_INVALID && q.written <= 1);
        for (uint64_t i = 0; i < q.written; i++) root_observe(r, output[i]);
        if (status == 0) break;
    }
    assert(q.cursor == 32);
    return 0;
}
static ROOT_THREAD_RESULT root_journal_worker(void *argument) {
    RootRace *r = argument; RootChange output[2];
    RootDrain q = {(uintptr_t)&r->buffer, (uintptr_t)output, 2, 0};
    uint64_t consumed = 0;
    for (;;) {
        uint64_t status = r->drain(&q);
        assert(status != GC_ROOT_INVALID && q.written <= 2);
        for (uint64_t i = 0; i < q.written; i++) {
            assert(output[i].scope == 101 && output[i].phase == UINT64_C(0x12340122));
            root_observe(r, output[i].previous); root_observe(r, output[i].next);
        }
        consumed += q.written;
        if (atomic_load_explicit(&r->finished, memory_order_acquire) &&
            atomic_load_explicit(&r->buffer.published, memory_order_acquire) ==
            atomic_load_explicit(&r->buffer.consumed, memory_order_acquire)) break;
        root_yield();
    }
    assert(consumed == 32);
    assert(atomic_load_explicit(&r->ctx.root_revision,memory_order_acquire)==64);
    return 0;
}
static void gc_run_root_races(RootStep step, RootScanner scan, RootDrainer drain) {
    for (unsigned round = 0; round < 64; round++) {
        RootRace r = {.step = step, .scan = scan, .drain = drain};
        r.ctx.abi_version = GC_ABI_VERSION; r.ctx.thread_id = 17;
        atomic_store(&r.ctx.adopted_phase, UINT64_C(0x12340122));
        r.segment.owner = 17; r.segment.scope = 101; r.segment.cells = (uintptr_t)r.cells;
        r.segment.capacity = 32; r.segment.journal = (uintptr_t)&r.buffer;
        atomic_store(&r.segment.state, GC_ROOT_SEGMENT_ACTIVE);
        r.buffer.entries = (uintptr_t)r.entries; r.buffer.capacity = 3;
        RootHandle handles[32];
        for (unsigned i = 0; i < 32; i++) {
            RootRequest q = {.segment = (uintptr_t)&r.segment};
            assert(step(&r.ctx, &q, GC_ROOT_RESERVE) == 0); handles[i] = q.handle;
            q.value = ((uint64_t)(i + 1) << 20) | 0x1510;
            assert(step(&r.ctx, &q, GC_ROOT_STORE) == 0);
            RootChange initial;
            RootDrain d = {(uintptr_t)&r.buffer, (uintptr_t)&initial, 1, 0};
            assert(drain(&d) == 0 && d.written == 1);
            assert(initial.next == q.value);
        }
        RootThread workers[2];
#ifdef _WIN32
        workers[0] = CreateThread(NULL, 0, root_snapshot_worker, &r, 0, NULL);
        workers[1] = CreateThread(NULL, 0, root_journal_worker, &r, 0, NULL);
        assert(workers[0] && workers[1]);
#else
        assert(pthread_create(&workers[0], NULL, root_snapshot_worker, &r) == 0);
        assert(pthread_create(&workers[1], NULL, root_journal_worker, &r) == 0);
#endif
        while (!atomic_load_explicit(&r.scanner_ready, memory_order_acquire)) root_yield();
        uint64_t expected = UINT32_MAX;
        for (unsigned i = 0; i < 32; i++) {
            RootRequest q = {(uintptr_t)&r.segment, handles[i], ((uint64_t)(i + 33) << 20) | 0x1510};
            uint64_t status, op = i & 1 ? GC_ROOT_STORE : GC_ROOT_RELEASE;
            do {
                status = step(&r.ctx, &q, op);
                if (status == GC_ROOT_FLUSH) root_yield();
            } while (status == GC_ROOT_FLUSH);
            assert(status == GC_ROOT_OK);
            if (i & 1) expected |= UINT64_C(1) << (i + 32);
        }
        RootRequest retire = {.segment = (uintptr_t)&r.segment};
        assert(step(&r.ctx, &retire, GC_ROOT_RETIRE) == 0);
        atomic_store_explicit(&r.finished, 1, memory_order_release);
        for (unsigned i = 0; i < 2; i++) {
#ifdef _WIN32
            assert(WaitForSingleObject(workers[i], 30000) == WAIT_OBJECT_0); assert(CloseHandle(workers[i]));
#else
            assert(pthread_join(workers[i], NULL) == 0);
#endif
        }
        assert(atomic_load(&r.marks) == expected);
        assert(atomic_load(&r.ctx.root_revision)==64);
        assert(atomic_load(&r.buffer.consumed) == 64 && atomic_load(&r.buffer.published) == 64);
    }
    puts("root overwrite/clear, journal wrap and deferred frame scan: 64 rounds PASS");
}
#undef ROOT_THREAD_RESULT
#endif
