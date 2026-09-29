#ifndef KLASSIC_GC_DESCRIPTOR_CONCURRENCY_H
#define KLASSIC_GC_DESCRIPTOR_CONCURRENCY_H
#include "gc_descriptor_cases.h"
#ifdef _WIN32
#include <windows.h>
#define DESC_THREAD_RESULT DWORD WINAPI
typedef HANDLE DescriptorThread;
static void descriptor_yield(void) { SwitchToThread(); }
#else
#include <pthread.h>
#include <sched.h>
#define DESC_THREAD_RESULT void *
typedef pthread_t DescriptorThread;
static void descriptor_yield(void) { sched_yield(); }
#endif
typedef struct {
    DescriptorRegistry registry;
    _Atomic uint64_t slots[4], start;
    TypeDescriptor descriptors[2];
    uint64_t bitmaps[2], results[2];
    DescriptorRegister reg;
    DescriptorScanner scan;
} DescriptorRace;
typedef struct { DescriptorRace *race; unsigned actor; } DescriptorRaceJob;
static DESC_THREAD_RESULT descriptor_racer(void *argument) {
    DescriptorRaceJob *job = argument;
    DescriptorRace *r = job->race;
    while (!atomic_load_explicit(&r->start, memory_order_acquire)) descriptor_yield();
    if (job->actor < 2) {
        unsigned i = job->actor;
        // Initialized after thread start: only registry publication can make
        // these non-atomic descriptor/bitmap writes visible to the reader.
        r->bitmaps[i] = i ? 10 : 1;
        r->descriptors[i] = (TypeDescriptor){.id = 3, .alignment = 8, .fixed_bytes = 32,
            .fixed_bitmap = (uintptr_t)&r->bitmaps[i], .fixed_bitmap_words = 1};
        r->results[i] = r->reg(&r->registry, &r->descriptors[i]);
    } else {
        uint64_t output[4] = {0};
        DescriptorScan scan = {3, 32, 0, 4, (uintptr_t)output, 4, 0, 0};
        uint64_t status;
        do {
            status = r->scan(&r->registry, &scan);
            if (status == GC_SCAN_INVALID) descriptor_yield();
        } while (status == GC_SCAN_INVALID);
        assert(status == GC_SCAN_DONE && scan.examined == 4 && scan.next_offset == 32);
        assert((scan.written == 1 && output[0] == 0) ||
               (scan.written == 2 && output[0] == 8 && output[1] == 24));
    }
    return 0;
}
static void gc_run_descriptor_races(DescriptorRegister reg, DescriptorScanner scan) {
    for (unsigned round = 0; round < 64; round++) {
        DescriptorRace r = {.reg = reg, .scan = scan};
        r.registry = (DescriptorRegistry){(uintptr_t)r.slots, 4};
        DescriptorRaceJob jobs[3]; DescriptorThread threads[3];
        for (unsigned i = 0; i < 3; i++) {
            jobs[i] = (DescriptorRaceJob){&r, i};
#ifdef _WIN32
            threads[i] = CreateThread(NULL, 0, descriptor_racer, &jobs[i], 0, NULL);
            assert(threads[i]);
#else
            assert(pthread_create(&threads[i], NULL, descriptor_racer, &jobs[i]) == 0);
#endif
        }
        atomic_store_explicit(&r.start, 1, memory_order_release);
        for (unsigned i = 0; i < 3; i++) {
#ifdef _WIN32
            assert(WaitForSingleObject(threads[i], 30000) == WAIT_OBJECT_0);
            assert(CloseHandle(threads[i]));
#else
            assert(pthread_join(threads[i], NULL) == 0);
#endif
        }
        assert((r.results[0] == GC_DESC_OK && r.results[1] == GC_DESC_DUPLICATE) ||
               (r.results[1] == GC_DESC_OK && r.results[0] == GC_DESC_DUPLICATE));
        unsigned winner = r.results[0] == GC_DESC_OK ? 0 : 1;
        assert(atomic_load(&r.slots[3]) == (uintptr_t)&r.descriptors[winner]);
    }
    puts("descriptor duplicate race and immutable publication: 64 rounds PASS");
}
#undef DESC_THREAD_RESULT
#endif
