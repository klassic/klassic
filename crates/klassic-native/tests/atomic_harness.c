#define _GNU_SOURCE
#include "atomic_progress.h"
#include "conformance_budget.h"
#include "../../../runtime/gc/gc_native_buffer_incoming_cases.h"
#include "../../../runtime/gc/gc_native_buffer_quiescence_cases.h"
#include "../../../runtime/gc/gc_native_heap_cases.h"
#include "../../../runtime/gc/gc_native_runtime_cases.h"
#include "../../../runtime/gc/gc_native_allocation_cases.h"
#include "../../../runtime/gc/gc_native_allocation_retire_cases.h"
#include "../../../runtime/gc/gc_native_bitmap_cases.h"
#include "../../../runtime/gc/gc_native_pin_wait_cases.h"
#include "../../../runtime/gc/gc_root_cancel_cases.h"
#include "../../../runtime/gc/gc_root_copy_cases.h"
#include "../../../runtime/gc/gc_logical_cas_cases.h"
#include "../../../runtime/gc/gc_heap_pressure_cases.h"
#include "../../../runtime/gc/gc_cycle_pressure_cases.h"
#include "../../../runtime/gc/gc_cycle_mark_start_cases.h"
#include "../../../runtime/gc/gc_native_cycle_cases.h"
#include "../../../runtime/gc/gc_native_graph_cases.h"
#include "../../../runtime/gc/gc_relocation_phase_cases.h"
#include "../../../runtime/gc/gc_root_storage_cases.h"
#include "../../../runtime/gc/gc_root_storage_disposal_race.h"
#include "../../../runtime/gc/gc_native_roots_cases.h"
#include "../../../runtime/gc/gc_native_root_publish_cases.h"
#include "../../../runtime/gc/gc_pending_follow_cases.h"
#include "../../../runtime/gc/gc_allocation_shape_cases.h"
#include "../../../runtime/gc/gc_native_builtin_registry_cases.h"
#include "../../../runtime/gc/gc_builtin_descriptor_cases.h"
#include "../../../runtime/gc/gc_mark_initializing_race.h"
#include "../../../runtime/gc/gc_initialization_cases.h"
#include "../../../runtime/gc/gc_scalar_cases.h"
#include "../../../runtime/gc/gc_rooted_scalar_cases.h"
#include "../../../runtime/gc/gc_rooted_store_commit_cases.h"
#include "../../../runtime/gc/gc_rooted_store_attempt_cases.h"
#include "../../../runtime/gc/gc_rooted_store_cases.h"
#include "../../../runtime/gc/gc_rooted_store_concurrency.h"
#include "../../../runtime/gc/gc_barrier_assist_cases.h"
#include "../../../runtime/gc/gc_initialization_store_cases.h"
#include "../../../runtime/gc/gc_initialization_copy_cases.h"
#include "../../../runtime/gc/gc_metadata_grow_cases.h"
#include "../../../runtime/gc/gc_native_metadata_cases.h"
#include "../../../runtime/gc/gc_relocation_job_cases.h"
#include "../../../runtime/gc/gc_relocation_complete_cases.h"
#include "../../../runtime/gc/gc_relocation_claim_cases.h"
#include "../../../runtime/gc/gc_relocation_cancel_cases.h"
#include "../../../runtime/gc/gc_alloc_into_cases.h"
#include "../../../runtime/gc/gc_alloc_start_cases.h"
#include "../../../runtime/gc/gc_relocation_claim_concurrency.h"
#include "../../../runtime/gc/gc_root_journal_concurrency.h"
#include "../../../runtime/gc/gc_root_walk_heal_cases.h"
#include "../../../runtime/gc/gc_root_walk_item_cases.h"
/* C driver for Rust-emitted machine code. Tested atomic operations are emitted
 * instructions; C atomics are limited to fixture payloads and control gates. */
#include <assert.h>
#include <stdint.h>
#include <stdatomic.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
typedef HANDLE Thread;
#define THREAD_RESULT DWORD WINAPI
#else
#include <pthread.h>
#include <sys/mman.h>
#include <unistd.h>
#ifdef __APPLE__
#include <libkern/OSCacheControl.h>
#endif
typedef pthread_t Thread;
#define THREAD_RESULT void *
#endif
#include "gc_atomic_code.h"
#include "gc_abi.h"
#include "gc_context.h"
#if defined(GC_LINUX_WORKERS) || defined(_WIN32) || (defined(__APPLE__) && defined(__aarch64__))
#include "../../../runtime/gc/gc_native_root_prepare_cases.h"
#include "../../../runtime/gc/gc_native_builtin_prepare_cases.h"
#endif
#ifdef GC_LINUX_WORKERS
#include <signal.h>
#include <sys/resource.h>
#include <sys/wait.h>
#endif

typedef uint64_t (*AtomicFn)(void *, uint64_t, uint64_t);
typedef struct { void *memory; size_t size; AtomicFn call; } Code;

static Code load_code(const unsigned char *bytes, size_t size) {
#ifdef _WIN32
    void *memory = VirtualAlloc(NULL, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
    assert(memory);
#else
    void *memory = mmap(NULL, size, PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    assert(memory != MAP_FAILED);
#endif
    memcpy(memory, bytes, size);
#ifdef _WIN32
    DWORD previous;
    assert(VirtualProtect(memory, size, PAGE_EXECUTE_READ, &previous));
    assert(FlushInstructionCache(GetCurrentProcess(), memory, size));
#else
#ifdef __APPLE__
    sys_icache_invalidate(memory, size);
#else
    __builtin___clear_cache((char *)memory, (char *)memory + size);
#endif
    assert(mprotect(memory, size, PROT_READ | PROT_EXEC) == 0);
#endif
    /* Native instruction bytes implement the exact three-argument C ABI.
     * They are immutable RX memory until both threads have joined. */
    return (Code){memory, size, (AtomicFn)memory};
}

static void unload_code(Code code) {
#ifdef _WIN32
    assert(VirtualFree(code.memory, 0, MEM_RELEASE));
#else
    assert(munmap(code.memory, code.size) == 0);
#endif
}

/* Reusable two-thread latch; no sleep-based scheduling. */
static struct {
#ifdef _WIN32
    CRITICAL_SECTION mutex;
    CONDITION_VARIABLE condition;
#else
    pthread_mutex_t mutex;
    pthread_cond_t condition;
#endif
    unsigned arrivals, epoch;
} latch;

static void rendezvous(void) {
#ifdef _WIN32
    EnterCriticalSection(&latch.mutex);
#else
    assert(pthread_mutex_lock(&latch.mutex) == 0);
#endif
    unsigned epoch = latch.epoch;
    if (++latch.arrivals == 2) {
        latch.arrivals = 0;
        latch.epoch++;
#ifdef _WIN32
        WakeAllConditionVariable(&latch.condition);
#else
        assert(pthread_cond_broadcast(&latch.condition) == 0);
#endif
    } else {
        while (latch.epoch == epoch) {
#ifdef _WIN32
            assert(SleepConditionVariableCS(&latch.condition, &latch.mutex, 30000));
#else
            assert(pthread_cond_wait(&latch.condition, &latch.mutex) == 0);
#endif
        }
    }
#ifdef _WIN32
    LeaveCriticalSection(&latch.mutex);
#else
    assert(pthread_mutex_unlock(&latch.mutex) == 0);
#endif
}

#define ROUNDS 4096
static _Atomic uint64_t shared;
static _Atomic uint64_t payload[2];
static uint64_t observations[2][ROUNDS];
static Code load, store, cas, fetch_or, fence;
static Code load32, store32, cas32;
static MutatorContext context32 = {.abi_version = GC_ABI_VERSION,
                                  .thread_id = UINT64_C(0x1234567887654321)};

typedef struct { unsigned actor, mode; } Job;
static THREAD_RESULT actor(void *argument) {
    const Job *job = argument;
    int narrow = job->mode >= 3;
    void *word = narrow ? (void *)&context32.state : (void *)&shared;
    AtomicFn release = narrow ? store32.call : store.call;
    AtomicFn acquire = narrow ? load32.call : load.call;
    for (unsigned round = 0; round < ROUNDS; round++) {
        if (job->actor == 0) release(word, 0, 0);
        rendezvous();
        if (job->mode == 0) {
            observations[job->actor][round] = cas.call(&shared, 0, job->actor + 1);
        } else if (job->mode == 1) {
            observations[job->actor][round] = fetch_or.call(&shared, UINT64_C(1) << job->actor, 0);
        } else if (job->mode == 3) {
            observations[job->actor][round] = cas32.call(word, UINT64_C(0xabcdef0100000000),
                UINT64_C(0xfacebeef00000000) | (job->actor + 1));
        } else if (job->actor == 0) {
            uint64_t value = round + UINT64_C(0x123400000000);
            atomic_store_explicit(&payload[0], value, memory_order_relaxed);
            atomic_store_explicit(&payload[1], ~value, memory_order_relaxed);
            release(word, round + 1, 0); // release publishes relaxed payload
        } else {
            uint64_t sequence;
            do { sequence = acquire(word, 0, 0); } while (sequence == 0);
            assert(sequence == round + 1);
            uint64_t expected = round + UINT64_C(0x123400000000);
            assert(atomic_load_explicit(&payload[0], memory_order_relaxed) == expected);
            assert(atomic_load_explicit(&payload[1], memory_order_relaxed) == ~expected);
        }
        rendezvous(); // both operations completed before resetting the next trial
    }
    return 0;
}

static void run_pair(unsigned mode) {
    Thread threads[2];
    Job jobs[2] = {{0, mode}, {1, mode}};
    for (unsigned i = 0; i < 2; i++) {
#ifdef _WIN32
        threads[i] = CreateThread(NULL, 0, actor, &jobs[i], 0, NULL);
        assert(threads[i]);
#else
        assert(pthread_create(&threads[i], NULL, actor, &jobs[i]) == 0);
#endif
    }
    for (unsigned i = 0; i < 2; i++) {
#ifdef _WIN32
        assert(WaitForSingleObject(threads[i], 30000) == WAIT_OBJECT_0);
        assert(CloseHandle(threads[i]));
#else
        assert(pthread_join(threads[i], NULL) == 0);
#endif
    }
    for (unsigned round = 0; round < ROUNDS && (mode < 2 || mode == 3); round++) {
        uint64_t a = observations[0][round], b = observations[1][round];
        assert((a == 0 && b == 1) || (a == 2 && b == 0));
    }
    if (mode == 1) assert(load.call(&shared, 0, 0) == 3);
    assert(context32.abi_version == GC_ABI_VERSION);
    assert(context32.thread_id == UINT64_C(0x1234567887654321));
}

static void verify_u32_width(_Atomic uint32_t *word) {
    const uint32_t values[] = {0, 1, UINT32_MAX, UINT32_C(0x80000000), UINT32_C(0x13579ace)};
    for (unsigned i = 0; i < sizeof(values) / sizeof(values[0]); i++) {
        uint32_t value = values[i], next = ~value;
        store32.call(word, UINT64_C(0x1234567800000000) | value, 0);
        assert(load32.call(word, 0, 0) == value);
        assert(cas32.call(word, UINT64_C(0xffffffff00000000) | (value ^ 1u), next) == value);
        assert(load32.call(word, 0, 0) == value);
        assert(cas32.call(word, UINT64_C(0xffffffff00000000) | value,
                          UINT64_C(0x8765432100000000) | next) == value);
        assert(load32.call(word, 0, 0) == next);
    }
}

static void context_transitions(void) {
    Code transition = load_code(gc_code_context_transition, sizeof(gc_code_context_transition));
    const uint64_t values[] = {0, 1, 2, 3, 4, 5, UINT64_MAX, UINT64_C(1) << 32};
    const unsigned edges[] = {0x0f, 0x03, 0x01, 0x10, 0};
    unsigned cases = 0;
    for (unsigned current = 0; current < 6; current++) {
        for (unsigned e = 0; e < 8; e++) for (unsigned d = 0; d < 8; d++) {
            for (unsigned abi = 0; abi < 2; abi++) for (unsigned access = 0; access < 2; access++) {
                MutatorContext actual, reference, untouched;
                // Isolated unpublished storage: poison unrelated fields/padding.
                memset(&actual, 0xa5, sizeof(actual));
                actual.abi_version = abi?GC_ABI_VERSION:GC_ABI_VERSION-1;
                atomic_init(&actual.state, current);
                atomic_init(&actual.access_epoch, access);
                atomic_init(&actual.private_producers, 0);
                memcpy(&reference, &actual, sizeof(actual));
                memcpy(&untouched, &actual, sizeof(actual));
                uint64_t expected = values[e], desired = values[d];
                int permitted = expected < 5 && desired < 5 && (edges[expected] & (1u << desired));
                uint64_t result = actual.abi_version != GC_ABI_VERSION ? GC_CONTEXT_WRONG_ABI :
                    expected != current ? GC_CONTEXT_WRONG_STATE :
                    access ? GC_CONTEXT_ACTIVE_ACCESS :
                    !permitted ? GC_CONTEXT_INVALID_EDGE : GC_CONTEXT_OK;
                assert(gc_context_transition_locked(&reference, expected, desired) == result);
                assert(transition.call(&actual, expected, desired) == result);
                if (result == GC_CONTEXT_OK) atomic_store(&untouched.state, (uint32_t)desired);
                assert(memcmp(&actual, &untouched, sizeof(actual)) == 0);
                assert(memcmp(&reference, &untouched, sizeof(actual)) == 0);
                cases++;
            }
        }
    }
    assert(cases == 1536);
    for(unsigned desired=0;desired<5;desired++){
        MutatorContext actual={.abi_version=GC_ABI_VERSION,.private_producers=1};
        MutatorContext before=actual,reference=actual;
        assert(gc_context_transition_locked(&reference,GC_RUNNING,desired)==GC_CONTEXT_PRIVATE_WORK);
        assert(transition.call(&actual,GC_RUNNING,desired)==GC_CONTEXT_PRIVATE_WORK);
        assert(!memcmp(&actual,&before,sizeof(actual))&&!memcmp(&reference,&before,sizeof(actual)));
    }

    unload_code(transition);
    puts("1536 C/emitted shared-context transition and preservation cases PASS");
}

static void exact_width(void) {
    assert(((uintptr_t)&context32.state & 7) == 4);
    verify_u32_width(&context32.state);
    assert(context32.abi_version == GC_ABI_VERSION);
    assert(context32.thread_id == UINT64_C(0x1234567887654321));
    // A 64-bit load masked down to 32 bits still violates the access contract.
    // Put the tested word immediately before an inaccessible page to catch it.
#ifdef _WIN32
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    size_t page = info.dwPageSize;
    unsigned char *memory = VirtualAlloc(NULL, page * 2, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
    assert(memory);
    DWORD old;
    assert(VirtualProtect(memory + page, page, PAGE_NOACCESS, &old));
#else
    size_t page = (size_t)sysconf(_SC_PAGESIZE);
    unsigned char *memory = mmap(NULL, page * 2, PROT_READ | PROT_WRITE,
                                MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    assert(memory != MAP_FAILED);
    assert(mprotect(memory + page, page, PROT_NONE) == 0);
#endif
    verify_u32_width((_Atomic uint32_t *)(memory + page - 4));
#ifdef _WIN32
    assert(VirtualFree(memory, 0, MEM_RELEASE));
#else
    assert(munmap(memory, page * 2) == 0);
#endif
}

#ifdef GC_LINUX_WORKERS
typedef int64_t (*StartFn)(void *, void *, void *, void *, _Atomic uint32_t *);
typedef int64_t (*JoinFn)(_Atomic uint32_t *);
typedef int64_t (*StackAllocFn)(uint64_t);
typedef int64_t (*WaitFn)(_Atomic uint32_t *, uint32_t);
typedef struct { int64_t value; uint64_t error; } AbiResult;
typedef AbiResult (*AbiFn)(void *, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
static AbiFn abi_call;
static int64_t checked_call(void *fn, uint64_t a, uint64_t b, uint64_t c, uint64_t d, uint64_t e) {
    AbiResult result = abi_call(fn, a, b, c, d, e);
    assert(result.error == 0);
    return result.value;
}
typedef struct {
    _Atomic uint64_t *counter;
    _Atomic uint64_t tls_self, tls_context;
    uint64_t bits;
    _Atomic uint64_t observed;
    _Atomic uint32_t *start;
    _Atomic uint64_t entry_sp;
} WorkerContext;
_Static_assert(sizeof(WorkerContext) == 56, "emitted worker fixture layout");
_Static_assert(offsetof(WorkerContext, start) == 40, "worker gate offset");
_Static_assert(offsetof(WorkerContext, entry_sp) == 48, "worker entry stack offset");
static _Thread_local volatile uint64_t parent_tls_marker;

static void assert_guard(void *address) {
    pid_t child = fork();
    assert(child >= 0);
    if (child == 0) {
        struct rlimit limit = {0, 0};
        if (setrlimit(RLIMIT_CORE, &limit) != 0) _exit(2);
        signal(SIGSEGV, SIG_DFL);
        *(volatile unsigned char *)address = 1;
        _exit(3);
    }
    int status;
    assert(waitpid(child, &status, 0) == child);
    assert(WIFSIGNALED(status) && WTERMSIG(status) == SIGSEGV);
}

static uint64_t inaccessible_bytes(void) {
    FILE *maps = fopen("/proc/self/maps", "r");
    assert(maps);
    char line[1024], perms[5];
    uint64_t sum = 0;
    unsigned long low, high;
    while (fgets(line, sizeof(line), maps)) {
        if (sscanf(line, "%lx-%lx %4s", &low, &high, perms) == 3 &&
            memcmp(perms, "---", 3) == 0) sum += high - low;
    }
    assert(!ferror(maps));
    assert(fclose(maps) == 0);
    return sum;
}

static void run_native_workers(void) {
    Code start = load_code(gc_code_worker_start, sizeof(gc_code_worker_start));
    Code join = load_code(gc_code_worker_join, sizeof(gc_code_worker_join));
    Code entry = load_code(gc_code_worker_entry, sizeof(gc_code_worker_entry));
    Code fail = load_code(gc_code_worker_fail, sizeof(gc_code_worker_fail));
    Code alloc = load_code(gc_code_stack_alloc, sizeof(gc_code_stack_alloc));
    Code release = load_code(gc_code_stack_free, sizeof(gc_code_stack_free));
    Code map_fail = load_code(gc_code_stack_map_fail, sizeof(gc_code_stack_map_fail));
    Code protect_fail = load_code(gc_code_stack_protect_fail, sizeof(gc_code_stack_protect_fail));
    Code park = load_code(gc_code_worker_park, sizeof(gc_code_worker_park));
    Code wake = load_code(gc_code_worker_wake, sizeof(gc_code_worker_wake));
    Code abi = load_code(gc_code_abi_call, sizeof(gc_code_abi_call));
    abi_call = (AbiFn)abi.memory;
    StartFn fail_call = (StartFn)fail.memory;
    JoinFn join_call = (JoinFn)join.memory;
    StackAllocFn alloc_call = (StackAllocFn)alloc.memory;
    WaitFn wake_call = (WaitFn)wake.memory;
    const size_t page = (size_t)sysconf(_SC_PAGESIZE);
    assert(page == 4096); // Linux x86-64 base page contract
    assert(alloc_call(0) == -22);
    assert(alloc_call(4095) == -22);
    assert(alloc_call(UINT64_MAX - 4095) == -75);
    assert(alloc_call(UINT64_C(0x7fffffffffffd000)) == -12); // mmap ENOMEM, no wrap
    const size_t stack_bytes = 32 * page, mapping_bytes = stack_bytes + 2 * page;
    uint64_t inaccessible_before = inaccessible_bytes();
    for (unsigned attempt = 0; attempt < 8; attempt++) {
        assert(((StackAllocFn)map_fail.memory)(stack_bytes) == -9);
        assert(((StackAllocFn)protect_fail.memory)(stack_bytes) == -22);
    }
    assert(inaccessible_bytes() == inaccessible_before); // failed mprotect unmapped its reservation
    void *stacks[2];
    struct { _Atomic uint32_t tid; uint32_t poison; } tids[2];
    _Alignas(16) uint64_t tls[2][2];
    _Atomic uint64_t counter = 0;
    _Atomic uint32_t gate = 0;
    WorkerContext contexts[2];
    parent_tls_marker = UINT64_C(0x123456789abcdef0);
    for (unsigned i = 0; i < 2; i++) {
        int64_t mapping = checked_call(alloc.memory, stack_bytes, 0, 0, 0, 0);
        assert(mapping > 0);
        stacks[i] = (void *)(uintptr_t)mapping;
        assert_guard(stacks[i]);
        assert_guard((char *)stacks[i] + page + stack_bytes);
        atomic_init(&tids[i].tid, 0);
        tids[i].poison = 0xdeadbeef; // join must load exactly the 32-bit kernel word
        contexts[i] = (WorkerContext){&counter, 0, 0, UINT64_C(1) << i, 99, &gate, 0};
        tls[i][0] = (uintptr_t)&tls[i];
        tls[i][1] = (uintptr_t)&contexts[i];
    }
    for (unsigned i = 0; i < 2; i++) {
        void *stack_end = (char *)stacks[i] + page + stack_bytes;
        assert(fail_call(stack_end, tls[i], entry.memory, &contexts[i], &tids[i].tid) == -22);
        assert(atomic_load_explicit(&tids[i].tid, memory_order_relaxed) == 0);
        int64_t id = checked_call(start.memory, (uintptr_t)stack_end, (uintptr_t)tls[i],
                                 (uintptr_t)entry.memory, (uintptr_t)&contexts[i], (uintptr_t)&tids[i].tid);
        assert(id > 0);
        assert(atomic_load_explicit(&tids[i].tid, memory_order_relaxed) == (uint64_t)id);
    }
    // Observe an actual blocked waiter using the kernel's wake count. Wake
    // without publishing first exercises predicate rechecking after a wake.
    int64_t woken;
    do { woken = wake_call(&gate, 1); assert(woken >= 0); } while (woken == 0);
    assert(woken == 1);
    atomic_store_explicit(&gate, 1, memory_order_release);
    assert(checked_call(wake.memory, (uintptr_t)&gate, INT32_MAX, 0, 0, 0) >= 0);
    assert(checked_call(park.memory, (uintptr_t)&gate, 0, 0, 0, 0) == -11); // publication before park cannot lose wakeup
    for (unsigned i = 0; i < 2; i++) {
        assert(checked_call(join.memory, (uintptr_t)&tids[i].tid, 0, 0, 0, 0) == 0);
        assert(join_call(&tids[i].tid) == 0); // already terminated
        assert(atomic_load_explicit(&tids[i].tid, memory_order_relaxed) == 0);
        assert(tids[i].poison == 0xdeadbeef);
        assert(load.call(&contexts[i].tls_self, 0, 0) == (uintptr_t)&tls[i]);
        assert(load.call(&contexts[i].tls_context, 0, 0) == (uintptr_t)&contexts[i]);
        uint64_t sp = load.call(&contexts[i].entry_sp, 0, 0);
        assert((sp & 15) == 8); // SysV entry after bootstrap CALL pushed its return address
        assert(sp >= (uintptr_t)stacks[i] + page && sp < (uintptr_t)stacks[i] + page + stack_bytes);
        assert(checked_call(release.memory, (uintptr_t)stacks[i], mapping_bytes, 0, 0, 0) == 0);
    }
    assert(load.call(&counter, 0, 0) == 3);
    uint64_t a = load.call(&contexts[0].observed, 0, 0);
    uint64_t b = load.call(&contexts[1].observed, 0, 0);
    assert((a == 0 && b == 1) || (a == 2 && b == 0));
    assert(parent_tls_marker == UINT64_C(0x123456789abcdef0));
    unload_code(start); unload_code(join); unload_code(entry); unload_code(fail);
    unload_code(alloc); unload_code(release); unload_code(park); unload_code(wake);
    unload_code(map_fail); unload_code(protect_fail);
    unload_code(abi);
    puts("emitted Linux clone/TLS/futex join and startup failure PASS");
}
#endif

#include "handshake_cases.h"
#include "linux_handshake_cases.h"
#include "gc_heap_cases.h"
#include "gc_forward_table_concurrency.h"
#include "gc_resolve_chain.h"
#include "gc_relocate_target_cases.h"
#include "gc_relocate_pipeline.h"
#include "gc_page_release_cases.h"
#include "gc_page_release_pipeline.h"
#include "gc_barrier_cases.h"
#include "gc_barrier_pipeline.h"
#include "gc_barrier_race.h"
#include "gc_logical_cas_order.h"
#include "gc_barrier_load_cases.h"
#include "gc_barrier_heal_cases.h"
#include "gc_barrier_load_pipeline.h"
#include "gc_relocate_cases.h"
#include "gc_pages_cases.h"
#include "gc_descriptor_concurrency.h"
#include "gc_roots_concurrency.h"
#include "gc_root_lifetime_concurrency.h"
#include "gc_root_walk_concurrency.h"
#include "gc_root_detach_concurrency.h"
#include "gc_detach_cases.h"
#include "gc_buffers_cases.h"
#include "gc_buffers_concurrency.h"
#include "gc_metadata_concurrency.h"
#include "gc_mark_cases.h"
#include "gc_mark_scan_commit_cases.h"
#include "gc_mark_scan_cases.h"
#include "gc_source_drain_cases.h"
#include "gc_source_uncommit_cases.h"
#include "gc_pin_page_cases.h"
#include "gc_pin_registry_cases.h"
#include "gc_pin_source_release_cases.h"
#include "gc_root_pin_cases.h"
#include "gc_native_pin_cases.h"
/* Dispatch only selects a generated entry; authentication stays in emitted code. */
static uint64_t (*pin_registry_actions[4])(PinPageRegistry *,const MetadataLeaseRequest *,RelocationGateRequest *);
static uint64_t emitted_pin_registry_step(PinPageRegistry *r,const MetadataLeaseRequest *lease,
                                         RelocationGateRequest *q,uint64_t op){
    assert(op<4);return pin_registry_actions[op](r,lease,q);
}

#include "gc_source_retire_cases.h"
#include "gc_source_retire_pipeline.h"
#include "gc_forward_incoming_cases.h"
#include "gc_mark_concurrency.h"
#include "gc_root_access_cases.h"
#include "gc_survivor_age_cases.h"
#include "gc_promotion_remember_cases.h"
#include "gc_relocation_turn_cases.h"
#include "gc_forward_compress_cases.h"
#include "gc_heap_incoming_cases.h"
#include "gc_heap_walk_cases.h"
#include "gc_object_walk_cases.h"
#include "gc_heap_audit_slice_cases.h"
#include "gc_heap_audit_cases.h"
#include "gc_root_access_cleanup_race.h"
#include "gc_cycle_cases.h"
#include "gc_remembered_cases.h"
#include "gc_remembered_inventory_cases.h"
#include "gc_remembered_transfer_cases.h"
#include "gc_remembered_owner_cases.h"
#include "gc_remembered_owner_race.h"
#include "gc_remembered_load_cases.h"
#include "gc_remembered_pipeline.h"
#include "gc_remembered_walk_cases.h"
#include "gc_mark_route_cases.h"
#include "gc_mark_move_pipeline.h"
#include "gc_mark_spill_cases.h"
#include "gc_forward_concurrency.h"
#include "gc_pages_pipeline.h"

#include "../../../runtime/gc/gc_weak_release_cases.h"
#include "../../../runtime/gc/gc_weak_table_cases.h"
#include "../../../runtime/gc/gc_weak_table_access_cases.h"
#include "../../../runtime/gc/gc_native_weak_cases.h"
#include "../../../runtime/gc/gc_weak_mark_cases.h"
#include "../../../runtime/gc/gc_weak_liveness_cases.h"
#include "../../../runtime/gc/gc_weak_process_cases.h"
#include "../../../runtime/gc/gc_weak_scan_cases.h"
#include "../../../runtime/gc/gc_weak_scan_concurrency.h"
#include "../../../runtime/gc/gc_weak_cycle_cases.h"
#include "../../../runtime/gc/gc_weak_incoming_cases.h"
#include "../../../runtime/gc/gc_weak_follow_cases.h"
int main(void) {
    atomic_stage("main",0);
    /* Keep the last completed fixture visible if emitted code faults. */
    assert(setvbuf(stdout,NULL,_IONBF,0)==0);
#ifdef _WIN32
    InitializeCriticalSection(&latch.mutex);
    InitializeConditionVariable(&latch.condition);
#else
    alarm(KLASSIC_CONFORMANCE_TIMEOUT_SECONDS); // bounded whole-suite budget
    assert(pthread_mutex_init(&latch.mutex, NULL) == 0);
    assert(pthread_cond_init(&latch.condition, NULL) == 0);
#endif
    load = load_code(gc_code_load, sizeof(gc_code_load));
    store = load_code(gc_code_store, sizeof(gc_code_store));
    cas = load_code(gc_code_cas, sizeof(gc_code_cas));
    fetch_or = load_code(gc_code_fetch_or, sizeof(gc_code_fetch_or));
    fence = load_code(gc_code_fence, sizeof(gc_code_fence));
    load32 = load_code(gc_code_load32, sizeof(gc_code_load32));
    store32 = load_code(gc_code_store32, sizeof(gc_code_store32));
    cas32 = load_code(gc_code_cas32, sizeof(gc_code_cas32));
    exact_width();
    context_transitions();
    Code buffer_step=load_code(gc_code_buffer_step,sizeof(gc_code_buffer_step));
    Code buffer_drain=load_code(gc_code_buffer_drain,sizeof(gc_code_buffer_drain));
    gc_run_buffer_cases((BarrierBufferStep)buffer_step.memory,(BarrierBufferDrain)buffer_drain.memory);
    gc_run_buffer_races((BarrierBufferStep)buffer_step.memory,(BarrierBufferDrain)buffer_drain.memory);
    Code life = load_code(gc_code_root_life, sizeof(gc_code_root_life));
    Code clear = load_code(gc_code_root_clear, sizeof(gc_code_root_clear));
    Code leased = load_code(gc_code_root_scan_leased, sizeof(gc_code_root_scan_leased));
    Code detach = load_code(gc_code_handshake_step, sizeof(gc_code_handshake_step));
    Code detach_drain = load_code(gc_code_root_drain, sizeof(gc_code_root_drain));
    Code detach_progress = load_code(gc_code_detach_step, sizeof(gc_code_detach_step));
    gc_run_detach_cases((DetachProgress)detach_progress.memory);
    gc_run_root_detach_races((RootLife)life.memory, (RootClear)clear.memory, (RootLeasedScan)leased.memory, (RootDetachHandshake)detach.memory, (RootDrainer)detach_drain.memory, (DetachProgress)detach_progress.memory, (BarrierBufferStep)buffer_step.memory, (BarrierBufferDrain)buffer_drain.memory);
    unload_code(detach_progress);
    unload_code(detach_drain);
    gc_run_root_lifetime_cases((RootLife)life.memory, (RootClear)clear.memory, (RootLeasedScan)leased.memory);
    gc_run_root_lifetime_races((RootLife)life.memory, (RootClear)clear.memory, (RootLeasedScan)leased.memory);
    Code walk = load_code(gc_code_root_walk, sizeof(gc_code_root_walk));
    Code walk_item = load_code(gc_code_root_walk_item, sizeof(gc_code_root_walk_item));
    Code walk_heal = load_code(gc_code_root_walk_heal, sizeof(gc_code_root_walk_heal));
    gc_run_root_walk_heal_cases((RootWalkHealer)walk_heal.memory);
    Code walk_scan = load_code(gc_code_root_walk_scan, sizeof(gc_code_root_walk_scan));
    gc_run_root_walk_item_cases((RootItemStep)walk_item.memory,(RootWalker)walk.memory,(RootWalkScanner)walk_scan.memory);
    gc_run_root_walk_heal_races((RootWalkHealer)walk_heal.memory,(RootWalker)walk.memory,(RootWalkScanner)walk_scan.memory,(RootLife)life.memory);
    gc_run_root_walk_cases((RootWalker)walk.memory, (RootWalkScanner)walk_scan.memory, (RootLife)life.memory, (RootClear)clear.memory, (RootLeasedScan)leased.memory);
    gc_run_root_walk_races((RootWalker)walk.memory, (RootWalkScanner)walk_scan.memory, (RootLife)life.memory, (RootClear)clear.memory, (RootLeasedScan)leased.memory);
    unload_code(walk);
    Code roots = load_code(gc_code_root_step, sizeof(gc_code_root_step));
    Code root_scan = load_code(gc_code_root_scan, sizeof(gc_code_root_scan));
    Code root_drain = load_code(gc_code_root_drain, sizeof(gc_code_root_drain));
    gc_run_root_cases((RootStep)roots.memory, (RootScanner)root_scan.memory, (RootDrainer)root_drain.memory);
    gc_run_root_races((RootStep)roots.memory, (RootScanner)root_scan.memory, (RootDrainer)root_drain.memory);
    Code descriptor = load_code(gc_code_descriptor_register, sizeof(gc_code_descriptor_register));
    Code scanner = load_code(gc_code_descriptor_scan, sizeof(gc_code_descriptor_scan));
    gc_run_descriptor_cases((DescriptorRegister)descriptor.memory, (DescriptorScanner)scanner.memory);
    Code root_copy=load_code(gc_code_root_copy,sizeof(gc_code_root_copy));
    gc_run_root_copy_cases((RootCopier)root_copy.memory);
    Code root_cancel=load_code(gc_code_root_cancel_empty,sizeof(gc_code_root_cancel_empty));
    gc_run_root_cancel_cases((RootCancelEmpty)root_cancel.memory);
    Code allocation_shape=load_code(gc_code_allocation_shape,sizeof(gc_code_allocation_shape));
    gc_run_allocation_shape_cases((AllocationShaper)allocation_shape.memory);
    Code root_storage_can_dispose=load_code(gc_code_root_storage_can_dispose,sizeof(gc_code_root_storage_can_dispose));
    Code root_storage_prepare=load_code(gc_code_root_storage_prepare,sizeof(gc_code_root_storage_prepare));
    gc_run_root_storage_cases((RootStoragePreparer)root_storage_prepare.memory);
    gc_run_root_storage_disposal_cases((RootStorageDisposalCheck)root_storage_can_dispose.memory);
    gc_run_root_disposal_race((RootStorageDisposalCheck)root_storage_can_dispose.memory);
    Code builtin_prepare=load_code(gc_code_builtin_prepare,sizeof(gc_code_builtin_prepare));
    gc_run_builtin_descriptor_cases((DescriptorRegister)descriptor.memory,(DescriptorScanner)scanner.memory,(BuiltinPreparer)builtin_prepare.memory);
    gc_run_descriptor_races((DescriptorRegister)descriptor.memory, (DescriptorScanner)scanner.memory);
    run_handshake_cases();
    Code allocate = load_code(gc_code_tlab_allocate, sizeof(gc_code_tlab_allocate));
    gc_run_tlab_cases((TlabAllocator)allocate.memory);
    Code guarded_heal=load_code(gc_code_guarded_heal,sizeof(gc_code_guarded_heal));
    gc_run_guarded_heal_cases((GuardedHealer)guarded_heal.memory);
    Code observe=load_code(gc_code_guarded_observe,sizeof(gc_code_guarded_observe));
    gc_run_observe_cases((GuardedObserver)observe.memory);
    Code guarded_load=load_code(gc_code_guarded_load,sizeof(gc_code_guarded_load));
    gc_run_guarded_load_cases((GuardedLoader)guarded_load.memory);
    Code remembered_load=load_code(gc_code_remembered_load,sizeof(gc_code_remembered_load));
    gc_run_remembered_load_cases((RememberedLoader)remembered_load.memory);
    Code block_size=load_code(gc_code_guarded_block_size,sizeof(gc_code_guarded_block_size));
    gc_run_block_size_cases((GuardedBlockSize)block_size.memory);
    Code identity=load_code(gc_code_guarded_identity,sizeof(gc_code_guarded_identity));
    gc_run_identity_cases((GuardedIdentity)identity.memory);
    Code logical_cas=load_code(gc_code_guarded_logical_cas,sizeof(gc_code_guarded_logical_cas));
    gc_run_logical_cas_cases((GuardedLogicalCas)logical_cas.memory);
    Code guarded_store=load_code(gc_code_guarded_store,sizeof(gc_code_guarded_store));
    gc_run_guarded_store_cases((GuardedStorer)guarded_store.memory);
    Code page_release=load_code(gc_code_page_release,sizeof(gc_code_page_release));
    gc_run_page_release_cases((PageReleaser)page_release.memory);
    Code pool = load_code(gc_code_pool_step, sizeof(gc_code_pool_step));
    gc_run_pool_cases((PoolStep)pool.memory);
    Code forward_lookup=load_code(gc_code_forward_lookup,sizeof(gc_code_forward_lookup));
    gc_run_forward_lookup_cases((ForwardFinder)forward_lookup.memory);
    Code resolve=load_code(gc_code_resolve_access,sizeof(gc_code_resolve_access));
    gc_run_resolve_cases((Resolver)resolve.memory);
    Code relocate_target=load_code(gc_code_relocate_target,sizeof(gc_code_relocate_target));
    gc_run_relocate_target_cases((RelocateTarget)relocate_target.memory);
    Code relocate_copy=load_code(gc_code_relocate_copy,sizeof(gc_code_relocate_copy));
    gc_run_relocate_cases((RelocateCopy)relocate_copy.memory);
    Code relocate_publish=load_code(gc_code_relocate_publish,sizeof(gc_code_relocate_publish));
    gc_run_relocate_publish_cases((RelocateCopy)relocate_copy.memory,(RelocatePublish)relocate_publish.memory);
    Code forward=load_code(gc_code_forward_step,sizeof(gc_code_forward_step));
    gc_run_forward_cases((ForwardStep)forward.memory);
    Code source_access=load_code(gc_code_source_access,sizeof(gc_code_source_access));
    gc_run_source_access_cases((SourceAccess)source_access.memory);
    gc_run_guarded_store_races((GuardedStorer)guarded_store.memory,(SourceAccess)source_access.memory);
    gc_run_logical_cas_order((GuardedLogicalCas)logical_cas.memory,(SourceAccess)source_access.memory);
    Code remembered_step=load_code(gc_code_remembered_step,sizeof(gc_code_remembered_step));
    Code remembered_inventory=load_code(gc_code_remembered_inventory,sizeof(gc_code_remembered_inventory));
    gc_run_guarded_store_moves((GuardedStorer)guarded_store.memory,(GuardedHealer)guarded_heal.memory,(SourceAccess)source_access.memory,(ForwardStep)forward.memory,(RelocateCopy)relocate_copy.memory,(RelocatePublish)relocate_publish.memory,(RelocateTarget)relocate_target.memory,(RememberedInventoryStep)remembered_inventory.memory);
    gc_run_forward_table_races((TlabAllocator)allocate.memory,(ForwardFinder)forward_lookup.memory,(SourceAccess)source_access.memory);
    Code guarded_scalar=load_code(gc_code_guarded_scalar,sizeof(gc_code_guarded_scalar));
    gc_run_scalar_cases((ScalarAccessor)guarded_scalar.memory);
    gc_run_forward_races((ForwardStep)forward.memory,(SourceAccess)source_access.memory,(Resolver)resolve.memory,(RelocateCopy)relocate_copy.memory,(RelocatePublish)relocate_publish.memory,(ScalarAccessor)guarded_scalar.memory);


    Code mark=load_code(gc_code_mark_step,sizeof(gc_code_mark_step));
    gc_run_mark_cases((MarkStep)mark.memory);
    Code mark_spill=load_code(gc_code_mark_spill,sizeof(gc_code_mark_spill));
    gc_run_mark_spill_cases((MarkStep)mark.memory,(MarkSpill)mark_spill.memory);
    Code mark_scan_commit=load_code(gc_code_mark_scan_commit,sizeof(gc_code_mark_scan_commit));
    gc_run_mark_scan_commit_cases((MarkScanCommitter)mark_scan_commit.memory);
    Code mark_scan=load_code(gc_code_mark_scan,sizeof(gc_code_mark_scan));
    gc_run_mark_scan_cases((MarkScanner)mark_scan.memory);
    Code source_drain=load_code(gc_code_source_drain,sizeof(gc_code_source_drain));
    gc_run_source_drain_cases((SourceDrainer)source_drain.memory);
    Code source_uncommit=load_code(gc_code_source_uncommit,sizeof(gc_code_source_uncommit));
    gc_run_source_uncommit_cases((SourceUncommitter)source_uncommit.memory);
    Code source_retire=load_code(gc_code_source_retire,sizeof(gc_code_source_retire));
    gc_run_source_retire_cases((SourceRetirer)source_retire.memory);
    Code pin_dispatch=load_code(gc_code_pin_dispatch,sizeof(gc_code_pin_dispatch));
    gc_run_pin_dispatch_cases((PinDispatcher)pin_dispatch.memory);
    Code native_pin_publish=load_code(gc_code_native_pin_publish,sizeof(gc_code_native_pin_publish));
    Code native_pin_prepare=load_code(gc_code_native_pin_prepare,sizeof(gc_code_native_pin_prepare));
    Code pin_worker_step=load_code(gc_code_worker_step,sizeof(gc_code_worker_step));
    Code finish_notify=load_code(gc_code_root_finish_notify,sizeof(gc_code_root_finish_notify));
    Code pin_wait=load_code(gc_code_native_pin_wait,sizeof(gc_code_native_pin_wait));
#ifdef GC_LINUX_WORKERS
    Code pin_notify=load_code(gc_code_registry_notify,sizeof(gc_code_registry_notify));
    Code pin_park=load_code(gc_code_worker_park,sizeof(gc_code_worker_park));
    gc_run_native_pin_wait_cases((NativePinWaitArm)pin_wait.memory,(PinEventNotify)pin_notify.memory,(PinEventPark)pin_park.memory,(RootFinishNotifier)finish_notify.memory);
    unload_code(pin_notify);unload_code(pin_park);
#else
    gc_run_native_pin_wait_cases((NativePinWaitArm)pin_wait.memory,gc_pin_event_notify,gc_pin_event_park,(RootFinishNotifier)finish_notify.memory);
#endif
    unload_code(pin_wait);unload_code(finish_notify);
    gc_run_native_heap_cases();
    gc_run_native_runtime_cases();
    gc_run_native_bitmap_cases();
    Code native_pin_progress=load_code(gc_code_native_pin_progress,sizeof(gc_code_native_pin_progress));
    Code native_pin_close=load_code(gc_code_native_pin_close,sizeof(gc_code_native_pin_close));
    Code native_pin_release=load_code(gc_code_native_pin_release,sizeof(gc_code_native_pin_release));
    Code native_pin_dispose=load_code(gc_code_native_pin_dispose,sizeof(gc_code_native_pin_dispose));
    Code native_pin_retire=load_code(gc_code_native_pin_retire,sizeof(gc_code_native_pin_retire));
    gc_run_native_pin_cases((PinDispatcher)pin_dispatch.memory,(NativePinOwnerOps){
        (uint64_t (*)(NativePin *,const NativePinStart *))native_pin_publish.memory,
        (uint64_t (*)(NativePin *,PinPageRegistry *,const RootRequest *))native_pin_prepare.memory,
        (uint64_t (*)(NativePin *))native_pin_retire.memory,
        (uint64_t (*)(NativePin *,const NativeMetadataOps *))native_pin_dispose.memory,
        (uint64_t (*)(NativePin *,HeapPool *))native_pin_release.memory,
        (uint64_t (*)(const NativePinRuntime *,NativePin *))native_pin_close.memory,
        (uint64_t (*)(RootRegistry *,WorkerRequest *,uint64_t))pin_worker_step.memory,
        (uint64_t (*)(const NativePinRuntime *,NativePin *,WorkerRequest *))native_pin_progress.memory});
    unload_code(native_pin_progress);
    unload_code(native_pin_close);unload_code(pin_worker_step);
    unload_code(native_pin_dispose);unload_code(native_pin_release);
    unload_code(native_pin_publish);unload_code(native_pin_prepare);unload_code(native_pin_retire);
    unload_code(pin_dispatch);
    Code root_pin_prepare=load_code(gc_code_root_pin_prepare,sizeof(gc_code_root_pin_prepare));
    Code root_pin_enter=load_code(gc_code_root_pin_enter,sizeof(gc_code_root_pin_enter));
    Code root_pin_exit=load_code(gc_code_root_pin_exit,sizeof(gc_code_root_pin_exit));
    Code root_pin_cleanup=load_code(gc_code_root_pin_cleanup,sizeof(gc_code_root_pin_cleanup));
    Code root_pin_cancel=load_code(gc_code_root_pin_cancel,sizeof(gc_code_root_pin_cancel));
    Code root_pin_finish=load_code(gc_code_root_pin_finish,sizeof(gc_code_root_pin_finish));
    gc_run_root_pin_cases((RootPinOps){
        (uint64_t (*)(RootPin *,const RootPinStart *))root_pin_prepare.memory,
        (uint64_t (*)(RootPin *))root_pin_enter.memory,
        (uint64_t (*)(RootPin *))root_pin_exit.memory,
        (uint64_t (*)(RootPin *))root_pin_cleanup.memory,
        (uint64_t (*)(RootPin *))root_pin_cancel.memory,
        (uint64_t (*)(RootPin *))root_pin_finish.memory});
    unload_code(root_pin_prepare);
    unload_code(root_pin_enter);
    unload_code(root_pin_exit);
    unload_code(root_pin_cleanup);
    unload_code(root_pin_cancel);
    unload_code(root_pin_finish);
    Code pin_source_drain=load_code(gc_code_pin_source_drain,sizeof(gc_code_pin_source_drain));
    Code pin_source_uncommit=load_code(gc_code_pin_source_uncommit,sizeof(gc_code_pin_source_uncommit));
    gc_run_pin_source_release_cases((PinSourceDrainer)pin_source_drain.memory,(PinSourceUncommitter)pin_source_uncommit.memory);
    unload_code(pin_source_drain);unload_code(pin_source_uncommit);
    Code pin_registry_bind=load_code(gc_code_pin_registry_bind,sizeof(gc_code_pin_registry_bind));
    Code pin_registry_unbind=load_code(gc_code_pin_registry_unbind,sizeof(gc_code_pin_registry_unbind));
    Code pin_registry_pin=load_code(gc_code_pin_registry_pin,sizeof(gc_code_pin_registry_pin));
    Code pin_registry_unpin=load_code(gc_code_pin_registry_unpin,sizeof(gc_code_pin_registry_unpin));
    Code pin_registry_select=load_code(gc_code_pin_registry_select,sizeof(gc_code_pin_registry_select));
    Code pin_registry_reopen=load_code(gc_code_pin_registry_reopen,sizeof(gc_code_pin_registry_reopen));
    Code pin_relocation_claim=load_code(gc_code_pin_relocation_claim,sizeof(gc_code_pin_relocation_claim));
    Code pin_selection=load_code(gc_code_pin_selection,sizeof(gc_code_pin_selection));
    pin_registry_actions[0]=(void *)pin_registry_pin.memory;
    pin_registry_actions[1]=(void *)pin_registry_unpin.memory;
    pin_registry_actions[2]=(void *)pin_registry_select.memory;
    pin_registry_actions[3]=(void *)pin_registry_reopen.memory;
    gc_run_pin_registry_cases((PinRegistryOwner)pin_registry_bind.memory,(PinRegistryOwner)pin_registry_unbind.memory,
        emitted_pin_registry_step,(PinSelectionCheck)pin_selection.memory,(PinRelocationClaimer)pin_relocation_claim.memory);
    unload_code(pin_relocation_claim);
    unload_code(pin_registry_pin);unload_code(pin_registry_unpin);
    unload_code(pin_registry_select);unload_code(pin_registry_reopen);unload_code(pin_selection);
    unload_code(pin_registry_bind);unload_code(pin_registry_unbind);
    Code pin_page=load_code(gc_code_pin_page,sizeof(gc_code_pin_page));
    gc_run_pin_page_cases((PinPageStep)pin_page.memory);
    unload_code(pin_page);
    Code relocation_gate=load_code(gc_code_relocation_gate,sizeof(gc_code_relocation_gate));
    gc_run_relocation_gate_cases((RelocationGateStep)relocation_gate.memory);
    Code relocation_job=load_code(gc_code_relocation_job,sizeof(gc_code_relocation_job));
    gc_run_relocation_job_cases((RelocationJobStep)relocation_job.memory);
    Code relocation_complete=load_code(gc_code_relocation_complete,sizeof(gc_code_relocation_complete));
    gc_run_relocation_complete_cases((RelocationCompleter)relocation_complete.memory);
    Code marked_relocation=load_code(gc_code_marked_relocation,sizeof(gc_code_marked_relocation));
    gc_run_marked_relocation_cases((MarkedRelocationClaimer)marked_relocation.memory);
    Code relocation_claim=load_code(gc_code_relocation_claim,sizeof(gc_code_relocation_claim));
    gc_run_relocation_claim_cases((RelocationClaimer)relocation_claim.memory);
    Code relocate_discard=load_code(gc_code_relocate_discard,sizeof(gc_code_relocate_discard));
    gc_run_relocate_discard_cases((RelocationDiscarder)relocate_discard.memory);
    Code relocation_cancel=load_code(gc_code_relocation_cancel,sizeof(gc_code_relocation_cancel));
    gc_run_relocation_cancel_cases((RelocationCanceller)relocation_cancel.memory);
    Code alloc_into=load_code(gc_code_alloc_into,sizeof(gc_code_alloc_into));
    gc_run_alloc_into_cases((AllocateInto)alloc_into.memory);
    Code allocation_start=load_code(gc_code_allocation_start,sizeof(gc_code_allocation_start));
    gc_run_alloc_start_cases((AllocateStart)allocation_start.memory);
    gc_run_native_allocation_cases((NativeAllocationStarter)allocation_start.memory);
    gc_run_native_allocation_retire_cases((NativeAllocationStarter)allocation_start.memory);
    gc_run_relocation_claim_races((RelocationClaimer)relocation_claim.memory,(RelocationGateStep)relocation_gate.memory,(ForwardStep)forward.memory,(SourceAccess)source_access.memory);
    Code forward_incoming=load_code(gc_code_forward_incoming,sizeof(gc_code_forward_incoming));
    gc_run_forward_incoming_cases((ForwardIncomingScanner)forward_incoming.memory);



    Code metadata=load_code(gc_code_metadata_step,sizeof(gc_code_metadata_step));
    gc_run_metadata_cases((MetadataStep)metadata.memory);
    Code reference_lease=load_code(gc_code_reference_lease,sizeof(gc_code_reference_lease));
    gc_run_reference_lease_cases((ReferenceAcquirer)reference_lease.memory);
    Code budget=load_code(gc_code_budget_step,sizeof(gc_code_budget_step));
    gc_run_budget_cases((BudgetStep)budget.memory,(PoolStep)pool.memory);
    NativeMetadataOps native_metadata_ops=gc_native_metadata_ops();
    NativeRootPreparer native_root_prepare=NULL;
    NativeRootDisposer native_root_dispose=NULL;
    NativeBuiltinRegistryPreparer native_builtin_registry_prepare=NULL;
    NativeStartupPreparer native_startup_prepare=NULL;
#ifdef GC_LINUX_WORKERS
    Code native_roots_prepare=load_code(gc_code_native_roots_prepare,sizeof(gc_code_native_roots_prepare));
    native_root_prepare=(NativeRootPreparer)native_roots_prepare.memory;
    Code native_roots_dispose=load_code(gc_code_native_roots_dispose,sizeof(gc_code_native_roots_dispose));
    native_root_dispose=(NativeRootDisposer)native_roots_dispose.memory;
    Code builtin_registry_prepare=load_code(gc_code_native_builtin_registry_prepare,sizeof(gc_code_native_builtin_registry_prepare));
    native_builtin_registry_prepare=(NativeBuiltinRegistryPreparer)builtin_registry_prepare.memory;
    Code startup_prepare=load_code(gc_code_native_startup_prepare,sizeof(gc_code_native_startup_prepare));
    native_startup_prepare=(NativeStartupPreparer)startup_prepare.memory;
    Code metadata_map=load_code(gc_code_metadata_map,sizeof(gc_code_metadata_map));
    Code metadata_unmap=load_code(gc_code_metadata_unmap,sizeof(gc_code_metadata_unmap));
    native_metadata_ops=(NativeMetadataOps){4096,(void *(*)(size_t))metadata_map.memory,
        (bool (*)(void *,size_t))metadata_unmap.memory};
    assert(!native_metadata_ops.map(0)&&!native_metadata_ops.map(1));
    assert(!native_metadata_ops.map(UINT64_MAX-4095)); // real mmap errno -> NULL
    assert(!native_metadata_ops.unmap(NULL,4096));
    assert(!native_metadata_ops.unmap((void *)4097,4096));
    assert(!native_metadata_ops.unmap((void *)(UINT64_MAX-4095),4096)); // real munmap errno -> false
#endif
#ifdef _WIN32
    unsigned char metadata_bytes[sizeof(gc_win_metadata_code)];
    memcpy(metadata_bytes,gc_win_metadata_code,sizeof(metadata_bytes));
    void *metadata_alloc_import=(void *)VirtualAlloc,*metadata_free_import=(void *)VirtualFree;
    memcpy(metadata_bytes+GC_WIN_METADATA_ALLOC_IMPORT,&metadata_alloc_import,sizeof(metadata_alloc_import));
    memcpy(metadata_bytes+GC_WIN_METADATA_FREE_IMPORT,&metadata_free_import,sizeof(metadata_free_import));
    Code metadata_native=load_code(metadata_bytes,sizeof(metadata_bytes));
    native_metadata_ops=(NativeMetadataOps){4096,
        (void *(*)(size_t))((unsigned char *)metadata_native.memory+GC_WIN_METADATA_MAP),
        (bool (*)(void *,size_t))((unsigned char *)metadata_native.memory+GC_WIN_METADATA_UNMAP)};
    native_root_prepare=(NativeRootPreparer)((unsigned char *)metadata_native.memory+GC_WIN_METADATA_ROOTS_PREPARE);
    native_root_dispose=(NativeRootDisposer)((unsigned char *)metadata_native.memory+GC_WIN_METADATA_ROOTS_DISPOSE);
    native_builtin_registry_prepare=(NativeBuiltinRegistryPreparer)((unsigned char *)metadata_native.memory+GC_WIN_METADATA_BUILTIN_REGISTRY_PREPARE);
    native_startup_prepare=(NativeStartupPreparer)((unsigned char *)metadata_native.memory+GC_WIN_METADATA_STARTUP_PREPARE);
#endif
#if defined(__APPLE__) && defined(__aarch64__)
    unsigned char metadata_bytes[sizeof(gc_darwin_metadata_code)];
    memcpy(metadata_bytes,gc_darwin_metadata_code,sizeof(metadata_bytes));
    void *metadata_map_import=(void *)mmap,*metadata_unmap_import=(void *)munmap;
    memcpy(metadata_bytes+GC_DARWIN_METADATA_MAP_IMPORT,&metadata_map_import,sizeof(metadata_map_import));
    memcpy(metadata_bytes+GC_DARWIN_METADATA_UNMAP_IMPORT,&metadata_unmap_import,sizeof(metadata_unmap_import));
    Code metadata_native=load_code(metadata_bytes,sizeof(metadata_bytes));
    native_metadata_ops=(NativeMetadataOps){16384,
        (void *(*)(size_t))((unsigned char *)metadata_native.memory+GC_DARWIN_METADATA_MAP),
        (bool (*)(void *,size_t))((unsigned char *)metadata_native.memory+GC_DARWIN_METADATA_UNMAP)};
    native_root_prepare=(NativeRootPreparer)((unsigned char *)metadata_native.memory+GC_DARWIN_METADATA_ROOTS_PREPARE);
    native_root_dispose=(NativeRootDisposer)((unsigned char *)metadata_native.memory+GC_DARWIN_METADATA_ROOTS_DISPOSE);
    native_builtin_registry_prepare=(NativeBuiltinRegistryPreparer)((unsigned char *)metadata_native.memory+GC_DARWIN_METADATA_BUILTIN_REGISTRY_PREPARE);
    native_startup_prepare=(NativeStartupPreparer)((unsigned char *)metadata_native.memory+GC_DARWIN_METADATA_STARTUP_PREPARE);
#endif
#if defined(GC_LINUX_WORKERS) || defined(_WIN32) || (defined(__APPLE__) && defined(__aarch64__))
    gc_run_native_root_prepare_cases(native_root_prepare,native_root_dispose,(BudgetStep)budget.memory,&native_metadata_ops);
#endif
    if(native_builtin_registry_prepare)gc_run_native_builtin_prepare_cases(native_builtin_registry_prepare,(BudgetStep)budget.memory,&native_metadata_ops);
    gc_run_native_metadata_cases((BudgetStep)budget.memory,&native_metadata_ops);
    gc_run_native_roots_cases((RootStoragePreparer)root_storage_prepare.memory,(RootStorageDisposalCheck)root_storage_can_dispose.memory,(BudgetStep)budget.memory,&native_metadata_ops);
    Code pending_follow=load_code(gc_code_pending_follow,sizeof(gc_code_pending_follow));
    Code weak_create=load_code(gc_code_weak_create,sizeof(gc_code_weak_create));
    Code weak_follow=load_code(gc_code_weak_follow,sizeof(gc_code_weak_follow));
    Code weak_table_access=load_code(gc_code_weak_table_access,sizeof(gc_code_weak_table_access));
    gc_run_weak_table_access_cases((WeakTableStepper)weak_table_access.memory);
    gc_run_weak_follow_cases((WeakTableFollower)weak_follow.memory);
    Code weak_incoming=load_code(gc_code_weak_incoming,sizeof(gc_code_weak_incoming));
    gc_run_weak_incoming_cases((PendingFollower)pending_follow.memory,(WeakIncomingMetadata)metadata.memory,(WeakIncomingStepper)weak_incoming.memory);
    Code source_payload=load_code(gc_code_native_source_uncommit,sizeof(gc_code_native_source_uncommit));
#ifdef GC_LINUX_WORKERS
    Code source_os_decommit=load_code(gc_code_heap_decommit,sizeof(gc_code_heap_decommit));
    NativePageDecommitter os_decommit=(NativePageDecommitter)source_os_decommit.memory;
    assert(!os_decommit(NULL,4096)&&!os_decommit((void *)4097,4096));
    assert(!os_decommit((void *)4096,0)&&!os_decommit((void *)4096,4097));
    assert(!os_decommit((void *)(uintptr_t)(UINT64_MAX&~UINT64_C(4095)),4096));
    assert(!os_decommit((void *)4096,4096));
#else
    NativePageDecommitter os_decommit=gc_native_heap_decommit_range;
#endif
    gc_run_source_retire_pipeline((SourceRetirer)source_retire.memory,(PoolStep)pool.memory,(BudgetStep)budget.memory,(SourceUncommitter)source_uncommit.memory,(WeakTableFollower)weak_follow.memory,(WeakIncomingStepper)weak_incoming.memory,(WeakTableStepper)weak_table_access.memory,(SourcePayloadUncommitter)source_payload.memory,os_decommit);
    puts("native payload decommit: OS callback drives source retirement PASS");
#ifdef GC_LINUX_WORKERS
    puts("Linux payload decommit: direct syscall callback drives source retirement PASS");
    unload_code(source_os_decommit);
#endif
    unload_code(source_payload);
    unload_code(weak_table_access);
    unload_code(weak_incoming);
    unload_code(weak_follow);
    Code weak_load=load_code(gc_code_weak_load,sizeof(gc_code_weak_load));
    gc_run_weak_load_cases((WeakLoader)weak_load.memory);
    Code weak_release=load_code(gc_code_weak_release,sizeof(gc_code_weak_release));
    gc_run_weak_release_cases((WeakReleaser)weak_release.memory,(WeakLoader)weak_load.memory);
    gc_run_weak_table_cases(&(WeakTableOps){(RootStep)roots.memory,(ReferenceAcquirer)reference_lease.memory,(WeakLoader)weak_load.memory,(WeakReleaser)weak_release.memory},(WeakTableCreator)weak_create.memory);
    unload_code(weak_create);
    gc_run_native_weak_cases((BudgetStep)budget.memory,&native_metadata_ops,&(WeakTableOps){(RootStep)roots.memory,(ReferenceAcquirer)reference_lease.memory,(WeakLoader)weak_load.memory,(WeakReleaser)weak_release.memory});
    unload_code(weak_release);unload_code(weak_load);
    Code pending_mark=load_code(gc_code_pending_mark,sizeof(gc_code_pending_mark));
    Code pending_access=load_code(gc_code_pending_access,sizeof(gc_code_pending_access));
    gc_run_pending_follow_cases((PendingFollower)pending_follow.memory,(PendingAccessor)pending_access.memory);
    Code native_root_publish=load_code(gc_code_native_root_publish,sizeof(gc_code_native_root_publish));
    gc_run_native_root_publish_cases((NativeRootPublisher)native_root_publish.memory,&native_metadata_ops);
    gc_run_native_builtin_registry_cases((DescriptorRegister)descriptor.memory,(BuiltinPreparer)builtin_prepare.memory,(BudgetStep)budget.memory,&native_metadata_ops);
    Code native_startup=load_code(gc_code_native_startup,sizeof(gc_code_native_startup));
    Code startup_release=load_code(gc_code_native_startup_release,sizeof(gc_code_native_startup_release));
    Code native_startup_commit=load_code(gc_code_native_startup_commit,sizeof(gc_code_native_startup_commit));
    Code allocation_sample=load_code(gc_code_allocation_sample,sizeof(gc_code_allocation_sample));
    Code relocation_retire=load_code(gc_code_relocation_retire,sizeof(gc_code_relocation_retire));
    gc_run_relocation_retire_cases((RelocationPhaseStep)relocation_retire.memory);
    Code relocation_phase=load_code(gc_code_relocation_phase,sizeof(gc_code_relocation_phase));
    gc_run_relocation_phase_cases((RelocationPhaseStep)relocation_phase.memory);
    Code cycle_mark_start=load_code(gc_code_cycle_mark_start,sizeof(gc_code_cycle_mark_start));
    gc_run_cycle_mark_start_cases((CycleMarkStarter)cycle_mark_start.memory);
    gc_run_native_cycle_cases((CycleMarkStarter)cycle_mark_start.memory);
    gc_run_native_graph_cases(&(NativeGraphOps){(NativeAllocationStarter)allocation_start.memory,(CycleMarkStarter)cycle_mark_start.memory,(PendingMarker)pending_mark.memory,(uint64_t (*)(MarkScan *))mark_scan.memory,(MarkScanCommitter)mark_scan_commit.memory});
    Code cycle_pressure=load_code(gc_code_cycle_pressure,sizeof(gc_code_cycle_pressure));
    gc_run_cycle_pressure_cases((CyclePressureStep)cycle_pressure.memory);
    Code heap_pressure=load_code(gc_code_heap_allocation_pressure,sizeof(gc_code_heap_allocation_pressure));
    gc_run_heap_pressure_cases((HeapAllocationPressure)heap_pressure.memory);
    Code allocation_pressure=load_code(gc_code_allocation_pressure,sizeof(gc_code_allocation_pressure));
    Code bind = load_code(gc_code_tlab_page, sizeof(gc_code_tlab_page));
    Code mark_prepare=load_code(gc_code_mark_prepare,sizeof(gc_code_mark_prepare));
    gc_run_mark_prepare_cases((MarkPreparer)mark_prepare.memory);
    Code mark_route=load_code(gc_code_mark_route,sizeof(gc_code_mark_route));
    gc_run_mark_route_cases((MarkRouter)mark_route.memory);
    Code metadata_grow=load_code(gc_code_metadata_grow,sizeof(gc_code_metadata_grow));
    gc_run_metadata_grow_cases((MetadataGrower)metadata_grow.memory);
    Code initialization=load_code(gc_code_initialization,sizeof(gc_code_initialization));
    gc_run_initialization_cases((InitializationPublisher)initialization.memory);
    Code initialization_store=load_code(gc_code_initialization_store,sizeof(gc_code_initialization_store));
    gc_run_initialization_store_cases((InitializationStorer)initialization_store.memory);
    Code initialization_copy=load_code(gc_code_initialization_copy,sizeof(gc_code_initialization_copy));
    gc_run_initialization_copy_cases((InitializationCopier)initialization_copy.memory);
    gc_run_mark_initializing_races((MarkRouter)mark_route.memory,(SourceAccess)source_access.memory,(InitializationPublisher)initialization.memory);
    Code worker_step=load_code(gc_code_worker_step,sizeof(gc_code_worker_step));
    gc_run_worker_cases((WorkerStep)worker_step.memory);
    Code root_journal=load_code(gc_code_root_journal,sizeof(gc_code_root_journal));
    gc_run_root_journal_cases((RootJournalStep)root_journal.memory);
    gc_run_root_journal_races((RootJournalStep)root_journal.memory,(RootStep)roots.memory);
    Code buffer_job=load_code(gc_code_buffer_job,sizeof(gc_code_buffer_job));
    gc_run_buffer_job_cases((BufferJobStep)buffer_job.memory);
    gc_run_native_buffer_quiescence_cases((BufferJobStep)buffer_job.memory);
    gc_run_native_buffer_incoming_cases();
    Code root_audit=load_code(gc_code_root_audit,sizeof(gc_code_root_audit));
    gc_run_root_audit_cases((RootAuditor)root_audit.memory);
    Code root_retire_audit=load_code(gc_code_root_retire_audit,sizeof(gc_code_root_retire_audit));
    gc_run_root_retire_audit_cases((RootAuditor)root_retire_audit.memory);
    Code mark_end=load_code(gc_code_mark_end,sizeof(gc_code_mark_end));
    gc_run_mark_end_cases((MarkEndChecker)mark_end.memory);
    Code retire_inventory=load_code(gc_code_retire_inventory,sizeof(gc_code_retire_inventory));
    gc_run_retire_inventory_cases((MarkEndChecker)retire_inventory.memory);
    Code mark_phase=load_code(gc_code_mark_phase,sizeof(gc_code_mark_phase));
    gc_run_mark_phase_cases((MarkPhaseStepper)mark_phase.memory);
    Code weak_mark_load=load_code(gc_code_weak_load,sizeof(gc_code_weak_load));
    Code weak_mark=load_code(gc_code_weak_mark,sizeof(gc_code_weak_mark));
    gc_run_weak_mark_cases((MarkPhaseStepper)mark_phase.memory,(WeakLoader)weak_mark_load.memory,(WeakMarkStepper)weak_mark.memory);
    Code weak_liveness=load_code(gc_code_weak_liveness,sizeof(gc_code_weak_liveness));
    gc_run_weak_liveness_cases((WeakLiveness)weak_liveness.memory);
    Code weak_capture=load_code(gc_code_weak_capture,sizeof(gc_code_weak_capture));
    Code weak_access=load_code(gc_code_weak_access,sizeof(gc_code_weak_access));
    Code weak_finish=load_code(gc_code_weak_finish,sizeof(gc_code_weak_finish));
    gc_run_weak_process_cases(&(WeakProcessOps){(ReferenceAcquirer)reference_lease.memory,(PendingFollower)pending_follow.memory,(PendingAccessor)pending_access.memory,(Resolver)resolve.memory,(WeakLiveness)weak_liveness.memory,(MetadataStep)metadata.memory},(WeakProcessCapture)weak_capture.memory,(WeakProcessAccess)weak_access.memory,(WeakProcessFinish)weak_finish.memory);
    unload_code(weak_capture);
    Code weak_scan_release=load_code(gc_code_weak_release,sizeof(gc_code_weak_release));
    Code weak_scan=load_code(gc_code_weak_scan,sizeof(gc_code_weak_scan));
    gc_run_weak_scan_cases(&(WeakProcessOps){(ReferenceAcquirer)reference_lease.memory,(PendingFollower)pending_follow.memory,(PendingAccessor)pending_access.memory,(Resolver)resolve.memory,(WeakLiveness)weak_liveness.memory,(MetadataStep)metadata.memory},&(WeakTableOps){(RootStep)roots.memory,(ReferenceAcquirer)reference_lease.memory,(WeakLoader)weak_mark_load.memory,(WeakReleaser)weak_scan_release.memory},(WeakScanStepper)weak_scan.memory,(WeakProcessAccess)weak_access.memory,(WeakProcessFinish)weak_finish.memory);
    gc_run_weak_scan_concurrency(&(WeakProcessOps){(ReferenceAcquirer)reference_lease.memory,(PendingFollower)pending_follow.memory,(PendingAccessor)pending_access.memory,(Resolver)resolve.memory,(WeakLiveness)weak_liveness.memory,(MetadataStep)metadata.memory},&(WeakTableOps){(RootStep)roots.memory,(ReferenceAcquirer)reference_lease.memory,(WeakLoader)weak_mark_load.memory,(WeakReleaser)weak_scan_release.memory},(WeakProcessAccess)weak_access.memory,(WeakProcessFinish)weak_finish.memory,(WeakScanStepper)weak_scan.memory);
    Code weak_relocation=load_code(gc_code_weak_relocation,sizeof(gc_code_weak_relocation));
    gc_run_weak_cycle_cases(&(WeakCycleOps){{(ReferenceAcquirer)reference_lease.memory,(PendingFollower)pending_follow.memory,(PendingAccessor)pending_access.memory,(Resolver)resolve.memory,(WeakLiveness)weak_liveness.memory,(MetadataStep)metadata.memory},{(RootStep)roots.memory,(ReferenceAcquirer)reference_lease.memory,(WeakLoader)weak_mark_load.memory,(WeakReleaser)weak_scan_release.memory},(WeakMarkStepper)weak_mark.memory,(PendingMarker)pending_mark.memory,(RootJournalStep)root_journal.memory,(MarkStep)mark.memory,(MarkScanner)mark_scan.memory,(RootAuditor)root_audit.memory,(RootWalkScanner)walk_scan.memory,(DescriptorRegister)descriptor.memory,(RelocationPhaseStep)relocation_phase.memory,(WeakProcessAccess)weak_access.memory,(WeakProcessFinish)weak_finish.memory,(WeakScanStepper)weak_scan.memory,(WeakRelocationBegin)weak_relocation.memory});
    unload_code(weak_mark);
    unload_code(weak_relocation);
    unload_code(weak_scan);
    unload_code(weak_finish);unload_code(weak_access);
    unload_code(weak_scan_release);unload_code(weak_mark_load);
    unload_code(weak_liveness);
    Code cycle_step=load_code(gc_code_cycle_step,sizeof(gc_code_cycle_step));
    gc_run_cycle_cases((CycleStepper)cycle_step.memory);
    gc_run_remembered_cases((RememberedStep)remembered_step.memory);
    gc_run_remembered_inventory_cases((RememberedInventoryStep)remembered_inventory.memory);
    Code remembered_transfer=load_code(gc_code_remembered_transfer,sizeof(gc_code_remembered_transfer));
    gc_run_remembered_transfer_cases((RememberedTransferStep)remembered_transfer.memory);
    Code remembered_owner=load_code(gc_code_remembered_owner,sizeof(gc_code_remembered_owner));
    gc_run_remembered_owner_cases((RememberedOwnerFinder)remembered_owner.memory);
    Code remembered_lease=load_code(gc_code_remembered_lease,sizeof(gc_code_remembered_lease));
    gc_run_remembered_lease_cases((RememberedAcquirer)remembered_lease.memory);
    Code remembered_walk=load_code(gc_code_remembered_walk,sizeof(gc_code_remembered_walk));
    gc_run_remembered_walk_cases((RememberedWalker)remembered_walk.memory,(RememberedInventoryStep)remembered_inventory.memory);
    gc_run_remembered_owner_races((RememberedOwnerFinder)remembered_owner.memory);
    gc_run_remembered_end((RememberedInventoryStep)remembered_inventory.memory,(MarkEndChecker)mark_end.memory);
    GraphLifecycle lifecycle={(WorkerStep)worker_step.memory,(RootDetachHandshake)detach.memory,(RootLife)life.memory,(RootClear)clear.memory,(RootLeasedScan)leased.memory,(RootJournalStep)root_journal.memory,(BufferJobStep)buffer_job.memory,(RootAuditor)root_audit.memory,(MarkEndChecker)mark_end.memory,(MarkPhaseStepper)mark_phase.memory,(RootWalkScanner)walk_scan.memory,(RootWalkHealer)walk_heal.memory,(uint64_t (*)(RootWalkItem *,uint64_t))walk_item.memory,(RelocationGateStep)relocation_gate.memory,(RelocationCompleter)relocation_complete.memory,(RelocationClaimer)relocation_claim.memory,(CycleStepper)cycle_step.memory,(CycleMarkStarter)cycle_mark_start.memory,(GuardedIdentity)identity.memory,(GuardedBlockSize)block_size.memory,(MarkedRelocationClaimer)marked_relocation.memory,(RelocationPhaseStep)relocation_phase.memory,(RelocationPhaseStep)relocation_retire.memory,(RootAuditor)root_retire_audit.memory,(MarkEndChecker)retire_inventory.memory,(SourceRetirer)source_retire.memory};
    Code root_follow=load_code(gc_code_root_follow,sizeof(gc_code_root_follow));
    Code access_cleanup=load_code(gc_code_root_access_cleanup,sizeof(gc_code_root_access_cleanup));
    gc_run_root_access_cleanup_cases((RootAccessCleanup)access_cleanup.memory);
    Code rooted_load_admit=load_code(gc_code_rooted_load_admit,sizeof(gc_code_rooted_load_admit));
    Code rooted_load=load_code(gc_code_rooted_load,sizeof(gc_code_rooted_load));
    Code rooted_mark_publish=load_code(gc_code_rooted_mark_publish,sizeof(gc_code_rooted_mark_publish));
    Code rooted_mark_set=load_code(gc_code_rooted_mark_set,sizeof(gc_code_rooted_mark_set));
    Code rooted_mark=load_code(gc_code_rooted_mark,sizeof(gc_code_rooted_mark));
    Code rooted_mark_spill=load_code(gc_code_rooted_mark_spill,sizeof(gc_code_rooted_mark_spill));
    Code rooted_store_dispatch=load_code(gc_code_rooted_store_dispatch,sizeof(gc_code_rooted_store_dispatch));
    gc_run_rooted_store_dispatch_cases((RootedStoreDispatcher)rooted_store_dispatch.memory);
    Code rooted_load_dispatch=load_code(gc_code_rooted_load_dispatch,sizeof(gc_code_rooted_load_dispatch));
    Code barrier_poll=load_code(gc_code_barrier_poll,sizeof(gc_code_barrier_poll));
    gc_run_rooted_store_concurrency((RootedStoreDispatcher)rooted_store_dispatch.memory,(RootedLoadDispatcher)rooted_load_dispatch.memory,(BarrierPoller)barrier_poll.memory,(PendingMarker)pending_mark.memory);
    Code barrier_assist=load_code(gc_code_barrier_assist,sizeof(gc_code_barrier_assist));
    gc_run_barrier_assist_cases((BarrierAssister)barrier_assist.memory,(BarrierPoller)barrier_poll.memory);
    unload_code(barrier_assist);
    unload_code(barrier_poll);
    unload_code(rooted_load_dispatch);
    Code rooted_store=load_code(gc_code_rooted_store,sizeof(gc_code_rooted_store));
    Code rooted_store_attempt=load_code(gc_code_rooted_store_attempt,sizeof(gc_code_rooted_store_attempt));
    gc_run_rooted_store_attempt_cases((RootedStoreAttempter)rooted_store_attempt.memory);
    Code rooted_store_commit=load_code(gc_code_rooted_store_commit,sizeof(gc_code_rooted_store_commit));
    gc_run_rooted_store_commit_cases((RootedStoreCommitter)rooted_store_commit.memory);
    Code rooted_value=load_code(gc_code_rooted_value,sizeof(gc_code_rooted_value));
    Code rooted_scalar=load_code(gc_code_rooted_scalar,sizeof(gc_code_rooted_scalar));
    gc_run_rooted_scalar_cases((RootedScalarStep)rooted_scalar.memory);
    Code rooted_observation=load_code(gc_code_rooted_observation,sizeof(gc_code_rooted_observation));
    Code heap_audit=load_code(gc_code_heap_audit,sizeof(gc_code_heap_audit));
    gc_run_heap_audit_cases((HeapAuditor)heap_audit.memory);
    Code heap_audit_slice=load_code(gc_code_heap_audit_slice,sizeof(gc_code_heap_audit_slice));
    gc_run_heap_audit_slice_cases((HeapAuditSlicer)heap_audit_slice.memory);
    Code object_walk=load_code(gc_code_object_walk,sizeof(gc_code_object_walk));
    gc_run_object_walk_cases((ObjectWalker)object_walk.memory);
    Code heap_walk=load_code(gc_code_heap_walk,sizeof(gc_code_heap_walk));
    gc_run_heap_walk_cases((HeapWalker)heap_walk.memory);
    Code heap_incoming=load_code(gc_code_heap_incoming,sizeof(gc_code_heap_incoming));
    gc_run_heap_incoming_cases((HeapIncomingScanner)heap_incoming.memory);
    Code forward_compress=load_code(gc_code_forward_compress,sizeof(gc_code_forward_compress));
    gc_run_forward_compress_cases((ForwardCompressor)forward_compress.memory);
    Code relocation_turn=load_code(gc_code_relocation_turn,sizeof(gc_code_relocation_turn));
    gc_run_relocation_turn_cases((RelocationTurnStep)relocation_turn.memory);
    Code promotion_remember=load_code(gc_code_promotion_remember,sizeof(gc_code_promotion_remember));
    Code marked_age=load_code(gc_code_marked_survivor_age,sizeof(gc_code_marked_survivor_age));
    gc_run_marked_survivor_age_cases((MarkedSurvivorAge)marked_age.memory);
    Code survivor_age=load_code(gc_code_survivor_age,sizeof(gc_code_survivor_age));
    gc_run_survivor_age_cases((SurvivorAge)survivor_age.memory);
    gc_run_promotion_remember_cases((PromotionRememberStep)promotion_remember.memory);
    Code root_access=load_code(gc_code_root_access,sizeof(gc_code_root_access));
    gc_run_root_access_cases((RootAccessor)root_access.memory);
    gc_run_root_access_cleanup_races((RootAccessor)root_access.memory,(RootAccessCleanup)access_cleanup.memory,(SourceAccess)source_access.memory,(ForwardStep)forward.memory);
    gc_run_root_follow_cases((RootFollower)root_follow.memory);
    Code mark_retarget=load_code(gc_code_mark_retarget,sizeof(gc_code_mark_retarget));
    gc_run_mark_retarget_cases((MarkRetargeter)mark_retarget.memory,(MarkStep)mark.memory);
    MarkMoveOps move={(PoolStep)pool.memory,(TlabPage)bind.memory,(TlabAllocator)allocate.memory,(DescriptorRegister)descriptor.memory,
        (ReferenceAcquirer)reference_lease.memory,(MetadataStep)metadata.memory,(Resolver)resolve.memory,(ResolveRootStep)roots.memory,
        (RootFollower)root_follow.memory,(MarkPreparer)mark_prepare.memory,(MarkRetargeter)mark_retarget.memory,(MarkStep)mark.memory,
        (MoveMarkSpill)mark_spill.memory,(MarkScanner)mark_scan.memory,(ForwardStep)forward.memory,(SourceAccess)source_access.memory,
        (RelocateTarget)relocate_target.memory,(RelocateCopy)relocate_copy.memory,(RelocatePublish)relocate_publish.memory,(SurvivorAge)survivor_age.memory,(MarkedSurvivorAge)marked_age.memory};
    GraphRememberedOps remembered={&move,(RememberedInventoryStep)remembered_inventory.memory,(RememberedOwnerFinder)remembered_owner.memory,(RememberedLoader)remembered_load.memory,(RememberedWalker)remembered_walk.memory,(RememberedAcquirer)remembered_lease.memory,(RememberedTransferStep)remembered_transfer.memory,(BarrierBufferStep)buffer_step.memory,(RootAccessor)root_access.memory,(PromotionRememberStep)promotion_remember.memory,(RootedLoader)rooted_load.memory,(RootedLoadAdmitter)rooted_load_admit.memory,(PendingFollower)pending_follow.memory,(PendingAccessor)pending_access.memory};
    atomic_stage("mark_graph begin",0);
    gc_run_mark_graph(&lifecycle,&remembered,(MarkRouter)mark_route.memory,(MarkStep)mark.memory,(MarkSpill)mark_spill.memory,(MarkScanner)mark_scan.memory,(MetadataStep)metadata.memory,(BudgetStep)budget.memory,(ReferenceAcquirer)reference_lease.memory,(Resolver)resolve.memory,(GuardedStorer)guarded_store.memory,(GuardedHealer)guarded_heal.memory,(PoolStep)pool.memory,(TlabPage)bind.memory,(AllocateInto)alloc_into.memory,(DescriptorRegister)descriptor.memory,(RootStep)roots.memory,(SourceDrainer)source_drain.memory,(SourceUncommitter)source_uncommit.memory,(ForwardIncomingScanner)forward_incoming.memory,(ForwardCompressor)forward_compress.memory,(HeapIncomingScanner)heap_incoming.memory,(HeapWalker)heap_walk.memory,(ObjectWalker)object_walk.memory,(HeapAuditSlicer)heap_audit_slice.memory,(HeapAuditor)heap_audit.memory);
    atomic_stage("mark_graph completed; mark_move begin",0);
    gc_run_mark_move_pipeline(&move,(RememberedInventoryStep)remembered_inventory.memory,(PromotionRememberStep)promotion_remember.memory);
    atomic_stage("mark_move completed; remembered begin",0);
    gc_run_remembered_pipeline(&move,(RememberedInventoryStep)remembered_inventory.memory,(RememberedLoader)remembered_load.memory,(MarkRouter)mark_route.memory,(RememberedTransferStep)remembered_transfer.memory,(RememberedOwnerFinder)remembered_owner.memory,(RememberedAcquirer)remembered_lease.memory,(ResolveRootDrain)root_drain.memory,(PromotionRememberStep)promotion_remember.memory);
    gc_run_resolve_chains((RootFollower)root_follow.memory,(Resolver)resolve.memory,(ReferenceAcquirer)reference_lease.memory,(MetadataStep)metadata.memory,(ForwardStep)forward.memory,(SourceAccess)source_access.memory,(RootStep)roots.memory,(RootDrainer)root_drain.memory,(RelocateCopy)relocate_copy.memory,(RelocatePublish)relocate_publish.memory,(RelocateTarget)relocate_target.memory,(PoolStep)pool.memory,(ForwardCompressor)forward_compress.memory,(ForwardIncomingScanner)forward_incoming.memory,(PendingFollower)pending_follow.memory,(PendingAccessor)pending_access.memory,(PendingMarker)pending_mark.memory);
    unload_code(pending_follow);
    unload_code(pending_mark);
    unload_code(pending_access);
    gc_run_relocate_pipeline((PoolStep)pool.memory,(TlabPage)bind.memory,(TlabAllocator)allocate.memory,(RelocateTarget)relocate_target.memory,(RelocateCopy)relocate_copy.memory,(RelocatePublish)relocate_publish.memory,(SourceAccess)source_access.memory,(ReferenceAcquirer)reference_lease.memory,(MetadataStep)metadata.memory,(Resolver)resolve.memory,(uint64_t (*)(RelocationGate *,RelocationGateRequest *,uint64_t))relocation_gate.memory,(uint64_t (*)(AllocationPage *,SourceDrain *))source_drain.memory,(uint64_t (*)(HeapPool *,PageRelease *,uint64_t))page_release.memory,(RelocationJobStep)relocation_job.memory,(RelocationCompleter)relocation_complete.memory,(RelocationClaimer)relocation_claim.memory,(RelocationDiscarder)relocate_discard.memory,(RelocationCanceller)relocation_cancel.memory,(RelocationTurnStep)relocation_turn.memory);
    gc_run_page_release_pipeline((PoolStep)pool.memory,(PageReleaser)page_release.memory,(BudgetStep)budget.memory,(TlabPage)bind.memory,(TlabAllocator)allocate.memory,(RelocateTarget)relocate_target.memory,(RelocateCopy)relocate_copy.memory,(ForwardStep)forward.memory,(SourceAccess)source_access.memory);
    gc_run_guarded_load_pipeline((MarkPreparer)mark_prepare.memory,(MarkStep)mark.memory,(MarkScanner)mark_scan.memory,(RootFollower)root_follow.memory,(GuardedLoader)guarded_load.memory,(GuardedStorer)guarded_store.memory,(PoolStep)pool.memory,(TlabPage)bind.memory,(TlabAllocator)allocate.memory,(DescriptorRegister)descriptor.memory,(ReferenceAcquirer)reference_lease.memory,(MetadataStep)metadata.memory,(Resolver)resolve.memory,(RootStep)roots.memory,(RootDrainer)root_drain.memory,(ForwardStep)forward.memory,(SourceAccess)source_access.memory,(RelocateTarget)relocate_target.memory,(RelocateCopy)relocate_copy.memory,(RelocatePublish)relocate_publish.memory,(RootAccessor)root_access.memory,(ScalarAccessor)guarded_scalar.memory,(RootCopier)root_copy.memory,(GuardedLogicalCas)logical_cas.memory,(GuardedIdentity)identity.memory,(GuardedObserver)observe.memory,(RootAccessCleanup)access_cleanup.memory,(RootedObserver)rooted_observation.memory,(MarkSpill)mark_spill.memory,(RootedMarker)rooted_mark.memory,(RootedMarker)rooted_mark_spill.memory,(RootedMarkSetter)rooted_mark_set.memory,(RootedMarkPublisher)rooted_mark_publish.memory,(GuardedHealer)guarded_heal.memory,(RootedLoader)rooted_load.memory,(WorkerStep)worker_step.memory,(RootedLoadAdmitter)rooted_load_admit.memory,(RootedLoader)rooted_value.memory,(RootedStorer)rooted_store.memory);
    unload_code(relocate_target);
    unload_code(relocate_publish);unload_code(relocate_copy);
    unload_code(forward_incoming);unload_code(relocation_gate);unload_code(relocation_job);unload_code(relocation_complete);unload_code(relocation_claim);unload_code(relocate_discard);unload_code(relocation_cancel);
    unload_code(source_uncommit);unload_code(source_retire);
    unload_code(source_drain);
    unload_code(forward);unload_code(resolve);


    gc_run_metadata_races((MetadataStep)metadata.memory,(BarrierBufferStep)buffer_step.memory,(BufferJobStep)buffer_job.memory,(PoolStep)pool.memory,(TlabPage)bind.memory,(RootDetachHandshake)detach.memory);
    unload_code(buffer_step);unload_code(buffer_drain);
    gc_run_binding_cases((TlabPage)bind.memory);
    gc_run_page_pipeline((PoolStep)pool.memory, (TlabPage)bind.memory, (AllocateStart)allocation_start.memory,
        (DescriptorRegister)descriptor.memory, (DescriptorScanner)scanner.memory,
        (RootStep)roots.memory,
        &(PagePublicationOps){(ReferenceAcquirer)reference_lease.memory,(RootAccessor)root_access.memory,(MetadataGrower)metadata_grow.memory,(BudgetStep)budget.memory,(MetadataStep)metadata.memory,
            (SourceAccess)source_access.memory,(InitializationPublisher)initialization.memory,(InitializationStorer)initialization_store.memory,(InitializationCopier)initialization_copy.memory,(ScalarAccessor)guarded_scalar.memory,
            (MarkRouter)mark_route.memory,(MarkStep)mark.memory,(MarkScanner)mark_scan.memory,
            (WorkerStep)worker_step.memory,(RootLife)life.memory,(RootLeasedScan)leased.memory,
            (RootJournalStep)root_journal.memory,(RootClear)clear.memory,(RootDetachHandshake)detach.memory,
            (MarkPhaseStepper)mark_phase.memory,(RootAuditor)root_audit.memory,(ForwardFinder)forward_lookup.memory,(MarkSpill)mark_spill.memory,&native_metadata_ops,(BuiltinPreparer)builtin_prepare.memory,(RootStoragePreparer)root_storage_prepare.memory,(RootStorageDisposalCheck)root_storage_can_dispose.memory,native_root_prepare,native_root_dispose,native_builtin_registry_prepare,(NativeStartupReserver)native_startup.memory,native_startup_prepare,(NativeStartupCommitter)native_startup_commit.memory,(AllocationSampler)allocation_sample.memory,(MarkScanCommitter)mark_scan_commit.memory,(AllocationPressure)allocation_pressure.memory,(CycleStepper)cycle_step.memory,(HeapAllocationPressure)heap_pressure.memory,(RootCopier)root_copy.memory,(NativeStartupReleaser)startup_release.memory,(RootAccessCleanup)access_cleanup.memory,(CyclePressureStep)cycle_pressure.memory,(CycleMarkStarter)cycle_mark_start.memory,(NativeRootPublisher)native_root_publish.memory});
    unload_code(native_root_publish);
#ifdef GC_LINUX_WORKERS
    unload_code(native_roots_prepare);
    unload_code(native_roots_dispose);
    unload_code(builtin_registry_prepare);
    unload_code(startup_prepare);
    unload_code(metadata_map);unload_code(metadata_unmap);
    puts("emitted Linux metadata: direct mmap/munmap preserve the native backing lifecycle PASS");
#endif
#ifdef _WIN32
    unload_code(metadata_native);
    puts("emitted Windows metadata: kernel32 imports back live lease storage through worker retirement PASS");
#endif
#if defined(__APPLE__) && defined(__aarch64__)
    unload_code(metadata_native);
    puts("emitted Darwin metadata: libSystem imports back live lease storage through worker retirement PASS");
#endif
    unload_code(budget);
    unload_code(mark_spill);
    unload_code(forward_lookup);
    unload_code(mark_scan_commit);
    unload_code(mark_scan);unload_code(mark);
    unload_code(source_access);unload_code(reference_lease);unload_code(metadata);
    unload_code(alloc_into);
    unload_code(allocation_start);
    unload_code(root_drain); unload_code(root_scan); unload_code(roots);
    unload_code(scanner);
    unload_code(root_copy);unload_code(root_cancel);
    unload_code(allocation_shape);
    unload_code(root_storage_can_dispose);
    unload_code(root_storage_prepare);
    unload_code(builtin_prepare);
    unload_code(descriptor);
    unload_code(bind);
    unload_code(native_startup);
    unload_code(startup_release);
    unload_code(native_startup_commit);
    unload_code(allocation_sample);
    unload_code(heap_pressure);
    unload_code(allocation_pressure);
    unload_code(remembered_inventory);unload_code(remembered_step);unload_code(cycle_step);unload_code(mark_phase);unload_code(mark_end);unload_code(retire_inventory);unload_code(root_audit);unload_code(root_retire_audit);unload_code(walk_scan);unload_code(walk_heal);unload_code(walk_item);
    unload_code(buffer_job);
    unload_code(root_journal);
    unload_code(worker_step);unload_code(detach);unload_code(leased);unload_code(clear);unload_code(life);
    unload_code(mark_retarget);
    unload_code(mark_prepare);
    unload_code(root_follow);
    unload_code(access_cleanup);
    unload_code(survivor_age);
    unload_code(marked_age);
    unload_code(promotion_remember);
    unload_code(relocation_turn);
    unload_code(forward_compress);
    unload_code(heap_incoming);
    unload_code(heap_walk);
    unload_code(object_walk);
    unload_code(heap_audit_slice);
    unload_code(heap_audit);
    unload_code(rooted_observation);
    unload_code(rooted_scalar);
    unload_code(rooted_value);
    unload_code(rooted_store_commit);
    unload_code(rooted_store_attempt);
    unload_code(rooted_store);
    unload_code(rooted_store_dispatch);
    unload_code(rooted_mark);
    unload_code(rooted_mark_set);
    unload_code(rooted_load_admit);
    unload_code(rooted_load);
    unload_code(rooted_mark_publish);
    unload_code(rooted_mark_spill);
    unload_code(root_access);
    unload_code(observe);
    unload_code(guarded_load);
    unload_code(guarded_heal);
    unload_code(remembered_load);
    unload_code(remembered_transfer);
    unload_code(remembered_owner);unload_code(remembered_walk);unload_code(remembered_lease);
    unload_code(guarded_scalar);
    unload_code(initialization_copy);
    unload_code(initialization_store);
    unload_code(initialization);unload_code(metadata_grow);
    unload_code(mark_route);
    unload_code(identity);
    unload_code(block_size);
    unload_code(logical_cas);
    unload_code(guarded_store);
    unload_code(page_release);
    unload_code(pool);
    unload_code(allocate);
    // High bits, failed CAS and idempotent OR exercise observed-value returns.
    store.call(&shared, UINT64_C(0xfedcba9876543210), 0);
    assert(cas.call(&shared, 7, 9) == UINT64_C(0xfedcba9876543210));
    assert(load.call(&shared, 0, 0) == UINT64_C(0xfedcba9876543210));
    assert(cas.call(&shared, UINT64_C(0xfedcba9876543210), 42) == UINT64_C(0xfedcba9876543210));
    assert(fetch_or.call(&shared, 2, 0) == 42);
    assert(load.call(&shared, 0, 0) == 42);
    fence.call(NULL, 0, 0);
    for (unsigned mode = 0; mode < 5; mode++) run_pair(mode);
#ifdef GC_LINUX_WORKERS
    run_native_workers();
    run_native_handshake();
#endif
    unload_code(load); unload_code(store); unload_code(cas); unload_code(fetch_or); unload_code(fence);
    unload_code(load32); unload_code(store32); unload_code(cas32);
#ifdef _WIN32
    DeleteCriticalSection(&latch.mutex);
#else
    assert(pthread_cond_destroy(&latch.condition) == 0);
    assert(pthread_mutex_destroy(&latch.mutex) == 0);
#endif
    printf("emitted atomics: %d CAS winner, %d fetch-or, %d publication trials PASS\n", ROUNDS, ROUNDS, ROUNDS);
    printf("emitted atomics: %d u32 CAS, %d u32 publication, exact-width context/guard checks PASS\n", ROUNDS, ROUNDS);
    atomic_stage("main completed",0);
    return 0;
}
