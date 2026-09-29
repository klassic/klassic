/* klassic_gc.c -- ZGC-style region collector in C: colored pointers + a
 * phase-dependent load barrier, incremental marking, and incremental
 * compacting relocation (evacuate-on-demand + in-object forwarding), all
 * driven in bounded quanta from the allocator. See klassic_gc.h for the
 * object layout, the color/phase model, and the migration context.
 */
#define _DEFAULT_SOURCE /* for MAP_ANONYMOUS under -std=c11 */

#include "klassic_gc.h"
#include "gc_test_hooks.h"
#include "gc_context.h"
#include "gc_handshake.h"
#include "gc_cycle.h"

#include <pthread.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h> /* clock_gettime for --gc-log pause measurement */

/* --- Region heap geometry ---------------------------------------- */
#define REGION_SHIFT 17
#define REGION_SIZE (1u << REGION_SHIFT) /* 128 KiB */
#define RESERVE_REGIONS 512
#define RESERVE_BYTES ((size_t)RESERVE_REGIONS * REGION_SIZE) /* 64 MiB */
#define SHADOW_MAX (1u << 20)
#define WORKLIST_MAX (1u << 18)
#define MAX_MUTATORS 64 /* registered mutator threads (thread table capacity) */

/* The whole heap is one up-front reservation; Linux demand-pages it. A
 * soft budget bounds how many regions the mutator touches before a
 * collection. */
static uint8_t *g_region_base;
static uint64_t g_region_top[RESERVE_REGIONS]; /* per-region bump watermark */
static uint64_t g_committed;                   /* high-water committed regions */
static uint64_t g_budget = 8;                  /* soft budget, doubles on stall */
static uint8_t *g_free_head;                    /* free-region pool, linked via base qword */
static uint64_t g_region_live[RESERVE_REGIONS]; /* live bytes per region (set by sweep) */
static uint8_t g_from_set[RESERVE_REGIONS];     /* 1 if region is in the relocation set */

/* Per-mutator state: the precise-root shadow stack and the active bump region
 * (TLAB). klassic compiled programs are single-threaded -- one mutator,
 * registered at init -- but the collector is built for N: each thread that
 * touches the heap registers a Mutator in the table and deregisters on exit,
 * and the GC thread walks the table (roots, active regions) at handshakes.
 * The shadow stack is heap-allocated per thread so idle table slots cost
 * nothing. */
typedef struct Mutator {
    MutatorContext context; /* shared fixed-width ABI; owner of the TLAB */
    ThreadRegistration registration;
    void ***shadow;     /* SHADOW_MAX slot addresses (heap-allocated) */
    uint64_t shadow_top;
    int in_use; /* table slot occupied */
} Mutator;
static Mutator g_mutators[MAX_MUTATORS];
static uint64_t g_mutator_hi;    /* 1 + highest in-use slot index (walk bound) */
static HandshakeState g_handshake; /* shared registry-lock protected counters */
static __thread Mutator *g_self; /* the calling thread's mutator */
/* The thread table is guarded by g_hs_lock (declared below), so registration
 * and a phase-transition rendezvous are mutually exclusive. */

/* A dedicated to-space bump for evacuation, separate from the mutator's
 * current region so compaction never disturbs in-progress allocation. */
static uint8_t *g_to_base;
static uint8_t *g_to_top;
static uint8_t *g_to_end;
static uint64_t g_relocations; /* objects evacuated (observability) */

/* The mark worklist (shared across all mutators + the GC thread). */
static void *g_worklist[WORKLIST_MAX];
static uint64_t g_worklist_top;
/* Private to the sole collector thread, retained between trace quanta. Mark
 * cannot finish while this cursor is active. Legacy relocation starts only
 * after mark termination, so the current payload stays at this address. This
 * is not a multi-worker or concurrent-moving cursor/lifetime protocol. */
static void **g_trace_cursor, **g_trace_end;

/* The current-cycle header mark bit (alternates each collection). */
static uint64_t g_header_mark = KLASSIC_GC_HMARK1;
static uint64_t g_collections;

/* --- Optional statistics (--gc-log; compiled only with -DKLASSIC_GC_LOG) --- *
 * Separate action time, request-to-quiescence, stopped interval through
 * release, and each mutator's park-to-resume interval. Root traversal,
 * sweep/selection and root evacuation remain unbounded legacy stopped work;
 * measuring them does not establish the generational latency contract. */
#ifdef KLASSIC_GC_LOG
static uint64_t g_stat_bytes;         /* total bytes handed out by gc_alloc */
static uint64_t g_stat_pause_ns_max;  /* longest rendezvous action (ns) */
static uint64_t g_stat_pause_ns_total; /* summed rendezvous action time (ns) */
static uint64_t g_stat_pauses;        /* number of STW handshakes */
static uint64_t g_stat_safepoint_ns_max, g_stat_safepoint_ns_total;
static uint64_t g_stat_stopped_ns_max, g_stat_stopped_ns_total;
static uint64_t g_stat_park_ns_max, g_stat_park_ns_total, g_stat_parks;

/* All duration writers hold hs_lock; exit reporting uses atomic reads. */
static void gc_stat_duration(uint64_t *total, uint64_t *maximum, uint64_t ns) {
    __atomic_add_fetch(total, ns, __ATOMIC_RELAXED);
    if (ns > __atomic_load_n(maximum, __ATOMIC_RELAXED))
        __atomic_store_n(maximum, ns, __ATOMIC_RELAXED);
}

#ifdef KLASSIC_GC_TEST_CLOCK
uint64_t gc_test_now_ns(void);
#define gc_now_ns gc_test_now_ns
#else

static uint64_t gc_now_ns(void) {
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
        perror("klassic gc: monotonic clock");
        exit(1);
    }
    return (uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec;
}
#endif

static void gc_log_atexit(void) {
    fprintf(stderr,
            "klassic gc: collections=%llu relocations=%llu bytes=%llu "
            "pauses=%llu pause_max=%lluns pause_total=%lluns "
            "safepoint_max=%lluns safepoint_total=%lluns "
            "stopped_max=%lluns stopped_total=%lluns "
            "parks=%llu park_max=%lluns park_total=%lluns\n",
            (unsigned long long)__atomic_load_n(&g_collections, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_relocations, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_stat_bytes, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_stat_pauses, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_stat_pause_ns_max, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_stat_pause_ns_total, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_stat_safepoint_ns_max, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_stat_safepoint_ns_total, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_stat_stopped_ns_max, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_stat_stopped_ns_total, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_stat_parks, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_stat_park_ns_max, __ATOMIC_RELAXED),
            (unsigned long long)__atomic_load_n(&g_stat_park_ns_total, __ATOMIC_RELAXED));
}
#endif

/* --- Incremental phase machine ----------------------------------- *
 * Idle -> MarkStart -> Mark (quanta) -> MarkEnd -> Relocate (quanta) ->
 * Idle. The background collector traces and relocates in quanta. Legacy
 * transition actions still scan roots and perform heap-wide stopped work. */
#define GC_PHASE_IDLE 0
#define GC_PHASE_MARK 1
#define GC_PHASE_RELOCATE 2
#define GC_MARK_QUANTUM 512   /* object openings plus pointer slots */
#define GC_RELOCATE_QUANTUM 8192 /* live bytes evacuated per relocate quantum */
static uint64_t g_phase = GC_PHASE_IDLE;
static uint64_t g_bytes_since_cycle;   /* drives the proactive cycle start */
/* Relocate walk cursor (resumed across quanta): the from-set region being
 * evacuated and the position within it. */
static uint64_t g_relocate_ridx;
static uint8_t *g_relocate_ptr;
/* Region-count snapshot taken in the RelocateStart pause. The concurrent
 * relocate walk bounds itself by this instead of the live g_committed (which
 * the mutator grows under g_gc_lock during the phase): any region committed
 * after selection is by construction not in the from-set, so stopping at the
 * snapshot is both correct and free of a racy read of g_committed. */
static uint64_t g_relocate_limit;

static int g_initialized;

/* --- Colored pointers -------------------------------------------- */
#define GC_COLOR_M0 (1ull << 60)
#define GC_COLOR_M1 (1ull << 61)
#define GC_COLOR_R (1ull << 62)
#define GC_COLOR_MASK (7ull << 60)

/* Mask cells (dso_local: defined here, referenced by the emitted fast
 * path). A stored pointer carries exactly one color bit -- the "good"
 * color at store time. `good` is R in Idle/Relocate and the current mark
 * color (M0 or M1, alternating) during Mark; `bad` is the two non-good
 * bits, so a load whose color is not current slow-paths. strip clears the
 * color. Normalizing every idle/relocate pointer to R means a single mark
 * color would let a stale pointer from two cycles ago fast-path wrongly;
 * alternating M0/M1 (with R between) closes that. */
uint64_t gc_good_color = GC_COLOR_R;
uint64_t gc_bad_mask = GC_COLOR_M0 | GC_COLOR_M1;
uint64_t gc_strip_mask = ~GC_COLOR_MASK;
static uint64_t g_mark_color = GC_COLOR_M0; /* alternates M0<->M1 each mark */

/* Safepoint handshake flag (dso_local: the emitted poll reads it). Raised by
 * the background GC thread at a phase transition; the mutator parks at its
 * next poll while the GC thread runs. */
uint64_t gc_handshake_requested = 0;

/* --- Background GC thread (true-zgc M4) --------------------------- *
 * A single background thread runs the collection concurrently with the
 * mutator. Locks, ordered to stay deadlock-free (a holder of g_gc_lock never
 * blocks on g_wake_lock/g_hs_lock, and the GC thread never holds g_gc_lock
 * while waiting for the mutator to park):
 *   g_gc_lock guards the mutable collector data raced between the GC thread
 *             and the mutator's barrier -- the worklist and (M4c) the
 *             to-space bump. Short critical sections only; the mutator takes
 *             it on the barrier SLOW path, never the fast path.
 *   g_wake_lock hands cycle requests to the GC thread and blocks the mutator
 *             until the cycle it asked for finishes.
 * Heap fields raced between the two threads are accessed atomically
 * (relaxed) so a concurrent trace/recolor and a mutator barrier load don't
 * race; mask/phase change only while the mutator is parked at a handshake. */
static pthread_mutex_t g_gc_lock = PTHREAD_MUTEX_INITIALIZER;
/* Phase-transition rendezvous: the GC thread parks EVERY registered mutator
 * here to flip masks / scan roots / sweep while none touches the heap.
 * Separate from g_gc_lock (a barrier holding g_gc_lock must still reach its
 * poll). g_hs_lock also guards the thread table (register/deregister), so a
 * mask/phase flip and a thread joining/leaving are serialized. */
static pthread_mutex_t g_hs_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_hs_cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t g_wake_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_wake_cond = PTHREAD_COND_INITIALIZER;  /* mutator -> GC */
/* Separate from the request event: a request signal must never be consumed by
 * a completion waiter while the collector itself remains asleep. */
static pthread_cond_t g_cycle_complete_cond = PTHREAD_COND_INITIALIZER;
/* The current legacy full collector uses only the Major lane. Young remains
 * separate in the shared ABI until the generational scheduler is activated. */
static CycleLedger g_cycles; /* request/start/completion, under g_wake_lock */
static pthread_t g_gc_thread;
static int g_gc_thread_started; /* set once in gc_init_once */
/* There is no teardown path: the GC thread is detached and runs until the
 * process exits, so g_gc_shutdown is never set (the gc_worker loop reads it to
 * keep a clean shutdown hook available for a future embedding API). */
static int g_gc_shutdown;
static void *gc_worker(void *arg);

/* Set the good color and derive the bad mask (the two other color bits). */
static void gc_set_good(uint64_t good) {
    gc_good_color = good;
    gc_bad_mask = GC_COLOR_MASK ^ good;
}

/* Check both header addition and alignment rounding before either can wrap. */
static uint64_t allocation_size(uint64_t payload) {
    if (payload > UINT64_MAX - 31) {
        fprintf(stderr, "klassic gc: invalid object size\n");
        exit(1);
    }
    return (payload + 31) & ~(uint64_t)15;
}

/* Raw slot APIs cannot attach: registration can park and invalidate slot. */
static void require_attached(void) {
    if (!g_self) {
        fprintf(stderr, "klassic gc: raw heap access requires an attached mutator\n");
        exit(1);
    }
    if (atomic_load_explicit(&g_self->context.state, memory_order_relaxed) != GC_RUNNING) {
        fprintf(stderr, "klassic gc: managed access requires a Running mutator\n");
        exit(1);
    }
}

static void require_context(MutatorContext *ctx, ThreadState state) {
    if (!g_self || ctx != &g_self->context ||
        atomic_load_explicit(&ctx->state, memory_order_relaxed) != state) {
        fprintf(stderr, "klassic gc: invalid thread lifecycle owner or state\n");
        exit(1);
    }
    if (atomic_load_explicit(&ctx->access_epoch, memory_order_relaxed) != 0) {
        fprintf(stderr, "klassic gc: thread transition with an active heap access guard\n");
        exit(1);
    }
}

static void context_transition_locked(MutatorContext *ctx, ThreadState desired) {
    uint64_t observed = atomic_load_explicit(&ctx->state, memory_order_acquire);
    if (gc_context_transition_locked(ctx, observed, desired) != GC_CONTEXT_OK) {
        fprintf(stderr, "klassic gc: invalid context state publication\n");
        exit(1);
    }
}

static void hs_step_locked(ThreadRegistration *registration, uint64_t operation) {
    uint64_t result = gc_handshake_step_locked(&g_handshake, registration, operation);
    if (result == GC_HS_OK || (operation == GC_HS_ACK && result == GC_HS_DUPLICATE)) return;
    if (result == GC_HS_EXHAUSTED) {
        fprintf(stderr, operation == GC_HS_BEGIN ?
                "klassic gc: handshake generation exhausted\n" :
                "klassic gc: thread identity exhausted\n");
    } else {
        fprintf(stderr, "klassic gc: invalid handshake bookkeeping\n");
    }
    exit(1);
}
static uint64_t block_size(uint64_t word0) { return word0 & ~(uint64_t)15u; }

/* Forwarding is encoded in word1: a not-yet-forwarded object holds its small
 * type tag there (all tags are heap addresses' worth below the reservation),
 * a forwarded one holds its to-space user pointer -- an address inside the
 * reservation. So a single CAS on word1 installs forwarding atomically (no
 * FWD flag bit, no two-word publish race), and word0 keeps size + mark bits
 * untouched. `gc_forwarded` distinguishes the two by the reservation range. */
static int gc_forwarded(uint64_t word1) {
    const uint8_t *p = (const uint8_t *)word1;
    /* Inclusive upper bound: a zero-payload object in the last qword of the
     * reservation has user pointer == base + RESERVE_BYTES. Tags (1/2/4) are
     * far below g_region_base, so this never misclassifies a tag. */
    return p >= g_region_base && p <= g_region_base + RESERVE_BYTES;
}

/* The heap-wide, once-only part of initialization: reserve the address space
 * and spawn the GC thread. Run through pthread_once so that when several
 * threads race to allocate for the first time (each triggering init), the
 * reservation and the GC-thread spawn happen exactly once and their writes are
 * published to every thread with a happens-before edge -- otherwise the init
 * stores race the first allocators' reads of g_region_base / g_committed /
 * g_free_head / g_budget. */
static pthread_once_t g_init_once = PTHREAD_ONCE_INIT;
static void gc_init_once(void) {
    void *base = mmap(NULL, RESERVE_BYTES, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (base == MAP_FAILED) {
        fprintf(stderr, "klassic gc: cannot reserve the heap\n");
        exit(1);
    }
    g_region_base = (uint8_t *)base;
    g_committed = 0;
    g_free_head = NULL;
    g_header_mark = KLASSIC_GC_HMARK1;
    __atomic_store_n(&g_collections, 0, __ATOMIC_RELAXED);
#ifdef KLASSIC_GC_LOG
    atexit(gc_log_atexit); /* report collection/pause stats when the program ends */
#endif
    __atomic_store_n(&g_initialized, 1, __ATOMIC_RELEASE);
    if (pthread_create(&g_gc_thread, NULL, gc_worker, NULL) != 0) {
        fprintf(stderr, "klassic gc: cannot start the GC thread\n");
        exit(1);
    }
    pthread_detach(g_gc_thread);
    g_gc_thread_started = 1;
}

void klassic_gc_init(void) {
    /* Registration runs the once-init and adds the caller to the table. */
    klassic_gc_register_thread();
}

static uint8_t *region_at(uint64_t index) {
    return g_region_base + (size_t)index * REGION_SIZE;
}
static uint64_t region_index(const uint8_t *p) {
    return (uint64_t)((p - g_region_base) >> REGION_SHIFT);
}

/* Park the calling thread for whatever rendezvous is in flight. Assumes
 * g_hs_lock is held. Counts the thread as parked for the current generation
 * and waits until the GC clears the request; if a *new* round starts before
 * this thread resumes, it re-counts for that round (the N-mutator form of the
 * single-mutator "re-assert parked on every wait" rule). On return no
 * handshake is pending, so the caller may touch the heap or leave the table. */
static void hs_park_locked(void) {
    require_context(&g_self->context, GC_RUNNING);
#ifdef KLASSIC_GC_LOG
    int parked = __atomic_load_n(&gc_handshake_requested, __ATOMIC_ACQUIRE) != 0;
    uint64_t park_start = parked ? gc_now_ns() : 0;
#endif
    while (__atomic_load_n(&gc_handshake_requested, __ATOMIC_ACQUIRE)) {
        hs_step_locked(&g_self->registration, GC_HS_ACK);
        GC_TEST_HOOK(GC_TEST_MUTATOR_PARKED, NULL);
        uint64_t my_gen = g_handshake.generation;
        pthread_cond_broadcast(&g_hs_cond); /* tell the GC we parked */
        while (__atomic_load_n(&gc_handshake_requested, __ATOMIC_ACQUIRE)
               && g_handshake.generation == my_gen) {
            pthread_cond_wait(&g_hs_cond, &g_hs_lock);
        }
    }
    hs_step_locked(&g_self->registration, GC_HS_RESUME);
#ifdef KLASSIC_GC_LOG
    if (parked) {
        gc_stat_duration(&g_stat_park_ns_total, &g_stat_park_ns_max, gc_now_ns() - park_start);
        __atomic_add_fetch(&g_stat_parks, 1, __ATOMIC_RELAXED);
    }
#endif
}

/* Register the calling thread as a mutator: claim a table slot and give it a
 * shadow stack. Idempotent per thread. The main thread is registered from
 * klassic_gc_init; extra threads auto-register on their first alloc/push (and
 * may also call this explicitly). Under g_hs_lock so it serializes with the
 * rendezvous: a joining thread waits out any in-flight handshake (it is not
 * yet counted, so it does not park) before it appears in the table, so the
 * GC's current round never waits on a thread it did not count. */
void klassic_gc_register_thread(void) {
    if (g_self) {
        require_attached();
        return;
    }
    /* A thread cannot run as a mutator before the heap exists, and it may
     * register (explicitly or via a first shadow push) before it ever
     * allocates, so ensure the once-init here rather than only in gc_alloc. */
    pthread_once(&g_init_once, gc_init_once);
    void ***shadow = (void ***)malloc((size_t)SHADOW_MAX * sizeof(void **));
    if (!shadow) {
        fprintf(stderr, "klassic gc: cannot allocate a shadow stack\n");
        exit(1);
    }
    pthread_mutex_lock(&g_hs_lock);
    while (__atomic_load_n(&gc_handshake_requested, __ATOMIC_ACQUIRE)) {
        GC_TEST_HOOK(GC_TEST_ATTACH_WAIT, NULL);
        pthread_cond_wait(&g_hs_cond, &g_hs_lock);
    }
    uint64_t i = 0;
    while (i < MAX_MUTATORS && g_mutators[i].in_use) {
        i++;
    }
    if (i == MAX_MUTATORS) {
        fprintf(stderr, "klassic gc: too many mutator threads\n");
        exit(1);
    }
    Mutator *m = &g_mutators[i];
    /* Slot is unpublished and hs_lock excludes all current table scanners.
     * No borrower may retain a context across detach. Initialize before making
     * the slot visible; do not treat legacy stack addresses as root segments. */
    m->context = (MutatorContext){0};
    m->context.abi_version = GC_ABI_VERSION;
    atomic_init(&m->context.state, GC_RUNNING);
    atomic_init(&m->context.adopted_phase, 0);
    atomic_init(&m->context.access_epoch, 0);
    atomic_init(&m->context.root_revision, 0);
    atomic_init(&m->context.satb.published, 0);
    atomic_init(&m->context.satb.consumed, 0);
    atomic_init(&m->context.remembered.published, 0);
    atomic_init(&m->context.remembered.consumed, 0);
    m->shadow = shadow;
    m->shadow_top = 0;
    m->in_use = 1;
    m->registration = (ThreadRegistration){ .context = (uint64_t)(uintptr_t)&m->context };
    hs_step_locked(&m->registration, GC_HS_REGISTER);
    if (i + 1 > g_mutator_hi) {
        g_mutator_hi = i + 1;
    }
    g_self = m;
    pthread_mutex_unlock(&g_hs_lock);
}

/* Deregister the calling thread on exit: flush its active region watermark,
 * free its shadow stack, release the table slot. Under g_hs_lock, and it
 * first parks for any in-flight rendezvous (the GC's current round still
 * counts this thread), then leaves the table once the request is clear, so
 * the count the next round snapshots no longer includes it. */
void klassic_gc_unregister_thread(void) {
    if (!g_self) {
        return;
    }
    pthread_mutex_lock(&g_hs_lock);
    hs_park_locked();
    Mutator *m = g_self;
    context_transition_locked(&m->context, GC_DETACHING);
    if (m->context.tlab_start) {
        g_region_top[region_index((uint8_t *)(uintptr_t)m->context.tlab_start)] = m->context.tlab_top;
    }
    free(m->shadow);
    m->shadow = NULL;
    m->shadow_top = 0;
    m->context.tlab_start = m->context.tlab_top = m->context.tlab_end = 0;
    m->in_use = 0;
    uint64_t hi = 0;
    for (uint64_t i = 0; i < MAX_MUTATORS; i++) {
        if (g_mutators[i].in_use) {
            hi = i + 1;
        }
    }
    g_mutator_hi = hi;
    hs_step_locked(&m->registration, GC_HS_DETACH);
    g_self = NULL;
    pthread_mutex_unlock(&g_hs_lock);
}

MutatorContext *gc_thread_current(void) {
    return g_self ? &g_self->context : NULL;
}

MutatorContext *gc_thread_attach(void) {
    klassic_gc_register_thread();
    return &g_self->context;
}

void gc_thread_detach(MutatorContext *ctx) {
    require_context(ctx, GC_RUNNING);
    klassic_gc_unregister_thread();
}

void gc_enter_native(MutatorContext *ctx) {
    require_context(ctx, GC_RUNNING);
    pthread_mutex_lock(&g_hs_lock);
    hs_park_locked();
    /* The mutex release publishes the legacy roots and private TLAB. They
     * remain immutable to this owner until leave_native acquires this lock. */
    hs_step_locked(&g_self->registration, GC_HS_ENTER_NATIVE);
    pthread_mutex_unlock(&g_hs_lock);
}

void gc_leave_native(MutatorContext *ctx) {
    require_context(ctx, GC_BLOCKED_NATIVE);
    pthread_mutex_lock(&g_hs_lock);
    /* This thread was excluded from expected, so must not increment parked.
     * Remaining BlockedNative across consecutive requests is also safe. */
    while (__atomic_load_n(&gc_handshake_requested, __ATOMIC_ACQUIRE)) {
        GC_TEST_HOOK(GC_TEST_LEAVE_NATIVE_WAIT, NULL);
        pthread_cond_wait(&g_hs_cond, &g_hs_lock);
    }
    hs_step_locked(&g_self->registration, GC_HS_LEAVE_NATIVE);
    pthread_mutex_unlock(&g_hs_lock);
}

/* Acquire a fresh, zeroed bump region: reuse from the free pool, else
 * commit the next tail region within the budget. Returns 0 on failure. */
static int acquire_region(void) {
    Mutator *m = g_self;
    /* Flush this thread's current region watermark before switching. */
    if (m->context.tlab_start) {
        g_region_top[region_index((uint8_t *)(uintptr_t)m->context.tlab_start)] = m->context.tlab_top;
    }
    uint8_t *base;
    if (g_free_head) {
        base = g_free_head;
        g_free_head = *(uint8_t **)base; /* pop */
        memset(base, 0, REGION_SIZE);    /* reused region: clear stale bytes */
    } else if (g_committed < g_budget) {
        base = region_at(g_committed);
        g_committed++;
        /* fresh mmap memory is already zero */
    } else {
        return 0;
    }
    m->context.tlab_start = (uint64_t)(uintptr_t)base;
    m->context.tlab_top = (uint64_t)(uintptr_t)base;
    m->context.tlab_end = (uint64_t)(uintptr_t)(base + REGION_SIZE);
    g_region_top[region_index(base)] = (uint64_t)(uintptr_t)base;
    /* A mutator bump region is never in the relocation set; clear the flag
     * defensively (symmetric with to_acquire) so no future path can let the
     * mutator bump into a phantom from-set region. */
    __atomic_store_n(&g_from_set[region_index(base)], 0, __ATOMIC_RELAXED);
    return 1;
}

/* Grow the soft budget (called under g_gc_lock). The store is atomic because
 * the allocation-pressure check in gc_alloc reads g_budget without the lock. */
static int grow_budget(void) {
    uint64_t b = g_budget;
    if (b >= RESERVE_REGIONS) {
        return 0;
    }
    b *= 2;
    if (b > RESERVE_REGIONS) {
        b = RESERVE_REGIONS;
    }
    __atomic_store_n(&g_budget, b, __ATOMIC_RELAXED);
    return 1;
}

static void gc_poll(void);            /* forward decl: park if handshake pending */
static void gc_request_background_cycle(void); /* coalesced pressure request */
static void gc_request_and_wait(void); /* forward decl: request + block for a cycle */

void *klassic_gc_alloc(uint64_t size, uint64_t type_tag) {
    /* Per-thread fast check: g_self is set only after this thread has both run
     * the once-init and registered, so a single test covers both. */
    if (!g_self) {
        klassic_gc_init(); /* pthread_once heap init + register this thread */
    }
    require_attached();
    uint64_t total = allocation_size(size);
    if (total > REGION_SIZE) {
        /* Large objects (contiguous region runs) are a later addition;
         * the foundation's test objects are small. */
        fprintf(stderr, "klassic gc: object larger than a region is unsupported\n");
        exit(1);
    }
#ifdef KLASSIC_GC_LOG
    __atomic_add_fetch(&g_stat_bytes, total, __ATOMIC_RELAXED);
#endif
    /* Park if the GC thread is waiting for a safepoint, then -- under memory
     * pressure -- ask it (asynchronously) for a cycle and keep running: the
     * mutator marks via its barrier and parks at polls while the GC thread
     * collects concurrently. */
    gc_poll();
    uint64_t since =
        __atomic_add_fetch(&g_bytes_since_cycle, total, __ATOMIC_RELAXED);
    uint64_t budget = __atomic_load_n(&g_budget, __ATOMIC_RELAXED);
    if (since >= (budget << REGION_SHIFT) / 2) {
        gc_request_background_cycle();
    }
    Mutator *m = g_self;
    for (;;) {
        /* This thread's bump region (TLAB) is private to it; the GC thread
         * reads its watermark only while the thread is parked at a handshake,
         * so the bump needs no lock. */
        if (m->context.tlab_start && m->context.tlab_end - m->context.tlab_top >= total) {
            uint8_t *block = (uint8_t *)(uintptr_t)m->context.tlab_top;
            m->context.tlab_top += total;
            *(uint64_t *)block = total;          /* word0 = size, flags clear */
            *(uint64_t *)(block + 8) = type_tag; /* word1 = type tag */
            /* Reused payload is not a valid pre-barrier input. Initialize all
             * pointer slots before publishing the black object to its caller. */
            if(type_tag>=KLASSIC_GC_POINTER_RECORD)memset(block+16,0,total-16);
            /* Allocate-black: an object born during Mark is live this cycle.
             * The block is still private to this thread, but the mark bit set
             * is atomic to match mark_visit. */
            if (__atomic_load_n(&g_phase, __ATOMIC_RELAXED) == GC_PHASE_MARK) {
                __atomic_fetch_or((uint64_t *)block, g_header_mark, __ATOMIC_RELAXED);
            }
            return block + 16;
        }
        /* Need a new region: the region table is shared with the GC thread. */
        pthread_mutex_lock(&g_gc_lock);
        int acquired = acquire_region();
        pthread_mutex_unlock(&g_gc_lock);
        if (acquired) {
            continue;
        }
        /* Budget exhausted: run a cycle to free regions, then grow. */
        gc_request_and_wait();
        pthread_mutex_lock(&g_gc_lock);
        acquired = acquire_region();
        pthread_mutex_unlock(&g_gc_lock);
        if (acquired) {
            continue;
        }
        pthread_mutex_lock(&g_gc_lock);
        acquired = grow_budget() && acquire_region();
        pthread_mutex_unlock(&g_gc_lock);
        if (acquired) {
            continue;
        }
        fprintf(stderr, "klassic gc: out of memory\n");
        exit(1);
    }
}

void klassic_gc_shadow_push(void **slot_addr) {
    if (!g_self) {
        klassic_gc_register_thread();
    }
    require_attached();
    Mutator *m = g_self;
    if (m->shadow_top >= SHADOW_MAX) {
        fprintf(stderr, "klassic gc: shadow stack overflow\n");
        exit(1);
    }
    m->shadow[m->shadow_top++] = slot_addr;
}

void klassic_gc_shadow_pop_n(uint64_t count) {
    require_attached();
    if (count > g_self->shadow_top) {
        fprintf(stderr, "klassic gc: shadow stack underflow\n");
        exit(1);
    }
    g_self->shadow_top -= count;
}

static void mark_visit(void *user);       /* forward decl */
static int in_from_set(uint64_t raw);     /* forward decl */
static uint64_t gc_evacuate_object(uint64_t user); /* forward decl */

/* The load barrier's out-of-line slow path (a bad-colored slot loaded).
 * Phase-dependent, and self-healing so later loads of the slot fast-path:
 *   Relocate: if the target is in the from-set, evacuate it on demand;
 *             else follow any forwarding (a ghost from the last cycle).
 *   Mark:     follow forwarding, then mark the target (load-barrier-driven
 *             incremental marking). This does not replace a generational
 *             SATB/store barrier.
 *   Idle:     follow forwarding (lazily remap the last cycle's ghosts).
 * In every phase the healed slot is recolored to the current good color. */
uint64_t klassic_gc_load_barrier_slow(uint64_t value, void **slot) {
    require_attached();
    uint64_t raw = value & gc_strip_mask;
    if (raw) {
        uint64_t *block = (uint64_t *)(raw - 16);
        /* Acquire-load word1: during a concurrent Relocate the GC thread (or
         * another mutator) may CAS-install forwarding into it; the acquire
         * pairs with the release-CAS in gc_evacuate_object. */
        uint64_t w1 = __atomic_load_n(&block[1], __ATOMIC_ACQUIRE);
        if (__atomic_load_n(&g_phase, __ATOMIC_RELAXED) == GC_PHASE_RELOCATE
            && in_from_set(raw) && !gc_forwarded(w1)) {
            raw = gc_evacuate_object(raw); /* evacuate on demand + follow */
        } else if (gc_forwarded(w1)) {
            raw = w1; /* follow forwarding */
        }
        GC_TEST_HOOK(GC_TEST_BEFORE_LOAD_HEAL, (void *)raw);
        /* Repair only the value that this read observed. A concurrent logical
         * store wins; the read still returns/marks its own resolved observation.
         * Retrying with CAS's updated expected would overwrite that store. */
        uint64_t expected=value;
        (void)__atomic_compare_exchange_n((uint64_t *)slot,&expected,
            raw|gc_good_color,0,__ATOMIC_RELAXED,__ATOMIC_RELAXED);
        if (__atomic_load_n(&g_phase, __ATOMIC_RELAXED) == GC_PHASE_MARK) {
            mark_visit((void *)raw);
        }
    }
    return raw;
}

void *klassic_gc_read(void **slot) {
    require_attached();
    /* NOT a safepoint -- deliberately. With a moving collector, `slot` was
     * computed by the caller from a heap pointer that is only valid until the
     * next safepoint; polling here (between the caller deriving `slot` and this
     * load using it) would let the GC relocate the underlying object and free
     * its region mid-call, making both the load and the self-heal below write
     * into reused memory. The emitted code has the same shape: its polls sit at
     * function entries and loop back-edges, and every heap address is re-derived
     * from a rooted slot after each poll, with the inline barrier fast path and
     * the slow-path call poll-free in between. Hand-written mutators poll via
     * klassic_gc_alloc or an explicit klassic_gc_safepoint() at their loop tops
     * and must re-derive heap pointers from rooted slots afterwards. */
    uint64_t value = __atomic_load_n((uint64_t *)slot, __ATOMIC_RELAXED);
    if (value & gc_bad_mask) {
        return (void *)klassic_gc_load_barrier_slow(value, slot);
    }
    return (void *)(value & gc_strip_mask);
}

/* Explicit safepoint for hand-written mutators (the C analogue of the polls
 * the backend emits at function entries and loop back-edges). Call it at loop
 * tops so a read-only loop still parks for the GC thread's handshakes -- and
 * treat every raw heap pointer as invalidated across it: re-derive them from
 * rooted (shadow-pushed) slots afterwards, because a rendezvous may have
 * relocated their targets and updated only the rooted slots. */
void klassic_gc_safepoint(void) {
    if (!g_self) {
        klassic_gc_register_thread();
    }
    require_attached();
    gc_poll();
}

void klassic_gc_write(void **slot, void *value) {
    require_attached();
    uint64_t raw=(uint64_t)value;
    if(__atomic_load_n(&g_phase,__ATOMIC_RELAXED)!=GC_PHASE_MARK) {
        __atomic_store_n((uint64_t *)slot,raw?(raw|gc_good_color):0,__ATOMIC_RELAXED);
        return;
    }
    /* Running owners cannot change phase without polling. This path does no
     * polling, so it can preserve both sides of the store in the current
     * mark epoch before publication. It is immediate mark assistance, not a
     * generational SATB buffer or a relocation source-access barrier. */
    if(raw) {
        uint64_t forwarded=__atomic_load_n((uint64_t *)(raw-8),__ATOMIC_ACQUIRE);
        if(gc_forwarded(forwarded))raw=forwarded;
        mark_visit((void *)raw); // includes children initialized into black owners
    }
    uint64_t desired=raw?(raw|gc_good_color):0;
    uint64_t previous=__atomic_load_n((uint64_t *)slot,__ATOMIC_RELAXED);
    do {
        mark_visit((void *)previous); // null clears still preserve the old edge
        GC_TEST_HOOK(GC_TEST_BEFORE_STORE_CAS,(void *)(previous&gc_strip_mask));
        // Failure returns the competing value. Protect it on the next attempt
        // before overwriting it; never turn this into an unconditional store.
    } while(!__atomic_compare_exchange_n((uint64_t *)slot,&previous,desired,0,
        __ATOMIC_RELAXED,__ATOMIC_RELAXED));
}

/* Worklist push/pop/empty under the data lock -- the worklist is raced
 * between the GC thread's trace and the mutator barrier's mark. Short
 * critical sections; no other lock is held across them, and gc_trace never
 * holds the lock while calling mark_visit (which re-takes it to push). */
static void worklist_push(void *user) {
    pthread_mutex_lock(&g_gc_lock);
    if (g_worklist_top >= WORKLIST_MAX) {
        fprintf(stderr, "klassic gc: mark worklist overflow\n");
        exit(1);
    }
    g_worklist[g_worklist_top++] = user;
    pthread_mutex_unlock(&g_gc_lock);
}
static void *worklist_pop(void) {
    void *user = NULL;
    pthread_mutex_lock(&g_gc_lock);
    if (g_worklist_top) {
        user = g_worklist[--g_worklist_top];
    }
    pthread_mutex_unlock(&g_gc_lock);
    return user;
}
/* Collector-only completion query; a popped partial object is still work. */
static int worklist_empty(void) {
    pthread_mutex_lock(&g_gc_lock);
    int empty = (g_worklist_top == 0 && !g_trace_cursor);
    pthread_mutex_unlock(&g_gc_lock);
    return empty;
}

/* Mark `user` if unmarked this cycle, pushing it for tracing. A pointer into
 * a ghost region (forwarded last cycle) is remapped to its to-space copy
 * first. Concurrent markers (mutator barriers + the GC thread) race on the
 * atomic mark bit; exactly the one that flips it takes ownership and pushes. */
static void mark_visit(void *user) {
    uint64_t raw = (uint64_t)user & gc_strip_mask; /* defensive: raw */
    if (!raw) {
        return;
    }
    uint64_t *block = (uint64_t *)(raw - 16);
    if (gc_forwarded(block[1])) {
        raw = block[1];
        block = (uint64_t *)(raw - 16);
    }
    uint64_t old = __atomic_fetch_or(&block[0], g_header_mark, __ATOMIC_RELAXED);
    if (old & g_header_mark) {
        return; /* already marked this cycle */
    }
    worklist_push((void *)raw);
}

/* Spend at most budget units: one for opening an object, one per field.
 * Null fields consume budget too; raw/empty objects consume their opening.
 * The collector keeps a partial payload cursor instead of requeueing its
 * already-marked owner or rescanning earlier fields. The completion query
 * includes this cursor even when the shared queue itself is empty. */
static void gc_trace(uint64_t budget) {
    while(budget) {
        if(!g_trace_cursor) {
            void *user=worklist_pop();
            if(!user)return;
            budget--;
            uint64_t *block=(uint64_t *)((uint8_t *)user-16);
            uint64_t tag=block[1];
            if(tag<KLASSIC_GC_POINTER_RECORD)continue;
            GC_TEST_HOOK(GC_TEST_BEFORE_HEADER_SCAN,user);
            uint64_t payload=block_size(__atomic_load_n(&block[0],__ATOMIC_RELAXED))-16;
            uint64_t slots=payload/8;
            uint64_t start=tag==KLASSIC_GC_POINTER_LIST?1:0;
            if(start>=slots)continue;
            g_trace_cursor=(void **)user+start;
            g_trace_end=(void **)user+slots;
            continue;
        }
        void **field=g_trace_cursor;
        budget--;
        uint64_t v=__atomic_load_n((uint64_t *)field,__ATOMIC_RELAXED);
        uint64_t raw=v&gc_strip_mask;
        if(raw) {
            uint64_t *fb=(uint64_t *)(raw-16);
            if(gc_forwarded(fb[1]))raw=fb[1];
            GC_TEST_HOOK(GC_TEST_BEFORE_TRACE_HEAL,(void *)raw);
            uint64_t expected=v;
            (void)__atomic_compare_exchange_n((uint64_t *)field,&expected,
                raw|gc_good_color,0,__ATOMIC_RELAXED,__ATOMIC_RELAXED);
            mark_visit((void *)raw);
        }
        g_trace_cursor++;
        if(g_trace_cursor==g_trace_end)g_trace_cursor=g_trace_end=NULL;
    }
}

/* Mark every registered mutator's roots. Called only inside a rendezvous
 * pause, so each parked thread's shadow stack is stable to walk. */
static void gc_mark_roots(void) {
    for (uint64_t t = 0; t < g_mutator_hi; t++) {
        Mutator *m = &g_mutators[t];
        if (!m->in_use) {
            continue;
        }
        for (uint64_t i = 0; i < m->shadow_top; i++) {
            mark_visit(*m->shadow[i]);
        }
    }
}

/* Is `base` any registered mutator's active TLAB region? Such a region is
 * spared by the sweep and excluded from the relocation set -- its owner is
 * still bumping into it. Called inside a pause, so the TLAB pointers are
 * stable. */
static int is_active_region(const uint8_t *base) {
    for (uint64_t t = 0; t < g_mutator_hi; t++) {
        if (g_mutators[t].in_use && g_mutators[t].context.tlab_start == (uint64_t)(uintptr_t)base) {
            return 1;
        }
    }
    return 0;
}

/* Flush every mutator's active-region watermark into g_region_top so the
 * sweep and relocate walks see the true fill. Called inside a pause. */
static void flush_all_tlabs(void) {
    for (uint64_t t = 0; t < g_mutator_hi; t++) {
        Mutator *m = &g_mutators[t];
        if (m->in_use && m->context.tlab_start) {
            g_region_top[region_index((uint8_t *)(uintptr_t)m->context.tlab_start)] =
                m->context.tlab_top;
        }
    }
}


/* Reclaim whole-dead regions and clear the stale (previous-cycle) mark bit
 * on every block. Clearing it on dead blocks too -- not just live ones --
 * is essential: the two mark bits alternate, so a dead block that kept its
 * old bit would be misread as live when that bit becomes current again two
 * cycles later (a resurrection/leak). */
static void gc_sweep(void) {
    __atomic_fetch_add(&g_collections, 1, __ATOMIC_RELAXED);
    flush_all_tlabs();
    uint64_t stale_clear = ~(uint64_t)(KLASSIC_GC_HMARK_BOTH ^ g_header_mark);
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        uint8_t *base = region_at(idx);
        uint8_t *top = (uint8_t *)(uintptr_t)g_region_top[idx];
        g_region_live[idx] = 0;
        if (top == base) {
            continue;
        }
        int any_live = 0;
        uint64_t live = 0;
        for (uint8_t *cur = base; cur < top;) {
            uint64_t *b = (uint64_t *)cur;
            uint64_t sz = block_size(b[0]);
            int is_live = (b[0] & g_header_mark) != 0;
            b[0] &= stale_clear; /* drop the stale bit on live AND dead blocks */
            if (is_live) {
                any_live = 1;
                live += sz;
            }
            cur += sz;
        }
        g_region_live[idx] = live;
        if (!any_live && !is_active_region(base)) {
            *(uint8_t **)base = g_free_head;
            g_free_head = base;
            g_region_top[idx] = (uint64_t)(uintptr_t)base;
            g_region_live[idx] = 0;
        }
    }
}

/* --- Incremental compacting relocation (moving GC) --------------- *
 * RelocateStart (an O(roots) pause at MarkEnd) selects the sparsest regions
 * as the from-set, flips the good color to R so every mark-colored pointer
 * slow-paths, and remaps the roots. Evacuation then runs in quanta from the
 * allocator (gc_relocate_step) alongside the mutator's own barrier, which
 * evacuates a from-set object on demand when the mutator loads a pointer to
 * it. A vacated object keeps its size in word0 and stashes its new user
 * pointer in word1 (installed by a single CAS, see gc_evacuate_object), so
 * the from-space stays linearly walkable; the fully-evacuated from-set
 * regions become ghosts, freed at the next MarkEnd once marking has remapped
 * every live pointer off them. The header operations are atomic (M2); a
 * background GC thread that drives this concurrently is M4
 * (docs/true-zgc-plan.md). */

/* Acquire a fresh zeroed region for the to-space bump (free pool first,
 * then a committed tail region), without disturbing the mutator's current
 * region. Grows the soft budget as a fallback -- the mutator allocates
 * during the concurrent Relocate phase and can race the free pool away, so
 * relocation must be able to grow the heap rather than deadlock. Returns 0
 * only on genuine reservation exhaustion. */
static int to_acquire(void) {
    uint8_t *base;
    if (g_free_head) {
        base = g_free_head;
        g_free_head = *(uint8_t **)base;
        memset(base, 0, REGION_SIZE);
    } else {
        if (g_committed >= g_budget) {
            grow_budget(); /* may fail at the reservation cap */
        }
        if (g_committed < g_budget) {
            base = region_at(g_committed);
            g_committed++;
        } else {
            return 0;
        }
    }
    g_to_base = base;
    g_to_top = base;
    g_to_end = base + REGION_SIZE;
    g_region_top[region_index(base)] = (uint64_t)(uintptr_t)base;
    /* A to-space region is a survivor destination, never from-space: mark it
     * so the fixup walk visits it and the free walk spares it, even for a
     * tail region committed after the selection loop already ran. */
    __atomic_store_n(&g_from_set[region_index(base)], 0, __ATOMIC_RELAXED);
    return 1;
}

/* Bump `total` bytes in to-space, switching to a fresh region when the
 * current one cannot fit the block. Returns NULL only when to_acquire cannot
 * even grow the budget -- i.e. the whole reservation is committed (true
 * out-of-memory). */
static uint8_t *to_bump(uint64_t total) {
    if (!g_to_base || (uint64_t)(g_to_end - g_to_top) < total) {
        if (g_to_base) {
            g_region_top[region_index(g_to_base)] = (uint64_t)(uintptr_t)g_to_top;
        }
        if (!to_acquire()) {
            return NULL;
        }
    }
    uint8_t *dst = g_to_top;
    g_to_top += total;
    return dst;
}

/* Is user pointer `raw` inside a region currently in the relocation set? */
static int in_from_set(uint64_t raw) {
    uintptr_t base = (uintptr_t)g_region_base;
    if (raw < base + 16 || raw > base + RESERVE_BYTES) {
        return 0;
    }
    /* A zero-payload user pointer may be one past its owning region (or
     * reservation). Ownership belongs to the header, never the next page. */
    const uint8_t *header = (const uint8_t *)(uintptr_t)(raw - 16);
    return __atomic_load_n(&g_from_set[region_index(header)], __ATOMIC_RELAXED);
}

/* Evacuate a from-set object to to-space unless already forwarded; return
 * its to-space user pointer. Copy-then-CAS: copy the object (which carries
 * its tag in word1), then atomically install the forwarding by CASing word1
 * from the tag to the new user pointer. First toucher wins; a loser abandons
 * its copy (un-bumping if it was the last allocation) and returns the
 * winner's pointer. Size stays in word0, so the from-space remains linearly
 * walkable throughout. */
static uint64_t gc_evacuate_object(uint64_t user) {
    uint64_t *block = (uint64_t *)(user - 16);
    uint64_t w1 = __atomic_load_n(&block[1], __ATOMIC_ACQUIRE);
    if (gc_forwarded(w1)) {
        return w1;
    }
    uint64_t sz = block_size(block[0]);
    /* Reserve the to-space slot under the data lock: the GC thread's
     * gc_relocate_step and any mutator's evacuate-on-demand bump the same
     * to-space during a concurrent Relocate. The copy and the forwarding CAS
     * run outside the lock (dst is private once reserved; the CAS is atomic). */
    pthread_mutex_lock(&g_gc_lock);
    uint8_t *dst = to_bump(sz);
    pthread_mutex_unlock(&g_gc_lock);
    if (!dst) {
        fprintf(stderr, "klassic gc: out of memory during relocation\n");
        exit(1);
    }
    /* Copy word0 (size|mark, stable during Relocate) and the immutable payload,
     * but set the destination's word1 from the tag we already loaded rather
     * than reading the source's word1 with memcpy: a concurrent evacuator (the
     * GC thread and a mutator can both reach the same from-object) may be
     * CAS-installing forwarding into the source's word1 right now, and a plain
     * memcpy read of it would race that atomic write. klassic heap objects are
     * immutable, so word0 and the payload are race-free to copy. dst is private
     * until the CAS publishes it, so its plain writes are safe. */
    uint64_t *d = (uint64_t *)dst;
    d[0] = block[0];
    d[1] = w1;
    if (sz > 16) {
        memcpy(dst + 16, (const uint8_t *)block + 16, sz - 16);
    }
    uint64_t newuser = (uint64_t)(uintptr_t)(dst + 16);
    uint64_t expected = w1; /* the tag we loaded */
    if (__atomic_compare_exchange_n(&block[1], &expected, newuser, 0,
                                    __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) {
        __atomic_fetch_add(&g_relocations, 1, __ATOMIC_RELAXED);
        return newuser;
    }
    /* Lost the race: reclaim the copy if it is still the top of to-space,
     * then take the winner's pointer (CAS wrote it into `expected`). */
    pthread_mutex_lock(&g_gc_lock);
    if (g_to_top == dst + sz) {
        g_to_top = dst;
    }
    pthread_mutex_unlock(&g_gc_lock);
    return expected;
}

/* Remap a raw root slot at RelocateStart: evacuate its target on demand if
 * still in the from-set, else follow any forwarding. Roots hold raw
 * pointers (strip defensively). */
static void relocate_root(void **slot) {
    uint64_t raw = (uint64_t)*slot & gc_strip_mask;
    if (!raw) {
        return;
    }
    uint64_t *block = (uint64_t *)(raw - 16);
    if (in_from_set(raw) && !gc_forwarded(block[1])) {
        raw = gc_evacuate_object(raw);
    } else if (gc_forwarded(block[1])) {
        raw = block[1];
    }
    *slot = (void *)raw;
}

/* RelocateStart (O(roots) pause): choose the sparsest regions as the
 * relocation set, flip good to R so every mark-colored pointer now
 * slow-paths, and remap the roots (evacuating their targets on demand).
 * The bulk of evacuation then runs in quanta; the from-set regions become
 * ghosts -- kept flagged in g_from_set -- freed at the next MarkEnd. If
 * nothing is worth relocating, return straight to Idle (good is still R). */
static void gc_relocate_start(void) {
    uint64_t free_regions = 0;
    for (uint8_t *p = g_free_head; p; p = *(uint8_t **)p) {
        free_regions++;
    }
    if (g_committed < g_budget) {
        free_regions += g_budget - g_committed;
    }
    /* A region under half live packs into at most one to-space region, so
     * capping the set at free_regions - 1 keeps a headroom region. That bounds
     * to-space itself, but the mutator also allocates during the concurrent
     * Relocate phase and competes for the same free pool, so to_acquire grows
     * the budget as a backstop rather than relying on this headroom alone. */
    uint64_t max_from = (free_regions > 1) ? free_regions - 1 : 0;
    uint64_t selected = 0;
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        __atomic_store_n(&g_from_set[idx], 0, __ATOMIC_RELAXED);
        if (selected >= max_from) {
            continue;
        }
        uint8_t *base = region_at(idx);
        if (is_active_region(base)) {
            continue;
        }
        uint64_t live = g_region_live[idx];
        if (live == 0 || live * 2 >= REGION_SIZE) {
            continue;
        }
        __atomic_store_n(&g_from_set[idx], 1, __ATOMIC_RELAXED);
        selected++;
    }
    gc_set_good(GC_COLOR_R);
    if (selected == 0) {
        __atomic_store_n(&g_phase, GC_PHASE_IDLE, __ATOMIC_RELAXED);
        __atomic_store_n(&g_bytes_since_cycle, 0, __ATOMIC_RELAXED);
        return;
    }
    g_to_base = g_to_top = g_to_end = NULL;
    __atomic_store_n(&g_phase, GC_PHASE_RELOCATE, __ATOMIC_RELAXED);
    for (uint64_t t = 0; t < g_mutator_hi; t++) {
        Mutator *m = &g_mutators[t];
        if (!m->in_use) {
            continue;
        }
        for (uint64_t i = 0; i < m->shadow_top; i++) {
            relocate_root(m->shadow[i]);
        }
    }
    g_relocate_ridx = 0;
    g_relocate_ptr = NULL;
    g_relocate_limit = g_committed; /* snapshot: from-set lives below this */
}

/* RelocateEnd: flush the last to-space region's watermark and return to
 * Idle. The from-set regions stay flagged (ghosts) and are freed at the
 * next MarkEnd. Flush under g_gc_lock: a mutator that raced the GC to
 * evacuate the very last from-object and lost the CAS un-bumps g_to_top
 * under the same lock, so reading g_to_base/g_to_top here without it is a
 * data race (harmless in practice -- an aligned pointer, worst case a stale
 * watermark leaving one dead hole -- but cheap to make correct). */
static void gc_relocate_end(void) {
    pthread_mutex_lock(&g_gc_lock);
    if (g_to_base) {
        g_region_top[region_index(g_to_base)] = (uint64_t)(uintptr_t)g_to_top;
    }
    pthread_mutex_unlock(&g_gc_lock);
    __atomic_store_n(&g_phase, GC_PHASE_IDLE, __ATOMIC_RELAXED);
    __atomic_store_n(&g_bytes_since_cycle, 0, __ATOMIC_RELAXED);
}

/* Evacuate up to `budget` live bytes from the from-set, resuming the linear
 * walk via the cursor. Objects the barrier already evacuated (forwarded) are
 * skipped; when the whole from-set is drained, finish the phase. */
static void gc_relocate_step(uint64_t budget) {
    for (;;) {
        if (!g_relocate_ptr) {
            while (g_relocate_ridx < g_relocate_limit
                   && !__atomic_load_n(&g_from_set[g_relocate_ridx],
                                       __ATOMIC_RELAXED)) {
                g_relocate_ridx++;
            }
            if (g_relocate_ridx >= g_relocate_limit) {
                gc_relocate_end();
                return;
            }
            g_relocate_ptr = region_at(g_relocate_ridx);
        }
        uint8_t *top = (uint8_t *)(uintptr_t)g_region_top[g_relocate_ridx];
        while (g_relocate_ptr < top) {
            uint64_t *b = (uint64_t *)g_relocate_ptr;
            uint64_t sz = block_size(b[0]);
            /* Acquire-load word1: a mutator's evacuate-on-demand may have
             * already forwarded this object; skip it if so. */
            uint64_t bw1 = __atomic_load_n(&b[1], __ATOMIC_ACQUIRE);
            if ((b[0] & g_header_mark) && !gc_forwarded(bw1)) {
                gc_evacuate_object((uint64_t)(uintptr_t)(g_relocate_ptr + 16));
                budget = (budget > sz) ? budget - sz : 0;
            }
            g_relocate_ptr += sz;
            if (budget == 0) {
                return; /* resume here next quantum */
            }
        }
        g_relocate_ridx++;
        g_relocate_ptr = NULL;
    }
}

/* MarkStart: flip the mark color (M0<->M1) and header parity, scan roots,
 * enter the Mark phase. The new bad mask catches the OLD good color | R,
 * so pointers left from the previous cycle slow-path and are re-marked
 * (incremental-update marking). */
static void gc_mark_start(void) {
    g_mark_color ^= (GC_COLOR_M0 | GC_COLOR_M1); /* alternate M0<->M1 */
    gc_set_good(g_mark_color);
    g_header_mark ^= KLASSIC_GC_HMARK_BOTH;
    g_worklist_top = 0;
    gc_mark_roots();
    __atomic_store_n(&g_phase, GC_PHASE_MARK, __ATOMIC_RELAXED);
}

/* MarkEnd commit: the caller has rescanned roots and proved the queue empty
 * with all mutators quiescent and the single marker between quanta. Only then
 * may ghosts be freed and selection begin. Sweep/selection/root evacuation
 * still contain unbounded legacy stopped work; there is no frontier drain. */
static void gc_mark_end(void) {
    /* Free last cycle's ghost regions: the just-finished mark remapped every
     * live pointer off them, so none is referenced. Freeing them before the
     * sweep also keeps the sweep from walking their forwarding words. */
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        if (__atomic_load_n(&g_from_set[idx], __ATOMIC_RELAXED)) {
            uint8_t *base = region_at(idx);
            *(uint8_t **)base = g_free_head;
            g_free_head = base;
            g_region_top[idx] = (uint64_t)(uintptr_t)base;
            g_region_live[idx] = 0;
            __atomic_store_n(&g_from_set[idx], 0, __ATOMIC_RELAXED);
        }
    }
    gc_sweep();
    gc_relocate_start();
}

/* Safepoint handshake: the mutator, at a poll, parks here until the GC thread
 * (which raised gc_handshake_requested and does the phase transition while
 * every mutator is stopped) releases it. Touches no heap while parked, so the
 * GC can flip masks / scan every thread's roots / sweep safely. */
void klassic_gc_handshake(void) {
    require_attached();
    pthread_mutex_lock(&g_hs_lock);
    hs_park_locked();
    pthread_mutex_unlock(&g_hs_lock);
}

/* Mutator safepoint poll (also emitted inline by the LLVM backend). */
static void gc_poll(void) {
    require_context(&g_self->context, GC_RUNNING);
    if (__atomic_load_n(&gc_handshake_requested, __ATOMIC_ACQUIRE)) {
        klassic_gc_handshake();
    }
}

/* GC thread: request quiescence from every Running owner. BlockedNative
 * owners already published stable roots/TLABs and are excluded from expected;
 * leave_native cannot resume them until this request completes. Snapshot and
 * all membership/native transitions share hs_lock, so expected cannot drift.
 * Run the legacy action only after all counted owners park. The GC thread
 * must not hold g_gc_lock here, or
 * a mutator blocked on g_gc_lock in a barrier could never reach its poll. */
/* Caller holds hs_lock. Splitting ownership from the operation also lets a
 * test force back-to-back requests before any waiting owner can resume. */
static void gc_rendezvous_locked(void (*action)(void)) {
    hs_step_locked(NULL, GC_HS_BEGIN);
    uint64_t expected = g_handshake.expected;
#ifdef KLASSIC_GC_LOG
    uint64_t requested_at = gc_now_ns();
#endif
    __atomic_store_n(&gc_handshake_requested, 1, __ATOMIC_RELEASE);
    while (g_handshake.acknowledged < expected) {
        pthread_cond_wait(&g_hs_cond, &g_hs_lock);
    }
    /* Every owner is parked or BlockedNative. The action remains unbounded. */
#ifdef KLASSIC_GC_LOG
    uint64_t quiescent_at = gc_now_ns();
#endif
    action();
#ifdef KLASSIC_GC_LOG
    gc_stat_duration(&g_stat_pause_ns_total, &g_stat_pause_ns_max, gc_now_ns() - quiescent_at);
    gc_stat_duration(&g_stat_safepoint_ns_total, &g_stat_safepoint_ns_max, quiescent_at - requested_at);
    __atomic_add_fetch(&g_stat_pauses, 1, __ATOMIC_RELAXED);
#endif
    hs_step_locked(NULL, GC_HS_END);
#ifdef KLASSIC_GC_LOG
    gc_stat_duration(&g_stat_stopped_ns_total, &g_stat_stopped_ns_max, gc_now_ns() - quiescent_at);
#endif
    __atomic_store_n(&gc_handshake_requested, 0, __ATOMIC_RELEASE);
    pthread_cond_broadcast(&g_hs_cond);
}
static void gc_rendezvous(void (*action)(void)) {
    pthread_mutex_lock(&g_hs_lock);
    gc_rendezvous_locked(action);
    pthread_mutex_unlock(&g_hs_lock);
}

/* A late barrier enqueue or root-rescan discovery invalidates termination.
 * Leave phase=Mark and resume all owners rather than tracing this frontier
 * while they are parked. In this legacy runtime the sole GC thread owns the
 * trace cursor; parked/native owners have completed their barrier pushes, so
 * there is no unaccounted producer between the empty check and commit. */
static void gc_finish_mark_action(void) {
    gc_mark_roots();
    if(!worklist_empty())return;
    gc_mark_end();
}

/* Concurrent mark retries preserve the same color/header epoch and retained
 * ghost pages. A fresh end rendezvous rescans roots each time. This removes
 * residual graph traversal from the end pause; roots, sweep, selection and
 * root evacuation still need migration to the generational bounded protocol. */
static void gc_run_concurrent_cycle(void) {
    gc_rendezvous(gc_mark_start);
    do {
        while (!worklist_empty()) {
            gc_trace(GC_MARK_QUANTUM);
        }
        GC_TEST_HOOK(GC_TEST_BEFORE_MARK_END,NULL);
        gc_rendezvous(gc_finish_mark_action);
    } while (__atomic_load_n(&g_phase, __ATOMIC_RELAXED) == GC_PHASE_MARK);
    while (__atomic_load_n(&g_phase, __ATOMIC_RELAXED) == GC_PHASE_RELOCATE) {
        gc_relocate_step(GC_RELOCATE_QUANTUM);
    }
}

static void cycle_require_ok(uint64_t result) {
    if(result==GC_CY_OK)return;
    fprintf(stderr,result==GC_CY_EXHAUSTED?"klassic gc: cycle identity exhausted\n":"klassic gc: invalid cycle bookkeeping\n");
    exit(1);
}

/* The background GC thread owns START/FINISH. Completion follows the entire
 * real collection, including concurrent relocation returning to Idle. */
static void *gc_worker(void *arg) {
    (void)arg;
    pthread_mutex_lock(&g_wake_lock);
    for (;;) {
        if(g_gc_shutdown)break;
        CycleTicket job={.kind=GC_CYCLE_MAJOR};
        uint64_t result=gc_cycle_step_locked(&g_cycles,&job,GC_CY_START);
        if(result==GC_CY_EMPTY){pthread_cond_wait(&g_wake_cond,&g_wake_lock);continue;}
        cycle_require_ok(result);
        pthread_mutex_unlock(&g_wake_lock);
        GC_TEST_HOOK(GC_TEST_CYCLE_STARTED,NULL);
        gc_run_concurrent_cycle();
        pthread_mutex_lock(&g_wake_lock);
        cycle_require_ok(gc_cycle_step_locked(&g_cycles,&job,GC_CY_FINISH));
        pthread_cond_broadcast(&g_cycle_complete_cond);
        // Pending next tickets are already recorded; waking completion waiters
        // cannot consume the collector's separate request notification.
    }
    pthread_mutex_unlock(&g_wake_lock);
    return NULL;
}

static CycleTicket gc_request_cycle(uint64_t kind) {
    // Do not mislabel the legacy full collector as a Young collection.
    if(kind!=GC_CYCLE_MAJOR){fprintf(stderr,"klassic gc: Young cycle runtime is not activated\n");exit(1);}
    CycleTicket ticket={.kind=kind};
    pthread_mutex_lock(&g_wake_lock);
    cycle_require_ok(gc_cycle_step_locked(&g_cycles,&ticket,GC_CY_REQUEST));
    pthread_cond_signal(&g_wake_cond);
    pthread_mutex_unlock(&g_wake_lock);
    return ticket;
}

/* Pressure hints do not queue endless follow-up cycles while one is running.
 * Explicit requests below always receive their own fresh-cycle obligation. */
static void gc_request_background_cycle(void) {
    pthread_mutex_lock(&g_wake_lock);
    if(g_cycles.started[GC_CYCLE_MAJOR]==g_cycles.completed[GC_CYCLE_MAJOR]&&
       g_cycles.requested[GC_CYCLE_MAJOR]==g_cycles.started[GC_CYCLE_MAJOR]) {
        CycleTicket ticket={.kind=GC_CYCLE_MAJOR};
        cycle_require_ok(gc_cycle_step_locked(&g_cycles,&ticket,GC_CY_REQUEST));
        pthread_cond_signal(&g_wake_cond);
    }
    pthread_mutex_unlock(&g_wake_lock);
}

static void gc_await_cycle(MutatorContext *ctx,CycleTicket ticket) {
    require_context(ctx,GC_RUNNING);
    pthread_mutex_lock(&g_wake_lock);
    uint64_t result=gc_cycle_step_locked(&g_cycles,&ticket,GC_CY_POLL);
    pthread_mutex_unlock(&g_wake_lock);
    if(result==GC_CY_OK)return;
    if(result!=GC_CY_WAIT)cycle_require_ok(result);
    // Publish the owner's roots/TLAB boundary and exclude it from handshakes
    // BEFORE sleeping. Do not acquire hs_lock while holding the request lock.
    gc_enter_native(ctx);
    GC_TEST_HOOK(GC_TEST_CYCLE_BEFORE_WAIT,NULL);
    pthread_mutex_lock(&g_wake_lock);
    for(;;){
        result=gc_cycle_step_locked(&g_cycles,&ticket,GC_CY_POLL);
        if(result!=GC_CY_WAIT)break;
        GC_TEST_HOOK(GC_TEST_CYCLE_WAIT,NULL);
        pthread_cond_wait(&g_cycle_complete_cond,&g_wake_lock);
    }
    pthread_mutex_unlock(&g_wake_lock);
    cycle_require_ok(result);
    GC_TEST_HOOK(GC_TEST_CYCLE_BEFORE_REENTRY,NULL);
    // Completion can coincide with another cycle's handshake. Controlled
    // reentry waits for it and restores Running before managed code resumes.
    gc_leave_native(ctx);
}

/* A running cycle cannot satisfy a request made after its START. */
static void gc_request_and_wait(void) {
    CycleTicket ticket=gc_request_cycle(GC_CYCLE_MAJOR);
    gc_await_cycle(&g_self->context,ticket);
}

/* Explicit full collection: request a cycle and block until it finishes. */
void klassic_gc_collect(void) {
    if (!__atomic_load_n(&g_initialized, __ATOMIC_ACQUIRE)) {
        return;
    }
    /* Waiting publishes the registered owner's boundary and enters native
     * blocking; controlled reentry restores it before returning to the caller. */
    if (!g_self) {
        klassic_gc_register_thread();
    }
    gc_request_and_wait();
}

uint64_t klassic_gc_collection_count(void) {
    return __atomic_load_n(&g_collections, __ATOMIC_RELAXED);
}
uint64_t klassic_gc_relocation_count(void) {
    /* Incremented atomically by evacuators on both threads during a concurrent
     * relocate, so read it atomically. */
    return __atomic_load_n(&g_relocations, __ATOMIC_RELAXED);
}

uint64_t klassic_gc_live_region_count(void) {
    if (!__atomic_load_n(&g_initialized, __ATOMIC_ACQUIRE)) {
        return 0;
    }
    /* Phase actions and detach publish watermarks under hs_lock; concurrent
     * region acquisition/to-space completion use gc_lock. This ordering is
     * shared with phase actions. Never poll or wait for a cycle under either
     * lock. Statistics observers need not be attached mutators. */
    pthread_mutex_lock(&g_hs_lock);
    pthread_mutex_lock(&g_gc_lock);
    uint64_t live = 0;
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        if ((uint8_t *)(uintptr_t)g_region_top[idx] != region_at(idx)) {
            live++;
        }
    }
    pthread_mutex_unlock(&g_gc_lock);
    pthread_mutex_unlock(&g_hs_lock);
    return live;
}
