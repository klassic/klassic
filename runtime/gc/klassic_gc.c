/* klassic_gc.c -- ZGC-style region collector in C: colored pointers + a
 * phase-dependent load barrier, incremental marking, and incremental
 * compacting relocation (evacuate-on-demand + in-object forwarding), all
 * driven in bounded quanta from the allocator. See klassic_gc.h for the
 * object layout, the color/phase model, and the migration context.
 */
#define _DEFAULT_SOURCE /* for MAP_ANONYMOUS under -std=c11 */

#include "klassic_gc.h"

#include <pthread.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

/* --- Region heap geometry ---------------------------------------- */
#define REGION_SHIFT 17
#define REGION_SIZE (1u << REGION_SHIFT) /* 128 KiB */
#define RESERVE_REGIONS 512
#define RESERVE_BYTES ((size_t)RESERVE_REGIONS * REGION_SIZE) /* 64 MiB */
#define SHADOW_MAX (1u << 20)
#define WORKLIST_MAX (1u << 18)

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

/* The current bump region. */
static uint8_t *g_heap_base;
static uint8_t *g_heap_top;
static uint8_t *g_heap_end;

/* A dedicated to-space bump for evacuation, separate from the mutator's
 * current region so compaction never disturbs in-progress allocation. */
static uint8_t *g_to_base;
static uint8_t *g_to_top;
static uint8_t *g_to_end;
static uint64_t g_relocations; /* objects evacuated (observability) */

/* Precise roots and the mark worklist. */
static void **g_shadow[SHADOW_MAX];
static uint64_t g_shadow_top;
static void *g_worklist[WORKLIST_MAX];
static uint64_t g_worklist_top;

/* The current-cycle header mark bit (alternates each collection). */
static uint64_t g_header_mark = KLASSIC_GC_HMARK1;
static uint64_t g_collections;

/* --- Incremental phase machine ----------------------------------- *
 * Idle -> MarkStart -> Mark (quanta) -> MarkEnd -> Relocate (quanta) ->
 * Idle. Mark and Relocate both run in bounded quanta driven from
 * gc_alloc, so pauses are O(roots) at the phase transitions, not
 * O(heap). */
#define GC_PHASE_IDLE 0
#define GC_PHASE_MARK 1
#define GC_PHASE_RELOCATE 2
#define GC_QUANTUM_POPS 512    /* objects traced per mark quantum */
#define GC_QUANTUM_BYTES 8192  /* allocation between quanta */
#define GC_RELOCATE_BYTES 8192 /* live bytes evacuated per relocate quantum */
static uint64_t g_phase = GC_PHASE_IDLE;
static uint64_t g_bytes_since_cycle;   /* drives the proactive cycle start */
static uint64_t g_bytes_since_quantum; /* drives a mark/relocate quantum */
/* Relocate walk cursor (resumed across quanta): the from-set region being
 * evacuated and the position within it. */
static uint64_t g_relocate_ridx;
static uint8_t *g_relocate_ptr;

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
/* Phase-transition rendezvous: the GC thread parks the mutator here to flip
 * masks / scan its roots / sweep while it touches no heap. Separate from
 * g_gc_lock (a barrier holding g_gc_lock must still reach its poll). */
static pthread_mutex_t g_hs_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_hs_cond = PTHREAD_COND_INITIALIZER;
static int g_hs_parked; /* mutator is parked in the handshake (under g_hs_lock) */
static pthread_mutex_t g_wake_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_wake_cond = PTHREAD_COND_INITIALIZER;  /* mutator -> GC */
static pthread_cond_t g_idle_cond = PTHREAD_COND_INITIALIZER;  /* GC -> mutator */
static int g_cycle_request;   /* a cycle has been asked for (under g_wake_lock) */
static int g_cycle_running;   /* a cycle is in progress (under g_wake_lock) */
static uint64_t g_cycle_done; /* completed-cycle counter, for wait predicates */
static pthread_t g_gc_thread;
static int g_gc_thread_started;
static int g_gc_shutdown;
static void *gc_worker(void *arg);

/* Set the good color and derive the bad mask (the two other color bits). */
static void gc_set_good(uint64_t good) {
    gc_good_color = good;
    gc_bad_mask = GC_COLOR_MASK ^ good;
}

static uint64_t align16(uint64_t n) { return (n + 15u) & ~(uint64_t)15u; }
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

void klassic_gc_init(void) {
    if (g_initialized) {
        return;
    }
    void *base = mmap(NULL, RESERVE_BYTES, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (base == MAP_FAILED) {
        fprintf(stderr, "klassic gc: cannot reserve the heap\n");
        exit(1);
    }
    g_region_base = (uint8_t *)base;
    g_committed = 0;
    g_free_head = NULL;
    g_heap_base = g_heap_top = g_heap_end = NULL;
    g_shadow_top = 0;
    g_header_mark = KLASSIC_GC_HMARK1;
    g_collections = 0;
    g_initialized = 1;
    /* Spawn the background GC thread that runs collections. */
    if (!g_gc_thread_started) {
        if (pthread_create(&g_gc_thread, NULL, gc_worker, NULL) != 0) {
            fprintf(stderr, "klassic gc: cannot start the GC thread\n");
            exit(1);
        }
        pthread_detach(g_gc_thread);
        g_gc_thread_started = 1;
    }
}

static uint8_t *region_at(uint64_t index) {
    return g_region_base + (size_t)index * REGION_SIZE;
}
static uint64_t region_index(const uint8_t *p) {
    return (uint64_t)((p - g_region_base) >> REGION_SHIFT);
}

/* Acquire a fresh, zeroed bump region: reuse from the free pool, else
 * commit the next tail region within the budget. Returns 0 on failure. */
static int acquire_region(void) {
    /* Flush the current region's watermark before switching. */
    if (g_heap_base) {
        g_region_top[region_index(g_heap_base)] = (uint64_t)(uintptr_t)g_heap_top;
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
    g_heap_base = base;
    g_heap_top = base;
    g_heap_end = base + REGION_SIZE;
    g_region_top[region_index(base)] = (uint64_t)(uintptr_t)base;
    /* A mutator bump region is never in the relocation set; clear the flag
     * defensively (symmetric with to_acquire) so no future path can let the
     * mutator bump into a phantom from-set region. */
    g_from_set[region_index(base)] = 0;
    return 1;
}

static int grow_budget(void) {
    if (g_budget >= RESERVE_REGIONS) {
        return 0;
    }
    g_budget *= 2;
    if (g_budget > RESERVE_REGIONS) {
        g_budget = RESERVE_REGIONS;
    }
    return 1;
}

static void gc_poll(void);            /* forward decl: park if handshake pending */
static void gc_request_cycle(void);   /* forward decl: async cycle request */
static void gc_request_and_wait(void); /* forward decl: request + block for a cycle */

void *klassic_gc_alloc(uint64_t size, uint64_t type_tag) {
    if (!g_initialized) {
        klassic_gc_init();
    }
    uint64_t total = align16(size + 16);
    if (total > REGION_SIZE) {
        /* Large objects (contiguous region runs) are a later addition;
         * the foundation's test objects are small. */
        fprintf(stderr, "klassic gc: object larger than a region is unsupported\n");
        exit(1);
    }
    /* Park if the GC thread is waiting for a safepoint, then -- under memory
     * pressure -- ask it (asynchronously) for a cycle and keep running: the
     * mutator marks via its barrier and parks at polls while the GC thread
     * collects concurrently. */
    gc_poll();
    uint64_t since =
        __atomic_add_fetch(&g_bytes_since_cycle, total, __ATOMIC_RELAXED);
    if (since >= (g_budget << REGION_SHIFT) / 2) {
        gc_request_cycle();
    }
    for (;;) {
        /* The current bump region is owned by this (mutator) thread; the GC
         * thread reads its watermark only while the mutator is parked at a
         * handshake, so the bump needs no lock. */
        if (g_heap_base && (uint64_t)(g_heap_end - g_heap_top) >= total) {
            uint8_t *block = g_heap_top;
            g_heap_top += total;
            *(uint64_t *)block = total;          /* word0 = size, flags clear */
            *(uint64_t *)(block + 8) = type_tag; /* word1 = type tag */
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
    if (g_shadow_top >= SHADOW_MAX) {
        fprintf(stderr, "klassic gc: shadow stack overflow\n");
        exit(1);
    }
    g_shadow[g_shadow_top++] = slot_addr;
}

void klassic_gc_shadow_pop_n(uint64_t count) {
    g_shadow_top -= count;
}

static void mark_visit(void *user);       /* forward decl */
static int in_from_set(uint64_t raw);     /* forward decl */
static uint64_t gc_evacuate_object(uint64_t user); /* forward decl */

/* The load barrier's out-of-line slow path (a bad-colored slot loaded).
 * Phase-dependent, and self-healing so later loads of the slot fast-path:
 *   Relocate: if the target is in the from-set, evacuate it on demand;
 *             else follow any forwarding (a ghost from the last cycle).
 *   Mark:     follow forwarding, then mark the target (load-barrier-driven
 *             incremental marking -- no store barrier, klassic has no
 *             in-place heap mutation).
 *   Idle:     follow forwarding (lazily remap the last cycle's ghosts).
 * In every phase the healed slot is recolored to the current good color. */
uint64_t klassic_gc_load_barrier_slow(uint64_t value, void **slot) {
    uint64_t raw = value & gc_strip_mask;
    if (raw) {
        uint64_t *block = (uint64_t *)(raw - 16);
        if (__atomic_load_n(&g_phase, __ATOMIC_RELAXED) == GC_PHASE_RELOCATE
            && in_from_set(raw) && !gc_forwarded(block[1])) {
            raw = gc_evacuate_object(raw); /* evacuate on demand + follow */
        } else if (gc_forwarded(block[1])) {
            raw = block[1]; /* follow forwarding */
        }
        /* Atomic self-heal: the GC thread may recolor the same field
         * concurrently during a concurrent mark. */
        __atomic_store_n((uint64_t *)slot, raw | gc_good_color, __ATOMIC_RELAXED);
        if (__atomic_load_n(&g_phase, __ATOMIC_RELAXED) == GC_PHASE_MARK) {
            mark_visit((void *)raw);
        }
    }
    return raw;
}

void *klassic_gc_read(void **slot) {
    /* Safepoint: the compiled backend polls at loop back-edges (M3), so a
     * read-only loop parks for the GC thread's handshakes even when it never
     * allocates. This C entry point (used by the runtime and the unit tests)
     * polls here so a hand-written read loop behaves the same. */
    gc_poll();
    uint64_t value = __atomic_load_n((uint64_t *)slot, __ATOMIC_RELAXED);
    if (value & gc_bad_mask) {
        return (void *)klassic_gc_load_barrier_slow(value, slot);
    }
    return (void *)(value & gc_strip_mask);
}

void klassic_gc_write(void **slot, void *value) {
    uint64_t raw = (uint64_t)value;
    /* Atomic store: a concurrent trace may recolor this field. */
    __atomic_store_n((uint64_t *)slot, raw ? (raw | gc_good_color) : 0,
                     __ATOMIC_RELAXED);
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
static int worklist_empty(void) {
    pthread_mutex_lock(&g_gc_lock);
    int empty = (g_worklist_top == 0);
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

/* Trace up to `budget` worklist objects (a bounded mark quantum). Field
 * loads/stores are atomic (relaxed) because the mutator's barrier may load or
 * self-heal the same field concurrently during a concurrent mark. */
static void gc_trace(uint64_t budget) {
    void *user;
    while (budget && (user = worklist_pop()) != NULL) {
        budget--;
        uint64_t *block = (uint64_t *)((uint8_t *)user - 16);
        uint64_t tag = block[1];
        if (tag < KLASSIC_GC_POINTER_RECORD) {
            continue; /* raw bytes: no pointer payload */
        }
        uint64_t payload = block_size(block[0]) - 16;
        void **fields = (void **)user;
        uint64_t slots = payload / 8;
        uint64_t start = (tag == KLASSIC_GC_POINTER_LIST) ? 1 : 0; /* skip length */
        for (uint64_t i = start; i < slots; i++) {
            uint64_t v = __atomic_load_n((uint64_t *)&fields[i], __ATOMIC_RELAXED);
            uint64_t raw = v & gc_strip_mask;
            if (raw) {
                uint64_t *fb = (uint64_t *)(raw - 16);
                if (gc_forwarded(fb[1])) {
                    raw = fb[1]; /* remap a ghost pointer to to-space */
                }
                /* Recolor-on-trace so a later barriered load fast-paths and no
                 * live field is left referencing a ghost. */
                __atomic_store_n((uint64_t *)&fields[i], raw | gc_good_color,
                                 __ATOMIC_RELAXED);
                mark_visit((void *)raw);
            }
        }
    }
}

/* Drain the worklist to fixpoint (used on the synchronous / parked path). */
static void gc_drain(void) {
    while (!worklist_empty()) {
        gc_trace(GC_QUANTUM_POPS);
    }
}

static void gc_mark_roots(void) {
    for (uint64_t i = 0; i < g_shadow_top; i++) {
        mark_visit(*g_shadow[i]);
    }
}

/* Reclaim whole-dead regions and clear the stale (previous-cycle) mark bit
 * on every block. Clearing it on dead blocks too -- not just live ones --
 * is essential: the two mark bits alternate, so a dead block that kept its
 * old bit would be misread as live when that bit becomes current again two
 * cycles later (a resurrection/leak). */
static void gc_sweep(void) {
    g_collections++;
    if (g_heap_base) {
        g_region_top[region_index(g_heap_base)] = (uint64_t)(uintptr_t)g_heap_top;
    }
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
        if (!any_live && base != g_heap_base) {
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
    g_from_set[region_index(base)] = 0;
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
    const uint8_t *p = (const uint8_t *)raw;
    if (p < g_region_base || p >= g_region_base + RESERVE_BYTES) {
        return 0;
    }
    return g_from_set[region_index(p)];
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
    uint8_t *dst = to_bump(sz);
    if (!dst) {
        fprintf(stderr, "klassic gc: out of memory during relocation\n");
        exit(1);
    }
    memcpy(dst, block, sz);
    uint64_t newuser = (uint64_t)(uintptr_t)(dst + 16);
    uint64_t expected = w1; /* the tag we loaded */
    if (__atomic_compare_exchange_n(&block[1], &expected, newuser, 0,
                                    __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) {
        g_relocations++;
        return newuser;
    }
    /* Lost the race: reclaim the copy if it is still the top of to-space,
     * then take the winner's pointer (CAS wrote it into `expected`). */
    if (g_to_top == dst + sz) {
        g_to_top = dst;
    }
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
        g_from_set[idx] = 0;
        if (selected >= max_from) {
            continue;
        }
        uint8_t *base = region_at(idx);
        if (base == g_heap_base) {
            continue;
        }
        uint64_t live = g_region_live[idx];
        if (live == 0 || live * 2 >= REGION_SIZE) {
            continue;
        }
        g_from_set[idx] = 1;
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
    for (uint64_t i = 0; i < g_shadow_top; i++) {
        relocate_root(g_shadow[i]);
    }
    g_relocate_ridx = 0;
    g_relocate_ptr = NULL;
    g_bytes_since_quantum = 0;
}

/* RelocateEnd: flush the last to-space region's watermark and return to
 * Idle. The from-set regions stay flagged (ghosts) and are freed at the
 * next MarkEnd. */
static void gc_relocate_end(void) {
    if (g_to_base) {
        g_region_top[region_index(g_to_base)] = (uint64_t)(uintptr_t)g_to_top;
    }
    __atomic_store_n(&g_phase, GC_PHASE_IDLE, __ATOMIC_RELAXED);
    __atomic_store_n(&g_bytes_since_cycle, 0, __ATOMIC_RELAXED);
}

/* Evacuate up to `budget` live bytes from the from-set, resuming the linear
 * walk via the cursor. Objects the barrier already evacuated (forwarded) are
 * skipped; when the whole from-set is drained, finish the phase. */
static void gc_relocate_step(uint64_t budget) {
    for (;;) {
        if (!g_relocate_ptr) {
            while (g_relocate_ridx < g_committed && !g_from_set[g_relocate_ridx]) {
                g_relocate_ridx++;
            }
            if (g_relocate_ridx >= g_committed) {
                gc_relocate_end();
                return;
            }
            g_relocate_ptr = region_at(g_relocate_ridx);
        }
        uint8_t *top = (uint8_t *)(uintptr_t)g_region_top[g_relocate_ridx];
        while (g_relocate_ptr < top) {
            uint64_t *b = (uint64_t *)g_relocate_ptr;
            uint64_t sz = block_size(b[0]);
            if ((b[0] & g_header_mark) && !gc_forwarded(b[1])) {
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

/* Drive relocation to completion (synchronous collect). */
static void gc_relocate_drain(void) {
    while (g_phase == GC_PHASE_RELOCATE) {
        gc_relocate_step(REGION_SIZE);
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
    g_bytes_since_quantum = 0;
}

/* MarkEnd (O(roots) pause): drain the mark frontier to fixpoint -- which
 * remaps every live pointer off the last cycle's ghosts -- free those
 * ghosts, sweep, and start relocation. */
static void gc_mark_end(void) {
    gc_drain();
    /* Free last cycle's ghost regions: the just-finished mark remapped every
     * live pointer off them, so none is referenced. Freeing them before the
     * sweep also keeps the sweep from walking their forwarding words. */
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        if (g_from_set[idx]) {
            uint8_t *base = region_at(idx);
            *(uint8_t **)base = g_free_head;
            g_free_head = base;
            g_region_top[idx] = (uint64_t)(uintptr_t)base;
            g_region_live[idx] = 0;
            g_from_set[idx] = 0;
        }
    }
    gc_sweep();
    gc_relocate_start();
}

/* Safepoint handshake: the mutator, at a poll, parks here until the GC thread
 * (which raised gc_handshake_requested and does the phase transition while the
 * mutator is stopped) releases it. The mutator touches no heap while parked,
 * so the GC thread can flip masks / scan its roots / sweep safely. */
void klassic_gc_handshake(void) {
    pthread_mutex_lock(&g_hs_lock);
    /* Re-assert "parked" on every wait, not just once. Consecutive rendezvous
     * reuse g_hs_parked (the GC clears it at the end of each), so if this
     * mutator wakes from one rendezvous to find a *new* request already
     * pending, it must announce itself again -- otherwise the GC's next
     * rendezvous waits forever for a parked flag that a single up-front
     * assignment cleared. Publishing inside the loop makes the flag current
     * for whichever request we are actually waiting on. */
    while (__atomic_load_n(&gc_handshake_requested, __ATOMIC_ACQUIRE)) {
        g_hs_parked = 1;
        pthread_cond_broadcast(&g_hs_cond); /* tell the GC thread we are parked */
        pthread_cond_wait(&g_hs_cond, &g_hs_lock);
    }
    pthread_mutex_unlock(&g_hs_lock);
}

/* Mutator safepoint poll (also emitted inline by the LLVM backend). */
static void gc_poll(void) {
    if (__atomic_load_n(&gc_handshake_requested, __ATOMIC_ACQUIRE)) {
        klassic_gc_handshake();
    }
}

/* GC thread: raise the handshake, wait for the mutator to park, run `action`
 * (a phase transition -- safe while the mutator is stopped), release. The GC
 * thread must not hold g_gc_lock here, or a mutator blocked on g_gc_lock in a
 * barrier could never reach its poll to park. */
static void gc_rendezvous(void (*action)(void)) {
    pthread_mutex_lock(&g_hs_lock);
    __atomic_store_n(&gc_handshake_requested, 1, __ATOMIC_RELEASE);
    while (!g_hs_parked) {
        pthread_cond_wait(&g_hs_cond, &g_hs_lock);
    }
    action();
    g_hs_parked = 0;
    __atomic_store_n(&gc_handshake_requested, 0, __ATOMIC_RELEASE);
    pthread_cond_broadcast(&g_hs_cond);
    pthread_mutex_unlock(&g_hs_lock);
}

/* The MarkEnd + relocate transition, run in one rendezvous pause. M4b keeps
 * relocation stop-the-world (mutator parked); M4c makes it concurrent. */
static void gc_finish_cycle_action(void) {
    gc_mark_roots();     /* re-scan roots: catch anything grayed since MarkStart */
    gc_mark_end();       /* drain to fixpoint + free ghosts + sweep + relocate_start */
    gc_relocate_drain(); /* finish relocation (STW for M4b) */
}

/* One concurrent collection cycle (M4b): a MarkStart pause flips to Mark and
 * scans roots; the mutator then runs while the GC thread drains the worklist
 * its barriers feed; a MarkEnd+relocate pause finishes the cycle. */
static void gc_run_concurrent_cycle(void) {
    gc_rendezvous(gc_mark_start); /* park: flip masks, scan roots, phase=MARK */
    while (!worklist_empty()) {
        gc_trace(GC_QUANTUM_POPS); /* concurrent mark: mutator running */
    }
    gc_rendezvous(gc_finish_cycle_action); /* park: re-scan + drain + sweep + relocate */
}

/* The background GC thread: sleep until a cycle is requested, run it. */
static void *gc_worker(void *arg) {
    (void)arg;
    pthread_mutex_lock(&g_wake_lock);
    for (;;) {
        while (!g_cycle_request && !g_gc_shutdown) {
            pthread_cond_wait(&g_wake_cond, &g_wake_lock);
        }
        if (g_gc_shutdown) {
            break;
        }
        g_cycle_request = 0;
        g_cycle_running = 1;
        pthread_mutex_unlock(&g_wake_lock);
        gc_run_concurrent_cycle();
        pthread_mutex_lock(&g_wake_lock);
        g_cycle_running = 0;
        g_cycle_done++;
        pthread_cond_broadcast(&g_idle_cond);
    }
    pthread_mutex_unlock(&g_wake_lock);
    return NULL;
}

/* Mutator: ask for a cycle without waiting (it keeps running and participates
 * via its safepoint polls). */
static void gc_request_cycle(void) {
    pthread_mutex_lock(&g_wake_lock);
    if (!g_cycle_request && !g_cycle_running) {
        g_cycle_request = 1;
        pthread_cond_signal(&g_wake_cond);
    }
    pthread_mutex_unlock(&g_wake_lock);
}

/* Mutator: request a cycle and block until one completes, polling meanwhile so
 * it can park for the GC thread's handshakes (which is how the cycle makes
 * progress). Used for explicit collect and heap-exhaustion fallback. */
static void gc_request_and_wait(void) {
    pthread_mutex_lock(&g_wake_lock);
    uint64_t target = g_cycle_done + 1;
    if (!g_cycle_request && !g_cycle_running) {
        g_cycle_request = 1;
        pthread_cond_signal(&g_wake_cond);
    }
    while (g_cycle_done < target) {
        pthread_mutex_unlock(&g_wake_lock);
        gc_poll();
        sched_yield();
        pthread_mutex_lock(&g_wake_lock);
    }
    pthread_mutex_unlock(&g_wake_lock);
}

/* Explicit full collection: request a cycle and block until it finishes. */
void klassic_gc_collect(void) {
    if (!g_initialized) {
        return;
    }
    gc_request_and_wait();
}

uint64_t klassic_gc_collection_count(void) { return g_collections; }
uint64_t klassic_gc_relocation_count(void) { return g_relocations; }

uint64_t klassic_gc_live_region_count(void) {
    uint64_t live = 0;
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        if ((uint8_t *)(uintptr_t)g_region_top[idx] != region_at(idx)) {
            live++;
        }
    }
    return live;
}
