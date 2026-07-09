/* klassic_gc.c -- ZGC-style region collector in C: colored pointers + a
 * phase-dependent load barrier, incremental marking, and incremental
 * compacting relocation (evacuate-on-demand + in-object forwarding), all
 * driven in bounded quanta from the allocator. See klassic_gc.h for the
 * object layout, the color/phase model, and the migration context.
 */
#define _DEFAULT_SOURCE /* for MAP_ANONYMOUS under -std=c11 */

#include "klassic_gc.h"

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
 * the background GC thread (M4) at a phase transition; each mutator acks at
 * its next poll. Never set until the GC thread lands, so polls are no-ops. */
uint64_t gc_handshake_requested = 0;

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

static void gc_driver(uint64_t total); /* forward decl: incremental driver */

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
    /* Drive the phase machine BEFORE the bump, so it observes only the
     * mutator's already-rooted state (the object about to be allocated is
     * not yet reachable from a root). None of the routines it calls
     * allocate, so gc_alloc is not re-entered. */
    gc_driver(total);
    for (;;) {
        if (g_heap_base && (uint64_t)(g_heap_end - g_heap_top) >= total) {
            uint8_t *block = g_heap_top;
            g_heap_top += total;
            *(uint64_t *)block = total;          /* word0 = size, flags clear */
            *(uint64_t *)(block + 8) = type_tag; /* word1 = type tag */
            /* Allocate-black: an object born during Mark is live this
             * cycle, so the in-progress mark does not reclaim it. (Atomic OR
             * for uniformity with mark_visit, though the block is still
             * private to this thread here.) */
            if (g_phase == GC_PHASE_MARK) {
                __atomic_fetch_or((uint64_t *)block, g_header_mark, __ATOMIC_RELAXED);
            }
            return block + 16;
        }
        if (acquire_region()) {
            continue;
        }
        /* Budget exhausted: collect, then (if that frees nothing) grow. */
        klassic_gc_collect();
        if (acquire_region()) {
            continue;
        }
        if (grow_budget() && acquire_region()) {
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
        if (g_phase == GC_PHASE_RELOCATE && in_from_set(raw)
            && !gc_forwarded(block[1])) {
            raw = gc_evacuate_object(raw); /* evacuate on demand + follow */
        } else if (gc_forwarded(block[1])) {
            raw = block[1]; /* follow forwarding */
        }
        *slot = (void *)(raw | gc_good_color);
        if (g_phase == GC_PHASE_MARK) {
            mark_visit((void *)raw);
        }
    }
    return raw;
}

void *klassic_gc_read(void **slot) {
    uint64_t value = (uint64_t)*slot;
    if (value & gc_bad_mask) {
        return (void *)klassic_gc_load_barrier_slow(value, slot);
    }
    return (void *)(value & gc_strip_mask);
}

void klassic_gc_write(void **slot, void *value) {
    uint64_t raw = (uint64_t)value;
    *slot = raw ? (void *)(raw | gc_good_color) : NULL;
}

/* Mark `user` if unmarked this cycle, pushing it for tracing. A pointer
 * into a ghost region (forwarded from the last cycle's relocate) is
 * remapped to its to-space copy first, so we only ever mark live to-space
 * objects. */
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
    /* Atomic test-and-set of the mark bit: concurrent markers (mutator
     * barriers + the GC thread) race here, and exactly the one that flips the
     * bit takes ownership and pushes the object. */
    uint64_t old = __atomic_fetch_or(&block[0], g_header_mark, __ATOMIC_RELAXED);
    if (old & g_header_mark) {
        return; /* already marked this cycle */
    }
    if (g_worklist_top >= WORKLIST_MAX) {
        fprintf(stderr, "klassic gc: mark worklist overflow\n");
        exit(1);
    }
    g_worklist[g_worklist_top++] = (void *)raw;
}

/* Drain the worklist, marking every reachable pointer. */
/* Trace up to `budget` worklist objects (a bounded mark quantum). */
static void gc_trace(uint64_t budget) {
    while (budget && g_worklist_top) {
        budget--;
        void *user = g_worklist[--g_worklist_top];
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
            uint64_t raw = (uint64_t)fields[i] & gc_strip_mask;
            if (raw) {
                uint64_t *fb = (uint64_t *)(raw - 16);
                if (gc_forwarded(fb[1])) {
                    raw = fb[1]; /* remap a ghost pointer to to-space */
                }
                /* Recolor-on-trace: rewrite the field to the (remapped)
                 * good-colored pointer so a later barriered load fast-paths
                 * and no live field is left referencing a ghost. */
                fields[i] = (void *)(raw | gc_good_color);
                mark_visit((void *)raw);
            }
        }
    }
}

/* Drain the worklist to fixpoint. */
static void gc_drain(void) {
    while (g_worklist_top) {
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
        g_phase = GC_PHASE_IDLE;
        g_bytes_since_cycle = 0;
        return;
    }
    g_to_base = g_to_top = g_to_end = NULL;
    g_phase = GC_PHASE_RELOCATE;
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
    g_phase = GC_PHASE_IDLE;
    g_bytes_since_cycle = 0;
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
    g_phase = GC_PHASE_MARK;
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

/* Synchronous full collection (exhaustion / explicit): drive the phase
 * machine all the way back to Idle. */
void klassic_gc_collect(void) {
    if (!g_initialized) {
        return;
    }
    if (g_phase == GC_PHASE_IDLE) {
        gc_mark_start();
    }
    if (g_phase == GC_PHASE_MARK) {
        gc_mark_end(); /* -> Relocate or Idle */
    }
    gc_relocate_drain();
}

/* Safepoint handshake: called from a mutator's poll when gc_handshake_requested
 * is set. Scans this thread's roots into the collector per the current phase
 * (mark them during Mark, remap them during Relocate) -- the O(roots) work a
 * phase transition needs from each mutator -- then acks by clearing the flag.
 * Dormant until the GC thread (M4) raises the flag; M4 also makes the roots
 * per-thread and the ack a barrier across all registered mutators. */
void klassic_gc_handshake(void) {
    uint64_t phase = __atomic_load_n(&g_phase, __ATOMIC_ACQUIRE);
    if (phase == GC_PHASE_MARK) {
        for (uint64_t i = 0; i < g_shadow_top; i++) {
            mark_visit(*g_shadow[i]);
        }
    } else if (phase == GC_PHASE_RELOCATE) {
        for (uint64_t i = 0; i < g_shadow_top; i++) {
            relocate_root(g_shadow[i]);
        }
    }
    __atomic_store_n(&gc_handshake_requested, 0, __ATOMIC_RELEASE); /* ack */
}

/* Poll the phase machine from an allocation point: proactively start a cycle
 * under memory pressure, run a mark quantum during Mark, and a relocate
 * quantum during Relocate. */
static void gc_driver(uint64_t total) {
    if (g_phase == GC_PHASE_MARK) {
        g_bytes_since_quantum += total;
        if (g_bytes_since_quantum >= GC_QUANTUM_BYTES) {
            g_bytes_since_quantum = 0;
            gc_trace(GC_QUANTUM_POPS);
            if (g_worklist_top == 0) {
                gc_mark_end(); /* -> Relocate or Idle */
            }
        }
        return;
    }
    if (g_phase == GC_PHASE_RELOCATE) {
        g_bytes_since_quantum += total;
        if (g_bytes_since_quantum >= GC_QUANTUM_BYTES) {
            g_bytes_since_quantum = 0;
            gc_relocate_step(GC_RELOCATE_BYTES); /* -> Idle when drained */
        }
        return;
    }
    g_bytes_since_cycle += total;
    if (g_bytes_since_cycle >= (g_budget << REGION_SHIFT) / 2) {
        gc_mark_start();
    }
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
