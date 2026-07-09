/* klassic_gc.c -- ZGC-style region collector in C: colored pointers + load
 * barrier, incremental marking, and stop-the-world compacting evacuation
 * with in-object forwarding. See klassic_gc.h for the object layout and
 * the migration context.
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

/* --- Incremental phase machine ----------------------------------- */
#define GC_PHASE_IDLE 0
#define GC_PHASE_MARK 1
#define GC_QUANTUM_POPS 512   /* objects traced per mark quantum */
#define GC_QUANTUM_BYTES 8192 /* allocation between mark quanta */
static uint64_t g_phase = GC_PHASE_IDLE;
static uint64_t g_bytes_since_cycle;   /* drives the proactive cycle start */
static uint64_t g_bytes_since_quantum; /* drives a mark quantum */

static int g_initialized;

/* --- Colored pointers -------------------------------------------- */
#define GC_COLOR_M0 (1ull << 60)
#define GC_COLOR_M1 (1ull << 61)
#define GC_COLOR_R (1ull << 62)
#define GC_COLOR_MASK (7ull << 60)

/* Mask cells (dso_local: defined here, referenced by the emitted fast
 * path). good = M0, bad catches the other colors; strip clears the color
 * bits. The moving milestone flips these; here they are static. */
uint64_t gc_good_color = GC_COLOR_M0;
uint64_t gc_bad_mask = GC_COLOR_M1 | GC_COLOR_R;
uint64_t gc_strip_mask = ~GC_COLOR_MASK;

static uint64_t align16(uint64_t n) { return (n + 15u) & ~(uint64_t)15u; }
static uint64_t block_size(uint64_t word0) { return word0 & ~(uint64_t)15u; }

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
             * cycle, so the in-progress mark does not reclaim it. */
            if (g_phase == GC_PHASE_MARK) {
                *(uint64_t *)block |= g_header_mark;
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

static void mark_visit(void *user); /* forward decl */

uint64_t klassic_gc_load_barrier_slow(uint64_t value, void **slot) {
    uint64_t raw = value & gc_strip_mask;
    /* Self-heal: store the good-colored pointer back so later barriered
     * loads of this slot take the fast path. (The moving milestone will
     * remap here first.) A plain store -- single mutator. */
    if (raw) {
        *slot = (void *)(raw | gc_good_color);
    }
    /* During Mark, the mutator has just pulled a raw pointer into a
     * register, so it must be marked to keep the strong tricolor
     * invariant (load-barrier-driven incremental update -- no store
     * barrier, since klassic has no in-place heap mutation). */
    if (g_phase == GC_PHASE_MARK) {
        mark_visit((void *)raw);
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

/* Mark `user` if unmarked this cycle, pushing it for tracing. */
static void mark_visit(void *user) {
    user = (void *)((uint64_t)user & gc_strip_mask); /* defensive: raw */
    if (!user) {
        return;
    }
    uint64_t *block = (uint64_t *)((uint8_t *)user - 16);
    if (block[0] & g_header_mark) {
        return; /* already marked this cycle */
    }
    block[0] |= g_header_mark;
    if (g_worklist_top >= WORKLIST_MAX) {
        fprintf(stderr, "klassic gc: mark worklist overflow\n");
        exit(1);
    }
    g_worklist[g_worklist_top++] = user;
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
                /* Recolor-on-trace: rewrite the field to the good color so
                 * a later barriered load fast-paths. */
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

/* Reclaim whole-dead regions; on a live block clear the stale mark bit. */
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
            if (b[0] & g_header_mark) {
                b[0] &= stale_clear;
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

/* --- Stop-the-world compacting evacuation (moving GC) ------------ *
 * Called at MarkEnd after sweep, so marking is complete (live == marked)
 * and -- being a safepoint reached from gc_alloc/collect -- no mutator
 * pointer is live in a register outside the shadow stack. Sparse regions
 * are evacuated into a fresh to-space, each vacated object left with an
 * in-object forwarding word (word0 = new_user | FWD), then every root and
 * live heap field is rewritten to the new location and the from-space
 * regions are freed. The mutator's colored-pointer model is unchanged;
 * lazy/concurrent relocation via the R-color barrier is a later
 * refinement (see docs/zgc-plan.md). */

/* Acquire a fresh zeroed region for the to-space bump (free pool first,
 * then a committed tail region), without disturbing the mutator's current
 * region. Returns 0 if none available. */
static int to_acquire(void) {
    uint8_t *base;
    if (g_free_head) {
        base = g_free_head;
        g_free_head = *(uint8_t **)base;
        memset(base, 0, REGION_SIZE);
    } else if (g_committed < g_budget) {
        base = region_at(g_committed);
        g_committed++;
    } else {
        return 0;
    }
    g_to_base = base;
    g_to_top = base;
    g_to_end = base + REGION_SIZE;
    g_region_top[region_index(base)] = (uint64_t)(uintptr_t)base;
    return 1;
}

/* Bump `total` bytes in to-space, switching to a fresh region when the
 * current one cannot fit the block. Returns NULL if to-space is exhausted
 * (the headroom invariant makes this unreachable in practice). */
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

/* Rewrite one colored heap-pointer slot to follow forwarding, if its
 * target has been evacuated. */
static void fixup_field(void **slot) {
    uint64_t raw = (uint64_t)*slot & gc_strip_mask;
    if (!raw) {
        return;
    }
    uint64_t w0 = *(uint64_t *)(raw - 16);
    if (w0 & KLASSIC_GC_FWD) {
        uint64_t nw = w0 & ~(uint64_t)15u; /* new user pointer (16-aligned) */
        *slot = (void *)(nw | gc_good_color);
    }
}

/* Rewrite a raw root slot to follow forwarding (roots hold raw pointers). */
static void fixup_root(void **slot) {
    uint64_t raw = (uint64_t)*slot;
    if (!raw) {
        return;
    }
    uint64_t w0 = *(uint64_t *)(raw - 16);
    if (w0 & KLASSIC_GC_FWD) {
        *slot = (void *)(w0 & ~(uint64_t)15u);
    }
}

/* Rewrite the pointer fields of every live block in a kept region. */
static void fixup_region(uint8_t *base, uint8_t *top) {
    for (uint8_t *cur = base; cur < top;) {
        uint64_t *b = (uint64_t *)cur;
        uint64_t sz = block_size(b[0]);
        if (b[0] & g_header_mark) {
            uint64_t tag = b[1];
            if (tag >= KLASSIC_GC_POINTER_RECORD) {
                void **fields = (void **)(cur + 16);
                uint64_t slots = (sz - 16) / 8;
                uint64_t start = (tag == KLASSIC_GC_POINTER_LIST) ? 1 : 0;
                for (uint64_t k = start; k < slots; k++) {
                    fixup_field(&fields[k]);
                }
            }
        }
        cur += sz;
    }
}

static void gc_relocate(void) {
    /* Free capacity, in regions: the free pool plus the still-uncommitted
     * budget tail. Reserve one region of headroom so evacuation (which
     * wastes at most a partial region to fragmentation) cannot deadlock. */
    uint64_t free_regions = 0;
    for (uint8_t *p = g_free_head; p; p = *(uint8_t **)p) {
        free_regions++;
    }
    if (g_committed < g_budget) {
        free_regions += g_budget - g_committed;
    }
    if (free_regions <= 1) {
        return; /* no room for a to-space -> mark-only cycle */
    }
    uint64_t budget_left = (free_regions - 1) * REGION_SIZE;

    /* Select the relocation set: non-current, non-empty regions that are
     * less than half full, capped by the headroom budget. */
    uint64_t selected = 0;
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        g_from_set[idx] = 0;
        uint8_t *base = region_at(idx);
        if (base == g_heap_base) {
            continue;
        }
        uint64_t live = g_region_live[idx];
        if (live == 0 || live * 2 >= REGION_SIZE || live > budget_left) {
            continue;
        }
        g_from_set[idx] = 1;
        budget_left -= live;
        selected++;
    }
    if (selected == 0) {
        return;
    }

    /* Evacuate: copy each live block into to-space, then overwrite its old
     * header word0 with the forwarding pointer. Size is read before the
     * overwrite, so the linear walk stays valid. */
    g_to_base = g_to_top = g_to_end = NULL;
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        if (!g_from_set[idx]) {
            continue;
        }
        uint8_t *base = region_at(idx);
        uint8_t *top = (uint8_t *)(uintptr_t)g_region_top[idx];
        for (uint8_t *cur = base; cur < top;) {
            uint64_t *b = (uint64_t *)cur;
            uint64_t sz = block_size(b[0]);
            if (b[0] & g_header_mark) {
                uint8_t *dst = to_bump(sz);
                if (!dst) {
                    fprintf(stderr, "klassic gc: evacuation overran to-space\n");
                    exit(1);
                }
                memcpy(dst, cur, sz);
                b[0] = (uint64_t)(uintptr_t)(dst + 16) | KLASSIC_GC_FWD;
                g_relocations++;
            }
            cur += sz;
        }
    }
    if (g_to_base) {
        g_region_top[region_index(g_to_base)] = (uint64_t)(uintptr_t)g_to_top;
    }

    /* Fix every reference to a moved object: roots, then the live fields of
     * every kept region (the from-space regions are about to be freed, so
     * their -- now stale -- contents are skipped). */
    for (uint64_t i = 0; i < g_shadow_top; i++) {
        fixup_root(g_shadow[i]);
    }
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        if (g_from_set[idx]) {
            continue;
        }
        uint8_t *base = region_at(idx);
        uint8_t *top = (uint8_t *)(uintptr_t)g_region_top[idx];
        if (top == base) {
            continue;
        }
        fixup_region(base, top);
    }

    /* Free the vacated (now fully forwarded) from-space regions. */
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        if (!g_from_set[idx]) {
            continue;
        }
        uint8_t *base = region_at(idx);
        *(uint8_t **)base = g_free_head;
        g_free_head = base;
        g_region_top[idx] = (uint64_t)(uintptr_t)base;
        g_region_live[idx] = 0;
    }
}

/* MarkStart: flip the mark color (M0<->M1) and header parity, scan roots,
 * enter the Mark phase. The new bad mask catches the OLD good color | R,
 * so pointers left from the previous cycle slow-path and are re-marked
 * (incremental-update marking). */
static void gc_mark_start(void) {
    uint64_t old_good = gc_good_color;
    gc_bad_mask = old_good | GC_COLOR_R;
    gc_good_color = old_good ^ (GC_COLOR_M0 | GC_COLOR_M1);
    g_header_mark ^= KLASSIC_GC_HMARK_BOTH;
    g_worklist_top = 0;
    gc_mark_roots();
    g_phase = GC_PHASE_MARK;
}

/* MarkEnd: drain the frontier to fixpoint, sweep, compact sparse regions,
 * return to Idle. */
static void gc_mark_end(void) {
    gc_drain();
    gc_sweep();
    gc_relocate();
    g_phase = GC_PHASE_IDLE;
    g_bytes_since_cycle = 0;
}

/* Synchronous full collection (exhaustion / explicit). If a cycle is in
 * progress, finish it; otherwise a from-scratch stop-the-world mark. */
void klassic_gc_collect(void) {
    if (!g_initialized) {
        return;
    }
    if (g_phase == GC_PHASE_MARK) {
        gc_mark_end();
        return;
    }
    gc_mark_start();
    gc_mark_end();
}

/* Poll the phase machine from an allocation point: proactively start a
 * cycle under memory pressure, and run a mark quantum during Mark. */
static void gc_driver(uint64_t total) {
    if (g_phase == GC_PHASE_MARK) {
        g_bytes_since_quantum += total;
        if (g_bytes_since_quantum >= GC_QUANTUM_BYTES) {
            g_bytes_since_quantum = 0;
            gc_trace(GC_QUANTUM_POPS);
            if (g_worklist_top == 0) {
                gc_mark_end();
            }
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
