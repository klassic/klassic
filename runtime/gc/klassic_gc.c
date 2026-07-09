/* klassic_gc.c -- non-moving region mark-sweep foundation (ZGC plan M4
 * equivalent), in C. See klassic_gc.h for the object layout and the
 * migration context. Colored pointers + load barrier, incremental
 * marking, and moving evacuation land in later commits; this file is the
 * base they build on.
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

/* The current bump region. */
static uint8_t *g_heap_base;
static uint8_t *g_heap_top;
static uint8_t *g_heap_end;

/* Precise roots and the mark worklist. */
static void **g_shadow[SHADOW_MAX];
static uint64_t g_shadow_top;
static void *g_worklist[WORKLIST_MAX];
static uint64_t g_worklist_top;

/* The current-cycle header mark bit (alternates each collection). */
static uint64_t g_header_mark = KLASSIC_GC_HMARK1;
static uint64_t g_collections;

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
    for (;;) {
        if (g_heap_base && (uint64_t)(g_heap_end - g_heap_top) >= total) {
            uint8_t *block = g_heap_top;
            g_heap_top += total;
            *(uint64_t *)block = total;           /* word0 = size, flags clear */
            *(uint64_t *)(block + 8) = type_tag;  /* word1 = type tag */
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

uint64_t klassic_gc_load_barrier_slow(uint64_t value, void **slot) {
    uint64_t raw = value & gc_strip_mask;
    /* Self-heal: store the good-colored pointer back so later barriered
     * loads of this slot take the fast path. (The moving milestone will
     * remap here first.) A plain store -- single mutator. */
    if (raw) {
        *slot = (void *)(raw | gc_good_color);
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
static void trace(void) {
    while (g_worklist_top) {
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

void klassic_gc_collect(void) {
    if (!g_initialized) {
        return;
    }
    g_collections++;
    /* Flip the mark parity: this cycle marks with the other bit, and the
     * sweep clears the now-stale parity, so liveness stays clean across
     * cycles. */
    g_header_mark ^= KLASSIC_GC_HMARK_BOTH;
    uint64_t stale_clear = ~(uint64_t)(KLASSIC_GC_HMARK_BOTH ^ g_header_mark);

    /* Mark from the shadow-stack roots. */
    g_worklist_top = 0;
    for (uint64_t i = 0; i < g_shadow_top; i++) {
        mark_visit(*g_shadow[i]);
    }
    trace();

    /* Flush the current bump region's watermark so the sweep covers it. */
    if (g_heap_base) {
        g_region_top[region_index(g_heap_base)] = (uint64_t)(uintptr_t)g_heap_top;
    }

    /* Sweep: reclaim whole-dead regions; on a live block clear the stale
     * mark bit (keep the current one). */
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        uint8_t *base = region_at(idx);
        uint8_t *top = (uint8_t *)(uintptr_t)g_region_top[idx];
        if (top == base) {
            continue; /* empty / already free */
        }
        int any_live = 0;
        for (uint8_t *cur = base; cur < top;) {
            uint64_t *b = (uint64_t *)cur;
            uint64_t sz = block_size(b[0]);
            if (b[0] & g_header_mark) {
                b[0] &= stale_clear; /* keep current mark, clear stale */
                any_live = 1;
            }
            cur += sz;
        }
        if (!any_live && base != g_heap_base) {
            /* Reclaim: push onto the free pool, mark the region empty. */
            *(uint8_t **)base = g_free_head;
            g_free_head = base;
            g_region_top[idx] = (uint64_t)(uintptr_t)base;
        }
    }
}

uint64_t klassic_gc_collection_count(void) { return g_collections; }

uint64_t klassic_gc_live_region_count(void) {
    uint64_t live = 0;
    for (uint64_t idx = 0; idx < g_committed; idx++) {
        if ((uint8_t *)(uintptr_t)g_region_top[idx] != region_at(idx)) {
            live++;
        }
    }
    return live;
}
