/* gc_test.c -- standalone unit tests for the C garbage collector,
 * compiled with -fsanitize=address,undefined (see run_tests.sh). These
 * are the payoff of moving the GC to C: the collector is exercised on
 * synthetic heaps + roots with a memory sanitizer catching any
 * use-after-free or out-of-bounds the asm version could never surface.
 *
 * A precise incremental/moving collector requires the mutator to root
 * every live heap pointer across an allocation (a "safepoint"), because a
 * proactive collection can fire inside any alloc. These helpers therefore
 * stage each intermediate value in a rooted shadow-stack slot before the
 * next allocation -- exactly the discipline the code generator must
 * follow (the asm backend's argument staging, M3b). Skipping it is a
 * mutator bug, not a collector bug: an unrooted temporary allocated
 * before a MarkStart is unmarked, and storing it into a survivor leaves a
 * dangling field.
 */
#include "klassic_gc.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>

/* A leaf: raw bytes holding one int64. The caller must root the result
 * before its next allocation. */
static int64_t *make_leaf(int64_t v) {
    int64_t *p = (int64_t *)klassic_gc_alloc(8, KLASSIC_GC_RAW_BYTES);
    p[0] = v;
    return p;
}

/* A pair record with two pointer fields. The pair's own allocation is a
 * safepoint: with a MOVING collector the GC may relocate a and b's targets
 * during it and update every ROOTED slot -- but not these parameter copies.
 * So the parameters are rooted across the allocation and re-read from their
 * (GC-updated) slots afterwards, the discipline the compiled backend follows
 * (record_literal reloads each staged value from its shadow slot after the
 * record's allocation). */
static void **make_pair(void *a, void *b) {
    klassic_gc_shadow_push(&a);
    klassic_gc_shadow_push(&b);
    void **p = (void **)klassic_gc_alloc(16, KLASSIC_GC_POINTER_RECORD);
    klassic_gc_write(&p[0], *(void *volatile *)&a); /* reload after safepoint */
    klassic_gc_write(&p[1], *(void *volatile *)&b);
    klassic_gc_shadow_pop_n(2);
    return p;
}

/* Read a pointer field through the load barrier (strips the color).
 * klassic_gc_read is NOT a safepoint, so `p` (and the returned pointer, until
 * the caller's next safepoint) stays valid across the call. */
static void *pair_field(void **p, int i) { return klassic_gc_read(&p[i]); }

/* Build a pair of two fresh int leaves, staging each intermediate in a
 * rooted slot so a collection mid-construction cannot strand it (and so the
 * slots track relocation: a and b are re-read from them after each alloc). */
static void **pair_of_leaves(int64_t av, int64_t bv) {
    void *a = 0, *b = 0;
    klassic_gc_shadow_push(&a);
    klassic_gc_shadow_push(&b);
    a = make_leaf(av); /* a rooted; survives (and tracks) b's allocation */
    b = make_leaf(bv); /* b rooted; a still rooted */
    void **p = make_pair(a, b);
    klassic_gc_shadow_pop_n(2);
    return p;
}

/* A cons cell [leaf(head)][tail]. `tail` is rooted here across the two
 * allocations (the caller's rooted slot keeps the LIST alive, but this copy
 * must also track relocation), and the fresh head leaf is staged likewise. */
static void **cons(int64_t head, void **tail) {
    void *leaf = 0;
    klassic_gc_shadow_push((void **)&tail);
    klassic_gc_shadow_push(&leaf);
    leaf = make_leaf(head);
    void **p = make_pair(leaf, tail); /* both re-read from rooted slots */
    klassic_gc_shadow_pop_n(2);
    return p;
}

/* The load barrier strips a color and heals a bad-colored slot. */
static void test_load_barrier_strips_and_heals(void) {
    void *target = 0;
    klassic_gc_shadow_push(&target);
    target = make_leaf(777);
    void **cell = (void **)klassic_gc_alloc(16, KLASSIC_GC_POINTER_RECORD);
    /* Simulate a stale bad-colored slot (as a previous cycle would leave):
     * store the raw target OR'd with a bad color directly. */
    cell[0] = (void *)((uint64_t)target | (gc_bad_mask & (7ull << 60)));
    cell[1] = 0;
    void *got = klassic_gc_read(&cell[0]); /* must strip + heal */
    assert(got == target);
    void *again = klassic_gc_read(&cell[0]); /* now good-colored -> fast */
    assert(again == target);
    assert(((int64_t *)got)[0] == 777);
    klassic_gc_shadow_pop_n(1);
    printf("test_load_barrier_strips_and_heals OK\n");
}

/* A rooted survivor and its whole graph are kept while unrooted garbage
 * churned around it is reclaimed. */
static void test_survivor_kept_and_intact(void) {
    void **keeper = pair_of_leaves(100, 23);
    klassic_gc_shadow_push((void **)&keeper);
    for (int i = 0; i < 200000; i++) {
        void **garbage = pair_of_leaves(i, i);
        (void)garbage; /* unrooted -> collectible */
    }
    klassic_gc_collect();
    int64_t *a = (int64_t *)pair_field(keeper, 0);
    int64_t *b = (int64_t *)pair_field(keeper, 1);
    assert(a[0] == 100 && b[0] == 23);
    assert(klassic_gc_collection_count() > 0);
    klassic_gc_shadow_pop_n(1);
    printf("test_survivor_kept_and_intact OK (collections=%llu)\n",
           (unsigned long long)klassic_gc_collection_count());
}

/* A long-lived linked structure survives many collections with its whole
 * chain intact. */
static void test_linked_structure_survives_churn(void) {
    void **list = 0;
    klassic_gc_shadow_push((void **)&list);
    for (int i = 0; i < 500; i++) {
        list = cons(i, list); /* whole chain stays rooted via `list` */
    }
    for (int i = 0; i < 200000; i++) {
        void **garbage = pair_of_leaves(i, i);
        (void)garbage;
    }
    klassic_gc_collect();
    int64_t sum = 0;
    int count = 0;
    for (void **node = list; node; node = (void **)pair_field(node, 1)) {
        int64_t *value = (int64_t *)pair_field(node, 0);
        sum += value[0];
        count++;
    }
    assert(count == 500);
    assert(sum == 124750); /* 0+1+...+499 */
    klassic_gc_shadow_pop_n(1);
    printf("test_linked_structure_survives_churn OK (nodes=%d sum=%lld)\n", count,
           (long long)sum);
}

/* The proactive incremental driver keeps a rooted survivor intact across
 * many cycles WITHOUT any explicit collection. */
static void test_incremental_driver_keeps_survivor(void) {
    void **keeper = pair_of_leaves(555, 444);
    klassic_gc_shadow_push((void **)&keeper);
    uint64_t before = klassic_gc_collection_count();
    for (int i = 0; i < 400000; i++) {
        void **garbage = pair_of_leaves(i, i);
        (void)garbage;
    }
    assert(klassic_gc_collection_count() > before); /* cycles ran on their own */
    assert(((int64_t *)pair_field(keeper, 0))[0] == 555);
    assert(((int64_t *)pair_field(keeper, 1))[0] == 444);
    klassic_gc_shadow_pop_n(1);
    printf("test_incremental_driver_keeps_survivor OK (cycles=%llu)\n",
           (unsigned long long)(klassic_gc_collection_count() - before));
}

/* Pure garbage is reclaimed: the live-region count stays bounded. */
static void test_garbage_is_reclaimed(void) {
    for (int i = 0; i < 400000; i++) {
        void **garbage = pair_of_leaves(i, i);
        (void)garbage;
    }
    klassic_gc_collect();
    uint64_t live = klassic_gc_live_region_count();
    assert(live <= 4);
    printf("test_garbage_is_reclaimed OK (live_regions=%llu)\n", (unsigned long long)live);
}

/* Compaction physically moves a rooted survivor: its address changes,
 * yet its payload and the root that points at it stay consistent. */
static void test_survivor_is_relocated_and_intact(void) {
    void **keeper = pair_of_leaves(314, 271);
    klassic_gc_shadow_push((void **)&keeper);
    void *before = (void *)keeper;
    uint64_t relocs_before = klassic_gc_relocation_count();
    for (int i = 0; i < 300000; i++) {
        void **garbage = pair_of_leaves(i, i);
        (void)garbage;
    }
    klassic_gc_collect();
    /* Objects were physically evacuated, and the rooted survivor is one of
     * them (its sparse region is a relocation target). */
    assert(klassic_gc_relocation_count() > relocs_before);
    assert((void *)keeper != before); /* the root was updated to to-space */
    /* Payload survived the copy; the leaves moved too but read back intact. */
    assert(((int64_t *)pair_field(keeper, 0))[0] == 314);
    assert(((int64_t *)pair_field(keeper, 1))[0] == 271);
    klassic_gc_shadow_pop_n(1);
    printf("test_survivor_is_relocated_and_intact OK (relocs=%llu)\n",
           (unsigned long long)(klassic_gc_relocation_count() - relocs_before));
}

/* A linked structure stays fully connected across compaction: every node
 * and edge is rewritten to the moved locations with no dangling pointer. */
static void test_linked_structure_survives_compaction(void) {
    void **list = 0;
    klassic_gc_shadow_push((void **)&list);
    for (int i = 0; i < 300; i++) {
        list = cons(i, list);
    }
    uint64_t relocs_before = klassic_gc_relocation_count();
    for (int i = 0; i < 300000; i++) {
        void **garbage = pair_of_leaves(i, i);
        (void)garbage;
    }
    klassic_gc_collect();
    assert(klassic_gc_relocation_count() > relocs_before);
    int64_t sum = 0;
    int count = 0;
    for (void **node = list; node; node = (void **)pair_field(node, 1)) {
        sum += ((int64_t *)pair_field(node, 0))[0];
        count++;
    }
    assert(count == 300);
    assert(sum == 44850); /* 0+1+...+299 */
    klassic_gc_shadow_pop_n(1);
    printf("test_linked_structure_survives_compaction OK (nodes=%d sum=%lld)\n", count,
           (long long)sum);
}

/* Read a rooted survivor's fields *during* heavy churn, so some reads land
 * while the collector is in the Relocate phase and the survivor's leaves are
 * still in the from-set: the load barrier must then evacuate on demand and
 * return the to-space copy, keeping the values intact. Exercises the
 * incremental-relocation barrier path the STW collector never had. */
static void test_reads_during_relocation(void) {
    void **keeper = pair_of_leaves(1000, 2000);
    klassic_gc_shadow_push((void **)&keeper);
    int64_t witness = 0;
    for (int i = 0; i < 400000; i++) {
        void **garbage = pair_of_leaves(i, i);
        (void)garbage;
        if ((i & 31) == 0) {
            /* barriered reads, some during Relocate -> evacuate-on-demand */
            witness += ((int64_t *)pair_field(keeper, 0))[0];
            witness += ((int64_t *)pair_field(keeper, 1))[0];
        }
    }
    assert(((int64_t *)pair_field(keeper, 0))[0] == 1000);
    assert(((int64_t *)pair_field(keeper, 1))[0] == 2000);
    assert(witness > 0);
    klassic_gc_shadow_pop_n(1);
    printf("test_reads_during_relocation OK (relocs=%llu)\n",
           (unsigned long long)klassic_gc_relocation_count());
}

int main(void) {
    klassic_gc_init();
    test_load_barrier_strips_and_heals();
    test_incremental_driver_keeps_survivor();
    test_survivor_kept_and_intact();
    test_linked_structure_survives_churn();
    test_garbage_is_reclaimed();
    test_survivor_is_relocated_and_intact();
    test_linked_structure_survives_compaction();
    test_reads_during_relocation();
    printf("ALL GC TESTS PASSED\n");
    return 0;
}
