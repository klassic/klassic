/* gc_test.c -- standalone unit tests for the C garbage collector,
 * compiled with -fsanitize=address,undefined (see run_tests.sh). These
 * are the payoff of moving the GC to C: the collector is exercised on
 * synthetic heaps + roots with a memory sanitizer catching any
 * use-after-free or out-of-bounds the asm version could never surface.
 */
#include "klassic_gc.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>

/* A leaf: raw bytes holding one int64. */
static int64_t *make_leaf(int64_t v) {
    int64_t *p = (int64_t *)klassic_gc_alloc(8, KLASSIC_GC_RAW_BYTES);
    p[0] = v;
    return p;
}

/* A pair record: two pointer fields. */
static void **make_pair(void *a, void *b) {
    void **p = (void **)klassic_gc_alloc(16, KLASSIC_GC_POINTER_RECORD);
    p[0] = a;
    p[1] = b;
    return p;
}

/* A cons cell: [int value][pointer to next]. Modeled as a pointer record
 * whose first field is a leaf (so it is traced uniformly). */
static void **cons(int64_t head, void **tail) {
    return make_pair(make_leaf(head), tail);
}

/* A rooted survivor is kept, and its whole reachable graph, while
 * unrooted garbage churned around it is reclaimed. */
static void test_survivor_kept_and_intact(void) {
    void **keeper = make_pair(make_leaf(100), make_leaf(23));
    klassic_gc_shadow_push((void **)&keeper);
    for (int i = 0; i < 200000; i++) {
        void **garbage = make_pair(make_leaf(i), make_leaf(i));
        (void)garbage; /* unrooted -> collectible */
    }
    klassic_gc_collect();
    int64_t *a = (int64_t *)keeper[0];
    int64_t *b = (int64_t *)keeper[1];
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
        list = cons(i, list); /* prepend; the whole chain stays rooted via `list` */
    }
    for (int i = 0; i < 200000; i++) {
        void **garbage = make_pair(make_leaf(i), make_leaf(i));
        (void)garbage;
    }
    klassic_gc_collect();
    /* Walk the chain: values 499,498,...,0. Sum = 499*500/2 = 124750. */
    int64_t sum = 0;
    int count = 0;
    for (void **node = list; node; node = (void **)node[1]) {
        int64_t *value = (int64_t *)node[0];
        sum += value[0];
        count++;
    }
    assert(count == 500);
    assert(sum == 124750);
    klassic_gc_shadow_pop_n(1);
    printf("test_linked_structure_survives_churn OK (nodes=%d sum=%lld)\n", count,
           (long long)sum);
}

/* Pure garbage is reclaimed: the live-region count stays bounded across a
 * churn far larger than the heap. */
static void test_garbage_is_reclaimed(void) {
    for (int i = 0; i < 400000; i++) {
        void **garbage = make_pair(make_leaf(i), make_leaf(i));
        (void)garbage;
    }
    klassic_gc_collect();
    uint64_t live = klassic_gc_live_region_count();
    /* Nothing is rooted here, so after a collection only the current bump
     * region (and maybe a straggler) remains live -- certainly not the
     * hundreds of regions the churn would need without reclamation. */
    assert(live <= 4);
    printf("test_garbage_is_reclaimed OK (live_regions=%llu)\n", (unsigned long long)live);
}

int main(void) {
    klassic_gc_init();
    test_survivor_kept_and_intact();
    test_linked_structure_survives_churn();
    test_garbage_is_reclaimed();
    printf("ALL GC TESTS PASSED\n");
    return 0;
}
