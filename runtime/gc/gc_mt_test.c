/* gc_mt_test.c -- multi-mutator stress test for the concurrent collector
 * (true-zgc M5). Several mutator threads each churn short-lived garbage while
 * holding a rooted survivor, and read that survivor back through the load
 * barrier, all while the background GC thread marks and relocates
 * concurrently. Run under ThreadSanitizer (data races, the N-mutator
 * handshake) and AddressSanitizer (use-after-free across a concurrent move).
 *
 * Each thread registers itself, roots its survivor in its OWN shadow stack,
 * and deregisters on exit. The GC thread's phase-transition handshakes must
 * stop every registered thread (parked-count rendezvous) before it flips the
 * mask / scans roots / relocates, or a running thread would observe a
 * half-updated collector. The survivor's payload must read back intact after
 * being relocated out from under the thread -- the whole point of precise
 * per-thread roots + the load barrier.
 */
#include "klassic_gc.h"

#include <assert.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* Same object shapes as gc_test.c, re-declared here so the harness is
 * self-contained. The rooting discipline (stage each intermediate in a rooted
 * slot before the next allocation) is per-thread now: each thread pushes onto
 * its own shadow stack. */
static int64_t *make_leaf(int64_t v) {
    int64_t *p = (int64_t *)klassic_gc_alloc(8, KLASSIC_GC_RAW_BYTES);
    p[0] = v;
    return p;
}
static void **make_pair(void *a, void *b) {
    void **p = (void **)klassic_gc_alloc(16, KLASSIC_GC_POINTER_RECORD);
    klassic_gc_write(&p[0], a);
    klassic_gc_write(&p[1], b);
    return p;
}
static void *pair_field(void **p, int i) { return klassic_gc_read(&p[i]); }
static void **pair_of_leaves(int64_t av, int64_t bv) {
    void *a = 0, *b = 0;
    klassic_gc_shadow_push(&a);
    klassic_gc_shadow_push(&b);
    a = make_leaf(av);
    b = make_leaf(bv);
    void **p = make_pair(a, b);
    klassic_gc_shadow_pop_n(2);
    return p;
}

#define MUTATORS 4
#define ITERS 100000

static void *mutator_main(void *arg) {
    int tid = (int)(intptr_t)arg;
    klassic_gc_register_thread();
    /* A survivor unique to this thread; its two leaves carry known values. */
    int64_t av = (int64_t)tid * 1000 + 1;
    int64_t bv = (int64_t)tid * 1000 + 2;
    void **keeper = pair_of_leaves(av, bv);
    klassic_gc_shadow_push((void **)&keeper); /* root it in this thread's stack */
    for (int i = 0; i < ITERS; i++) {
        void **garbage = pair_of_leaves(i, i);
        (void)garbage;
        if ((i & 1023) == 0) {
            /* Read the survivor's leaves through the barrier; a concurrent
             * relocation may have moved the pair and/or the leaves, and the
             * barrier must return the current copies with the right payload. */
            int64_t *la = (int64_t *)pair_field(keeper, 0);
            int64_t *lb = (int64_t *)pair_field(keeper, 1);
            assert(la[0] == av);
            assert(lb[0] == bv);
        }
    }
    int64_t *la = (int64_t *)pair_field(keeper, 0);
    int64_t *lb = (int64_t *)pair_field(keeper, 1);
    assert(la[0] == av);
    assert(lb[0] == bv);
    klassic_gc_shadow_pop_n(1);
    klassic_gc_unregister_thread();
    return NULL;
}

int main(void) {
    pthread_t th[MUTATORS];
    for (int i = 0; i < MUTATORS; i++) {
        if (pthread_create(&th[i], NULL, mutator_main, (void *)(intptr_t)i) != 0) {
            fprintf(stderr, "cannot spawn mutator %d\n", i);
            return 1;
        }
    }
    for (int i = 0; i < MUTATORS; i++) {
        pthread_join(th[i], NULL);
    }
    printf("MT: %d mutators x %d iters, survivors intact\n", MUTATORS, ITERS);
    printf("ALL GC MT TESTS PASSED\n");
    return 0;
}
