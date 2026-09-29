/* Test-only scheduling seams. No callbacks or identity lookup in production. */
#ifndef KLASSIC_GC_TEST_HOOKS_H
#define KLASSIC_GC_TEST_HOOKS_H

#include <stdint.h>

typedef enum {
    GC_TEST_BEFORE_HEADER_SCAN = 1,
    GC_TEST_ATTACH_WAIT = 2,
    GC_TEST_LEAVE_NATIVE_WAIT = 3,
    GC_TEST_CYCLE_STARTED = 4,
    GC_TEST_CYCLE_BEFORE_WAIT = 5,
    GC_TEST_CYCLE_WAIT = 6,
    GC_TEST_CYCLE_BEFORE_REENTRY = 7,
    GC_TEST_BEFORE_LOAD_HEAL = 8,
    GC_TEST_BEFORE_TRACE_HEAL = 9,
    GC_TEST_BEFORE_MARK_END = 10,
    GC_TEST_BEFORE_STORE_CAS = 11,
    GC_TEST_MUTATOR_PARKED = 12
} GcTestEvent;

#ifdef KLASSIC_GC_TEST_HOOKS
/* IDs belong to the fixture's logical object registry, never to physical
 * addresses. A fixture testing relocation must carry the ID to the copy.
 * Zero means an object not tracked by this fixture. Header callbacks may block
 * only when the fixture guarantees no handshake meanwhile. Lifecycle WAIT
 * callbacks execute with hs_lock held and must never block or call the GC.
 * CYCLE_STARTED runs outside every GC lock, before its first rendezvous; a
 * controlled fixture may block it to order new requests against that start.
 * CYCLE_BEFORE_WAIT runs native-blocked, outside every GC lock. CYCLE_WAIT
 * holds wake_lock immediately before cond_wait and must never block or call GC.
 * CYCLE_BEFORE_REENTRY runs native-blocked, outside every GC lock, after POLL
 * has observed completion. A fixture may delay controlled managed reentry.
 * BEFORE_*_HEAL runs after observing/resolving a referent, before CAS. A
 * fixture may block here only while preventing collection/relocation; a raw
 * slot and referent are live across this test-only seam.
 * BEFORE_STORE_CAS runs after pre-marking the observed old value, under the
 * same no-collection constraint as healing hooks. BEFORE_MARK_END
 * runs on the collector outside all locks, after its concurrent trace loop
 * observes an empty queue and before requesting the end-check rendezvous.
 * MUTATOR_PARKED runs after ACK under hs_lock; it must not block or call GC. */
uint64_t gc_test_object_id(const void *object);
void gc_test_hook(GcTestEvent event, uint64_t object_id);
#define GC_TEST_HOOK(event, object) gc_test_hook(event, gc_test_object_id(object))
#else
#define GC_TEST_HOOK(event, object) ((void)0)
#endif

#endif
