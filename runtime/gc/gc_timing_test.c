/* Real handshake/park paths with a deterministic clock. This verifies interval
 * boundaries, not machine latency; the LLVM log regression uses the OS clock. */
#define KLASSIC_GC_LOG
#define KLASSIC_GC_TEST_HOOKS
#define KLASSIC_GC_TEST_CLOCK
#include "klassic_gc.c"
#include <assert.h>
#include <sched.h>

static _Atomic uint64_t test_time = 100;
static _Atomic unsigned early_ready, early_parked;
static uint64_t action_end = 500, expected_owners = 2;

uint64_t gc_test_now_ns(void) { return atomic_load(&test_time); }
uint64_t gc_test_object_id(const void *object) { (void)object; return 0; }
void gc_test_hook(GcTestEvent event, uint64_t id) {
    (void)id;
    if (event == GC_TEST_MUTATOR_PARKED)
        atomic_store_explicit(&early_parked, 1, memory_order_release);
}
static void timing_action(void) {
    assert(g_handshake.expected == expected_owners);
    assert(g_handshake.acknowledged == expected_owners);
    atomic_store(&test_time, action_end);
}
static void *collector(void *argument) {
    (void)argument;
    gc_rendezvous(timing_action);
    return NULL;
}
static void chained_action(void) {
    assert(g_handshake.expected == 2 && g_handshake.acknowledged == 2);
    atomic_fetch_add(&test_time, 1000);
}
static void *chained_collector(void *argument) {
    (void)argument;
    /* Force the collector to win the mutex for the next request. Waiting
     * owners see a new generation before they can observe a released flag.
     * This is the real rendezvous operation, not a second model of its loop. */
    pthread_mutex_lock(&g_hs_lock);
    gc_rendezvous_locked(chained_action);
    gc_rendezvous_locked(chained_action);
    pthread_mutex_unlock(&g_hs_lock);
    return NULL;
}
static void *early_mutator(void *argument) {
    (void)argument;
    klassic_gc_register_thread();
    atomic_store_explicit(&early_ready, 1, memory_order_release);
    while (!__atomic_load_n(&gc_handshake_requested, __ATOMIC_ACQUIRE)) sched_yield();
    klassic_gc_handshake();
    klassic_gc_unregister_thread();
    return NULL;
}
int main(void) {
    klassic_gc_init();
    pthread_t early, coordinator;
    assert(pthread_create(&early, NULL, early_mutator, NULL) == 0);
    while (!atomic_load_explicit(&early_ready, memory_order_acquire)) sched_yield();
    assert(pthread_create(&coordinator, NULL, collector, NULL) == 0);
    while (!atomic_load_explicit(&early_parked, memory_order_acquire)) sched_yield();
    /* The early owner has ACKed at 100. The collector cannot complete until
     * this Running owner polls at 300; its action finishes at 500. */
    atomic_store(&test_time, 300);
    klassic_gc_handshake();
    assert(pthread_join(coordinator, NULL) == 0);
    assert(pthread_join(early, NULL) == 0);
    assert(g_stat_pauses == 1 && g_stat_parks == 2);
    assert(g_stat_safepoint_ns_max == 200 && g_stat_safepoint_ns_total == 200);
    assert(g_stat_pause_ns_max == 200 && g_stat_pause_ns_total == 200);
    assert(g_stat_stopped_ns_max == 200 && g_stat_stopped_ns_total == 200);
    assert(g_stat_park_ns_max == 400 && g_stat_park_ns_total == 600);
    /* No request: calling the handshake wrapper creates no park sample. */
    klassic_gc_handshake();
    assert(g_stat_parks == 2 && g_stat_park_ns_total == 600);
    /* BlockedNative owners are safe already and do not create park samples. */
    gc_enter_native(&g_self->context);
    expected_owners = 0; action_end = 1500;
    atomic_store(&test_time, 1000);
    assert(pthread_create(&coordinator, NULL, collector, NULL) == 0);
    assert(pthread_join(coordinator, NULL) == 0);
    gc_leave_native(&g_self->context);
    assert(g_stat_pauses == 2 && g_stat_parks == 2);
    assert(g_stat_safepoint_ns_total == 200);
    assert(g_stat_pause_ns_max == 500 && g_stat_pause_ns_total == 700);
    assert(g_stat_stopped_ns_max == 500 && g_stat_stopped_ns_total == 700);
    assert(g_stat_park_ns_max == 400 && g_stat_park_ns_total == 600);
    /* Two more requests with no opportunity to resume in between: each
     * owner must record ONE interval of 2000, not two intervals of 1000. */
    atomic_store(&test_time, 2000);
    atomic_store(&early_ready, 0);
    atomic_store(&early_parked, 0);
    assert(pthread_create(&early, NULL, early_mutator, NULL) == 0);
    while (!atomic_load_explicit(&early_ready, memory_order_acquire)) sched_yield();
    assert(pthread_create(&coordinator, NULL, chained_collector, NULL) == 0);
    while (!atomic_load_explicit(&early_parked, memory_order_acquire)) sched_yield();
    klassic_gc_handshake();
    assert(pthread_join(coordinator, NULL) == 0);
    assert(pthread_join(early, NULL) == 0);
    assert(g_stat_pauses == 4 && g_stat_parks == 4);
    assert(g_stat_pause_ns_max == 1000 && g_stat_pause_ns_total == 2700);
    assert(g_stat_stopped_ns_max == 1000 && g_stat_stopped_ns_total == 2700);
    assert(g_stat_safepoint_ns_max == 200 && g_stat_safepoint_ns_total == 200);
    assert(g_stat_park_ns_max == 2000 && g_stat_park_ns_total == 4600);
    klassic_gc_unregister_thread();
    puts("GC timing: consecutive rendezvous retain continuous mutator park intervals PASS (deterministic clock)");
    puts("GC timing: staggered parks, request latency and native exclusion PASS (deterministic clock)");
}
