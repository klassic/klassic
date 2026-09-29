/* Real C collector lifecycle and deterministic handshake races. */
#define KLASSIC_GC_TEST_HOOKS
#include "klassic_gc.c"
#include <assert.h>
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>

static _Atomic unsigned attach_waits, native_waits;
static _Atomic unsigned cycle_gate, cycle_wait_gate, cycle_reentry_gate, cycle_waits;
static _Thread_local _Atomic unsigned *cycle_actor_waits;
uint64_t gc_test_object_id(const void *object) { (void)object; return 0; }
void gc_test_hook(GcTestEvent event, uint64_t object_id) {
    (void)object_id;
    if(event==GC_TEST_CYCLE_STARTED) {
        unsigned gate=atomic_load(&cycle_gate);
        if(gate==1||gate==3) {
            atomic_store(&cycle_gate,gate+1);
            while(atomic_load(&cycle_gate)==gate+1)sched_yield();
        }
    }
    if(event==GC_TEST_CYCLE_BEFORE_WAIT&&atomic_load(&cycle_wait_gate)==1) {
        atomic_store(&cycle_wait_gate,2);
        while(atomic_load(&cycle_wait_gate)==2)sched_yield();
    }
    if(event==GC_TEST_CYCLE_BEFORE_REENTRY&&atomic_load(&cycle_reentry_gate)==1) {
        atomic_store(&cycle_reentry_gate,2);
        while(atomic_load(&cycle_reentry_gate)==2)sched_yield();
    }
    if(event==GC_TEST_CYCLE_WAIT) {
        atomic_fetch_add(&cycle_waits,1);
        if(cycle_actor_waits)atomic_fetch_add(cycle_actor_waits,1);
    }
    if (event == GC_TEST_ATTACH_WAIT) atomic_fetch_add(&attach_waits, 1);
    if (event == GC_TEST_LEAVE_NATIVE_WAIT) atomic_fetch_add(&native_waits, 1);
}

static pthread_mutex_t gate_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t gate_cond = PTHREAD_COND_INITIALIZER;
static unsigned ready;
static int release_workers;
static MutatorContext *contexts[8];
static uint64_t identities[8];

static void *native_worker(void *arg) {
    unsigned index = (unsigned)(uintptr_t)arg;
    assert(gc_thread_current() == NULL);
    MutatorContext *ctx = gc_thread_attach();
    assert(ctx == gc_thread_current() && ctx == gc_thread_attach());
    assert(ctx->abi_version == GC_ABI_VERSION && ctx->thread_id != 0);
    assert(atomic_load(&ctx->state) == GC_RUNNING);
    void *root = klassic_gc_alloc(8, KLASSIC_GC_RAW_BYTES);
    *(uint64_t *)root = UINT64_C(0x123456789abcdef0) + index;
    klassic_gc_shadow_push(&root);
    // Leave the root's region sparse and inactive so it can actually move
    // while this owner is BlockedNative. Allocation polls may also collect.
    for (unsigned i = 0; i < 17; i++) {
        (void)klassic_gc_alloc(8192, KLASSIC_GC_RAW_BYTES);
    }
    assert(ctx->tlab_start && ctx->tlab_top > ctx->tlab_start);
    assert(ctx->tlab_top <= ctx->tlab_end);
    gc_enter_native(ctx);
    assert(atomic_load(&ctx->state) == GC_BLOCKED_NATIVE);
    pthread_mutex_lock(&gate_lock);
    contexts[index] = ctx;
    identities[index] = ctx->thread_id;
    ready++;
    pthread_cond_broadcast(&gate_cond);
    while (!release_workers) pthread_cond_wait(&gate_cond, &gate_lock);
    pthread_mutex_unlock(&gate_lock);
    gc_leave_native(ctx);
    assert(atomic_load(&ctx->state) == GC_RUNNING);
    uint64_t *resolved = klassic_gc_read(&root); // re-resolve after transition
    assert(*resolved == UINT64_C(0x123456789abcdef0) + index);
    klassic_gc_shadow_pop_n(1);
    gc_thread_detach(ctx);
    assert(gc_thread_current() == NULL);
    return NULL;
}

static void blocked_progress(unsigned count) {
    pthread_t threads[8];
    MutatorContext *self = gc_thread_current();
    ready = 0;
    release_workers = 0;
    gc_enter_native(self); // the controller's condition wait is native too
    for (unsigned i = 0; i < count; i++) {
        assert(pthread_create(&threads[i], NULL, native_worker, (void *)(uintptr_t)i) == 0);
    }
    pthread_mutex_lock(&gate_lock);
    while (ready != count) pthread_cond_wait(&gate_cond, &gate_lock);
    for (unsigned i = 0; i < count; i++) {
        assert(atomic_load(&contexts[i]->state) == GC_BLOCKED_NATIVE);
        for (unsigned j = 0; j < i; j++) assert(identities[i] != identities[j]);
    }
    pthread_mutex_unlock(&gate_lock);
    gc_leave_native(self);
    uint64_t before = klassic_gc_collection_count();
    uint64_t moved_before = klassic_gc_relocation_count();
    for (unsigned round = 0; round < 32; round++) klassic_gc_collect();
    assert(klassic_gc_collection_count() >= before + 32);
    // Only these blocked workers have roots; this cannot be an unrelated
    // observer allocation satisfying the relocation condition.
    uint64_t moved_after = klassic_gc_relocation_count();
    assert(moved_after > moved_before);
    printf("lifecycle native cohort %u: %llu relocations while blocked\n", count,
           (unsigned long long)(moved_after - moved_before));
    gc_enter_native(self);
    pthread_mutex_lock(&gate_lock);
    release_workers = 1;
    pthread_cond_broadcast(&gate_cond);
    pthread_mutex_unlock(&gate_lock);
    for (unsigned i = 0; i < count; i++) assert(pthread_join(threads[i], NULL) == 0);
    gc_leave_native(self);
    pthread_mutex_lock(&g_hs_lock);
    assert(g_handshake.members == 1 && g_handshake.native == 0);
    pthread_mutex_unlock(&g_hs_lock);
}

/* Hold a Running worker at a finite test gate just before its next transition.
 * The controller starts the real rendezvous and waits until both a new attach
 * and a native return are blocked on that exact request. No sleeps force races. */
static _Atomic unsigned runner_ready, leaver_ready, run_transition, return_native;
static _Atomic unsigned attached_after, action_count;
static MutatorContext *runner_context, *leaver_context;
static unsigned transition_kind;

static void await(_Atomic unsigned *flag) {
    while (!atomic_load_explicit(flag, memory_order_acquire)) sched_yield();
}

static void *running_transition(void *unused) {
    (void)unused;
    MutatorContext *ctx = gc_thread_attach();
    runner_context = ctx;
    atomic_store_explicit(&runner_ready, 1, memory_order_release);
    await(&run_transition);
    if (transition_kind == 0) klassic_gc_safepoint();
    else if (transition_kind == 1) {
        gc_enter_native(ctx);
        gc_leave_native(ctx);
    } else {
        gc_thread_detach(ctx);
        return NULL;
    }
    gc_thread_detach(ctx);
    return NULL;
}

static void *native_return(void *unused) {
    (void)unused;
    MutatorContext *ctx = gc_thread_attach();
    gc_enter_native(ctx);
    leaver_context = ctx;
    atomic_store_explicit(&leaver_ready, 1, memory_order_release);
    await(&return_native);
    gc_leave_native(ctx);
    gc_thread_detach(ctx);
    return NULL;
}

static void *late_attach(void *unused) {
    (void)unused;
    MutatorContext *ctx = gc_thread_attach();
    assert(atomic_load(&action_count) == 1);
    atomic_store(&attached_after, 1);
    gc_thread_detach(ctx);
    return NULL;
}

static void transition_action(void) {
    assert(g_handshake.members == 3 && g_handshake.native == 2);
    assert(g_handshake.acknowledged == 1);
    assert(atomic_load(&runner_context->state) == GC_AT_SAFEPOINT);
    assert(atomic_load(&leaver_context->state) == GC_BLOCKED_NATIVE);
    assert(atomic_load(&attach_waits) && atomic_load(&native_waits));
    assert(!atomic_load(&attached_after));
    atomic_fetch_add(&action_count, 1);
}

static void *requester(void *unused) {
    (void)unused;
    gc_rendezvous(transition_action);
    return NULL;
}

static void handshake_race(unsigned kind) {
    pthread_t runner, leaver, request, attacher;
    transition_kind = kind;
    atomic_store(&runner_ready, 0);
    atomic_store(&leaver_ready, 0);
    atomic_store(&run_transition, 0);
    atomic_store(&return_native, 0);
    atomic_store(&attach_waits, 0);
    atomic_store(&native_waits, 0);
    atomic_store(&attached_after, 0);
    atomic_store(&action_count, 0);
    MutatorContext *self = gc_thread_current();
    gc_enter_native(self);
    assert(pthread_create(&runner, NULL, running_transition, NULL) == 0);
    assert(pthread_create(&leaver, NULL, native_return, NULL) == 0);
    await(&runner_ready);
    await(&leaver_ready);
    assert(pthread_create(&request, NULL, requester, NULL) == 0);
    while (!__atomic_load_n(&gc_handshake_requested, __ATOMIC_ACQUIRE)) sched_yield();
    assert(pthread_create(&attacher, NULL, late_attach, NULL) == 0);
    atomic_store(&return_native, 1);
    await(&attach_waits);
    await(&native_waits);
    atomic_store(&run_transition, 1);
    assert(pthread_join(runner, NULL) == 0);
    assert(pthread_join(leaver, NULL) == 0);
    assert(pthread_join(request, NULL) == 0);
    assert(pthread_join(attacher, NULL) == 0);
    assert(atomic_load(&attached_after) == 1 && atomic_load(&action_count) == 1);
    gc_leave_native(self);
    pthread_mutex_lock(&g_hs_lock);
    assert(g_handshake.members == 1 && g_handshake.native == 0);
    pthread_mutex_unlock(&g_hs_lock);
}

/* Fork only before this process starts the background collector. A rejected
 * transition must report the expected diagnostic and exit nonzero. */
static void reject_misuse(unsigned kind) {
    int output[2];
    assert(pipe(output) == 0);
    pid_t child = fork();
    assert(child >= 0);
    if (child == 0) {
        close(output[0]);
        assert(dup2(output[1], STDERR_FILENO) >= 0);
        close(output[1]);
        MutatorContext *ctx = gc_thread_attach();
        void *root = NULL;
        if (kind == 0) gc_leave_native(ctx);
        else if (kind == 1) gc_thread_detach(NULL);
        else if (kind == 2) {
            atomic_store(&ctx->access_epoch, 1);
            gc_enter_native(ctx);
        } else if (kind == 8) {
            g_handshake.generation = UINT64_MAX;
            gc_rendezvous(transition_action);
        } else if (kind == 9) {
            gc_thread_detach(ctx);
            g_handshake.last_identity = UINT64_MAX;
            (void)gc_thread_attach();
        } else {
            gc_enter_native(ctx);
            if (kind == 3) gc_enter_native(ctx);
            else if (kind == 4) (void)klassic_gc_read(&root);
            else if (kind == 5) (void)klassic_gc_alloc(8, KLASSIC_GC_RAW_BYTES);
            else if (kind == 6) klassic_gc_shadow_push(&root);
            else gc_thread_detach(ctx);
        }
        _exit(0);
    }
    close(output[1]);
    char diagnostic[512];
    ssize_t n = read(output[0], diagnostic, sizeof(diagnostic) - 1);
    assert(n > 0);
    diagnostic[n] = 0;
    close(output[0]);
    int status;
    assert(waitpid(child, &status, 0) == child);
    assert(WIFEXITED(status) && WEXITSTATUS(status) == 1);
    const char *expected = kind == 8 ? "handshake generation exhausted" :
        kind == 9 ? "thread identity exhausted" : kind == 2 ? "active heap access guard" :
        (kind >= 4 && kind <= 6) ? "Running mutator" : "lifecycle owner or state";
    assert(strstr(diagnostic, expected));
}

static void fresh_ticket_rounds(MutatorContext *self) {
    klassic_gc_collect();
    for(unsigned round=0;round<16;round++) {
        atomic_store(&cycle_gate,1);
        CycleTicket first=gc_request_cycle(GC_CYCLE_MAJOR);
        while(atomic_load(&cycle_gate)!=2)sched_yield();
        CycleTicket later=gc_request_cycle(GC_CYCLE_MAJOR);
        CycleTicket coalesced=gc_request_cycle(GC_CYCLE_MAJOR);
        assert(later.cycle==first.cycle+1&&coalesced.cycle==later.cycle);
        atomic_store(&cycle_gate,3);
        while(atomic_load(&cycle_gate)!=4){gc_poll();sched_yield();}
        pthread_mutex_lock(&g_wake_lock);
        assert(gc_cycle_step_locked(&g_cycles,&first,GC_CY_POLL)==GC_CY_OK);
        assert(gc_cycle_step_locked(&g_cycles,&later,GC_CY_POLL)==GC_CY_WAIT);
        assert(gc_cycle_step_locked(&g_cycles,&coalesced,GC_CY_POLL)==GC_CY_WAIT);
        assert(g_cycles.completed[GC_CYCLE_MAJOR]==first.cycle&&g_cycles.started[GC_CYCLE_MAJOR]==later.cycle);
        pthread_mutex_unlock(&g_wake_lock);
        atomic_store(&cycle_gate,5);
        gc_await_cycle(self,later);gc_await_cycle(self,coalesced);
        atomic_store(&cycle_gate,0);
    }
    puts("cycle tickets: 16 real collector pairs, late requests wait past the running cycle PASS");
}

typedef struct {CycleTicket ticket;_Atomic unsigned returned,waits;} AwaitActor;
static void *cycle_awaiter(void *argument) {
    AwaitActor *a=argument;MutatorContext *ctx=gc_thread_attach();
    void *root=klassic_gc_alloc(8,KLASSIC_GC_RAW_BYTES);
    *(uint64_t *)root=UINT64_C(0x12345abcdef);klassic_gc_shadow_push(&root);
    cycle_actor_waits=&a->waits;
    gc_await_cycle(ctx,a->ticket);
    cycle_actor_waits=NULL;
    assert(atomic_load(&ctx->state)==GC_RUNNING);
    assert(*(uint64_t *)klassic_gc_read(&root)==UINT64_C(0x12345abcdef));
    atomic_store(&a->returned,1);klassic_gc_shadow_pop_n(1);gc_thread_detach(ctx);
    return NULL;
}
static void blocking_ticket_rounds(MutatorContext *self) {
    for(unsigned round=0;round<16;round++)for(unsigned mode=0;mode<3;mode++) {
        atomic_store(&cycle_wait_gate,mode==0?1:0);
        atomic_store(&cycle_reentry_gate,mode==2?1:0);
        atomic_store(&cycle_gate,1);unsigned before=atomic_load(&cycle_waits);
        AwaitActor actor={.ticket=gc_request_cycle(GC_CYCLE_MAJOR)};
        while(atomic_load(&cycle_gate)!=2)sched_yield();
        gc_enter_native(self);pthread_t waiter;
        assert(pthread_create(&waiter,NULL,cycle_awaiter,&actor)==0);
        if(mode==0) {
            // Complete the collection after native entry but before the
            // request-lock predicate check/cond_wait. No wakeup may be lost.
            while(atomic_load(&cycle_wait_gate)!=2)sched_yield();
            atomic_store(&cycle_gate,5);
            for(;;){pthread_mutex_lock(&g_wake_lock);
                uint64_t status=gc_cycle_step_locked(&g_cycles,&actor.ticket,GC_CY_POLL);
                pthread_mutex_unlock(&g_wake_lock);if(status==GC_CY_OK)break;assert(status==GC_CY_WAIT);sched_yield();}
            atomic_store(&cycle_wait_gate,3);
        }else{
            while(atomic_load(&cycle_waits)==before)sched_yield();
            if(mode==1) {
                // Acquiring wake_lock orders this broadcast after the waiter
                // atomically released it in cond_wait. The cycle is still held.
                pthread_mutex_lock(&g_wake_lock);pthread_cond_broadcast(&g_cycle_complete_cond);pthread_mutex_unlock(&g_wake_lock);
                while(atomic_load(&cycle_waits)<before+2)sched_yield();
                assert(!atomic_load(&actor.returned));
            }
            atomic_store(&cycle_gate,5);
            if(mode==2) {
                while(atomic_load(&cycle_reentry_gate)!=2)sched_yield();
                pthread_mutex_lock(&g_hs_lock);
                assert(g_handshake.members==2&&g_handshake.native==2);
                hs_step_locked(NULL,GC_HS_BEGIN);assert(g_handshake.expected==0);
                __atomic_store_n(&gc_handshake_requested,1,__ATOMIC_RELEASE);
                unsigned prior=atomic_load(&native_waits);pthread_mutex_unlock(&g_hs_lock);
                atomic_store(&cycle_reentry_gate,3);
                while(atomic_load(&native_waits)==prior)sched_yield();
                assert(!atomic_load(&actor.returned));
                pthread_mutex_lock(&g_hs_lock);hs_step_locked(NULL,GC_HS_END);
                __atomic_store_n(&gc_handshake_requested,0,__ATOMIC_RELEASE);pthread_cond_broadcast(&g_hs_cond);pthread_mutex_unlock(&g_hs_lock);
            }
        }
        assert(pthread_join(waiter,NULL)==0&&atomic_load(&actor.returned));
        gc_leave_native(self);
        if(mode==0)assert(atomic_load(&cycle_waits)==before);
        // A completed ticket returns directly without another native wait.
        unsigned after=atomic_load(&cycle_waits);gc_await_cycle(self,actor.ticket);assert(atomic_load(&cycle_waits)==after);
        atomic_store(&cycle_wait_gate,0);atomic_store(&cycle_reentry_gate,0);atomic_store(&cycle_gate,0);
    }
    puts("cycle wait: 48 rooted native-blocked wake-before-wait, spurious-wake and reentry rounds PASS");
}

static void blocking_ticket_cohorts(MutatorContext *self) {
    for(unsigned round=0;round<8;round++)for(unsigned n=1;n<=8;n*=n==2?4:2) {
        atomic_store(&cycle_gate,1);
        CycleTicket ticket=gc_request_cycle(GC_CYCLE_MAJOR);
        while(atomic_load(&cycle_gate)!=2)sched_yield();
        gc_enter_native(self);AwaitActor actors[8]={0};pthread_t waiters[8];
        for(unsigned i=0;i<n;i++){
            actors[i].ticket=ticket;assert(pthread_create(&waiters[i],NULL,cycle_awaiter,&actors[i])==0);
        }
        // Count each owner separately: spurious wakes from one owner must
        // not stand in for another owner reaching the condition wait.
        for(unsigned i=0;i<n;i++)await(&actors[i].waits);
        pthread_mutex_lock(&g_hs_lock);assert(g_handshake.members==n+1&&g_handshake.native==n+1);pthread_mutex_unlock(&g_hs_lock);
        atomic_store(&cycle_gate,5);
        for(unsigned i=0;i<n;i++)assert(pthread_join(waiters[i],NULL)==0&&atomic_load(&actors[i].returned));
        gc_leave_native(self);atomic_store(&cycle_gate,0);
    }
    puts("cycle wait: 24 real-collector 1/2/8-owner completion broadcasts preserve rooted values PASS");
}

static void mixed_ticket_cohorts(MutatorContext *self) {
    for(unsigned round=0;round<8;round++)for(unsigned n=1;n<=8;n*=n==2?4:2) {
        atomic_store(&cycle_gate,1);
        CycleTicket first=gc_request_cycle(GC_CYCLE_MAJOR);
        while(atomic_load(&cycle_gate)!=2)sched_yield();
        CycleTicket later=gc_request_cycle(GC_CYCLE_MAJOR);
        assert(later.cycle==first.cycle+1);
        gc_enter_native(self);
        AwaitActor current[8]={0},next[8]={0};pthread_t current_threads[8],next_threads[8];
        unsigned previous_waits[8];
        for(unsigned i=0;i<n;i++) {
            current[i].ticket=first;next[i].ticket=later;
            assert(pthread_create(&current_threads[i],NULL,cycle_awaiter,&current[i])==0);
            assert(pthread_create(&next_threads[i],NULL,cycle_awaiter,&next[i])==0);
        }
        for(unsigned i=0;i<n;i++){await(&current[i].waits);await(&next[i].waits);}
        pthread_mutex_lock(&g_wake_lock);
        for(unsigned i=0;i<n;i++)previous_waits[i]=atomic_load(&next[i].waits);
        pthread_mutex_unlock(&g_wake_lock);
        // Both cohorts sleep on the same condition. Complete the first cycle
        // and hold the next at START, before any new handshake can run.
        atomic_store(&cycle_gate,3);
        while(atomic_load(&cycle_gate)!=4)sched_yield();
        for(unsigned i=0;i<n;i++) {
            assert(pthread_join(current_threads[i],NULL)==0&&atomic_load(&current[i].returned));
            while(atomic_load(&next[i].waits)==previous_waits[i])sched_yield();
            assert(!atomic_load(&next[i].returned));
        }
        pthread_mutex_lock(&g_wake_lock);
        assert(gc_cycle_step_locked(&g_cycles,&first,GC_CY_POLL)==GC_CY_OK);
        assert(gc_cycle_step_locked(&g_cycles,&later,GC_CY_POLL)==GC_CY_WAIT);
        pthread_mutex_unlock(&g_wake_lock);
        pthread_mutex_lock(&g_hs_lock);
        assert(g_handshake.members==n+1&&g_handshake.native==n+1);
        pthread_mutex_unlock(&g_hs_lock);
        atomic_store(&cycle_gate,5);
        for(unsigned i=0;i<n;i++)assert(pthread_join(next_threads[i],NULL)==0&&atomic_load(&next[i].returned));
        gc_leave_native(self);atomic_store(&cycle_gate,0);
    }
    puts("cycle wait: 24 mixed-ticket 1/2/8-owner cohorts return only for their completed cycle PASS");
}

int main(void) {
    alarm(45); // watchdog only; no timeout is considered a pass
    for (unsigned kind = 0; kind < 10; kind++) reject_misuse(kind);
    MutatorContext *self = gc_thread_attach();
    uint64_t first_identity = self->thread_id;
    // Eight owners retain two regions each. Reserve enough to-space headroom:
    // the legacy selector deliberately declines relocation with <= 1 free
    // region. The fixture must force relocation, not assume every GC moves.
    pthread_mutex_lock(&g_gc_lock);
    while (g_budget < 32) assert(grow_budget());
    pthread_mutex_unlock(&g_gc_lock);
    fresh_ticket_rounds(self);
    blocking_ticket_rounds(self);
    blocking_ticket_cohorts(self);
    mixed_ticket_cohorts(self);
    for (unsigned n = 1; n <= 8; n *= (n == 2 ? 4 : 2)) blocked_progress(n);
    for (unsigned round = 0; round < 16; round++) {
        for (unsigned kind = 0; kind < 3; kind++) handshake_race(kind);
    }
    gc_thread_detach(self);
    assert(gc_thread_current() == NULL);
    self = gc_thread_attach();
    assert(self->thread_id > first_identity);
    assert(self->tlab_start == 0 && self->tlab_top == 0 && self->tlab_end == 0);
    assert(atomic_load(&self->state) == GC_RUNNING);
    gc_thread_detach(self);
    puts("LIFECYCLE PASSED: 1/2/8 native-blocked workers, 96 real cycles, 48 controlled handshakes, 10 misuse/exhaustion cases");
    return 0;
}
