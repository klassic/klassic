/* White-box boundary regressions: compile this translation unit alone. */
#define KLASSIC_GC_TEST_HOOKS
#include "klassic_gc.c"
#include <assert.h>
#include <sys/wait.h>
#include <unistd.h>

/* Latch the scanner at the header seam until the marker is running. The
 * registry has one fixture-owned logical ID; these tests do not move it. */
static const void *tracked_object;
static pthread_mutex_t hook_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t hook_cond = PTHREAD_COND_INITIALIZER;
static int scanner_ready, marker_ready;
static GcTestEvent tracked_event = GC_TEST_BEFORE_HEADER_SCAN;
static int mark_retry_enabled;
static _Atomic unsigned mark_end_attempts, retry_injected, retry_scanned;
static _Atomic uint64_t mutator_progress;

uint64_t gc_test_object_id(const void *object) {
    return object && object == tracked_object ? 1 : 0;
}

void gc_test_hook(GcTestEvent event, uint64_t object_id) {
    if(mark_retry_enabled) {
        if(event==GC_TEST_BEFORE_MARK_END) {
            if(atomic_fetch_add(&mark_end_attempts,1)==0) {
                /* A retained, already-live parent is enqueued after the
                 * concurrent empty observation. This injects queue work,
                 * not a new unrooted object or a fabricated liveness bit. */
                worklist_push((void *)tracked_object);
                atomic_store(&retry_injected,1);
            }
        } else if(event==GC_TEST_BEFORE_HEADER_SCAN) {
            assert(!__atomic_load_n(&gc_handshake_requested,__ATOMIC_ACQUIRE));
            if(atomic_load(&retry_injected)&&object_id==1) {
                uint64_t before=atomic_load(&mutator_progress);
                while(atomic_load(&mutator_progress)==before)sched_yield();
                atomic_fetch_add(&retry_scanned,1);
            }
        }
        return;
    }
    if (event != tracked_event || object_id != 1) {
        return;
    }
    pthread_mutex_lock(&hook_lock);
    if (!scanner_ready) {
        scanner_ready = 1;
        pthread_cond_broadcast(&hook_cond);
        while (!marker_ready) {
            pthread_cond_wait(&hook_cond, &hook_lock);
        }
    }
    pthread_mutex_unlock(&hook_lock);
}

static void expect_rejection(const char *mode, const char *diagnostic) {
    int output[2];
    assert(pipe(output) == 0);
    pid_t pid = fork();
    assert(pid >= 0);
    if (pid == 0) {
        close(output[0]);
        assert(dup2(output[1], STDERR_FILENO) >= 0);
        close(output[1]);
        if (!strcmp(mode, "read")) {
            void *slot = NULL;
            (void)klassic_gc_read(&slot);
        } else if (!strcmp(mode, "slow")) {
            void *slot = NULL;
            (void)klassic_gc_load_barrier_slow(0, &slot);
        } else if (!strcmp(mode, "write")) {
            void *slot = NULL;
            klassic_gc_write(&slot, NULL);
        } else {
            uint64_t size = strtoull(mode, NULL, 10);
            (void)klassic_gc_alloc(size, KLASSIC_GC_RAW_BYTES);
        }
        _exit(0);
    }
    close(output[1]);
    char message[1024];
    ssize_t n = read(output[0], message, sizeof(message) - 1);
    assert(n >= 0);
    message[n] = '\0';
    close(output[0]);
    int status;
    assert(waitpid(pid, &status, 0) == pid);
    assert(WIFEXITED(status) && WEXITSTATUS(status) == 1);
    assert(strstr(message, diagnostic));
}

static int stop_observer;
static void *observe_stats(void *unused) {
    (void)unused;
    uint64_t previous = 0;
    while (!__atomic_load_n(&stop_observer, __ATOMIC_ACQUIRE)) {
        uint64_t current = klassic_gc_collection_count();
        assert(current >= previous);
        previous = current;
        assert(klassic_gc_live_region_count() <= RESERVE_REGIONS);
    }
    return NULL;
}

static void *mark_header(void *user) {
    uint64_t *header = (uint64_t *)user - 2;
    pthread_mutex_lock(&hook_lock);
    while (!scanner_ready) {
        pthread_cond_wait(&hook_cond, &hook_lock);
    }
    marker_ready = 1;
    pthread_cond_broadcast(&hook_cond);
    pthread_mutex_unlock(&hook_lock);
    do {
        __atomic_fetch_or(header, g_header_mark, __ATOMIC_RELAXED);
    } while (!__atomic_load_n(&stop_observer, __ATOMIC_ACQUIRE));
    return NULL;
}

typedef struct { void **slot; void *replacement; } HealWriter;
static void *write_during_heal(void *argument) {
    HealWriter *writer=argument;
    klassic_gc_register_thread();
    pthread_mutex_lock(&hook_lock);
    while(!scanner_ready)assert(pthread_cond_wait(&hook_cond,&hook_lock)==0);
    klassic_gc_write(writer->slot,writer->replacement);
    marker_ready=1;
    assert(pthread_cond_broadcast(&hook_cond)==0);
    pthread_mutex_unlock(&hook_lock);
    klassic_gc_unregister_thread();
    return NULL;
}

/* The legacy public collector APIs are exercised here, not the generational
 * leaf harness. No cycle is requested: both targets remain physically live
 * while the hook forces a real null/non-null store between observation and
 * attempted healing. A failed heal must still return/mark its old observation. */
static void heal_store_races(void) {
    klassic_gc_init();
    void *original=klassic_gc_alloc(16,KLASSIC_GC_RAW_BYTES);
    void *replacement=klassic_gc_alloc(16,KLASSIC_GC_RAW_BYTES);
    void **owner=klassic_gc_alloc(16,KLASSIC_GC_POINTER_RECORD);
    tracked_object=original;
    __atomic_store_n(&g_phase,GC_PHASE_MARK,__ATOMIC_RELAXED);
    for(unsigned path=0;path<2;path++)for(unsigned round=0;round<64;round++) {
        tracked_event=path?GC_TEST_BEFORE_TRACE_HEAL:GC_TEST_BEFORE_LOAD_HEAL;
        scanner_ready=marker_ready=0;
        __atomic_fetch_and((uint64_t *)original-2,~(uint64_t)KLASSIC_GC_HMARK_BOTH,__ATOMIC_RELAXED);
        // This fixture isolates healing; the replacement is already marked.
        __atomic_fetch_or((uint64_t *)replacement-2,g_header_mark,__ATOMIC_RELAXED);
        __atomic_store_n((uint64_t *)&owner[0],(uintptr_t)original|GC_COLOR_M0,__ATOMIC_RELAXED);
        HealWriter writer={owner,round%2?replacement:NULL};
        pthread_t thread;
        assert(pthread_create(&thread,NULL,write_during_heal,&writer)==0);
        if(path) {worklist_push(owner);gc_trace(3);}
        else assert(klassic_gc_read(owner)==original);
        assert(pthread_join(thread,NULL)==0);
        uint64_t expected=writer.replacement?((uintptr_t)writer.replacement|gc_good_color):0;
        assert(__atomic_load_n((uint64_t *)owner,__ATOMIC_RELAXED)==expected);
        assert(worklist_pop()==original&&worklist_empty());
    }
    __atomic_store_n(&g_phase,GC_PHASE_IDLE,__ATOMIC_RELAXED);
    klassic_gc_unregister_thread();
    puts("legacy heal: 128 load/trace races preserve null and replacement stores PASS");
}

static void mark_end_retries(void) {
    klassic_gc_init();
    for(unsigned round=0;round<16;round++) {
        void *root=klassic_gc_alloc(16,KLASSIC_GC_POINTER_RECORD);
        klassic_gc_shadow_push(&root);
        uint64_t *child=klassic_gc_alloc(16,KLASSIC_GC_RAW_BYTES);
        *child=UINT64_C(0xabcdef00)+round;
        klassic_gc_write(root,child);
        klassic_gc_write((void **)root+1,NULL);
        tracked_object=root;mark_retry_enabled=1;
        atomic_store(&mark_end_attempts,0);atomic_store(&retry_injected,0);
        atomic_store(&retry_scanned,0);atomic_store(&mutator_progress,0);
        CycleTicket ticket=gc_request_cycle(GC_CYCLE_MAJOR);
        for(;;) {
            klassic_gc_safepoint();
            atomic_fetch_add(&mutator_progress,1);
            pthread_mutex_lock(&g_wake_lock);
            uint64_t status=gc_cycle_step_locked(&g_cycles,&ticket,GC_CY_POLL);
            pthread_mutex_unlock(&g_wake_lock);
            if(status==GC_CY_OK)break;
            assert(status==GC_CY_WAIT);sched_yield();
        }
        assert(atomic_load(&mark_end_attempts)==2&&atomic_load(&retry_scanned)==1);
        assert(*((uint64_t *)klassic_gc_read(root))==UINT64_C(0xabcdef00)+round);
        mark_retry_enabled=0;
        klassic_gc_shadow_pop_n(1);
    }
    klassic_gc_unregister_thread();
    puts("legacy mark end: 16 late-frontier retries trace outside pause with mutator progress PASS");
}

static void bounded_trace(void) {
    klassic_gc_init();
    for(unsigned pattern=0;pattern<3;pattern++)for(unsigned kind=0;kind<2;kind++) {
        uint64_t tag=kind?KLASSIC_GC_POINTER_LIST:KLASSIC_GC_POINTER_RECORD;
        void *root=klassic_gc_alloc(2048*8,tag);
        klassic_gc_shadow_push(&root);
        uint64_t *leaf=klassic_gc_alloc(16,KLASSIC_GC_RAW_BYTES);
        *leaf=UINT64_C(0x123456789);
        uint64_t *fields=root;
        unsigned start=kind?1:0;
        if(kind)fields[0]=UINT64_MAX; // nonpointer length must never be chased
        for(unsigned i=start;i<2048;i++)fields[i]=pattern==2||(pattern==1&&(i&1))?0:((uintptr_t)leaf|GC_COLOR_M0);
        // An already marked child isolates the retained scan from queue jobs.
        __atomic_fetch_or(leaf-2,g_header_mark,__ATOMIC_RELAXED);
        worklist_push(root);
        gc_trace(0);
        assert(g_worklist_top==1&&!g_trace_cursor);
        gc_trace(1); // charge opening the object, not its entire payload
        assert(g_trace_cursor==(void **)fields+start);
        unsigned scanned=start,quanta=0;
        const unsigned budgets[]={1,7,31,512};
        while(scanned<2048) {
            assert(!g_worklist_top&&!worklist_empty());
            unsigned budget=budgets[quanta++%4];
            gc_trace(budget);
            unsigned end=scanned+budget;if(end>2048)end=2048;
            for(unsigned i=scanned;i<end;i++)assert(fields[i]==(pattern==2||(pattern==1&&(i&1))?0:((uintptr_t)leaf|gc_good_color)));
            assert(g_trace_cursor==(end==2048?NULL:(void **)fields+end));
            if(end<2048)assert(fields[end]==(pattern==2||(pattern==1&&(end&1))?0:((uintptr_t)leaf|GC_COLOR_M0)));
            if(kind)assert(fields[0]==UINT64_MAX);
            scanned=end;
        }
        assert(worklist_empty());
        // Exercise completion and relocation through the real cycle driver.
        klassic_gc_collect();
        fields=root;
        for(unsigned i=start;i<2048;i++) {
            uint64_t *value=klassic_gc_read((void **)&fields[i]);
            if(pattern==2||(pattern==1&&(i&1)))assert(!value);
            else assert(*value==UINT64_C(0x123456789));
        }
        klassic_gc_shadow_pop_n(1);
    }
    klassic_gc_unregister_thread();
    puts("legacy trace: record/list field budgets retain active scans through empty queues PASS");
}

int main(int argc, char **argv) {
    assert(argc == 2);
    if (!strcmp(argv[1], "trace-budget")) {
        bounded_trace();
    } else if (!strcmp(argv[1], "mark-retry")) {
        mark_end_retries();
    } else if (!strcmp(argv[1], "heal")) {
        heal_store_races();
    } else if (!strcmp(argv[1], "overflow")) {
        /* Cover header addition and alignment rounding separately. */
        expect_rejection("18446744073709551615", "invalid object size");
        expect_rejection("18446744073709551599", "invalid object size");
        expect_rejection("18446744073709551585", "invalid object size");
    } else if (!strcmp(argv[1], "attach")) {
        expect_rejection("read", "requires an attached mutator");
        expect_rejection("write", "requires an attached mutator");
        expect_rejection("slow", "requires an attached mutator");
    } else if (!strcmp(argv[1], "stats")) {
        klassic_gc_init();
        pthread_t observer;
        assert(pthread_create(&observer, NULL, observe_stats, NULL) == 0);
        for (int i = 0; i < 200000; i++) {
            (void)klassic_gc_alloc(48, KLASSIC_GC_RAW_BYTES);
        }
        klassic_gc_collect();
        __atomic_store_n(&stop_observer, 1, __ATOMIC_RELEASE);
        assert(pthread_join(observer, NULL) == 0);
        klassic_gc_unregister_thread();
    } else if (!strcmp(argv[1], "header")) {
        klassic_gc_init();
        void *object = klassic_gc_alloc(16, KLASSIC_GC_POINTER_RECORD);
        tracked_object = object;
        pthread_t marker;
        assert(pthread_create(&marker, NULL, mark_header, object) == 0);
        /* No requested cycle: exercise the real scanner and atomic marker
         * access pattern without unrelated phase changes. TSan checks the
         * conflicting accesses even when the same bit is repeatedly set. */
        for (int i = 0; i < 10000; i++) {
            worklist_push(object);
            gc_trace(3);
        }
        __atomic_store_n(&stop_observer, 1, __ATOMIC_RELEASE);
        assert(pthread_join(marker, NULL) == 0);
        klassic_gc_unregister_thread();
    } else {
        assert(!strcmp(argv[1], "zero"));
        klassic_gc_init();
        (void)klassic_gc_alloc(REGION_SIZE - 32, KLASSIC_GC_RAW_BYTES);
        void *empty = klassic_gc_alloc(0, KLASSIC_GC_RAW_BYTES);
        assert((uint8_t *)empty == g_region_base + REGION_SIZE);
        /* Select only the header's region, not the next region. */
        __atomic_store_n(&g_from_set[0], 1, __ATOMIC_RELAXED);
        assert(in_from_set((uint64_t)(uintptr_t)empty));
        __atomic_store_n(&g_from_set[0], 0, __ATOMIC_RELAXED);
        /* Reservation-end user pointers have the same ownership rule. */
        __atomic_store_n(&g_from_set[RESERVE_REGIONS - 1], 1, __ATOMIC_RELAXED);
        assert(in_from_set((uint64_t)(uintptr_t)(g_region_base + RESERVE_BYTES)));
        __atomic_store_n(&g_from_set[RESERVE_REGIONS - 1], 0, __ATOMIC_RELAXED);
        klassic_gc_shadow_push(&empty);
        uintptr_t original = (uintptr_t)empty;
        (void)klassic_gc_alloc(16, KLASSIC_GC_RAW_BYTES); /* retire first TLAB */
        klassic_gc_collect();
        assert((uintptr_t)empty != original); /* rooted zero-size object moved */
        assert(block_size(*((uint64_t *)empty - 2)) == 16);
        klassic_gc_collect(); /* reclaim its old page after remapping */
        assert(block_size(*((uint64_t *)empty - 2)) == 16);
        klassic_gc_shadow_pop_n(1);
        klassic_gc_unregister_thread();
    }
    printf("gc_boundary_test: %s PASS\n", argv[1]);
    return 0;
}
