/* Production legacy C store barriers. Scheduling hooks prevent relocation
 * across borrowed raw test pointers; real-cycle checks run before selection. */
#define KLASSIC_GC_TEST_HOOKS
#include "klassic_gc.c"
#include <assert.h>
static void *parent_identity,*child_identity;
static _Atomic unsigned mode,ready,resume,end_checks;
static _Atomic unsigned cas_visits;
static _Thread_local unsigned intercept_store;
uint64_t gc_test_object_id(const void *object) {
    return object==parent_identity?1:object==child_identity?2:0;
}
void gc_test_hook(GcTestEvent event,uint64_t id) {
    if(atomic_load(&mode)==1) {
        if(event==GC_TEST_BEFORE_HEADER_SCAN&&id==1) {
            atomic_store(&ready,1);
            while(!atomic_load(&resume))sched_yield();
        }
        if(event==GC_TEST_BEFORE_MARK_END) {
            // The leaf lost its only edge before its parent was scanned.
            // Inspect its mark before any sweep/selection/source retirement.
            assert(__atomic_load_n((uint64_t *)child_identity-2,__ATOMIC_RELAXED)&g_header_mark);
            atomic_fetch_add(&end_checks,1);
        }
    }
    if(event==GC_TEST_BEFORE_STORE_CAS&&intercept_store) {
        if(atomic_fetch_add(&cas_visits,1)==0) {
            atomic_store(&ready,1);
            while(!atomic_load(&resume))sched_yield();
        }
    }
}
static void last_edge_cycles(void) {
    for(unsigned round=0;round<16;round++) {
        void *root=klassic_gc_alloc(16,KLASSIC_GC_POINTER_RECORD);
        klassic_gc_shadow_push(&root);
        void *leaf=klassic_gc_alloc(16,KLASSIC_GC_RAW_BYTES);
        klassic_gc_write(root,leaf);klassic_gc_write((void **)root+1,NULL);
        parent_identity=root;child_identity=leaf;
        atomic_store(&ready,0);atomic_store(&resume,0);atomic_store(&end_checks,0);
        atomic_store(&mode,1);
        CycleTicket ticket=gc_request_cycle(GC_CYCLE_MAJOR);
        while(!atomic_load(&ready)){klassic_gc_safepoint();sched_yield();}
        klassic_gc_write(root,NULL);
        atomic_store(&resume,1);
        gc_await_cycle(gc_thread_current(),ticket);
        assert(atomic_load(&end_checks)>=1&&!klassic_gc_read(root));
        atomic_store(&mode,0);klassic_gc_shadow_pop_n(1);
    }
    puts("legacy store: 16 real mark cycles retain a deleted last edge PASS");
}
static void *collision_slot,*middle_value;
static void *competing_writer(void *unused) {
    (void)unused;klassic_gc_register_thread();
    while(!atomic_load(&ready))sched_yield();
    klassic_gc_write(collision_slot,middle_value);
    atomic_store(&resume,1);klassic_gc_unregister_thread();return NULL;
}
static void black_init_and_collision(void) {
    // No cycle is requested in these white-box schedules; the collector is
    // asleep, so phase/poison injection cannot race collection or reclamation.
    for(unsigned round=0;round<32;round++) {
        void *old=klassic_gc_alloc(16,KLASSIC_GC_RAW_BYTES);
        void *next=klassic_gc_alloc(16,KLASSIC_GC_RAW_BYTES);
        middle_value=klassic_gc_alloc(16,KLASSIC_GC_RAW_BYTES);
        if(g_self->context.tlab_end-g_self->context.tlab_top<32)
            (void)klassic_gc_alloc(16,KLASSIC_GC_RAW_BYTES);
        uint64_t *unused=(uint64_t *)(uintptr_t)g_self->context.tlab_top;
        unused[2]=UINT64_MAX;unused[3]=UINT64_MAX; // reused pointer payload
        __atomic_store_n(&g_phase,GC_PHASE_MARK,__ATOMIC_RELAXED);
        void **owner=klassic_gc_alloc(16,KLASSIC_GC_POINTER_RECORD);
        assert(!owner[0]&&!owner[1]);
        assert(*((uint64_t *)owner-2)&g_header_mark); // allocated black
        klassic_gc_write(owner,old);
        assert(__atomic_load_n((uint64_t *)old-2,__ATOMIC_RELAXED)&g_header_mark);
        atomic_store(&ready,0);atomic_store(&resume,0);atomic_store(&cas_visits,0);
        collision_slot=owner;pthread_t writer;
        assert(pthread_create(&writer,NULL,competing_writer,NULL)==0);
        intercept_store=1;klassic_gc_write(owner,next);intercept_store=0;
        assert(pthread_join(writer,NULL)==0);
        assert(atomic_load(&cas_visits)==2);
        assert(__atomic_load_n((uint64_t *)owner,__ATOMIC_RELAXED)==((uintptr_t)next|gc_good_color));
        assert(__atomic_load_n((uint64_t *)next-2,__ATOMIC_RELAXED)&g_header_mark);
        assert(__atomic_load_n((uint64_t *)middle_value-2,__ATOMIC_RELAXED)&g_header_mark);
        // A controlled immutable forwarding input must publish its canonical
        // target, not a good-colored ghost. Restore the fixture header before
        // the queued original is scanned; no actual relocation runs here.
        __atomic_store_n((uint64_t *)old-1,(uintptr_t)next,__ATOMIC_RELEASE);
        klassic_gc_write(owner+1,old);
        assert(__atomic_load_n((uint64_t *)(owner+1),__ATOMIC_RELAXED)==((uintptr_t)next|gc_good_color));
        __atomic_store_n((uint64_t *)old-1,KLASSIC_GC_RAW_BYTES,__ATOMIC_RELEASE);
        while(!worklist_empty())gc_trace(7);
        __atomic_store_n(&g_phase,GC_PHASE_IDLE,__ATOMIC_RELAXED);
    }
    puts("legacy store: 32 zeroed black initializations and competing-store retries PASS");
}
int main(void) {
    klassic_gc_init();last_edge_cycles();
    black_init_and_collision();
    klassic_gc_unregister_thread();return 0;
}
