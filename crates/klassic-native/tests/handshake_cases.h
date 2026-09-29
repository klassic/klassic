/* Included by the emitted-code driver. Isolated registries have one owner;
 * production callers must hold the registry lock and never copy live entries. */
#include "gc_handshake.h"
typedef struct {
    HandshakeState h;
    ThreadRegistration registrations[3];
    MutatorContext contexts[3];
} HandshakeFixture;
static Code handshake_code;
static unsigned handshake_cases;
static void run_handshake_threads(void);
static FILE *handshake_vectors;
#define HS_POINTERS(i) offsetof(HandshakeFixture,registrations[i].context), \
    offsetof(HandshakeFixture,registrations[i].registry), \
    offsetof(HandshakeFixture,registrations[i].previous), \
    offsetof(HandshakeFixture,registrations[i].next)
static const uint64_t handshake_pointers[]={offsetof(HandshakeFixture,h.head),HS_POINTERS(0),HS_POINTERS(1),HS_POINTERS(2)};
#undef HS_POINTERS
static uint64_t handshake_normalize(HandshakeFixture *dst,const HandshakeFixture *src) {
    uint64_t mask=0;
    for(unsigned i=0;i<sizeof(handshake_pointers)/8;i++) {
        unsigned char *p=(unsigned char *)dst+handshake_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)) {
            v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);
        }
    }
    return mask;
}

static void handshake_fixture_init(HandshakeFixture *f) {
    memset(f, 0, sizeof(*f));
    for (unsigned i = 0; i < 3; i++) {
        f->contexts[i].abi_version = GC_ABI_VERSION;
        atomic_init(&f->contexts[i].state, GC_RUNNING);
        atomic_init(&f->contexts[i].access_epoch, 0);
        f->contexts[i].tlab_start = UINT64_C(0x123456789abcdef0);
        f->registrations[i].context = (uintptr_t)&f->contexts[i];
    }
}
static void handshake_fixture_copy(HandshakeFixture *dst, const HandshakeFixture *src) {
    memcpy(dst, src, sizeof(*dst));
    uint64_t mask=handshake_normalize(dst,src);
    for(unsigned i=0;i<sizeof(handshake_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)) {
        unsigned char *p=(unsigned char *)dst+handshake_pointers[i];uint64_t v;memcpy(&v,p,8);
        v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static uint64_t compare_handshake_step(HandshakeFixture *actual, HandshakeFixture *reference,
                                       unsigned participant, uint64_t operation, uint64_t expected) {
    ThreadRegistration *a = participant < 3 ? &actual->registrations[participant] : NULL;
    ThreadRegistration *r = participant < 3 ? &reference->registrations[participant] : NULL;
    HandshakeFixture untouched;
    memcpy(&untouched, actual, sizeof(untouched));
    uint64_t c_result = gc_handshake_step_locked(&reference->h, r, operation);
    uint64_t emitted = handshake_code.call(&actual->h, (uintptr_t)a, operation);
    assert(emitted == c_result);
    if (expected != UINT64_MAX) assert(emitted == expected);
    HandshakeFixture x,y;
    memcpy(&x,actual,sizeof(x));memcpy(&y,reference,sizeof(y));
    assert(handshake_normalize(&x,actual)==handshake_normalize(&y,reference));
    assert(!memcmp(&x,&y,sizeof(x)));
    if (emitted != GC_HS_OK) assert(memcmp(actual, &untouched, sizeof(*actual)) == 0);
    if (handshake_vectors) {
        memcpy(&x,&untouched,sizeof(x));memcpy(&y,reference,sizeof(y));
        uint64_t record[]={operation,c_result,participant,handshake_normalize(&x,actual),handshake_normalize(&y,reference)};
        assert(fwrite(record,sizeof(record),1,handshake_vectors)==1);
        assert(fwrite(&x,sizeof(x),1,handshake_vectors)==1);
        assert(fwrite(&y,sizeof(y),1,handshake_vectors)==1);
    }
    handshake_cases++;
    return emitted;
}

// Independent inventory oracle: every live slot appears exactly once, newest
// identity first, with reciprocal links. Used only on known-valid fixtures.
static void handshake_inventory(const HandshakeFixture *f) {
    uint64_t seen=0,count=0,previous=0,last=UINT64_MAX;
    for(uint64_t ptr=f->h.head;ptr;) {
        unsigned i=0;while(i<3&&ptr!=(uintptr_t)&f->registrations[i])i++;
        assert(i<3&&!(seen&(UINT64_C(1)<<i)));seen|=UINT64_C(1)<<i;
        const ThreadRegistration *p=&f->registrations[i];
        assert(p->registry==(uintptr_t)&f->h&&p->registered==1&&p->previous==previous);
        assert(p->identity&&p->identity<last&&p->identity==f->contexts[i].thread_id);
        assert(p->context==(uintptr_t)&f->contexts[i]);
        previous=ptr;last=p->identity;ptr=p->next;count++;
    }
    assert(count==f->h.members);
    for(unsigned i=0;i<3;i++)assert(!!f->registrations[i].registered==!!(seen&(UINT64_C(1)<<i)));
}

static void run_handshake_cases(void) {
    handshake_code = load_code(gc_code_handshake_step, sizeof(gc_code_handshake_step));
    const char *path = getenv("KLASSIC_GC_HANDSHAKE_VECTORS");
    if (path) {
        handshake_vectors = fopen(path, "wb");
        assert(handshake_vectors);
        assert(fwrite("KLHST002", 8, 1, handshake_vectors) == 1);
        uint64_t sizes[] = {sizeof(HandshakeFixture),offsetof(HandshakeFixture,registrations),sizeof(ThreadRegistration),sizeof(handshake_pointers)/8};
        assert(fwrite(sizes, sizeof(sizes), 1, handshake_vectors) == 1);
        assert(fwrite(handshake_pointers,sizeof(handshake_pointers),1,handshake_vectors)==1);
    }
    HandshakeFixture a, r;
    handshake_fixture_init(&a);
    handshake_fixture_init(&r);
#define STEP(p, op, result) compare_handshake_step(&a, &r, p, op, result)
    STEP(0, GC_HS_REGISTER, GC_HS_OK);
    STEP(1, GC_HS_REGISTER, GC_HS_OK);
    assert(a.h.members == 2 && a.contexts[0].thread_id == 1 && a.contexts[1].thread_id == 2);
    STEP(0, GC_HS_REGISTER, GC_HS_INVALID);
    STEP(1, GC_HS_ENTER_NATIVE, GC_HS_OK);
    STEP(3, GC_HS_BEGIN, GC_HS_OK);
    assert(a.h.expected == 1 && a.h.generation == 1);
    STEP(2, GC_HS_REGISTER, GC_HS_WAIT);
    STEP(1, GC_HS_LEAVE_NATIVE, GC_HS_WAIT);
    STEP(0, GC_HS_DETACH, GC_HS_WAIT);
    STEP(0, GC_HS_ACK, GC_HS_OK);
    STEP(0, GC_HS_ACK, GC_HS_DUPLICATE);
    assert(a.h.acknowledged == 1);
    STEP(3, GC_HS_END, GC_HS_OK);
    // A second request lands before the first owner resumes: it stays parked
    // but must acknowledge the new generation exactly once.
    STEP(3, GC_HS_BEGIN, GC_HS_OK);
    STEP(0, GC_HS_RESUME, GC_HS_WAIT);
    STEP(3, GC_HS_END, GC_HS_INCOMPLETE);
    STEP(0, GC_HS_ACK, GC_HS_OK);
    STEP(0, GC_HS_ACK, GC_HS_DUPLICATE);
    STEP(3, GC_HS_END, GC_HS_OK);
    STEP(0, GC_HS_RESUME, GC_HS_OK);
    STEP(1, GC_HS_LEAVE_NATIVE, GC_HS_OK);
    STEP(3, GC_HS_BEGIN, GC_HS_OK);
    assert(a.h.expected == 2);
    STEP(0, GC_HS_ACK, GC_HS_OK);
    STEP(0, GC_HS_ACK, GC_HS_DUPLICATE);
    STEP(3, GC_HS_END, GC_HS_INCOMPLETE);
    STEP(1, GC_HS_ACK, GC_HS_OK);
    STEP(3, GC_HS_END, GC_HS_OK);
    STEP(0, GC_HS_RESUME, GC_HS_OK);
    STEP(1, GC_HS_RESUME, GC_HS_OK);
    assert(gc_context_transition_locked(&a.contexts[0], GC_RUNNING, GC_DETACHING) == 0);
    assert(gc_context_transition_locked(&r.contexts[0], GC_RUNNING, GC_DETACHING) == 0);
    a.contexts[0].tlab_start = r.contexts[0].tlab_start = 0;
    STEP(0, GC_HS_DETACH, GC_HS_OK);
    assert(a.h.members == 1 && a.registrations[0].context == 0);
    STEP(0, GC_HS_DETACH, GC_HS_INVALID);
    memset(&a.contexts[0], 0, sizeof(a.contexts[0]));
    a.contexts[0].abi_version = GC_ABI_VERSION;
    a.registrations[0].context = (uintptr_t)&a.contexts[0];
    handshake_fixture_copy(&r, &a);
    STEP(0, GC_HS_REGISTER, GC_HS_OK);
    assert(a.contexts[0].thread_id == 3);

    // All six detach orders exercise head, middle, tail and final removal.
    for(unsigned first=0;first<3;first++)for(unsigned second=0;second<3;second++)if(first!=second) {
        handshake_fixture_init(&a);handshake_fixture_copy(&r,&a);
        for(unsigned i=0;i<3;i++){STEP(i,GC_HS_REGISTER,GC_HS_OK);handshake_inventory(&a);handshake_inventory(&r);}
        unsigned order[]={first,second,3-first-second};
        for(unsigned n=0;n<3;n++) {
            unsigned i=order[n];a.contexts[i].tlab_start=r.contexts[i].tlab_start=0;
            atomic_store(&a.contexts[i].state,GC_DETACHING);atomic_store(&r.contexts[i].state,GC_DETACHING);
            STEP(i,GC_HS_DETACH,GC_HS_OK);handshake_inventory(&a);handshake_inventory(&r);
        }
        unsigned i=first;memset(&a.contexts[i],0,sizeof(a.contexts[i]));a.contexts[i].abi_version=GC_ABI_VERSION;
        a.registrations[i].context=(uintptr_t)&a.contexts[i];handshake_fixture_copy(&r,&a);
        STEP(i,GC_HS_REGISTER,GC_HS_OK);assert(a.registrations[i].identity==4);
        handshake_inventory(&a);handshake_inventory(&r);
        atomic_store(&a.contexts[i].state,GC_DETACHING);atomic_store(&r.contexts[i].state,GC_DETACHING);
        STEP(i,GC_HS_DETACH,GC_HS_OK);handshake_inventory(&a);handshake_inventory(&r);
    }
    // Invalid links are rejected before writes, including neighbor updates.
    for(unsigned fault=0;fault<28;fault++) {
        handshake_fixture_init(&a);unsigned participant=1;uint64_t op=GC_HS_DETACH;
        if(fault<17) {
            for(unsigned i=0;i<3;i++)assert(gc_handshake_step_locked(&a.h,&a.registrations[i],GC_HS_REGISTER)==0);
            a.contexts[1].tlab_start=0;atomic_store(&a.contexts[1].state,GC_DETACHING);
        } else {
            assert(gc_handshake_step_locked(&a.h,&a.registrations[1],GC_HS_REGISTER)==0);
            participant=0;op=GC_HS_REGISTER;
        }
        switch(fault) {
        case 0:a.h.head=0;break;case 1:a.h.members=0;break;case 2:a.registrations[1].registry=0;break;
        case 3:a.registrations[1].previous++;break;case 4:a.registrations[1].next++;break;
        case 5:a.registrations[2].registry=0;break;case 6:a.registrations[2].registered=0;break;
        case 7:a.registrations[2].next=0;break;case 8:a.registrations[2].identity=2;break;
        case 9:a.registrations[0].registry=0;break;case 10:a.registrations[0].registered=0;break;
        case 11:a.registrations[0].previous=0;break;case 12:a.registrations[0].identity=0;break;
        case 13:a.registrations[0].identity=2;break;case 14:a.registrations[1].previous=0;break;
        case 15:a.h.members=1;break;case 16:a.registrations[1].previous=a.registrations[1].next=0;break;
        case 17:a.h.head++;break;case 18:a.registrations[1].registry=0;break;
        case 19:a.registrations[1].registered=0;break;case 20:a.registrations[1].previous=a.h.head;break;
        case 21:a.registrations[1].identity=0;break;case 22:a.registrations[1].identity=2;break;
        case 23:a.registrations[0].registry=(uintptr_t)&a.h;break;
        case 24:a.registrations[0].previous=a.h.head;break;case 25:a.registrations[0].next=a.h.head;break;
        case 26:a.h.head=(uintptr_t)&a.registrations[0];break;case 27:a.h.head=0;break;
        }
        handshake_fixture_copy(&r,&a);STEP(participant,op,GC_HS_INVALID);
    }

    // Differential invalid-state matrix, including complete/incomplete rounds,
    // stale/future acknowledgement, wrong identities, ABI and active guards.
    for (unsigned op = 0; op < 8; op++) for (unsigned state = 0; state < 6; state++)
    for (unsigned active = 0; active < 2; active++) for (unsigned registered = 0; registered < 2; registered++)
    for (unsigned age = 0; age < 3; age++) for (unsigned native = 0; native < 3; native++)
    for (unsigned guard = 0; guard < 2; guard++) for (unsigned abi = 0; abi < 2; abi++)
    for (unsigned identity = 0; identity < 3; identity++) for (unsigned complete = 0; complete < 2; complete++) {
        handshake_fixture_init(&a);
        a.h = (HandshakeState){3, 2, native, active ? 2 - native : 0,
                              active && complete ? 2 - native : 0, active, 5,(uintptr_t)&a.registrations[1]};
        unsigned tail=registered?0:2;
        a.registrations[1].registered=1;a.registrations[1].identity=4;
        a.registrations[1].registry=(uintptr_t)&a.h;
        a.registrations[1].next=(uintptr_t)&a.registrations[tail];
        a.contexts[1].thread_id=4;
        a.registrations[tail].previous=(uintptr_t)&a.registrations[1];
        a.registrations[tail].registry=(uintptr_t)&a.h;
        a.registrations[tail].registered=1;a.registrations[tail].identity=3;
        a.contexts[tail].thread_id=3;
        a.contexts[0].abi_version = abi ? GC_ABI_VERSION : 0;
        atomic_store(&a.contexts[0].state, state);
        atomic_store(&a.contexts[0].access_epoch, guard);
        a.contexts[0].thread_id = registered ? 1 : 0;
        a.registrations[0].registered = registered;
        a.registrations[0].acknowledged_generation = age == 0 ? 0 : age == 1 ? 3 : 4;
        a.registrations[0].identity = identity;
        handshake_fixture_copy(&r, &a);
        STEP(0, op, UINT64_MAX);
    }
    for (unsigned fault = 0; fault < 10; fault++) {
        handshake_fixture_init(&a);
        uint64_t op = GC_HS_BEGIN, result = GC_HS_INVALID;
        if (fault == 0) { a.h.generation = UINT64_MAX; result = GC_HS_EXHAUSTED; }
        if (fault == 1) { a.h.last_identity = UINT64_MAX; op = GC_HS_REGISTER; result = GC_HS_EXHAUSTED; }
        if (fault == 2) { assert(gc_handshake_step_locked(&a.h,&a.registrations[1],GC_HS_REGISTER)==0); a.h.members = UINT64_MAX; op = GC_HS_REGISTER; result = GC_HS_EXHAUSTED; }
        if (fault == 3) a.h.native = 1;
        if (fault == 4) a.h.active = 2;
        if (fault == 5) a.h.expected = 1;
        if (fault == 6) a.h.acknowledged = 1;
        if (fault == 7) a.h.active = 1; // active generation zero
        if (fault == 8) op = UINT64_MAX;
        if (fault == 9) { a.registrations[0].context = 0; op = GC_HS_REGISTER; }
        handshake_fixture_copy(&r, &a);
        STEP(0, op, result);
    }
    // Detach cannot erase context ownership before roots/TLAB/buffers finish.
    for(unsigned fault=0;fault<15;fault++) {
        handshake_fixture_init(&a);a.contexts[0].tlab_start=0;
        assert(gc_handshake_step_locked(&a.h,&a.registrations[0],GC_HS_REGISTER)==0);
        atomic_store(&a.contexts[0].state,GC_DETACHING);
        uint64_t expected=GC_HS_WAIT;
        switch(fault){
        case 0:a.contexts[0].root_segments=8;break;
        case 1:a.contexts[0].retiring_roots=1;break;
        case 2:a.contexts[0].tlab_start=16;break;
        case 3:a.contexts[0].tlab_top=32;break;
        case 4:a.contexts[0].tlab_end=48;break;
        default:{BufferCursor *b=fault<10?&a.contexts[0].satb:&a.contexts[0].remembered;
            switch((fault-5)%5){
            case 0:b->top=1;break;
            case 1:b->top=1;atomic_store(&b->published,1);break;
            case 2:atomic_store(&b->published,1);expected=GC_HS_INVALID;break;
            case 3:atomic_store(&b->consumed,1);expected=GC_HS_INVALID;break;
            case 4:b->top=UINT64_MAX;atomic_store(&b->published,UINT64_MAX);atomic_store(&b->consumed,UINT64_MAX);expected=GC_HS_OK;break;
            }break;}
        }
        handshake_fixture_copy(&r,&a);STEP(0,GC_HS_DETACH,expected);
    }
    handshake_fixture_init(&a);a.contexts[0].tlab_start=0;
    assert(gc_handshake_step_locked(&a.h,&a.registrations[0],GC_HS_REGISTER)==0);
    atomic_store(&a.contexts[0].state,GC_DETACHING);a.contexts[0].retiring_roots=1;
    handshake_fixture_copy(&r,&a);
    for(unsigned round=0;round<2;round++) {
        STEP(3,GC_HS_BEGIN,GC_HS_OK);STEP(0,GC_HS_DETACH,GC_HS_WAIT);
        STEP(0,GC_HS_ACK,GC_HS_OK);STEP(0,GC_HS_ACK,GC_HS_DUPLICATE);
        STEP(3,GC_HS_END,GC_HS_OK);STEP(0,GC_HS_RESUME,GC_HS_OK);
        assert(atomic_load(&a.contexts[0].state)==GC_DETACHING);
    }
#undef STEP
    run_handshake_threads();
    if (handshake_vectors) {
        assert(fclose(handshake_vectors) == 0);
        handshake_vectors = NULL;
    }
    unload_code(handshake_code);
    printf("%u C/emitted handshake sequence, failure and preservation cases PASS\n", handshake_cases);
}

static HandshakeState live_handshake;
static ThreadRegistration live_registrations[8];
static MutatorContext live_contexts[8];
static _Atomic uint64_t live_progress[8];
static int stop_handshake_workers;
static Code state_code;
static void hs_lock(void) {
#ifdef _WIN32
    EnterCriticalSection(&latch.mutex);
#else
    assert(pthread_mutex_lock(&latch.mutex) == 0);
#endif
}
static void hs_unlock(void) {
#ifdef _WIN32
    LeaveCriticalSection(&latch.mutex);
#else
    assert(pthread_mutex_unlock(&latch.mutex) == 0);
#endif
}
static void hs_notify(void) {
#ifdef _WIN32
    WakeAllConditionVariable(&latch.condition);
#else
    assert(pthread_cond_broadcast(&latch.condition) == 0);
#endif
}
static void hs_wait(void) {
#ifdef _WIN32
    assert(SleepConditionVariableCS(&latch.condition, &latch.mutex, 30000));
#else
    assert(pthread_cond_wait(&latch.condition, &latch.mutex) == 0);
#endif
}
static uint64_t live_step(unsigned index, uint64_t operation) {
    return handshake_code.call(&live_handshake,
        index < 8 ? (uintptr_t)&live_registrations[index] : 0, operation);
}
static THREAD_RESULT handshake_actor(void *arg) {
    unsigned index = (unsigned)(uintptr_t)arg;
    uint64_t progressed_generation = UINT64_MAX;
    hs_lock();
    assert(live_step(index, GC_HS_REGISTER) == GC_HS_OK);
    hs_notify();
    while (!stop_handshake_workers) {
        if (live_handshake.active) {
            uint64_t generation = live_handshake.generation;
            assert(live_step(index, GC_HS_ACK) == GC_HS_OK);
            assert(live_step(index, GC_HS_ACK) == GC_HS_DUPLICATE);
            hs_notify();
            while (live_handshake.active && live_handshake.generation == generation) hs_wait();
        } else if (progressed_generation != live_handshake.generation) {
            assert(live_step(index, GC_HS_RESUME) == GC_HS_OK);
            progressed_generation = live_handshake.generation;
            hs_unlock();
            // This witnesses test work outside the coordinator lock, not
            // managed-heap allocation or collector throughput.
            atomic_fetch_add_explicit(&live_progress[index], 1, memory_order_release);
            hs_lock();
            hs_notify();
        } else {
            hs_wait();
        }
    }
    assert(state_code.call(&live_contexts[index], GC_RUNNING, GC_DETACHING) == GC_CONTEXT_OK);
    assert(live_step(index, GC_HS_DETACH) == GC_HS_OK);
    hs_notify();
    hs_unlock();
    return 0;
}
static int all_progressed(unsigned count, uint64_t target) {
    for (unsigned i = 0; i < count; i++) {
        if (atomic_load_explicit(&live_progress[i], memory_order_acquire) < target) return 0;
    }
    return 1;
}
static void run_handshake_threads(void) {
    state_code = load_code(gc_code_context_transition, sizeof(gc_code_context_transition));
    for (unsigned count = 1; count <= 8; count *= (count == 2 ? 4 : 2)) {
        Thread threads[8];
        memset(&live_handshake, 0, sizeof(live_handshake));
        memset(live_contexts, 0, sizeof(live_contexts));
        memset(live_registrations, 0, sizeof(live_registrations));
        stop_handshake_workers = 0;
        for (unsigned i = 0; i < count; i++) {
            live_contexts[i].abi_version = GC_ABI_VERSION;
            atomic_init(&live_contexts[i].state, GC_RUNNING);
            atomic_init(&live_contexts[i].access_epoch, 0);
            atomic_store(&live_progress[i], 0);
            live_registrations[i].context = (uintptr_t)&live_contexts[i];
#ifdef _WIN32
            threads[i] = CreateThread(NULL, 0, handshake_actor, (void *)(uintptr_t)i, 0, NULL);
            assert(threads[i]);
#else
            assert(pthread_create(&threads[i], NULL, handshake_actor, (void *)(uintptr_t)i) == 0);
#endif
        }
        hs_lock();
        while (live_handshake.members != count || !all_progressed(count, 1)) hs_wait();
        for (unsigned round = 0; round < 64; round++) {
            for (unsigned consecutive = 0; consecutive < 2; consecutive++) {
                assert(live_step(8, GC_HS_BEGIN) == GC_HS_OK);
                assert(live_handshake.expected == count);
                hs_notify();
                while (live_handshake.acknowledged != count) hs_wait();
                assert(live_step(8, GC_HS_END) == GC_HS_OK);
                hs_notify();
                // Keep the lock through the next BEGIN: owners must acknowledge
                // again before any has resumed from the preceding request.
            }
            while (!all_progressed(count, round + 2)) hs_wait();
        }
        assert(live_handshake.generation == 128);
        stop_handshake_workers = 1;
        hs_notify();
        while (live_handshake.members) hs_wait();
        assert(!live_handshake.head);
        hs_unlock();
        for (unsigned i = 0; i < count; i++) {
#ifdef _WIN32
            assert(WaitForSingleObject(threads[i], 30000) == WAIT_OBJECT_0);
            assert(CloseHandle(threads[i]));
#else
            assert(pthread_join(threads[i], NULL) == 0);
#endif
            assert(atomic_load(&live_contexts[i].state) == GC_DETACHED);
            assert(live_registrations[i].registered == 0 && live_registrations[i].context == 0);
        }
    }
    unload_code(state_code);
    puts("emitted handshake: 1/2/8 owners, 384 rounds, duplicate suppression and outside-lock progress PASS");
}
