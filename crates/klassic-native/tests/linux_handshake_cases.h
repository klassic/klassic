/* Linux x86-64 only: the worker has custom FS, so it executes emitted code and
 * raw syscalls exclusively. C only owns storage and orchestrates requests. */
#ifdef GC_LINUX_WORKERS
typedef struct {
    HandshakeState *handshake;
    ThreadRegistration *registration;
    _Atomic uint32_t *lock, *coordinator_event, *owner_event, *stop;
    _Atomic uint64_t *progress;
    uint64_t tls_observed, entry_sp;
} NativeOwnerControl;
_Static_assert(sizeof(NativeOwnerControl) == 72, "raw owner control size");
_Static_assert(offsetof(NativeOwnerControl, handshake) == 0, "handshake");
_Static_assert(offsetof(NativeOwnerControl, registration) == 8, "registration");
_Static_assert(offsetof(NativeOwnerControl, lock) == 16, "lock");
_Static_assert(offsetof(NativeOwnerControl, coordinator_event) == 24, "coordinator event");
_Static_assert(offsetof(NativeOwnerControl, owner_event) == 32, "owner event");
_Static_assert(offsetof(NativeOwnerControl, stop) == 40, "stop");
_Static_assert(offsetof(NativeOwnerControl, progress) == 48, "progress");
_Static_assert(offsetof(NativeOwnerControl, tls_observed) == 56, "TLS");
_Static_assert(offsetof(NativeOwnerControl, entry_sp) == 64, "SP");
typedef struct { _Atomic uint32_t value; uint32_t canary; } NativeWord;
static Code native_lock_code, native_unlock_code, native_notify_code, native_park_code;
static NativeWord native_mutex, native_coordinator_event;
static int64_t native_call(Code code, void *arg) {
    return checked_call(code.memory, (uintptr_t)arg, 0, 0, 0, 0);
}
static void native_lock(void) { assert(native_call(native_lock_code, &native_mutex.value) == 0); }
static void native_unlock(void) { assert(native_call(native_unlock_code, &native_mutex.value) == 0); }
static void native_notify(_Atomic uint32_t *event) {
    assert(native_call(native_notify_code, event) == 0);
}
static void native_wait(void) {
    // Only this coordinator consumes this event; workers only publish to it.
    atomic_store_explicit(&native_coordinator_event.value, 0, memory_order_release);
    native_unlock();
    int64_t status = native_call(native_park_code, &native_coordinator_event.value);
    assert(status == 0 || status == -11 || status == -4);
    native_lock();
}
static void run_native_handshake(void) {
    Code abi = load_code(gc_code_abi_call, sizeof(gc_code_abi_call));
    abi_call = (AbiFn)abi.memory;
    Code start = load_code(gc_code_worker_start, sizeof(gc_code_worker_start));
    Code join = load_code(gc_code_worker_join, sizeof(gc_code_worker_join));
    Code alloc = load_code(gc_code_stack_alloc, sizeof(gc_code_stack_alloc));
    Code release = load_code(gc_code_stack_free, sizeof(gc_code_stack_free));
    Code entry = load_code(gc_code_handshake_worker, sizeof(gc_code_handshake_worker));
    Code step_code = load_code(gc_code_handshake_step, sizeof(gc_code_handshake_step));
    native_lock_code = load_code(gc_code_registry_lock, sizeof(gc_code_registry_lock));
    native_unlock_code = load_code(gc_code_registry_unlock, sizeof(gc_code_registry_unlock));
    native_notify_code = load_code(gc_code_registry_notify, sizeof(gc_code_registry_notify));
    native_park_code = load_code(gc_code_worker_park, sizeof(gc_code_worker_park));
    // On x86 the memory instruction accepts this misalignment; the futex must
    // reject it. Propagate errors instead of spinning or pretending to wake.
    _Alignas(8) unsigned char misaligned[8] = {0};
    uint32_t one = 1;
    memcpy(misaligned + 1, &one, sizeof(one));
    assert(native_call(native_lock_code, misaligned + 1) == -22);
    assert(native_call(native_unlock_code, misaligned + 1) == -22);
    assert(native_call(native_notify_code, misaligned + 1) == -22);
    const uint64_t stack_bytes = 65536;
    for (unsigned count = 1; count <= 8; count *= (count == 2 ? 4 : 2)) {
        HandshakeState h = {0};
        ThreadRegistration registrations[8] = {0};
        MutatorContext contexts[8] = {0};
        NativeOwnerControl controls[8] = {0};
        NativeWord events[8], tids[8], stop = {0, 0xdeadbeef};
        _Atomic uint64_t progress[8];
        uint64_t tls[8][2];
        void *stacks[8];
        native_mutex = (NativeWord){0, 0xdeadbeef};
        native_coordinator_event = (NativeWord){0, 0xdeadbeef};
        for (unsigned i = 0; i < count; i++) {
            contexts[i].abi_version = GC_ABI_VERSION;
            atomic_init(&contexts[i].state, GC_RUNNING);
            atomic_init(&contexts[i].access_epoch, 0);
            registrations[i].context = (uintptr_t)&contexts[i];
            atomic_init(&progress[i], 0);
            events[i] = (NativeWord){0, 0xdeadbeef};
            tids[i] = (NativeWord){0, 0xdeadbeef};
            controls[i] = (NativeOwnerControl){&h, &registrations[i], &native_mutex.value,
                &native_coordinator_event.value, &events[i].value, &stop.value, &progress[i], 0, 0};
            tls[i][0] = (uintptr_t)tls[i];
            tls[i][1] = (uintptr_t)&contexts[i];
            int64_t mapping = checked_call(alloc.memory, stack_bytes, 0, 0, 0, 0);
            assert(mapping > 0);
            stacks[i] = (void *)(uintptr_t)mapping;
            void *stack_end = (char *)stacks[i] + 4096 + stack_bytes;
            assert(checked_call(start.memory, (uintptr_t)stack_end, (uintptr_t)tls[i],
                (uintptr_t)entry.memory, (uintptr_t)&controls[i], (uintptr_t)&tids[i].value) > 0);
        }
        native_lock();
        while (h.members != count) native_wait();
        for (unsigned round = 0; round < 64; round++) {
            for (unsigned consecutive = 0; consecutive < 2; consecutive++) {
                assert(step_code.call(&h, 0, GC_HS_BEGIN) == GC_HS_OK);
                for (unsigned i = 0; i < count; i++) native_notify(&events[i].value);
                while (h.acknowledged != count) native_wait();
                assert(step_code.call(&h, 0, GC_HS_END) == GC_HS_OK);
                for (unsigned i = 0; i < count; i++) native_notify(&events[i].value);
                // Deliberately retain the generated lock through the next BEGIN.
            }
            for (unsigned i = 0; i < count; i++) {
                while (atomic_load_explicit(&progress[i], memory_order_acquire) != h.generation)
                    native_wait();
            }
        }
        assert(h.generation == 128 && h.members == count && h.native == 0);
        atomic_store_explicit(&stop.value, 1, memory_order_release);
        for (unsigned i = 0; i < count; i++) native_notify(&events[i].value);
        while (h.members) native_wait();
        native_unlock();
        for (unsigned i = 0; i < count; i++) {
            assert(native_call(join, &tids[i].value) == 0);
            assert(atomic_load(&tids[i].value) == 0);
            assert(tids[i].canary == 0xdeadbeef && events[i].canary == 0xdeadbeef);
            assert(controls[i].tls_observed == (uintptr_t)&contexts[i]);
            assert((controls[i].entry_sp & 15) == 8);
            assert(controls[i].entry_sp >= (uintptr_t)stacks[i] + 4096);
            assert(controls[i].entry_sp < (uintptr_t)stacks[i] + 4096 + stack_bytes);
            assert(atomic_load(&contexts[i].state) == GC_DETACHED);
            assert(!registrations[i].registered && !registrations[i].context);
            assert(checked_call(release.memory, (uintptr_t)stacks[i], stack_bytes + 8192, 0, 0, 0) == 0);
        }
        assert(native_mutex.canary == 0xdeadbeef && native_coordinator_event.canary == 0xdeadbeef);
        assert(stop.canary == 0xdeadbeef);
    }
    unload_code(start); unload_code(join); unload_code(alloc); unload_code(release);
    unload_code(entry); unload_code(step_code); unload_code(abi);
    unload_code(native_lock_code); unload_code(native_unlock_code);
    unload_code(native_notify_code); unload_code(native_park_code);
    puts("emitted Linux lifecycle: 1/2/8 raw-clone owners, 384 rounds, TLS and futex registry PASS");
}
#endif
