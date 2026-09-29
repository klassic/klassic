/* Execute the actual Win64 emitter bytes. On Windows imports are resolved from
 * kernel32. On other x86-64 hosts the explicitly labeled API model below checks
 * calling conventions and failure paths, not Windows OS conformance. */
#define _GNU_SOURCE
#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
#define WINABI
#else
#include <pthread.h>
#include <sys/mman.h>
#include <unistd.h>
#define WINABI __attribute__((ms_abi))
#endif
#include "gc_windows_code.h"
#include "../../../runtime/gc/gc_native_root_prepare_cases.h"
#include "../../../runtime/gc/gc_native_builtin_prepare_cases.h"
#include "../../../runtime/gc/gc_native_startup_cases.h"

typedef uint64_t (WINABI *Leaf)(uint64_t, uint64_t);
static void WINABI native_lock_probe(uint64_t *state){
    assert(state[0]==UINT64_C(0xabcdef0123456789));state[1]++;
}
static Leaf startup_prepare_entry,builtin_registry_entry,root_prepare_entry,root_dispose_entry,root_map_entry,root_unmap_entry;
static uint64_t root_prepare_adapter(MetadataCharge *charge,const RootStorageSpec *spec){
#ifndef _WIN32
    // The ms_abi call must preserve the registers used as portable arguments.
    register uint64_t saved_di __asm__("rdi")=UINT64_C(0x1122334455667788);
    register uint64_t saved_si __asm__("rsi")=UINT64_C(0x8877665544332211);
    __asm__ volatile("" : "+r"(saved_di), "+r"(saved_si));
#endif
    uint64_t result=root_prepare_entry((uintptr_t)charge,(uintptr_t)spec);
#ifndef _WIN32
    __asm__ volatile("" : "+r"(saved_di), "+r"(saved_si));
    assert(saved_di==UINT64_C(0x1122334455667788)&&saved_si==UINT64_C(0x8877665544332211));
#endif
    return result;
}
static uint64_t root_dispose_adapter(MetadataCharge *charge){return root_dispose_entry((uintptr_t)charge,0);}
static uint64_t startup_prepare_adapter(const NativeStartupRequest *q,uint64_t owner){return startup_prepare_entry((uintptr_t)q,owner);}
static uint64_t builtin_registry_adapter(MetadataCharge *charge,uint64_t capacity){return builtin_registry_entry((uintptr_t)charge,capacity);}
static void *root_map_adapter(size_t bytes){return (void *)(uintptr_t)root_map_entry(bytes,0);}
static bool root_unmap_adapter(void *p,size_t bytes){return root_unmap_entry((uintptr_t)p,bytes)!=0;}

typedef uint32_t (WINABI *Entry)(void *);
typedef struct { uint64_t entry, context; uint32_t tls_index, reserved; } Launch;
_Static_assert(sizeof(Launch) == 24, "worker launch ABI");
typedef struct {
    uint64_t event, tls_index;
    _Atomic uint64_t tls_seen;
    _Atomic uint64_t *counter;
    uint64_t bits;
    _Atomic uint64_t observed, entry_sp;
    _Atomic uint64_t *gate;
} Context;
_Static_assert(sizeof(Context) == 64, "test payload, not MutatorContext");
_Static_assert(offsetof(Context, gate) == 56, "test gate offset");

#ifndef _WIN32
typedef struct {
    int kind, joined;
    pthread_t thread;
    Entry entry;
    void *argument;
    uint32_t result;
    pthread_mutex_t mutex;
    pthread_cond_t condition;
    int signaled;
    unsigned waiters;
} Handle;
static _Thread_local uint32_t last_error;
static pthread_key_t tls_key;
static unsigned handles, keys;
static int fail_event, fail_get_exit, fail_close, fail_tls_clear;
static int fail_counter, fail_frequency;
static int64_t counter_ticks = 1, counter_frequency = 10000000;
/* Deliberately dirty the unspecified high half of DWORD/BOOL API returns. */
static uint64_t dword(uint32_t value) { return UINT64_C(0xa55a5aa500000000) | value; }
static void *metadata_mapping[2];
static size_t metadata_bytes[2];
static unsigned metadata_map_calls,fail_metadata_map_call;
static int fail_metadata_map,fail_metadata_unmap,fail_payload_decommit;
static uint64_t WINABI api_VirtualAlloc(void *address,size_t bytes,uint32_t flags,uint32_t protection){
    assert(!address&&bytes&&!(bytes&4095)&&flags==0x3000&&protection==4);
    metadata_map_calls++;
    if(fail_metadata_map||metadata_map_calls==fail_metadata_map_call)return 0;
    unsigned slot=metadata_mapping[0]?1:0;assert(!metadata_mapping[slot]);
    metadata_mapping[slot]=mmap(NULL,bytes,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
    if(metadata_mapping[slot]==MAP_FAILED){metadata_mapping[slot]=NULL;return 0;}
    metadata_bytes[slot]=bytes;
    return (uintptr_t)metadata_mapping[slot];
}
static uint64_t WINABI api_VirtualFree(void *address,size_t bytes,uint32_t flags){
    unsigned slot=address==metadata_mapping[0]?0:1;
    assert(address==metadata_mapping[slot]);
    if(flags==0x4000){
        assert(bytes==metadata_bytes[slot]);
        if(fail_payload_decommit)return dword(0);
        assert(mprotect(address,bytes,PROT_NONE)==0);
        assert(madvise(address,bytes,MADV_DONTNEED)==0);
        return dword(256);
    }
    assert(!bytes&&flags==0x8000);
    if(fail_metadata_unmap)return dword(0);
    assert(munmap(address,metadata_bytes[slot])==0);metadata_mapping[slot]=NULL;metadata_bytes[slot]=0;
    return dword(256); // noncanonical true, with a zero low byte
}
static uint64_t WINABI api_GetLastError(void) { return dword(last_error); }
static uint64_t WINABI api_QueryPerformanceCounter(int64_t *value) {
    if (fail_counter) return dword(0);
    *value = counter_ticks;
    return dword(1);
}
static uint64_t WINABI api_QueryPerformanceFrequency(int64_t *value) {
    if (fail_frequency) return dword(0);
    *value = counter_frequency;
    return dword(1);
}
static uint64_t WINABI api_TlsAlloc(void) {
    assert(keys == 0);
    assert(pthread_key_create(&tls_key, NULL) == 0);
    keys++;
    return dword(7);
}
static uint64_t WINABI api_TlsFree(uint32_t index) {
    assert(index == 7 && keys == 1);
    assert(pthread_key_delete(tls_key) == 0);
    keys--;
    return dword(1);
}
static uint64_t WINABI api_TlsSetValue(uint32_t index, void *value) {
    if (index != 7 || keys != 1) { last_error = 87; return dword(0); }
    if (value == NULL && fail_tls_clear) { last_error = 5; return dword(0); }
    assert(pthread_setspecific(tls_key, value) == 0);
    return dword(1);
}
static void *WINABI api_TlsGetValue(uint32_t index) {
    if (index != 7 || keys != 1) { last_error = 87; return NULL; }
    last_error = 0;
    return pthread_getspecific(tls_key);
}
static void *thread_entry(void *pointer) {
    Handle *handle = pointer;
    handle->result = handle->entry(handle->argument);
    return NULL;
}
static void *WINABI api_CreateThread(void *attributes, size_t stack_bytes, Entry entry,
                                    void *argument, uint32_t flags, uint32_t *id) {
    assert(attributes == NULL && flags == 0x10000 && id == NULL);
    if (stack_bytes == UINT64_MAX) { last_error = 8; return NULL; }
    Handle *handle = calloc(1, sizeof(*handle));
    assert(handle);
    handle->kind = 1; handle->entry = entry; handle->argument = argument;
    assert(pthread_create(&handle->thread, NULL, thread_entry, handle) == 0);
    handles++;
    return handle;
}
static void *WINABI api_CreateEventA(void *attributes, int manual, int signaled, const char *name) {
    assert(attributes == NULL && !manual && !signaled && name == NULL);
    if (fail_event) { last_error = 8; return NULL; }
    Handle *handle = calloc(1, sizeof(*handle));
    assert(handle);
    handle->kind = 2;
    assert(pthread_mutex_init(&handle->mutex, NULL) == 0);
    assert(pthread_cond_init(&handle->condition, NULL) == 0);
    handles++;
    return handle;
}
static uint64_t WINABI api_WaitForSingleObject(Handle *handle, uint32_t timeout) {
    if (!handle) { last_error = 6; return dword(UINT32_MAX); }
    if (handle->kind == 1) {
        assert(timeout == UINT32_MAX);
        if (!handle->joined) {
            assert(pthread_join(handle->thread, NULL) == 0);
            handle->joined = 1;
        }
    } else {
        assert(handle->kind == 2);
        assert(pthread_mutex_lock(&handle->mutex) == 0);
        if (!handle->signaled && timeout == 0) {
            assert(pthread_mutex_unlock(&handle->mutex) == 0);
            return dword(258); // WAIT_TIMEOUT
        }
        handle->waiters++;
        assert(pthread_cond_broadcast(&handle->condition) == 0);
        while (!handle->signaled) assert(pthread_cond_wait(&handle->condition, &handle->mutex) == 0);
        handle->waiters--;
        handle->signaled = 0;
        assert(pthread_mutex_unlock(&handle->mutex) == 0);
    }
    return dword(0);
}
static uint64_t WINABI api_SetEvent(Handle *handle) {
    assert(handle && handle->kind == 2);
    assert(pthread_mutex_lock(&handle->mutex) == 0);
    handle->signaled = 1;
    assert(pthread_cond_broadcast(&handle->condition) == 0);
    assert(pthread_mutex_unlock(&handle->mutex) == 0);
    return dword(1);
}
static uint64_t WINABI api_GetExitCodeThread(Handle *handle, uint32_t *result) {
    assert(handle && handle->kind == 1 && handle->joined);
    if (fail_get_exit) { last_error = 5; return dword(0); }
    *result = handle->result;
    return dword(1);
}
static uint64_t WINABI api_CloseHandle(Handle *handle) {
    assert(handle);
    if (fail_close) { last_error = 5; return dword(0); }
    if (handle->kind == 1) assert(handle->joined);
    else {
        assert(handle->waiters == 0);
        assert(pthread_cond_destroy(&handle->condition) == 0);
        assert(pthread_mutex_destroy(&handle->mutex) == 0);
    }
    handles--;
    free(handle);
    return dword(1);
}
static void await_waiter(uint64_t value) {
    Handle *handle = (Handle *)(uintptr_t)value;
    assert(pthread_mutex_lock(&handle->mutex) == 0);
    while (handle->waiters == 0) assert(pthread_cond_wait(&handle->condition, &handle->mutex) == 0);
    assert(pthread_mutex_unlock(&handle->mutex) == 0);
}
#endif

static void *resolve(const char *name) {
#ifdef _WIN32
    void *result = (void *)GetProcAddress(GetModuleHandleA("kernel32.dll"), name);
    assert(result);
    return result;
#else
#define API(n) if (strcmp(name, #n) == 0) return (void *)api_##n
    API(VirtualAlloc); API(VirtualFree);
    API(CreateThread); API(WaitForSingleObject); API(GetExitCodeThread);
    API(TlsAlloc); API(TlsFree); API(TlsSetValue); API(TlsGetValue);
    API(CreateEventA); API(SetEvent); API(CloseHandle); API(GetLastError);
    API(QueryPerformanceCounter); API(QueryPerformanceFrequency);
#undef API
    fprintf(stderr, "missing API model: %s\n", name);
    abort();
#endif
}

int main(void) {
#ifndef _WIN32
    alarm(40);
    void *code = mmap(NULL, sizeof(win_code), PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    assert(code != MAP_FAILED);
#else
    void *code = VirtualAlloc(NULL, sizeof(win_code), MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    assert(code);
#endif
    memcpy(code, win_code, sizeof(win_code));
    for (size_t i = 0; i < sizeof(win_imports) / sizeof(win_imports[0]); i++) {
        void *function = resolve(win_imports[i].name);
        memcpy((char *)code + win_imports[i].offset, &function, sizeof(function));
    }
#ifdef _WIN32
    DWORD previous;
    assert(VirtualProtect(code, sizeof(win_code), PAGE_EXECUTE_READ, &previous));
    assert(FlushInstructionCache(GetCurrentProcess(), code, sizeof(win_code)));
#else
    assert(mprotect(code, sizeof(win_code), PROT_READ | PROT_EXEC) == 0);
#endif
#define FN(n) Leaf n = (Leaf)((char *)code + WIN_##n)
    FN(start); FN(join); FN(event); FN(park); FN(wake); FN(close);
    FN(tls_alloc); FN(tls_free); FN(tls_get); FN(tls_set);
    FN(clock); FN(metadata_map); FN(metadata_unmap); FN(heap_decommit); FN(native_lock_call);
#undef FN
    uint64_t lock_probe[2]={UINT64_C(0xabcdef0123456789),0};
    assert(native_lock_call((uintptr_t)lock_probe,(uintptr_t)native_lock_probe)==0&&lock_probe[1]==1);
    puts("Win64 native unary callback ABI PASS");
    // Execute the same locally relocated payload used by the full Windows
    // allocation harness, binding OS imports before making code executable.
#ifdef _WIN32
    void *metadata_code=VirtualAlloc(NULL,sizeof(gc_win_metadata_code),MEM_RESERVE|MEM_COMMIT,PAGE_READWRITE);
    assert(metadata_code);
#else
    void *metadata_code=mmap(NULL,sizeof(gc_win_metadata_code),PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
    assert(metadata_code!=MAP_FAILED);
#endif
    memcpy(metadata_code,gc_win_metadata_code,sizeof(gc_win_metadata_code));
    void *map_import=resolve("VirtualAlloc"),*unmap_import=resolve("VirtualFree");
    memcpy((char *)metadata_code+GC_WIN_METADATA_ALLOC_IMPORT,&map_import,sizeof(map_import));
    memcpy((char *)metadata_code+GC_WIN_METADATA_FREE_IMPORT,&unmap_import,sizeof(unmap_import));
#ifdef _WIN32
    DWORD metadata_previous;
    assert(VirtualProtect(metadata_code,sizeof(gc_win_metadata_code),PAGE_EXECUTE_READ,&metadata_previous));
    assert(FlushInstructionCache(GetCurrentProcess(),metadata_code,sizeof(gc_win_metadata_code)));
#else
    assert(mprotect(metadata_code,sizeof(gc_win_metadata_code),PROT_READ|PROT_EXEC)==0);
#endif
    metadata_map=(Leaf)((char *)metadata_code+GC_WIN_METADATA_MAP);
    metadata_unmap=(Leaf)((char *)metadata_code+GC_WIN_METADATA_UNMAP);
    assert(!metadata_map(0,0)&&!metadata_map(1,0));
    assert(!metadata_unmap(0,4096)&&!metadata_unmap(4097,4096));
#ifndef _WIN32
    fail_metadata_map=1;assert(!metadata_map(8192,0));fail_metadata_map=0;
#endif
    uint64_t storage=metadata_map(8192,0);assert(storage&&!(storage&4095));
    unsigned char *bytes=(void *)(uintptr_t)storage;
    for(unsigned i=0;i<8192;i++)assert(!bytes[i]);
    bytes[0]=0x51;bytes[8191]=0xa7;
#ifndef _WIN32
    fail_metadata_unmap=1;assert(!metadata_unmap(storage,8192));
    assert(bytes[0]==0x51&&bytes[8191]==0xa7);fail_metadata_unmap=0;
#endif
    assert(!heap_decommit(0,8192)&&!heap_decommit(storage+1,8192)&&!heap_decommit(storage,0));
    assert(!heap_decommit(storage,8193)&&!heap_decommit(UINT64_MAX&~UINT64_C(4095),4096));
#ifndef _WIN32
    fail_payload_decommit=1;assert(!heap_decommit(storage,8192));
    assert(bytes[0]==0x51&&bytes[8191]==0xa7);fail_payload_decommit=0;
#endif
    assert(heap_decommit(storage,8192)==1);
    puts("Win64 payload decommit: retained reservation and normalized BOOL PASS");
    assert(metadata_unmap(storage,8192)==1);
    puts("Win64 metadata backing lifecycle PASS");
    root_prepare_entry=(Leaf)((char *)metadata_code+GC_WIN_METADATA_ROOTS_PREPARE);
    root_dispose_entry=(Leaf)((char *)metadata_code+GC_WIN_METADATA_ROOTS_DISPOSE);
    root_map_entry=metadata_map;root_unmap_entry=metadata_unmap;
    NativeMetadataOps root_ops={4096,root_map_adapter,root_unmap_adapter};
    gc_run_native_root_prepare_cases(root_prepare_adapter,root_dispose_adapter,gc_budget_step_locked,&root_ops);
#ifndef _WIN32
    MetadataCharge root_charge={.bytes=4096,.owner=0x1000,.stage=1};
    RootStorageSpec root_spec={17,3,2,4096};
    assert(root_prepare_adapter(&root_charge,&root_spec)==0);
    MetadataCharge saved_charge=root_charge;
    RootStorage saved_storage=*(RootStorage *)(uintptr_t)root_charge.allocation;
    fail_metadata_unmap=1;
    assert(root_dispose_adapter(&root_charge)==GC_NATIVE_METADATA_OS_ERROR);
    assert(!memcmp(&root_charge,&saved_charge,sizeof(root_charge)));
    assert(!memcmp((void *)(uintptr_t)root_charge.allocation,&saved_storage,sizeof(saved_storage)));
    fail_metadata_unmap=0;
    assert(root_dispose_adapter(&root_charge)==0&&!root_charge.allocation);
    assert(root_dispose_adapter(&root_charge)==GC_NATIVE_METADATA_INVALID);
#endif
    puts("Win64 native root preparation PASS");
    builtin_registry_entry=(Leaf)((char *)metadata_code+GC_WIN_METADATA_BUILTIN_REGISTRY_PREPARE);
    gc_run_native_builtin_prepare_cases(builtin_registry_adapter,gc_budget_step_locked,&root_ops);
    startup_prepare_entry=(Leaf)((char *)metadata_code+GC_WIN_METADATA_STARTUP_PREPARE);
    gc_run_native_startup_cases(4096,gc_native_startup_request_locked);
    unsigned startup_scenarios=1;
#ifndef _WIN32
    startup_scenarios=3;
#endif
    for(unsigned failure=0;failure<startup_scenarios;failure++){
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage),.metadata_max=sizeof(AllocationPage)+8192};
        MetadataCharge roots={0},descriptors={0};
        NativeStartupRequest q={(uintptr_t)&roots,(uintptr_t)&descriptors,3,2,64,4096};
        assert(gc_native_startup_request_locked(&heap,&q)==GC_BUDGET_OK);
        NativeStartupPreparationOps ops={&root_ops,root_prepare_adapter,builtin_registry_adapter,NULL,NULL,NULL};
        // C fallback validation cases need the same initializers as the C owner.
        ops.initialize_roots=gc_root_storage_prepare;ops.initialize_descriptors=gc_builtin_descriptors_prepare;
        ops.register_descriptor=gc_descriptor_register;
        gc_run_startup_prepare_rejections(&q,17,&ops,startup_prepare_adapter);
#ifndef _WIN32
        metadata_map_calls=0;fail_metadata_map_call=failure;
#endif
        assert(startup_prepare_adapter(&q,17)==(failure?GC_NATIVE_METADATA_OS_ERROR:GC_NATIVE_METADATA_OK));
        assert((roots.allocation!=0)==(failure!=1)&&(descriptors.allocation!=0)==(!failure));
        if(roots.allocation)assert(root_dispose_adapter(&roots)==GC_NATIVE_METADATA_OK);
        if(descriptors.allocation)assert(gc_native_metadata_dispose(&descriptors,&root_ops)==GC_NATIVE_METADATA_OK);
        assert(gc_budget_step_locked(&heap,&roots,GC_BUDGET_ABORT)==GC_BUDGET_OK);
        assert(gc_budget_step_locked(&heap,&descriptors,GC_BUDGET_ABORT)==GC_BUDGET_OK);
        assert(!heap.metadata_pending&&heap.metadata_committed==sizeof(AllocationPage));
#ifndef _WIN32
        fail_metadata_map_call=0;assert(!metadata_mapping[0]&&!metadata_mapping[1]);
#endif
    }
    puts("Win64 paired startup preparation PASS");


#ifdef _WIN32
    assert(VirtualFree(metadata_code,0,MEM_RELEASE));
#else
    assert(munmap(metadata_code,sizeof(gc_win_metadata_code))==0);
#endif
    puts("Win64 locally relocated metadata payload PASS");
    uint64_t ns = 99;
#ifdef _WIN32
    assert(clock((uintptr_t)&ns, 0) == 0);
    uint64_t first = ns, previous_ns = ns;
    for (unsigned sample = 0; sample < 10000; sample++) {
        assert(clock((uintptr_t)&ns, 0) == 0);
        assert(ns >= previous_ns);
        previous_ns = ns;
    }
    assert(ns > first);
#else
    for (unsigned sample = 1; sample <= 10000; sample++) {
        counter_ticks = sample;
        assert(clock((uintptr_t)&ns, 0) == 0 && ns == sample * 100);
    }
    const int64_t counts[] = {0, 1, 2, 10000001, INT64_MAX};
    const int64_t frequencies[] = {1, 3, 3579545, 10000000, INT64_MAX};
    for (unsigned i = 0; i < sizeof(counts) / sizeof(counts[0]); i++) {
        for (unsigned j = 0; j < sizeof(frequencies) / sizeof(frequencies[0]); j++) {
            counter_ticks = counts[i]; counter_frequency = frequencies[j];
            __uint128_t expected = (__uint128_t)counts[i] * 1000000000 / frequencies[j];
            ns = 99;
            uint64_t error = clock((uintptr_t)&ns, 0);
            if (expected > UINT64_MAX) assert(error == 534 && ns == 99);
            else assert(error == 0 && ns == (uint64_t)expected);
        }
    }
    counter_ticks = -1; counter_frequency = 10000000; ns = 99;
    assert(clock((uintptr_t)&ns, 0) == 13 && ns == 99);
    counter_ticks = 1; counter_frequency = 0;
    assert(clock((uintptr_t)&ns, 0) == 13 && ns == 99);
    counter_frequency = -1;
    assert(clock((uintptr_t)&ns, 0) == 13 && ns == 99);
    counter_frequency = 10000000;
    fail_counter = 1;
    assert(clock((uintptr_t)&ns, 0) == 50 && ns == 99);
    fail_counter = 0; fail_frequency = 1;
    assert(clock((uintptr_t)&ns, 0) == 50 && ns == 99);
    fail_frequency = 0;
#endif
    uint64_t index = tls_alloc(0, 0);
    assert(index < UINT32_MAX);
    uint64_t main_marker = 1234;
    assert(tls_set(index, (uintptr_t)&main_marker) == 1);
    assert(tls_get(index, 0) == (uintptr_t)&main_marker);
    struct { uint32_t code, canary; } exit_status = {UINT32_MAX, 0xdeadbeef};
    assert(join(0, (uintptr_t)&exit_status.code) != 0);
    assert(exit_status.code == UINT32_MAX && exit_status.canary == 0xdeadbeef);
#ifndef _WIN32
    fail_event = 1;
    assert(event(0, 0) == 0);
    fail_event = 0;
#endif
    // Repeated lifecycles also catch forgotten handle/TLS ownership.
    for (unsigned cycle = 0; cycle < 32; cycle++) {
        _Atomic uint64_t gate = 0, counter = 0;
        Context contexts[2]; Launch launches[2]; uint64_t threads[2];
        for (unsigned i = 0; i < 2; i++) {
            uint64_t notification = event(0, 0);
            assert(notification);
            assert(wake(notification, 0) == 1);
            assert(park(notification, 0) == 0); // notify-before-park persists
            assert(park(notification, 0) == 258); // auto-reset consumed it once
            contexts[i] = (Context){notification, index, 0, &counter, UINT64_C(1) << i, 99, 0, &gate};
            launches[i] = (Launch){(uintptr_t)code + WIN_entry, (uintptr_t)&contexts[i], (uint32_t)index, 0};
            assert(start((uintptr_t)&launches[i], UINT64_MAX) == 0);
            threads[i] = start((uintptr_t)&launches[i], 1024 * 1024);
            assert(threads[i]);
        }
#ifndef _WIN32
        for (unsigned i = 0; i < 2; i++) await_waiter(contexts[i].event);
#endif
        atomic_store_explicit(&gate, 1, memory_order_release);
        for (unsigned i = 0; i < 2; i++) assert(wake(contexts[i].event, 0) == 1);
        for (unsigned i = 0; i < 2; i++) {
#ifndef _WIN32
            fail_get_exit = 1;
            assert(join(threads[i], (uintptr_t)&exit_status.code) == 5);
            fail_get_exit = 0;
            fail_close = 1;
            assert(join(threads[i], (uintptr_t)&exit_status.code) == 5);
            fail_close = 0;
#endif
            assert(join(threads[i], (uintptr_t)&exit_status.code) == 0);
            assert(exit_status.code == 0 && exit_status.canary == 0xdeadbeef);
            assert(atomic_load_explicit(&contexts[i].tls_seen, memory_order_acquire) == (uintptr_t)&contexts[i]);
            assert((atomic_load_explicit(&contexts[i].entry_sp, memory_order_acquire) & 15) == 8);
            assert(close(contexts[i].event, 0) == 1);
        }
        assert(atomic_load_explicit(&counter, memory_order_acquire) == 3);
        uint64_t a = atomic_load_explicit(&contexts[0].observed, memory_order_acquire);
        uint64_t b = atomic_load_explicit(&contexts[1].observed, memory_order_acquire);
        assert((a == 0 && b == 1) || (a == 2 && b == 0));
        assert(tls_get(index, 0) == (uintptr_t)&main_marker);
    }
    // Failure after CreateThread succeeded still requires join. Entry cannot run
    // with invalid TLS; a null context makes an erroneous callback fail loudly.
    Launch bad = {(uintptr_t)code + WIN_entry, 0, UINT32_MAX, 0};
    uint64_t thread = start((uintptr_t)&bad, 0);
    assert(thread);
    assert(join(thread, (uintptr_t)&exit_status.code) == 0);
    assert(exit_status.code != 0 && exit_status.canary == 0xdeadbeef);
#ifndef _WIN32
    // TLS teardown failure must not be disguised as the entry's successful exit.
    _Atomic uint64_t open_gate = 1, final_counter = 0;
    Context final_context = {0, index, 0, &final_counter, 4, 99, 0, &open_gate};
    Launch final_launch = {(uintptr_t)code + WIN_entry, (uintptr_t)&final_context, (uint32_t)index, 0};
    fail_tls_clear = 1;
    thread = start((uintptr_t)&final_launch, 0);
    assert(thread);
    assert(join(thread, (uintptr_t)&exit_status.code) == 0);
    assert(exit_status.code == 5 && exit_status.canary == 0xdeadbeef);
    assert(atomic_load_explicit(&final_counter, memory_order_acquire) == 4);
    fail_tls_clear = 0;
#endif
    assert(tls_set(index, 0) == 1);
    assert(tls_free(index, 0) == 1);
#ifdef _WIN32
    assert(VirtualFree(code, 0, MEM_RELEASE));
    puts("Windows OS worker execution PASS (32 two-worker cycles)");
#else
    assert(handles == 0 && keys == 0);
    assert(munmap(code, sizeof(win_code)) == 0);
    puts("Win64 worker execution with POSIX API model PASS (32 two-worker cycles)");
#endif
    return 0;
}
