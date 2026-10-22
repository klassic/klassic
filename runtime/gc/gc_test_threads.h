/* Test-only threads for conformance fixtures. Production code uses OS adapters. */
#ifndef KLASSIC_GC_TEST_THREADS_H
#define KLASSIC_GC_TEST_THREADS_H
#ifdef _WIN32
#include <windows.h>
#include <stdlib.h>
#include <errno.h>
typedef CRITICAL_SECTION GcTestMutex;
typedef struct GcTestThreadState {
    HANDLE handle;
    void *(*start)(void *);
    void *argument, *result;
} *GcTestThread;
static inline DWORD WINAPI gc_test_thread_start(void *argument) {
    GcTestThread state = argument;
    state->result = state->start(state->argument);
    return 0;
}
static inline int gc_test_thread_create(GcTestThread *thread, const void *attributes,
                                       void *(*start)(void *), void *argument) {
    (void)attributes;
    GcTestThread state = malloc(sizeof(*state));
    if (!state) return ENOMEM;
    state->start = start;
    state->argument = argument;
    state->result = NULL;
    state->handle = CreateThread(NULL, 0, gc_test_thread_start, state, 0, NULL);
    if (!state->handle) { free(state); return EAGAIN; }
    *thread = state;
    return 0;
}
static inline int gc_test_thread_join(GcTestThread thread, void **result) {
    if (WaitForSingleObject(thread->handle, INFINITE) != WAIT_OBJECT_0) return EINVAL;
    if (result) *result = thread->result;
    if (!CloseHandle(thread->handle)) return EINVAL;
    free(thread);
    return 0;
}
static inline int gc_test_mutex_init(GcTestMutex *mutex, const void *attributes) {
    (void)attributes;
    InitializeCriticalSection(mutex);
    return 0;
}
static inline int gc_test_mutex_lock(GcTestMutex *mutex) {
    EnterCriticalSection(mutex);
    return 0;
}
static inline int gc_test_mutex_unlock(GcTestMutex *mutex) {
    LeaveCriticalSection(mutex);
    return 0;
}
static inline int gc_test_mutex_destroy(GcTestMutex *mutex) {
    DeleteCriticalSection(mutex);
    return 0;
}
static inline void gc_test_yield(void) { SwitchToThread(); }
#else
#include <pthread.h>
#include <sched.h>
typedef pthread_t GcTestThread;
typedef pthread_mutex_t GcTestMutex;
#define gc_test_thread_create pthread_create
#define gc_test_thread_join pthread_join
#define gc_test_mutex_init pthread_mutex_init
#define gc_test_mutex_lock pthread_mutex_lock
#define gc_test_mutex_unlock pthread_mutex_unlock
#define gc_test_mutex_destroy pthread_mutex_destroy
#define gc_test_yield sched_yield
#endif
#endif
