#ifndef KLASSIC_ATOMIC_PROGRESS_H
#define KLASSIC_ATOMIC_PROGRESS_H
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef _WIN32
#include <windows.h>
static uint64_t atomic_wall_ms(void) {
    LARGE_INTEGER now,frequency;
    assert(QueryPerformanceCounter(&now));
    assert(QueryPerformanceFrequency(&frequency));
    return (uint64_t)(now.QuadPart/frequency.QuadPart)*1000+
        (uint64_t)(now.QuadPart%frequency.QuadPart)*1000/(uint64_t)frequency.QuadPart;
}
static uint64_t atomic_cpu_ms(void) {
    FILETIME created,exited,kernel,user;
    assert(GetProcessTimes(GetCurrentProcess(),&created,&exited,&kernel,&user));
    uint64_t k=((uint64_t)kernel.dwHighDateTime<<32)|kernel.dwLowDateTime;
    uint64_t u=((uint64_t)user.dwHighDateTime<<32)|user.dwLowDateTime;
    return (k+u)/10000;
}
#else
#include <time.h>
static uint64_t atomic_clock_ms(clockid_t clock) {
    struct timespec now;assert(clock_gettime(clock,&now)==0);
    return (uint64_t)now.tv_sec*1000+(uint64_t)now.tv_nsec/1000000;
}
static uint64_t atomic_wall_ms(void) {return atomic_clock_ms(CLOCK_MONOTONIC);}
static uint64_t atomic_cpu_ms(void) {return atomic_clock_ms(CLOCK_PROCESS_CPUTIME_ID);}
#endif
static uint64_t atomic_started_wall,atomic_started_cpu;
static void atomic_stage(const char *stage,unsigned round) {
    if(!getenv("KLASSIC_GC_GRAPH_TRACE"))return;
    uint64_t wall=atomic_wall_ms(),cpu=atomic_cpu_ms();
    if(!atomic_started_wall){atomic_started_wall=wall;atomic_started_cpu=cpu;}
    fprintf(stderr,"atomic progress: stage=%s round=%u elapsed_ms=%llu cpu_ms=%llu\n",
        stage,round,(unsigned long long)(wall-atomic_started_wall),
        (unsigned long long)(cpu-atomic_started_cpu));
    fflush(stderr);
}
#define GC_MARK_GRAPH_PROGRESS(round,stage) atomic_stage("mark_graph " stage,round)
#endif
