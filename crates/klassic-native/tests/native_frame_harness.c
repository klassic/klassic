#define _GNU_SOURCE
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#include <unistd.h>
#ifdef __APPLE__
#include <libkern/OSCacheControl.h>
#endif
#endif
#include "native_frame_code.h"
typedef struct {uintptr_t argument,stack,leaf,touched;} Probe;
#if defined(__x86_64__) || defined(_M_X64)
typedef uint64_t (__attribute__((sysv_abi)) *Sysv)(Probe *);
typedef uint64_t (__attribute__((ms_abi)) *Win64)(Probe *);
#else
typedef uint64_t (*Sysv)(Probe *);
#endif
int main(void) {
    for(unsigned i=0;i<sizeof(fixtures)/sizeof(fixtures[0]);i++){
        const FrameFixture *f=&fixtures[i];
        size_t bytes=4096;
#ifdef _WIN32
        unsigned char *p=VirtualAlloc(NULL,bytes,MEM_RESERVE|MEM_COMMIT,PAGE_READWRITE);
        assert(p);
#else
        unsigned char *p=mmap(NULL,bytes,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
        assert(p!=MAP_FAILED);
#endif
        assert(f->bytes<=bytes);memcpy(p,f->code,f->bytes);
#ifdef _WIN32
        DWORD old=0;assert(VirtualProtect(p,bytes,PAGE_EXECUTE_READ,&old));
        assert(FlushInstructionCache(GetCurrentProcess(),p,bytes));
#else
#ifdef __APPLE__
        sys_icache_invalidate(p,bytes);
#endif
        assert(!mprotect(p,bytes,PROT_READ|PROT_EXEC));
#endif
        Probe q={0};uint64_t result;
#if defined(__x86_64__) || defined(_M_X64)
        if(f->windows)result=((Win64)(p+f->entry))(&q);
        else
#endif
        result=((Sysv)(p+f->entry))(&q);
        assert(result==UINT64_C(0x123456789abcdef0));
        assert(q.argument==(uintptr_t)&q && !(q.stack&15));
        assert(q.leaf==(uintptr_t)(p+f->leaf) && q.touched==UINT64_C(0x444f4e45));
#if defined(__x86_64__) || defined(_M_X64)
        if(f->windows)result=((Win64)q.leaf)(&q);
        else
#endif
        result=((Sysv)q.leaf)(&q);
        assert(result==UINT64_C(0x4c454146));
#ifdef _WIN32
        assert(VirtualFree(p,0,MEM_RELEASE));
#else
        assert(!munmap(p,bytes));
#endif
    }
    puts("native frame C calls: PASS");
}
