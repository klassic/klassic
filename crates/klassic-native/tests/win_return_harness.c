/* Execute the Win64 ABI wrapper even on a SysV x86-64 conformance host. */
#define _GNU_SOURCE
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
#define WINABI
#else
#include <sys/mman.h>
#define WINABI __attribute__((ms_abi))
#endif
#include "gc_win_return_code.h"
int main(void) {
#ifdef _WIN32
    void *memory=VirtualAlloc(NULL,sizeof(code),MEM_COMMIT|MEM_RESERVE,PAGE_READWRITE);
    assert(memory);memcpy(memory,code,sizeof(code));
    DWORD previous;
    assert(VirtualProtect(memory,sizeof(code),PAGE_EXECUTE_READ,&previous));
    assert(FlushInstructionCache(GetCurrentProcess(),memory,sizeof(code)));
#else
    void *memory=mmap(NULL,sizeof(code),PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
    assert(memory!=MAP_FAILED);memcpy(memory,code,sizeof(code));
    assert(mprotect(memory,sizeof(code),PROT_READ|PROT_EXEC)==0);
#endif
    uint64_t (WINABI *copy)(void *,void *,void *)=memory;
    /* Root-copy has its own RETs. Invalid inputs must return through the ABI
     * wrapper, instead of using a saved nonvolatile register as a return PC. */
    for(unsigned i=0;i<256;i++)assert(copy(NULL,NULL,NULL)==1);
#ifdef _WIN32
    assert(VirtualFree(memory,0,MEM_RELEASE));
#else
    assert(munmap(memory,sizeof(code))==0);
#endif
    puts("Win64 nested returns restore entry registers PASS");
}
