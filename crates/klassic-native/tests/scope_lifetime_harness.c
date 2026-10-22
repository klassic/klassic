#define _GNU_SOURCE
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#endif
#include "gc_scope_lifetime_code.h"
int main(void){
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
    for(unsigned i=0;i<sizeof(entries)/sizeof(entries[0]);i++){
        uint64_t (*scope)(void)=(void *)((unsigned char *)memory+entries[i]);
        uint64_t overwritten=scope();
        if(overwritten)fprintf(stderr,"scope %u: %llu live root slots overwritten before retirement\n",i,(unsigned long long)overwritten);
        assert(overwritten==0);
    }
#ifdef _WIN32
    assert(VirtualFree(memory,0,MEM_RELEASE));
#else
    assert(munmap(memory,sizeof(code))==0);
#endif
    puts("scope roots retain stack storage through retirement PASS");
}
