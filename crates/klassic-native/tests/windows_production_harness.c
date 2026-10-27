/* Load the compiler's complete PE, including real code/data/IAT layout.
 * POSIX substitutes explicit Win64 API models; Windows executes kernel APIs. */
#define _GNU_SOURCE
#include "windows_production_model.h"
#ifndef _WIN32
#include <stddef.h>
#include <linux/filter.h>
#include <linux/seccomp.h>
#include <sys/prctl.h>
#endif
#if __has_include("windows_production_fixture.h")
#include "windows_production_fixture.h"
#endif
#ifdef GC_WINDOWS_PRODUCTION_PIN_ABI
#define GC_SCOPED_PIN_NATIVE_ABI WINABI
#include "pin_abi_body.h"
#endif
static uint16_t u16(const unsigned char *p){uint16_t n;memcpy(&n,p,2);return n;}
static uint32_t u32(const unsigned char *p){uint32_t n;memcpy(&n,p,4);return n;}
static uint64_t u64(const unsigned char *p){uint64_t n;memcpy(&n,p,8);return n;}
int main(int argc,char **argv){
#ifdef GC_REQUIRE_NATIVE_CONSOLE_BORROWS
    require_native_console_borrows=GC_REQUIRE_NATIVE_CONSOLE_BORROWS;
#endif
#ifdef GC_WINDOWS_PRODUCTION_FIXTURE
    (void)argc;(void)argv;fault=GC_WINDOWS_PRODUCTION_MODE;
#else
    assert(argc==3);fault=atoi(argv[2]);
#endif
#ifdef _WIN32
    main_thread=GetCurrentThreadId();
#else
    alarm(60);main_thread=pthread_self();
#endif
#ifdef GC_WINDOWS_PRODUCTION_FIXTURE
    long length=sizeof(production_fixture);unsigned char *file=malloc((size_t)length);assert(file);memcpy(file,production_fixture,(size_t)length);
#else
    FILE *f=fopen(argv[1],"rb");assert(f&&fseek(f,0,SEEK_END)==0);long length=ftell(f);assert(length>256&&fseek(f,0,SEEK_SET)==0);
    unsigned char *file=malloc((size_t)length);assert(file&&fread(file,1,(size_t)length,f)==(size_t)length);assert(fclose(f)==0);
#endif
    size_t nt=u32(file+0x3c);assert(nt+24<(size_t)length&&!memcmp(file+nt,"PE\0\0",4));size_t coff=nt+4,opt=coff+20;
    assert(u16(file+coff)==0x8664&&u16(file+opt)==0x20b);size_t bytes=u32(file+opt+56),headers=u32(file+opt+60);void *base=(void *)(uintptr_t)u64(file+opt+24);
#ifdef _WIN32
    unsigned char *image=VirtualAlloc(base,bytes,MEM_RESERVE|MEM_COMMIT,PAGE_READWRITE);assert(image==base);
#else
    unsigned char *image=mmap(base,bytes,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE,-1,0);assert(image==base);
#endif
    assert(headers<=(size_t)length&&headers<=bytes);memcpy(image,file,headers);
    size_t section=opt+u16(file+coff+16);unsigned sections=u16(file+coff+2);
    for(unsigned i=0;i<sections;i++){
        size_t p=section+i*40,va=u32(file+p+12),n=u32(file+p+16),off=u32(file+p+20);assert(p+40<=(size_t)length&&off+n<=(size_t)length&&va+n<=bytes);memcpy(image+va,file+off,n);
    }
#ifdef GC_WINDOWS_PRODUCTION_PIN_ABI
    unsigned callback_patches=0,mode_patches=0;
    for(unsigned i=0;i<sections;i++){
        size_t p=section+i*40;if(!(u32(file+p+36)&0x80000000))continue;
        size_t va=u32(file+p+12),n=u32(file+p+16);
        for(size_t j=0;j+8<=n;j+=8){
            uint64_t value=u64(image+va+j);
            if(value==PIN_CALLBACK_MARKER){value=(uintptr_t)body;memcpy(image+va+j,&value,8);callback_patches++;}
            if(value==PIN_MODE_MARKER){value=pin_abi_mode;memcpy(image+va+j,&value,8);mode_patches++;}
        }
    }
    assert(callback_patches==1&&mode_patches==1);
#endif
    size_t imports=u32(file+opt+112+8);assert(imports&&imports+20<bytes);
    for(size_t d=imports;u32(image+d+12);d+=20){
        assert(d+20<=bytes);size_t names=u32(image+d),iat=u32(image+d+16);
        for(size_t i=0;u64(image+names+i*8);i++){
            size_t name=(size_t)u64(image+names+i*8);assert(!(name>>63)&&name+2<bytes&&memchr(image+name+2,0,bytes-name-2));
            void *fn=resolve((const char *)image+name+2);assert(iat+i*8+8<=bytes);memcpy(image+iat+i*8,&fn,8);
        }
    }
    for(unsigned i=0;i<sections;i++){
        size_t p=section+i*40;if(!(u32(file+p+36)&0x20000000))continue;
        size_t va=u32(file+p+12),n=(u32(file+p+8)+4095)&~(size_t)4095;
#ifdef _WIN32
        DWORD previous;assert(VirtualProtect(image+va,n,PAGE_EXECUTE_READ,&previous));assert(FlushInstructionCache(GetCurrentProcess(),image+va,n));
#else
        assert(mprotect(image+va,n,PROT_READ|PROT_EXEC)==0);
#endif
    }
#ifndef _WIN32
    // Win64 code must enter the OS only through its modeled imports. A Linux
    // syscall accidentally emitted into the PE is killed, even if its number
    // would happen to work on this POSIX conformance host.
    uintptr_t lo=(uintptr_t)image,hi=lo+bytes;assert((lo>>32)==(hi>>32));
    struct sock_filter rules[]={
        BPF_STMT(BPF_LD|BPF_W|BPF_ABS,offsetof(struct seccomp_data,instruction_pointer)+4),
        BPF_JUMP(BPF_JMP|BPF_JEQ|BPF_K,(uint32_t)(lo>>32),0,4),
        BPF_STMT(BPF_LD|BPF_W|BPF_ABS,offsetof(struct seccomp_data,instruction_pointer)),
        BPF_JUMP(BPF_JMP|BPF_JGE|BPF_K,(uint32_t)lo,0,2),
        BPF_JUMP(BPF_JMP|BPF_JGE|BPF_K,(uint32_t)hi,1,0),
        BPF_STMT(BPF_RET|BPF_K,SECCOMP_RET_KILL_PROCESS),
        BPF_STMT(BPF_RET|BPF_K,SECCOMP_RET_ALLOW)
    };
    struct sock_fprog filter={sizeof(rules)/sizeof(rules[0]),rules};
    assert(prctl(PR_SET_NO_NEW_PRIVS,1,0,0,0)==0);
    assert(prctl(PR_SET_SECCOMP,SECCOMP_MODE_FILTER,&filter)==0);
#endif
    typedef void (WINABI *Start)(void);Start entry=(Start)(image+u32(file+opt+16));free(file);entry();assert(!"PE entry returned");
}
