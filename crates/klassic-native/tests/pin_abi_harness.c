/* Load the complete direct ELF and supply a real C-ABI body. No GC state is
 * modeled or fabricated here: C invokes the emitted public function table. */
#define _GNU_SOURCE
#include <assert.h>
#include <elf.h>
#include <signal.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <unistd.h>
#include "gc_scoped_pin.h"
#include "pin_abi_fixture.h"
#include "pin_abi_body.h"
static void execute(unsigned mode){
    const Elf64_Ehdr *eh=(const void *)pin_fixture;
    assert(!memcmp(eh->e_ident,ELFMAG,SELFMAG)&&eh->e_machine==EM_X86_64);
    assert(eh->e_phoff+eh->e_phnum*sizeof(Elf64_Phdr)<=sizeof(pin_fixture));
    const Elf64_Phdr *ph=(const void *)(pin_fixture+eh->e_phoff);
    unsigned callback_patches=0,mode_patches=0;
    for(unsigned i=0;i<eh->e_phnum;i++){
        if(ph[i].p_type!=PT_LOAD)continue;
        assert(ph[i].p_offset+ph[i].p_filesz<=sizeof(pin_fixture));
        uintptr_t low=ph[i].p_vaddr&~(uintptr_t)4095;
        size_t bytes=(ph[i].p_vaddr-low+ph[i].p_memsz+4095)&~(size_t)4095;
        unsigned char *p=mmap((void *)low,bytes,PROT_READ|PROT_WRITE,
            MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE,-1,0);
        assert(p==(void *)low);
        unsigned char *data=(void *)(uintptr_t)ph[i].p_vaddr;
        memcpy(data,pin_fixture+ph[i].p_offset,ph[i].p_filesz);
        if(ph[i].p_flags&PF_W){
            for(size_t j=0;j+8<=ph[i].p_filesz;j+=8){
                uint64_t value;memcpy(&value,data+j,8);
                if(value==PIN_CALLBACK_MARKER){value=(uintptr_t)body;memcpy(data+j,&value,8);callback_patches++;}
                if(value==PIN_MODE_MARKER){value=mode;memcpy(data+j,&value,8);mode_patches++;}
            }
        }
        int prot=PROT_READ|((ph[i].p_flags&PF_W)?PROT_WRITE:0)|((ph[i].p_flags&PF_X)?PROT_EXEC:0);
        assert(mprotect(p,bytes,prot)==0);
    }
    assert(callback_patches==1&&mode_patches==1);
    ((void (*)(void))(uintptr_t)eh->e_entry)();
    assert(!"direct ELF entry returned");
}
int main(void){
    alarm(60);
    for(unsigned mode=0;mode<4;mode++){
        pid_t child=fork();assert(child>=0);
        if(!child){execute(mode);_exit(99);}
        int status=0;assert(waitpid(child,&status,0)==child);
        assert(WIFEXITED(status)&&WEXITSTATUS(status)==0);
    }
    return 0;
}
