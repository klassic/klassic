/* Reuse the complete PE/import loader and its Win64 API model. Each mode gets
 * a fresh process because the generated program owns process termination. */
static unsigned pin_abi_mode;
#define main pin_fixture_main
#include "windows_production_harness.c"
#undef main
#ifndef _WIN32
#include <sys/wait.h>
int main(void){
    alarm(60);
    for(unsigned mode=0;mode<4;mode++){
        pid_t child=fork();assert(child>=0);
        if(!child){pin_abi_mode=mode;pin_fixture_main(0,NULL);_exit(99);}
        int status=0;assert(waitpid(child,&status,0)==child);
        assert(WIFEXITED(status)&&WEXITSTATUS(status)==0);
    }
    return 0;
}

#else
int main(int argc,char **argv){
    assert(argc==2);pin_abi_mode=(unsigned)atoi(argv[1]);assert(pin_abi_mode<4);
    return pin_fixture_main(0,NULL);
}
#endif
