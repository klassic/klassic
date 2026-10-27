/* Exercise subprocess supervision, independently of collector correctness.
 * Each pipe receives more data than ordinary OS pipe capacity before exit. */
#include <assert.h>
#include <stdio.h>
#include <string.h>
#ifndef _WIN32
#include <unistd.h>
#endif
int main(void){
#ifndef _WIN32
    alarm(40);
#endif
    char block[4096];memset(block,'x',sizeof(block));
    for(unsigned i=0;i<1024;i++){
        assert(fwrite(block,1,sizeof(block),stdout)==sizeof(block));
        assert(fwrite(block,1,sizeof(block),stderr)==sizeof(block));
    }
    assert(fputs("\nconcurrent pipe drain PASS\n",stdout)>=0);
    return 0;
}
