/* Public rooted UTF-8 API: real allocation, movement and native output only. */
#include "klassic_gen_string.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
int main(int argc,char **argv){
    kg_configure(UINT64_C(8)*1024*1024,UINT64_C(1024)*1024,UINT64_C(4)*1024*1024,5);kg_init();
    RootCell *a=kg_root_new(),*alias=kg_root_new(),*empty=kg_root_new();
    if(argc>1){
        const unsigned char truncated[]={0xf0,0x9f},surrogate[]={0xed,0xa0,0x80},overlong[]={0xc0,0x80};
        const unsigned char *bad=!strcmp(argv[1],"truncated")?truncated:!strcmp(argv[1],"surrogate")?surrogate:overlong;
        kg_string_from_utf8(bad,!strcmp(argv[1],"surrogate")?3:2,a);return 3;
    }
    const unsigned char text[]={0xe7,0x81,0xaf,0xf0,0x9f,0x98,0x80,0};
    kg_string_from_utf8(text,sizeof(text),a);kg_root_copy(alias,a);
    kg_string_from_utf8(NULL,0,empty);assert(kg_string_length(empty)==0);
    assert(kg_string_length(a)==3&&kg_load_i64(a,0)==8);
    kg_string_concat(a,a,a);assert(kg_string_length(a)==6&&kg_load_i64(a,0)==16);
    kg_string_concat(empty,a,a);assert(kg_string_length(a)==6);
    assert(kg_string_compare(empty,a)<0&&kg_string_compare(a,alias)>0);
    for(unsigned i=0;i<48;i++){
        kg_string_concat(a,alias,a);assert(kg_string_length(a)==6+(i+1)*3);
        assert(kg_string_length(alias)==3&&kg_string_compare(alias,alias)==0);
    }
    kg_string_println(alias);kg_string_println(empty);
    assert(kg_verification_count()>4&&kg_committed_bytes()>=UINT64_C(1024)*1024);
    kg_root_release(empty);kg_root_release(alias);kg_root_release(a);kg_shutdown();
    puts("canonical C UTF-8: aliased outputs, Unicode/NUL, actual GC and native output PASS");return 0;
}
