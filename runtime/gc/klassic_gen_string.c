#include "klassic_gen_string.h"
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
/* RAW_BYTES payload: byte length, Unicode scalar count, padded UTF-8 bytes.
 * The descriptor never scans these scalar bytes as references. Every scalar
 * access resolves the actual rooted receiver again, so collection can move a
 * partially filled private string between any two stores. */
static _Noreturn void kgs_fail(const char *message){
    fprintf(stderr,"klassic generational string: %s\n",message);abort();
}
static uint64_t kgs_utf8(const unsigned char *text,uint64_t length){
    uint64_t chars=0;
    for(uint64_t i=0;i<length;chars++){
        unsigned c=text[i++],needed=0,minimum=0,value=0;
        if(c<128)continue;
        if(c>=0xc2&&c<=0xdf){needed=1;minimum=0x80;value=c&31;}
        else if(c>=0xe0&&c<=0xef){needed=2;minimum=0x800;value=c&15;}
        else if(c>=0xf0&&c<=0xf4){needed=3;minimum=0x10000;value=c&7;}
        else kgs_fail("invalid UTF-8 input");
        if(needed>length-i)kgs_fail("truncated UTF-8 input");
        while(needed--){unsigned next=text[i++];if((next&0xc0)!=0x80)kgs_fail("invalid UTF-8 continuation");value=(value<<6)|(next&63);}
        if(value<minimum||value>0x10ffff||(value>=0xd800&&value<=0xdfff))kgs_fail("invalid UTF-8 scalar");
    }
    return chars;
}
static uint64_t kgs_payload(uint64_t bytes){
    if(bytes>INT64_MAX||bytes>UINT64_MAX-23)kgs_fail("string length overflow");
    return 16+((bytes+7)&~UINT64_C(7));
}
void kg_string_from_utf8(const unsigned char *text,uint64_t length,RootCell *out){
    if(!text&&length)kgs_fail("null UTF-8 input");
    uint64_t payload=kgs_payload(length),chars=kgs_utf8(text,length);
    kg_alloc_into(payload,1,out);
    for(uint64_t offset=0;offset<length;offset+=8){
        uint64_t bits=0,n=length-offset;if(n>8)n=8;
        for(uint64_t i=0;i<n;i++)bits|=(uint64_t)text[offset+i]<<(i*8);
        kg_store_i64(out,16+offset,bits);
    }
    kg_store_i64(out,0,length);kg_store_i64(out,8,chars);
}
typedef struct {const RootCell *root;uint64_t offset,bits;bool loaded;} KgStringReader;
static unsigned kgs_byte(KgStringReader *reader,uint64_t index){
    uint64_t offset=index&~UINT64_C(7);
    if(!reader->loaded||reader->offset!=offset){reader->bits=kg_load_i64(reader->root,16+offset);reader->offset=offset;reader->loaded=true;}
    return (unsigned)((reader->bits>>((index&7)*8))&255);
}
uint64_t kg_string_length(const RootCell *string){return kg_load_i64(string,8);}
void kg_string_concat(const RootCell *left,const RootCell *right,RootCell *out){
    /* Output may alias either input. Capture both values before allocation
     * clears that output cell or another GC boundary is reached. */
    RootCell *a=kg_root_new(),*b=kg_root_new();kg_root_copy(a,left);kg_root_copy(b,right);
    uint64_t na=kg_load_i64(a,0),nb=kg_load_i64(b,0);
    if(na>UINT64_MAX-nb)kgs_fail("string length overflow");
    uint64_t length=na+nb,payload=kgs_payload(length);
    uint64_t ca=kg_string_length(a),cb=kg_string_length(b);
    if(ca>INT64_MAX||cb>(uint64_t)INT64_MAX-ca)kgs_fail("character count overflow");
    kg_alloc_into(payload,1,out);KgStringReader ar={.root=a},br={.root=b};
    for(uint64_t offset=0;offset<length;offset+=8){
        uint64_t bits=0,n=length-offset;if(n>8)n=8;
        for(uint64_t i=0;i<n;i++){
            uint64_t index=offset+i;unsigned byte=index<na?kgs_byte(&ar,index):kgs_byte(&br,index-na);
            bits|=(uint64_t)byte<<(i*8);
        }
        kg_store_i64(out,16+offset,bits);
    }
    kg_store_i64(out,0,length);kg_store_i64(out,8,ca+cb);kg_root_release(b);kg_root_release(a);
}
int64_t kg_string_compare(const RootCell *left,const RootCell *right){
    uint64_t a=kg_load_i64(left,0),b=kg_load_i64(right,0),n=a<b?a:b;
    KgStringReader ar={.root=left},br={.root=right};
    for(uint64_t i=0;i<n;i++){unsigned x=kgs_byte(&ar,i),y=kgs_byte(&br,i);if(x!=y)return x<y?-1:1;}
    return a==b?0:a<b?-1:1;
}
void kg_string_println(const RootCell *string){
    uint64_t length=kg_load_i64(string,0);KgStringReader reader={.root=string};
    for(uint64_t offset=0;offset<length;){
        unsigned char buffer[256];uint64_t n=length-offset;if(n>sizeof(buffer))n=sizeof(buffer);
        for(uint64_t i=0;i<n;i++)buffer[i]=(unsigned char)kgs_byte(&reader,offset+i);
        /* Only immutable native copy-out bytes cross the Native I/O boundary.
         * The managed root remains registered while a collector can move it. */
        kg_native_enter();size_t written=fwrite(buffer,1,(size_t)n,stdout);kg_native_exit();
        if(written!=n)kgs_fail("native string output failed");offset+=n;
    }
    kg_native_enter();int status=fputc('\n',stdout),flushed=fflush(stdout);kg_native_exit();if(status==EOF||flushed==EOF)kgs_fail("native string output failed");
}
