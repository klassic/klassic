#ifndef KLASSIC_GC_FORWARD_TABLE_CASES_H
#define KLASSIC_GC_FORWARD_TABLE_CASES_H
#include "gc_forward_table.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*ForwardFinder)(const ForwardTable *,ForwardLookup *);
typedef struct {ForwardTable g;ForwardLookup q;ForwardCell cells[33];} LookupFixture;
static const uint64_t lookup_pointers[]={offsetof(LookupFixture,g.cells),offsetof(LookupFixture,q.cell)};
static FILE *lookup_vectors;static unsigned lookup_cases;
static void lookup_fixture(LookupFixture *f) {
    memset(f,0,sizeof(*f));f->g=(ForwardTable){(uintptr_t)f->cells,33,0};
    f->q=(ForwardLookup){32,27,0};
    for(unsigned i=0;i<33;i++)f->cells[i]=(ForwardCell){32+32*i,27,0,0};
}
static uint64_t lookup_normalize(LookupFixture *dst,const LookupFixture *src) {
    uint64_t mask=0;for(unsigned i=0;i<2;i++){
        unsigned char *p=(unsigned char *)dst+lookup_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void lookup_copy(LookupFixture *dst,const LookupFixture *src) {
    memcpy(dst,src,sizeof(*dst));uint64_t mask=lookup_normalize(dst,src);
    for(unsigned i=0;i<2;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+lookup_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void lookup_compare(LookupFixture *a,LookupFixture *c,ForwardFinder step,uint64_t expected) {
    LookupFixture before_a,before,x,y;memcpy(&before_a,a,sizeof(*a));memcpy(&before,c,sizeof(*c));
    uint64_t actual=step(&a->g,&a->q),reference=gc_forward_lookup(&c->g,&c->q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"forward lookup case %u got %llu C %llu expected %llu\n",lookup_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(expected){assert(!memcmp(&before,c,sizeof(before)));assert(!memcmp(&before_a,a,sizeof(before_a)));}
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(lookup_normalize(&x,a)==lookup_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(lookup_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t record[]={0,reference,lookup_normalize(&x,c),lookup_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,lookup_vectors)==1);assert(fwrite(&x,sizeof(x),1,lookup_vectors)==1);assert(fwrite(&y,sizeof(y),1,lookup_vectors)==1);}
    lookup_cases++;
}
static void gc_run_forward_lookup_cases(ForwardFinder step) {
    LookupFixture a,c;lookup_cases=0;const char *path=getenv("KLASSIC_GC_FORWARD_LOOKUP_VECTORS");
    if(path){lookup_vectors=fopen(path,"wb");assert(lookup_vectors);assert(fwrite("KLFLKP01",8,1,lookup_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(LookupFixture,g),offsetof(LookupFixture,q),2};assert(fwrite(header,sizeof(header),1,lookup_vectors)==1);assert(fwrite(lookup_pointers,sizeof(lookup_pointers),1,lookup_vectors)==1);}
    // Includes every insertion gap, an initialized but unpublished suffix,
    // empty prefixes and lengths around powers of two.
    for(unsigned used=0;used<=33;used++)for(unsigned key=1;key<=68;key++) {
        lookup_fixture(&a);atomic_store(&a.g.used,used);a.q.source_offset=key*16;lookup_copy(&c,&a);
        lookup_compare(&a,&c,step,key%2==0&&key/2<=used?GC_FL_OK:GC_FL_MISSING);
    }
    for(unsigned fault=0;fault<13;fault++) {
        lookup_fixture(&a);atomic_store(&a.g.used,1);
        switch(fault){case 0:a.q.cell=8;break;case 1:a.q.source_offset=0;break;case 2:a.q.source_offset++;break;
        case 3:a.q.source_offset=UINT64_MAX;break;case 4:a.q.incarnation=0;break;case 5:a.q.incarnation++;break;
        case 6:a.g.cells=0;break;case 7:a.g.cells++;break;case 8:a.g.capacity=0;break;case 9:a.g.capacity=UINT64_MAX;break;
        case 10:a.g.cells=UINT64_MAX-7;break;case 11:atomic_store(&a.g.used,34);break;case 12:a.cells[0].incarnation=0;break;}
        lookup_copy(&c,&a);lookup_compare(&a,&c,step,GC_FL_INVALID);
    }
    // Lookup does not inspect mutable forwarding/access words or heap bytes.
    for(unsigned state=0;state<4;state++) {
        lookup_fixture(&a);atomic_store(&a.g.used,1);atomic_store(&a.cells[0].word,state);atomic_store(&a.cells[0].access,UINT64_MAX);
        lookup_copy(&c,&a);lookup_compare(&a,&c,step,GC_FL_OK);
    }
    if(lookup_vectors){assert(fclose(lookup_vectors)==0);lookup_vectors=NULL;}
    printf("forward lookup %u C/candidate metadata-only cases PASS\n",lookup_cases);
}
#endif
