#ifndef KLASSIC_GC_REMEMBERED_OWNER_CASES_H
#define KLASSIC_GC_REMEMBERED_OWNER_CASES_H
#include "gc_remembered_owner.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RememberedOwnerFinder)(RememberedOwner *);
typedef struct {
    RememberedOwner q;RememberedRegistration r;RememberedSet set;RememberedJob claim;RememberedInventory inventory;
    AllocationPage page;ForwardTable table;ForwardCell cells[33];_Atomic uint64_t starts[4];
} OwnerFixture;
static const uint64_t owner_pointers[]={offsetof(OwnerFixture,q.registration),offsetof(OwnerFixture,q.claim),offsetof(OwnerFixture,q.page),offsetof(OwnerFixture,q.cell),
    offsetof(OwnerFixture,r.registry),offsetof(OwnerFixture,r.set),offsetof(OwnerFixture,page.forward_table),offsetof(OwnerFixture,page.starts),offsetof(OwnerFixture,table.cells)};
static FILE *owner_vectors;static unsigned owner_cases;
static void owner_fixture(OwnerFixture *f,unsigned used,uint64_t slot){
    memset(f,0,sizeof(*f));
    // Registry counters/maps are intentionally not populated: this metadata
    // leaf may read only stable claim-owned fields, never unlocked aggregates.
    f->r=(RememberedRegistration){.registry=(uintptr_t)&f->inventory,.set=(uintptr_t)&f->set,.identity=1,.epoch=0x21101};
    f->set=(RememberedSet){.start=4096,.limit=8192,.incarnation=17,.epoch=0x21101,.active_serial=9,.active_offset=slot};
    f->claim=(RememberedJob){.offset=slot,.incarnation=17,.phase=0x21101,.serial=9};
    f->q=(RememberedOwner){.registration=(uintptr_t)&f->r,.claim=(uintptr_t)&f->claim,.page=(uintptr_t)&f->page};
    f->page=(AllocationPage){.start=4096,.limit=8192,.incarnation=17,.generation=GC_OLD,.state=GC_PAGE_CLOSED,
        .starts=(uintptr_t)f->starts,.starts_words=4,.allocated_top=4096+96*used,.forward_table=(uintptr_t)&f->table};
    f->table=(ForwardTable){(uintptr_t)f->cells,33,used};
    for(unsigned i=0;i<33;i++){
        uint64_t owner=4096+96*i+32;f->cells[i]=(ForwardCell){owner,17,0,0};
        if(i<used){uint64_t bit=(owner-4096)/16;atomic_fetch_or(&f->starts[bit/64],UINT64_C(1)<<(bit%64));}
    }
}
static uint64_t owner_normalize(OwnerFixture *d,const OwnerFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<sizeof(owner_pointers)/8;i++){unsigned char *p=(unsigned char *)d+owner_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}}return mask;}
static void owner_clone(OwnerFixture *d,const OwnerFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=owner_normalize(d,s);
    for(unsigned i=0;i<sizeof(owner_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+owner_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void owner_effect(const OwnerFixture *before,const OwnerFixture *after,uint64_t status){
    OwnerFixture e;memcpy(&e,before,sizeof(e));if(status==GC_RO_OK){
        // Independent linear oracle; the implementation uses binary search.
        unsigned used=(unsigned)atomic_load(&e.table.used),candidate=used;
        for(unsigned i=0;i<used;i++)if(e.cells[i].source_offset<=e.claim.offset)candidate=i;
        assert(candidate<used);uint64_t start=e.cells[candidate].source_offset;
        assert(start<=e.claim.offset&&e.claim.offset<atomic_load(&e.page.allocated_top));
        if(candidate+1<used)assert(e.claim.offset+8<=e.cells[candidate+1].source_offset-32);
        e.q.owner=start;e.q.byte_offset=e.claim.offset-start;e.q.cell=e.table.cells+candidate*sizeof(ForwardCell);
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void owner_compare(OwnerFixture *a,RememberedOwnerFinder step,uint64_t expected){
    OwnerFixture c,ba,bc,x,y;owner_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->q),reference=gc_remembered_owner(&c.q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"remembered owner case %u got %llu C %llu expected %llu\n",owner_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    owner_effect(&ba,a,actual);owner_effect(&bc,&c,reference);memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(owner_normalize(&x,a)==owner_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(owner_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,owner_normalize(&x,&c),owner_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,owner_vectors)==1);assert(fwrite(&x,sizeof(x),1,owner_vectors)==1);assert(fwrite(&y,sizeof(y),1,owner_vectors)==1);}owner_cases++;
}
static void gc_run_remembered_owner_cases(RememberedOwnerFinder step){
    OwnerFixture a;owner_cases=0;const char *path=getenv("KLASSIC_GC_REMEMBERED_OWNER_VECTORS");
    if(path){owner_vectors=fopen(path,"wb");assert(owner_vectors);assert(fwrite("KLROWN01",8,1,owner_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(OwnerFixture,q),offsetof(OwnerFixture,q),sizeof(owner_pointers)/8};assert(fwrite(header,sizeof(header),1,owner_vectors)==1);assert(fwrite(owner_pointers,sizeof(owner_pointers),1,owner_vectors)==1);}
    const unsigned offsets[]={0,24,32,40,88};
    for(unsigned used=0;used<=33;used++)for(unsigned object=0;object<=33;object++)for(unsigned at=0;at<5;at++){
        owner_fixture(&a,used,4096+96*object+offsets[at]);owner_compare(&a,step,object<used&&offsets[at]>=32?GC_RO_OK:GC_RO_MISSING);
    }
    // A release-published table suffix can precede starts/top publication.
    for(unsigned used=1;used<33;used++)for(unsigned at=0;at<3;at++){
        uint64_t slot=at==2?4096+96*used+32:4096+96*(used-1)+(at?88:32);
        owner_fixture(&a,used,slot);atomic_store(&a.table.used,used+1);owner_compare(&a,step,at==2?GC_RO_MISSING:GC_RO_OK);
    }
    for(unsigned fault=0;fault<48;fault++){
        owner_fixture(&a,2,4136);
        switch(fault){case 0:a.q.owner=1;break;case 1:a.q.byte_offset=1;break;case 2:a.q.cell=8;break;
        case 3:a.q.registration=0;break;case 4:a.q.registration++;break;case 5:a.q.claim=0;break;case 6:a.q.claim++;break;
        case 7:a.q.page=0;break;case 8:a.q.page++;break;case 9:a.r.registry=0;break;case 10:a.r.registry++;break;
        case 11:a.r.identity=0;break;case 12:a.r.set=0;break;case 13:a.r.set++;break;case 14:a.claim.serial=0;break;
        case 15:a.claim.serial++;break;case 16:a.claim.incarnation++;break;case 17:a.claim.offset+=8;break;
        case 18:a.claim.phase++;break;case 19:a.r.epoch++;break;case 20:a.set.incarnation=0;break;
        case 21:a.set.start=0;break;case 22:a.set.limit=a.set.start;break;case 23:a.set.limit=UINT64_MAX;break;
        case 24:a.set.start++;break;case 25:a.claim.offset=a.set.active_offset=a.set.limit;break;case 26:a.claim.offset=a.set.active_offset=4137;break;
        case 27:a.page.start+=16;break;case 28:a.page.limit+=16;break;case 29:a.page.incarnation++;break;case 30:a.page.generation=GC_YOUNG;break;
        case 31:atomic_store(&a.page.state,GC_PAGE_READY);break;case 32:atomic_store(&a.page.allocated_top,8193);break;
        case 33:atomic_store(&a.page.allocated_top,4095);break;case 34:a.page.forward_table=0;break;case 35:a.page.forward_table++;break;
        case 36:a.page.starts=0;break;case 37:a.page.starts++;break;case 38:a.page.starts_words=3;break;case 39:a.page.starts_words=UINT64_MAX;break;
        case 40:a.page.starts=UINT64_MAX-7;break;case 41:a.table.cells=0;break;case 42:a.table.cells++;break;
        case 43:a.table.capacity=0;break;case 44:a.table.capacity=UINT64_MAX;break;case 45:a.table.cells=UINT64_MAX-7;break;
        case 46:atomic_store(&a.table.used,34);break;case 47:atomic_store(&a.starts[0],0);break;}
        owner_compare(&a,step,GC_RO_INVALID);
    }
    for(unsigned fault=0;fault<7;fault++){
        owner_fixture(&a,2,4136);
        switch(fault){case 0:a.cells[0].incarnation++;break;case 1:a.cells[0].source_offset++;break;
        case 2:a.cells[0].source_offset=4096;break;case 3:a.cells[1].incarnation++;break;
        case 4:a.cells[1].source_offset++;break;case 5:a.cells[1].source_offset=a.cells[0].source_offset+32;break;case 6:a.cells[1].source_offset=8192;break;}
        owner_compare(&a,step,GC_RO_INVALID);
    }
    for(unsigned state=0;state<4;state++){
        owner_fixture(&a,2,4136);atomic_store(&a.cells[0].word,state);atomic_store(&a.cells[0].access,UINT64_MAX);
        owner_compare(&a,step,GC_RO_OK);
    }
    owner_fixture(&a,0,4136);atomic_store(&a.page.allocated_top,4192);owner_compare(&a,step,GC_RO_MISSING);
    for(unsigned state=GC_PAGE_EVACUATED;state<=GC_PAGE_UNCOMMITTED;state++){
    owner_fixture(&a,2,4136);atomic_store(&a.page.state,state);
    for(unsigned i=0;i<2;i++){atomic_store(&a.cells[i].word,((UINT64_C(0x50000)+i*48)<<16)|0x1512);atomic_store(&a.cells[i].access,GC_SOURCE_CLOSED);}
    owner_compare(&a,step,GC_RO_OK);
    }
    if(owner_vectors){assert(!fclose(owner_vectors));owner_vectors=NULL;}
    printf("remembered owner %u C/candidate predecessor, prefix and claim validation cases PASS\n",owner_cases);
}
#endif
