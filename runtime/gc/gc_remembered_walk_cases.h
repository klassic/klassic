#ifndef KLASSIC_GC_REMEMBERED_WALK_CASES_H
#define KLASSIC_GC_REMEMBERED_WALK_CASES_H
#include "gc_remembered_walk.h"
#include "gc_remembered_inventory_cases.h"
typedef uint64_t (*RememberedWalker)(RememberedInventory *,RememberedWalk *,uint64_t);
typedef struct {RememberedInventoryFixture f;RememberedWalk w[2];} RememberedWalkFixture;
static uint64_t rmw_pointers[32];static unsigned rmw_pointer_count,rmw_cases;static FILE *rmw_vectors;
static uint64_t rmw_normalize(RememberedWalkFixture *d,const RememberedWalkFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<rmw_pointer_count;i++){uint64_t v;unsigned char *p=(unsigned char *)d+rmw_pointers[i];memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}}return mask;}
static void rmw_clone(RememberedWalkFixture *d,const RememberedWalkFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=rmw_normalize(d,s);
    for(unsigned i=0;i<rmw_pointer_count;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+rmw_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void rmw_compare(RememberedWalkFixture *a,RememberedWalker step,uint64_t op,uint64_t expected){
    RememberedWalkFixture c,ba,bc,x,y;rmw_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t got=step(&a->f.g,&a->w[0],op),reference=gc_remembered_walk_locked(&c.f.g,&c.w[0],op);
    if(got!=expected||reference!=expected){fprintf(stderr,"remembered walk case %u op %llu got %llu C %llu expected %llu\n",rmw_cases,(unsigned long long)op,(unsigned long long)got,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(got==GC_RMW_INVALID||got==GC_RMW_EXHAUSTED)assert(!memcmp(&ba,a,sizeof(ba))&&!memcmp(&bc,&c,sizeof(bc)));
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(rmw_normalize(&x,a)==rmw_normalize(&y,&c)&&!memcmp(&x,&y,sizeof(x)));
    if(rmw_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,rmw_normalize(&x,&c),rmw_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,rmw_vectors)==1&&fwrite(&x,sizeof(x),1,rmw_vectors)==1&&fwrite(&y,sizeof(y),1,rmw_vectors)==1);}rmw_cases++;
}
static void rmw_inventory(RememberedWalkFixture *a,RememberedInventoryStep step,uint64_t op,uint64_t expected){
    RememberedWalkFixture before;memcpy(&before,a,sizeof(before));assert(step(&a->f.g,&a->f.q,op)==expected);
    if(expected!=GC_RS_OK)assert(!memcmp(&before,a,sizeof(before)));
}
static void gc_run_remembered_walk_cases(RememberedWalker step,RememberedInventoryStep inventory){
    rmw_pointer_count=sizeof(ri_pointers)/8;memcpy(rmw_pointers,ri_pointers,sizeof(ri_pointers));
    const uint64_t extra[]={offsetof(RememberedWalkFixture,w[0].registry),offsetof(RememberedWalkFixture,w[0].registration),offsetof(RememberedWalkFixture,w[1].registry),offsetof(RememberedWalkFixture,w[1].registration)};
    memcpy(rmw_pointers+rmw_pointer_count,extra,sizeof(extra));rmw_pointer_count+=sizeof(extra)/8;assert(rmw_pointer_count<32);rmw_cases=0;
    const char *path=getenv("KLASSIC_GC_REMEMBERED_WALK_VECTORS");
    if(path){rmw_vectors=fopen(path,"wb");assert(rmw_vectors);assert(fwrite("KLRMWK01",8,1,rmw_vectors)==1);
        uint64_t header[]={sizeof(RememberedWalkFixture),offsetof(RememberedWalkFixture,f.g),offsetof(RememberedWalkFixture,w[0]),rmw_pointer_count};
        assert(fwrite(header,sizeof(header),1,rmw_vectors)==1&&fwrite(rmw_pointers,rmw_pointer_count*8,1,rmw_vectors)==1);}
    const unsigned epochs[]={0,15,16,31},orders[6][3]={{0,1,2},{0,2,1},{1,0,2},{1,2,0},{2,0,1},{2,1,0}};
    RememberedWalkFixture a;
    for(unsigned ep=0;ep<4;ep++)for(unsigned order=0;order<6;order++)for(unsigned members=0;members<=3;members++){
        memset(&a,0,sizeof(a));ri_fixture(&a.f,epochs[ep]);
        for(unsigned i=0;i<members;i++){ri_select(&a.f,orders[order][i]);rmw_inventory(&a,inventory,GC_RI_ATTACH,GC_RS_OK);}
        rmw_compare(&a,step,GC_RMW_BEGIN,members?GC_RMW_ITEM:GC_RMW_DONE);
        if(!members){assert(!a.f.g.walkers&&!a.w[0].registry);continue;}
        assert(step(&a.f.g,&a.w[1],GC_RMW_BEGIN)==GC_RMW_ITEM&&a.f.g.walkers==2);
        for(unsigned i=0;i<members;i++){
            unsigned expected=orders[order][members-1-i];assert(a.w[0].registration==(uintptr_t)&a.f.r[expected]&&a.w[0].limit==members);
            ri_select(&a.f,expected);rmw_inventory(&a,inventory,GC_RI_DETACH,GC_RS_WAIT);
            rmw_inventory(&a,inventory,GC_RI_CHECK,GC_RS_WAIT);
            a.f.q.phase=((a.f.g.epoch+0x10000)^0x1000)&~UINT64_C(15);a.f.q.phase|=1;
            rmw_inventory(&a,inventory,GC_RI_BEGIN,GC_RS_WAIT);
            if(!i&&members<3){ri_select(&a.f,orders[order][members]);rmw_inventory(&a,inventory,GC_RI_ATTACH,GC_RS_OK);}
            rmw_compare(&a,step,GC_RMW_NEXT,i+1<members?GC_RMW_ITEM:GC_RMW_DONE);
        }
        assert(a.f.g.walkers==1&&!a.w[0].registry);
        rmw_compare(&a,step,GC_RMW_CANCEL,GC_RMW_INVALID); // no double release
        assert(step(&a.f.g,&a.w[1],GC_RMW_CANCEL)==GC_RMW_DONE&&!a.f.g.walkers);
        rmw_inventory(&a,inventory,GC_RI_CHECK,GC_RS_EMPTY);
        unsigned total=members<3?members+1:members;
        rmw_compare(&a,step,GC_RMW_BEGIN,GC_RMW_ITEM);
        for(unsigned i=0;i<total;i++){
            assert(a.w[0].registration==(uintptr_t)&a.f.r[orders[order][total-1-i]]);
            rmw_compare(&a,step,GC_RMW_NEXT,i+1<total?GC_RMW_ITEM:GC_RMW_DONE);
        }
        for(unsigned i=0;i<total;i++){ri_select(&a.f,orders[order][i]);rmw_inventory(&a,inventory,GC_RI_DETACH,GC_RS_OK);}
        assert(!a.f.g.members&&!a.f.g.walkers);
    }
    // Each returned page may be flipped/claimed through the regular inventory
    // wrapper. Releasing the walk never consumes that independently held job.
    for(unsigned ep=0;ep<32;ep++){
        memset(&a,0,sizeof(a));ri_fixture(&a.f,ep);
        for(unsigned i=0;i<3;i++){ri_select(&a.f,i);rmw_inventory(&a,inventory,GC_RI_ATTACH,GC_RS_OK);rmw_inventory(&a,inventory,GC_RI_RECORD,GC_RS_OK);}
        a.f.q.phase=((a.f.g.epoch+0x10000)^0x1000)&~UINT64_C(15);a.f.q.phase|=1;rmw_inventory(&a,inventory,GC_RI_BEGIN,GC_RS_OK);
        rmw_compare(&a,step,GC_RMW_BEGIN,GC_RMW_ITEM);
        for(unsigned i=0;i<3;i++){
            ri_select(&a.f,2-i);assert(a.w[0].registration==a.f.q.registration);
            rmw_inventory(&a,inventory,GC_RI_FLIP,GC_RS_OK);rmw_inventory(&a,inventory,GC_RI_TAKE,GC_RS_OK);
            rmw_compare(&a,step,GC_RMW_NEXT,i==2?GC_RMW_DONE:GC_RMW_ITEM);
            assert(a.f.g.claims==1);rmw_inventory(&a,inventory,GC_RI_CHECK,GC_RS_WAIT);
            rmw_inventory(&a,inventory,GC_RI_ACK,GC_RS_OK);
        }
        rmw_inventory(&a,inventory,GC_RI_CHECK,GC_RS_EMPTY);assert(!a.f.g.walkers&&!a.f.g.claims&&!a.f.g.pending&&!a.f.g.unflipped);
    }
    for(unsigned fault=0;fault<27;fault++){
        memset(&a,0,sizeof(a));ri_fixture(&a.f,0);
        for(unsigned i=0;i<3;i++){ri_select(&a.f,i);assert(inventory(&a.f.g,&a.f.q,GC_RI_ATTACH)==GC_RS_OK);}
        uint64_t op=GC_RMW_BEGIN,expected=GC_RMW_INVALID;
        if(fault>=8){assert(step(&a.f.g,&a.w[0],GC_RMW_BEGIN)==GC_RMW_ITEM);op=GC_RMW_NEXT;}
        switch(fault){
        case 0:op=3;break;case 1:a.w[0].registry=(uintptr_t)&a.f.g;break;case 2:a.w[0].registration=(uintptr_t)&a.f.r[0];break;
        case 3:a.w[0].identity=1;break;case 4:a.w[0].limit=1;break;case 5:a.w[0].epoch=1;break;
        case 6:a.f.g.walkers=UINT64_MAX;expected=GC_RMW_EXHAUSTED;break;case 7:a.f.r[2].set=0;break;
        case 8:a.w[0].registry=0;break;case 9:a.f.g.walkers=0;break;case 10:a.w[0].epoch++;break;
        case 11:a.w[0].identity=0;break;case 12:a.w[0].identity=4;break;case 13:a.w[0].limit=4;break;
        case 14:a.w[0].registration=0;break;case 15:a.w[0].registration++;break;case 16:a.w[0].identity=2;break;
        case 17:a.f.r[2].next=(uintptr_t)&a.f.r[2];break;case 18:a.f.r[1].previous=0;break;
        case 19:a.f.r[1].set=0;break;case 20:a.f.r[1].registry=0;break;case 21:a.f.s[1].epoch++;break;
        case 22:a.f.g.pending=4;break;case 23:a.f.g.epoch=0;break;case 24:a.f.g.head=0;break;
        case 25:a.w[0].limit=0;break;case 26:op=GC_RMW_BEGIN;break;}
        rmw_compare(&a,step,op,expected);
    }
    if(rmw_vectors){assert(fclose(rmw_vectors)==0);rmw_vectors=NULL;}
    printf("remembered walk %u C/candidate bounded traversal, membership retention and claim handoff cases PASS\n",rmw_cases);
}
#endif
