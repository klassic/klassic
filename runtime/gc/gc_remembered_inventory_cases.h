#ifndef KLASSIC_GC_REMEMBERED_INVENTORY_CASES_H
#define KLASSIC_GC_REMEMBERED_INVENTORY_CASES_H
#include "gc_remembered_inventory.h"
#include "gc_mark_end_cases.h"
typedef uint64_t (*RememberedInventoryStep)(RememberedInventory *,RememberedInventoryRequest *,uint64_t);
typedef struct {RememberedInventory g;RememberedRegistration r[3];RememberedSet s[3];uint64_t bits[3][2][2];RememberedJob job;RememberedInventoryRequest q;AllocationPage page;} RememberedInventoryFixture;
#define RI_PTR(i) offsetof(RememberedInventoryFixture,r[i].registry),offsetof(RememberedInventoryFixture,r[i].set),offsetof(RememberedInventoryFixture,r[i].previous),offsetof(RememberedInventoryFixture,r[i].next),offsetof(RememberedInventoryFixture,s[i].bits[0]),offsetof(RememberedInventoryFixture,s[i].bits[1])
static const uint64_t ri_pointers[]={offsetof(RememberedInventoryFixture,g.head),RI_PTR(0),RI_PTR(1),RI_PTR(2),offsetof(RememberedInventoryFixture,q.registration),offsetof(RememberedInventoryFixture,q.job),offsetof(RememberedInventoryFixture,q.phase)};
#undef RI_PTR
static unsigned ri_cases;static FILE *ri_vectors;
static uint64_t ri_normalize(RememberedInventoryFixture *d,const RememberedInventoryFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<sizeof(ri_pointers)/8;i++){unsigned char *p=(unsigned char *)d+ri_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}}return mask;}
static void ri_clone(RememberedInventoryFixture *d,const RememberedInventoryFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=ri_normalize(d,s);
    for(unsigned i=0;i<sizeof(ri_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+ri_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void ri_fixture(RememberedInventoryFixture *f,unsigned ep){memset(f,0,sizeof(*f));f->g.epoch=f->g.last_phase=0x10022|(ep<<8);
    for(unsigned i=0;i<3;i++){f->r[i].set=(uintptr_t)&f->s[i];f->s[i]=(RememberedSet){.start=4096*(i+1),.limit=4096*(i+1)+520,.incarnation=17+i,
        .bits={(uintptr_t)f->bits[i][0],(uintptr_t)f->bits[i][1]},.words=2,.epoch=f->g.epoch,.last_phase=f->g.epoch};}
    f->q=(RememberedInventoryRequest){.registration=(uintptr_t)&f->r[0],.job=(uintptr_t)&f->job};}
static void ri_select(RememberedInventoryFixture *f,unsigned index){f->q.registration=(uintptr_t)&f->r[index];f->job=(RememberedJob){.offset=f->s[index].start+512,.incarnation=f->s[index].incarnation,.phase=f->g.epoch,.budget=2};}
static void ri_audit(const RememberedInventoryFixture *f){
    uint64_t members=0,pending=0,claims=0,unflipped=0,previous=0,id=UINT64_MAX;
    for(uint64_t p=f->g.head;p;){assert(++members<=3);RememberedRegistration *r=(RememberedRegistration *)(uintptr_t)p;
        assert(r->registry==(uintptr_t)&f->g&&r->previous==previous&&r->identity&&r->identity<=id);id=r->identity-1;
        RememberedSet *s=(RememberedSet *)(uintptr_t)r->set;assert(s->epoch==r->epoch&&s->last_phase<=f->g.last_phase);pending+=!!s->pending;claims+=!!s->active_serial;unflipped+=r->epoch!=f->g.epoch;
        unsigned index=(unsigned)(s-f->s);assert(index<3);unsigned counts[2]={0};
        for(unsigned bank=0;bank<2;bank++)for(unsigned w=0;w<2;w++)for(unsigned bit=0;bit<64;bit++)counts[bank]+=(f->bits[index][bank][w]>>bit)&1;
        assert(s->current_count==counts[s->current]&&s->pending==counts[1-s->current]);previous=p;p=r->next;
    }
    assert(members==f->g.members&&pending==f->g.pending&&claims==f->g.claims&&unflipped==f->g.unflipped);
}
static void ri_compare(RememberedInventoryFixture *a,RememberedInventoryStep step,uint64_t op,uint64_t expected){
    RememberedInventoryFixture c,ba,bc,x,y;ri_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->g,&a->q,op),reference=gc_remembered_inventory_step_locked(&c.g,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"remembered inventory case %u op %llu got %llu C %llu expected %llu\n",ri_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(actual!=GC_RS_OK&&actual!=GC_RS_MORE)assert(!memcmp(&ba,a,sizeof(ba))&&!memcmp(&bc,&c,sizeof(bc)));
    else{ri_audit(a);ri_audit(&c);}
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(ri_normalize(&x,a)==ri_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(ri_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,ri_normalize(&x,&c),ri_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,ri_vectors)==1);assert(fwrite(&x,sizeof(x),1,ri_vectors)==1);assert(fwrite(&y,sizeof(y),1,ri_vectors)==1);}ri_cases++;
}
static void gc_run_remembered_inventory_cases(RememberedInventoryStep step){
    RememberedInventoryFixture a;ri_cases=0;const char *path=getenv("KLASSIC_GC_REMEMBERED_INVENTORY_VECTORS");
    if(path){ri_vectors=fopen(path,"wb");assert(ri_vectors);assert(fwrite("KLREMI01",8,1,ri_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(RememberedInventoryFixture,g),offsetof(RememberedInventoryFixture,q),sizeof(ri_pointers)/8};assert(fwrite(header,sizeof(header),1,ri_vectors)==1);assert(fwrite(ri_pointers,sizeof(ri_pointers),1,ri_vectors)==1);}
    const unsigned epochs[]={0,15,16,31},orders[6][3]={{0,1,2},{0,2,1},{1,0,2},{1,2,0},{2,0,1},{2,1,0}};
    for(unsigned ep=0;ep<4;ep++)for(unsigned order=0;order<6;order++){
        ri_fixture(&a,epochs[ep]);uint64_t old=a.g.epoch;
#define RI(op,status) ri_compare(&a,step,op,status)
        for(unsigned i=0;i<3;i++){ri_select(&a,i);RI(GC_RI_ATTACH,GC_RS_OK);RI(GC_RI_RECORD,GC_RS_OK);}
        a.q.phase=((old+0x10000)^0x1000)&~UINT64_C(15);a.q.phase|=1;RI(GC_RI_BEGIN,GC_RS_OK);RI(GC_RI_CHECK,GC_RS_WAIT);
        ri_select(&a,0);RI(GC_RI_RECORD,GC_RS_WAIT);RI(GC_RI_TAKE,GC_RS_WAIT);a.job.phase=old;RI(GC_RI_RECORD,GC_RS_OK);
        for(unsigned i=0;i<3;i++){ri_select(&a,orders[order][i]);RI(GC_RI_FLIP,GC_RS_OK);RI(GC_RI_CHECK,GC_RS_WAIT);}
        for(unsigned i=0;i<3;i++){
            ri_select(&a,i);RI(GC_RI_TAKE,GC_RS_OK);RememberedJob claim=a.job;RI(GC_RI_CHECK,GC_RS_WAIT);
            if(i==0){ri_select(&a,0);a.job.phase=old;RI(GC_RI_RECORD,GC_RS_OK);a.job=claim;}
            RI(GC_RI_ACK,GC_RS_OK);RI(GC_RI_ACK,GC_RS_INVALID);
            if(i==0){ri_select(&a,0);RI(GC_RI_TAKE,GC_RS_OK);RI(GC_RI_ACK,GC_RS_OK);}
        }
        RI(GC_RI_CHECK,GC_RS_EMPTY);ri_select(&a,0);RI(GC_RI_DETACH,GC_RS_WAIT);
        a.q.phase=(a.g.epoch+0x10000)^0x1000;RI(GC_RI_BEGIN,GC_RS_OK);
        for(unsigned i=0;i<3;i++){ri_select(&a,i);RI(GC_RI_FLIP,GC_RS_OK);}
        ri_select(&a,0);RI(GC_RI_TAKE,GC_RS_OK);RI(GC_RI_ACK,GC_RS_OK);RI(GC_RI_CHECK,GC_RS_EMPTY);
        for(unsigned i=0;i<3;i++){ri_select(&a,orders[order][i]);RI(GC_RI_DETACH,GC_RS_OK);}
        RI(GC_RI_CHECK,GC_RS_EMPTY);assert(!a.g.head&&!a.g.members&&a.g.last_identity==3);
#undef RI
    }
    for(unsigned fault=0;fault<31;fault++) {
        ri_fixture(&a,0);uint64_t op=GC_RI_ATTACH,expected=GC_RS_INVALID;
        if(fault>=10){for(unsigned i=0;i<3;i++){ri_select(&a,i);assert(gc_remembered_inventory_step_locked(&a.g,&a.q,GC_RI_ATTACH)==0);}ri_select(&a,1);op=GC_RI_RECORD;}
        switch(fault){case 0:op=10;break;case 1:a.g.head=(uintptr_t)&a.r[0];break;case 2:a.g.epoch=0;break;
        case 3:a.g.previous_epoch=a.g.epoch;break;case 4:a.g.last_identity=UINT64_MAX;expected=GC_RS_EXHAUSTED;break;
        case 5:a.q.registration=0;break;case 6:a.q.job++;break;case 7:a.r[0].set=0;break;
        case 8:a.r[0].identity=1;break;case 9:a.s[0].current_count=1;break;
        case 10:a.r[1].registry=0;break;case 11:a.r[1].identity=0;break;case 12:a.r[1].epoch++;break;
        case 13:a.r[1].previous++;break;case 14:a.r[2].next=0;break;case 15:a.r[0].previous=0;break;
        case 16:a.g.pending=3;break;case 17:a.g.claims=3;break;case 18:a.g.unflipped=4;break;
        case 19:a.q.phase=0x21022;op=GC_RI_BEGIN;break;case 20:a.q.phase=0x10021;op=GC_RI_BEGIN;break;
        case 21:a.q.phase=0x21021;op=GC_RI_BEGIN;a.g.pending=1;expected=GC_RS_WAIT;break;
        case 22:op=GC_RI_FLIP;break;case 23:a.job.phase=0x10022^0x1000;break;
        case 24:a.r[1].next=(uintptr_t)&a.r[2];break;case 25:a.r[1].previous=0;break;
        case 26:a.g.members=0;break;case 27:a.g.previous_epoch=0x90000;break;
        case 28:a.g.unflipped=3;break;case 29:a.g.last_phase=0;break;case 30:a.g.last_phase|=0xe000;break;}
        ri_compare(&a,step,op,expected);
    }
    ri_fixture(&a,0);ri_select(&a,0);ri_compare(&a,step,GC_RI_ATTACH,GC_RS_OK);
    a.job.phase=0x50022;ri_compare(&a,step,GC_RI_RECORD,GC_RS_OK);
    a.q.phase=0x21021;ri_compare(&a,step,GC_RI_BEGIN,GC_RS_INVALID);
    a.q.phase=0x61021;ri_compare(&a,step,GC_RI_BEGIN,GC_RS_OK);
    ri_compare(&a,step,GC_RI_CHECK,GC_RS_WAIT);
    // Exact page binding and borrowed metadata must be checked before unlink.
    for(unsigned fault=0;fault<15;fault++){
        ri_fixture(&a,0);ri_select(&a,0);
        assert(gc_remembered_inventory_step_locked(&a.g,&a.q,GC_RI_ATTACH)==GC_RS_OK);
        a.page=(AllocationPage){.start=a.s[0].start,.limit=a.s[0].limit,
            .incarnation=a.s[0].incarnation,.generation=GC_OLD,.state=GC_PAGE_CLOSED};
        a.q.phase=(uintptr_t)&a.page;uint64_t expected=GC_RS_INVALID;
        switch(fault){
        case 0:a.q.phase=0;break;case 1:a.q.phase++;break;
        case 2:a.page.start+=16;break;case 3:a.page.limit+=16;break;
        case 4:a.page.incarnation++;break;case 5:a.page.generation=GC_YOUNG;break;
        case 6:atomic_store(&a.page.state,GC_PAGE_FREE);break;
        case 7:atomic_store(&a.page.state,GC_PAGE_ALLOCATING);expected=GC_RS_WAIT;break;
        case 8:a.page.owner=1;expected=GC_RS_WAIT;break;
        case 9:a.page.metadata_leases=1;expected=GC_RS_WAIT;break;
        case 10:a.page.metadata_leases=UINT64_MAX;expected=GC_RS_WAIT;break;
        case 11:a.g.walkers=1;expected=GC_RS_WAIT;break;
        case 12:expected=GC_RS_OK;break;
        case 13:assert(gc_remembered_inventory_step_locked(&a.g,&a.q,GC_RI_RECORD)==GC_RS_OK);expected=GC_RS_WAIT;break;
        case 14:atomic_store(&a.page.state,GC_PAGE_READY);break;
        }
        ri_compare(&a,step,GC_RI_DETACH_PAGE,expected);
    }
    // A held token can drain later; the exact same request then unlinks.
    ri_fixture(&a,0);ri_select(&a,0);
    assert(gc_remembered_inventory_step_locked(&a.g,&a.q,GC_RI_ATTACH)==GC_RS_OK);
    a.page=(AllocationPage){.start=a.s[0].start,.limit=a.s[0].limit,
        .incarnation=a.s[0].incarnation,.generation=GC_OLD,.state=GC_PAGE_CLOSED,.metadata_leases=1};
    a.q.phase=(uintptr_t)&a.page;
    ri_compare(&a,step,GC_RI_DETACH_PAGE,GC_RS_WAIT);
    a.page.metadata_leases=0;ri_compare(&a,step,GC_RI_DETACH_PAGE,GC_RS_OK);
    for(unsigned state=GC_PAGE_EVACUATED;state<=GC_PAGE_UNCOMMITTED;state++){
    ri_fixture(&a,0);ri_select(&a,0);
    assert(gc_remembered_inventory_step_locked(&a.g,&a.q,GC_RI_ATTACH)==GC_RS_OK);
    a.page=(AllocationPage){.start=a.s[0].start,.limit=a.s[0].limit,
        .incarnation=a.s[0].incarnation,.generation=GC_OLD,.state=state,.metadata_leases=1};
    a.q.phase=(uintptr_t)&a.page;ri_compare(&a,step,GC_RI_DETACH_PAGE,GC_RS_WAIT);
    a.page.metadata_leases=0;ri_compare(&a,step,GC_RI_DETACH_PAGE,GC_RS_OK);
    }
    if(ri_vectors){assert(fclose(ri_vectors)==0);ri_vectors=NULL;}
    printf("remembered inventory %u C/candidate registration, aggregate work and flip cases PASS\n",ri_cases);
}
/* Both live page registrations are operated through the shared wrapper. An
 * actual completed owner handshake/empty queue/root certificate cannot hide
 * unflipped pages, pending slots or the last retained claim. No page traversal
 * occurs inside the end predicate. Old marking is independent of this work. */
static void gc_run_remembered_end(RememberedInventoryStep step,MarkEndChecker end){
    for(unsigned round=0;round<32;round++)for(unsigned gen=0;gen<2;gen++){
        RememberedInventoryFixture f;ri_fixture(&f,round);uint64_t old=f.g.epoch;
        for(unsigned i=0;i<2;i++){ri_select(&f,i);assert(step(&f.g,&f.q,GC_RI_ATTACH)==0);assert(step(&f.g,&f.q,GC_RI_RECORD)==0);}
        f.q.phase=((old+0x10000)^0x1000)&~UINT64_C(15);f.q.phase|=1;assert(step(&f.g,&f.q,GC_RI_BEGIN)==0);
        EndFixture e;end_fixture(&e,(f.g.epoch+0x10000)|0x33,gen,3,round%4);e.q.remembered=(uintptr_t)&f.g;
#define END_BUSY() do {EndFixture saved;memcpy(&saved,&e,sizeof(e));assert(end(&e.f.g,&e.q,0)==(gen?GC_ME_READY:GC_ME_WAIT));assert(!memcmp(&saved,&e,sizeof(e)));} while(0)
        END_BUSY();
        for(unsigned i=0;i<2;i++){ri_select(&f,i);assert(step(&f.g,&f.q,GC_RI_FLIP)==0);END_BUSY();}
        RememberedJob held[2];
        for(unsigned i=0;i<2;i++){ri_select(&f,i);assert(step(&f.g,&f.q,GC_RI_TAKE)==0);held[i]=f.job;END_BUSY();}
        assert(!f.g.pending&&f.g.claims==2);
        for(unsigned i=0;i<2;i++){ri_select(&f,i);f.job=held[i];assert(step(&f.g,&f.q,GC_RI_ACK)==0);if(i==0)END_BUSY();}
        assert(end(&e.f.g,&e.q,0)==GC_ME_READY);ri_audit(&f);
#undef END_BUSY
    }
    puts("remembered inventory: 64 two-page Young end checks retain unflipped, pending and claimed work PASS");
}
#endif
