#ifndef KLASSIC_GC_REMEMBERED_TRANSFER_CASES_H
#define KLASSIC_GC_REMEMBERED_TRANSFER_CASES_H
#include "gc_remembered_inventory_cases.h"
#include "gc_remembered_transfer.h"
typedef uint64_t (*RememberedTransferStep)(RememberedInventory *,const RememberedTransfer *);
typedef struct {RememberedInventoryFixture f;RememberedTransfer q;RememberedJob destination_claim;} RememberedTransferFixture;
static uint64_t rt_pointers[32];static unsigned rt_pointer_count,rt_cases;static FILE *rt_vectors;
static void rt_fixture(RememberedTransferFixture *f,unsigned ep,unsigned slot,unsigned source_mode,unsigned destination_mode,unsigned active){
    memset(f,0,sizeof(*f));ri_fixture(&f->f,ep);RememberedInventoryFixture *a=&f->f;
    unsigned source=ep%3,destination=(source+1+(ep/3)%2)%3;uint64_t old=a->g.epoch;
    for(unsigned i=0;i<3;i++){ri_select(a,i);assert(gc_remembered_inventory_step_locked(&a->g,&a->q,GC_RI_ATTACH)==0);}
    ri_select(a,source);a->job.offset=a->s[source].start+slot*8;assert(gc_remembered_inventory_step_locked(&a->g,&a->q,GC_RI_RECORD)==0);
    if(active){ri_select(a,destination);a->job.offset=a->s[destination].start+slot*8;assert(gc_remembered_inventory_step_locked(&a->g,&a->q,GC_RI_RECORD)==0);}
    a->q.phase=((old+0x10000)^0x1000)&~UINT64_C(15);a->q.phase|=1;assert(gc_remembered_inventory_step_locked(&a->g,&a->q,GC_RI_BEGIN)==0);
    for(unsigned i=0;i<3;i++){ri_select(a,i);assert(gc_remembered_inventory_step_locked(&a->g,&a->q,GC_RI_FLIP)==0);}
    if(active){ri_select(a,destination);assert(gc_remembered_inventory_step_locked(&a->g,&a->q,GC_RI_TAKE)==0);f->destination_claim=a->job;}
    ri_select(a,source);assert(gc_remembered_inventory_step_locked(&a->g,&a->q,GC_RI_TAKE)==0);RememberedJob claim=a->job;
    for(unsigned which=0;which<2;which++){
        unsigned index=which?destination:source,mode=which?destination_mode:source_mode;
        if(mode){ri_select(a,index);a->job.offset=a->s[index].start+slot*8;a->job.phase=mode==2?old:(a->g.epoch+0x10000);
            assert(gc_remembered_inventory_step_locked(&a->g,&a->q,GC_RI_RECORD)==0);}
    }
    if(ep&1)for(unsigned which=0;which<2;which++){
        unsigned index=which?destination:source;ri_select(a,index);a->job.offset=a->s[index].start+(slot?0:8);a->job.phase=old;
        assert(gc_remembered_inventory_step_locked(&a->g,&a->q,GC_RI_RECORD)==0);
    }
    a->job=claim;a->q.registration=(uintptr_t)&a->r[source];
    f->q=(RememberedTransfer){.source=(uintptr_t)&a->r[source],.claim=(uintptr_t)&a->job,.destination=(uintptr_t)&a->r[destination],
        .offset=a->s[destination].start+slot*8,.incarnation=a->s[destination].incarnation};ri_audit(a);
}
static uint64_t rt_normalize(RememberedTransferFixture *d,const RememberedTransferFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<rt_pointer_count;i++){unsigned char *p=(unsigned char *)d+rt_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}}return mask;}
static void rt_clone(RememberedTransferFixture *d,const RememberedTransferFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=rt_normalize(d,s);
    for(unsigned i=0;i<rt_pointer_count;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+rt_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void rt_effect(const RememberedTransferFixture *before,const RememberedTransferFixture *after,uint64_t status){
    RememberedTransferFixture e;memcpy(&e,before,sizeof(e));if(status==GC_RS_OK){
        unsigned si=(unsigned)((RememberedRegistration *)(uintptr_t)before->q.source)->identity-1;
        unsigned di=(unsigned)((RememberedRegistration *)(uintptr_t)before->q.destination)->identity-1;assert(si<3&&di<3&&si!=di);
        RememberedSet *s=&e.f.s[si],*d=&e.f.s[di];uint64_t sb=(e.f.job.offset-s->start)/8,db=(e.q.offset-d->start)/8;
        uint64_t sm=UINT64_C(1)<<(sb%64),dm=UINT64_C(1)<<(db%64);
        if(e.f.bits[si][s->current][sb/64]&sm)e.f.bits[di][d->current][db/64]|=dm;
        e.f.bits[di][1-d->current][db/64]|=dm;
        for(unsigned bank=0;bank<2;bank++)e.f.bits[si][bank][sb/64]&=~sm;
        s->active_serial=s->active_offset=0;if(s->last_phase>d->last_phase)d->last_phase=s->last_phase;
        e.f.g.pending=e.f.g.claims=0;
        for(unsigned i=0;i<3;i++){
            unsigned counts[2]={0};for(unsigned b=0;b<2;b++)for(unsigned w=0;w<2;w++)for(unsigned bit=0;bit<64;bit++)counts[b]+=(e.f.bits[i][b][w]>>bit)&1;
            e.f.s[i].current_count=counts[e.f.s[i].current];e.f.s[i].pending=counts[1-e.f.s[i].current];
            e.f.g.pending+=!!e.f.s[i].pending;e.f.g.claims+=!!e.f.s[i].active_serial;
        }
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void rt_compare(RememberedTransferFixture *a,RememberedTransferStep step,uint64_t expected){
    RememberedTransferFixture c,ba,bc,x,y;rt_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->f.g,&a->q),reference=gc_remembered_transfer_locked(&c.f.g,&c.q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"remembered transfer case %u got %llu C %llu expected %llu\n",rt_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    rt_effect(&ba,a,actual);rt_effect(&bc,&c,reference);if(actual==GC_RS_OK){ri_audit(&a->f);ri_audit(&c.f);assert(a->f.g.pending);}
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(rt_normalize(&x,a)==rt_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(rt_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,rt_normalize(&x,&c),rt_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,rt_vectors)==1);assert(fwrite(&x,sizeof(x),1,rt_vectors)==1);assert(fwrite(&y,sizeof(y),1,rt_vectors)==1);}rt_cases++;
}
static void gc_run_remembered_transfer_cases(RememberedTransferStep step){
    rt_cases=0;rt_pointer_count=sizeof(ri_pointers)/8;memcpy(rt_pointers,ri_pointers,sizeof(ri_pointers));
    const uint64_t extra[]={offsetof(RememberedTransferFixture,q.source),offsetof(RememberedTransferFixture,q.claim),offsetof(RememberedTransferFixture,q.destination)};
    memcpy(rt_pointers+rt_pointer_count,extra,sizeof(extra));rt_pointer_count+=sizeof(extra)/8;assert(rt_pointer_count<32);
    RememberedTransferFixture a;const char *path=getenv("KLASSIC_GC_REMEMBERED_TRANSFER_VECTORS");
    if(path){rt_vectors=fopen(path,"wb");assert(rt_vectors);assert(fwrite("KLREMT01",8,1,rt_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(RememberedTransferFixture,f.g),offsetof(RememberedTransferFixture,q),rt_pointer_count};assert(fwrite(header,sizeof(header),1,rt_vectors)==1);assert(fwrite(rt_pointers,rt_pointer_count*8,1,rt_vectors)==1);}
    const unsigned slots[]={0,63,64};
    for(unsigned ep=0;ep<32;ep++)for(unsigned slot=0;slot<3;slot++)for(unsigned s=0;s<3;s++)for(unsigned d=0;d<3;d++)for(unsigned active=0;active<2;active++){
        rt_fixture(&a,ep,slots[slot],s,d,active);rt_compare(&a,step,GC_RS_OK);
    }
    for(unsigned fault=0;fault<36;fault++){
        rt_fixture(&a,0,64,2,2,1);
        switch(fault){case 0:a.q.source=0;break;case 1:a.q.source++;break;case 2:a.q.destination=0;break;case 3:a.q.destination++;break;
        case 4:a.q.claim=0;break;case 5:a.q.claim++;break;case 6:a.q.destination=a.q.source;a.q.incarnation=17;break;
        case 7:a.f.job.serial++;break;case 8:a.f.job.incarnation++;break;case 9:a.f.job.offset+=8;break;case 10:a.f.job.phase++;break;
        case 11:a.f.job.keep=1;break;case 12:a.q.offset++;break;case 13:a.q.offset=a.f.s[1].limit;break;case 14:a.q.offset=a.f.s[1].start-8;break;
        case 15:a.q.incarnation++;break;case 16:a.f.r[1].registry=0;break;case 17:a.f.r[0].identity=0;break;case 18:a.f.r[1].next=0;break;
        case 19:a.f.g.claims=1;break;case 20:a.f.g.pending=1;break;case 21:a.f.s[0].current_count=0;break;case 22:a.f.s[0].pending=0;break;
        case 23:a.f.s[1].current_count=0;break;case 24:a.f.s[1].pending=0;break;case 25:a.f.s[1].incarnation++;break;
        case 26:a.f.s[0].last_phase=a.f.g.last_phase+0x10000;break;case 27:a.f.s[1].last_phase=a.f.g.last_phase+0x10000;break;
        case 28:a.f.s[1].bits[0]=0;break;case 29:a.f.s[1].current=2;break;case 30:a.f.r[1].epoch++;break;
        case 31:a.f.g.epoch=0;break;case 32:a.f.s[0].active_offset+=8;break;case 33:a.f.s[0].active_serial=0;break;
        case 34:a.f.s[1].start=a.f.s[0].start;a.f.s[1].limit=a.f.s[0].limit;a.q.offset=a.f.s[1].start+512;break;
        case 35:a.f.r[1].set=(uintptr_t)&a.f.s[0];a.q.offset=a.f.s[0].start;a.q.incarnation=17;break;}
        rt_compare(&a,step,GC_RS_INVALID);
    }
    // Undo only an empty destination flip to represent deferred page adoption.
    rt_fixture(&a,0,0,0,0,0);a.f.s[1].epoch=a.f.s[1].last_phase=a.f.g.previous_epoch;a.f.s[1].previous_epoch=0;
    a.f.s[1].current^=1;a.f.r[1].epoch=a.f.g.previous_epoch;a.f.g.unflipped=1;rt_compare(&a,step,GC_RS_WAIT);
    rt_fixture(&a,0,0,0,0,0);rt_compare(&a,step,GC_RS_OK);rt_compare(&a,step,GC_RS_INVALID);
    // A buffer consumer may have captured the old location before publication
    // and only record it after transfer. It remains real source work; another
    // claim remaps it and coalesces with the existing destination pending bit.
    rt_fixture(&a,0,0,0,0,0);rt_compare(&a,step,GC_RS_OK);
    a.f.job=(RememberedJob){.offset=a.f.s[0].start,.incarnation=a.f.s[0].incarnation,.phase=a.f.g.previous_epoch,.budget=2};
    assert(gc_remembered_inventory_step_locked(&a.f.g,&a.f.q,GC_RI_RECORD)==GC_RS_OK&&a.f.g.pending==2);
    assert(gc_remembered_inventory_step_locked(&a.f.g,&a.f.q,GC_RI_TAKE)==GC_RS_OK&&a.f.g.claims==1);
    rt_compare(&a,step,GC_RS_OK);assert(a.f.g.pending==1&&!a.f.g.claims&&a.f.s[1].current_count==1);
    if(rt_vectors){assert(!fclose(rt_vectors));rt_vectors=NULL;}
    printf("remembered transfer %u C/candidate moved-slot, late-bit and claim accounting cases PASS\n",rt_cases);
}
#endif
