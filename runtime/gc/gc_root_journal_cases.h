#ifndef KLASSIC_GC_ROOT_JOURNAL_CASES_H
#define KLASSIC_GC_ROOT_JOURNAL_CASES_H
#include "gc_root_journal.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RootJournalStep)(RootRegistry *,RootJournalRequest *,uint64_t);
typedef struct {
    RootRegistry g;RootRegistration r;RootSegment s;RootLease lease,other;
    RootJournalRequest q;BufferCursor b;RootChange entries[300],output[257];
} JournalFixture;
static const uint64_t journal_pointers[]={offsetof(JournalFixture,r.segment),offsetof(JournalFixture,s.registration),
    offsetof(JournalFixture,s.journal),offsetof(JournalFixture,q.registration),offsetof(JournalFixture,q.lease),
    offsetof(JournalFixture,q.output),offsetof(JournalFixture,b.entries),offsetof(JournalFixture,lease.registration),offsetof(JournalFixture,other.registration)};
static unsigned journal_cases;static FILE *journal_vectors;
static void journal_fixture(JournalFixture *f,uint64_t cap,uint64_t start,uint64_t count,uint64_t budget,uint64_t state){
    memset(f,0,sizeof(*f));f->g.last_lease=27;f->g.active=state==1;f->g.retiring=state==2;
    f->r=(RootRegistration){.segment=(uintptr_t)&f->s,.scope=31,.registered=1};
    f->s=(RootSegment){.owner=17,.scope=31,.journal=(uintptr_t)&f->b,.registration=(uintptr_t)&f->r};atomic_store(&f->s.state,state);
    f->b=(BufferCursor){.entries=(uintptr_t)f->entries,.capacity=cap,.top=UINT64_MAX};
    atomic_store(&f->b.consumed,start);atomic_store(&f->b.published,start+count);
    f->q=(RootJournalRequest){.registration=(uintptr_t)&f->r,.lease=(uintptr_t)&f->lease,.scope=31,.output=(uintptr_t)f->output,.capacity=budget};
    for(uint64_t i=0;i<count;i++)f->entries[(start+i)%cap]=(RootChange){i*16,i*16+16,31,(i%32)<<8|0x22};
    memset(f->output,0xab,sizeof(f->output));
}
static uint64_t journal_normalize(JournalFixture *d,const JournalFixture *s){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(journal_pointers)/8;i++){unsigned char *p=(unsigned char *)d+journal_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}}return mask;
}
static void journal_clone(JournalFixture *d,const JournalFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=journal_normalize(d,s);
    for(unsigned i=0;i<sizeof(journal_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+journal_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void journal_effect(const JournalFixture *before,const JournalFixture *after,uint64_t op,uint64_t result){
    JournalFixture e;memcpy(&e,before,sizeof(e));
    if(result==GC_RJ_OK){
        if(op==GC_RJ_CLAIM){uint64_t begin=atomic_load(&e.b.consumed),n=atomic_load(&e.b.published)-begin;if(n>e.q.capacity)n=e.q.capacity;
            e.lease=(RootLease){before->q.registration,e.r.scope,++e.g.last_lease,4,begin};
            e.g.leases++;e.r.leases++;e.r.journal_serial=e.lease.serial;e.r.journal_end=begin+n;
            e.q.serial=e.lease.serial;e.q.begin=begin;e.q.end=begin+n;e.q.written=0;
        }else if(op==GC_RJ_COPY){uint64_t n=e.q.end-e.q.begin;
            for(uint64_t i=0;i<n;i++)e.output[i]=e.entries[(e.q.begin+i)%e.b.capacity];
            e.q.written=n;e.lease.cursor=e.q.end;
        }else{atomic_store(&e.b.consumed,e.q.end);e.r.journal_serial=e.r.journal_end=0;e.g.leases--;e.r.leases--;e.lease.kind=0;}
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void journal_compare(JournalFixture *a,RootJournalStep step,uint64_t op,uint64_t expected){
    JournalFixture c,ba,bc,x,y;journal_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->g,&a->q,op),reference=gc_root_journal_step(&c.g,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"journal case %u op %llu got %llu C %llu expected %llu\n",journal_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    journal_effect(&ba,a,op,actual);journal_effect(&bc,&c,op,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(journal_normalize(&x,a)==journal_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(journal_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,journal_normalize(&x,&c),journal_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,journal_vectors)==1);assert(fwrite(&x,sizeof(x),1,journal_vectors)==1);assert(fwrite(&y,sizeof(y),1,journal_vectors)==1);}
    journal_cases++;
}
static void gc_run_root_journal_cases(RootJournalStep step){
    JournalFixture a;journal_cases=0;const char *path=getenv("KLASSIC_GC_ROOT_JOURNAL_VECTORS");
    if(path){journal_vectors=fopen(path,"wb");assert(journal_vectors);assert(fwrite("KLRJNL01",8,1,journal_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(JournalFixture,g),offsetof(JournalFixture,q),sizeof(journal_pointers)/8};
        assert(fwrite(header,sizeof(header),1,journal_vectors)==1);assert(fwrite(journal_pointers,sizeof(journal_pointers),1,journal_vectors)==1);}
    const uint64_t caps[]={1,3,7,300},budgets[]={1,7,256};
    for(unsigned c=0;c<4;c++)for(unsigned st=0;st<3;st++)for(unsigned n=0;n<2;n++)for(unsigned b=0;b<3;b++)for(unsigned state=1;state<=2;state++){
        uint64_t cap=caps[c],start=st==0?0:st==1?1:UINT64_MAX-cap;
        journal_fixture(&a,cap,start,n?cap:1,budgets[b],state);
        journal_compare(&a,step,GC_RJ_CLAIM,GC_RJ_OK);
        journal_compare(&a,step,GC_RJ_ACK,GC_RJ_WAIT);
        journal_compare(&a,step,GC_RJ_COPY,GC_RJ_OK);
        assert(atomic_load(&a.b.consumed)==start&&a.g.leases==1&&a.r.leases==1);
        journal_compare(&a,step,GC_RJ_COPY,GC_RJ_INVALID);
        journal_compare(&a,step,GC_RJ_ACK,GC_RJ_OK);
        journal_compare(&a,step,GC_RJ_ACK,GC_RJ_INVALID);
    }
    for(unsigned fault=0;fault<29;fault++){
        journal_fixture(&a,7,0,3,2,1);uint64_t result=GC_RJ_INVALID,op=GC_RJ_CLAIM;
        switch(fault){case 0:op=3;break;case 1:a.q.registration=0;break;case 2:a.q.registration++;break;
        case 3:a.q.lease=0;break;case 4:a.q.lease++;break;case 5:a.r.registered=0;break;case 6:a.r.scope=0;break;
        case 7:a.q.scope++;break;case 8:a.r.segment=0;break;case 9:a.r.segment++;break;case 10:a.s.registration=0;break;
        case 11:a.s.scope++;break;case 12:atomic_store(&a.s.state,3);break;case 13:a.s.journal=0;break;case 14:a.s.journal++;break;
        case 15:a.b.entries=0;break;case 16:a.b.entries++;break;case 17:a.b.capacity=0;break;case 18:a.b.capacity=UINT64_C(1)<<32;break;
        case 19:a.b.entries=UINT64_MAX-7;break;case 20:atomic_store(&a.b.consumed,4);break;case 21:atomic_store(&a.b.published,8);break;
        case 22:a.lease.kind=1;break;case 23:a.r.leases=1;break;case 24:a.r.journal_end=1;break;
        case 25:a.r.journal_serial=1;break;case 26:a.q.capacity=0;break;case 27:a.q.capacity=257;break;
        case 28:a.g.last_lease=UINT64_MAX;result=GC_RJ_EXHAUSTED;break;}
        journal_compare(&a,step,op,result);
    }
    for(unsigned fault=0;fault<23;fault++){
        journal_fixture(&a,7,0,3,2,1);assert(gc_root_journal_step(&a.g,&a.q,GC_RJ_CLAIM)==GC_RJ_OK);
        uint64_t op=GC_RJ_COPY,result=GC_RJ_INVALID;
        switch(fault){case 0:a.lease.kind=1;break;case 1:a.q.serial=0;break;case 2:a.q.serial++;break;case 3:a.lease.registration=0;break;
        case 4:a.lease.scope++;break;case 5:a.r.journal_serial++;break;case 6:a.q.begin++;break;case 7:a.q.end++;break;
        case 8:a.r.journal_end=a.q.end=0;break;case 9:a.r.journal_end=a.q.end=4;break;case 10:a.lease.cursor++;break;
        case 11:a.q.written=1;break;case 12:a.q.output=0;break;case 13:a.q.output++;break;case 14:a.q.capacity=1;break;
        case 15:a.q.capacity=257;break;case 16:a.q.output=UINT64_MAX-7;break;
        case 17:op=GC_RJ_ACK;a.r.leases=0;break;case 18:op=GC_RJ_ACK;a.g.leases=0;break;
        case 19:op=GC_RJ_ACK;a.lease.cursor=a.q.end;result=GC_RJ_WAIT;break;
        case 20:a.q.lease=(uintptr_t)&a.other;op=GC_RJ_CLAIM;result=GC_RJ_WAIT;break;
        case 21:atomic_store(&a.s.state,0);break;case 22:atomic_store(&a.s.state,4);break;}
        journal_compare(&a,step,op,result);
    }
    journal_fixture(&a,7,UINT64_MAX,0,2,1);journal_compare(&a,step,GC_RJ_CLAIM,GC_RJ_EMPTY);
    journal_fixture(&a,7,0,3,2,1);a.g.leases=UINT64_MAX;journal_compare(&a,step,GC_RJ_CLAIM,GC_RJ_EXHAUSTED);
    journal_fixture(&a,7,0,3,2,1);a.g.leases=a.r.leases=UINT64_MAX;journal_compare(&a,step,GC_RJ_CLAIM,GC_RJ_EXHAUSTED);
    // A producer may extend the published suffix during a claim. ACK consumes
    // only the captured prefix; the later entry remains pending.
    journal_fixture(&a,7,0,2,2,1);journal_compare(&a,step,GC_RJ_CLAIM,GC_RJ_OK);
    a.entries[2]=(RootChange){17,18,31,0x22};atomic_store(&a.b.published,3);
    journal_compare(&a,step,GC_RJ_COPY,GC_RJ_OK);journal_compare(&a,step,GC_RJ_ACK,GC_RJ_OK);
    assert(atomic_load(&a.b.consumed)==2&&atomic_load(&a.b.published)==3);
    journal_fixture(&a,300,0,300,256,1);assert(gc_root_journal_step(&a.g,&a.q,GC_RJ_CLAIM)==GC_RJ_OK);
    a.q.end=a.r.journal_end=257;journal_compare(&a,step,GC_RJ_COPY,GC_RJ_INVALID);
    journal_fixture(&a,7,0,3,1,1);journal_compare(&a,step,GC_RJ_CLAIM,GC_RJ_OK);
    journal_compare(&a,step,GC_RJ_COPY,GC_RJ_OK);RootJournalRequest stale=a.q;
    journal_compare(&a,step,GC_RJ_ACK,GC_RJ_OK);journal_compare(&a,step,GC_RJ_CLAIM,GC_RJ_OK);
    RootJournalRequest current=a.q;a.q=stale;journal_compare(&a,step,GC_RJ_ACK,GC_RJ_INVALID);a.q=current;
    journal_compare(&a,step,GC_RJ_COPY,GC_RJ_OK);journal_compare(&a,step,GC_RJ_ACK,GC_RJ_OK);
    if(journal_vectors){assert(!fclose(journal_vectors));journal_vectors=NULL;}
    printf("root journal %u C/candidate claim, copy, handoff and preservation cases PASS\n",journal_cases);
}
#endif
