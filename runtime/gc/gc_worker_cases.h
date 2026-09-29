#ifndef KLASSIC_GC_WORKER_CASES_H
#define KLASSIC_GC_WORKER_CASES_H
#include "gc_worker.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*WorkerStep)(RootRegistry *,WorkerRequest *,uint64_t);
typedef struct {RootRegistry roots;WorkerRequest q;HandshakeState h;ThreadRegistration p;MutatorContext ctx;} WorkerFixture;
static const uint64_t worker_pointers[]={offsetof(WorkerFixture,q.handshake),offsetof(WorkerFixture,q.participant),offsetof(WorkerFixture,p.context),offsetof(WorkerFixture,h.head),offsetof(WorkerFixture,p.registry),offsetof(WorkerFixture,p.previous),offsetof(WorkerFixture,p.next)};
static unsigned worker_cases;static FILE *worker_vectors;
static void worker_fixture(WorkerFixture *f,uint64_t phase){
    memset(f,0,sizeof(*f));f->roots.phase=phase;f->q=(WorkerRequest){(uintptr_t)&f->h,(uintptr_t)&f->p};
    f->p.context=(uintptr_t)&f->ctx;f->ctx.abi_version=GC_ABI_VERSION;
}
static uint64_t worker_normalize(WorkerFixture *d,const WorkerFixture *s){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(worker_pointers)/8;i++){unsigned char *p=(unsigned char *)d+worker_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}}
    return mask;
}
static void worker_clone(WorkerFixture *d,const WorkerFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=worker_normalize(d,s);
    for(unsigned i=0;i<sizeof(worker_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+worker_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void worker_effect(const WorkerFixture *before,const WorkerFixture *after,uint64_t op,uint64_t result){
    WorkerFixture e;memcpy(&e,before,sizeof(e));
    if(result==GC_WK_OK){
        if(op==GC_WK_ATTACH){e.ctx.thread_id=++e.h.last_identity;e.p.identity=e.ctx.thread_id;e.p.registered=1;
            e.p.acknowledged_generation=e.h.generation;e.h.members++;
            assert(!e.h.head);e.h.head=(uintptr_t)&after->p;e.p.registry=(uintptr_t)&after->h;atomic_store(&e.ctx.adopted_phase,e.roots.phase);}
        else if(op==GC_WK_ENTER_NATIVE){atomic_store(&e.ctx.state,GC_BLOCKED_NATIVE);e.h.native++;}
        else if(op==GC_WK_LEAVE_NATIVE){atomic_store(&e.ctx.state,GC_RUNNING);e.h.native--;e.p.acknowledged_generation=e.h.generation;atomic_store(&e.ctx.adopted_phase,e.roots.phase);}
        else if(atomic_load(&e.ctx.state)!=GC_DETACHING)atomic_store(&e.ctx.state,GC_RUNNING);
    }else if(result==GC_WK_PARK&&e.p.acknowledged_generation!=e.h.generation){
        if(atomic_load(&e.ctx.state)!=GC_DETACHING)atomic_store(&e.ctx.state,GC_AT_SAFEPOINT);
        e.p.acknowledged_generation=e.h.generation;e.h.acknowledged++;atomic_store(&e.ctx.adopted_phase,e.roots.phase);
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void worker_compare(WorkerFixture *a,WorkerStep step,uint64_t op,uint64_t expected){
    WorkerFixture c,ba,bc,x,y;worker_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->roots,&a->q,op),reference=gc_worker_step_locked(&c.roots,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"worker case %u op %llu got %llu C %llu expected %llu\n",worker_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    worker_effect(&ba,a,op,actual);worker_effect(&bc,&c,op,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(worker_normalize(&x,a)==worker_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(worker_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,worker_normalize(&x,&c),worker_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,worker_vectors)==1);assert(fwrite(&x,sizeof(x),1,worker_vectors)==1);assert(fwrite(&y,sizeof(y),1,worker_vectors)==1);}
    worker_cases++;
}
static void gc_run_worker_cases(WorkerStep step){
    WorkerFixture a;worker_cases=0;const char *path=getenv("KLASSIC_GC_WORKER_VECTORS");
    if(path){worker_vectors=fopen(path,"wb");assert(worker_vectors);assert(fwrite("KLWORK01",8,1,worker_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(WorkerFixture,roots),offsetof(WorkerFixture,q),sizeof(worker_pointers)/8};
        assert(fwrite(header,sizeof(header),1,worker_vectors)==1);assert(fwrite(worker_pointers,sizeof(worker_pointers),1,worker_vectors)==1);}
    for(unsigned epoch=0;epoch<32;epoch++)for(unsigned phase=0;phase<8;phase++){
        worker_fixture(&a,(UINT64_C(123)<<16)|(epoch<<8)|phase|(((phase+3)%8)<<4));
        worker_compare(&a,step,GC_WK_ATTACH,GC_WK_OK);worker_compare(&a,step,GC_WK_POLL,GC_WK_OK);
        worker_compare(&a,step,GC_WK_ENTER_NATIVE,GC_WK_OK);
        a.roots.phase=(a.roots.phase+0x10000)^0x100;
        assert(gc_handshake_step_locked(&a.h,NULL,GC_HS_BEGIN)==GC_HS_OK);
        worker_compare(&a,step,GC_WK_LEAVE_NATIVE,GC_WK_RETRY);
        assert(gc_handshake_step_locked(&a.h,NULL,GC_HS_END)==GC_HS_OK);
        worker_compare(&a,step,GC_WK_LEAVE_NATIVE,GC_WK_OK);
        a.roots.phase+=0x10000;assert(gc_handshake_step_locked(&a.h,NULL,GC_HS_BEGIN)==GC_HS_OK);
        worker_compare(&a,step,GC_WK_POLL,GC_WK_PARK);worker_compare(&a,step,GC_WK_POLL,GC_WK_PARK);
        assert(gc_handshake_step_locked(&a.h,NULL,GC_HS_END)==GC_HS_OK);
        worker_compare(&a,step,GC_WK_POLL,GC_WK_OK);
    }
    for(unsigned fault=0;fault<37;fault++){
        worker_fixture(&a,0x10022);assert(gc_worker_step_locked(&a.roots,&a.q,GC_WK_ATTACH)==GC_WK_OK);
        a.roots.phase=0x20022;assert(gc_handshake_step_locked(&a.h,NULL,GC_HS_BEGIN)==GC_HS_OK);
        uint64_t op=GC_WK_POLL,result=GC_WK_INVALID;
        switch(fault){case 0:op=4;break;case 1:a.q.handshake=0;break;case 2:a.q.handshake++;break;
        case 3:a.q.participant=0;break;case 4:a.q.participant++;break;case 5:a.p.context=0;break;case 6:a.p.context++;break;
        case 7:a.roots.phase|=8;break;case 8:a.roots.phase|=0xe000;break;case 9:a.ctx.abi_version++;break;
        case 10:atomic_store(&a.ctx.access_epoch,1);break;case 11:atomic_store(&a.ctx.adopted_phase,8);break;
        case 12:a.roots.phase=0x22;break;case 13:a.roots.phase=0x10023;break;case 14:op=GC_WK_ENTER_NATIVE;result=GC_WK_RETRY;break;
        case 15:a.h.active=a.h.expected=0;break;case 16:a.ctx.tlab_start=16;result=GC_WK_CLOSE_TLAB;break;
        case 17:a.ctx.tlab_top=32;result=GC_WK_CLOSE_TLAB;break;case 18:a.ctx.tlab_end=48;result=GC_WK_CLOSE_TLAB;break;
        case 19:a.ctx.satb.top=1;result=GC_WK_FLUSH_SATB;break;case 20:a.ctx.remembered.top=1;result=GC_WK_FLUSH_REMEMBERED;break;
        case 21:atomic_store(&a.ctx.satb.published,1);break;case 22:atomic_store(&a.ctx.satb.consumed,1);break;
        case 23:atomic_store(&a.ctx.remembered.published,1);break;case 24:atomic_store(&a.ctx.remembered.consumed,1);break;
        case 25:a.p.identity++;break;case 26:a.p.registered=0;break;case 27:a.p.acknowledged_generation=2;break;
        case 28:a.h.native=2;break;case 29:a.h.active=2;break;case 30:a.h.expected=2;break;case 31:a.h.acknowledged=2;break;
        case 32:atomic_store(&a.ctx.state,GC_BLOCKED_NATIVE);break;case 33:atomic_store(&a.ctx.state,GC_DETACHED);break;
        case 34:a.ctx.thread_id=0;break;case 35:a.h.generation=0;break;case 36:a.p.acknowledged_generation=1;break;}
        worker_compare(&a,step,op,result);
    }
    for(unsigned fault=0;fault<12;fault++){
        worker_fixture(&a,0x10022);uint64_t expected=GC_WK_INVALID;
        switch(fault){case 0:atomic_store(&a.ctx.adopted_phase,1);break;case 1:a.ctx.root_segments=8;break;
        case 2:a.ctx.retiring_roots=1;break;case 3:a.ctx.tlab_start=16;break;case 4:a.h.last_identity=UINT64_MAX;expected=GC_WK_EXHAUSTED;break;
        case 5:assert(gc_handshake_step_locked(&a.h,NULL,GC_HS_BEGIN)==GC_HS_OK);expected=GC_WK_RETRY;break;
        case 6:a.ctx.thread_id=1;break;case 7:atomic_store(&a.ctx.state,GC_BLOCKED_NATIVE);break;
        case 8:a.ctx.satb_consumer.serial=1;break;case 9:a.ctx.satb_consumer.end=1;break;
        case 10:a.ctx.remembered_consumer.serial=1;break;case 11:a.ctx.remembered_consumer.end=1;break;}
        worker_compare(&a,step,GC_WK_ATTACH,expected);
    }
    worker_fixture(&a,0);worker_compare(&a,step,GC_WK_ATTACH,GC_WK_OK);
    a.ctx.satb.top=1;atomic_store(&a.ctx.satb.published,1); // unconsumed is a holder, not an adoption blocker
    assert(gc_handshake_step_locked(&a.h,NULL,GC_HS_BEGIN)==GC_HS_OK);
    worker_compare(&a,step,GC_WK_ENTER_NATIVE,GC_WK_RETRY);
    worker_compare(&a,step,GC_WK_POLL,GC_WK_PARK);
    a.roots.phase=0x10000;worker_compare(&a,step,GC_WK_POLL,GC_WK_INVALID); // changed snapshot after ACK
    if(worker_vectors){assert(!fclose(worker_vectors));worker_vectors=NULL;}
    printf("worker rendezvous %u C/candidate registration, phase adoption and preservation cases PASS\n",worker_cases);
}
#endif
