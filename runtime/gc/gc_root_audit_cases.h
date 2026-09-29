#ifndef KLASSIC_GC_ROOT_AUDIT_CASES_H
#define KLASSIC_GC_ROOT_AUDIT_CASES_H
#include "gc_root_audit.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RootAuditor)(RootRegistry *,RootAudit *,uint64_t);
typedef struct {
    RootRegistry g;MutatorContext ctx[2];RootRegistration r[3];RootSegment s[3];RootCell cells[3];
    BufferCursor buffers[3];RootChange changes[3];RootAudit a;MarkQueue queue;
} AuditFixture;
#define AUDIT_PTR(i) offsetof(AuditFixture,r[i].segment),offsetof(AuditFixture,r[i].inventory_previous),offsetof(AuditFixture,r[i].inventory_next),offsetof(AuditFixture,r[i].context),offsetof(AuditFixture,r[i].owner_previous),offsetof(AuditFixture,r[i].owner_next),offsetof(AuditFixture,s[i].cells),offsetof(AuditFixture,s[i].journal),offsetof(AuditFixture,s[i].registration),offsetof(AuditFixture,buffers[i].entries)
static const uint64_t audit_pointers[]={AUDIT_PTR(0),AUDIT_PTR(1),AUDIT_PTR(2),offsetof(AuditFixture,ctx[0].root_segments),offsetof(AuditFixture,ctx[1].root_segments),offsetof(AuditFixture,g.inventory),offsetof(AuditFixture,g.head),offsetof(AuditFixture,a.context),offsetof(AuditFixture,a.queue),offsetof(AuditFixture,a.walk.registry),offsetof(AuditFixture,a.walk.lease.registration)};
#undef AUDIT_PTR
static unsigned audit_cases;static FILE *audit_vectors;
static void audit_fixture(AuditFixture *f,uint64_t phase,uint64_t generation,unsigned count){
    memset(f,0,sizeof(*f));f->g.phase=phase;f->g.last_lease=17;f->queue.epoch=61;f->queue.generation=generation;
    f->a.queue=(uintptr_t)&f->queue;f->a.context=(uintptr_t)&f->ctx[0];
    for(unsigned i=0;i<2;i++){f->ctx[i].abi_version=GC_ABI_VERSION;f->ctx[i].thread_id=17+i;atomic_store(&f->ctx[i].adopted_phase,phase);atomic_store(&f->ctx[i].root_revision,7+i*4);}
    for(unsigned i=0;i<count;i++){
        unsigned owner=i%2;f->s[i]=(RootSegment){.owner=17+owner,.cells=(uintptr_t)&f->cells[i],.capacity=1,.journal=(uintptr_t)&f->buffers[i]};
        f->buffers[i]=(BufferCursor){.entries=(uintptr_t)&f->changes[i],.capacity=1};f->r[i].segment=(uintptr_t)&f->s[i];
        RootLifeRequest q={.registration=(uintptr_t)&f->r[i],.context=(uintptr_t)&f->ctx[owner]};assert(gc_root_life_locked(&f->g,&q,GC_RL_ATTACH)==0);
        // Quiescent prepared baseline, preceding this audit's discovery.
        f->cells[i].active=1;f->cells[i].reuse_generation=1;atomic_store(&f->cells[i].value,((uint64_t)(i+1)<<20)|0x1510);atomic_store(&f->s[i].used,1);
    }
}
static uint64_t audit_normalize(AuditFixture *d,const AuditFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<sizeof(audit_pointers)/8;i++){unsigned char *p=(unsigned char *)d+audit_pointers[i];uint64_t v;memcpy(&v,p,8);if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}}return mask;}
static void audit_clone(AuditFixture *d,const AuditFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=audit_normalize(d,s);
    for(unsigned i=0;i<sizeof(audit_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+audit_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void audit_effect(const AuditFixture *before,const AuditFixture *after,uint64_t op,uint64_t result){
    AuditFixture e;memcpy(&e,before,sizeof(e));
    if(op<2&&(result==GC_RA_ITEM||result==GC_RA_DONE)){
        RootWalk *w=&e.a.walk;uint64_t next;uintptr_t base=before->a.queue - offsetof(AuditFixture,queue);
        if(op==GC_RA_BEGIN){e.a.serial=++e.g.last_lease;e.a.last_scope=e.g.last_scope;e.a.phase=e.g.phase;e.a.epoch=e.queue.epoch;e.a.generation=e.queue.generation;
            w->registry=base+offsetof(AuditFixture,g);w->phase=e.g.phase;w->mask=UINT64_C(1)<<e.queue.generation;next=e.g.inventory;
            if(next){e.g.leases++;e.g.walkers++;}
        }else{size_t previous=(before->a.walk.lease.registration-(base+offsetof(AuditFixture,r)))/sizeof(RootRegistration);assert(previous<3);
            next=e.r[previous].inventory_next;e.r[previous].leases--;if(!next){e.g.leases--;e.g.walkers--;}}
        w->active=!!next;w->cursor=w->completed_mask=0;w->limit=0;e.a.state=next?1:2;
        if(next){size_t index=(next-(base+offsetof(AuditFixture,r)))/sizeof(RootRegistration);assert(index<3);RootRegistration *r=&e.r[index];r->leases++;
            w->limit=atomic_load(&e.s[index].used);w->lease=(RootLease){next,r->scope,++e.g.last_lease,1,0};
            size_t owner=(r->context-(base+offsetof(AuditFixture,ctx)))/sizeof(MutatorContext);assert(owner<2);MutatorContext *ctx=&e.ctx[owner];uint64_t gen=e.a.generation;
            if(ctx->root_audit_serial[gen]!=e.a.serial){ctx->root_audit_serial[gen]=e.a.serial;ctx->root_audit_identity[gen]=ctx->thread_id;ctx->root_audit_revision[gen]=atomic_load(&ctx->root_revision);}
        }else w->lease.kind=0;
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void audit_compare_mode(AuditFixture *a,RootAuditor step,RootAuditor reference_step,uint64_t op,uint64_t expected){
    AuditFixture c,ba,bc,x,y;audit_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->g,&a->a,op),reference=reference_step(&c.g,&c.a,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"root audit case %u op %llu got %llu C %llu expected %llu\n",audit_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    audit_effect(&ba,a,op,actual);audit_effect(&bc,&c,op,reference);memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(audit_normalize(&x,a)==audit_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(audit_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,audit_normalize(&x,&c),audit_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,audit_vectors)==1);assert(fwrite(&x,sizeof(x),1,audit_vectors)==1);assert(fwrite(&y,sizeof(y),1,audit_vectors)==1);}audit_cases++;
}
static void audit_compare(AuditFixture *a,RootAuditor step,uint64_t op,uint64_t expected){
    audit_compare_mode(a,step,gc_root_audit_step_locked,op,expected);
}
static void retire_audit_compare(AuditFixture *a,RootAuditor step,uint64_t op,uint64_t expected){
    audit_compare_mode(a,step,gc_root_retire_audit_step_locked,op,expected);
}
static void audit_scan_item(AuditFixture *f){uint64_t value=0;RootWalkScan scan={(uintptr_t)&f->a.walk,f->a.walk.lease.serial,(uintptr_t)&value,1,0};
    assert(gc_root_walk_scan(&scan)==0&&scan.written==1);assert(value>=UINT64_C(0x101510)&&value<=UINT64_C(0x301510));
    f->a.walk.completed_mask=f->a.walk.mask;}
static void audit_finish_fixture_mode(AuditFixture *f,RootAuditor step){uint64_t status=step(&f->g,&f->a,GC_RA_BEGIN);while(status==GC_RA_ITEM){audit_scan_item(f);status=step(&f->g,&f->a,GC_RA_NEXT);}assert(status==GC_RA_DONE);atomic_store(&f->ctx[0].state,GC_AT_SAFEPOINT);atomic_store(&f->ctx[1].state,GC_BLOCKED_NATIVE);}
static void audit_finish_fixture(AuditFixture *f){audit_finish_fixture_mode(f,gc_root_audit_step_locked);}
static void gc_run_root_audit_cases(RootAuditor step){
    AuditFixture a;audit_cases=0;const char *path=getenv("KLASSIC_GC_ROOT_AUDIT_VECTORS");
    if(path){audit_vectors=fopen(path,"wb");assert(audit_vectors);assert(fwrite("KLRAUD01",8,1,audit_vectors)==1);uint64_t header[]={sizeof(a),offsetof(AuditFixture,g),offsetof(AuditFixture,a),sizeof(audit_pointers)/8};assert(fwrite(header,sizeof(header),1,audit_vectors)==1);assert(fwrite(audit_pointers,sizeof(audit_pointers),1,audit_vectors)==1);}
    const uint64_t epochs[]={0,1,2,31};
    for(unsigned gen=0;gen<2;gen++)for(unsigned epoch=0;epoch<4;epoch++)for(unsigned y=1;y<=3;y++)for(unsigned old=1;old<=3;old++){
        audit_fixture(&a,0x90000|(epochs[epoch]<<8)|y|(old<<4),gen,3);audit_compare(&a,step,GC_RA_BEGIN,GC_RA_ITEM);
        for(unsigned node=0;node<3;node++){audit_scan_item(&a);audit_compare(&a,step,GC_RA_NEXT,node==2?GC_RA_DONE:GC_RA_ITEM);}
        audit_compare(&a,step,GC_RA_CHECK,GC_RA_DONE);
        for(unsigned owner=0;owner<2;owner++){atomic_store(&a.ctx[owner].state,owner?GC_BLOCKED_NATIVE:GC_AT_SAFEPOINT);a.a.context=(uintptr_t)&a.ctx[owner];audit_compare(&a,step,GC_RA_OWNER,GC_RA_DONE);}
    }
    for(unsigned fault=0;fault<26;fault++){
        audit_fixture(&a,0x22,0,3);uint64_t op=GC_RA_BEGIN,result=GC_RA_INVALID;
        switch(fault){case 0:op=4;break;case 1:a.a.queue=0;break;case 2:a.a.queue++;break;case 3:a.a.state=1;break;
        case 4:a.a.walk.active=1;break;case 5:a.a.walk.lease.kind=1;break;case 6:a.g.phase=0x28;break;case 7:a.g.phase=0xe022;break;
        case 8:a.queue.epoch=0;break;case 9:a.queue.generation=2;break;case 10:a.g.phase=0x20;break;case 11:a.g.phase=0x24;break;
        case 12:a.g.last_lease=UINT64_MAX;result=GC_RA_EXHAUSTED;break;case 13:a.g.last_lease=UINT64_MAX-1;result=GC_RA_EXHAUSTED;break;
        case 14:a.r[2].context=0;break;case 15:a.r[2].context++;break;case 16:a.r[2].segment=0;break;case 17:a.r[2].segment++;break;
        case 18:a.ctx[0].abi_version++;break;case 19:a.ctx[0].thread_id=0;break;case 20:a.s[2].owner++;break;case 21:a.s[2].cells=0;break;
        case 22:a.g.leases=UINT64_MAX;result=GC_RA_EXHAUSTED;break;case 23:a.g.walkers=UINT64_MAX;result=GC_RA_EXHAUSTED;break;
        case 24:a.r[2].registered=0;break;case 25:a.g.inventory++;break;}
        audit_compare(&a,step,op,result);
    }
    for(unsigned fault=0;fault<14;fault++){
        audit_fixture(&a,0x22,0,3);assert(gc_root_audit_step_locked(&a.g,&a.a,GC_RA_BEGIN)==0);audit_scan_item(&a);uint64_t result=GC_RA_INVALID;
        switch(fault){case 0:a.a.state=0;break;case 1:a.a.serial=0;break;case 2:a.a.generation=2;break;case 3:a.a.walk.mask=3;break;
        case 4:a.a.walk.lease.scope++;break;case 5:a.s[2].journal++;break;case 6:atomic_store(&a.buffers[2].consumed,1);break;
        case 7:atomic_store(&a.buffers[2].published,1);result=GC_RA_WAIT;break;case 8:a.r[2].journal_serial=1;result=GC_RA_WAIT;break;
        case 9:a.r[1].context=0;break;case 10:a.ctx[1].abi_version++;break;case 11:a.a.walk.cursor=0;result=GC_RA_WAIT;break;
        case 12:a.a.walk.completed_mask=0;result=GC_RA_WAIT;break;case 13:a.r[1].scope=0;break;}
        audit_compare(&a,step,GC_RA_NEXT,result);
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned fault=0;fault<25;fault++){
        audit_fixture(&a,0x22,gen,3);audit_finish_fixture(&a);uint64_t result=GC_RA_INVALID;
        switch(fault){case 0:a.a.context=0;break;case 1:a.a.context++;break;case 2:atomic_store(&a.ctx[0].state,GC_RUNNING);break;
        case 3:atomic_store(&a.ctx[0].state,GC_DETACHED);break;case 4:a.ctx[0].abi_version++;break;case 5:a.ctx[0].thread_id=0;break;
        case 6:atomic_store(&a.ctx[0].access_epoch,7);break;case 7:a.ctx[0].retiring_roots=1;result=GC_RA_WAIT;break;
        case 8:a.ctx[0].root_audit_serial[gen]++;result=GC_RA_STALE;break;case 9:a.ctx[0].root_audit_identity[gen]++;result=GC_RA_STALE;break;
        case 10:atomic_fetch_add(&a.ctx[0].root_revision,1);result=GC_RA_STALE;break;case 11:a.a.generation=2;break;case 12:a.a.epoch=0;break;
        case 13:a.a.serial=0;break;case 14:a.g.phase+=0x10000;result=GC_RA_DONE;break;
        case 15:a.g.phase=gen?0x32:0x23;result=GC_RA_DONE;break;case 16:a.g.phase=gen?0x20:2;result=GC_RA_DONE;break;
        case 17:a.g.phase^=UINT64_C(1)<<(8+gen);result=GC_RA_STALE;break;case 18:a.queue.epoch+=2;result=GC_RA_STALE;break;
        case 19:a.queue.generation^=1;result=GC_RA_STALE;break;case 20:a.g.last_scope++;result=GC_RA_STALE;break;
        case 21:a.g.phase|=0x2000;break;case 22:a.a.walk.mask=0;break;case 23:a.a.walk.phase++;break;
        case 24:a.g.phase=gen?0x42:0x24;result=GC_RA_STALE;break;}
        audit_compare(&a,step,GC_RA_OWNER,result);
    }
    audit_fixture(&a,0x22,0,0);audit_compare(&a,step,GC_RA_BEGIN,GC_RA_DONE);audit_compare(&a,step,GC_RA_CHECK,GC_RA_DONE);
    atomic_store(&a.ctx[0].state,GC_AT_SAFEPOINT);audit_compare(&a,step,GC_RA_OWNER,GC_RA_DONE);
    // Same owner revisited after an update: never overwrite its first sample.
    audit_fixture(&a,0x22,0,3);audit_compare(&a,step,GC_RA_BEGIN,GC_RA_ITEM);uint64_t revision=a.ctx[0].root_audit_revision[0];
    atomic_fetch_add(&a.ctx[0].root_revision,1);
    for(unsigned node=0;node<3;node++){audit_scan_item(&a);audit_compare(&a,step,GC_RA_NEXT,node==2?GC_RA_DONE:GC_RA_ITEM);}
    assert(a.ctx[0].root_audit_revision[0]==revision);atomic_store(&a.ctx[0].state,GC_AT_SAFEPOINT);audit_compare(&a,step,GC_RA_OWNER,GC_RA_STALE);
    // A second generation's audit preserves the first generation certificate.
    audit_fixture(&a,0x22,0,3);audit_finish_fixture(&a);uint64_t serial=a.ctx[0].root_audit_serial[0];a.queue.generation=1;
    audit_compare(&a,step,GC_RA_BEGIN,GC_RA_ITEM);for(unsigned node=0;node<3;node++){audit_scan_item(&a);audit_compare(&a,step,GC_RA_NEXT,node==2?GC_RA_DONE:GC_RA_ITEM);}
    assert(a.ctx[0].root_audit_serial[0]==serial&&a.ctx[0].root_audit_serial[1]!=serial);
    audit_fixture(&a,0x22,0,0);a.g.last_lease=UINT64_MAX-1;
    audit_compare(&a,step,GC_RA_BEGIN,GC_RA_DONE);audit_compare(&a,step,GC_RA_CHECK,GC_RA_DONE);
    audit_compare(&a,step,GC_RA_BEGIN,GC_RA_EXHAUSTED);
    audit_fixture(&a,0x22,0,1);a.g.last_lease=UINT64_MAX-2;
    audit_compare(&a,step,GC_RA_BEGIN,GC_RA_ITEM);audit_scan_item(&a);audit_compare(&a,step,GC_RA_NEXT,GC_RA_DONE);
    audit_compare(&a,step,GC_RA_CHECK,GC_RA_DONE);audit_compare(&a,step,GC_RA_BEGIN,GC_RA_EXHAUSTED);
    for(unsigned field=0;field<4;field++)for(unsigned op=0;op<2;op++){
        audit_fixture(&a,0x22,0,3);
        if(op){assert(gc_root_audit_step_locked(&a.g,&a.a,GC_RA_BEGIN)==0);audit_scan_item(&a);}
        if(field==0)a.a.walk.pending=1;if(field==1)a.a.walk.pending_reference=0x101510;
        if(field==2)a.a.walk.pending_observed=0x101510;if(field==3)a.a.walk.pending_index=1;
        audit_compare(&a,step,op,op?GC_RA_WAIT:GC_RA_INVALID);
    }
    if(audit_vectors){assert(!fclose(audit_vectors));audit_vectors=NULL;}printf("root audit %u C/candidate inventory, cycle and owner certificate cases PASS\n",audit_cases);
}
static void gc_run_root_retire_audit_cases(RootAuditor step){
    AuditFixture a;audit_cases=0;
    for(unsigned gen=0;gen<2;gen++)for(unsigned colors=0;colors<32;colors++)for(unsigned other=0;other<8;other++){
        uint64_t phase=0x90000|(colors<<8)|(gen?(7<<4)|other:7|(other<<4));
        audit_fixture(&a,phase,gen,3);retire_audit_compare(&a,step,GC_RA_BEGIN,GC_RA_ITEM);
        for(unsigned node=0;node<3;node++){audit_scan_item(&a);retire_audit_compare(&a,step,GC_RA_NEXT,node==2?GC_RA_DONE:GC_RA_ITEM);}
        retire_audit_compare(&a,step,GC_RA_CHECK,GC_RA_DONE);
        for(unsigned owner=0;owner<2;owner++){
            atomic_store(&a.ctx[owner].state,owner?GC_BLOCKED_NATIVE:GC_AT_SAFEPOINT);
            a.a.context=(uintptr_t)&a.ctx[owner];retire_audit_compare(&a,step,GC_RA_OWNER,GC_RA_DONE);
        }
        // Retire is strict about both lanes, all colors and the snapshot serial.
        for(unsigned bit=0;bit<32;bit++){
            a.g.phase=phase^(UINT64_C(1)<<bit);
            retire_audit_compare(&a,step,GC_RA_CHECK,gc_abi_valid_phase((PhaseSnapshot){a.g.phase})?GC_RA_STALE:GC_RA_INVALID);
        }
        a.g.phase=phase;
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned phase=0;phase<7;phase++){
        audit_fixture(&a,phase<<(4*gen),gen,0);retire_audit_compare(&a,step,GC_RA_BEGIN,GC_RA_INVALID);
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned fault=0;fault<9;fault++){
        audit_fixture(&a,7<<(4*gen),gen,3);retire_audit_compare(&a,step,GC_RA_BEGIN,GC_RA_ITEM);
        // Mutation before a repeated owner must not replace its first sample.
        if(fault==0)atomic_fetch_add(&a.ctx[0].root_revision,1);
        for(unsigned node=0;node<3;node++){audit_scan_item(&a);retire_audit_compare(&a,step,GC_RA_NEXT,node==2?GC_RA_DONE:GC_RA_ITEM);}
        atomic_store(&a.ctx[0].state,GC_AT_SAFEPOINT);uint64_t result=GC_RA_STALE;
        switch(fault){case 0:break;case 1:a.g.last_scope++;break;case 2:a.queue.epoch++;break;
        case 3:a.queue.generation^=1;break;case 4:a.ctx[0].root_audit_identity[gen]++;break;
        case 5:a.ctx[0].retiring_roots=1;result=GC_RA_WAIT;break;
        case 6:atomic_store(&a.ctx[0].state,GC_RUNNING);result=GC_RA_INVALID;break;
        case 7:atomic_store(&a.ctx[0].access_epoch,1);result=GC_RA_INVALID;break;
        case 8:a.ctx[0].root_audit_serial[gen]++;break;
        }
        retire_audit_compare(&a,step,GC_RA_OWNER,result);
    }
    for(unsigned fault=0;fault<6;fault++){
        audit_fixture(&a,7,0,1);retire_audit_compare(&a,step,GC_RA_BEGIN,GC_RA_ITEM);audit_scan_item(&a);
        switch(fault){case 0:atomic_store(&a.buffers[0].published,1);break;case 1:a.r[0].journal_serial=1;break;
        case 2:a.a.walk.pending=1;break;case 3:a.a.walk.pending_reference=0x101510;break;
        case 4:a.a.walk.cursor=0;break;case 5:a.a.walk.completed_mask=0;break;}
        retire_audit_compare(&a,step,GC_RA_NEXT,GC_RA_WAIT);
    }
    audit_fixture(&a,7,0,0);a.g.last_lease=UINT64_MAX-1;
    retire_audit_compare(&a,step,GC_RA_BEGIN,GC_RA_DONE);retire_audit_compare(&a,step,GC_RA_CHECK,GC_RA_DONE);
    retire_audit_compare(&a,step,GC_RA_BEGIN,GC_RA_EXHAUSTED);
    for(unsigned gen=0;gen<2;gen++)for(unsigned nodes=0;nodes<2;nodes++)for(unsigned fault=0;fault<17;fault++){
        audit_fixture(&a,7<<(4*gen),gen,nodes);
        retire_audit_compare(&a,step,GC_RA_BEGIN,nodes?GC_RA_ITEM:GC_RA_DONE);
        if(nodes){audit_scan_item(&a);retire_audit_compare(&a,step,GC_RA_NEXT,GC_RA_DONE);}
        atomic_store(&a.ctx[0].state,GC_AT_SAFEPOINT);uint64_t result=GC_RA_WAIT;
        if(fault==0)atomic_store(&a.ctx[0].private_producers,1);
        else if(fault<=6){
            BufferConsumer *c=fault<=3?&a.ctx[0].satb_consumer:&a.ctx[0].remembered_consumer;
            if((fault-1)%3==0)c->serial=1;else if((fault-1)%3==1)c->end=1;else c->cursor=1;
        }else if(fault<=14){
            BufferCursor *b=fault<=10?&a.ctx[0].satb:&a.ctx[0].remembered;
            unsigned kind=(fault-7)%4;
            if(kind==0)b->top=1; // unpublished producer work
            else if(kind==1){b->top=1;atomic_store(&b->published,1);} // published work
            else if(kind==2){atomic_store(&b->consumed,1);result=GC_RA_INVALID;}
            else{atomic_store(&b->published,1);result=GC_RA_INVALID;}
        }else{
            // Monotonic counters and historical claim serials need not be zero.
            BufferCursor *b=fault==15?&a.ctx[0].satb:&a.ctx[0].remembered;
            b->top=UINT64_MAX;atomic_store(&b->published,UINT64_MAX);atomic_store(&b->consumed,UINT64_MAX);
            a.ctx[0].satb_consumer.last_serial=17;a.ctx[0].remembered_consumer.last_serial=19;
            result=GC_RA_DONE;
        }
        retire_audit_compare(&a,step,GC_RA_OWNER,result);
    }
    printf("retire root audit %u C/candidate snapshot, revision and journal cases PASS\n",audit_cases);
}
#endif
