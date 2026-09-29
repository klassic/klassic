#ifndef KLASSIC_GC_MARK_END_CASES_H
#define KLASSIC_GC_MARK_END_CASES_H
#include "gc_root_audit_cases.h"
#include "gc_mark_end.h"
typedef uint64_t (*MarkEndChecker)(RootRegistry *,MarkEndRequest *,uint64_t);
typedef struct {AuditFixture f;HandshakeState h;ThreadRegistration p[2];MarkEndRequest q;MarkTask task;RememberedInventory remembered;RememberedRegistration remembered_node;RememberedSet remembered_set;uint64_t remembered_bits[2];RememberedJob remembered_job;} EndFixture;
static uint64_t end_pointers[64];static unsigned end_pointer_count,end_cases;static FILE *end_vectors;
static void end_fixture_mode(EndFixture *f,uint64_t phase,uint64_t generation,unsigned roots,unsigned native,int retire) {
    memset(f,0,sizeof(*f));audit_fixture(&f->f,phase,generation,roots);f->h.last_identity=16;
    f->f.queue.tasks=(uintptr_t)&f->task;f->f.queue.capacity=1;f->f.queue.free_head=1;
    for(unsigned i=0;i<2;i++){
        f->f.ctx[i].thread_id=0;f->p[i].context=(uintptr_t)&f->f.ctx[i];
        assert(gc_handshake_step_locked(&f->h,&f->p[i],GC_HS_REGISTER)==0);
        atomic_store(&f->f.ctx[i].adopted_phase,phase);
    }
    audit_finish_fixture_mode(&f->f,retire?gc_root_retire_audit_step_locked:gc_root_audit_step_locked);
    for(unsigned i=0;i<2;i++){
        atomic_store(&f->f.ctx[i].state,GC_RUNNING);
        if(native&(1u<<i))assert(gc_handshake_step_locked(&f->h,&f->p[i],GC_HS_ENTER_NATIVE)==0);
    }
    assert(gc_handshake_step_locked(&f->h,NULL,GC_HS_BEGIN)==0);
    for(unsigned i=0;i<2;i++)if(!(native&(1u<<i)))assert(gc_handshake_step_locked(&f->h,&f->p[i],GC_HS_ACK)==0);
    f->remembered.epoch=f->remembered.last_phase=phase;
    f->q=(MarkEndRequest){(uintptr_t)&f->h,(uintptr_t)&f->f.a,f->h.generation,(uintptr_t)&f->remembered};
}
static void end_fixture(EndFixture *f,uint64_t phase,uint64_t generation,unsigned roots,unsigned native){
    end_fixture_mode(f,phase,generation,roots,native,0);
}
static uint64_t end_normalize(EndFixture *d,const EndFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<end_pointer_count;i++){unsigned char *p=(unsigned char *)d+end_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}}return mask;}
static void end_clone(EndFixture *d,const EndFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=end_normalize(d,s);
    for(unsigned i=0;i<end_pointer_count;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+end_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void end_remembered_setup(EndFixture *f,unsigned stage) {
    f->remembered=(RememberedInventory){.epoch=0x70022,.last_phase=0x70022};
    f->remembered_set=(RememberedSet){.start=4096,.limit=4160,.incarnation=17,.bits={(uintptr_t)&f->remembered_bits[0],(uintptr_t)&f->remembered_bits[1]},.words=1,.epoch=0x70022,.last_phase=0x70022};
    f->remembered_node=(RememberedRegistration){.set=(uintptr_t)&f->remembered_set};
    RememberedInventoryRequest q={(uintptr_t)&f->remembered_node,(uintptr_t)&f->remembered_job,0x81021};
    assert(gc_remembered_inventory_step_locked(&f->remembered,&q,GC_RI_ATTACH)==0);
    f->remembered_job=(RememberedJob){.offset=4096,.incarnation=17,.phase=0x70022,.budget=1};
    assert(gc_remembered_inventory_step_locked(&f->remembered,&q,GC_RI_RECORD)==0);
    assert(gc_remembered_inventory_step_locked(&f->remembered,&q,GC_RI_BEGIN)==0);
    if(stage>=1){f->remembered_job.phase=q.phase;assert(gc_remembered_inventory_step_locked(&f->remembered,&q,GC_RI_FLIP)==0);}
    if(stage>=2)assert(gc_remembered_inventory_step_locked(&f->remembered,&q,GC_RI_TAKE)==0);
    if(stage>=3)assert(gc_remembered_inventory_step_locked(&f->remembered,&q,GC_RI_ACK)==0);
}
static void end_compare_mode(EndFixture *a,MarkEndChecker step,MarkEndChecker reference_step,uint64_t op,uint64_t expected) {
    EndFixture c,ba,bc,x;end_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->f.g,&a->q,op),reference=reference_step(&c.f.g,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"mark end case %u got %llu C %llu expected %llu\n",end_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    // Every result, including READY, must preserve every byte in all owners.
    assert(!memcmp(&ba,a,sizeof(ba))&&!memcmp(&bc,&c,sizeof(bc)));
    if(end_vectors){memcpy(&x,&c,sizeof(x));uint64_t mask=end_normalize(&x,&c),record[]={op,reference,mask,mask};
        assert(fwrite(record,sizeof(record),1,end_vectors)==1);assert(fwrite(&x,sizeof(x),1,end_vectors)==1);assert(fwrite(&x,sizeof(x),1,end_vectors)==1);}end_cases++;
}
static void end_compare(EndFixture *a,MarkEndChecker step,uint64_t op,uint64_t expected){
    end_compare_mode(a,step,gc_mark_end_check_locked,op,expected);
}
static void retire_inventory_compare(EndFixture *a,MarkEndChecker step,uint64_t op,uint64_t expected){
    end_compare_mode(a,step,gc_retire_inventory_check_locked,op,expected);
}
static void end_pointer_init(void) {
    end_pointer_count=sizeof(audit_pointers)/8;memcpy(end_pointers,audit_pointers,sizeof(audit_pointers));
#define END_PTR(i) offsetof(EndFixture,p[i].context),offsetof(EndFixture,p[i].registry),offsetof(EndFixture,p[i].previous),offsetof(EndFixture,p[i].next)
    const uint64_t extra[]={offsetof(EndFixture,f.queue.overflow_head),offsetof(EndFixture,f.queue.overflow_tail),offsetof(EndFixture,h.head),END_PTR(0),END_PTR(1),offsetof(EndFixture,q.handshake),offsetof(EndFixture,q.audit),offsetof(EndFixture,f.queue.tasks),offsetof(EndFixture,q.remembered),offsetof(EndFixture,remembered.head),offsetof(EndFixture,remembered_node.registry),offsetof(EndFixture,remembered_node.set),offsetof(EndFixture,remembered_node.previous),offsetof(EndFixture,remembered_node.next),offsetof(EndFixture,remembered_set.bits[0]),offsetof(EndFixture,remembered_set.bits[1])};
#undef END_PTR
    memcpy(end_pointers+end_pointer_count,extra,sizeof(extra));end_pointer_count+=sizeof(extra)/8;assert(end_pointer_count<64);
}
static void gc_run_mark_end_cases(MarkEndChecker step) {
    end_pointer_init();
    EndFixture a;end_cases=0;const char *path=getenv("KLASSIC_GC_MARK_END_VECTORS");
    if(path){end_vectors=fopen(path,"wb");assert(end_vectors);assert(fwrite("KLMEND01",8,1,end_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(EndFixture,f.g),offsetof(EndFixture,q),end_pointer_count};
        assert(fwrite(header,sizeof(header),1,end_vectors)==1);assert(fwrite(end_pointers,end_pointer_count*8,1,end_vectors)==1);}
    const uint64_t epochs[]={0,1,31};
    for(unsigned gen=0;gen<2;gen++)for(unsigned ep=0;ep<3;ep++)for(unsigned y=1;y<=3;y++)for(unsigned o=1;o<=3;o++)
    for(unsigned roots=0;roots<4;roots++)for(unsigned native=0;native<4;native++) {
        end_fixture(&a,0x90000|(epochs[ep]<<8)|y|(o<<4),gen,roots,native);
        end_compare(&a,step,0,GC_ME_READY);
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned owner=0;owner<2;owner++)for(unsigned fault=0;fault<28;fault++) {
        end_fixture(&a,0x90022,gen,3,0);MutatorContext *ctx=&a.f.ctx[owner];ThreadRegistration *p=&a.p[owner];uint64_t expected=GC_ME_INVALID;
        switch(fault) {
        case 0:p->registry=0;break;case 1:p->registered=0;break;case 2:p->previous=(uintptr_t)p;break;
        case 3:p->identity=0;break;case 4:p->identity=a.h.last_identity+1;break;case 5:p->context=0;break;
        case 6:p->context++;break;case 7:p->acknowledged_generation++;break;case 8:p->acknowledged_generation=0;break;
        case 9:ctx->thread_id++;break;case 10:atomic_store(&ctx->state,GC_RUNNING);break;case 11:atomic_store(&ctx->state,GC_DETACHED);break;
        case 12:atomic_store(&ctx->adopted_phase,a.f.g.phase+0x10000);break;case 13:ctx->abi_version++;break;
        case 14:atomic_store(&ctx->access_epoch,1);break;case 15:ctx->retiring_roots=1;expected=GC_ME_WAIT;break;
        case 16:ctx->root_audit_serial[gen]++;expected=GC_ME_STALE;break;
        case 17:ctx->root_audit_identity[gen]++;expected=GC_ME_STALE;break;
        case 18:atomic_fetch_add(&ctx->root_revision,1);expected=GC_ME_STALE;break;
        case 19:ctx->satb_consumer.serial=1;expected=GC_ME_WAIT;break;
        case 20:ctx->satb_consumer.end=1;expected=GC_ME_WAIT;break;
        case 21:ctx->remembered_consumer.serial=1;expected=GC_ME_WAIT;break;
        case 22:ctx->remembered_consumer.end=1;expected=GC_ME_WAIT;break;
        case 23:ctx->satb.top=1;expected=GC_ME_WAIT;break;
        case 24:ctx->remembered.top=1;atomic_store(&ctx->remembered.published,1);expected=GC_ME_WAIT;break;
        case 25:atomic_store(&ctx->satb.published,1);break;case 26:atomic_store(&ctx->remembered.consumed,1);break;
        case 27:atomic_store(&ctx->state,GC_DETACHING);expected=GC_ME_READY;break;
        }
        end_compare(&a,step,0,expected);
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned fault=0;fault<48;fault++) {
        end_fixture(&a,0x90022,gen,3,1);uint64_t op=0,expected=GC_ME_INVALID;
        switch(fault) {
        case 0:op=1;break;case 1:a.q.handshake=0;break;case 2:a.q.handshake++;break;
        case 3:a.q.audit=0;break;case 4:a.q.audit++;break;case 5:a.h.active=2;break;
        case 6:a.h.native=3;break;case 7:a.h.head=0;break;case 8:a.h.members=0;break;
        case 9:a.h.acknowledged=2;break;case 10:a.h.generation=0;break;case 11:a.h.expected=2;break;
        case 12:a.h.active=0;break;case 13:a.q.generation=0;break;
        case 14:a.q.generation++;expected=GC_ME_STALE;break;
        case 15:a.h.active=a.h.acknowledged=a.h.expected=0;expected=GC_ME_WAIT;break;
        case 16:a.h.acknowledged=0;expected=GC_ME_WAIT;break;
        case 17:a.f.g.retiring=1;expected=GC_ME_WAIT;break;case 18:a.f.g.leases=1;expected=GC_ME_WAIT;break;
        case 19:a.f.g.walkers=1;expected=GC_ME_WAIT;break;case 20:a.f.a.state=1;break;
        case 21:a.f.a.serial=0;break;case 22:a.f.a.last_scope++;expected=GC_ME_STALE;break;
        case 23:a.f.a.epoch++;expected=GC_ME_STALE;break;case 24:a.f.g.phase^=UINT64_C(1)<<(8+gen);expected=GC_ME_STALE;break;
        case 25:a.f.queue.tasks=0;break;case 26:a.f.queue.capacity=0;break;case 27:a.f.queue.free_head=0;break;
        case 28:a.f.queue.queued=1;a.f.queue.head=a.f.queue.tail=1;a.f.queue.free_head=0;expected=GC_ME_WAIT;break;
        case 29:a.f.queue.in_flight=1;a.f.queue.free_head=0;expected=GC_ME_WAIT;break;
        case 30:a.f.queue.overflow_count=1;a.f.queue.overflow_head=a.f.queue.overflow_tail=(uintptr_t)&a.task;expected=GC_ME_WAIT;break;
        case 31:a.h.head++;break;case 32:a.h.head=(uintptr_t)&a.p[0];break;
        case 33:a.p[0].next=(uintptr_t)&a.p[1];break;
        case 34:a.p[1].next=0;break;case 35:a.p[0].identity=a.p[1].identity;a.f.ctx[0].thread_id=a.p[0].identity;break;
        case 36:a.h.native=0;a.h.expected=a.h.acknowledged=2;break;
        case 37:a.f.queue.overflow_count=1;break;case 38:a.f.queue.queued=2;break;
        case 39:a.f.a.queue=0;break;case 40:a.f.a.queue++;break;
        case 41:a.h.members=3;a.h.expected=a.h.acknowledged=2;break;
        case 42:a.h.last_identity=UINT64_MAX;a.p[1].identity=UINT64_MAX;a.f.ctx[1].thread_id=UINT64_MAX;a.f.ctx[1].root_audit_identity[gen]=UINT64_MAX;a.f.s[1].owner=UINT64_MAX;expected=GC_ME_READY;break;
        case 43:atomic_store(&a.f.ctx[0].adopted_phase,0);expected=GC_ME_READY;break; // native owner has not yet adopted
        case 44:a.f.ctx[0].satb.top=UINT64_MAX;atomic_store(&a.f.ctx[0].satb.published,UINT64_MAX);atomic_store(&a.f.ctx[0].satb.consumed,UINT64_MAX);expected=GC_ME_READY;break;
        case 45:a.f.ctx[0].root_audit_serial[1-gen]++;expected=GC_ME_READY;break;
        case 46:a.p[0].acknowledged_generation=a.h.generation+1;break;
        case 47:a.h.generation=a.q.generation=UINT64_MAX;a.p[1].acknowledged_generation=UINT64_MAX;expected=GC_ME_READY;break;
        }
        end_compare(&a,step,op,expected);
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned fault=0;fault<13;fault++) {
        end_fixture(&a,0x91022,gen,3,0);end_remembered_setup(&a,fault%4);
        uint64_t expected=gen||fault%4==3?GC_ME_READY:GC_ME_WAIT;
        switch(fault){case 0:case 1:case 2:case 3:break;
        case 4:a.q.remembered=0;expected=GC_ME_INVALID;break;
        case 5:a.q.remembered++;expected=GC_ME_INVALID;break;
        case 6:a.remembered.pending=2;expected=GC_ME_INVALID;break;
        case 7:a.remembered.epoch+=0x20000;a.remembered.last_phase=a.remembered.epoch;expected=gen?GC_ME_READY:GC_ME_STALE;break;
        case 8:a.remembered.previous_epoch=0;a.remembered.epoch^=0x1000;expected=gen?GC_ME_READY:GC_ME_STALE;break;
        case 9:a.remembered.head=0;expected=GC_ME_INVALID;break;
        case 10:a.remembered.unflipped=2;expected=GC_ME_INVALID;break;
        case 11:a.remembered.last_identity=0;expected=GC_ME_INVALID;break;
        case 12:a.remembered.last_phase+=0x30000;expected=gen?GC_ME_READY:GC_ME_STALE;break;}
        end_compare(&a,step,0,expected);
    }
    for(unsigned gen=0;gen<2;gen++){
        end_fixture(&a,0x91022,gen,3,0);end_remembered_setup(&a,3);
        a.remembered.walkers=1;end_compare(&a,step,0,gen?GC_ME_READY:GC_ME_WAIT);
    }
    // Empty membership is also a real rendezvous, with a completed empty audit.
    end_fixture(&a,0x90022,0,0,0);assert(gc_handshake_step_locked(&a.h,NULL,GC_HS_END)==0);
    for(unsigned i=0;i<2;i++){atomic_store(&a.f.ctx[i].state,GC_DETACHING);assert(gc_handshake_step_locked(&a.h,&a.p[i],GC_HS_DETACH)==0);}
    assert(gc_handshake_step_locked(&a.h,NULL,GC_HS_BEGIN)==0);a.q.generation=a.h.generation;end_compare(&a,step,0,GC_ME_READY);
    if(end_vectors){assert(fclose(end_vectors)==0);end_vectors=NULL;}
    printf("mark end %u C/candidate full-inventory, rendezvous and pending-work cases PASS\n",end_cases);
    // A forged/premature ACK cannot turn private work into mark completion.
    for(unsigned gen=0;gen<2;gen++)for(unsigned native=0;native<4;native++)for(unsigned owner=0;owner<2;owner++){
        end_fixture(&a,0x90022,gen,3,native);
        atomic_store(&a.f.ctx[owner].private_producers,1);
        end_compare(&a,step,0,GC_ME_WAIT);
    }
    puts("mark end private producers: 16 premature acknowledgements retain pending work PASS");

}
static void gc_run_retire_inventory_cases(MarkEndChecker step){
    end_pointer_init();end_cases=0;EndFixture a;
    for(unsigned gen=0;gen<2;gen++)for(unsigned ep=0;ep<32;ep++)for(unsigned other=0;other<5;other++)
    for(unsigned roots=0;roots<4;roots++)for(unsigned native=0;native<4;native++){
        uint64_t phase=0x90000|(ep<<8)|(7<<(4*gen))|(other<<(4*(1-gen)));
        end_fixture_mode(&a,phase,gen,roots,native,1);retire_inventory_compare(&a,step,0,GC_ME_READY);
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned owner=0;owner<2;owner++)for(unsigned native=0;native<4;native++)
    for(unsigned fault=0;fault<15;fault++){
        end_fixture_mode(&a,0x90000|(7<<(4*gen)),gen,3,native,1);
        MutatorContext *ctx=&a.f.ctx[owner];ThreadRegistration *p=&a.p[owner];uint64_t result=GC_ME_INVALID;
        switch(fault){
        case 0:atomic_store(&ctx->private_producers,1);result=GC_ME_WAIT;break;
        case 1:ctx->satb_consumer.cursor=1;result=GC_ME_WAIT;break;
        case 2:ctx->remembered_consumer.cursor=1;result=GC_ME_WAIT;break;
        case 3:ctx->satb.top=1;result=GC_ME_WAIT;break;
        case 4:ctx->remembered.top=1;atomic_store(&ctx->remembered.published,1);result=GC_ME_WAIT;break;
        case 5:atomic_fetch_add(&ctx->root_revision,1);result=GC_ME_STALE;break;
        case 6:ctx->root_audit_identity[gen]++;result=GC_ME_STALE;break;
        case 7:atomic_store(&ctx->access_epoch,1);break;
        case 8:ctx->retiring_roots=1;result=GC_ME_WAIT;break;
        case 9:ctx->thread_id++;break;case 10:p->identity=0;break;
        case 11:atomic_store(&ctx->state,GC_RUNNING);break;case 12:ctx->abi_version++;break;
        case 13:atomic_store(&ctx->adopted_phase,0);if(native&(1u<<owner))result=GC_ME_READY;break;
        case 14:p->acknowledged_generation=0;if(native&(1u<<owner))result=GC_ME_READY;break;
        }
        retire_inventory_compare(&a,step,0,result);
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned fault=0;fault<33;fault++){
        end_fixture_mode(&a,0x90000|(7<<(4*gen)),gen,3,1,1);uint64_t result=GC_ME_INVALID,op=0;
        switch(fault){
        case 0:a.q.generation++;result=GC_ME_STALE;break;
        case 1:a.h.acknowledged=0;result=GC_ME_WAIT;break;
        case 2:a.f.g.retiring=1;result=GC_ME_WAIT;break;
        case 3:a.f.g.leases=1;result=GC_ME_WAIT;break;
        case 4:a.f.g.walkers=1;result=GC_ME_WAIT;break;
        case 5:a.h.head=0;break;case 6:a.p[0].next=(uintptr_t)&a.p[1];break;
        case 7:a.p[1].next=0;break;case 8:a.h.members=3;a.h.expected=a.h.acknowledged=2;break;
        case 9:a.f.g.phase^=0x10000;result=GC_ME_STALE;break;
        case 10:a.f.a.last_scope++;result=GC_ME_STALE;break;
        case 11:a.f.queue.epoch++;result=GC_ME_STALE;break;
        case 12:a.q.generation=0;break;
        case 13:a.h.active=a.h.expected=a.h.acknowledged=0;result=GC_ME_WAIT;break;
        case 14:a.f.queue.free_head=0;break;
        case 15:a.f.queue.queued=1;a.f.queue.head=a.f.queue.tail=1;a.f.queue.free_head=0;result=GC_ME_WAIT;break;
        case 16:a.f.queue.in_flight=1;a.f.queue.free_head=0;result=GC_ME_WAIT;break;
        case 17:a.f.queue.overflow_count=1;a.f.queue.overflow_head=a.f.queue.overflow_tail=(uintptr_t)&a.task;result=GC_ME_WAIT;break;
        case 18:a.f.queue.head=1;break;case 19:a.q.remembered=0;break;
        case 20:a.remembered.walkers=1;result=GC_ME_WAIT;break;
        case 21:a.remembered.epoch^=0x1000;a.remembered.last_phase=a.remembered.epoch;result=GC_ME_STALE;break;
        case 22:a.remembered.last_phase+=0x10000;result=GC_ME_STALE;break;
        case 23:op=1;break;case 24:a.h.native=0;a.h.expected=a.h.acknowledged=2;break;
        case 25:a.f.a.state=1;break;case 26:a.q.handshake=0;break;case 27:a.q.audit=0;break;
        case 28:a.h.head++;break;case 29:a.p[0].acknowledged_generation=a.h.generation+1;break;
        case 30:a.f.queue.generation^=1;result=GC_ME_STALE;break;
        case 31:a.f.queue.epoch=0;result=GC_ME_STALE;break;
        case 32:a.f.g.phase-=1<<(4*gen);result=GC_ME_STALE;break;
        }
        retire_inventory_compare(&a,step,op,result);
    }
    // Real remembered flip/take/ack controls gate both retiring generations.
    for(unsigned gen=0;gen<2;gen++)for(unsigned stage=0;stage<4;stage++){
        end_fixture_mode(&a,0x91000|(7<<(4*gen)),gen,3,0,1);end_remembered_setup(&a,stage);
        retire_inventory_compare(&a,step,0,stage==3?GC_ME_READY:GC_ME_WAIT);
    }
    end_fixture_mode(&a,0x90007,0,0,0,1);assert(gc_handshake_step_locked(&a.h,NULL,GC_HS_END)==GC_HS_OK);
    for(unsigned i=0;i<2;i++){atomic_store(&a.f.ctx[i].state,GC_DETACHING);assert(gc_handshake_step_locked(&a.h,&a.p[i],GC_HS_DETACH)==GC_HS_OK);}
    assert(gc_handshake_step_locked(&a.h,NULL,GC_HS_BEGIN)==GC_HS_OK);a.q.generation=a.h.generation;
    retire_inventory_compare(&a,step,0,GC_ME_READY);
    printf("retire inventory %u C/candidate rendezvous, all-owner and registered-work cases PASS\n",end_cases);
}
#endif
