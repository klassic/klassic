#ifndef KLASSIC_GC_ROOT_LIFETIME_CASES_H
#define KLASSIC_GC_ROOT_LIFETIME_CASES_H
#include "gc_root_lifetime.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RootLife)(RootRegistry *, RootLifeRequest *, uint64_t);
typedef uint64_t (*RootClear)(RootLifeRequest *);
typedef uint64_t (*RootLeasedScan)(RootScanJob *);
typedef struct {
    RootRegistry g; MutatorContext ctx; RootSegment s[3]; RootRegistration r[3]; RootLease l[3];
    RootLifeRequest q; RootScanJob job; uint64_t output[256]; RootCell cells[3][300];
    BufferCursor journal; RootChange entries[8];
} RootLifeFixture;
_Static_assert(sizeof(RootLifeFixture) < 0x8000, "ARM replay arena bound");
#define LIFE_PTR(i) offsetof(RootLifeFixture,s[i].cells), offsetof(RootLifeFixture,s[i].registration), \
    offsetof(RootLifeFixture,r[i].segment), offsetof(RootLifeFixture,r[i].previous), offsetof(RootLifeFixture,r[i].next), \
    offsetof(RootLifeFixture,l[i].registration), offsetof(RootLifeFixture,r[i].inventory_previous), offsetof(RootLifeFixture,r[i].inventory_next), \
    offsetof(RootLifeFixture,r[i].context), offsetof(RootLifeFixture,r[i].owner_previous), offsetof(RootLifeFixture,r[i].owner_next), offsetof(RootLifeFixture,s[i].journal)
static const uint64_t life_pointers[] = {LIFE_PTR(0), LIFE_PTR(1), LIFE_PTR(2),
    offsetof(RootLifeFixture,ctx.root_segments),
    offsetof(RootLifeFixture,journal.entries),
    offsetof(RootLifeFixture,g.inventory), offsetof(RootLifeFixture,g.head), offsetof(RootLifeFixture,q.registration), offsetof(RootLifeFixture,q.lease),
    offsetof(RootLifeFixture,q.context), offsetof(RootLifeFixture,job.scan.segment),
    offsetof(RootLifeFixture,job.scan.output), offsetof(RootLifeFixture,job.lease)};
#undef LIFE_PTR
static FILE *life_vectors[3]; static unsigned life_cases[3];
static void life_fixture(RootLifeFixture *f, uint64_t phase) {
    memset(f,0,sizeof(*f)); f->g.phase = phase; f->ctx.abi_version = GC_ABI_VERSION; f->ctx.thread_id = 17;
    atomic_store(&f->ctx.adopted_phase, phase);
    f->journal.entries=(uintptr_t)f->entries; f->journal.capacity=8;
    for (unsigned i=0;i<3;i++) { f->s[i].owner=17; f->s[i].cells=(uintptr_t)f->cells[i]; f->s[i].capacity=300; f->r[i].segment=(uintptr_t)&f->s[i]; }
    f->q=(RootLifeRequest){.registration=(uintptr_t)&f->r[0],.lease=(uintptr_t)&f->l[0],.context=(uintptr_t)&f->ctx};
    f->job=(RootScanJob){.scan={(uintptr_t)&f->s[0],0,(uintptr_t)f->output,256,0},.lease=(uintptr_t)&f->l[0]};
    memset(f->output,0xab,sizeof(f->output));
}
static uint64_t life_normalize(RootLifeFixture *dst,const RootLifeFixture *src) {
    uint64_t mask=0;
    for (unsigned i=0;i<sizeof(life_pointers)/8;i++) {
        unsigned char *p=(unsigned char *)dst+life_pointers[i]; uint64_t v; memcpy(&v,p,8);
        if(v>=(uintptr_t)src && v-(uintptr_t)src<sizeof(*src)) { v-=(uintptr_t)src; mask|=UINT64_C(1)<<i; memcpy(p,&v,8); }
    } return mask;
}
static void life_copy(RootLifeFixture *dst,const RootLifeFixture *src) {
    memcpy(dst,src,sizeof(*dst)); uint64_t mask=life_normalize(dst,src);
    for(unsigned i=0;i<sizeof(life_pointers)/8;i++) if(mask&(UINT64_C(1)<<i)) {
        unsigned char *p=(unsigned char *)dst+life_pointers[i]; uint64_t v; memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void life_compare(RootLifeFixture *a,RootLifeFixture *c,unsigned kind,uint64_t op,
        RootLife step,RootClear clear,RootLeasedScan scan,uint64_t expected) {
    RootLifeFixture before_a,before_c,x,y;
    memcpy(&before_a,a,sizeof(*a));memcpy(&before_c,c,sizeof(*c));
    uint64_t result=kind==0?step(&a->g,&a->q,op):kind==1?clear(&a->q):scan(&a->job);
    uint64_t reference=kind==0?gc_root_life_locked(&c->g,&c->q,op):kind==1?gc_root_clear_chunk(&c->q):gc_root_scan_leased(&c->job);
    if(result!=expected||reference!=expected){fprintf(stderr,"root life kind %u op %llu case %u got %llu C %llu expected %llu\n",kind,(unsigned long long)op,life_cases[kind],(unsigned long long)result,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if((kind==0&&result)||result==1){assert(!memcmp(a,&before_a,sizeof(*a)));assert(!memcmp(c,&before_c,sizeof(*c)));}
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(life_normalize(&x,a)==life_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(life_vectors[kind]) {
        memcpy(&x,&before_c,sizeof(x));memcpy(&y,c,sizeof(y));
        uint64_t record[]={op,reference,life_normalize(&x,c),life_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,life_vectors[kind])==1);
        assert(fwrite(&x,sizeof(x),1,life_vectors[kind])==1);assert(fwrite(&y,sizeof(y),1,life_vectors[kind])==1);
    } life_cases[kind]++;
}
static void life_select(RootLifeFixture *f,unsigned entry,unsigned lease) {
    f->q.registration=(uintptr_t)&f->r[entry];f->q.scope=f->r[entry].scope;
    f->q.lease=(uintptr_t)&f->l[lease];f->q.serial=f->l[lease].serial;
}
static void gc_run_root_lifetime_cases(RootLife step,RootClear clear,RootLeasedScan scan) {
    RootLifeFixture *a=malloc(sizeof(*a)),*c=malloc(sizeof(*c));assert(a&&c);
    const char *names[]={"KLASSIC_GC_ROOT_LIFE_VECTORS","KLASSIC_GC_ROOT_CLEAR_VECTORS","KLASSIC_GC_ROOT_LEASED_VECTORS"};
    const char *magic[]={"KLRLIF01","KLRCLR01","KLRLSC01"};
    for(unsigned k=0;k<3;k++){life_cases[k]=0;const char *path=getenv(names[k]);if(path){
        life_vectors[k]=fopen(path,"wb");assert(life_vectors[k]);assert(fwrite(magic[k],8,1,life_vectors[k])==1);
        uint64_t h[]={sizeof(*a),k==0?offsetof(RootLifeFixture,g):k==1?offsetof(RootLifeFixture,q):offsetof(RootLifeFixture,job),offsetof(RootLifeFixture,q),sizeof(life_pointers)/8};
        assert(fwrite(h,sizeof(h),1,life_vectors[k])==1);assert(fwrite(life_pointers,sizeof(life_pointers),1,life_vectors[k])==1);
    }}
#define LIFE(kind,op,result) life_compare(a,c,kind,op,step,clear,scan,result)
#define SELECT(e,l) do { life_select(a,e,l);life_select(c,e,l); } while(0)
    const unsigned phases[]={0,1,2,4}, sizes[]={0,1,256,300};
    for(unsigned yp=0;yp<4;yp++)for(unsigned op=0;op<4;op++)for(unsigned sz=0;sz<4;sz++){
        unsigned count=sizes[sz];uint64_t phase=phases[yp]|(phases[op]<<4);
        life_fixture(a,phase);life_copy(c,a); LIFE(0,GC_RL_ATTACH,0);
        uint64_t scope=a->q.scope;
        assert(scope==1&&a->g.active==1&&a->s[0].registration==(uintptr_t)&a->r[0]);
        for(unsigned i=0;i<count;i++){
            atomic_store(&a->cells[0][i].value,((uint64_t)(i+1)<<20)|0x1510);
            a->cells[0][i].active=1;a->cells[0][i].reuse_generation=12;
        }
        atomic_store(&a->s[0].used,count); life_copy(c,a);
        LIFE(0,GC_RL_ACQUIRE,0);uint64_t old_serial=a->q.serial;
        a->job.serial=c->job.serial=old_serial;
        LIFE(2,0,count>256?2:0);
        RootRequest raw={.segment=(uintptr_t)&a->s[0]}; assert(gc_root_step(&a->ctx,&raw,GC_ROOT_RETIRE)==GC_ROOT_INVALID);
        LIFE(0,GC_RL_RETIRE,0);
        assert(a->g.active==0&&a->g.retiring==1&&a->g.head==(uintptr_t)&a->r[0]);
        uint64_t mask=(phases[yp]==1||phases[yp]==2?1u:0u)|(phases[op]==1||phases[op]==2?2u:0u);
        assert(a->r[0].required_mask==mask&&a->r[0].limit==count);
        SELECT(0,1); LIFE(0,GC_RL_ACQUIRE,1);
        while(a->r[0].cursor<count){
            LIFE(0,GC_RL_CLAIM,0);
            uint64_t begin=a->q.begin,end=a->q.end;
            LIFE(0,GC_RL_ACK,2); // reading the claimed range is required, even with matching mask
            assert(end>begin&&end-begin<=256&&a->q.mask==mask&&a->q.phase==phase);
            a->job.lease=(uintptr_t)&a->l[1];c->job.lease=(uintptr_t)&c->l[1];
            a->job.serial=c->job.serial=a->q.serial;a->job.scan.cursor=c->job.scan.cursor=begin;
            a->job.scan.capacity=c->job.scan.capacity=end-begin;
            a->job.scan.cursor=c->job.scan.cursor=begin+1;LIFE(2,0,1);
            a->job.scan.cursor=c->job.scan.cursor=begin;
            LIFE(2,0,end<count?2:0);
            assert(a->job.scan.written==end-begin);
            for(uint64_t i=0;i<end-begin;i++)assert(a->output[i]==((begin+i+1)<<20|0x1510));
            // ACK is withheld until both captured mark obligations are handled.
            a->q.mask=c->q.mask=mask^1;LIFE(0,GC_RL_ACK,2);a->q.mask=c->q.mask=mask;
            LIFE(0,GC_RL_ACK,0);LIFE(0,GC_RL_ACK,1);LIFE(2,0,1);
            assert(a->r[0].cursor==end);
        }
        SELECT(0,1);LIFE(0,GC_RL_CLAIM,2);LIFE(0,GC_RL_CLEAN,2); // old reader still owns storage
        SELECT(0,0);LIFE(0,GC_RL_RELEASE,0);LIFE(0,GC_RL_RELEASE,1);
        SELECT(0,2);LIFE(0,GC_RL_CLEAN,0);
        if(count)LIFE(0,GC_RL_FINISH,2);
        do { uint64_t cursor=atomic_load(&a->r[0].cleanup_cursor);LIFE(1,0,count-cursor>256?2:0); }
        while(atomic_load(&a->r[0].cleanup_cursor)!=count);
        for(unsigned i=0;i<count;i++)assert(!atomic_load(&a->cells[0][i].value)&&!a->cells[0][i].active&&a->cells[0][i].reuse_generation==12);
        LIFE(0,GC_RL_FINISH,0);LIFE(0,GC_RL_FINISH,1);LIFE(1,0,1);
        assert(!a->g.head&&!a->g.retiring&&!a->g.leases&&!a->s[0].scope&&!a->s[0].registration);
        LIFE(0,GC_RL_ATTACH,0);assert(a->q.scope==scope+1);
        SELECT(0,0);LIFE(0,GC_RL_ACQUIRE,0);assert(a->q.serial>old_serial);
        a->job.lease=(uintptr_t)&a->l[0];c->job.lease=(uintptr_t)&c->l[0];
        a->job.serial=c->job.serial=old_serial;a->job.scan.cursor=c->job.scan.cursor=0;
        LIFE(2,0,1);a->job.serial=c->job.serial=a->q.serial;LIFE(2,0,0);
        // Actual same-address cell reservation increments generation after clean.
        RootRequest request={.segment=(uintptr_t)&a->s[0]};assert(gc_root_step(&a->ctx,&request,0)==0);
        assert(request.handle.reuse_generation==(count?13u:1u));
        RootHandle current=request.handle;request.handle.scope=scope;
        assert(gc_root_step(&a->ctx,&request,3)==GC_ROOT_INVALID);
        request.handle=current;request.handle.reuse_generation--;
        assert(gc_root_step(&a->ctx,&request,3)==GC_ROOT_INVALID);
    }
    // Queue removal in middle, then head, then tail, all constant work.
    life_fixture(a,0);life_copy(c,a);
    for(unsigned i=0;i<3;i++){SELECT(i,0);LIFE(0,0,0);LIFE(0,3,0);}
    const unsigned order[]={1,2,0};
    for(unsigned j=0;j<3;j++){SELECT(order[j],0);LIFE(0,6,0);LIFE(1,0,0);LIFE(0,7,0);assert(a->g.retiring==2-j);}
    assert(!a->g.head);
    for(unsigned fault=0;fault<16;fault++){
        life_fixture(a,0);uint64_t op=0,result=1;
        switch(fault){
        case 0:a->q.registration=0;break;case 1:a->q.registration++;break;
        case 2:a->r[0].segment=0;break;case 3:a->s[0].cells++;break;
        case 4:a->s[0].capacity=0;break;case 5:a->s[0].capacity=UINT64_MAX;break;
        case 6:a->s[0].cells=UINT64_MAX-7;break;case 7:a->q.context=0;break;
        case 8:a->ctx.abi_version++;break;case 9:a->ctx.thread_id++;break;
        case 10:atomic_store(&a->ctx.state,GC_BLOCKED_NATIVE);break;
        case 11:atomic_store(&a->ctx.access_epoch,1);break;
        case 12:a->g.phase=2;break;case 13:a->g.last_scope=UINT64_MAX;result=3;break;
        case 14:a->g.retiring=1;break;case 15:op=8;break;
        } life_copy(c,a);LIFE(0,op,result);
    }
    // Detach retirement accepts exactly the u32 state without widening ABI load.
    life_fixture(a,0);life_copy(c,a);LIFE(0,0,0);atomic_store(&a->ctx.state,GC_DETACHING);atomic_store(&c->ctx.state,GC_DETACHING);LIFE(0,3,0);
    // Three nested owner segments; retire a middle segment without a traversal.
    life_fixture(a,0x22);life_copy(c,a);
    for(unsigned i=0;i<3;i++){SELECT(i,0);LIFE(0,0,0);assert(a->ctx.root_segments==(uintptr_t)&a->r[i]);}
    assert(a->r[2].owner_next==(uintptr_t)&a->r[1]&&a->r[1].owner_next==(uintptr_t)&a->r[0]);
    for(unsigned j=0;j<3;j++){
        unsigned i=order[j];SELECT(i,0);LIFE(0,3,0);
        assert(a->ctx.retiring_roots==1&&a->r[i].context==(uintptr_t)&a->ctx);
        LIFE(0,6,0);LIFE(1,0,0);LIFE(0,7,0);assert(!a->ctx.retiring_roots);
    }
    assert(!a->ctx.root_segments&&!a->g.inventory);
    // Full state preservation for malformed owner chains and counter overflow.
    for(unsigned fault=0;fault<10;fault++){
        life_fixture(a,0);for(unsigned i=0;i<3;i++){life_select(a,i,0);assert(step(&a->g,&a->q,0)==0);}
        life_select(a,1,0);uint64_t expected=1;
        switch(fault){
        case 0:a->r[1].context=0;break;
        case 1:a->ctx.retiring_roots=UINT64_MAX;expected=3;break;
        case 2:a->r[1].owner_previous++;break;
        case 3:a->r[2].owner_next=0;break;
        case 4:a->r[2].context=0;break;
        case 5:a->r[1].owner_next++;break;
        case 6:a->r[0].owner_previous=0;break;
        case 7:a->r[0].context=0;break;
        case 8:life_select(a,2,0);a->ctx.root_segments=0;break;
        case 9:life_select(a,2,0);a->ctx.root_segments++;break;
        }
        life_copy(c,a);LIFE(0,3,expected);
    }
    // Completion retains its owner context while a root journal is unconsumed.
    for(unsigned fault=0;fault<8;fault++){
        life_fixture(a,0);a->s[0].journal=(uintptr_t)&a->journal;
        assert(step(&a->g,&a->q,0)==0);assert(step(&a->g,&a->q,3)==0);
        assert(step(&a->g,&a->q,6)==0);assert(clear(&a->q)==0);
        uint64_t expected=1;
        switch(fault){case 0:a->r[0].context=0;break;case 1:a->r[0].context++;break;
        case 2:a->ctx.abi_version++;break;case 3:a->ctx.thread_id++;break;
        case 4:a->ctx.retiring_roots=0;break;case 5:a->s[0].journal++;break;
        case 6:atomic_store(&a->journal.consumed,1);break;
        case 7:a->journal.top=1;atomic_store(&a->journal.published,1);expected=2;break;}
        life_copy(c,a);LIFE(0,7,expected);
        if(fault==7){atomic_store(&a->journal.consumed,1);atomic_store(&c->journal.consumed,1);LIFE(0,7,0);assert(!a->ctx.retiring_roots);}
    }
    for(unsigned k=0;k<3;k++)if(life_vectors[k]){assert(fclose(life_vectors[k])==0);life_vectors[k]=NULL;}
    printf("root lifetime %u / clear %u / leased scan %u cases PASS\n",life_cases[0],life_cases[1],life_cases[2]);
    free(a);free(c);
#undef LIFE
#undef SELECT
}
#endif
