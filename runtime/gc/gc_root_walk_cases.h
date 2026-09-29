#ifndef KLASSIC_GC_ROOT_WALK_CASES_H
#define KLASSIC_GC_ROOT_WALK_CASES_H
#include "gc_root_walk.h"
#include "gc_root_lifetime_cases.h"
typedef uint64_t (*RootWalker)(RootRegistry *,RootWalk *,uint64_t);
typedef uint64_t (*RootWalkScanner)(RootWalkScan *);
typedef struct {
    RootRegistry g; MutatorContext ctx; RootRegistration r[4]; RootSegment s[4];
    RootWalk w; RootWalkScan q; RootLease worker; RootLifeRequest life;
    BufferCursor buffer; RootChange entries[8]; uint64_t output[257]; RootCell cells[306];
} WalkFixture;
#define WALK_PTR(i) offsetof(WalkFixture,r[i].segment),offsetof(WalkFixture,r[i].previous),offsetof(WalkFixture,r[i].next), \
    offsetof(WalkFixture,r[i].inventory_previous),offsetof(WalkFixture,r[i].inventory_next),offsetof(WalkFixture,s[i].cells), \
    offsetof(WalkFixture,s[i].registration),offsetof(WalkFixture,s[i].journal), \
    offsetof(WalkFixture,r[i].context),offsetof(WalkFixture,r[i].owner_previous),offsetof(WalkFixture,r[i].owner_next)
static const uint64_t walk_pointers[]={WALK_PTR(0),WALK_PTR(1),WALK_PTR(2),WALK_PTR(3),
    offsetof(WalkFixture,ctx.root_segments),
    offsetof(WalkFixture,g.head),offsetof(WalkFixture,g.inventory),offsetof(WalkFixture,w.registry),offsetof(WalkFixture,w.lease.registration),
    offsetof(WalkFixture,q.walk),offsetof(WalkFixture,q.output),offsetof(WalkFixture,worker.registration),
    offsetof(WalkFixture,life.registration),offsetof(WalkFixture,life.lease),offsetof(WalkFixture,life.context),offsetof(WalkFixture,buffer.entries)};
#undef WALK_PTR
static FILE *walk_vectors[2];static unsigned walk_cases[2];
static void walk_fixture(WalkFixture *f,uint64_t phase){
    memset(f,0,sizeof(*f));f->g.phase=phase;f->ctx.abi_version=GC_ABI_VERSION;f->ctx.thread_id=17;atomic_store(&f->ctx.adopted_phase,phase);
    f->buffer.entries=(uintptr_t)f->entries;f->buffer.capacity=8;
    for(unsigned i=0;i<4;i++){f->r[i].segment=(uintptr_t)&f->s[i];f->s[i].owner=17;f->s[i].cells=(uintptr_t)(f->cells+(i?298+2*i:0));
        f->s[i].capacity=i?2:300;f->s[i].journal=(uintptr_t)&f->buffer;}
    f->q=(RootWalkScan){(uintptr_t)&f->w,0,(uintptr_t)f->output,256,999};
    f->life.context=(uintptr_t)&f->ctx;f->life.lease=(uintptr_t)&f->worker;
    memset(f->output,0xab,sizeof(f->output));
}
static uint64_t walk_normalize(WalkFixture *dst,const WalkFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(walk_pointers)/8;i++){
        unsigned char *p=(unsigned char *)dst+walk_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void walk_copy(WalkFixture *dst,const WalkFixture *src){
    memcpy(dst,src,sizeof(*dst));uint64_t mask=walk_normalize(dst,src);
    for(unsigned i=0;i<sizeof(walk_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+walk_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void walk_compare(WalkFixture *a,WalkFixture *c,unsigned kind,uint64_t op,RootWalker step,RootWalkScanner scan,uint64_t expected){
    WalkFixture before_a,before_c,x,y;memcpy(&before_a,a,sizeof(*a));memcpy(&before_c,c,sizeof(*c));
    uint64_t result=kind?scan(&a->q):step(&a->g,&a->w,op);
    uint64_t reference=kind?gc_root_walk_scan(&c->q):gc_root_walk_locked(&c->g,&c->w,op);
    if(result!=expected||reference!=expected){fprintf(stderr,"walk kind %u op %llu case %u got %llu C %llu expected %llu\n",kind,(unsigned long long)op,walk_cases[kind],(unsigned long long)result,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(result==1||(!kind&&(result==3||result==4))){assert(!memcmp(a,&before_a,sizeof(*a)));assert(!memcmp(c,&before_c,sizeof(*c)));}
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(walk_normalize(&x,a)==walk_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(walk_vectors[kind]){memcpy(&x,&before_c,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t rec[]={op,reference,walk_normalize(&x,c),walk_normalize(&y,c)};
        assert(fwrite(rec,sizeof(rec),1,walk_vectors[kind])==1);assert(fwrite(&x,sizeof(x),1,walk_vectors[kind])==1);assert(fwrite(&y,sizeof(y),1,walk_vectors[kind])==1);}
    walk_cases[kind]++;
}
static void walk_select(WalkFixture *f,unsigned i){f->life.registration=(uintptr_t)&f->r[i];f->life.scope=f->r[i].scope;}
static uint64_t walk_value(unsigned segment,uint64_t cell){return ((segment*300+cell+1)<<20)|0x1510;}
static void walk_attach(WalkFixture *f,unsigned i,unsigned used,RootLife life){
    walk_select(f,i);assert(life(&f->g,&f->life,GC_RL_ATTACH)==0);
    // Prepared fixture roots before discovery; no concurrent observer exists yet.
    RootCell *cells=(RootCell *)(uintptr_t)f->s[i].cells;
    for(unsigned j=0;j<used;j++){cells[j].active=1;cells[j].reuse_generation++;atomic_store(&cells[j].value,walk_value(i,j));}
    atomic_store_explicit(&f->s[i].used,used,memory_order_release);
}
static void walk_retire(WalkFixture *f,unsigned i,RootLife life,RootClear clear,RootLeasedScan scan,bool held){
    walk_select(f,i);assert(life(&f->g,&f->life,GC_RL_RETIRE)==0);
    while(f->r[i].cursor<f->r[i].limit){
        assert(life(&f->g,&f->life,GC_RL_CLAIM)==0);
        RootScanJob job={.scan={f->r[i].segment,f->life.begin,(uintptr_t)f->output,f->life.end-f->life.begin,0},
            .lease=(uintptr_t)&f->worker,.serial=f->life.serial};
        assert(scan(&job)==(f->life.end==f->r[i].limit?0u:2u));
        for(uint64_t j=0;j<job.scan.written;j++)assert(f->output[j]==walk_value(i,f->life.begin+j));
        assert(life(&f->g,&f->life,GC_RL_ACK)==0);
    }
    assert(life(&f->g,&f->life,GC_RL_CLEAN)==(held?GC_RL_WAIT:0u));
    if(!held){while(clear(&f->life)==GC_RL_WAIT){}assert(life(&f->g,&f->life,GC_RL_FINISH)==0);}
}
static void gc_run_root_walk_cases(RootWalker step,RootWalkScanner scan,RootLife life,RootClear clear,RootLeasedScan leased){
    WalkFixture *a=malloc(sizeof(*a)),*c=malloc(sizeof(*c));assert(a&&c);
    const char *names[]={"KLASSIC_GC_ROOT_WALK_VECTORS","KLASSIC_GC_ROOT_WALK_SCAN_VECTORS"};const char *magic[]={"KLRWLK01","KLRWSC01"};
    for(unsigned k=0;k<2;k++){walk_cases[k]=0;const char *path=getenv(names[k]);if(path){walk_vectors[k]=fopen(path,"wb");assert(walk_vectors[k]);
        assert(fwrite(magic[k],8,1,walk_vectors[k])==1);uint64_t h[]={sizeof(*a),k?offsetof(WalkFixture,q):offsetof(WalkFixture,g),offsetof(WalkFixture,w),sizeof(walk_pointers)/8};
        assert(fwrite(h,sizeof(h),1,walk_vectors[k])==1);assert(fwrite(walk_pointers,sizeof(walk_pointers),1,walk_vectors[k])==1);}}
#define WALK(k,op,result) walk_compare(a,c,k,op,step,scan,result)
    const unsigned sizes[]={0,1,255,256,300},budgets[]={1,17,256};
    for(unsigned yp=0;yp<8;yp++)for(unsigned old=0;old<8;old++)for(unsigned size=0;size<5;size++){
        walk_fixture(a,yp|(old<<4));walk_copy(c,a);WALK(0,0,2);assert(!a->g.walkers&&!a->w.active);
        walk_attach(a,0,sizes[size],life);walk_attach(c,0,sizes[size],gc_root_life_locked);
        WALK(0,0,0);assert(a->g.walkers==1&&a->g.leases==1&&a->w.limit==sizes[size]);
        assert(a->w.mask==((yp>=1&&yp<=3?1u:0u)|(old>=1&&old<=3?2u:0u)));
        if(sizes[size])WALK(0,1,4);
        a->q.serial=c->q.serial=a->w.lease.serial;a->q.capacity=c->q.capacity=budgets[size%3];
        while(a->w.cursor<a->w.limit){uint64_t begin=a->w.cursor,n=a->w.limit-begin;if(n>a->q.capacity)n=a->q.capacity;
            WALK(1,0,begin+n==a->w.limit?0:2);assert(a->q.written==n);
            for(uint64_t j=0;j<n;j++)assert(a->output[j]==walk_value(0,begin+j));
            assert(a->output[a->q.capacity]==UINT64_C(0xabababababababab));
        }
        WALK(1,0,0);assert(!a->q.written);a->w.completed_mask=c->w.completed_mask=a->w.mask^1;WALK(0,1,4);
        a->w.completed_mask=c->w.completed_mask=a->w.mask;WALK(0,1,2);assert(!a->g.walkers&&!a->g.leases);
        WALK(1,0,1);WALK(0,1,1);
    }
    // Retire an unseen middle node, then reuse its slot at the new list head.
    // Current cursor stays pinned, old cohort order remains 2 -> 0.
    for(unsigned retired=0;retired<3;retired++){
        walk_fixture(a,0x22);walk_copy(c,a);
        for(unsigned i=0;i<3;i++){walk_attach(a,i,i?2:1,life);walk_attach(c,i,i?2:1,gc_root_life_locked);}
        WALK(0,0,0);uint64_t first_serial=a->w.lease.serial;
        walk_retire(a,retired,life,clear,leased,retired==2);walk_retire(c,retired,gc_root_life_locked,gc_root_clear_chunk,gc_root_scan_leased,retired==2);
        if(retired!=2){walk_attach(a,retired,0,life);walk_attach(c,retired,0,gc_root_life_locked);}
        walk_attach(a,3,0,life);walk_attach(c,3,0,gc_root_life_locked);
        // New roots after BEGIN are recorded through the real storage journal.
        for(unsigned variant=0;variant<2;variant++){WalkFixture *f=variant?c:a;
            RootRequest q={.segment=(uintptr_t)&f->s[3]};assert(gc_root_step(&f->ctx,&q,0)==0);q.value=walk_value(3,0);assert(gc_root_step(&f->ctx,&q,1)==0);
            assert(f->entries[0].previous==0&&f->entries[0].next==q.value);
        }
        unsigned seen=0;
        while(a->w.active){RootRegistration *r=(RootRegistration *)(uintptr_t)a->w.lease.registration;unsigned i=(unsigned)(r-a->r);
            assert(i<3&&(i!=retired||retired==2));seen|=1u<<i;
            a->q.serial=c->q.serial=a->w.lease.serial;a->q.capacity=c->q.capacity=256;WALK(1,0,0);
            for(uint64_t j=0;j<a->q.written;j++)assert(a->output[j]==walk_value(i,j));
            a->w.completed_mask=c->w.completed_mask=a->w.mask;
            bool next=r->inventory_next!=0;WALK(0,1,next?0:2);
            a->q.serial=c->q.serial=first_serial;WALK(1,0,1);
        }
        assert(seen==(retired==2?7u:7u^(1u<<retired)));
        if(retired==2){walk_select(a,2);walk_select(c,2);assert(life(&a->g,&a->life,6)==0);assert(gc_root_life_locked(&c->g,&c->life,6)==0);
            assert(clear(&a->life)==0&&gc_root_clear_chunk(&c->life)==0);
            assert(life(&a->g,&a->life,7)==GC_RL_WAIT&&gc_root_life_locked(&c->g,&c->life,7)==GC_RL_WAIT);
            for(unsigned v=0;v<2;v++){WalkFixture *f=v?c:a;RootChange change;
                RootDrain drain={(uintptr_t)&f->buffer,(uintptr_t)&change,1,0};assert(gc_root_drain(&drain)==0&&drain.written==1);
                assert(change.next==walk_value(3,0));}
            assert(life(&a->g,&a->life,7)==0&&gc_root_life_locked(&c->g,&c->life,7)==0);}
        assert(!a->g.walkers&&!a->g.leases);
    }
    // The captured prefix does not grow while the owner appends another root.
    walk_fixture(a,0x22);walk_attach(a,0,1,life);walk_copy(c,a);WALK(0,0,0);
    for(unsigned v=0;v<2;v++){WalkFixture *f=v?c:a;RootRequest q={.segment=(uintptr_t)&f->s[0]};assert(gc_root_step(&f->ctx,&q,0)==0);q.value=walk_value(0,1);assert(gc_root_step(&f->ctx,&q,1)==0);}
    a->q.serial=c->q.serial=a->w.lease.serial;WALK(1,0,0);assert(a->q.written==1&&a->w.limit==1&&atomic_load(&a->s[0].used)==2);
    assert(a->entries[0].next==walk_value(0,1));
    for(unsigned fault=0;fault<16;fault++){
        walk_fixture(a,0);walk_attach(a,0,1,life);uint64_t result=1;
        switch(fault){case 0:a->w.active=1;break;case 1:a->w.lease.kind=1;break;case 2:a->g.phase=8;break;case 3:a->g.phase=0x80;break;
        case 4:a->g.inventory++;break;case 5:a->r[0].registered=0;break;case 6:a->r[0].scope=0;break;case 7:a->r[0].inventory_previous=(uintptr_t)&a->r[1];break;
        case 8:a->r[0].segment=0;break;case 9:atomic_store(&a->s[0].state,3);break;case 10:a->s[0].registration=0;break;
        case 11:atomic_store(&a->s[0].used,301);break;case 12:a->g.last_lease=UINT64_MAX;result=3;break;
        case 13:a->g.leases=UINT64_MAX;result=3;break;case 14:a->g.walkers=UINT64_MAX;result=3;break;case 15:a->s[0].cells=UINT64_MAX-7;break;}
        walk_copy(c,a);WALK(0,0,result);
    }
    for(unsigned fault=0;fault<12;fault++){
        walk_fixture(a,0);walk_attach(a,0,1,life);assert(step(&a->g,&a->w,0)==0);a->q.serial=a->w.lease.serial;
        switch(fault){case 0:a->q.walk=0;break;case 1:a->q.walk++;break;case 2:a->q.output=0;break;case 3:a->q.output++;break;
        case 4:a->q.capacity=0;break;case 5:a->q.capacity=257;break;case 6:a->q.output=UINT64_MAX-7;break;
        case 7:a->q.serial++;break;case 8:a->w.cursor=2;break;case 9:a->w.limit=2;break;case 10:a->s[0].scope++;break;case 11:atomic_store(&a->s[0].state,3);break;}
        walk_copy(c,a);WALK(1,0,1);
    }
    // Pending snapshots, including malformed leftover holders, cannot be
    // bypassed through another scanner, NEXT or a fresh BEGIN.
    for(unsigned field=0;field<4;field++){
        walk_fixture(a,0x22);walk_attach(a,0,1,life);assert(step(&a->g,&a->w,0)==0);
        a->q.serial=a->w.lease.serial;a->w.cursor=a->w.limit;a->w.completed_mask=a->w.mask;
        if(field==0)a->w.pending=1;if(field==1)a->w.pending_reference=walk_value(0,0);
        if(field==2)a->w.pending_observed=walk_value(0,0);if(field==3)a->w.pending_index=1;
        walk_copy(c,a);WALK(0,1,4);WALK(1,0,1);
        a->w.active=0;a->w.lease.kind=0;walk_copy(c,a);WALK(0,0,1);
    }
    for(unsigned k=0;k<2;k++)if(walk_vectors[k]){assert(fclose(walk_vectors[k])==0);walk_vectors[k]=NULL;}
    printf("root inventory walk %u / captured-prefix scan %u cases PASS\n",walk_cases[0],walk_cases[1]);free(a);free(c);
#undef WALK
}
#endif
