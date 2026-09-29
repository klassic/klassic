#ifndef KLASSIC_GC_ROOT_WALK_HEAL_CASES_H
#define KLASSIC_GC_ROOT_WALK_HEAL_CASES_H
#include "gc_root_walk_concurrency.h"
typedef uint64_t (*RootWalkHealer)(RootWalkHeal *);
typedef struct {RootWalkHeal q;RootWalk w;RootRegistration r;RootSegment s;RootCell cells[4];} WalkHealFixture;
static const uint64_t walk_heal_pointers[]={offsetof(WalkHealFixture,q.walk),offsetof(WalkHealFixture,w.lease.registration),
    offsetof(WalkHealFixture,r.segment),offsetof(WalkHealFixture,s.registration),offsetof(WalkHealFixture,s.cells)};
static unsigned walk_heal_cases;static FILE *walk_heal_vectors;
static uint64_t walk_heal_normalize(WalkHealFixture *dst,const WalkHealFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(walk_heal_pointers)/8;i++){
        uint64_t *p=(uint64_t *)((unsigned char *)dst+walk_heal_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}}
    return mask;
}
static void walk_heal_fixture(WalkHealFixture *f,unsigned epoch,unsigned index){
    memset(f,0,sizeof(*f));uint64_t color=gc_abi_color((PhaseSnapshot){epoch<<8});
    f->q=(RootWalkHeal){(uintptr_t)&f->w,7,index,((uint64_t)(index+1)<<20)|color,(UINT64_C(0x10000)<<16)|color};
    f->w=(RootWalk){.lease={(uintptr_t)&f->r,9,7,1,0},.cursor=4,.limit=4,.active=1};
    f->r=(RootRegistration){.segment=(uintptr_t)&f->s,.scope=9,.registered=1,.leases=1};
    f->s=(RootSegment){.owner=17,.scope=9,.cells=(uintptr_t)f->cells,.capacity=4,.used=4,.state=1,.registration=(uintptr_t)&f->r};
    for(unsigned i=0;i<4;i++){f->cells[i].active=1;f->cells[i].reuse_generation=5;atomic_store(&f->cells[i].value,((uint64_t)(i+1)<<20)|color);}
}
static void walk_heal_compare(WalkHealFixture *a,RootWalkHealer heal,uint64_t status){
    WalkHealFixture c,before,expected,x,y;memcpy(&c,a,sizeof(c));uint64_t mask=walk_heal_normalize(&c,a);
    for(unsigned i=0;i<sizeof(walk_heal_pointers)/8;i++)if(mask&(UINT64_C(1)<<i))
        *(uint64_t *)((unsigned char *)&c+walk_heal_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&expected,&c,sizeof(c));
    if(status==GC_RWH_OK)atomic_store(&expected.cells[c.q.index].value,c.q.desired);
    uint64_t actual=heal(&a->q),reference=gc_root_walk_heal(&c.q);
    if(actual!=status||reference!=status){fprintf(stderr,"root heal case %u got %llu C %llu expected %llu\n",walk_heal_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)status);abort();}
    assert(!memcmp(&expected,&c,sizeof(c)));memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(walk_heal_normalize(&x,a)==walk_heal_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(walk_heal_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,walk_heal_normalize(&x,&c),walk_heal_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,walk_heal_vectors)==1);assert(fwrite(&x,sizeof(x),1,walk_heal_vectors)==1);assert(fwrite(&y,sizeof(y),1,walk_heal_vectors)==1);}
    walk_heal_cases++;
}
static void gc_run_root_walk_heal_cases(RootWalkHealer heal){
    WalkHealFixture f;walk_heal_cases=0;const char *path=getenv("KLASSIC_GC_ROOT_WALK_HEAL_VECTORS");
    if(path){walk_heal_vectors=fopen(path,"wb");assert(walk_heal_vectors);assert(fwrite("KLRWHL01",8,1,walk_heal_vectors)==1);
        uint64_t header[]={sizeof(f),offsetof(WalkHealFixture,q),0,sizeof(walk_heal_pointers)/8};
        assert(fwrite(header,sizeof(header),1,walk_heal_vectors)==1);assert(fwrite(walk_heal_pointers,sizeof(walk_heal_pointers),1,walk_heal_vectors)==1);}
    for(unsigned epoch=0;epoch<32;epoch++)for(unsigned state=1;state<=2;state++)for(unsigned index=0;index<4;index++)for(unsigned mode=0;mode<3;mode++){
        walk_heal_fixture(&f,epoch,index);atomic_store(&f.s.state,state);
        if(mode==1)atomic_store(&f.cells[index].value,f.q.desired);
        if(mode==2){atomic_store(&f.cells[index].value,0);f.cells[index].active=0;}
        walk_heal_compare(&f,heal,mode?GC_RWH_CHANGED:GC_RWH_OK);
    }
    for(unsigned fault=0;fault<32;fault++){
        walk_heal_fixture(&f,0,0);
        switch(fault){case 0:f.q.walk=0;break;case 1:f.q.walk++;break;case 2:f.q.expected=0;break;
        case 3:f.q.desired=0;break;case 4:f.q.desired^=0x300;break;case 5:f.q.expected|=1;break;
        case 6:f.q.desired|=UINT64_C(1)<<16;break;case 7:f.q.expected&=0xffff;break;case 8:f.q.desired&=0xffff;break;
        case 9:f.w.active=0;break;case 10:f.w.lease.kind=2;break;case 11:f.q.serial=0;break;
        case 12:f.q.serial++;break;case 13:f.w.lease.registration=0;break;case 14:f.w.lease.registration++;break;
        case 15:f.w.cursor=5;break;case 16:f.q.index=4;break;case 17:f.q.index=UINT64_MAX;break;
        case 18:f.r.registered=0;break;case 19:f.r.scope++;break;case 20:f.r.segment=0;break;
        case 21:f.r.segment++;break;case 22:f.s.owner=0;break;case 23:f.s.cells=0;break;
        case 24:f.s.cells++;break;case 25:f.s.capacity=UINT64_MAX;break;case 26:atomic_store(&f.s.state,3);break;
        case 27:f.s.scope++;break;case 28:f.s.registration=0;break;case 29:atomic_store(&f.s.used,5);break;
        case 30:atomic_store(&f.s.used,3);break;case 31:f.s.cells=UINT64_MAX-7;break;}
        walk_heal_compare(&f,heal,GC_RWH_INVALID);
    }
    for(unsigned index=0;index<4;index++){walk_heal_fixture(&f,0,index);f.q.desired=f.q.expected;walk_heal_compare(&f,heal,GC_RWH_OK);}
    assert(walk_heal_cases==804);if(walk_heal_vectors){assert(fclose(walk_heal_vectors)==0);walk_heal_vectors=NULL;}
    puts("root walk heal 804 C/candidate exact-CAS, lease authentication and color preservation cases PASS");
}

typedef struct {WalkFixture f;RootLifetimeRace sync;RootWalker walk;RootWalkScanner scan;RootWalkHealer heal;uint64_t result;} WalkHealRace;
#ifdef _WIN32
static DWORD WINAPI walk_heal_reader(void *arg)
#else
static void *walk_heal_reader(void *arg)
#endif
{
    WalkHealRace *r=arg;WalkFixture *f=&r->f;
    life_lock(&r->sync);assert(r->walk(&f->g,&f->w,GC_RW_BEGIN)==GC_RW_ITEM);life_unlock(&r->sync);
    uint64_t observed=0;RootWalkScan scan={(uintptr_t)&f->w,f->w.lease.serial,(uintptr_t)&observed,1,0};
    assert(r->scan(&scan)==GC_ROOT_OK&&scan.written==1&&observed==walk_value(0,0));
    RootWalkHeal q={(uintptr_t)&f->w,scan.serial,0,observed,walk_value(1,0)};
    atomic_store_explicit(&r->sync.ready,1,memory_order_release);
    while(!atomic_load_explicit(&r->sync.resume,memory_order_acquire))root_yield();
    r->result=r->heal(&q);f->w.completed_mask=f->w.mask;
    life_lock(&r->sync);assert(r->walk(&f->g,&f->w,GC_RW_NEXT)==GC_RW_DONE);life_unlock(&r->sync);
    assert(r->heal(&q)==GC_RWH_INVALID);return 0;
}
static void gc_run_root_walk_heal_races(RootWalkHealer heal,RootWalker walk,RootWalkScanner scan,RootLife life){
    WalkHealRace *r=calloc(1,sizeof(*r));assert(r);r->heal=heal;r->walk=walk;r->scan=scan;
#ifdef _WIN32
    InitializeCriticalSection(&r->sync.lock);
#else
    assert(pthread_mutex_init(&r->sync.lock,NULL)==0);
#endif
    for(unsigned round=0;round<64;round++){
        WalkFixture *f=&r->f;walk_fixture(f,0x22);walk_attach(f,0,1,life);
        atomic_store(&r->sync.ready,0);atomic_store(&r->sync.resume,0);RootThread reader;
#ifdef _WIN32
        reader=CreateThread(NULL,0,walk_heal_reader,r,0,NULL);assert(reader);
#else
        assert(pthread_create(&reader,NULL,walk_heal_reader,r)==0);
#endif
        while(!atomic_load_explicit(&r->sync.ready,memory_order_acquire))root_yield();
        unsigned mode=round%4;uint64_t value=mode==1?walk_value(2,0):mode==2?0:walk_value(0,0);
        if(mode){RootRequest q={.segment=(uintptr_t)&f->s[0],.handle={(uintptr_t)&f->cells[0],1,f->s[0].scope},.value=value};
            assert(gc_root_step(&f->ctx,&q,mode==2?GC_ROOT_RELEASE:GC_ROOT_STORE)==GC_ROOT_OK);}
        uint64_t revision=atomic_load(&f->ctx.root_revision);
        atomic_store_explicit(&r->sync.resume,1,memory_order_release);
#ifdef _WIN32
        assert(WaitForSingleObject(reader,30000)==WAIT_OBJECT_0);assert(CloseHandle(reader));
#else
        assert(pthread_join(reader,NULL)==0);
#endif
        bool changed=mode==1||mode==2;assert(r->result==(changed?GC_RWH_CHANGED:GC_RWH_OK));
        assert(atomic_load(&f->cells[0].value)==(changed?value:walk_value(1,0)));
        assert(atomic_load(&f->ctx.root_revision)==revision&&revision==(mode!=0));
        assert(atomic_load(&f->buffer.published)==(mode!=0));
        if(mode)assert(f->entries[0].previous==walk_value(0,0)&&f->entries[0].next==value);
        assert(!f->g.leases&&!f->g.walkers);
    }
#ifdef _WIN32
    DeleteCriticalSection(&r->sync.lock);
#else
    assert(pthread_mutex_destroy(&r->sync.lock)==0);
#endif
    free(r);puts("root walk heal: 64 scanner/owner store-release rounds preserve journals and logical revisions PASS");
}
#endif
