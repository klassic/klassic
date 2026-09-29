#ifndef KLASSIC_GC_DETACH_CASES_H
#define KLASSIC_GC_DETACH_CASES_H
#include "gc_root_detach_concurrency.h"
typedef struct {RootLifeFixture f;HandshakeState h;ThreadRegistration p;DetachRequest q;} DetachFixture;
_Static_assert(sizeof(DetachFixture)<0x8000,"detach ARM replay arena");
static uint64_t detach_pointers[64];static unsigned detach_pointer_count,detach_cases;
static FILE *detach_vectors;
static void detach_fixture(DetachFixture *f,uint64_t phase,unsigned roots) {
    memset(f,0,sizeof(*f));life_fixture(&f->f,phase);
    f->h.head=(uintptr_t)&f->p;f->h.members=1;f->h.last_identity=17;
    f->p=(ThreadRegistration){.context=(uintptr_t)&f->f.ctx,.identity=17,.registered=1,.registry=(uintptr_t)&f->h};
    f->q=(DetachRequest){(uintptr_t)&f->f.g,(uintptr_t)&f->h,(uintptr_t)&f->p};
    for(unsigned i=0;i<roots;i++){life_select(&f->f,i,0);assert(gc_root_life_locked(&f->f.g,&f->f.q,0)==0);}
    atomic_store(&f->f.ctx.state,GC_DETACHING);
}
static uint64_t detach_normalize(DetachFixture *dst,const DetachFixture *src) {
    uint64_t mask=0;for(unsigned i=0;i<detach_pointer_count;i++){
        unsigned char *p=(unsigned char *)dst+detach_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void detach_copy(DetachFixture *dst,const DetachFixture *src) {
    memcpy(dst,src,sizeof(*dst));uint64_t mask=detach_normalize(dst,src);
    for(unsigned i=0;i<detach_pointer_count;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+detach_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void detach_compare(DetachFixture *a,DetachFixture *c,DetachProgress step,uint64_t expected) {
    DetachFixture before,x,y;memcpy(&before,c,sizeof(before));
    uint64_t actual=step(&a->q),reference=gc_detach_step_locked(&c->q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"detach case %u got %llu C %llu expected %llu\n",detach_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(expected>1)assert(!memcmp(&before,c,sizeof(before)));
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(detach_normalize(&x,a)==detach_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(detach_vectors){memcpy(&x,&before,sizeof(x));uint64_t record[]={0,reference,detach_normalize(&x,c),0};
        // Recompute the after-state relocation mask from the reference fixture.
        memcpy(&y,c,sizeof(y));record[3]=detach_normalize(&y,c);
        assert(fwrite(record,sizeof(record),1,detach_vectors)==1);assert(fwrite(&x,sizeof(x),1,detach_vectors)==1);assert(fwrite(&y,sizeof(y),1,detach_vectors)==1);}
    detach_cases++;
}
static void gc_run_detach_cases(DetachProgress step) {
    detach_pointer_count=sizeof(life_pointers)/8;memcpy(detach_pointers,life_pointers,sizeof(life_pointers));
    const uint64_t extra[]={offsetof(DetachFixture,h.head),offsetof(DetachFixture,p.registry),offsetof(DetachFixture,p.previous),offsetof(DetachFixture,p.next),offsetof(DetachFixture,p.context),offsetof(DetachFixture,q.roots),offsetof(DetachFixture,q.handshake),offsetof(DetachFixture,q.participant)};
    memcpy(detach_pointers+detach_pointer_count,extra,sizeof(extra));detach_pointer_count+=sizeof(extra)/8;assert(detach_pointer_count<64);
    DetachFixture *a=malloc(sizeof(*a)),*c=malloc(sizeof(*c));assert(a&&c);detach_cases=0;
    const char *path=getenv("KLASSIC_GC_DETACH_VECTORS");if(path){detach_vectors=fopen(path,"wb");assert(detach_vectors);
        assert(fwrite("KLDTCH01",8,1,detach_vectors)==1);uint64_t header[]={sizeof(*a),offsetof(DetachFixture,q),0,detach_pointer_count};
        assert(fwrite(header,sizeof(header),1,detach_vectors)==1);assert(fwrite(detach_pointers,detach_pointer_count*8,1,detach_vectors)==1);}
#define DT(status) detach_compare(a,c,step,status)
    for(unsigned young=0;young<8;young++)for(unsigned old=0;old<8;old++)for(unsigned roots=0;roots<=3;roots++) {
        detach_fixture(a,young|(old<<4),roots);detach_copy(c,a);
        for(unsigned i=0;i<roots;i++){DT(GC_DT_MORE);assert(a->f.ctx.retiring_roots==i+1);}
        if(roots)DT(GC_DT_WAIT);
        for(unsigned i=0;i<roots;i++)for(unsigned variant=0;variant<2;variant++){
            RootLifeFixture *f=variant?&c->f:&a->f;life_select(f,i,0);
            assert(gc_root_life_locked(&f->g,&f->q,6)==0);assert(gc_root_clear_chunk(&f->q)==0);assert(gc_root_life_locked(&f->g,&f->q,7)==0);
        }
        DT(GC_DT_DONE);assert(!a->h.members&&!a->p.context&&atomic_load(&a->f.ctx.state)==GC_DETACHED);
        DT(GC_DT_INVALID);
    }
    for(unsigned fault=0;fault<24;fault++) {
        detach_fixture(a,0x22,1);uint64_t expected=GC_DT_INVALID;
        switch(fault){case 0:a->q.roots=0;break;case 1:a->q.roots++;break;
        case 2:a->q.handshake=0;break;case 3:a->q.handshake++;break;
        case 4:a->q.participant=0;break;case 5:a->q.participant++;break;
        case 6:a->p.context=0;break;case 7:a->p.context++;break;
        case 8:atomic_store(&a->f.ctx.state,GC_RUNNING);break;
        case 9:a->p.registered=0;break;case 10:a->p.identity++;break;case 11:atomic_store(&a->f.ctx.access_epoch,1);break;
        case 12:a->f.ctx.tlab_top=16;expected=GC_DT_CLOSE_TLAB;break;
        case 13:a->f.ctx.satb.top=1;expected=GC_DT_FLUSH_SATB;break;
        case 14:a->f.ctx.remembered.top=1;expected=GC_DT_FLUSH_REMEMBERED;break;
        case 15:atomic_store(&a->f.ctx.satb.published,1);break;
        case 16:atomic_store(&a->f.ctx.remembered.consumed,1);break;
        case 17:atomic_store(&a->f.ctx.adopted_phase,0);expected=GC_DT_ADOPT_PHASE;break;
        case 18:a->f.ctx.root_segments++;break;
        case 19:a->f.ctx.retiring_roots=UINT64_MAX;expected=GC_DT_EXHAUSTED;break;
        case 20:a->f.r[0].context=0;break;
        case 21:a->h.active=2;break;case 22:a->h.native=2;break;
        case 23:a->p.acknowledged_generation=1;break;}
        detach_copy(c,a);DT(expected);
    }
    detach_fixture(a,0x22,1);detach_copy(c,a);
    for(unsigned round=0;round<3;round++) {
        assert(gc_handshake_step_locked(&a->h,NULL,GC_HS_BEGIN)==0);assert(gc_handshake_step_locked(&c->h,NULL,GC_HS_BEGIN)==0);
        DT(GC_DT_MORE);DT(GC_DT_WAIT);assert(a->f.ctx.root_segments&&a->f.ctx.retiring_roots==0);
        assert(gc_handshake_step_locked(&a->h,NULL,GC_HS_END)==0);assert(gc_handshake_step_locked(&c->h,NULL,GC_HS_END)==0);
    }
    DT(GC_DT_MORE);DT(GC_DT_WAIT);
    if(detach_vectors){assert(fclose(detach_vectors)==0);detach_vectors=NULL;}
    printf("detach progress %u C/candidate bounded-step cases PASS\n",detach_cases);free(a);free(c);
#undef DT
}
#endif
