#ifndef KLASSIC_GC_REMEMBERED_LEASE_CASES_H
#define KLASSIC_GC_REMEMBERED_LEASE_CASES_H
#include "gc_remembered_lease.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RememberedAcquirer)(MetadataLeasePool *,RememberedLease *);
typedef struct {
    MetadataLeasePool g;RememberedLease q;HeapPool h;AllocationPage pages[8];MetadataLeaseSlot slots[2];
    RememberedInventory inventory;RememberedRegistration registration;RememberedSet set;RememberedJob job;uint64_t bits[2][1024];
} RememberedLeaseFixture;
static const uint64_t rlease_pointers[]={offsetof(RememberedLeaseFixture,g.heap),offsetof(RememberedLeaseFixture,g.slots),
    offsetof(RememberedLeaseFixture,q.registration),offsetof(RememberedLeaseFixture,q.claim),offsetof(RememberedLeaseFixture,q.lease.page),
    offsetof(RememberedLeaseFixture,h.pages),offsetof(RememberedLeaseFixture,slots[0].page),offsetof(RememberedLeaseFixture,slots[1].page),
    offsetof(RememberedLeaseFixture,inventory.head),offsetof(RememberedLeaseFixture,registration.registry),offsetof(RememberedLeaseFixture,registration.set),
    offsetof(RememberedLeaseFixture,registration.previous),offsetof(RememberedLeaseFixture,registration.next),offsetof(RememberedLeaseFixture,set.bits[0]),offsetof(RememberedLeaseFixture,set.bits[1])};
static unsigned rlease_cases;static FILE *rlease_vectors;
static uint64_t rlease_normalize(RememberedLeaseFixture *d,const RememberedLeaseFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<sizeof(rlease_pointers)/8;i++){unsigned char *p=(unsigned char *)d+rlease_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}}return mask;}
static void rlease_clone(RememberedLeaseFixture *d,const RememberedLeaseFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=rlease_normalize(d,s);
    for(unsigned i=0;i<sizeof(rlease_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+rlease_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void rlease_fixture(RememberedLeaseFixture *f,unsigned head,unsigned run,unsigned closed,uint64_t offset){
    memset(f,0,sizeof(*f));f->g=(MetadataLeasePool){(uintptr_t)&f->h,(uintptr_t)f->slots,2,1,0};f->slots[0].next_free=2;
    f->h=(HeapPool){.pages=(uintptr_t)f->pages,.page_count=8,.heap_base=UINT64_C(0x100000000)};
    AllocationPage *p=&f->pages[head];p->heap_base=f->h.heap_base;p->start=(head+1)*GC_HEAP_PAGE_BYTES;
    p->limit=(head+run+1)*GC_HEAP_PAGE_BYTES;p->run_head=head;p->incarnation=27;p->identity_base=1024;p->generation=GC_OLD;
    atomic_store(&p->state,closed?GC_PAGE_CLOSED:GC_PAGE_ALLOCATING);
    // No object starts or heap mapping are supplied: this is metadata binding,
    // not owner lookup, descriptor validation or a strong-reference operation.
    f->inventory=(RememberedInventory){.epoch=0x10000,.last_phase=0x10000};
    f->set=(RememberedSet){.start=p->start,.limit=p->limit,.incarnation=p->incarnation,.bits={(uintptr_t)f->bits[0],(uintptr_t)f->bits[1]},
        .words=run*256,.epoch=0x10000,.last_phase=0x10000};f->registration.set=(uintptr_t)&f->set;
    RememberedInventoryRequest q={(uintptr_t)&f->registration,(uintptr_t)&f->job,0x21101};
    assert(gc_remembered_inventory_step_locked(&f->inventory,&q,GC_RI_ATTACH)==GC_RS_OK);
    f->job=(RememberedJob){.offset=offset,.incarnation=p->incarnation,.phase=0x10000,.budget=256};
    assert(gc_remembered_inventory_step_locked(&f->inventory,&q,GC_RI_RECORD)==GC_RS_OK);
    assert(gc_remembered_inventory_step_locked(&f->inventory,&q,GC_RI_BEGIN)==GC_RS_OK);f->job.phase=q.phase;
    assert(gc_remembered_inventory_step_locked(&f->inventory,&q,GC_RI_FLIP)==GC_RS_OK);
    uint64_t status;do{status=gc_remembered_inventory_step_locked(&f->inventory,&q,GC_RI_TAKE);}while(status==GC_RS_MORE);assert(status==GC_RS_OK);
    f->q=(RememberedLease){.registration=(uintptr_t)&f->registration,.claim=(uintptr_t)&f->job};
}
static void rlease_compare(RememberedLeaseFixture *a,RememberedAcquirer acquire,uint64_t expected){
    RememberedLeaseFixture c,ba,bc,x,y;rlease_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t got=acquire(&a->g,&a->q),reference=gc_remembered_lease_locked(&c.g,&c.q);
    if(got!=expected||reference!=expected){fprintf(stderr,"remembered lease case %u got %llu C %llu expected %llu\n",rlease_cases,(unsigned long long)got,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(got)assert(!memcmp(&ba,a,sizeof(ba))&&!memcmp(&bc,&c,sizeof(bc)));
    else{assert(!memcmp(&a->inventory,&ba.inventory,sizeof(a->inventory))&&!memcmp(&a->job,&ba.job,sizeof(a->job))&&!memcmp(a->bits,ba.bits,sizeof(a->bits)));}
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(rlease_normalize(&x,a)==rlease_normalize(&y,&c)&&!memcmp(&x,&y,sizeof(x)));
    if(rlease_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,rlease_normalize(&x,&c),rlease_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,rlease_vectors)==1&&fwrite(&x,sizeof(x),1,rlease_vectors)==1&&fwrite(&y,sizeof(y),1,rlease_vectors)==1);}rlease_cases++;
}
static void gc_run_remembered_lease_cases(RememberedAcquirer acquire){
    RememberedLeaseFixture a;rlease_cases=0;const char *path=getenv("KLASSIC_GC_REMEMBERED_LEASE_VECTORS");
    if(path){rlease_vectors=fopen(path,"wb");assert(rlease_vectors);assert(fwrite("KLRMLS01",8,1,rlease_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(RememberedLeaseFixture,g),offsetof(RememberedLeaseFixture,q),sizeof(rlease_pointers)/8};
        assert(fwrite(header,sizeof(header),1,rlease_vectors)==1&&fwrite(rlease_pointers,sizeof(rlease_pointers),1,rlease_vectors)==1);}
    for(unsigned head=0;head<5;head++)for(unsigned run=1;run<=4;run++)for(unsigned page=head;page<head+run;page++)for(unsigned closed=0;closed<2;closed++)for(unsigned position=0;position<4;position++){
        uint64_t within=position==0?0:position==1?8:position==2?32:GC_HEAP_PAGE_BYTES-8;
        rlease_fixture(&a,head,run,closed,(page+1)*GC_HEAP_PAGE_BYTES+within);rlease_compare(&a,acquire,GC_ML_OK);
        assert(a.q.lease.page==(uintptr_t)&a.pages[head]&&a.q.lease.incarnation==27&&a.pages[head].metadata_leases==1&&a.g.active==1);
        rlease_compare(&a,acquire,GC_ML_INVALID);
        assert(gc_metadata_step_locked(&a.g,&a.q.lease,GC_ML_RELEASE)==GC_ML_OK&&!a.g.active&&!a.pages[head].metadata_leases);
    }
    for(unsigned fault=0;fault<42;fault++){
        rlease_fixture(&a,1,3,1,3*GC_HEAP_PAGE_BYTES+40);AllocationPage *p=&a.pages[1];uint64_t expected=GC_ML_INVALID;
        switch(fault){
        case 0:a.q.registration=0;break;case 1:a.q.registration++;break;case 2:a.q.claim=0;break;case 3:a.q.claim++;break;
        case 4:a.q.lease.page=8;break;case 5:a.q.lease.incarnation=1;break;case 6:a.q.lease.token=1;break;
        case 7:a.registration.registry=0;break;case 8:a.registration.registry++;break;case 9:a.registration.identity=0;break;
        case 10:a.job.serial=0;break;case 11:a.job.serial++;break;case 12:a.job.offset+=8;break;
        case 13:a.job.phase++;break;case 14:a.job.incarnation++;break;case 15:a.g.heap=0;break;case 16:a.g.heap++;break;
        case 17:a.h.pages=0;break;case 18:a.h.pages++;break;case 19:a.h.page_count=0;break;case 20:a.h.page_count=UINT64_MAX;break;
        case 21:a.h.pages=UINT64_MAX-7;break;case 22:a.h.heap_base++;break;case 23:a.h.heap_base=UINT64_MAX-15;break;
        case 24:a.set.start+=8;break;case 25:a.set.limit-=8;break;case 26:a.h.page_count=3;break;
        case 27:p->run_head++;break;case 28:p->heap_base+=16;break;case 29:p->start+=16;break;case 30:p->limit-=16;break;
        case 31:p->generation=GC_YOUNG;break;case 32:p->incarnation++;break;case 33:p->identity_base=0;break;
        case 34:atomic_store(&p->state,GC_PAGE_READY);break;case 35:p->metadata_leases=UINT64_MAX;expected=GC_ML_EXHAUSTED;break;
        case 36:a.slots[0].generation=UINT32_MAX;expected=GC_ML_EXHAUSTED;break;
        case 37:a.g.active=2;a.g.free_head=0;expected=GC_ML_FULL;break;
        case 38:a.g.free_head=3;break;case 39:a.inventory.claims=0;break;case 40:a.registration.set=0;break;case 41:a.set.incarnation=0;break;}
        rlease_compare(&a,acquire,expected);
    }
    if(rlease_vectors){assert(fclose(rlease_vectors)==0);rlease_vectors=NULL;}
    printf("remembered lease %u C/candidate claim, run binding and preserved failure cases PASS\n",rlease_cases);
}
#endif
