/* Publication integration with source users admitted through the same gate.
 * This does not establish compiler-wide guard coverage or phase adoption. */
#ifndef KLASSIC_GC_FORWARD_CONCURRENCY_H
#define KLASSIC_GC_FORWARD_CONCURRENCY_H
#include "gc_forward_cases.h"
#include "gc_source_access_cases.h"
#include "gc_resolve_cases.h"
#include "gc_relocate_cases.h"
#include "gc_context.h"
#include "gc_scalar_cases.h"
#include "gc_metadata_concurrency.h"
typedef struct {
    ForwardCell cell;ForwardTable table;AllocationPage page;ForwardStep step;SourceAccess access;Resolver resolve;RelocateCopy copy;RelocatePublish publish;MarkRecord record;void *mapping;
    MutatorContext users[2];
    uint64_t payload,retain;
    ScalarAccessor scalar;DescriptorRegistry registry;TypeDescriptor descriptor;_Atomic uint64_t slots[2];
    uint64_t scalar_before[2],scalar_after[2];
    _Atomic uint64_t ready,go,half,finish,losers,cleaned,users_in,release_users,selected;
} ForwardRace;
typedef struct {ForwardRace *r;unsigned ticket;} ForwardActor;
#ifdef _WIN32
static DWORD WINAPI forward_source_user(void *argument)
#else
static void *forward_source_user(void *argument)
#endif
{
    ForwardActor *a=argument;ForwardRace *r=a->r;unsigned i=a->ticket-1;
    ResolveRequest q={.reference=(r->cell.source_offset<<16)|0x1510,.scope={.context=(uintptr_t)&r->users[i]}};
    uint64_t result;do{result=r->resolve(&r->page,&q,GC_RESOLVE_ENTER);if(result==GC_SA_WAIT)metadata_yield();}while(result==GC_SA_WAIT);assert(result==0);
    assert(gc_context_transition_locked(&r->users[i],GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_ACTIVE_ACCESS);
    atomic_fetch_add_explicit(&r->users_in,1,memory_order_release);
    // Controlled preemption point: deliberately keep the raw scope alive.
    // No GC poll, OS wait, yield or callback occurs inside this scope.
    while(!atomic_load_explicit(&r->release_users,memory_order_acquire)){}
    if(r->payload>=16){
        GuardedScalar value={(uintptr_t)&q.scope,(uintptr_t)&r->page,(uintptr_t)&r->registry,i*8,0,0};
        assert(r->scalar(&value,GC_SCALAR_LOAD)==GC_SCALAR_OK&&value.value==r->scalar_before[i]);
        value.value=r->scalar_after[i];
        assert(r->scalar(&value,GC_SCALAR_STORE)==GC_SCALAR_OK);
        value.value=0;
        assert(r->scalar(&value,GC_SCALAR_LOAD)==GC_SCALAR_OK&&value.value==r->scalar_after[i]);
        assert(!r->users[i].satb.top&&!r->users[i].remembered.top&&!atomic_load(&r->users[i].root_revision));
    }else if(r->payload>i)((unsigned char *)r->mapping)[r->cell.source_offset+i]=(unsigned char)(0xe0+i);
    do{result=r->resolve(&r->page,&q,GC_RESOLVE_EXIT);}while(result==GC_SA_WAIT);
    assert(result==0&&!q.scope.held&&!atomic_load(&r->users[i].access_epoch));
    return 0;
}
static void forward_copy_until(ForwardRace *r,RelocationCopy *copy,uint64_t end) {
    if(copy->stage&&copy->cursor==end)return;
    do {
        uint64_t n=end-copy->cursor;copy->budget=n&&n<GC_COPY_MAX_BYTES?n:GC_COPY_MAX_BYTES;
        uint64_t before=copy->cursor,status=r->copy(&r->cell,copy);
        assert(status==GC_COPY_DONE||status==GC_COPY_MORE);
        assert(copy->cursor>=before&&copy->cursor-before<=GC_COPY_MAX_BYTES&&copy->cursor<=end);
        if(status==GC_COPY_DONE)assert(copy->cursor==r->payload);
        metadata_yield(); // Both reservations remain protected across slices.
    }while(copy->cursor<end);
}
#ifdef _WIN32
static DWORD WINAPI forward_actor(void *argument)
#else
static void *forward_actor(void *argument)
#endif
{
    ForwardActor *a=argument;ForwardRace *r=a->r;
    unsigned char *candidate=(unsigned char *)r->mapping+(a->ticket+1)*GC_HEAP_PAGE_BYTES;
    pool_vm_commit(candidate,GC_HEAP_PAGE_BYTES);
    atomic_fetch_add_explicit(&r->ready,1,memory_order_release);
    while(!atomic_load_explicit(&r->go,memory_order_acquire))metadata_yield();
    ForwardRequest q={r->cell.source_offset,r->cell.incarnation,a->ticket,0};
    uint64_t result=r->step(&r->cell,&q,GC_FWD_CLAIM);
    if(result!=GC_FWD_OK){assert(result==GC_FWD_BUSY);pool_vm_decommit(candidate,GC_HEAP_PAGE_BYTES);atomic_fetch_add(&r->cleaned,1);atomic_fetch_add_explicit(&r->losers,1,memory_order_release);return 0;}
    SourceAccessRequest selection={0,r->cell.source_offset,r->cell.incarnation,0,a->ticket};
    do{result=r->access(&r->cell,&selection,GC_SA_SELECT);if(result==GC_SA_WAIT)metadata_yield();}while(result==GC_SA_WAIT);assert(result==0);
    atomic_store_explicit(&r->selected,1,memory_order_release);
    do{result=r->access(&r->cell,&selection,GC_SA_CHECK);if(result==GC_SA_WAIT)metadata_yield();}while(result==GC_SA_WAIT);assert(result==0);
    RelocationCopy copy={(uintptr_t)r->mapping,r->cell.incarnation,a->ticket,2*GC_HEAP_PAGE_BYTES,
        (a->ticket+1)*GC_HEAP_PAGE_BYTES+sizeof(ObjectHeader),(a->ticket+2)*GC_HEAP_PAGE_BYTES,GC_COPY_MAX_BYTES,0,0};
    forward_copy_until(r,&copy,r->payload/2);atomic_store_explicit(&r->half,1,memory_order_release);
    while(!atomic_load_explicit(&r->finish,memory_order_acquire))metadata_yield();
    if(r->retain){pool_vm_decommit(candidate,GC_HEAP_PAGE_BYTES);atomic_fetch_add(&r->cleaned,1);assert(r->step(&r->cell,&q,GC_FWD_RETAIN)==0);}
    else {
        forward_copy_until(r,&copy,r->payload);assert(copy.stage==2&&copy.cursor==r->payload);
        assert(r->publish(&r->cell,&copy,0x1510)==GC_COPY_DONE);
    }
    return 0;
}
static void gc_run_forward_races(ForwardStep step,SourceAccess access,Resolver resolve,RelocateCopy copy,RelocatePublish publish,ScalarAccessor scalar) {
    const uint64_t lengths[]={0,7,1025,16385};unsigned scalar_rounds=0;
    for(unsigned round=0;round<64;round++) {
        ForwardRace r={.step=step,.access=access,.resolve=resolve,.copy=copy,.publish=publish,.payload=lengths[round%4],.retain=round%5==0,.scalar=scalar};
        r.mapping=pool_vm_reserve(4*GC_HEAP_PAGE_BYTES);pool_vm_commit((char *)r.mapping+GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES);
        r.cell.source_offset=GC_HEAP_PAGE_BYTES+sizeof(ObjectHeader);r.cell.incarnation=round+1;r.record.object_id=42;r.record.old_epoch=9;
        r.table=(ForwardTable){(uintptr_t)&r.cell,1,1};
        r.page=(AllocationPage){.heap_base=(uintptr_t)r.mapping,.start=GC_HEAP_PAGE_BYTES,.limit=2*GC_HEAP_PAGE_BYTES,.incarnation=r.cell.incarnation,
            .state=GC_PAGE_CLOSED,.allocated_top=2*GC_HEAP_PAGE_BYTES,.forward_table=(uintptr_t)&r.table};
        ObjectHeader *source=(ObjectHeader *)((char *)r.mapping+GC_HEAP_PAGE_BYTES);
        source->payload_bytes=r.payload;source->descriptor_id=1;source->mark_record=(uintptr_t)&r.record;atomic_store(&source->state,GC_OBJECT_PUBLISHED);
        unsigned char *bytes=(unsigned char *)source+sizeof(*source);for(uint64_t i=0;i<r.payload;i++)bytes[i]=(unsigned char)(i*37+round);
        r.descriptor=(TypeDescriptor){.id=1,.alignment=16,.element_bytes=1};
        r.registry=(DescriptorRegistry){(uintptr_t)r.slots,2};atomic_store(&r.slots[1],(uintptr_t)&r.descriptor);
        if(r.payload>=16){
            memcpy(r.scalar_before,bytes,16);
            r.scalar_after[0]=UINT64_MAX;r.scalar_after[1]=(UINT64_C(0x301510)|((uint64_t)(round+1)<<32));
            scalar_rounds++;
        }
        MetadataThread workers[2],users[2];ForwardActor actors[2]={{&r,1},{&r,2}};
        for(unsigned i=0;i<2;i++){
            r.users[i]=(MutatorContext){.abi_version=GC_ABI_VERSION,.thread_id=i+1};
#ifdef _WIN32
            users[i]=CreateThread(NULL,0,forward_source_user,&actors[i],0,NULL);assert(users[i]);
#else
            assert(pthread_create(&users[i],NULL,forward_source_user,&actors[i])==0);
#endif
        }
        while(atomic_load_explicit(&r.users_in,memory_order_acquire)!=2)metadata_yield();
        for(unsigned i=0;i<2;i++){
#ifdef _WIN32
            workers[i]=CreateThread(NULL,0,forward_actor,&actors[i],0,NULL);assert(workers[i]);
#else
            assert(pthread_create(&workers[i],NULL,forward_actor,&actors[i])==0);
#endif
        }
        while(atomic_load_explicit(&r.ready,memory_order_acquire)!=2)metadata_yield();atomic_store_explicit(&r.go,1,memory_order_release);
        while(!atomic_load_explicit(&r.selected,memory_order_acquire))metadata_yield();
        assert(atomic_load(&r.cell.access)==GC_SOURCE_CLOSED+2);
        SourceAccessRequest selection={0,r.cell.source_offset,r.cell.incarnation,0,atomic_load(&r.cell.word)>>2};
        assert(access(&r.cell,&selection,GC_SA_CHECK)==GC_SA_WAIT);
        MutatorContext late={.abi_version=GC_ABI_VERSION,.thread_id=99};
        SourceAccessRequest late_scope={(uintptr_t)&late,r.cell.source_offset,r.cell.incarnation,0,0};
        assert(access(&r.cell,&late_scope,GC_SA_ENTER)==GC_SA_WAIT&&!late_scope.held&&!atomic_load(&late.access_epoch));
        ResolveRequest late_resolve={.reference=(r.cell.source_offset<<16)|0x1510,.scope={.context=(uintptr_t)&late}};
        assert(resolve(&r.page,&late_resolve,GC_RESOLVE_ENTER)==GC_RESOLVE_WAIT&&!late_resolve.scope.held);
        // Even a correctly prepared copy cannot start while admitted scalar
        // writers retain their guards. The real copier is waiting on CHECK.
        uint64_t ticket=atomic_load(&r.cell.word)>>2;
        RelocationCopy blocked={(uintptr_t)r.mapping,r.cell.incarnation,ticket,2*GC_HEAP_PAGE_BYTES,
            (ticket+1)*GC_HEAP_PAGE_BYTES+sizeof(ObjectHeader),(ticket+2)*GC_HEAP_PAGE_BYTES,GC_COPY_MAX_BYTES,0,0};
        RelocationCopy unchanged=blocked;
        assert(copy(&r.cell,&blocked)==GC_COPY_WAIT&&!memcmp(&blocked,&unchanged,sizeof(blocked)));
        atomic_store_explicit(&r.release_users,1,memory_order_release);
        while(!atomic_load_explicit(&r.half,memory_order_acquire)||!atomic_load_explicit(&r.losers,memory_order_acquire))metadata_yield();
        ForwardRequest query={r.cell.source_offset,r.cell.incarnation,0,0};assert(step(&r.cell,&query,GC_FWD_READ)==GC_FWD_BUSY&&!query.destination);
        // A mark arriving after the physical copy starts remains shared.
        r.record.old_epoch=10;
        atomic_store_explicit(&r.finish,1,memory_order_release);
        uint64_t result;do{result=step(&r.cell,&query,GC_FWD_READ);if(result==GC_FWD_BUSY)metadata_yield();}while(result==GC_FWD_BUSY);
        if(r.retain){
            assert(result==GC_FWD_RETAINED&&!query.destination);
            assert(access(&r.cell,&late_scope,GC_SA_ENTER)==GC_SA_WAIT);
            SourceAccessRequest reopen={0,r.cell.source_offset,r.cell.incarnation,0,0};assert(access(&r.cell,&reopen,GC_SA_REOPEN)==0);
            assert(access(&r.cell,&late_scope,GC_SA_ENTER)==0);assert(access(&r.cell,&late_scope,GC_SA_EXIT)==0);
            assert(resolve(&r.page,&late_resolve,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);assert(resolve(&r.page,&late_resolve,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        }
        else {
            assert(resolve(&r.page,&late_resolve,GC_RESOLVE_ENTER)==GC_RESOLVE_FORWARDED&&late_resolve.destination==query.destination&&!late_resolve.scope.held);
            assert(result==GC_FWD_OK);assert(access(&r.cell,&late_scope,GC_SA_ENTER)==GC_SA_WAIT);
            SourceAccessRequest reopen={0,r.cell.source_offset,r.cell.incarnation,0,0};assert(access(&r.cell,&reopen,GC_SA_REOPEN)==GC_SA_INVALID);
            ObjectHeader *dest=(ObjectHeader *)((char *)r.mapping+(query.destination>>16)-sizeof(ObjectHeader));
            assert(atomic_load_explicit(&dest->state,memory_order_acquire)==GC_OBJECT_PUBLISHED);
            assert(dest->payload_bytes==source->payload_bytes&&dest->descriptor_id==source->descriptor_id&&dest->mark_record==source->mark_record);
            assert(((MarkRecord *)(uintptr_t)dest->mark_record)->old_epoch==10);
            assert(!memcmp((unsigned char *)dest+sizeof(*dest),bytes,(size_t)r.payload));
        }
        for(unsigned i=0;i<2;i++){
#ifdef _WIN32
            assert(WaitForSingleObject(workers[i],30000)==WAIT_OBJECT_0);assert(CloseHandle(workers[i]));assert(WaitForSingleObject(users[i],30000)==WAIT_OBJECT_0);assert(CloseHandle(users[i]));
#else
            assert(pthread_join(workers[i],NULL)==0);assert(pthread_join(users[i],NULL)==0);
#endif
        }
        assert(atomic_load(&r.losers)==1&&atomic_load(&r.cleaned)==(r.retain?2:1));
        for(uint64_t i=0;i<r.payload;i++){
            unsigned char expected=(unsigned char)(i*37+round);
            if(r.payload>=16&&i<16)expected=((unsigned char *)r.scalar_after)[i];
            else if(r.payload<16&&i<2)expected=(unsigned char)(0xe0+i);
            assert(bytes[i]==expected);
        }
        pool_vm_release(r.mapping,4*GC_HEAP_PAGE_BYTES);
    }
    assert(scalar_rounds==32);
    puts("scalar relocation: 32 two-user updates survive closed admission, copy drain and retained-source paths PASS");
    puts("relocation copy: 64 emitted/C bounded-copy, partial-abandonment and publication rounds PASS");
    puts("resolve access: 64 competing-copy, busy retry and retained-source rounds PASS");
    puts("source access: 64 delayed-source-user, closed-admission, drain and retained-reopen rounds PASS");
    puts("forwarding: 64 two-worker partial-copy, acquire-reader, loser cleanup and retention rounds PASS");
}
#endif
