#ifndef KLASSIC_GC_PIN_PAGE_CASES_H
#define KLASSIC_GC_PIN_PAGE_CASES_H
#include "gc_pin_page.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
typedef uint64_t (*PinPageStep)(RelocationGate *,RelocationGateRequest *,uint64_t);
typedef struct {
    PinPageStep step;
    pthread_mutex_t lock;
    RelocationGate page;
    RelocationPermit permit;
    RelocationGateRequest pin,selection;
    uint64_t pinned,selected;
} PinPageRace;
static void *gc_pin_page_racer(void *arg){
    PinPageRace *r=arg;
    assert(!pthread_mutex_lock(&r->lock));
    r->pinned=r->step(&r->page,&r->pin,GC_PP_PIN);
    assert(!pthread_mutex_unlock(&r->lock));return NULL;
}
static void gc_run_pin_page_races(PinPageStep step){
    for(unsigned i=0;i<128;i++){
        PinPageRace r={.step=step,.page={.epoch=1}};
        assert(!pthread_mutex_init(&r.lock,NULL));r.pin.permit=(uintptr_t)&r.permit;
        pthread_t thread;assert(!pthread_create(&thread,NULL,gc_pin_page_racer,&r));
        assert(!pthread_mutex_lock(&r.lock));
        r.selected=step(&r.page,&r.selection,GC_PP_SELECT);
        assert(!pthread_mutex_unlock(&r.lock));assert(!pthread_join(thread,NULL));
        assert((r.selected==GC_RG_OK&&r.pinned==GC_RG_WAIT)||
               (r.selected==GC_RG_WAIT&&r.pinned==GC_RG_OK));
        if(r.pinned==GC_RG_OK){
            assert(step(&r.page,&r.pin,GC_PP_UNPIN)==GC_RG_OK);
            assert(step(&r.page,&r.selection,GC_PP_SELECT)==GC_RG_OK);
        }
        assert(step(&r.page,&r.selection,GC_PP_REOPEN)==GC_RG_OK);
        assert(!r.page.active&&!r.page.head&&!r.page.closed);
        assert(!pthread_mutex_destroy(&r.lock));
    }
    puts("page pin admission: 128 two-actor selection races PASS");
}
static void gc_run_pin_page_cases(PinPageStep step){
    RelocationGate page={.epoch=1};RelocationPermit a={0},b={0};
    RelocationGateRequest x={.permit=(uintptr_t)&a},y={.permit=(uintptr_t)&b},selection={0};
    assert(step(&page,&x,GC_PP_PIN)==GC_RG_OK);
    RelocationGate saved=page;
    assert(step(&page,&selection,GC_PP_SELECT)==GC_RG_WAIT);
    assert(!memcmp(&saved,&page,sizeof(page))&&!selection.epoch);
    assert(step(&page,&y,GC_PP_PIN)==GC_RG_OK);
    /* Copied permit controls cannot release the actual registered pin. */
    RelocationPermit copy=a;RelocationGateRequest forged=x;forged.permit=(uintptr_t)&copy;
    assert(step(&page,&forged,GC_PP_UNPIN)==GC_RG_INVALID);
    RelocationGateRequest stale=x;
    assert(step(&page,&x,GC_PP_UNPIN)==GC_RG_OK);
    assert(step(&page,&stale,GC_PP_UNPIN)==GC_RG_INVALID);
    assert(step(&page,&selection,GC_PP_SELECT)==GC_RG_WAIT);
    assert(step(&page,&y,GC_PP_UNPIN)==GC_RG_OK);
    assert(step(&page,&selection,GC_PP_SELECT)==GC_RG_OK);
    RelocationGateRequest competing={0};
    assert(step(&page,&competing,GC_PP_SELECT)==GC_RG_WAIT);
    x=(RelocationGateRequest){.permit=(uintptr_t)&a};saved=page;
    assert(step(&page,&x,GC_PP_PIN)==GC_RG_WAIT);
    assert(!memcmp(&saved,&page,sizeof(page))&&!x.serial&&!a.gate);
    RelocationGateRequest old_selection=selection;
    assert(step(&page,&selection,GC_PP_REOPEN)==GC_RG_OK);
    assert(page.epoch==2&&!page.closed&&!selection.epoch);
    assert(step(&page,&x,GC_PP_PIN)==GC_RG_OK);
    assert(step(&page,&stale,GC_PP_UNPIN)==GC_RG_INVALID);
    assert(step(&page,&old_selection,GC_PP_REOPEN)==GC_RG_INVALID);
    assert(step(&page,&x,GC_PP_UNPIN)==GC_RG_OK);
    page.epoch=UINT64_MAX;
    assert(step(&page,&selection,GC_PP_SELECT)==GC_RG_OK);
    saved=page;old_selection=selection;
    assert(step(&page,&selection,GC_PP_REOPEN)==GC_RG_EXHAUSTED);
    assert(!memcmp(&saved,&page,sizeof(page))&&!memcmp(&old_selection,&selection,sizeof(selection)));
    puts("page pin admission: selection exclusion, replay rejection and unpin/reselect PASS");
    gc_run_pin_page_races(step);
}
#endif
