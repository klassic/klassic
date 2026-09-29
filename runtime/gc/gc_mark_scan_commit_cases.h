#ifndef KLASSIC_GC_MARK_SCAN_COMMIT_CASES_H
#define KLASSIC_GC_MARK_SCAN_COMMIT_CASES_H
#include "gc_mark_cases.h"
#include "gc_mark_scan_commit.h"
static void gc_run_mark_scan_commit_cases(MarkScanCommitter candidate){
    unsigned cases=0;
    for(unsigned implementation=0;implementation<2;implementation++)
    for(unsigned generation=0;generation<2;generation++)
    for(uint64_t status=0;status<=1;status++)for(unsigned fault=0;fault<16;fault++){
        MarkScanCommitter commit=implementation?candidate:gc_mark_scan_commit_locked;
        MarkFixture f,before;mark_fixture(&f,1);f.g.generation=generation;
        assert(gc_mark_step_locked(&f.g,&f.q,GC_MARK_OFFER)==GC_MARK_OK);
        assert(gc_mark_step_locked(&f.g,&f.q,GC_MARK_TAKE)==GC_MARK_OK);
        MarkScan s={.job=(uintptr_t)&f.q,.capacity=2,.next_cursor=16,.examined=2,.written=1};
        uint64_t input_status=status,expected=GC_MARK_INVALID;
        switch(fault){
        case 0:expected=GC_MARK_OK;break;
        case 1:input_status=2;break;
        case 2:s.capacity=0;break;
        case 3:s.capacity=257;break;
        case 4:s.examined=3;break;
        case 5:s.written=3;break;
        case 6:s.job=0;break;
        case 7:s.job++;break;
        case 8:f.q.index=0;break;
        case 9:f.q.serial=0;break;
        case 10:f.q.cursor=24;break;
        case 11:s.next_cursor=0;expected=status?GC_MARK_INVALID:GC_MARK_OK;break;
        case 12:s.next_cursor=17;expected=status?GC_MARK_INVALID:GC_MARK_OK;break;
        case 13:f.q.serial++;break;
        case 14:f.g.tasks=0;break;
        case 15:f.g.last_serial=UINT64_MAX;expected=status?GC_MARK_EXHAUSTED:GC_MARK_OK;break;
        }
        before=f;MarkScan saved=s;
        assert(commit(&f.g,&s,input_status)==expected);
        if(expected){assert(!memcmp(&f,&before,sizeof(f))&&!memcmp(&s,&saved,sizeof(s)));}
        else{
            assert(!s.job);saved.job=0;assert(!memcmp(&s,&saved,sizeof(s)));
            if(status){
                assert(f.g.queued==1&&!f.g.in_flight&&!f.q.index&&!f.q.metadata_lease);
                assert(gc_mark_step_locked(&f.g,&f.q,GC_MARK_TAKE)==GC_MARK_OK);
                assert(f.q.cursor==saved.next_cursor&&f.q.metadata_lease==before.q.metadata_lease);
            }else{
                assert(!f.g.queued&&!f.g.in_flight&&!f.q.index&&!f.q.serial);
                assert(f.q.cursor==saved.next_cursor&&f.q.metadata_lease==before.q.metadata_lease);
            }
            before=f;
            assert(commit(&f.g,&s,input_status)==GC_MARK_INVALID);
            assert(!memcmp(&f,&before,sizeof(f))&&!memcmp(&s,&saved,sizeof(s)));
        }
        cases++;
    }
    assert(cases==128);
    puts("mark scan commit: 128 C/candidate receipt, cursor rollback and claim transfer cases PASS");
}
#endif
