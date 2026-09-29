#ifndef KLASSIC_GC_BUILTIN_DESCRIPTOR_CASES_H
#define KLASSIC_GC_BUILTIN_DESCRIPTOR_CASES_H
#include "gc_builtin_descriptors.h"
#include "gc_descriptor_cases.h"
typedef void (*BuiltinPreparer)(BuiltinDescriptors *);
static void gc_run_builtin_descriptor_cases(DescriptorRegister reg,DescriptorScanner scan,BuiltinPreparer prepare){
    BuiltinDescriptors shapes,expected;memset(&shapes,0xa5,sizeof(shapes));
    prepare(&shapes);gc_builtin_descriptors_prepare(&expected);
    expected.record.element_bitmap=expected.list.element_bitmap=(uintptr_t)&shapes.pointer_bit;
    assert(!memcmp(&shapes,&expected,sizeof(shapes)));
    _Atomic uint64_t slots[8]={0};DescriptorRegistry registry={(uintptr_t)slots,8};
    const TypeDescriptor *descriptors[]={&shapes.raw,&shapes.record,&shapes.list};
    const uint64_t counts[]={0,1,2,31,32,255,256,257,1024};
    for(unsigned kind=0;kind<3;kind++){
        const TypeDescriptor *d=descriptors[kind];
        assert(reg(&registry,d)==GC_DESC_OK);
        assert(reg(&registry,d)==GC_DESC_DUPLICATE);
        for(unsigned c=0;c<sizeof(counts)/sizeof(*counts);c++){
            uint64_t bytes=d->fixed_bytes+counts[c]*d->element_bytes,output[7],seen=0;
            DescriptorScan q={d->id,bytes,0,7,(uintptr_t)output,7,0,0};
            for(;;){
                uint64_t status=scan(&registry,&q);assert(status==GC_SCAN_DONE||status==GC_SCAN_MORE);
                assert(q.written<=7);
                for(uint64_t i=0;i<q.written;i++){
                    assert(kind&&output[i]==(seen+(kind==2))*8);seen++;
                }
                if(status==GC_SCAN_DONE)break;
            }
            assert(seen==(kind?counts[c]:0)&&q.next_offset==bytes);
        }
        if(kind){
            uint64_t output[2]={17,23};DescriptorScan q={d->id,d->fixed_bytes+1,0,2,(uintptr_t)output,2,31,47},before=q;
            assert(scan(&registry,&q)==GC_SCAN_INVALID);
            assert(!memcmp(&q,&before,sizeof(q))&&output[0]==17&&output[1]==23);
        }
    }
    puts("builtin descriptors: raw, pointer records and scalar-prefixed lists scan exact logical fields PASS");
}
#endif
