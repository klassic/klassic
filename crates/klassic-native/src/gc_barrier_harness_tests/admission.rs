//! Fresh allocation must leave enough destination pages for relocation work.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_abi::ObjectHeader;
use crate::gc_codegen::production::test_support::Counter;
use crate::portable_asm::{PortableAsm, Reg};
use std::mem::{offset_of, size_of};

fn refill_while_relocating(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 4242);
    h.root(0);
    h.repeat(5000, |h| {
        h.alloc(2, 16, 1);
        h.store(Src::Raw(1), 0, Src::Root(0), true);
        h.value(Src::Raw(1));
        h.set_root(0);
    });
    h.report("before", |h| h.object_id(Src::Root(0)));
    h.gc_until(1);
    // No OS worker: ordinary quanta cannot drain 5000 objects in four
    // allocations. Refill admission must stall and drive bounded quanta.
    h.repeat(4, |h| h.alloc(1, 60000, 1));
    h.root(1);
    h.report("fresh_descriptor", |h| {
        h.value(Src::Root(1));
        h.g.load_ptr_disp32(
            Reg::V0,
            Reg::V0,
            offset_of!(ObjectHeader, descriptor_id) as i32 - size_of::<ObjectHeader>() as i32,
        );
    });
    h.report("fresh_bytes", |h| {
        h.value(Src::Root(1));
        h.g.load_ptr_disp32(
            Reg::V0,
            Reg::V0,
            offset_of!(ObjectHeader, payload_bytes) as i32 - size_of::<ObjectHeader>() as i32,
        );
    });
    h.report_counter("stalls", Counter::AllocationStalls);
    h.report_counter("phase_open", Counter::PhaseOpen);
    h.report("after", |h| h.object_id(Src::Root(0)));
    h.repeat(5000, |h| {
        h.load_barriered(Src::Root(0), 0);
        h.set_root(0);
    });
    h.report("leaf_id", |h| h.object_id(Src::Root(0)));
    h.report("leaf", |h| h.load(Src::Root(0), 0));
}

#[test]
fn production_refill_reserves_to_space_and_drives_normal_relocation_quanta() {
    let o = run(refill_while_relocating, 0, false);
    assert!(o.get("stalls") > 0, "{}", o.text());
    assert_eq!(o.get("phase_open"), 0, "{}", o.text());
    assert_eq!(o.get("before"), o.get("after"));
    assert_eq!(o.get("leaf_id"), 1);
    assert_eq!(o.get("leaf"), 4242);
    assert_eq!(o.get("fresh_descriptor"), 1);
    assert_eq!(o.get("fresh_bytes"), 60000);
}
