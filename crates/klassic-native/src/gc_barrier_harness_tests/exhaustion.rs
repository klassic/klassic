//! Resource failures must identify the resource, rather than claim live-heap OOM.
use super::{H, run_failure};
use crate::NativeCodeGenerator;
use crate::portable_asm::{PortableAsm, Reg};

fn native_mapping_failure(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    // Child-process-only RLIMIT_AS=0: startup mappings already exist, but the
    // first private copy-metadata mapping must fail. No host limit is changed.
    h.g.mov_imm64(Reg::V0, 0);
    h.g.store_rbp_slot(56, Reg::V0);
    h.g.store_rbp_slot(64, Reg::V0);
    h.g.mov_reg_reg(Reg::V6, Reg::V5);
    h.g.sub_reg_imm32(Reg::V6, 64);
    h.g.mov_imm64(Reg::V7, 9); // RLIMIT_AS
    h.g.mov_imm64(Reg::V0, 160); // Linux setrlimit
    h.g.asm.syscall();
    h.keep(3);
    h.report("limit_result", |h| h.value(super::Src::Raw(3)));
    h.collect();
    h.finish_collection(); // Root copying is now outside the start pause.
}

#[test]
fn production_copy_metadata_mapping_failure_reports_native_resource_exhaustion() {
    let o = run_failure(native_mapping_failure, 0);
    assert_eq!(o.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&o.stdout), "limit_result=0\n");
    assert_eq!(
        o.stderr,
        crate::gc_codegen::production::NATIVE_OOM_TEXT,
        "{}",
        String::from_utf8_lossy(&o.stderr)
    );
}

fn metadata_failure(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    crate::gc_codegen::production::test_support::leave_one_mark_record(h.g);
    h.alloc(1, 16, 0);
    h.alloc(1, 16, 1);
}

#[test]
fn production_allocation_metadata_exhaustion_is_distinct_from_live_heap_oom() {
    let o = run_failure(metadata_failure, 0);
    assert_eq!(o.status.code(), Some(1));
    assert_eq!(o.stderr, crate::gc_codegen::production::METADATA_OOM_TEXT);
}

fn moving_live_exhaustion(g: &mut NativeCodeGenerator, _arg: u64) {
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    let enable = h.g.gc_gen.as_ref().unwrap().labels.moving_enable_actor;
    h.g.call_label(enable);
    // Every distinct allocation remains in a real live RootCell. The requested
    // payload alone exceeds the configured physical heap; retry must terminate
    // through actual copy/heap exhaustion rather than spin on fresh admission.
    for _ in 0..256 {
        h.alloc(1, 100_000, 0);
        h.root(0);
    }
}

#[test]
fn production_moving_refill_still_terminates_when_the_retained_live_set_cannot_fit() {
    let o = super::execute(moving_live_exhaustion, 0, true, false);
    assert_eq!(o.status.code(), Some(1), "{o:?}");
    let text = String::from_utf8_lossy(&o.stderr);
    assert!(text.contains("out of memory"), "{text}");
    assert!(!text.contains("runtime invariant"), "{text}");
}
