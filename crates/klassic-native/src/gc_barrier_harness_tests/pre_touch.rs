//! Real Linux residency proves pre-touch writes rather than only counting bytes.
use super::{H, Src, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::{
    CommitMetric as M, GENERATION_YOUNG, commit_metric, commit_page, heap_page_address, pool_claim,
    uncommit_page,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::os::unix::process::ExitStatusExt;
const PAGE: u64 = 128 << 10;
const MINIMUM: u64 = 4 << 20;

fn configure(g: &mut NativeCodeGenerator, enabled: u64) {
    g.gc_min_heap = MINIMUM;
    g.gc_max_heap = 64 << 20;
    g.gc_pre_touch = enabled != 0;
}
// mincore reports a byte per OS page without faulting the payload in.
// Return whether *every* OS page of the range is resident.
fn resident(h: &mut H<'_>, page: u64, bytes: u64) {
    heap_page_address(h.g, page);
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.mov_imm64(R::V6, bytes);
    h.g.sub_reg_imm32(R::V4, 4096);
    h.g.mov_reg_reg(R::V2, R::V4);
    h.g.mov_imm64(R::V0, 27);
    h.g.asm.syscall();
    let missing = h.g.create_text_label();
    let done = h.g.create_text_label();
    let next = h.g.create_text_label();
    h.g.test_reg_reg(R::V0, R::V0);
    h.g.jcc_label(C::NotEqual, missing);
    h.g.mov_reg_reg(R::V8, R::V4);
    h.g.mov_imm64(R::V9, bytes / 4096 / 8);
    h.g.mov_imm64(R::V2, 0x0101_0101_0101_0101);
    h.g.bind_text_label(next);
    h.g.load_ptr_disp32(R::V0, R::V8, 0);
    h.g.and_reg_reg(R::V0, R::V2);
    h.g.cmp_reg_reg(R::V0, R::V2);
    h.g.jcc_label(C::NotEqual, missing);
    h.g.add_reg_imm32(R::V8, 8);
    h.g.sub_reg_imm32(R::V9, 1);
    h.g.jcc_label(C::NotEqual, next);
    h.g.mov_imm64(R::V0, 1);
    h.g.jmp_label(done);
    h.g.bind_text_label(missing);
    h.g.mov_imm64(R::V0, 0);
    h.g.bind_text_label(done);
    h.g.add_reg_imm32(R::V4, 4096);
}
fn grow(g: &mut NativeCodeGenerator, enabled: u64) {
    configure(g, enabled);
    let mut h = H::begin(g);
    h.report("initial_resident", |h| resident(h, 1, MINIMUM));
    h.report("initial_touched", |h| commit_metric(h.g, M::Pretouched));
    commit_page(h.g, 33);
    h.report("grown_resident", |h| resident(h, 33, PAGE));
    h.report("grown_touched", |h| commit_metric(h.g, M::Pretouched));
    heap_page_address(h.g, 33);
    h.g.mov_imm64(R::V1, 42);
    h.g.store_ptr_disp32(R::V0, 4096, R::V1);
    commit_page(h.g, 33);
    h.report("retained", |h| {
        heap_page_address(h.g, 33);
        h.g.load_ptr_disp32(R::V0, R::V0, 4096);
    });
    h.report("duplicate_touched", |h| commit_metric(h.g, M::Pretouched));
    h.report("commits", |h| commit_metric(h.g, M::Commits));
}
#[test]
fn production_pre_touch_faults_in_each_initial_and_new_os_page_without_retouching_live_storage() {
    for enabled in [0, 1] {
        let o = run(grow, enabled, false);
        for name in ["initial_resident", "grown_resident"] {
            assert_eq!(o.get(name), enabled as i64, "{}", o.text());
        }
        assert_eq!(o.get("initial_touched"), (enabled * MINIMUM) as i64);
        for name in ["grown_touched", "duplicate_touched"] {
            assert_eq!(o.get(name), (enabled * (MINIMUM + PAGE)) as i64);
        }
        assert_eq!(o.get("retained"), 42);
        assert_eq!(o.get("commits"), 33);
    }
}
fn reuse(g: &mut NativeCodeGenerator, enabled: u64) {
    configure(g, enabled);
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.collect();
    h.finish_collection();
    uncommit_page(h.g, 1);
    h.keep(20);
    h.report("released", |h| h.value(Src::Raw(20)));
    pool_claim(h.g, 1, GENERATION_YOUNG);
    commit_page(h.g, 1);
    h.report("recommitted_resident", |h| resident(h, 1, PAGE));
    h.report("touched", |h| commit_metric(h.g, M::Pretouched));
    h.report("commits", |h| commit_metric(h.g, M::Commits));
    h.report("recommits", |h| commit_metric(h.g, M::Recommits));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_pre_touch_recommits_all_os_pages_and_preserves_moved_live_values() {
    for enabled in [0, 1] {
        let o = run(reuse, enabled, false);
        assert_eq!(o.get("released"), 1);
        assert_eq!(o.get("recommits"), 1);
        assert_eq!(
            o.get("recommitted_resident"),
            enabled as i64,
            "{}",
            o.text()
        );
        assert_eq!(
            o.get("touched"),
            enabled as i64 * o.get("commits") * PAGE as i64
        );
        assert_eq!(o.get("value"), 84);
    }
}

fn protected(g: &mut NativeCodeGenerator, page: u64) {
    configure(g, 1);
    let mut h = H::begin(g);
    heap_page_address(h.g, page);
    h.g.load_ptr_disp32(R::V0, R::V0, 0);
    h.keep(20);
    h.report("unexpected", |h| h.value(Src::Raw(20)));
}
#[test]
fn production_pre_touch_keeps_the_guard_and_uncommitted_payload_inaccessible() {
    for page in [0, 33] {
        let output = run_failure(protected, page);
        assert_eq!(output.status.signal(), Some(11), "page {page}: {output:?}");
    }
}
