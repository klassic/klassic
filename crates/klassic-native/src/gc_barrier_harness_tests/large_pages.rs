//! Native THP backing and grouped ownership/protection probes.
use super::{H, Observed, Src, execute, run, run_failure};
use crate::gc_codegen::production::{large_pages, test_support::*};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use crate::{GcLargePages as Mode, NativeCodeGenerator};
use std::process::Output;
use std::sync::OnceLock;
const HUGE: u64 = 2 << 20;
const MIN: u64 = 4 << 20;
const PAGE: u64 = 128 << 10;
fn configure(g: &mut NativeCodeGenerator, mode: Mode) {
    g.gc_max_heap = 64 << 20;
    g.gc_min_heap = MIN;
    g.gc_large_pages = mode;
}
// Independent OS capability probe: no call into the production THP helper.
fn capability(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, Mode::Off);
    let mut h = H::begin(g);
    h.g.mov_imm64(R::V7, 0);
    h.g.mov_imm64(R::V6, 2 * HUGE);
    h.g.mov_imm64(R::V2, 3);
    h.g.mov_imm64(R::V10, 0x22);
    h.g.mov_imm64(R::V8, u64::MAX);
    h.g.mov_imm64(R::V9, 0);
    h.g.mov_imm64(R::V0, 9);
    h.g.asm.syscall();
    h.keep(30);
    let failed = h.g.create_text_label();
    let done = h.g.create_text_label();
    h.g.cmp_reg_imm32(R::V0, -4095);
    h.g.jcc_label(C::AboveOrEqual, failed);
    h.g.add_reg_imm32(R::V0, (HUGE - 1) as i32);
    h.g.and_reg_imm32(R::V0, -(HUGE as i32));
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.mov_imm64(R::V2, 0);
    h.g.store_ptr_disp32(R::V7, 0, R::V2);
    h.g.mov_imm64(R::V6, HUGE);
    h.g.mov_imm64(R::V2, 25);
    h.g.mov_imm64(R::V0, 28);
    h.g.asm.syscall();
    h.keep(31);
    h.value(Src::Raw(30));
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.mov_imm64(R::V6, 2 * HUGE);
    h.g.mov_imm64(R::V0, 11);
    h.g.asm.syscall();
    h.value(Src::Raw(31));
    h.g.jmp_label(done);
    h.g.bind_text_label(failed);
    h.g.mov_imm64(R::V0, u64::MAX);
    h.g.bind_text_label(done);
    h.keep(31);
    h.report("collapse", |h| h.value(Src::Raw(31)));
}
fn has_thp() -> bool {
    static AVAILABLE: OnceLock<bool> = OnceLock::new();
    *AVAILABLE.get_or_init(|| {
        let result = run(capability, 0, false).get("collapse");
        eprintln!("independent-thp-collapse-status={result}");
        result == 0
    })
}
fn checked_required(
    body: fn(&mut NativeCodeGenerator, u64),
    arg: u64,
    worker: bool,
) -> Option<(Observed, Output)> {
    let output = execute(body, arg, worker, false);
    if !has_thp() {
        assert_eq!(output.status.code(), Some(1), "{output:?}");
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("required large-page commit failed")
        );
        eprintln!(
            "THP unsupported here: required path diagnosed failure; no positive backing evidence"
        );
        return None;
    }
    let text = String::from_utf8_lossy(&output.stdout).into_owned();
    assert!(
        output.status.success(),
        "status={:?}\n{text}\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let values = text
        .lines()
        .filter_map(|l| l.split_once('='))
        .filter_map(|(k, v)| Some((k.to_owned(), v.parse().ok()?)))
        .collect();
    Some((Observed { values, text }, output))
}
fn field(h: &mut H<'_>, name: &str, index: usize) {
    h.report(name, |h| {
        h.g.load_ptr_disp32(R::V0, R::V3, large_pages::live_offsets()[index])
    });
}
fn dump_smaps(h: &mut H<'_>) {
    let path = h.g.asm.data_label_with_bytes(b"/proc/self/smaps\0");
    h.g.mov_data_addr(R::V7, path);
    h.g.mov_imm64(R::V6, 0);
    h.g.mov_imm64(R::V2, 0);
    h.g.mov_imm64(R::V0, 2);
    h.g.asm.syscall();
    h.g.mov_reg_reg(R::V10, R::V0);
    h.g.sub_reg_imm32(R::V4, 4096);
    let next = h.g.create_text_label();
    let done = h.g.create_text_label();
    h.g.bind_text_label(next);
    h.g.mov_reg_reg(R::V7, R::V10);
    h.g.mov_reg_reg(R::V6, R::V4);
    h.g.mov_imm64(R::V2, 4096);
    h.g.mov_imm64(R::V0, 0);
    h.g.asm.syscall();
    h.g.test_reg_reg(R::V0, R::V0);
    h.g.jcc_label(C::LessEqual, done);
    h.g.mov_reg_reg(R::V2, R::V0);
    h.g.mov_imm64(R::V7, 2);
    h.g.mov_reg_reg(R::V6, R::V4);
    h.g.mov_imm64(R::V0, 1);
    h.g.asm.syscall();
    h.g.jmp_label(next);
    h.g.bind_text_label(done);
    h.g.mov_reg_reg(R::V7, R::V10);
    h.g.mov_imm64(R::V0, 3);
    h.g.asm.syscall();
    h.g.add_reg_imm32(R::V4, 4096);
}
fn huge_kib(text: &str, start: u64, end: u64) -> u64 {
    let mut relevant = false;
    let mut total = 0;
    for line in text.lines() {
        if let Some((a, b)) = line
            .split_whitespace()
            .next()
            .and_then(|x| x.split_once('-'))
            && let (Ok(a), Ok(b)) = (u64::from_str_radix(a, 16), u64::from_str_radix(b, 16))
        {
            relevant = a < end && b > start;
        }
        if relevant && line.starts_with("AnonHugePages:") {
            total += line
                .split_whitespace()
                .nth(1)
                .unwrap()
                .parse::<u64>()
                .unwrap();
        }
    }
    total
}
fn grow(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, Mode::Required);
    let mut h = H::begin(g);
    h.report("payload", |h| heap_page_address(h.g, 1));
    field(&mut h, "reserved", 2);
    field(&mut h, "advised", 3);
    field(&mut h, "collapsed_initial", 5);
    commit_page(h.g, 33);
    h.report("committed", |h| commit_metric(h.g, CommitMetric::Current));
    field(&mut h, "collapsed_grown", 5);
    heap_page_address(h.g, 48);
    h.g.mov_imm64(R::V1, 42);
    h.g.store_ptr_disp32(R::V0, 4096, R::V1);
    commit_page(h.g, 48);
    h.report("retained", |h| {
        heap_page_address(h.g, 48);
        h.g.load_ptr_disp32(R::V0, R::V0, 4096)
    });
    h.report("commits", |h| commit_metric(h.g, CommitMetric::Commits));
    field(&mut h, "collapsed_duplicate", 5);
    dump_smaps(&mut h);
}
#[test]
fn production_required_large_pages_use_real_thp_and_commit_a_group_once_without_overwriting_neighbors()
 {
    let Some((o, output)) = checked_required(grow, 0, false) else {
        return;
    };
    assert_eq!(o.get("payload") as u64 % HUGE, 0);
    assert_eq!(o.get("reserved"), (64 << 20) + PAGE as i64 + HUGE as i64);
    assert_eq!(o.get("advised"), 64 << 20);
    assert_eq!(o.get("collapsed_initial"), MIN as i64);
    assert_eq!(o.get("committed"), (MIN + HUGE) as i64);
    for key in ["collapsed_grown", "collapsed_duplicate"] {
        assert_eq!(o.get(key), (MIN + HUGE) as i64);
    }
    assert_eq!(o.get("commits"), 48);
    assert_eq!(o.get("retained"), 42);
    let physical = huge_kib(
        &String::from_utf8_lossy(&output.stderr),
        o.get("payload") as u64,
        o.get("payload") as u64 + (64 << 20),
    );
    assert_eq!(physical, (MIN + HUGE) / 1024);
    eprintln!(
        "strict-thp: actual_anon_huge_kib={physical} committed={}",
        o.get("committed")
    );
}
fn reuse(g: &mut NativeCodeGenerator, mode: u64) {
    configure(g, Mode::Required);
    g.gc_pre_touch = true;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.collect();
    h.finish_collection();
    h.report("before", |h| commit_metric(h.g, CommitMetric::Current));
    if mode == 1 {
        pool_claim(h.g, 1, GENERATION_YOUNG);
    }
    if mode == 2 {
        pool_page_state(h.g, 15, 0);
    }
    uncommit_page(h.g, 2);
    h.keep(20);
    h.report("released", |h| h.value(Src::Raw(20)));
    h.report("after", |h| commit_metric(h.g, CommitMetric::Current));
    if mode >= 3 {
        heap_page_address(h.g, if mode == 3 { 1 } else { 16 });
        h.g.load_ptr_disp32(R::V0, R::V0, 0);
    }
    if mode == 0 {
        pool_claim(h.g, 16, GENERATION_YOUNG);
        commit_page(h.g, 16);
        h.report("recommits", |h| commit_metric(h.g, CommitMetric::Recommits));
        h.report("uncommits", |h| commit_metric(h.g, CommitMetric::Uncommits));
        h.report("zero", |h| {
            heap_page_address(h.g, 1);
            h.g.load_ptr_disp32(R::V0, R::V0, 4096)
        });
        h.report("touched", |h| commit_metric(h.g, CommitMetric::Pretouched));
        h.report("commits", |h| commit_metric(h.g, CommitMetric::Commits));
    }
    if mode == 2 {
        pool_page_state(h.g, 15, crate::gc_abi::PAGE_READY);
    }
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_required_large_pages_reclaim_only_wholly_free_clean_groups_and_recommit_zeroed_storage()
 {
    for (mode, worker) in [(0, false), (1, false), (2, false), (0, true)] {
        let Some((o, _)) = checked_required(reuse, mode, worker) else {
            continue;
        };
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("released"), i64::from(mode == 0), "{}", o.text());
        if mode == 0 {
            assert_eq!(o.get("before") - o.get("after"), HUGE as i64);
            assert!(o.get("after") >= MIN as i64);
            assert_eq!(o.get("recommits"), 16);
            assert_eq!(o.get("uncommits"), 16);
            assert_eq!(o.get("zero"), 0);
            assert_eq!(o.get("touched"), o.get("commits") * PAGE as i64);
        } else {
            assert_eq!(o.get("before"), o.get("after"));
        }
    }
}
fn try_failure(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, Mode::Try);
    g.gc_large_pages_fault = true;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
    field(&mut h, "advised", 3);
    field(&mut h, "advice_failures", 4);
    field(&mut h, "collapsed", 5);
}
#[test]
fn production_try_large_pages_falls_back_after_real_advice_failure_without_claiming_huge_backing() {
    let o = run(try_failure, 0, false);
    assert_eq!(o.get("value"), 42);
    assert_eq!(o.get("advised"), 0);
    assert_eq!(o.get("advice_failures"), 1);
    assert_eq!(o.get("collapsed"), 0);
}
fn required_failure(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, Mode::Required);
    g.gc_large_pages_fault = true;
    let mut h = H::begin(g);
    h.report("unexpected", |h| h.g.mov_imm64(R::V0, 42));
}
#[test]
fn production_required_large_pages_report_a_controlled_failure_when_collapse_is_rejected() {
    let output = run_failure(required_failure, 0);
    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("required large-page commit failed"));
}

fn aged(g: &mut NativeCodeGenerator, _: u64) {
    configure(g, Mode::Required);
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.collect();
    h.finish_collection();
    const SECOND: u64 = 1_000_000_000;
    poll_uncommit(h.g, SECOND);
    // Both initially committed Free groups have a later final member.
    // The old 1-second candidates must not authorize either group early.
    set_free_since(h.g, 16, 2 * SECOND);
    set_free_since(h.g, 32, 2 * SECOND);
    poll_uncommit(h.g, 301 * SECOND);
    h.report("early", |h| commit_metric(h.g, CommitMetric::Uncommits));
    h.report("late_member", |h| free_since(h.g, 16));
    poll_uncommit(h.g, 302 * SECOND);
    h.report("released", |h| commit_metric(h.g, CommitMetric::Uncommits));
    h.report("remaining", |h| commit_metric(h.g, CommitMetric::Current));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_required_large_pages_idle_uncommit_waits_for_every_member_and_preserves_minimum() {
    let Some((o, _)) = checked_required(aged, 0, false) else {
        return;
    };
    assert_eq!(o.get("early"), 0, "{}", o.text());
    assert_eq!(o.get("late_member"), 2_000_000_000);
    assert_eq!(o.get("released"), 16);
    assert_eq!(o.get("remaining"), MIN as i64);
    assert_eq!(o.get("value"), 84);
}
fn protected(g: &mut NativeCodeGenerator, page: u64) {
    configure(g, Mode::Required);
    let mut h = H::begin(g);
    commit_page(h.g, 33);
    heap_page_address(h.g, page);
    h.g.load_ptr_disp32(R::V0, R::V0, 0);
    h.keep(20);
    h.report("unexpected", |h| h.value(Src::Raw(20)));
}
#[test]
fn production_required_large_pages_keep_guard_uncommitted_and_both_retired_group_ends_inaccessible()
{
    use std::os::unix::process::ExitStatusExt;
    if !has_thp() {
        let _ = checked_required(grow, 0, false);
        return;
    }
    for (body, arg) in [
        (protected as fn(&mut NativeCodeGenerator, u64), 0),
        (protected, 49),
        (reuse, 3),
        (reuse, 4),
    ] {
        let output = run_failure(body, arg);
        assert_eq!(output.status.signal(), Some(11), "case {arg}: {output:?}");
    }
}
