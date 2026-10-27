//! Actual Linux worker TLS, guarded stack, kernel termination and resource release.
use super::{H, Src, execute, run};
use crate::gc_codegen::{linux_production as linux, production};
use crate::portable_asm::{PortableAsm, Reg as R};
use std::os::unix::process::ExitStatusExt;

fn stop(h: &mut H<'_>) {
    let runtime = h.g.gc_gen.take().unwrap();
    production::shutdown_worker(h.g, &runtime.labels, runtime.data.runtime);
    h.g.gc_gen = Some(runtime);
}
fn lifecycle(g: &mut crate::NativeCodeGenerator, _: u64) {
    let [stack, tid, tls, sp, joins, releases] = linux::offsets();
    let mut h = H::begin(g);
    h.g.load_ptr_disp32(R::V0, R::V3, stack);
    h.keep(30);
    stop(&mut h);
    stop(&mut h);
    h.report("base", |h| h.value(Src::Raw(30)));
    h.report("runtime", |h| h.g.mov_reg_reg(R::V0, R::V3));
    for (name, offset) in [
        ("stack", stack),
        ("tid", tid),
        ("tls", tls),
        ("sp", sp),
        ("joins", joins),
        ("releases", releases),
    ] {
        h.report(name, |h| h.g.load_ptr_disp32(R::V0, R::V3, offset));
    }
    h.report("mincore", |h| {
        h.value(Src::Raw(30));
        h.g.mov_reg_reg(R::V7, R::V0);
        h.g.mov_imm64(R::V6, linux::STACK_BYTES + 8192);
        h.g.sub_reg_imm32(R::V4, 80); // one vector byte per mapped base page
        h.g.mov_reg_reg(R::V2, R::V4);
        h.g.mov_imm64(R::V0, 27); // mincore, requires mapped pages
        h.g.asm.syscall();
        h.g.add_reg_imm32(R::V4, 80);
    });
}
#[test]
fn production_linux_worker_joins_kernel_clear_tid_and_unmaps_its_guarded_stack_once() {
    let o = run(lifecycle, 0, true);
    assert!(o.get("base") > 0, "{}", o.text());
    assert_eq!(o.get("tls"), o.get("runtime"));
    assert!(
        (o.get("base") + 4096..o.get("base") + 4096 + linux::STACK_BYTES as i64)
            .contains(&o.get("sp")),
        "{}",
        o.text()
    );
    for name in ["stack", "tid"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
    for name in ["joins", "releases"] {
        assert_eq!(o.get(name), 1, "{}", o.text());
    }
    assert_eq!(o.get("mincore"), -12, "{}", o.text()); // ENOMEM: mapping has gone
}
fn guard(g: &mut crate::NativeCodeGenerator, upper: u64) {
    let mut h = H::begin(g);
    h.g.load_ptr_disp32(R::V0, R::V3, linux::offsets()[0]);
    if upper != 0 {
        h.g.add_reg_imm32(R::V0, (linux::STACK_BYTES + 4096) as i32);
    }
    h.g.load_ptr_disp32(R::V0, R::V0, 0);
    h.keep(30);
    h.report("unexpected", |h| h.value(Src::Raw(30)));
}
#[test]
fn production_linux_worker_stack_has_inaccessible_guards_at_both_ends() {
    for upper in [0, 1] {
        let output = execute(guard, upper, true, false);
        assert_eq!(output.status.signal(), Some(11), "{output:?}");
    }
}

fn fallback(g: &mut crate::NativeCodeGenerator, mode: u64) {
    g.linux_worker_fault = Some(if mode == 0 {
        linux::Fault::Clone
    } else {
        linux::Fault::Bootstrap
    });
    let mut h = H::begin(g);
    let [stack, tid, tls, _, joins, releases] = linux::offsets();
    h.report("bootstrap", |h| {
        h.g.load_ptr_disp32(R::V0, R::V3, linux::bootstrap_offset())
    });
    h.report("mincore", |h| {
        h.g.load_ptr_disp32(R::V7, R::V3, linux::last_stack_offset());
        h.g.mov_imm64(R::V6, linux::STACK_BYTES + 8192);
        h.g.sub_reg_imm32(R::V4, 80);
        h.g.mov_reg_reg(R::V2, R::V4);
        h.g.mov_imm64(R::V0, 27);
        h.g.asm.syscall();
        h.g.add_reg_imm32(R::V4, 80);
    });
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.collect();
    h.finish_collection();
    stop(&mut h);
    stop(&mut h);
    for (name, offset) in [
        ("stack", stack),
        ("tid", tid),
        ("tls", tls),
        ("joins", joins),
        ("releases", releases),
    ] {
        h.report(name, |h| h.g.load_ptr_disp32(R::V0, R::V3, offset));
    }
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_linux_worker_bootstrap_os_failure_joins_before_releasing_stack_and_falls_back() {
    let o = run(fallback, 1, true);
    assert_eq!(o.get("bootstrap"), -22, "{}", o.text());
    for name in ["stack", "tid", "tls"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
    for name in ["joins", "releases"] {
        assert_eq!(o.get(name), 1, "{}", o.text());
    }
    assert_eq!(o.get("mincore"), -12, "{}", o.text());
    assert_eq!(o.get("value"), 84, "{}", o.text());
}
#[test]
fn production_linux_worker_clone_failure_unmaps_stack_and_preserves_fallback_collection() {
    let o = run(fallback, 0, true);
    for name in ["stack", "tid", "tls", "joins"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
    assert_eq!(o.get("releases"), 1, "{}", o.text());
    assert_eq!(o.get("mincore"), -12, "{}", o.text());
    assert_eq!(o.get("value"), 84, "{}", o.text());
}
fn failure(g: &mut crate::NativeCodeGenerator, mode: u64) {
    g.linux_worker_fault = Some(match mode {
        0 => linux::Fault::Map,
        1 => linux::Fault::Protect,
        2 => linux::Fault::Join,
        3 => linux::Fault::Free,
        4 => linux::Fault::CloneCleanup,
        _ => unreachable!(),
    });
    let mut h = H::begin(g);
    stop(&mut h);
}
#[test]
fn production_linux_worker_reports_stack_join_release_and_startup_cleanup_failures() {
    for mode in 0..5 {
        let output = execute(failure, mode, true, false);
        assert_eq!(output.status.code(), Some(1), "mode{mode}: {output:?}");
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(
            error.contains(if mode < 2 {
                "GC worker stack allocation failed"
            } else {
                "GC worker lifecycle failed"
            }),
            "mode{mode}: {error}"
        );
    }
}
