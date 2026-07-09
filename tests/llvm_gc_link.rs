//! M7 foundation: proves the C garbage collector links and runs correctly
//! when driven from clang-compiled LLVM IR through its real ABI -- the same
//! calls the emitter will produce (`klassic_gc_init` / `klassic_gc_alloc` /
//! shadow-stack push / color-on-store `klassic_gc_write` / a full collection
//! / the inline load-barrier fast path validated in the M2 spike). The M2
//! spike covered only the barrier; this exercises allocation, precise roots,
//! and a whole mark/sweep/relocate cycle against the real collector. Gated on
//! a clang >= 15 (opaque pointers) being present so CI without one stays green.
#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::process::Command;

fn find_cc() -> Option<String> {
    if let Ok(explicit) = std::env::var("KLASSIC_CLANG") {
        return Some(explicit);
    }
    ["clang-18", "clang-17", "clang-16", "clang-15", "clang"]
        .into_iter()
        .find(|name| {
            Command::new(name)
                .arg("--version")
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .status()
                .is_ok_and(|status| status.success())
        })
        .map(str::to_owned)
}

/// Hand-written LLVM IR mirroring the emitter's eventual output: build a
/// two-field record of two int leaves, keep them on the shadow stack, force a
/// collection, then read a field back through the inline barrier fast path.
/// RAW_BYTES = 1, POINTER_RECORD = 2 (see klassic_gc.h).
const IR: &str = r#"
target triple = "x86_64-pc-linux-gnu"

@.fmt = private constant [5 x i8] c"%ld\0A\00"
@gc_bad_mask   = external dso_local global i64
@gc_strip_mask = external dso_local global i64

declare void @klassic_gc_init()
declare ptr @klassic_gc_alloc(i64, i64)
declare void @klassic_gc_shadow_push(ptr)
declare void @klassic_gc_shadow_pop_n(i64)
declare void @klassic_gc_write(ptr, ptr)
declare i64 @klassic_gc_load_barrier_slow(i64, ptr)
declare void @klassic_gc_collect()
declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  call void @klassic_gc_init()

  ; leaf_a = alloc(8, RAW_BYTES); *leaf_a = 100; root it
  %la = call ptr @klassic_gc_alloc(i64 8, i64 1)
  store i64 100, ptr %la
  %slot_a = alloca ptr
  store ptr %la, ptr %slot_a
  call void @klassic_gc_shadow_push(ptr %slot_a)

  ; leaf_b = alloc(8, RAW_BYTES); *leaf_b = 23; root it
  %lb = call ptr @klassic_gc_alloc(i64 8, i64 1)
  store i64 23, ptr %lb
  %slot_b = alloca ptr
  store ptr %lb, ptr %slot_b
  call void @klassic_gc_shadow_push(ptr %slot_b)

  ; pair = alloc(16, POINTER_RECORD); root it
  %pair = call ptr @klassic_gc_alloc(i64 16, i64 2)
  %slot_pair = alloca ptr
  store ptr %pair, ptr %slot_pair
  call void @klassic_gc_shadow_push(ptr %slot_pair)

  ; color-on-store the fields (reload leaves from their roots first)
  %la2 = load ptr, ptr %slot_a
  %lb2 = load ptr, ptr %slot_b
  %f0 = getelementptr i8, ptr %pair, i64 0
  call void @klassic_gc_write(ptr %f0, ptr %la2)
  %f1 = getelementptr i8, ptr %pair, i64 8
  call void @klassic_gc_write(ptr %f1, ptr %lb2)

  ; force a full mark/sweep/relocate cycle (roots get fixed)
  call void @klassic_gc_collect()

  ; reload the (possibly moved) pair from its root, read field 0 through the
  ; inline load-barrier fast path, then dereference the leaf.
  %pair2 = load ptr, ptr %slot_pair
  %f0b = getelementptr i8, ptr %pair2, i64 0
  %v = load i64, ptr %f0b
  %bm = load i64, ptr @gc_bad_mask
  %bad = and i64 %v, %bm
  %isb = icmp ne i64 %bad, 0
  br i1 %isb, label %slow, label %ok
slow:
  %healed = call i64 @klassic_gc_load_barrier_slow(i64 %v, ptr %f0b)
  br label %ok
ok:
  %val = phi i64 [ %v, %entry ], [ %healed, %slow ]
  %sm = load i64, ptr @gc_strip_mask
  %raw = and i64 %val, %sm
  %rawp = inttoptr i64 %raw to ptr
  %leafval = load i64, ptr %rawp

  call void @klassic_gc_shadow_pop_n(i64 3)
  %r = call i32 (ptr, ...) @printf(ptr @.fmt, i64 %leafval)
  ret i32 0
}
"#;

#[test]
fn c_gc_links_and_runs_from_llvm_ir() {
    let Some(cc) = find_cc() else {
        eprintln!("skipping: no clang >= 15 found");
        return;
    };
    let gc_src = concat!(env!("CARGO_MANIFEST_DIR"), "/runtime/gc/klassic_gc.c");
    let dir = std::env::temp_dir();
    let stamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or_default();
    let ll = dir.join(format!("klassic-gc-link-{stamp}.ll"));
    let exe = dir.join(format!("klassic-gc-link-{stamp}"));
    std::fs::write(&ll, IR).expect("write IR");

    let build = Command::new(&cc)
        .args(["-O2", "-o"])
        .arg(&exe)
        .arg(&ll)
        .arg(gc_src)
        .args(["-lm"])
        .output()
        .expect("clang runs");
    assert!(
        build.status.success(),
        "clang failed to build:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&exe).output().expect("program runs");
    let _ = std::fs::remove_file(&ll);
    let _ = std::fs::remove_file(&exe);
    assert!(
        run.status.success(),
        "program crashed: {:?}\nstderr:\n{}",
        run.status,
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "100\n",
        "the record's first field should read back as 100 after a collection"
    );
}
