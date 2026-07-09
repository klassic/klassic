//! Differential tests for the LLVM backend (`--backend llvm`, migration
//! plan `docs/llvm-backend-plan.md`). Each program is run through both
//! the evaluator (the oracle) and the LLVM backend, asserting
//! byte-identical stdout. Gated on a clang >= 15 being present so CI
//! without the toolchain stays green; the emitter itself is also covered
//! by hermetic unit tests in `crates/klassic-native/src/llvm.rs`.
#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn klassic_bin() -> &'static str {
    env!("CARGO_BIN_EXE_klassic")
}

/// A clang that accepts opaque-pointer IR, or None (tests then skip).
fn find_clang() -> Option<String> {
    if let Ok(explicit) = std::env::var("KLASSIC_CLANG") {
        return Some(explicit);
    }
    ["clang-18", "clang-17", "clang-16", "clang-15"]
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

/// Evaluate `source`, then build+run it through `--backend llvm`, and
/// assert the two stdout streams match. No-op when clang is absent.
fn assert_llvm_matches_evaluator(source: &str) {
    let Some(clang) = find_clang() else {
        eprintln!("skipping: no clang >= 15 found");
        return;
    };
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time after epoch")
        .as_nanos();
    let dir = std::env::temp_dir();
    let src = dir.join(format!("klassic_llvm_{stamp}.kl"));
    let bin = dir.join(format!("klassic_llvm_{stamp}.bin"));
    std::fs::write(&src, source).expect("write temp source");

    let eval = Command::new(klassic_bin())
        .arg(&src)
        .output()
        .expect("evaluator runs");
    assert!(
        eval.status.success(),
        "evaluator failed\nstderr:\n{}",
        String::from_utf8_lossy(&eval.stderr)
    );

    let build = Command::new(klassic_bin())
        .env("KLASSIC_CLANG", &clang)
        .args(["--backend", "llvm", "build"])
        .arg(&src)
        .arg("-o")
        .arg(&bin)
        .output()
        .expect("llvm build runs");
    assert!(
        build.status.success(),
        "--backend llvm build failed\nstderr:\n{}",
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&bin).output().expect("compiled binary runs");
    let _ = std::fs::remove_file(&src);
    let _ = std::fs::remove_file(&bin);
    assert!(
        run.status.success(),
        "compiled binary crashed\nstderr:\n{}",
        String::from_utf8_lossy(&run.stderr)
    );

    assert_eq!(
        String::from_utf8_lossy(&eval.stdout),
        String::from_utf8_lossy(&run.stdout),
        "LLVM backend output must match the evaluator for:\n{source}"
    );
}

#[test]
fn scalars_arithmetic_and_println() {
    assert_llvm_matches_evaluator(
        "println(2 * 3 + 4 * 5)\n\
         println(7 / 2)\n\
         println(3.5 + 1.25)\n\
         println((3.5 + 1.25) * 2.0)\n\
         println(10 > 3)\n\
         println(2.0 < 1.5)\n\
         println((5 > 1) && (2 == 2))\n\
         println(!(3 > 5))\n\
         println(-7)\n\
         println(-3.5 < -2.0)\n",
    );
}

#[test]
fn mutable_and_while_loop() {
    assert_llvm_matches_evaluator(
        "val n = 10\n\
         mutable sum = 0\n\
         mutable i = 1\n\
         while (i <= n) { sum = sum + i; i = i + 1 }\n\
         println(sum)\n",
    );
}

#[test]
fn if_expression_and_nested_blocks() {
    assert_llvm_matches_evaluator(
        "val x = 7\n\
         val y = if (x > 5) { val z = x * 2; z + 1 } else 0\n\
         println(y)\n\
         println(if (x > 0) 1 else if (x < 0) -1 else 0)\n",
    );
}

#[test]
fn functions_and_recursion() {
    assert_llvm_matches_evaluator(
        "def fact(n: Int): Int = if (n <= 1) 1 else n * fact(n - 1)\n\
         def fib(n: Int): Int = if (n < 2) n else fib(n - 1) + fib(n - 2)\n\
         def isEven(n: Int): Bool = n - (n / 2) * 2 == 0\n\
         println(fact(6))\n\
         println(fib(12))\n\
         println(isEven(10))\n\
         println(isEven(7))\n",
    );
}

#[test]
fn function_with_multiple_params_and_double() {
    assert_llvm_matches_evaluator(
        "def clamp(x: Int, lo: Int, hi: Int): Int = \
           if (x < lo) lo else if (x > hi) hi else x\n\
         def avg(a: Double, b: Double): Double = (a + b) / 2.0\n\
         println(clamp(99, 0, 10))\n\
         println(clamp(-3, 0, 10))\n\
         println(avg(3.0, 4.0))\n",
    );
}

// --- Heap records through the garbage collector (M7) ------------------

#[test]
fn record_construct_and_field_access() {
    assert_llvm_matches_evaluator(
        "val r = record { x: 10; y: 20 }\n\
         println(r.x + r.y)\n\
         val p = record { a: 3.5; b: 1.25 }\n\
         println(p.a + p.b)\n",
    );
}

#[test]
fn record_field_read_after_reassignment() {
    // Fields are read through the load barrier; exercise several reads and
    // arithmetic on a record held in a local.
    assert_llvm_matches_evaluator(
        "val point = record { x: 7; y: 11 }\n\
         mutable sum = 0\n\
         sum = sum + point.x\n\
         sum = sum + point.y\n\
         sum = sum + point.x * point.y\n\
         println(sum)\n",
    );
}

#[test]
fn record_survives_moving_collection() {
    // Churn enough short-lived records to force many moving collections while
    // a rooted survivor is kept; its fields must read back intact after being
    // relocated (the whole point of precise roots + the load barrier).
    assert_llvm_matches_evaluator(
        "val keeper = record { x: 314; y: 271 }\n\
         mutable i = 0\n\
         while (i < 200000) {\n\
           val garbage = record { x: i; y: i }\n\
           i = i + 1\n\
         }\n\
         println(keeper.x + keeper.y)\n",
    );
}
