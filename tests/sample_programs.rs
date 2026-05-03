use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Mutex;

static SIDE_EFFECT_SAMPLE_LOCK: Mutex<()> = Mutex::new(());

fn klassic_bin() -> &'static str {
    env!("CARGO_BIN_EXE_klassic")
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn sample_programs() -> Vec<&'static str> {
    vec![
        "arrow-kind-test.kl",
        "block-comment.kl",
        "boolean-literal.kl",
        "builtin_functions.kl",
        "builtin_functions-list.kl",
        "builtin_functions-thread.kl",
        "cleanup-expression.kl",
        "debug-hkt.kl",
        "distance.kl",
        "fanction-def-fact.kl",
        "file-input.kl",
        "file-output.kl",
        "function-params-evaluation-count.kl",
        "function_application.kl",
        "functions-simple.kl",
        "functions.kl",
        "functor-basic.kl",
        "higher-kinded-typeclass.kl",
        "hkt-application-test.kl",
        "hkt-no-constraints.kl",
        "line-comment.kl",
        "list-literal.kl",
        "list-map-debug.kl",
        "map-literal.kl",
        "map_syntax.kl",
        "monad-example.kl",
        "monad-with-newlines.kl",
        "mutable.kl",
        "numeric-literals.kl",
        "record.kl",
        "reduce_syntax.kl",
        "set-literal.kl",
        "simple-debug.kl",
        "stdlib_prelude.kl",
        "simple-function-type.kl",
        "simple-higher-kinded.kl",
        "string-interpolation.kl",
        "ternary-expression.kl",
        "test-curried-syntax.kl",
        "test-function-types.kl",
        "test-list-literal.kl",
        "test-method-names.kl",
        "test-methods-semicolon.kl",
        "test-monad-bind.kl",
        "test-monad-methods.kl",
        "test-multiple-methods.kl",
        "test-simple-types.kl",
        "test-type-syntax.kl",
        "test-typevar-functions.kl",
        "type-cast.kl",
        "typeclass-current-state.kl",
        "typeclass-example.kl",
        "working-monad-example.kl",
    ]
}

fn expected_stdout(program: &str) -> Option<&'static str> {
    match program {
        "arrow-kind-test.kl" => Some("Arrow kind parsed successfully\n"),
        "boolean-literal.kl" => Some("true\nfalse\n"),
        "builtin_functions-thread.kl" => {
            Some("Hello from main thread.\nHello from another thread.\n")
        }
        "debug-hkt.kl" => Some("Higher-kinded typeclass parsing test passed\n"),
        "file-output.kl" => Some(
            "File written successfully\n\
Content appended successfully\n\
File exists: true\n\
Multiple lines written\n\
File content: Hello, Klassic!\nAppended line\n\
Lines read: [Line 1, Line 2, Line 3]\n\
Test files cleaned up\n",
        ),
        "functor-basic.kl" => Some("Basic functor works\n"),
        "higher-kinded-typeclass.kl" => Some("Higher-kinded typeclasses parsed successfully\n"),
        "hkt-application-test.kl" => Some("Higher-kinded type application parsed successfully\n"),
        "hkt-no-constraints.kl" => Some("Original: [1, 2, 3, 4, 5]\nDoubled: [2, 4, 6, 8, 10]\n"),
        "list-map-debug.kl" => Some("Result: [2, 4, 6]\n"),
        "map-literal.kl" => Some("Map literals test passed\n"),
        "monad-example.kl" | "monad-with-newlines.kl" => {
            Some("Higher-kinded typeclasses parsed successfully\n")
        }
        "working-monad-example.kl" => Some("[2]\n"),
        "numeric-literals.kl" => Some(
            "100\n127\n-128\n100\n200\n300\n32767\n-32768\n2147483647\n-2147483648\n100\n200\n300\n9223372036854775807\n-9223372036854775808\n1.0\n1.5\n1.0\n1.5\n",
        ),
        "set-literal.kl" => Some("%(1, 2, 3)\n%(1, 2, 3)\n%(1, 2, 3)\n"),
        "simple-debug.kl" => Some("Result: [2, 4, 6]\n"),
        "stdlib_prelude.kl" => Some(
            "[0, 1, 2, 3, 4]\n\
[1, 2, 3, 4]\n\
[10, 20, 30]\n\
[30, 40, 50]\n\
[2, 4, 6]\n\
3\n\
true\n\
true\n\
2\n\
[7, 7, 7]\n\
3\n\
15\n\
24\n\
20\n\
10\n\
2\n\
true\n\
true\n\
[3, 2, 1]\n\
[2, 4, 6]\n\
6\n\
[1, 2, 3, 4]\n\
[1, 2, 3, 4, 5]\n\
true\n\
false\n\
2\n\
-1\n",
        ),
        "simple-function-type.kl" => Some("Function types parsed\n"),
        "test-curried-syntax.kl" => Some("Curried syntax works\n"),
        "test-function-types.kl" => Some("Function types work\n"),
        "test-list-literal.kl" => Some("[1, 2, 3]\n"),
        "test-method-names.kl" => Some("Method names work\n"),
        "test-methods-semicolon.kl" => Some("Semicolon works\n"),
        "test-monad-bind.kl" => Some("Monad bind works\n"),
        "test-monad-methods.kl" => Some("Monad methods work\n"),
        "test-multiple-methods.kl" => Some("Multiple methods work\n"),
        "test-simple-types.kl" => Some("Simple types work\n"),
        "test-type-syntax.kl" => Some("Type syntaxes work\n"),
        "test-typevar-functions.kl" => Some("Type variable functions work\n"),
        "type-cast.kl" => Some("100\n"),
        "typeclass-current-state.kl" => Some(
            "=== What works ===\n\
Direct calls:\n\
Int: 42\n\
String: hello\n\
\n\
Multiple type classes:\n\
5 == 5: true\n\
\"foo\" == \"bar\": false\n\
\n\
=== Rust parity status ===\n\
Constrained polymorphism works\n\
Higher-kinded List helpers work\n\
Proof/trust surface is verified\n",
        ),
        "typeclass-example.kl" => Some("Int: 42\nString: Hello\nList[3 elements]\n"),
        _ => None,
    }
}

fn expected_stderr(program: &str) -> Option<&'static str> {
    match program {
        "builtin_functions-thread.kl" => Some(""),
        "builtin_functions.kl" => Some("this param is displayed into standard error\n"),
        _ => Some(""),
    }
}

fn run_program(path: &Path) -> std::process::Output {
    Command::new(klassic_bin())
        .current_dir(repo_root())
        .arg(path)
        .output()
        .expect("klassic binary should run")
}

fn side_effect_sample_guard(program: &str) -> Option<std::sync::MutexGuard<'static, ()>> {
    if program == "file-output.kl" {
        Some(
            SIDE_EFFECT_SAMPLE_LOCK
                .lock()
                .expect("side-effect sample lock should not be poisoned"),
        )
    } else {
        None
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn run_native_program(path: &Path, output_path: &Path) -> std::process::Output {
    let build = Command::new(klassic_bin())
        .current_dir(repo_root())
        .args([
            "build",
            path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic native build should run");
    assert!(
        build.status.success(),
        "native build failed for {}\nstdout:\n{}\nstderr:\n{}",
        path.display(),
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(
        build.stdout.is_empty(),
        "native build stdout should be empty for {}:\n{}",
        path.display(),
        String::from_utf8_lossy(&build.stdout)
    );
    assert!(
        build.stderr.is_empty(),
        "native build stderr should be empty for {}:\n{}",
        path.display(),
        String::from_utf8_lossy(&build.stderr)
    );

    Command::new(output_path)
        .current_dir(repo_root())
        .output()
        .expect("generated native executable should run")
}

#[test]
fn top_level_sample_programs_succeed() {
    let root = repo_root().join("test-programs");
    let mut failures = Vec::new();

    for program in sample_programs() {
        let path = root.join(program);
        let _side_effect_guard = side_effect_sample_guard(program);
        let output = run_program(&path);
        if !output.status.success() {
            failures.push(format!(
                "{} failed\nstdout:\n{}\nstderr:\n{}",
                program,
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            ));
            continue;
        }

        if let Some(expected) = expected_stdout(program) {
            let actual = String::from_utf8_lossy(&output.stdout);
            if actual != expected {
                failures.push(format!(
                    "{} stdout mismatch\nexpected:\n{}\nactual:\n{}",
                    program, expected, actual
                ));
            }
        }

        if let Some(expected) = expected_stderr(program) {
            let actual = String::from_utf8_lossy(&output.stderr);
            if actual != expected {
                failures.push(format!(
                    "{} stderr mismatch\nexpected:\n{}\nactual:\n{}",
                    program, expected, actual
                ));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "sample program regressions:\n{}",
        failures.join("\n\n")
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_top_level_sample_programs_match_expected_outputs() {
    let root = repo_root().join("test-programs");
    let mut failures = Vec::new();

    for program in sample_programs() {
        let _side_effect_guard = side_effect_sample_guard(program);
        let unique = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let path = root.join(program);
        let output_path = std::env::temp_dir().join(format!(
            "klassic-native-sample-{}-{unique}",
            program.replace(['/', '.'], "-")
        ));
        let output = run_native_program(&path, &output_path);
        let _ = std::fs::remove_file(&output_path);

        if !output.status.success() {
            failures.push(format!(
                "{} native executable failed\nstdout:\n{}\nstderr:\n{}",
                program,
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            ));
            continue;
        }

        if let Some(expected) = expected_stdout(program) {
            let actual = String::from_utf8_lossy(&output.stdout);
            if actual != expected {
                failures.push(format!(
                    "{} native stdout mismatch\nexpected:\n{}\nactual:\n{}",
                    program, expected, actual
                ));
            }
        }

        if let Some(expected) = expected_stderr(program) {
            let actual = String::from_utf8_lossy(&output.stderr);
            if actual != expected {
                failures.push(format!(
                    "{} native stderr mismatch\nexpected:\n{}\nactual:\n{}",
                    program, expected, actual
                ));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "native sample program regressions:\n{}",
        failures.join("\n\n")
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_promoted_future_and_typeclass_examples_build_and_run() {
    let programs = [
        repo_root().join("test-programs/future-features/record_inference.kl"),
        repo_root().join("test-programs/future-features/typeclass-polymorphic-demo.kl"),
        repo_root().join("test-programs/future-features/typeclass-polymorphic.kl"),
        repo_root().join("test-programs/future-features/typeclass-usage.kl"),
        repo_root().join("examples/typeclass-complete.kl"),
        repo_root().join("examples/typeclass-final-example.kl"),
        repo_root().join("examples/typeclass-full.kl"),
    ];
    let mut failures = Vec::new();

    for (index, path) in programs.iter().enumerate() {
        let unique = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let output_path =
            std::env::temp_dir().join(format!("klassic-native-promoted-{index}-{unique}"));
        let output = run_native_program(path, &output_path);
        let _ = std::fs::remove_file(&output_path);
        if !output.status.success() {
            failures.push(format!(
                "{} native executable failed\nstdout:\n{}\nstderr:\n{}",
                path.display(),
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            ));
        }
    }

    assert!(
        failures.is_empty(),
        "native promoted program regressions:\n{}",
        failures.join("\n\n")
    );
}

#[test]
fn future_record_inference_program_now_succeeds() {
    let path = repo_root().join("test-programs/future-features/record_inference.kl");
    let output = run_program(&path);
    assert!(
        output.status.success(),
        "record_inference.kl failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn future_typeclass_polymorphic_demo_now_succeeds() {
    let path = repo_root().join("test-programs/future-features/typeclass-polymorphic-demo.kl");
    let output = run_program(&path);
    assert!(
        output.status.success(),
        "typeclass-polymorphic-demo.kl failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "Direct calls:\n\
Int(42)\n\
String(\"hello\")\n\
Bool(true)\n\
\n\
Polymorphic function:\n\
Int(123), Int(123)\n\
String(\"world\"), String(\"world\")\n\
\n\
Showing lists:\n\
[Int(1), Int(2), Int(3)]\n\
[String(\"a\"), String(\"b\"), String(\"c\")]\n\
\n\
Equality tests:\n\
true\n\
false\n\
true\n\
false\n"
    );
}

#[test]
fn future_typeclass_usage_program_now_succeeds() {
    let path = repo_root().join("test-programs/future-features/typeclass-usage.kl");
    let output = run_program(&path);
    assert!(
        output.status.success(),
        "typeclass-usage.kl failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "=== Show examples ===\n\
42\n\
\"Hello\"\n\
Point(10, 20)\n\
\n\
=== Eq examples ===\n\
42 == 42: true\n\
42 != 43: true\n\
p1 == p2: true\n\
p1 != p3: true\n\
\n\
=== Using showPair ===\n\
(1, 2)\n\
(\"foo\", \"bar\")\n\
(Point(10, 20), Point(5, 10))\n\
\n\
=== List of shown items ===\n\
100 | \"Klassic\" | Point(3, 4)\n"
    );
}

#[test]
fn future_typeclass_polymorphic_program_now_succeeds() {
    let path = repo_root().join("test-programs/future-features/typeclass-polymorphic.kl");
    let output = run_program(&path);
    assert!(
        output.status.success(),
        "typeclass-polymorphic.kl failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "=== Testing polymorphic display ===\n\
Displaying: Int(42)\n\
Displaying: \"Hello, Klassic!\"\n\
Displaying: true\n\
\n\
=== Testing showIfEqual ===\n\
They are equal: Int(10)\n\
Int(10) != Int(20)\n\
They are equal: \"foo\"\n\
\"foo\" != \"bar\"\n\
\n\
=== Testing showList ===\n\
Original: [Int(1),Int(2),Int(3),Int(4),Int(5)]\n\
As strings: Int(1), Int(2), Int(3), Int(4), Int(5)\n\
\n\
=== Testing with custom type ===\n\
Displaying: Person(name=\"Alice\", age=Int(30))\n\
Displaying: Person(name=\"Bob\", age=Int(25))\n\
They are equal: Person(name=\"Alice\", age=Int(30))\n\
Person(name=\"Alice\", age=Int(30)) != Person(name=\"Bob\", age=Int(25))\n"
    );
}
