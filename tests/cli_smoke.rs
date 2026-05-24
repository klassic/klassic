use std::fs;
use std::io::Write;
use std::process::{Command, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

fn klassic_bin() -> &'static str {
    env!("CARGO_BIN_EXE_klassic")
}

#[test]
fn evaluates_expression_argument() {
    let output = Command::new(klassic_bin())
        .args(["-e", "1 + 2"])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "3\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn evaluates_process_exit_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "println(\"before exit\")\nProcess#exit({ println(\"code path\"); 7 })\nprintln(\"after exit\")",
        ])
        .output()
        .expect("binary should run");

    assert_eq!(output.status.code(), Some(7));
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "before exit\ncode path\n"
    );
    assert!(output.stderr.is_empty());
}

#[test]
fn evaluates_standard_input_via_cli() {
    let mut child = Command::new(klassic_bin())
        .args([
            "-e",
            "val text = StandardInput#all()\nprintln(trimRight(text))\nprintln(length(text))\nassertResult(\"alpha\\nbeta\\n\")(text)",
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("binary should run");

    {
        let mut stdin = child.stdin.take().expect("stdin should be piped");
        stdin
            .write_all(b"alpha\nbeta\n")
            .expect("stdin should accept input");
    }

    let output = child
        .wait_with_output()
        .expect("binary should finish after stdin closes");

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "alpha\nbeta\n11\n()\n"
    );
    assert!(output.stderr.is_empty());
}

#[test]
fn evaluates_environment_vars_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "val vars = env()\nmutable found = false\nforeach(entry in vars) {\n  if(entry == \"KLASSIC_EVAL_ENV_TEST=alpha\") {\n    found = true\n  }\n}\nprintln(found)\nassert(found)",
        ])
        .env("KLASSIC_EVAL_ENV_TEST", "alpha")
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "true\n()\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn evaluates_environment_get_and_exists_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "println(getEnv(\"KLASSIC_EVAL_ENV_GET_TEST\"))\nprintln(hasEnv(\"KLASSIC_EVAL_ENV_GET_TEST\"))\nprintln(Environment#exists(\"KLASSIC_EVAL_ENV_GET_MISSING\"))\nassertResult(\"alpha\")(Environment#get(\"KLASSIC_EVAL_ENV_GET_TEST\"))",
        ])
        .env("KLASSIC_EVAL_ENV_GET_TEST", "alpha")
        .env_remove("KLASSIC_EVAL_ENV_GET_MISSING")
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "alpha\ntrue\nfalse\n()\n"
    );
    assert!(output.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_basic_program() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-basic-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-basic-{unique}"));
    fs::write(
        &source_path,
        "println(1 + 2)\nprintln(true)\nval parsed = {\n  rule {\n    S = \"a\";\n  }\n  7\n}\nprintln(parsed)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            "--target",
            "linux-x86_64",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\ntrue\n7\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_with_native_target_alias() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-alias-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-alias-{unique}"));
    fs::write(&source_path, "println(7 * 6)\n").expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            "--target",
            "native",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "native alias build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "42\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_with_linux_target_triple_alias() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-triple-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-triple-{unique}"));
    fs::write(&source_path, "println(6 * 7)\n").expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            "--target",
            "x86_64-unknown-linux-gnu",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "linux target triple alias build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "42\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_control_flow_and_mutation() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-control-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-control-{unique}"));
    fs::write(
        &source_path,
        "mutable i = 1\nwhile(i < 4) {\n  i += 1\n}\nif(i == 4) {\n  println(\"done\")\n}\nmutable total = 0\nforeach(e in [1, 2, 3]) {\n  total += e\n}\nprintln(total)\nassertResult(6)(total)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "done\n6\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_integer_functions() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-fact-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-fact-{unique}"));
    fs::write(
        &source_path,
        "def fact(n) = if(n < 2) 1 else n * fact(n - 1)\nprintln(fact(5))\nassertResult(120)(fact(5))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "120\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_runtime_string_parameter() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-string-param-{unique}.kl"));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-string-param-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-string-param-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"def countA(s: String, i: Int): Int = if(i >= length(s)) 0 else if(s.at(i) == "a") 1 + countA(s, i + 1) else countA(s, i + 1)
val text = FileInput#all("{}")
println(countA(text, 0))
assertResult(3)(countA(text, 0))
"#,
            input_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "recursive runtime string build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path, "banana").expect("input source should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "recursive runtime string run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_runtime_string_parameter_rewrite() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-string-param-rewrite-{unique}.kl"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-string-param-rewrite-{unique}.txt"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-string-param-rewrite-{unique}"
    ));
    fs::write(
        &source_path,
        format!(
            r#"def consume(s: String, i: Int): Int = if(i >= length(s)) i else consume(substring(s, 1, length(s)), i + 1)
def sumOldLengths(s: String, acc: Int): Int = if(isEmptyString(s)) acc else sumOldLengths(substring(s, 1, length(s)), acc + length(s))
val text = FileInput#all("{}")
println(consume(text, 0))
println(sumOldLengths(text, 0))
assertResult(3)(consume(text, 0))
assertResult(21)(sumOldLengths(text, 0))
"#,
            input_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "recursive runtime string rewrite build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path, "banana").expect("input source should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "recursive runtime string rewrite run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n21\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_runtime_line_list_parameter() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-lines-param-{unique}.kl"));
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-lines-param-{unique}.txt"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-lines-param-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"def countLines(lines: List<String>, i: Int): Int = if(i >= lines.size()) i else countLines(lines, i + 1)
val lines = FileInput#lines("{}")
println(countLines(lines, 0))
println(countLines(["one", "two"], 0))
assertResult(3)(countLines(lines, 0))
assertResult(2)(countLines(["one", "two"], 0))
"#,
            input_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "recursive runtime line-list build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path, "alpha\nbeta\ngamma")
        .expect("input source should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "recursive runtime line-list run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n2\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_runtime_line_list_parameter_rewrite() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-lines-param-rewrite-{unique}.kl"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-lines-param-rewrite-{unique}.txt"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-lines-param-rewrite-{unique}"
    ));
    fs::write(
        &source_path,
        format!(
            r#"def consume(lines: List<String>, count: Int): Int = if(lines.isEmpty()) count else consume(tail(lines), count + 1)
def sumOldSizes(lines: List<String>, acc: Int): Int = if(lines.isEmpty()) acc else sumOldSizes(tail(lines), acc + lines.size())
val lines = FileInput#lines("{}")
println(consume(lines, 0))
println(sumOldSizes(lines, 0))
assertResult(3)(consume(lines, 0))
assertResult(6)(sumOldSizes(lines, 0))
"#,
            input_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "recursive runtime line-list rewrite build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path, "alpha\nbeta\ngamma")
        .expect("input source should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "recursive runtime line-list rewrite run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n6\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_reentrant_runtime_parameter_staging() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-reentrant-runtime-param-{unique}.kl"
    ));
    let first_text_path = std::env::temp_dir().join(format!(
        "klassic-native-reentrant-runtime-param-first-{unique}.txt"
    ));
    let second_text_path = std::env::temp_dir().join(format!(
        "klassic-native-reentrant-runtime-param-second-{unique}.txt"
    ));
    let first_lines_path = std::env::temp_dir().join(format!(
        "klassic-native-reentrant-runtime-param-first-lines-{unique}.txt"
    ));
    let second_lines_path = std::env::temp_dir().join(format!(
        "klassic-native-reentrant-runtime-param-second-lines-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-reentrant-runtime-param-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"def lengthPlus(s: String, n: Int): Int = length(s) + n
def sizePlus(lines: List<String>, n: Int): Int = lines.size() + n
val first = FileInput#all("{}")
val second = FileInput#all("{}")
val firstLines = FileInput#lines("{}")
val secondLines = FileInput#lines("{}")
println(lengthPlus(first, lengthPlus(second, 0)))
println(sizePlus(firstLines, sizePlus(secondLines, 0)))
assertResult(7)(lengthPlus(first, lengthPlus(second, 0)))
assertResult(4)(sizePlus(firstLines, sizePlus(secondLines, 0)))
"#,
            first_text_path.display(),
            second_text_path.display(),
            first_lines_path.display(),
            second_lines_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "reentrant runtime parameter build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&first_text_path, "hello").expect("first text should write after native build");
    fs::write(&second_text_path, "xy").expect("second text should write after native build");
    fs::write(&first_lines_path, "a\nb\nc").expect("first lines should write after native build");
    fs::write(&second_lines_path, "z").expect("second lines should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&first_text_path);
    let _ = fs::remove_file(&second_text_path);
    let _ = fs::remove_file(&first_lines_path);
    let _ = fs::remove_file(&second_lines_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "reentrant runtime parameter run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "7\n4\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_mutable_runtime_string_and_line_list_bindings() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-mutable-runtime-{unique}.kl"));
    let text_path =
        std::env::temp_dir().join(format!("klassic-native-mutable-runtime-text-{unique}.txt"));
    let lines_path =
        std::env::temp_dir().join(format!("klassic-native-mutable-runtime-lines-{unique}.txt"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-mutable-runtime-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"mutable text = FileInput#all("{}")
val currentText = () => text
text = text + "!"
text = toUpperCase(text)
println(currentText())
println(text)

mutable rest = FileInput#lines("{}")
val currentHead = () => head(rest)
println(currentHead())
assertResult("a")(currentHead())
mutable joined = ""
while(!rest.isEmpty()) {{
  joined = joined + head(rest)
  rest = tail(rest)
}}
println(joined)
rest = ["x", "y"]
rest = cons("z")(rest)
println(join(rest, "|"))
assertResult("HELLO!")(currentText())
assertResult("HELLO!")(text)
assertResult("abc")(joined)
assertResult(["z", "x", "y"])(rest)
"#,
            text_path.display(),
            lines_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "mutable runtime binding build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&text_path, "hello").expect("text should write after native build");
    fs::write(&lines_path, "a\nb\nc").expect("lines should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&text_path);
    let _ = fs::remove_file(&lines_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "mutable runtime binding run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "HELLO!\nHELLO!\na\nabc\nz|x|y\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_runtime_string_return() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-string-return-{unique}.kl"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-string-return-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-string-return-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"def reverseFrom(s: String, i: Int): String = if(i < 0) "" else s.at(i) + reverseFrom(s, i - 1)
val text = FileInput#all("{}")
println(reverseFrom(text, length(text) - 1))
println(reverseFrom("xy", 1) + reverseFrom("ab", 1))
assertResult("cba")(reverseFrom(text, length(text) - 1))
assertResult("yxba")(reverseFrom("xy", 1) + reverseFrom("ab", 1))
"#,
            input_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "recursive runtime string return build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path, "abc").expect("input source should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "recursive runtime string return run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "cba\nyxba\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_runtime_line_list_return() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-lines-return-{unique}.kl"));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-lines-return-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-lines-return-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"def keepLines(lines: List<String>, n: Int): List<String> = if(n <= 0) lines else keepLines(lines, n - 1)
val lines = FileInput#lines("{}")
println(join(keepLines(lines, 2), "|"))
println(join(keepLines(["x", "y"], 1), ":"))
assertResult(["a", "b", "c"])(keepLines(lines, 2))
assertResult(["x", "y"])(keepLines(["x", "y"], 1))
"#,
            input_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "recursive runtime line-list return build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path, "a\nb\nc").expect("input source should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "recursive runtime line-list return run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "a|b|c\nx:y\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_return_function_aliases() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-return-alias-{unique}.kl"));
    let text_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-return-alias-{unique}.txt"));
    let lines_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-return-alias-lines-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-return-alias-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"def reverseFrom(s: String, i: Int): String = if(i < 0) "" else s.at(i) + reverseFrom(s, i - 1)
def keepLines(lines: List<String>, n: Int): List<String> = if(n <= 0) lines else keepLines(lines, n - 1)
val rev = reverseFrom
val keep = keepLines
mutable cleanups = 0
val text = FileInput#all("{}")
val lines = FileInput#lines("{}")
println(rev(text, length(text) - 1) + rev("xy", 1))
println(join(keep(lines, 2), "|") + "/" + join(keep(["x", "y"], 1), ":"))
println(({{ rev }})(text, length(text) - 1) + ({{ rev }} cleanup {{ cleanups += 1 }})("xy", 1))
println(join(({{ keep }} cleanup {{ cleanups += 10 }})(lines, 2), "|"))
assertResult("cbayx")(rev(text, length(text) - 1) + rev("xy", 1))
assertResult("a|b|c/x:y")(join(keep(lines, 2), "|") + "/" + join(keep(["x", "y"], 1), ":"))
assertResult(11)(cleanups)
"#,
            text_path.display(),
            lines_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime return alias build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&text_path, "abc").expect("text input should write after native build");
    fs::write(&lines_path, "a\nb\nc").expect("lines input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&text_path);
    let _ = fs::remove_file(&lines_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime return alias run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "cbayx\na|b|c/x:y\ncbayx\na|b|c\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_return_record_function_fields() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-return-field-{unique}.kl"));
    let text_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-return-field-{unique}.txt"));
    let lines_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-return-field-lines-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-return-field-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"def reverseFrom(s: String, i: Int): String = if(i < 0) "" else s.at(i) + reverseFrom(s, i - 1)
def keepLines(lines: List<String>, n: Int): List<String> = if(n <= 0) lines else keepLines(lines, n - 1)
val funcs = record {{ rev: reverseFrom, keep: keepLines }}
val text = FileInput#all("{}")
val lines = FileInput#lines("{}")
println(funcs.rev(text, length(text) - 1) + funcs.rev("xy", 1))
println(join(funcs.keep(lines, 2), "|") + "/" + join(funcs.keep(["x", "y"], 1), ":"))
assertResult("cbayx")(funcs.rev(text, length(text) - 1) + funcs.rev("xy", 1))
assertResult("a|b|c/x:y")(join(funcs.keep(lines, 2), "|") + "/" + join(funcs.keep(["x", "y"], 1), ":"))
"#,
            text_path.display(),
            lines_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime return field build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&text_path, "abc").expect("text input should write after native build");
    fs::write(&lines_path, "a\nb\nc").expect("lines input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&text_path);
    let _ = fs::remove_file(&lines_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime return field run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "cbayx\na|b|c/x:y\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_conditional_runtime_return_calls() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-conditional-return-call-{unique}.kl"
    ));
    let text_path = std::env::temp_dir().join(format!(
        "klassic-native-conditional-return-call-{unique}.txt"
    ));
    let lines_path = std::env::temp_dir().join(format!(
        "klassic-native-conditional-return-call-lines-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-conditional-return-call-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"def reverseFrom(s: String, i: Int): String = if(i < 0) "" else s.at(i) + reverseFrom(s, i - 1)
def markedReverseFrom(s: String, i: Int): String = if(i < 0) "!" else s.at(i) + markedReverseFrom(s, i - 1)
def keepLines(lines: List<String>, n: Int): List<String> = if(n <= 0) lines else keepLines(lines, n - 1)
def dropHead(lines: List<String>, n: Int): List<String> = tail(lines)
val usePlain = size(CommandLine#args()) == 0
mutable picks = 0
val chosenText = if({{ picks += 1; usePlain }}) reverseFrom else markedReverseFrom
val chosenLines = if(usePlain) keepLines else dropHead
val stringFns = [if({{ picks += 10; usePlain }}) reverseFrom else markedReverseFrom]
val lineFns = [if(usePlain) keepLines else dropHead]
val recordFns = record {{ text: if(usePlain) reverseFrom else markedReverseFrom, lines: if(usePlain) keepLines else dropHead }}
val stringMap = %["text": if(usePlain) reverseFrom else markedReverseFrom]
val lineMap = %["lines": if(usePlain) keepLines else dropHead]
val text = FileInput#all("{}")
val lines = FileInput#lines("{}")
val pickedText = (if(usePlain) reverseFrom else markedReverseFrom)(text, length(text) - 1)
val pickedLines = (if(usePlain) keepLines else dropHead)(lines, 1)
println(pickedText + (if(usePlain) reverseFrom else markedReverseFrom)("xy", 1))
println((if(usePlain) reverseFrom else markedReverseFrom)(text, length(text) - 1) + "!")
println(join(pickedLines, "|"))
println(picks)
println(chosenText(text, length(text) - 1))
println(join(chosenLines(lines, 1), "|"))
println(head(stringFns)(text, length(text) - 1))
println(join(head(lineFns)(lines, 1), "|"))
println(recordFns.text(text, length(text) - 1))
println(join(recordFns.lines(lines, 1), "|"))
println(Map#get(stringMap, "text")(text, length(text) - 1))
println(join(Map#get(lineMap, "lines")(lines, 1), "|"))
println(picks)
assertResult(11)(picks)
	"#,
            text_path.display(),
            lines_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "conditional return call build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&text_path, "abc").expect("text input should write after native build");
    fs::write(&lines_path, "a\nb\nc").expect("lines input should write after native build");
    let true_run = Command::new(&output_path)
        .output()
        .expect("generated executable should run true branch");
    let false_run = Command::new(&output_path)
        .arg("pick-marked")
        .output()
        .expect("generated executable should run false branch");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&text_path);
    let _ = fs::remove_file(&lines_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        true_run.status.success(),
        "conditional return call true branch failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&true_run.stdout),
        String::from_utf8_lossy(&true_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&true_run.stdout),
        "cbayx\ncba!\na|b|c\n11\ncba\na|b|c\ncba\na|b|c\ncba\na|b|c\ncba\na|b|c\n11\n"
    );
    assert!(true_run.stderr.is_empty());

    assert!(
        false_run.status.success(),
        "conditional return call false branch failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&false_run.stdout),
        String::from_utf8_lossy(&false_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&false_run.stdout),
        "cba!yx!\ncba!!\nb|c\n11\ncba!\nb|c\ncba!\nb|c\ncba!\nb|c\ncba!\nb|c\n11\n"
    );
    assert!(false_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_return_list_function_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-return-list-fn-{unique}.kl"));
    let text_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-return-list-fn-{unique}.txt"
    ));
    let lines_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-return-list-fn-lines-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-return-list-fn-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"def reverseFrom(s: String, i: Int): String = if(i < 0) "" else s.at(i) + reverseFrom(s, i - 1)
def keepLines(lines: List<String>, n: Int): List<String> = if(n <= 0) lines else keepLines(lines, n - 1)
val stringFn = reverseFrom
val lineFn = keepLines
val stringFns = [reverseFrom, reverseFrom]
val lineFns = [keepLines, keepLines]
val text = FileInput#all("{}")
val lines = FileInput#lines("{}")
println(head(stringFns)(text, length(text) - 1) + stringFns.head()("xy", 1))
println(join(head(lineFns)(lines, 2), "|"))
println(head(tail(stringFns))(text, length(text) - 1) + stringFns.tail().head()("xy", 1))
println(join(head(tail(lineFns))(lines, 2), "|"))
println(head(cons(stringFn)(stringFns))(text, length(text) - 1) + cons(stringFn)(stringFns).head()("xy", 1))
println(join(head(cons(lineFn)(lineFns))(lines, 2), "|"))
assertResult("cbayx")(head(stringFns)(text, length(text) - 1) + stringFns.head()("xy", 1))
assertResult(["a", "b", "c"])(lineFns.head()(lines, 2))
assertResult("cbayx")(head(tail(stringFns))(text, length(text) - 1) + stringFns.tail().head()("xy", 1))
assertResult(["a", "b", "c"])(lineFns.tail().head()(lines, 2))
assertResult("cbayx")(head(cons(stringFn)(stringFns))(text, length(text) - 1) + cons(stringFn)(stringFns).head()("xy", 1))
assertResult(["a", "b", "c"])(head(cons(lineFn)(lineFns))(lines, 2))
"#,
            text_path.display(),
            lines_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime return list function build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&text_path, "abc").expect("text input should write after native build");
    fs::write(&lines_path, "a\nb\nc").expect("lines input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&text_path);
    let _ = fs::remove_file(&lines_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime return list function run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "cbayx\na|b|c\ncbayx\na|b|c\ncbayx\na|b|c\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_return_map_function_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-return-map-fn-{unique}.kl"));
    let text_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-return-map-fn-{unique}.txt"));
    let lines_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-return-map-fn-lines-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-return-map-fn-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"def reverseFrom(s: String, i: Int): String = if(i < 0) "" else s.at(i) + reverseFrom(s, i - 1)
def keepLines(lines: List<String>, n: Int): List<String> = if(n <= 0) lines else keepLines(lines, n - 1)
def plusOne(x: Int): Int = x + 1
def plusTwo(x: Int): Int = x + 2
val stringFns = %["reverse": reverseFrom]
val lineFns = %["keep": keepLines]
val intFns = %["one": plusOne, "two": plusTwo]
val builtinFns = %["lower": toLowerCase, "upper": toUpperCase]
val builtinFnsByLength = %[3: toLowerCase, 5: toUpperCase]
val builtinFnsByFlag = %[true: toUpperCase, false: toLowerCase]
val sameBuiltinFns = %["upper": toUpperCase, "again": toUpperCase]
val lineGroups = %["keep": ["a", "b", "c"], "short": ["x"]]
val staticLineOptions = [["a", "b", "c"], ["x"]]
val suffix = "verse"
val keepKey = "ke" + "ep"
val runtimeStringKey = head(args())
val runtimeLineKey = head(tail(args()))
val runtimeBuiltinKey = head(tail(tail(args())))
val runtimeIntFnKey = head(tail(tail(tail(args()))))
val runtimeCapturedFnKey = head(tail(tail(tail(tail(args())))))
val text = FileInput#all("{}")
val lines = FileInput#lines("{}")
val delta = length(runtimeStringKey)
val capturedFns = %["add": (x: Int) => x + delta, "sub": (x: Int) => x - delta]
println(Map#get(stringFns, "reverse")(text, length(text) - 1) + stringFns.get("reverse")("xy", 1))
println(join(Map#get(lineFns, "keep")(lines, 2), "|"))
println(Map#get(stringFns, "re" + suffix)(text, length(text) - 1) + stringFns.get("re" + suffix)("xy", 1))
println(join(Map#get(lineFns, keepKey)(lines, 2), "|"))
println(Map#get(stringFns, runtimeStringKey)(text, length(text) - 1))
println(stringFns.get(runtimeStringKey)("xy", 1))
println(join(Map#get(lineFns, runtimeLineKey)(lines, 2), "|"))
println(Map#get(builtinFns, runtimeBuiltinKey)("AbC"))
val pickedBuiltin = Map#get(builtinFns, runtimeBuiltinKey)
println(pickedBuiltin)
println("picked=#{{pickedBuiltin}}")
println("picked=" + pickedBuiltin)
println(toString(pickedBuiltin))
val missingBuiltin = Map#get(builtinFns, "missing-" + runtimeBuiltinKey)
println("missing=#{{missingBuiltin}}")
println("missing=" + missingBuiltin)
println(toString(missingBuiltin))
println(pickedBuiltin("AbC"))
val pickedLengthBuiltin = Map#get(builtinFnsByLength, length(runtimeBuiltinKey))
println(pickedLengthBuiltin)
println(pickedLengthBuiltin("AbC"))
val pickedFlagBuiltin = Map#get(builtinFnsByFlag, runtimeBuiltinKey == "upper")
println(pickedFlagBuiltin)
println(pickedFlagBuiltin("AbC"))
val pickedIntFn = Map#get(intFns, runtimeIntFnKey)
println(pickedIntFn(40))
val pickedCapturedFn = Map#get(capturedFns, runtimeCapturedFnKey)
println("captured=#{{pickedCapturedFn}}")
println("captured=" + pickedCapturedFn)
println(toString(pickedCapturedFn))
println(pickedBuiltin == pickedBuiltin)
println(pickedBuiltin != pickedCapturedFn)
println(pickedBuiltin == "not-a-function")
println(pickedCapturedFn(10))
val pickedSameBuiltin = Map#get(sameBuiltinFns, runtimeBuiltinKey)
println(pickedSameBuiltin)
println(pickedSameBuiltin("AbC"))
println(join(Map#get(lineGroups, runtimeLineKey), "|"))
println(lineGroups.containsValue(Map#get(lineGroups, runtimeLineKey)))
println(staticLineOptions.contains(Map#get(lineGroups, runtimeLineKey)))
assertResult("cbayx")(Map#get(stringFns, "reverse")(text, length(text) - 1) + stringFns.get("reverse")("xy", 1))
assertResult(["a", "b", "c"])(lineFns.get("keep")(lines, 2))
assertResult("cbayx")(Map#get(stringFns, "re" + suffix)(text, length(text) - 1) + stringFns.get("re" + suffix)("xy", 1))
assertResult(["a", "b", "c"])(Map#get(lineFns, keepKey)(lines, 2))
assertResult("cba")(Map#get(stringFns, runtimeStringKey)(text, length(text) - 1))
assertResult("yx")(stringFns.get(runtimeStringKey)("xy", 1))
assertResult(["a", "b", "c"])(Map#get(lineFns, runtimeLineKey)(lines, 2))
assertResult("ABC")(Map#get(builtinFns, runtimeBuiltinKey)("AbC"))
assertResult("ABC")(pickedBuiltin("AbC"))
assertResult("<builtin:toUpperCase>")(toString(pickedBuiltin))
assertResult("picked=<builtin:toUpperCase>")("picked=" + pickedBuiltin)
assertResult("null")(toString(missingBuiltin))
assertResult("missing=null")("missing=" + missingBuiltin)
assertResult("ABC")(pickedLengthBuiltin("AbC"))
assertResult("ABC")(pickedFlagBuiltin("AbC"))
assertResult(42)(pickedIntFn(40))
assertResult("<function>")(toString(pickedCapturedFn))
assertResult("captured=<function>")("captured=" + pickedCapturedFn)
assert(!(pickedBuiltin == pickedBuiltin))
assert(pickedBuiltin != pickedCapturedFn)
assert(!(pickedBuiltin == "not-a-function"))
assertResult(17)(pickedCapturedFn(10))
assertResult("ABC")(pickedSameBuiltin("AbC"))
assertResult(["a", "b", "c"])(Map#get(lineGroups, runtimeLineKey))
assert(lineGroups.containsValue(Map#get(lineGroups, runtimeLineKey)))
assert(staticLineOptions.contains(Map#get(lineGroups, runtimeLineKey)))
"#,
            text_path.display(),
            lines_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime return map function build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&text_path, "abc").expect("text input should write after native build");
    fs::write(&lines_path, "a\nb\nc").expect("lines input should write after native build");
    let run = Command::new(&output_path)
        .arg("reverse")
        .arg("keep")
        .arg("upper")
        .arg("two")
        .arg("add")
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&text_path);
    let _ = fs::remove_file(&lines_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime return map function run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "cbayx\na|b|c\ncbayx\na|b|c\ncba\nyx\na|b|c\nABC\n<builtin:toUpperCase>\npicked=<builtin:toUpperCase>\npicked=<builtin:toUpperCase>\n<builtin:toUpperCase>\nmissing=null\nmissing=null\nnull\nABC\n<builtin:toUpperCase>\nABC\n<builtin:toUpperCase>\nABC\n42\ncaptured=<function>\ncaptured=<function>\n<function>\nfalse\ntrue\nfalse\n17\n<builtin:toUpperCase>\nABC\na|b|c\ntrue\ntrue\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_function_static_top_level_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-static-capture-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-static-capture-{unique}"));
    fs::write(
        &source_path,
        "val one = 1\ndef fact(n: Int): Int = if(n < 2) one else n * fact(n - one)\nprintln(fact(5))\nassertResult(120)(fact(5))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "120\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_folded_recursive_list_function() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-static-list-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-static-list-{unique}"));
    fs::write(
        &source_path,
        "def sum(xs: List<Int>): Int = if(isEmpty(xs)) 0 else head(xs) + sum(tail(xs))\n\
def joinLoop(xs: List<String>): String = if(isEmpty(xs)) \"\" else head(xs) + joinLoop(tail(xs))\n\
def down(n: Int): List<Int> = if(n < 1) [] else cons(n)(down(n - 1))\n\
def decorate(xs: List<String>): List<String> = if(isEmpty(xs)) [] else cons(head(xs) + \"!\")(decorate(tail(xs)))\n\
def myMap(xs, f) = if(isEmpty(xs)) [] else cons(f(head(xs)))(myMap(tail(xs), f))\n\
def bump(x: Int): Int = x + 1\n\
val addTen = (x: Int) => x + 10\n\
println(sum([1, 2, 3, 4]))\n\
println(joinLoop([\"a\", \"b\", \"c\"]))\n\
println(down(3))\n\
println(decorate([\"a\", \"b\"]))\n\
println(myMap([1, 2, 3], (x) => x + 1))\n\
println(myMap([1, 2, 3], bump))\n\
println(myMap([1, 2], addTen))\n\
println(myMap([\"a\", \"b\"], toUpperCase))\n\
assertResult(10)(sum([1, 2, 3, 4]))\n\
assertResult(\"abc\")(joinLoop([\"a\", \"b\", \"c\"]))\n\
assertResult([3, 2, 1])(down(3))\n\
assertResult([\"a!\", \"b!\"])(decorate([\"a\", \"b\"]))\n\
assertResult([2, 3, 4])(myMap([1, 2, 3], (x) => x + 1))\n\
assertResult([2, 3, 4])(myMap([1, 2, 3], bump))\n\
assertResult([11, 12])(myMap([1, 2], addTen))\n\
assertResult([\"A\", \"B\"])(myMap([\"a\", \"b\"], toUpperCase))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "recursive static list function build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "recursive static list function run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "10\nabc\n[3, 2, 1]\n[a!, b!]\n[2, 3, 4]\n[2, 3, 4]\n[11, 12]\n[A, B]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_function_builtin_alias_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-builtin-capture-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-builtin-capture-{unique}"));
    fs::write(
        &source_path,
        "val print = println\ndef countdown(n: Int): Int = if(n < 1) 0 else {\n  print(n)\n  countdown(n - 1)\n}\nprintln(countdown(3))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n2\n1\n0\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_function_lambda_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-lambda-capture-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-lambda-capture-{unique}"));
    fs::write(
        &source_path,
        "val inc = (x) => x + 1\ndef repeatInc(n: Int, x: Int): Int = if(n < 1) x else repeatInc(n - 1, inc(x))\nprintln(repeatInc(3, 0))\nassertResult(3)(repeatInc(3, 0))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_user_function_direct_call_shadows_builtin_name() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-function-shadows-builtin-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-function-shadows-builtin-{unique}"));
    fs::write(
        &source_path,
        "def repeat(n: Int): Int = n + 1\nval r = repeat\nprintln(repeat(1))\nprintln(r(2))\nassertResult(2)(repeat(1))\nassertResult(3)(r(2))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "2\n3\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_boolean_function_arguments_and_returns() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-bool-fn-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-bool-fn-{unique}"));
    fs::write(
        &source_path,
        "def isTwo(n: Int): Boolean = n == 2\ndef isThree(n) = n == 3\nval isSmall = (x) => x < 5\ndef choose(flag: Boolean): Int = if(flag) 10 else 20\nprintln(isTwo(2))\nprintln(isThree(3))\nprintln(isSmall(4))\nprintln(choose(isTwo(1)))\nassert(isTwo(2) && isThree(3) && isSmall(4) && !isTwo(3))\nassertResult(false)(isTwo(3))\nassertResult(true)(isThree(3))\nassertResult(true)(isSmall(4))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "true\ntrue\ntrue\n20\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_stack_argument_functions() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-stack-args-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-stack-args-{unique}"));
    fs::write(
        &source_path,
        "def encode7(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int): Int = a * 1000000 + b * 100000 + c * 10000 + d * 1000 + e * 100 + f * 10 + g\ndef encode8(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int): Int = a * 10000000 + b * 1000000 + c * 100000 + d * 10000 + e * 1000 + f * 100 + g * 10 + h\ndef makeAdder(n: Int) = (x: Int) => x + n\nval add2 = makeAdder(2)\nval t = stopwatch( => 1)\nval seven = encode7(1, 2, 3, 4, 5, 6, t) - t\nval eight = encode8(1, 2, 3, 4, 5, 6, t, 8) - t * 10\nval withClosureArg = encode7(1, add2(1), 3, 4, 5, 6, 7)\nprintln(seven)\nprintln(eight)\nprintln(withClosureArg)\nassertResult(1234560)(seven)\nassertResult(12345608)(eight)\nassertResult(1334567)(withClosureArg)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "1234560\n12345608\n1334567\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_top_level_lambda_bindings() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-lambda-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-lambda-{unique}"));
    fs::write(
        &source_path,
        "def inc(x: Int): Int = x + 1\ndef dec(x: Int): Int = x - 1\nval add = (x, y) => x + y\nmutable f = inc\nprintln(inc)\nprintln([inc])\nprintln(record { f: inc })\nprintln(add)\nprintln(add(2, 3))\nprintln(f)\nprintln(f(2))\nf = dec\nprintln(f(2))\nmutable g = (x) => x + 1\nprintln(g)\nprintln(g(2))\ng = (x) => x + 2\nprintln(g(2))\nval base = 3\nmutable noisy = (x) => { println(\"noise\"); x + base }\nmutable n = 4\nprintln(noisy(n))\nnoisy = (x) => { println(\"again\"); x + 4 }\nprintln(noisy(n))\nprintln(({ println(\"pick\"); noisy })(n))\nassertResult(5)(add(2, 3))\nassertResult(1)(f(2))\nassertResult(4)(g(2))\nassertResult(8)(noisy(n))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "<function>\n[<function>]\n#(<function>)\n<function>\n5\n<function>\n3\n1\n<function>\n3\n4\nnoise\n7\nagain\n8\npick\nagain\n8\nagain\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_functions_capturing_top_level_bindings() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-top-level-captures-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-top-level-captures-{unique}"));
    fs::write(
        &source_path,
        "val base = 40\nmutable counter = 0\nval print = println\ndef addBase(x: Int): Int = x + base + 2\ndef bump(x: Int): Int = {\n  counter += x\n  counter\n}\ndef say(x: Int): Int = {\n  print(\"say\")\n  x\n}\nval t = stopwatch(() => 1)\nprintln(addBase(t) - t)\nprintln(bump(2))\nprintln(bump(3))\nprintln(say(7))\nassertResult(5)(counter)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "42\n2\n5\nsay\n7\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_block_local_mutable_closure_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-local-closure-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-local-closure-{unique}"));
    fs::write(
        &source_path,
        "val f = {\n  mutable x = 0\n  (y) => {\n    x = x + 1\n    x + y\n  }\n}\nprintln(f(1))\nprintln(f(1))\nassertResult(4)(f(1))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "2\n3\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_mutable_static_capture_binding_identity() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-static-capture-id-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-static-capture-id-{unique}"));
    fs::write(
        &source_path,
        "mutable xs = [\"a\", \"b\"]\nval first = () => head(xs)\nprintln(first())\nxs = tail(xs)\nprintln(first())\nassertResult(\"b\")(first())\n\nmutable row = record { x: 1 }\nval get = () => row.x\nprintln(get())\nrow = record { x: 2 }\nprintln(get())\nassertResult(2)(get())\n\nval x = 1\nval f = () => x\n{\n  val x = 2\n  println(f())\n  assertResult(1)(f())\n}\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "a\nb\n1\n2\n1\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_record_closures_sharing_local_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-record-closure-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-record-closure-{unique}"));
    fs::write(
        &source_path,
        "val pair = {\n  mutable x = 0\n  val inc = (y) => {\n    x = x + y\n    x\n  }\n  val get = () => x\n  record { inc: inc, get: get }\n}\nprintln(pair.inc(2))\nprintln(pair.get())\nprintln(pair.inc(3))\nprintln(pair.get())\nassertResult(5)(pair.get())\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "2\n2\n5\n5\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_function_returning_mutable_closure_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-function-closure-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-function-closure-{unique}"));
    fs::write(
        &source_path,
        "def make() = {\n  mutable x = 0\n  (y) => {\n    x = x + y\n    x\n  }\n}\nval f = make()\nprintln(f(2))\nprintln(f(3))\nassertResult(5)(f(0))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "2\n5\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_honors_deny_trust() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-trust-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-trust-{unique}"));
    fs::write(
        &source_path,
        "trust theorem foo(): { true } = assert(true)\nprintln(1)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "--deny-trust",
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("trusted proof 'foo' is not allowed (level 1)")
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_print_string_concat() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-concat-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-concat-{unique}"));
    fs::write(
        &source_path,
        "println(\"x = \" + (1 + 2))\nprintln(\"ok? \" + true)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "x = 3\nok? true\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_print_string_interpolation() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-interpolation-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-interpolation-{unique}"));
    fs::write(
        &source_path,
        "val x = 10\nval y = 20\nval parts = split(\"a,b\", \",\")\nval message = \"x = #{x :> *}, y = #{y :> *}\"\nprintln(\"x = #{x :> *}, sum = #{(x + 5) :> *}, parts = #{parts :> *}\")\nprintln(message)\nassertResult(\"x = 10, y = 20\")(message)\nassertResult(\"x + y = 30\")(\"x + y = #{(x + y) :> *}\")\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "x = 10, sum = 15, parts = [a, b]\nx = 10, y = 20\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_string_interpolation_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-interpolation-side-effects-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-interpolation-side-effects-{unique}"
    ));
    fs::write(
        &source_path,
        "mutable hits = 0\nval text = \"x=#{ { hits += 1; 42 } }\"\nprintln(hits)\nprintln(text)\nprintln(\"inline=#{ { hits += 1; 7 } }\")\nprintln(hits)\nassertResult(2)(hits)\nassertResult(\"x=42\")(text)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "1\nx=42\ninline=7\n2\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_integer_numeric_helpers() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-numeric-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-numeric-{unique}"));
    fs::write(
        &source_path,
        "println(\"abs = \" + abs(-10))\nprintln(\"int = \" + int(7))\nprintln(\"floor = \" + floor(8))\nprintln(\"ceil = \" + ceil(9))\nprintln([1 + 1, 6 / 2, 2 * 3])\nassertResult(10)(abs(-10))\nassertResult(7)(int(7))\nassertResult(8)(floor(8))\nassertResult(9)(ceil(9))\nassertResult([2, 3, 6])([1 + 1, 6 / 2, 2 * 3])\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "abs = 10\nint = 7\nfloor = 8\nceil = 9\n[2, 3, 6]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_bitwise_folds() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-bitwise-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-bitwise-{unique}"));
    fs::write(
        &source_path,
        "val xor = _ ^ _\nprintln([1 & 1, 1 | 0, 1 ^ 1])\nprintln(foldLeft([1, 3])(7)(xor))\nassertResult([1, 1, 0])([1 & 1, 1 | 0, 1 ^ 1])\nassertResult(5)(foldLeft([1, 3])(7)(xor))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "[1, 1, 0]\n5\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_double_helpers() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-double-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-double-{unique}"));
    fs::write(
        &source_path,
        "def neg() = -1.25\nval hyp = sqrt(3.0 * 3.0 + 4.0 * 4.0)\nval product = foldLeft([1.0, 2.0, 3.0, 4.0])(1.0)((x, y) => x * y)\nval shifted = map([1.0, 2.0])((x) => x + 0.5)\nprintln(1.5)\nprintln(double(10))\nprintln(\"sqrt = \" + sqrt(9.0))\nprintln([1.0, 2.5, abs(-3.5)])\nprintln(product)\nprintln(shifted)\nprintln(neg())\nassertResult(3.0)(sqrt(9.0))\nassertResult(5.0)(hyp)\nassertResult(3)(int(3.14159265359))\nassertResult(1)(floor(1.5))\nassertResult(-1)(floor(-1.5))\nassertResult(5)(ceil(4.4))\nassertResult(-4)(ceil(-4.5))\nassertResult(10.5)(abs(-10.5))\nassertResult(24.0)(product)\nassertResult([1.5, 2.5])(shifted)\nassertResult([1.0, 2.5, 3.5])([1.0, 2.5, abs(-3.5)])\nassertResult(-1.25)(neg())\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "1.5\n10.0\nsqrt = 3.0\n[1.0, 2.5, 3.5]\n24.0\n[1.5, 2.5]\n-1.25\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_float_helpers() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-float-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-float-{unique}"));
    fs::write(
        &source_path,
        "val pure = 1.23456789F\nval widened = 1.25F + 2\nval product = foldLeft([1.5F, 2.0F])(1.0F)((x, y) => x * y)\nval shifted = map([1.0F, 2.0F])((x) => x + 0.25F)\nprintln(pure)\nprintln([1.0F, abs(-2.5F), widened])\nprintln(product)\nprintln(shifted)\nprintln(\"float = \" + pure)\nprintln(\"sqrt = \" + sqrt(double(9.0F)))\nassert(widened > pure)\nassertResult(true)(1.0F == 1)\nassertResult(1.2345679F)(pure)\nassertResult(3.25F)(widened)\nassertResult(3.0F)(product)\nassertResult([1.25F, 2.25F])(shifted)\nassertResult([1.0F, 2.5F, 3.25F])([1.0F, abs(-2.5F), widened])\nassertResult(3)(int(3.75F))\nassertResult(-1)(floor(-1.25F))\nassertResult(2)(ceil(1.25F))\nassertResult(2.5F)(abs(-2.5F))\nassertResult(3.0)(sqrt(double(9.0F)))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "1.2345679\n[1.0, 2.5, 3.25]\n3.0\n[1.25, 2.25]\nfloat = 1.2345679\nsqrt = 3.0\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_numeric_helper_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-numeric-side-effects-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-numeric-side-effects-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval a = sqrt({ hits += 1; 9.0 })\nval b = int({ hits += 1; 3.8 })\nval c = floor({ hits += 1; -1.25F })\nval d = ceil({ hits += 1; 1.25F })\nval e = abs({ hits += 1; -2.5 })\nval f = double({ hits += 1; 4 })\nprintln(hits)\nprintln(a)\nprintln(b)\nprintln(c)\nprintln(d)\nprintln(e)\nprintln(f)\nassertResult(6)(hits)\nassertResult(3.0)(a)\nassertResult(3)(b)\nassertResult(-1)(c)\nassertResult(2)(d)\nassertResult(2.5)(e)\nassertResult(4.0)(f)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "6\n3.0\n3\n-1\n2\n2.5\n4.0\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_if_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-static-if-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-static-if-{unique}"));
    fs::write(
        &source_path,
        "val label = if(true) \"yes\" else \"no\"\nval xs = if(false) [0] else [1, 2]\nval row = if(size(xs) == 2) record { name: \"ok\", count: size(xs) } else record { name: \"bad\", count: 0 }\nval mapper = if(true) ((x) => x + 1) else ((x) => x + 2)\nval mapped = map([1, 2])(mapper)\nval mappedMethod = [1, 2].map(if(false) ((x) => x + 1) else ((x) => x + 2))\nprintln(label)\nprintln(xs)\nprintln(row.name)\nprintln(row.count)\nprintln(mapped)\nprintln(mappedMethod)\nassertResult(\"yes\")(label)\nassertResult([1, 2])(xs)\nassertResult(record { name: \"ok\", count: 2 })(row)\nassertResult([2, 3])(mapped)\nassertResult([3, 4])(mappedMethod)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "yes\n[1, 2]\nok\n2\n[2, 3]\n[3, 4]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_static_list_branches() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-static-list-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-static-list-{unique}"));
    fs::write(
        &source_path,
        r#"val key = head(args())
val ints = if(key == "left") [1, 2, 3] else [4, 5, 6]
val strs = if(key == "left") ["alpha", "beta"] else ["gamma", "delta"]
println(head(ints))
println(head(tail(ints)))
println(size(ints))
println(head(strs))
println(head(tail(strs)))
println(size(strs))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if static-list branch build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&left_run.stdout),
        "1\n2\n3\nalpha\nbeta\n2\n"
    );
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "4\n5\n3\ngamma\ndelta\n2\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_static_int_list_branches_with_different_lengths() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-static-int-list-len-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-static-int-list-len-{unique}"
    ));
    fs::write(
        &source_path,
        r#"val key = head(args())
val shortFirst: List<Int> = if(key == "left") [1, 2] else [3, 4, 5]
val longFirst: List<Int> = if(key == "left") [10, 20, 30, 40] else [50, 60]
println(size(shortFirst))
println(head(shortFirst))
println(size(longFirst))
println(head(longFirst))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if differing-length static-list build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(String::from_utf8_lossy(&left_run.stdout), "2\n1\n4\n10\n");
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(String::from_utf8_lossy(&right_run.stdout), "3\n3\n2\n50\n");
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_static_list_branches_with_cons_and_tail() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-static-list-cons-tail-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-static-list-cons-tail-{unique}"
    ));
    fs::write(
        &source_path,
        r#"val key = head(args())
val withCons: List<Int> = if(key == "left") cons(99)([1, 2, 3]) else [4, 5]
val withTail: List<Int> = if(key == "left") tail([1, 2, 3]) else [40, 50, 60]
val twoCons: List<Int> = if(key == "left") cons(7)([1, 2]) else cons(8)([1, 2, 3])
println(size(withCons))
println(head(withCons))
println(size(withTail))
println(head(withTail))
println(size(twoCons))
println(head(twoCons))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if cons/tail static-list build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&left_run.stdout),
        "4\n99\n2\n2\n3\n7\n"
    );
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "2\n4\n3\n40\n4\n8\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_cons_with_dynamic_head_and_static_list_tail() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-cons-dynamic-head-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-cons-dynamic-head-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val n: Int = if(arg == "a") 0 else 5
mutable counter = 0
val plain = cons(n)([1, 2, 3])
val nested = cons(n)(cons(99)([1]))
val withEffect = cons({ counter += 1; n })([10, 20])
println(plain)
println(size(plain))
println(nested)
println(size(nested))
println(withEffect)
println(counter)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "cons dynamic head build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&a_run.stdout),
        "[0, 1, 2, 3]\n4\n[0, 99, 1]\n3\n[0, 10, 20]\n1\n"
    );
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&b_run.stdout),
        "[5, 1, 2, 3]\n4\n[5, 99, 1]\n3\n[5, 10, 20]\n1\n"
    );
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_list_map_with_dynamic_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-static-list-map-dynamic-cap-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-static-list-map-dynamic-cap-{unique}"
    ));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val n: Int = if(arg == "a") 1 else 2
val ints = map([10, 20, 30])((x) => x + n)
val suffix: String = if(arg == "a") "!" else "?"
val strs = map(["alpha", "beta"])((s) => s + suffix)
mutable counter = 0
val effects = map([5, 6, 7])((x) => { counter += 1; x + counter })
println(ints)
println(strs)
println(effects)
println(counter)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "static list map dynamic capture build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&a_run.stdout),
        "[11, 21, 31]\n[alpha!, beta!]\n[6, 8, 10]\n3\n"
    );
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&b_run.stdout),
        "[12, 22, 32]\n[alpha?, beta?]\n[6, 8, 10]\n3\n"
    );
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_list_fold_left_with_dynamic_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-static-list-fold-dynamic-cap-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-static-list-fold-dynamic-cap-{unique}"
    ));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val n: Int = if(arg == "a") 10 else 20
val total = foldLeft([1, 2, 3])(0)((acc, x) => acc + x * n)
val joined = foldLeft(["x", "y", "z"])("")((acc, s) => acc + s + (n + ""))
mutable counter = 0
val effects = foldLeft([5, 6, 7])(0)((acc, x) => { counter += 1; acc + x })
println(total)
println(joined)
println(effects)
println(counter)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "static list foldLeft dynamic capture build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&a_run.stdout),
        "60\nx10y10z10\n18\n3\n"
    );
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&b_run.stdout),
        "120\nx20y20z20\n18\n3\n"
    );
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_list_helpers_with_transitive_dynamic_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-static-list-transitive-cap-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-static-list-transitive-cap-{unique}"
    ));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val n: Int = if(arg == "a") 10 else 20
def addN(x: Int): Int = x + n
def square(x: Int): Int = addN(x * x)
val mapped = map([1, 2, 3])(addN)
val nested = map([1, 2, 3])(square)
val total = foldLeft([1, 2, 3])(0)((acc, x) => addN(x) + acc)
println(mapped)
println(nested)
println(total)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "static list transitive capture build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&a_run.stdout),
        "[11, 12, 13]\n[11, 14, 19]\n36\n",
    );
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&b_run.stdout),
        "[21, 22, 23]\n[21, 24, 29]\n66\n",
    );
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_with_map_results() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-map-results-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-map-results-{unique}"));
    fs::write(
        &source_path,
        r#"val key = head(args())
val xs = if(key == "left") map([1, 2])((x) => x * 10) else map([3, 4, 5])((x) => x * 100)
val ys = if(key == "left") map([1])((x) => x + 1) else cons(99)([0])
println(xs)
println(size(xs))
println(ys)
println(size(ys))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if with map results build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&left_run.stdout),
        "[10, 20]\n2\n[2]\n1\n"
    );
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "[300, 400, 500]\n3\n[99, 0]\n2\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_static_string_list_with_non_string_helpers() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-strlist-nonstring-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-strlist-nonstring-{unique}"
    ));
    fs::write(
        &source_path,
        r#"val key = head(args())
val xs: List<String> = if(key == "left") ["alpha"] else ["beta", "gamma"]
val lengths = map(xs)((s) => length(s))
val total = foldLeft(xs)(0)((acc, s) => acc + length(s))
val joined = map(xs)((s) => s + "!")
println(lengths)
println(total)
println(joined)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if static string list build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&left_run.stdout),
        "[5]\n5\n[alpha!]\n"
    );
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "[4, 5]\n9\n[beta!, gamma!]\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_with_runtime_list_map_results() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-runtime-map-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-runtime-map-{unique}"));
    fs::write(
        &source_path,
        r#"val key = head(args())
val xs: List<Int> = if(key == "left") [1, 2] else [3, 4, 5]
val n: Int = if(key == "left") 10 else 20
val curried = if(key == "left") map(xs)((x) => x + n) else [99]
val method = if(key == "left") xs.map((x) => x + n) else cons(0)([100])
println(curried)
println(method)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if with runtime map results build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&left_run.stdout),
        "[11, 12]\n[11, 12]\n"
    );
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "[99]\n[0, 100]\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_lines_map_to_scalar_list() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-lines-scalar-{unique}.kl"));
    let lines_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-lines-scalar-{unique}.txt"));
    let lines_path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-lines-scalar-path-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-lines-scalar-{unique}"));
    fs::write(&lines_path_holder, lines_path.to_string_lossy().as_bytes())
        .expect("lines path holder should write");
    fs::write(&lines_path, "alpha\nbeta\ngamma").expect("lines should write");
    let source = format!(
        r#"val pathHolder = "{}"
val linesPath = FileInput#all(pathHolder)
val runtimeLines = FileInput#lines(linesPath)
val branched = if(head(args()) == "left") runtimeLines else ["short", "tiny"]
val lengths = map(branched)((s) => length(s))
val gtFour = map(branched)((s) => length(s) > 4)
println(lengths)
println(gtFour)
"#,
        lines_path_holder.display()
    );
    fs::write(&source_path, source).expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime lines scalar map build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&lines_path_holder);
    let _ = fs::remove_file(&lines_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&left_run.stdout),
        "[5, 4, 5]\n[true, false, true]\n"
    );
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "[5, 4]\n[true, false]\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_foreach_growing_mutable_list_via_cons() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-foreach-mutable-cons-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-foreach-mutable-cons-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
mutable result: List<Int> = []
mutable filtered: List<Int> = []
val xs: List<Int> = [1, 2, 3, 4, 5]
foreach(x in xs) {
  result = (if(arg == "a") cons(x)(result) else if(x > 2) cons(x * 10)(result) else result)
}
foreach(x in xs) {
  if(x > 2) {
    filtered = cons(x)(filtered)
  }
}
mutable strs: List<String> = []
foreach(s in ["alpha", "beta", "gamma"]) {
  strs = cons(s + "!")(strs)
}
println(result)
println(filtered)
println(strs)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "foreach mutable cons build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&a_run.stdout),
        "[5, 4, 3, 2, 1]\n[5, 4, 3]\n[gamma!, beta!, alpha!]\n"
    );
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&b_run.stdout),
        "[50, 40, 30]\n[5, 4, 3]\n[gamma!, beta!, alpha!]\n"
    );
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_while_loop_growing_mutable_int_list_via_cons() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-while-mutable-cons-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-while-mutable-cons-{unique}"));
    fs::write(
        &source_path,
        r#"mutable acc: List<Int> = []
mutable i = 0
while(i < 5) {
  acc = cons(i)(acc)
  i = i + 1
}
mutable seeded: List<Int> = [99]
mutable j = 1
while(j < 4) {
  seeded = cons(j * 10)(seeded)
  j = j + 1
}
println(acc)
println(size(acc))
println(head(acc))
println(seeded)
println(size(seeded))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "while mutable cons build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[4, 3, 2, 1, 0]\n5\n4\n[30, 20, 10, 99]\n4\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_while_loop_growing_mutable_string_list_via_cons() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-while-mutable-strs-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-while-mutable-strs-{unique}"));
    fs::write(
        &source_path,
        r#"mutable strs: List<String> = []
mutable i = 0
while(i < 3) {
  strs = cons("v" + i)(strs)
  i = i + 1
}
println(strs)
println(size(strs))
println(head(strs))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "while mutable string-list build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[v2, v1, v0]\n3\nv2\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_list_or_null_branches() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-list-or-null-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-list-or-null-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val maybe = if(arg == "a") [1, 2, 3] else null
println(maybe == null)
println(maybe != null)
println(null == maybe)
if(maybe != null) {
  println(head(maybe))
  println(size(maybe))
} else {
  println("(null)")
}
val reversed = if(arg == "a") null else [10, 20]
if(reversed != null) {
  println(head(reversed))
} else {
  println("(also null)")
}
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "list or null build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&a_run.stdout),
        "false\ntrue\nfalse\n1\n3\n(also null)\n"
    );
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&b_run.stdout),
        "true\nfalse\ntrue\n(null)\n10\n"
    );
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_static_set_branches() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-set-merge-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-set-merge-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val nums = if(arg == "a") %(1, 2) else %(3, 4, 5)
val labels = if(arg == "a") %("alpha", "beta") else %("gamma")
println(size(nums))
println(Set#size(nums))
println(isEmpty(nums))
println(nums.contains(2))
println(nums.contains(99))
println(size(labels))
println(labels.contains("alpha"))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "set merge build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&a_run.stdout),
        "2\n2\nfalse\ntrue\nfalse\n2\ntrue\n"
    );
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&b_run.stdout),
        "3\n3\nfalse\nfalse\nfalse\n1\nfalse\n"
    );
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_static_map_branches_same_size() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-map-merge-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-map-merge-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val m = if(arg == "a") %["x": 1, "y": 2] else %["a": 3, "b": 4]
println(Map#size(m))
println(size(m))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "map merge build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(String::from_utf8_lossy(&a_run.stdout), "2\n2\n");
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(String::from_utf8_lossy(&b_run.stdout), "2\n2\n");
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_static_map_branches_different_sizes() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-map-merge-diff-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-map-merge-diff-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val small = if(arg == "a") %["x": 1] else %["y": 2, "z": 3]
val big = if(arg == "a") %["a": 4, "b": 5] else %["c": 6]
println(Map#size(small))
println(size(small))
println(Map#size(big))
println(size(big))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "map merge diff sizes build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(String::from_utf8_lossy(&a_run.stdout), "1\n1\n2\n2\n");
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(String::from_utf8_lossy(&b_run.stdout), "2\n2\n1\n1\n");
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_map_get_after_branch_merge() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-map-get-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-map-get-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val m = if(arg == "a") %["x": 1, "y": 2] else %["a": 3, "b": 4]
println(Map#get(m, "x"))
println(Map#get(m, "y"))
println(Map#get(m, "a"))
println(Map#get(m, "b"))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime map get build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(String::from_utf8_lossy(&a_run.stdout), "1\n2\n0\n0\n");
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(String::from_utf8_lossy(&b_run.stdout), "0\n0\n3\n4\n");
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_map_contains_and_null_check() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-map-contains-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-map-contains-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val m = if(arg == "a") %["x": 1, "y": 2] else %["a": 3, "b": 4]
println(Map#containsKey(m, "x"))
println(Map#containsKey(m, "a"))
println(Map#containsKey(m, "z"))
println(Map#containsValue(m, 1))
println(Map#containsValue(m, 3))
println(Map#containsValue(m, 99))
println(Map#get(m, "x") == null)
println(Map#get(m, "z") == null)
println(Map#get(m, "x") != null)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime map contains build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&a_run.stdout),
        "true\nfalse\nfalse\ntrue\nfalse\nfalse\nfalse\ntrue\ntrue\n"
    );
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&b_run.stdout),
        "false\ntrue\nfalse\nfalse\ntrue\nfalse\ntrue\ntrue\nfalse\n"
    );
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_map_get_with_string_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-map-strings-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-map-strings-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val m = if(arg == "a") %["x": "alpha", "y": "beta"] else %["a": "gamma", "b": "delta"]
println(Map#get(m, "x"))
println(Map#get(m, "y"))
println(Map#get(m, "a"))
println(Map#get(m, "b"))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime map string values build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(String::from_utf8_lossy(&a_run.stdout), "alpha\nbeta\n\n\n");
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(String::from_utf8_lossy(&b_run.stdout), "\n\ngamma\ndelta\n");
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_map_and_set_is_empty() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-isempty-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-isempty-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val m = if(arg == "full") %["a": 1, "b": 2] else %["x": 9]
val s = if(arg == "full") %(1 2 3) else %(7)
println(Map#isEmpty(m))
println(Set#isEmpty(s))
println(Map#size(m))
println(Set#size(s))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime map/set isEmpty build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let full_run = Command::new(&output_path)
        .arg("full")
        .output()
        .expect("generated executable should run full");
    let other_run = Command::new(&output_path)
        .arg("other")
        .output()
        .expect("generated executable should run other");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(full_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&full_run.stdout),
        "false\nfalse\n2\n3\n"
    );
    assert!(full_run.stderr.is_empty());

    assert!(other_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&other_run.stdout),
        "false\nfalse\n1\n1\n"
    );
    assert!(other_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_alloc_and_collect() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-basic-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-basic-{unique}"));
    fs::write(
        &source_path,
        r#"val a = __gc_alloc(100)
println(a > 0)
__gc_collect()
val b = __gc_alloc(100)
println(b > 0)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc basic build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "true\ntrue\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_negative_allocation_sizes_runtime_errors() {
    let cases = [
        ("__gc_alloc(-1)\n", "__gc_alloc"),
        ("__gc_record(-1)\n", "__gc_record"),
        ("__gc_array(-1)\n", "__gc_array"),
        ("__gc_string_alloc(-1)\n", "__gc_string_alloc"),
        ("__gc_list_int(-1)\n", "__gc_list_int"),
        ("__gc_list_ptr(-1)\n", "__gc_list_ptr"),
    ];
    for (index, (source, builtin_name)) in cases.iter().enumerate() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path =
            std::env::temp_dir().join(format!("klassic-native-gc-neg-alloc-{index}-{unique}.kl"));
        let output_path =
            std::env::temp_dir().join(format!("klassic-native-gc-neg-alloc-{index}-{unique}"));
        fs::write(&source_path, source).expect("source should write");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");

        assert!(
            build.status.success(),
            "gc negative allocation build failed for {builtin_name}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);

        assert!(!run.status.success(), "{builtin_name} should fail");
        assert!(run.stdout.is_empty());
        assert_eq!(
            String::from_utf8_lossy(&run.stderr),
            format!(
                "{}:1:1: {builtin_name} expects a non-negative integer index\n",
                source_path.display()
            )
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_allocation_size_overflow_runtime_errors() {
    let cases = [
        ("__gc_alloc(9223372036854771682)\n", "__gc_alloc"),
        (
            "__gc_string_alloc(9223372036854771667)\n",
            "__gc_string_alloc",
        ),
        ("__gc_record(1152921504606846461)\n", "__gc_record"),
        ("__gc_array(1152921504606846461)\n", "__gc_array"),
        ("__gc_list_int(1152921504606846460)\n", "__gc_list_int"),
        ("__gc_list_ptr(1152921504606846460)\n", "__gc_list_ptr"),
    ];
    for (index, (source, builtin_name)) in cases.iter().enumerate() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-alloc-overflow-{index}-{unique}.kl"
        ));
        let output_path =
            std::env::temp_dir().join(format!("klassic-native-gc-alloc-overflow-{index}-{unique}"));
        fs::write(&source_path, source).expect("source should write");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");

        assert!(
            build.status.success(),
            "gc allocation overflow build failed for {builtin_name}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);

        assert!(!run.status.success(), "{builtin_name} should fail");
        assert!(run.stdout.is_empty());
        assert_eq!(
            String::from_utf8_lossy(&run.stderr),
            format!(
                "{}:1:1: {builtin_name} allocation size overflow\n",
                source_path.display()
            )
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_derived_size_overflow_runtime_errors() {
    let cases = [
        (
            r#"val xs = __gc_list_int(0)
__gc_write(xs, 0, 1152921504606846459)
__gc_list_int_push(xs, 1)
"#,
            "__gc_list_int_push",
            3,
        ),
        (
            r#"val a = __gc_list_int(0)
__gc_write(a, 0, 1152921504606846459)
val b = __gc_list_int(1)
__gc_list_concat(a, b)
"#,
            "__gc_list_concat",
            4,
        ),
        (
            r#"val xs = __gc_list_ptr(0)
__gc_write(xs, 0, 1152921504606846459)
__gc_list_ptr_push(xs, __gc_string("x"))
"#,
            "__gc_list_ptr_push",
            3,
        ),
        (
            r#"val a = __gc_list_ptr(0)
__gc_write(a, 0, 1152921504606846459)
val b = __gc_list_ptr(1)
__gc_list_ptr_set(b, 0, __gc_string("x"))
__gc_list_ptr_concat(a, b)
"#,
            "__gc_list_ptr_concat",
            5,
        ),
    ];
    for (index, (source, builtin_name, line)) in cases.iter().enumerate() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-list-derived-overflow-{index}-{unique}.kl"
        ));
        let output_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-list-derived-overflow-{index}-{unique}"
        ));
        fs::write(&source_path, source).expect("source should write");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");
        assert!(
            build.status.success(),
            "gc list derived overflow build failed for {builtin_name}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);

        assert_eq!(run.status.code(), Some(1), "{builtin_name} should fail");
        assert!(run.stdout.is_empty());
        assert_eq!(
            String::from_utf8_lossy(&run.stderr),
            format!(
                "{}:{line}:1: {builtin_name} allocation size overflow\n",
                source_path.display()
            )
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_transform_size_overflow_runtime_errors() {
    let cases = [
        (
            r#"val xs = __gc_list_int(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_int_pop(xs)
"#,
            "__gc_list_int_pop",
        ),
        (
            r#"val xs = __gc_list_int(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_int_reverse(xs)
"#,
            "__gc_list_int_reverse",
        ),
        (
            r#"val xs = __gc_list_ptr(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_ptr_pop(xs)
"#,
            "__gc_list_ptr_pop",
        ),
        (
            r#"val xs = __gc_list_ptr(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_ptr_reverse(xs)
"#,
            "__gc_list_ptr_reverse",
        ),
    ];
    for (index, (source, builtin_name)) in cases.iter().enumerate() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-list-transform-overflow-{index}-{unique}.kl"
        ));
        let output_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-list-transform-overflow-{index}-{unique}"
        ));
        fs::write(&source_path, source).expect("source should write");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");
        assert!(
            build.status.success(),
            "gc list transform overflow build failed for {builtin_name}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);

        assert_eq!(run.status.code(), Some(1), "{builtin_name} should fail");
        assert!(run.stdout.is_empty());
        assert_eq!(
            String::from_utf8_lossy(&run.stderr),
            format!(
                "{}:3:1: {builtin_name} allocation size overflow\n",
                source_path.display()
            )
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_iteration_length_overflow_runtime_errors() {
    let cases = [
        (
            r#"val xs = __gc_list_int(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_int_println(xs)
"#,
            "__gc_list_int_println",
        ),
        (
            r#"val xs = __gc_list_int(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_int_sum(xs)
"#,
            "__gc_list_int_sum",
        ),
        (
            r#"val xs = __gc_list_int(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_int_min(xs)
"#,
            "__gc_list_int_min",
        ),
        (
            r#"val parts = __gc_list_ptr(0)
__gc_write(parts, 0, 1152921504606846460)
__gc_list_ptr_join(parts, __gc_string(","))
"#,
            "__gc_list_ptr_join",
        ),
        (
            r#"val xs = __gc_list_int(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_int_to_string(xs, __gc_string(","))
"#,
            "__gc_list_int_to_string",
        ),
    ];
    for (index, (source, builtin_name)) in cases.iter().enumerate() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-list-iteration-overflow-{index}-{unique}.kl"
        ));
        let output_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-list-iteration-overflow-{index}-{unique}"
        ));
        fs::write(&source_path, source).expect("source should write");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");
        assert!(
            build.status.success(),
            "gc list iteration overflow build failed for {builtin_name}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);

        assert_eq!(run.status.code(), Some(1), "{builtin_name} should fail");
        assert!(run.stdout.is_empty());
        assert_eq!(
            String::from_utf8_lossy(&run.stderr),
            format!(
                "{}:3:1: {builtin_name} list length overflow\n",
                source_path.display()
            )
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_stored_length_overflow_runtime_errors() {
    let cases = [
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_println(s)
"#,
            "__gc_string_println",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_get_byte(s, 0)
"#,
            "__gc_string_get_byte",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_set_byte(s, 0, 65)
"#,
            "__gc_string_set_byte",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_substring(s, 0, 0)
"#,
            "__gc_string_substring",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_starts_with(s, __gc_string(""))
"#,
            "__gc_string_starts_with",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_ends_with(s, __gc_string(""))
"#,
            "__gc_string_ends_with",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_contains(s, __gc_string("x"))
"#,
            "__gc_string_contains",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_repeat(s, 1)
"#,
            "__gc_string_repeat",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_index_of(s, 65)
"#,
            "__gc_string_index_of",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_index_of_from(s, 65, 0)
"#,
            "__gc_string_index_of_from",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_last_index_of(s, 65)
"#,
            "__gc_string_last_index_of",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_to_int(s)
"#,
            "__gc_string_to_int",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_split(s, 44)
"#,
            "__gc_string_split",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_replace(s, __gc_string("a"), __gc_string("b"))
"#,
            "__gc_string_replace",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_trim(s)
"#,
            "__gc_string_trim",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_to_lower(s)
"#,
            "__gc_string_to_lower",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_to_upper(s)
"#,
            "__gc_string_to_upper",
        ),
    ];
    for (index, (source, builtin_name)) in cases.iter().enumerate() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-string-length-overflow-{index}-{unique}.kl"
        ));
        let output_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-string-length-overflow-{index}-{unique}"
        ));
        fs::write(&source_path, source).expect("source should write");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");
        assert!(
            build.status.success(),
            "gc string length overflow build failed for {builtin_name}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);

        assert_eq!(run.status.code(), Some(1), "{builtin_name} should fail");
        assert!(run.stdout.is_empty());
        assert_eq!(
            String::from_utf8_lossy(&run.stderr),
            format!(
                "{}:3:1: {builtin_name} string length overflow\n",
                source_path.display()
            )
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_indexed_length_overflow_runtime_errors() {
    let cases = [
        (
            r#"val xs = __gc_list_int(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_int_get(xs, 0)
"#,
            "__gc_list_int_get",
        ),
        (
            r#"val xs = __gc_list_int(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_int_set(xs, 0, 1)
"#,
            "__gc_list_int_set",
        ),
        (
            r#"val xs = __gc_list_ptr(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_ptr_get(xs, 0)
"#,
            "__gc_list_ptr_get",
        ),
        (
            r#"val xs = __gc_list_ptr(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_ptr_get_string(xs, 0)
"#,
            "__gc_list_ptr_get_string",
        ),
        (
            r#"val xs = __gc_list_ptr(0)
__gc_write(xs, 0, 1152921504606846460)
__gc_list_ptr_set(xs, 0, __gc_string("x"))
"#,
            "__gc_list_ptr_set",
        ),
    ];
    for (index, (source, builtin_name)) in cases.iter().enumerate() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-list-indexed-overflow-{index}-{unique}.kl"
        ));
        let output_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-list-indexed-overflow-{index}-{unique}"
        ));
        fs::write(&source_path, source).expect("source should write");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");
        assert!(
            build.status.success(),
            "gc list indexed overflow build failed for {builtin_name}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);

        assert_eq!(run.status.code(), Some(1), "{builtin_name} should fail");
        assert!(run.stdout.is_empty());
        assert_eq!(
            String::from_utf8_lossy(&run.stderr),
            format!(
                "{}:3:1: {builtin_name} list length overflow\n",
                source_path.display()
            )
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_equality_length_overflow_runtime_errors() {
    let cases = [
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
__gc_string_eq(s, __gc_string(""))
"#,
            "__gc_string_eq",
        ),
        (
            r#"val s = __gc_string_alloc(0)
__gc_write(s, 0, 9223372036854771667)
s == __gc_string("")
"#,
            "heap string equality",
        ),
    ];
    for (index, (source, diagnostic_name)) in cases.iter().enumerate() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-string-eq-overflow-{index}-{unique}.kl"
        ));
        let output_path = std::env::temp_dir().join(format!(
            "klassic-native-gc-string-eq-overflow-{index}-{unique}"
        ));
        fs::write(&source_path, source).expect("source should write");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");
        assert!(
            build.status.success(),
            "gc string equality overflow build failed for {diagnostic_name}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);

        assert_eq!(run.status.code(), Some(1), "{diagnostic_name} should fail");
        assert!(run.stdout.is_empty());
        assert_eq!(
            String::from_utf8_lossy(&run.stderr),
            format!(
                "{}:3:1: {diagnostic_name} string length overflow\n",
                source_path.display()
            )
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_reclaims_dead_allocations() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-stress-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-stress-{unique}"));
    // Allocate 200_000 bytes 10 times = 2 MiB total but the heap is only
    // 1 MiB. Each iteration's allocation is unreachable (the result is
    // discarded), so a GC trigger on the bump-allocator's first overflow
    // reclaims previous blocks and every subsequent call succeeds.
    fs::write(
        &source_path,
        r#"mutable count = 0
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
  __gc_alloc(200000)
  count = count + 1
}
println(count)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc stress build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "10\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_pinned_root_survives_collection() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-pin-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-pin-{unique}"));
    // Allocate a small block, write a sentinel, pin it, then run a loop
    // whose unpinned allocations force the bump allocator to overflow and
    // trigger collections. The pinned block should survive the sweep — its
    // sentinel must read back unchanged.
    fs::write(
        &source_path,
        r#"val a = __gc_alloc(16)
__gc_write(a, 0, 12345)
__gc_pin(a)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
  __gc_alloc(200000)
}
__gc_collect()
println(__gc_read(a, 0))
__gc_unpin(a)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc pin build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "12345\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_stack_slot_auto_rooted() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-auto-root-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-auto-root-{unique}"));
    // No explicit __gc_pin: the val binding's stack slot itself is
    // registered with the GC's shadow stack because __gc_alloc returns a
    // tagged heap pointer. The sentinel we wrote must read back unchanged
    // after the heap-stress loop forces multiple collections.
    fs::write(
        &source_path,
        r#"val a = __gc_alloc(16)
__gc_write(a, 0, 12345)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
  __gc_alloc(200000)
}
__gc_collect()
println(__gc_read(a, 0) == 12345)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc auto-root build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "true\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_mutable_heap_pointer_reassignment() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-mutable-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-mutable-{unique}"));
    // The shadow stack tracks the slot's address, so reassigning the
    // mutable to a fresh heap pointer keeps the new pointer rooted
    // automatically — there is only one shadow entry, but its dereference
    // sees whichever pointer the slot currently holds.
    fs::write(
        &source_path,
        r#"mutable a = __gc_alloc(16)
__gc_write(a, 0, 11111)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
println(__gc_read(a, 0))
a = __gc_alloc(16)
__gc_write(a, 0, 99999)
foreach(j in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
println(__gc_read(a, 0))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc mutable build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "11111\n99999\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_pointer_record_keeps_children_alive() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-record-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-record-{unique}"));
    // The mark phase must recurse through the parent's pointer fields:
    // pin only the parent, store the two child blocks as fields, then
    // stress the heap and read back the children's sentinels.
    fs::write(
        &source_path,
        r#"val parent = __gc_record(2)
val child1 = __gc_alloc(16)
__gc_write(child1, 0, 1111)
val child2 = __gc_alloc(16)
__gc_write(child2, 0, 2222)
__gc_write(parent, 0, child1)
__gc_write(parent, 8, child2)
__gc_pin(parent)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
  __gc_alloc(150000)
}
__gc_collect()
println(__gc_read(child1, 0))
println(__gc_read(child2, 0))
__gc_unpin(parent)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc pointer-record build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "1111\n2222\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_write_accepts_heap_string_value() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-write-heap-string-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-gc-write-heap-string-{unique}"));
    // Pointer records may hold any heap pointer qword. HeapString has a
    // distinct native value kind from generic HeapPointer, so cover the raw
    // write path directly instead of only exercising list_ptr helpers.
    fs::write(
        &source_path,
        r#"val parent = __gc_record(1)
__gc_write(parent, 0, __gc_string("child"))
__gc_pin(parent)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
println("ok")
__gc_unpin(parent)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc write heap-string value build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "ok\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_read_ptr_preserves_pointer_value() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-read-ptr-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-read-ptr-{unique}"));
    // __gc_read keeps scalar qword reads as Int. __gc_read_ptr reads the same
    // field but tags the result as a heap pointer so address-taking helpers can
    // consume fields after strict plain-Int rejection.
    fs::write(
        &source_path,
        r#"val parent = __gc_record(1)
__gc_write(parent, 0, __gc_string("child"))
__gc_pin(parent)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
__gc_string_println(__gc_read_ptr(parent, 0))
__gc_unpin(parent)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc read_ptr build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "child\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_read_string_preserves_heap_string_value() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-read-string-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-read-string-{unique}"));
    fs::write(
        &source_path,
        r#"val parent = __gc_record(1)
__gc_write(parent, 0, __gc_string("child"))
__gc_pin(parent)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
val child = __gc_read_string(parent, 0)
println(child)
val shouted = child + __gc_string("!")
println(shouted)
assertResult("child")(toString(child))
assertResult("child!")(toString(shouted))
__gc_unpin(parent)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc read_string build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "child\nchild!\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_introspection_and_byte_access() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-strops-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-strops-{unique}"));
    // Exercise:
    //   __gc_string_len + __gc_string_eq (true and false cases)
    //   __gc_string_alloc + __gc_string_set_byte + __gc_string_get_byte
    //   round-trip building a heap string byte-by-byte and printing it
    fs::write(
        &source_path,
        r#"val a = __gc_string("klassic")
val b = __gc_string("klassic")
val c = __gc_string("klassik")
println(__gc_string_len(a))
println(__gc_string_eq(a, b))
println(__gc_string_eq(a, c))
val d = __gc_string_alloc(5)
__gc_string_set_byte(d, 0, 72)
__gc_string_set_byte(d, 1, 101)
__gc_string_set_byte(d, 2, 108)
__gc_string_set_byte(d, 3, 108)
__gc_string_set_byte(d, 4, 111)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
println(__gc_string_len(d))
println(__gc_string_get_byte(d, 0))
__gc_string_println(d)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc string introspection build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "7\ntrue\nfalse\n5\n72\nHello\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_introspection_helpers() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-introspect-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-introspect-{unique}"));
    // __gc_pointer_count infers the number of slots from the GC header
    // for both __gc_record and __gc_array. __gc_segment_count starts at
    // 1 and grows when heap pressure forces a fresh mmap.
    fs::write(
        &source_path,
        r#"val rec = __gc_record(7)
val arr = __gc_array(13)
println(__gc_pointer_count(rec))
println(__gc_pointer_count(arr))
println(__gc_segment_count())
val a = __gc_alloc(150000)
val b = __gc_alloc(150000)
val c = __gc_alloc(150000)
val d = __gc_alloc(150000)
val e = __gc_alloc(150000)
val f = __gc_alloc(150000)
val g = __gc_alloc(150000)
val h = __gc_alloc(150000)
println(__gc_segment_count())
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc introspection build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    let stdout = String::from_utf8_lossy(&run.stdout);
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines.len(), 4);
    // gc_alloc rounds total size up to a 16-byte boundary, so a record
    // requested with 7 pointer slots actually owns 8 (one trailing
    // padding slot) and a 13-slot array owns 14. The header-derived
    // count exposed via __gc_pointer_count reports those rounded sizes.
    assert_eq!(lines[0], "8");
    assert_eq!(lines[1], "14");
    assert_eq!(lines[2], "1");
    // After the 8 * 150 KiB allocations exhaust the initial segment and
    // the runtime grows once, the count should be at least 2.
    let final_segments: i64 = lines[3].parse().expect("segment count is a number");
    assert!(
        final_segments >= 2,
        "expected segment count >= 2 after growth, got {final_segments}"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_heap_string_natural_println() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-hsp-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-hsp-{unique}"));
    // Heap-allocated strings should now print via the regular `println`
    // builtin (no need to call `__gc_string_println` explicitly). This
    // is the first user-visible step of routing language values through
    // the GC heap.
    fs::write(
        &source_path,
        r#"println(__gc_string("hello"))
println(__gc_string_concat(__gc_string("foo "), __gc_string("bar")))
println(__gc_int_to_string(-42))
println(__gc_string_to_upper(__gc_string("klassic")))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc heap-string println build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "hello\nfoo bar\n-42\nKLASSIC\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_smap_round_trip() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-smap-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-smap-{unique}"));
    fs::write(
        &source_path,
        r#"mutable m = __gc_smap_new()
println(__gc_smap_size(m))
m = __gc_smap_set(m, __gc_string("alice"), __gc_string("engineer"))
m = __gc_smap_set(m, __gc_string("bob"), __gc_string("designer"))
m = __gc_smap_set(m, __gc_string("carol"), __gc_string("manager"))
println(__gc_smap_size(m))
println(__gc_smap_has(m, __gc_string("alice")))
println(__gc_smap_has(m, __gc_string("dave")))
__gc_string_println(__gc_smap_get(m, __gc_string("bob")))
m = __gc_smap_set(m, __gc_string("alice"), __gc_string("CEO"))
__gc_string_println(__gc_smap_get(m, __gc_string("alice")))
println(__gc_smap_size(m))
val keys = __gc_smap_keys(m)
val vals = __gc_smap_values(m)
__gc_string_println(__gc_list_ptr_join(keys, __gc_string(",")))
__gc_string_println(__gc_list_ptr_join(vals, __gc_string(",")))
println(__gc_smap_get(m, __gc_string("missing")))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc smap build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "0\n3\ntrue\nfalse\ndesigner\nCEO\n3\nalice,bob,carol\nCEO,designer,manager\n0\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_smap_get_string_preserves_heap_string_value() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-smap-get-string-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-gc-smap-get-string-{unique}"));
    fs::write(
        &source_path,
        r#"mutable m = __gc_smap_new()
m = __gc_smap_set(m, __gc_string("lang"), __gc_string("klassic"))
m = __gc_smap_set(m, __gc_string("mode"), __gc_string("native"))
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
val lang = __gc_smap_get_string(m, __gc_string("lang"))
val mode = __gc_smap_get_string(m, __gc_string("mode"))
println(lang)
println(mode + __gc_string("!"))
assertResult("klassic")(toString(lang))
assertResult("native!")(toString(mode + __gc_string("!")))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc smap_get_string build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "klassic\nnative!\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_replace_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-rep2-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-rep2-{unique}"));
    fs::write(
        &source_path,
        r#"__gc_string_println(__gc_string_replace(__gc_string("hello"), __gc_string("l"), __gc_string("L")))
__gc_string_println(__gc_string_replace(__gc_string("foo bar foo"), __gc_string("foo"), __gc_string("baz")))
__gc_string_println(__gc_string_replace(__gc_string("aaa"), __gc_string("a"), __gc_string("xyz")))
__gc_string_println(__gc_string_replace(__gc_string("nothing"), __gc_string("zzz"), __gc_string("???")))
__gc_string_println(__gc_string_replace(__gc_string(""), __gc_string("a"), __gc_string("b")))
__gc_string_println(__gc_string_replace(__gc_string("abc"), __gc_string(""), __gc_string("X")))
__gc_string_println(__gc_string_replace(__gc_string("abcabc"), __gc_string("abc"), __gc_string("")))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string_replace build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "heLLo\nbaz bar baz\nxyzxyzxyz\nnothing\n\nabc\n\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_trim_and_case() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-tcase-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-tcase-{unique}"));
    fs::write(
        &source_path,
        r#"__gc_string_println(__gc_string_trim(__gc_string("   hello   ")))
__gc_string_println(__gc_string_trim(__gc_string("\t\nklassic\r\n")))
__gc_string_println(__gc_string_trim(__gc_string("nochange")))
println(__gc_string_len(__gc_string_trim(__gc_string("    "))))
println(__gc_string_len(__gc_string_trim(__gc_string(""))))
__gc_string_println(__gc_string_to_lower(__gc_string("Hello, World!")))
__gc_string_println(__gc_string_to_upper(__gc_string("Hello, World!")))
__gc_string_println(__gc_string_to_lower(__gc_string("ALREADYlower MIXED 123")))
__gc_string_println(__gc_string_to_upper(__gc_string("alreadyUPPER mixed 123")))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string_trim/case build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "hello\nklassic\nnochange\n0\n0\nhello, world!\nHELLO, WORLD!\nalreadylower mixed 123\nALREADYUPPER MIXED 123\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_lines() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-lines-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-lines-{unique}"));
    fs::write(
        &source_path,
        r#"val parts = __gc_string_lines(__gc_string("first\nsecond\nthird"))
println(__gc_list_ptr_len(parts))
__gc_string_println(__gc_list_ptr_get(parts, 0))
__gc_string_println(__gc_list_ptr_get(parts, 1))
__gc_string_println(__gc_list_ptr_get(parts, 2))
val trailing = __gc_string_lines(__gc_string("one\ntwo\n"))
println(__gc_list_ptr_len(trailing))
__gc_string_println(__gc_list_ptr_get(trailing, 0))
__gc_string_println(__gc_list_ptr_get(trailing, 1))
println(__gc_string_len(__gc_list_ptr_get(trailing, 2)))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string_lines build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    // 3 segments, then "one\ntwo\n" → ["one", "two", ""] (3 entries, last empty).
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "3\nfirst\nsecond\nthird\n3\none\ntwo\n0\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_to_string() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-li2s-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-li2s-{unique}"));
    fs::write(
        &source_path,
        r#"mutable xs = __gc_list_int(0)
xs = __gc_list_int_push(xs, 1)
xs = __gc_list_int_push(xs, 22)
xs = __gc_list_int_push(xs, 333)
__gc_string_println(__gc_list_int_to_string(xs, __gc_string(", ")))
mutable ys = __gc_list_int(0)
ys = __gc_list_int_push(ys, 0)
ys = __gc_list_int_push(ys, -7)
ys = __gc_list_int_push(ys, 42)
__gc_string_println(__gc_list_int_to_string(ys, __gc_string(":")))
val empty = __gc_list_int(0)
println(__gc_string_len(__gc_list_int_to_string(empty, __gc_string(","))))
val one = __gc_list_int(1)
__gc_list_int_set(one, 0, 99)
__gc_string_println(__gc_list_int_to_string(one, __gc_string(", ")))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list_int_to_string build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "1, 22, 333\n0:-7:42\n0\n99\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_ptr_pop_concat_reverse() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-pcrr-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-pcrr-{unique}"));
    fs::write(
        &source_path,
        r#"mutable xs = __gc_list_ptr(0)
val a = __gc_alloc(16)
__gc_write(a, 0, 100)
val b = __gc_alloc(16)
__gc_write(b, 0, 200)
val c = __gc_alloc(16)
__gc_write(c, 0, 300)
xs = __gc_list_ptr_push(xs, a)
xs = __gc_list_ptr_push(xs, b)
xs = __gc_list_ptr_push(xs, c)
val rev = __gc_list_ptr_reverse(xs)
println(__gc_read(__gc_list_ptr_get(rev, 0), 0))
println(__gc_read(__gc_list_ptr_get(rev, 1), 0))
println(__gc_read(__gc_list_ptr_get(rev, 2), 0))
val popped = __gc_list_ptr_pop(xs)
println(__gc_list_ptr_len(popped))
println(__gc_read(__gc_list_ptr_get(popped, 0), 0))
println(__gc_read(__gc_list_ptr_get(popped, 1), 0))
mutable ys = __gc_list_ptr(0)
ys = __gc_list_ptr_push(ys, a)
val combined = __gc_list_ptr_concat(ys, popped)
println(__gc_list_ptr_len(combined))
println(__gc_read(__gc_list_ptr_get(combined, 0), 0))
println(__gc_read(__gc_list_ptr_get(combined, 1), 0))
println(__gc_read(__gc_list_ptr_get(combined, 2), 0))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list_ptr pop/concat/reverse build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "300\n200\n100\n2\n100\n200\n3\n100\n100\n200\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_ptr_pop_empty_bounds() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-ppop-bnd-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-ppop-bnd-{unique}"));
    fs::write(
        &source_path,
        r#"val xs = __gc_list_ptr(0)
__gc_list_ptr_pop(xs)
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(build.status.success());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "klassic gc: index out of bounds\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_reductions() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-redux-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-redux-{unique}"));
    fs::write(
        &source_path,
        r#"mutable xs = __gc_list_int(0)
xs = __gc_list_int_push(xs, 4)
xs = __gc_list_int_push(xs, -2)
xs = __gc_list_int_push(xs, 7)
xs = __gc_list_int_push(xs, 1)
xs = __gc_list_int_push(xs, -10)
xs = __gc_list_int_push(xs, 3)
println(__gc_list_int_sum(xs))
println(__gc_list_int_min(xs))
println(__gc_list_int_max(xs))
val empty = __gc_list_int(0)
println(__gc_list_int_sum(empty))
val one = __gc_list_int(1)
__gc_list_int_set(one, 0, 99)
println(__gc_list_int_min(one))
println(__gc_list_int_max(one))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc reductions build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    // sum(4 + -2 + 7 + 1 + -10 + 3) = 3, min = -10, max = 7, sum([]) = 0,
    // min([99]) = 99, max([99]) = 99.
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "3\n-10\n7\n0\n99\n99\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_min_empty_bounds() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-min-bnd-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-min-bnd-{unique}"));
    fs::write(
        &source_path,
        r#"val xs = __gc_list_int(0)
__gc_list_int_min(xs)
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(build.status.success());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "klassic gc: index out of bounds\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_split_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-split-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-split-{unique}"));
    fs::write(
        &source_path,
        r#"val parts = __gc_string_split(__gc_string("a,b,c"), 44)
println(__gc_list_ptr_len(parts))
__gc_string_println(__gc_list_ptr_get(parts, 0))
__gc_string_println(__gc_list_ptr_get(parts, 1))
__gc_string_println(__gc_list_ptr_get(parts, 2))
val empty_first = __gc_string_split(__gc_string(",x,y"), 44)
println(__gc_list_ptr_len(empty_first))
__gc_string_println(__gc_list_ptr_get(empty_first, 0))
__gc_string_println(__gc_list_ptr_get(empty_first, 1))
__gc_string_println(__gc_list_ptr_get(empty_first, 2))
val no_sep = __gc_string_split(__gc_string("hello"), 44)
println(__gc_list_ptr_len(no_sep))
__gc_string_println(__gc_list_ptr_get(no_sep, 0))
val empty_input = __gc_string_split(__gc_string(""), 44)
println(__gc_list_ptr_len(empty_input))
println(__gc_string_len(__gc_list_ptr_get(empty_input, 0)))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string_split build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "3\na\nb\nc\n3\n\nx\ny\n1\nhello\n1\n0\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_ptr_join_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-join-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-join-{unique}"));
    fs::write(
        &source_path,
        r#"mutable parts = __gc_list_ptr(0)
parts = __gc_list_ptr_push(parts, __gc_string("alpha"))
parts = __gc_list_ptr_push(parts, __gc_string("beta"))
parts = __gc_list_ptr_push(parts, __gc_string("gamma"))
__gc_string_println(__gc_list_ptr_join(parts, __gc_string(", ")))
val empty_sep_parts = __gc_list_ptr(0)
__gc_string_println(__gc_list_ptr_join(empty_sep_parts, __gc_string("--")))
mutable single = __gc_list_ptr(0)
single = __gc_list_ptr_push(single, __gc_string("alone"))
__gc_string_println(__gc_list_ptr_join(single, __gc_string(", ")))
mutable two = __gc_list_ptr(0)
two = __gc_list_ptr_push(two, __gc_string("ab"))
two = __gc_list_ptr_push(two, __gc_string("cd"))
__gc_string_println(__gc_list_ptr_join(two, __gc_string("")))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list_ptr_join build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "alpha, beta, gamma\n\nalone\nabcd\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_ptr_get_string_preserves_heap_string_value() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-list-get-string-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-gc-list-get-string-{unique}"));
    fs::write(
        &source_path,
        r#"mutable parts = __gc_list_ptr(0)
parts = __gc_list_ptr_push(parts, __gc_string("alpha"))
parts = __gc_list_ptr_push(parts, __gc_string("beta"))
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
val first = __gc_list_ptr_get_string(parts, 0)
val second = __gc_list_ptr_get_string(parts, 1)
println(first)
println(second + __gc_string("!"))
assertResult("alpha")(toString(first))
assertResult("beta!")(toString(second + __gc_string("!")))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list_ptr_get_string build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "alpha\nbeta!\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_int_to_string_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-i2s-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-i2s-{unique}"));
    fs::write(
        &source_path,
        r#"__gc_string_println(__gc_int_to_string(0))
__gc_string_println(__gc_int_to_string(7))
__gc_string_println(__gc_int_to_string(42))
__gc_string_println(__gc_int_to_string(-1))
__gc_string_println(__gc_int_to_string(-12345))
__gc_string_println(__gc_int_to_string(123456789))
println(__gc_string_len(__gc_int_to_string(0)))
println(__gc_string_len(__gc_int_to_string(-99)))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc int_to_string build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "0\n7\n42\n-1\n-12345\n123456789\n1\n3\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_starts_ends_with() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-se-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-se-{unique}"));
    fs::write(
        &source_path,
        r#"val s = __gc_string("klassic")
println(__gc_string_starts_with(s, __gc_string("kla")))
println(__gc_string_starts_with(s, __gc_string("klax")))
println(__gc_string_starts_with(s, __gc_string("")))
println(__gc_string_starts_with(s, __gc_string("klassic+")))
println(__gc_string_starts_with(s, __gc_string("klassic")))
println(__gc_string_ends_with(s, __gc_string("sic")))
println(__gc_string_ends_with(s, __gc_string("xic")))
println(__gc_string_ends_with(s, __gc_string("")))
println(__gc_string_ends_with(s, __gc_string("klassic+")))
println(__gc_string_ends_with(s, __gc_string("klassic")))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc starts/ends_with build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "true\nfalse\ntrue\nfalse\ntrue\ntrue\nfalse\ntrue\nfalse\ntrue\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_contains() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-cnt-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-cnt-{unique}"));
    fs::write(
        &source_path,
        r#"val s = __gc_string("the quick brown fox")
println(__gc_string_contains(s, __gc_string("the")))
println(__gc_string_contains(s, __gc_string("quick")))
println(__gc_string_contains(s, __gc_string("fox")))
println(__gc_string_contains(s, __gc_string("slow")))
println(__gc_string_contains(s, __gc_string("")))
println(__gc_string_contains(s, __gc_string("the quick brown fox jumps")))
println(__gc_string_contains(__gc_string(""), __gc_string("a")))
println(__gc_string_contains(__gc_string(""), __gc_string("")))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string_contains build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "true\ntrue\ntrue\nfalse\ntrue\nfalse\nfalse\ntrue\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_reverse() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-rev-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-rev-{unique}"));
    fs::write(
        &source_path,
        r#"mutable xs = __gc_list_int(5)
__gc_list_int_set(xs, 0, 10)
__gc_list_int_set(xs, 1, 20)
__gc_list_int_set(xs, 2, 30)
__gc_list_int_set(xs, 3, 40)
__gc_list_int_set(xs, 4, 50)
__gc_list_int_println(__gc_list_int_reverse(xs))
val one = __gc_list_int(1)
__gc_list_int_set(one, 0, 99)
__gc_list_int_println(__gc_list_int_reverse(one))
val empty = __gc_list_int(0)
__gc_list_int_println(__gc_list_int_reverse(empty))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list_int_reverse build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[50, 40, 30, 20, 10]\n[99]\n[]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_repeat_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-rep-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-rep-{unique}"));
    fs::write(
        &source_path,
        r#"__gc_string_println(__gc_string_repeat(__gc_string("ab"), 3))
	println(__gc_string_len(__gc_string_repeat(__gc_string("x"), 0)))
	println(__gc_string_len(__gc_string_repeat(__gc_string(""), 5)))
	println(__gc_string_len(__gc_string_repeat(__gc_string(""), 9223372036854775807)))
	__gc_string_println(__gc_string_repeat(__gc_string("ha"), 4))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string_repeat build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "ababab\n0\n0\n0\nhahahaha\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_repeat_negative_count() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-rep-neg-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-rep-neg-{unique}"));
    fs::write(
        &source_path,
        r#"val s = __gc_string("hi")
__gc_string_repeat(s, -1)
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(build.status.success());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "klassic gc: index out of bounds\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_repeat_size_overflow() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-rep-overflow-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-rep-overflow-{unique}"));
    fs::write(
        &source_path,
        r#"__gc_string_repeat(__gc_string("xx"), 4611686018427385834)
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string_repeat overflow build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        format!(
            "{}:1:1: __gc_string_repeat allocation size overflow\n",
            source_path.display()
        )
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_index_of_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-idx-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-idx-{unique}"));
    fs::write(
        &source_path,
        r#"val s = __gc_string("klassic")
println(__gc_string_index_of(s, 107))
println(__gc_string_index_of(s, 97))
println(__gc_string_index_of(s, 99))
println(__gc_string_index_of(s, 122))
val empty = __gc_string("")
println(__gc_string_index_of(empty, 97))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string_index_of build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    // 107='k' at 0, 97='a' at 2, 99='c' at 6, 122='z' missing, 97 in empty missing.
    assert_eq!(String::from_utf8_lossy(&run.stdout), "0\n2\n6\n-1\n-1\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_index_of_from_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-idx-from-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-idx-from-{unique}"));
    fs::write(
        &source_path,
        r#"val s = __gc_string("banana")
println(__gc_string_index_of_from(s, 97, 0))
println(__gc_string_index_of_from(s, 97, 2))
println(__gc_string_index_of_from(s, 97, 4))
println(__gc_string_index_of_from(s, 97, 6))
println(__gc_string_index_of_from(s, 97, 99))
println(__gc_string_index_of_from(s, 110, 2))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string_index_of_from build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "1\n3\n5\n-1\n-1\n2\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_last_index_of_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-last-idx-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-last-idx-{unique}"));
    fs::write(
        &source_path,
        r#"val s = __gc_string("banana")
println(__gc_string_last_index_of(s, 98))
println(__gc_string_last_index_of(s, 97))
println(__gc_string_last_index_of(s, 110))
println(__gc_string_last_index_of(s, 122))
val one = __gc_string("x")
println(__gc_string_last_index_of(one, 120))
val empty = __gc_string("")
println(__gc_string_last_index_of(empty, 97))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string_last_index_of build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "0\n5\n4\n-1\n0\n-1\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_index_of_from_negative_start() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-idx-from-negative-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-gc-idx-from-negative-{unique}"));
    fs::write(
        &source_path,
        r#"__gc_string_index_of_from(__gc_string("abc"), 97, -1)
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(build.status.success());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert!(run.stdout.is_empty());
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "klassic gc: index out of bounds\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_to_int_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-toi-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-toi-{unique}"));
    fs::write(
        &source_path,
        r#"println(__gc_string_to_int(__gc_string("42")))
println(__gc_string_to_int(__gc_string("0")))
println(__gc_string_to_int(__gc_string("-7")))
println(__gc_string_to_int(__gc_string("")))
println(__gc_string_to_int(__gc_string("100x")))
println(__gc_string_to_int(__gc_string("abc")))
println(__gc_string_to_int(__gc_string("123456789")))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string_to_int build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "42\n0\n-7\n0\n100\n0\n123456789\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_len_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-len-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-len-{unique}"));
    fs::write(
        &source_path,
        r#"val empty = __gc_list_int(0)
println(__gc_list_int_len(empty))
val fixed = __gc_list_int(3)
println(__gc_list_int_len(fixed))
mutable xs = __gc_list_int(0)
xs = __gc_list_int_push(xs, 10)
xs = __gc_list_int_push(xs, 20)
println(__gc_list_int_len(xs))
xs = __gc_list_int_pop(xs)
println(__gc_list_int_len(xs))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list_int_len build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "0\n3\n2\n1\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_pop_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-pop-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-pop-{unique}"));
    fs::write(
        &source_path,
        r#"mutable xs = __gc_list_int(3)
__gc_list_int_set(xs, 0, 10)
__gc_list_int_set(xs, 1, 20)
__gc_list_int_set(xs, 2, 30)
xs = __gc_list_int_pop(xs)
__gc_list_int_println(xs)
xs = __gc_list_int_pop(xs)
__gc_list_int_println(xs)
xs = __gc_list_int_pop(xs)
__gc_list_int_println(xs)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list_int_pop build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "[10, 20]\n[10]\n[]\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_pop_empty_bounds() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-pop-bnd-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-pop-bnd-{unique}"));
    fs::write(
        &source_path,
        r#"val xs = __gc_list_int(0)
__gc_list_int_pop(xs)
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(build.status.success());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "klassic gc: index out of bounds\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_concat_basic() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-cc2-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-cc2-{unique}"));
    fs::write(
        &source_path,
        r#"mutable a = __gc_list_int(2)
__gc_list_int_set(a, 0, 1)
__gc_list_int_set(a, 1, 2)
mutable b = __gc_list_int(3)
__gc_list_int_set(b, 0, 3)
__gc_list_int_set(b, 1, 4)
__gc_list_int_set(b, 2, 5)
__gc_list_int_println(__gc_list_concat(a, b))
val empty = __gc_list_int(0)
__gc_list_int_println(__gc_list_concat(empty, a))
__gc_list_int_println(__gc_list_concat(a, empty))
__gc_list_int_println(__gc_list_concat(empty, empty))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list_concat build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[1, 2, 3, 4, 5]\n[1, 2]\n[1, 2]\n[]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_ptr_push_grows_list() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-pp-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-pp-{unique}"));
    fs::write(
        &source_path,
        r#"mutable xs = __gc_list_ptr(0)
val a = __gc_alloc(16)
__gc_write(a, 0, 11)
val b = __gc_alloc(16)
__gc_write(b, 0, 22)
val c = __gc_alloc(16)
__gc_write(c, 0, 33)
xs = __gc_list_ptr_push(xs, a)
xs = __gc_list_ptr_push(xs, b)
xs = __gc_list_ptr_push(xs, c)
println(__gc_list_ptr_len(xs))
println(__gc_read(__gc_list_ptr_get(xs, 0), 0))
println(__gc_read(__gc_list_ptr_get(xs, 1), 0))
println(__gc_read(__gc_list_ptr_get(xs, 2), 0))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list_ptr_push build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n11\n22\n33\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_ptr_push_traces_inline_children() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-pp-survive-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-pp-survive-{unique}"));
    // The appended children are only ever held by the list — they are
    // inline __gc_alloc results never bound to a val. The list's tag-4
    // mark phase must be the sole reason they survive both the
    // destination allocations triggered by each push and the explicit
    // heap-pressure collection that follows.
    fs::write(
        &source_path,
        r#"mutable xs = __gc_list_ptr(0)
xs = __gc_list_ptr_push(xs, __gc_alloc(16))
__gc_write(__gc_list_ptr_get(xs, 0), 0, 100)
xs = __gc_list_ptr_push(xs, __gc_alloc(16))
__gc_write(__gc_list_ptr_get(xs, 1), 0, 200)
xs = __gc_list_ptr_push(xs, __gc_alloc(16))
__gc_write(__gc_list_ptr_get(xs, 2), 0, 300)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
println(__gc_list_ptr_len(xs))
println(__gc_read(__gc_list_ptr_get(xs, 0), 0))
println(__gc_read(__gc_list_ptr_get(xs, 1), 0))
println(__gc_read(__gc_list_ptr_get(xs, 2), 0))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list_ptr_push (survive) build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n100\n200\n300\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_push_grows_list() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-push-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-push-{unique}"));
    fs::write(
        &source_path,
        r#"mutable xs = __gc_list_int(0)
println(__gc_list_int_println(xs))
xs = __gc_list_int_push(xs, 10)
__gc_list_int_println(xs)
xs = __gc_list_int_push(xs, 20)
xs = __gc_list_int_push(xs, 30)
__gc_list_int_println(xs)
xs = __gc_list_int_push(xs, 40)
xs = __gc_list_int_push(xs, 50)
__gc_list_int_println(xs)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list-push build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[]\n()\n[10]\n[10, 20, 30]\n[10, 20, 30, 40, 50]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_push_survives_collection() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-push-survive-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-push-survive-{unique}"));
    // Build a list of length 6 incrementally with a heap-pressure
    // collection between pushes. Each iteration discards the previous
    // list (the old one is unreachable once xs is reassigned), so the
    // collector must reclaim only the dropped versions while keeping
    // the freshly-pushed list intact.
    fs::write(
        &source_path,
        r#"mutable xs = __gc_list_int(0)
xs = __gc_list_int_push(xs, 1)
foreach(i in [1, 2, 3, 4, 5, 6, 7]) {
  __gc_alloc(150000)
}
__gc_collect()
xs = __gc_list_int_push(xs, 2)
xs = __gc_list_int_push(xs, 3)
foreach(i in [1, 2, 3, 4, 5, 6, 7]) {
  __gc_alloc(150000)
}
__gc_collect()
xs = __gc_list_int_push(xs, 4)
xs = __gc_list_int_push(xs, 5)
xs = __gc_list_int_push(xs, 6)
__gc_collect()
__gc_list_int_println(xs)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc list-push (survive) build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "[1, 2, 3, 4, 5, 6]\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_substring_in_bounds() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-sub-ok-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-sub-ok-{unique}"));
    fs::write(
        &source_path,
        r#"val s = __gc_string("klassic")
val mid = __gc_string_substring(s, 1, 5)
__gc_string_println(mid)
println(__gc_string_len(mid))
val whole = __gc_string_substring(s, 0, 7)
__gc_string_println(whole)
val empty = __gc_string_substring(s, 3, 3)
println(__gc_string_len(empty))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc substring build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "lass\n4\nklassic\n0\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_substring_survives_collection() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-sub-survive-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-sub-survive-{unique}"));
    // The destination allocation can trigger a collection; the source
    // string must survive because it is bound to an auto-rooted slot.
    fs::write(
        &source_path,
        r#"val s = __gc_string("the quick brown fox jumps over the lazy dog")
val piece = __gc_string_substring(s, 16, 19)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
__gc_string_println(piece)
__gc_string_println(s)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc substring (survive) build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "fox\nthe quick brown fox jumps over the lazy dog\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_substring_negative_start() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-sub-neg-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-sub-neg-{unique}"));
    fs::write(
        &source_path,
        r#"val s = __gc_string("klassic")
__gc_string_substring(s, -1, 3)
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(build.status.success());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "klassic gc: index out of bounds\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_substring_end_past_length() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-sub-past-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-sub-past-{unique}"));
    fs::write(
        &source_path,
        r#"val s = __gc_string("klassic")
__gc_string_substring(s, 0, 8)
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(build.status.success());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "klassic gc: index out of bounds\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_substring_start_after_end() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-sub-bad-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-sub-bad-{unique}"));
    fs::write(
        &source_path,
        r#"val s = __gc_string("klassic")
__gc_string_substring(s, 5, 2)
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(build.status.success());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "klassic gc: index out of bounds\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_collect_counter() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-cc-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-cc-{unique}"));
    fs::write(
        &source_path,
        r#"println(__gc_collect_count())
__gc_collect()
println(__gc_collect_count())
__gc_collect()
__gc_collect()
println(__gc_collect_count())
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc collect-counter build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "0\n1\n3\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_bounds_check_negative() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-bnd-neg-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-bnd-neg-{unique}"));
    fs::write(
        &source_path,
        r#"val xs = __gc_list_int(3)
__gc_list_int_set(xs, -1, 99)
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc bounds (negative) build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "klassic gc: index out of bounds\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_bounds_check_overflow() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-bnd-ovf-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-bnd-ovf-{unique}"));
    fs::write(
        &source_path,
        r#"val xs = __gc_list_int(3)
__gc_list_int_set(xs, 0, 1)
__gc_list_int_set(xs, 1, 2)
__gc_list_int_set(xs, 2, 3)
println(__gc_list_int_get(xs, 3))
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc bounds (overflow) build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "klassic gc: index out of bounds\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_ptr_traces_skipping_length_qword() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-listptr-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-listptr-{unique}"));
    // Build four heap blocks, store all into a heap-backed pointer
    // list (tag 4), pin only the list, then stress the heap. The mark
    // phase must skip the leading length qword (== 4) — interpreting it
    // as a heap pointer would dereference an out-of-heap address. The
    // four children must survive and read back their original
    // sentinels through __gc_list_ptr_get.
    fs::write(
        &source_path,
        r#"val list = __gc_list_ptr(4)
val c0 = __gc_alloc(16)
__gc_write(c0, 0, 1001)
val c1 = __gc_alloc(16)
__gc_write(c1, 0, 1002)
val c2 = __gc_alloc(16)
__gc_write(c2, 0, 1003)
val c3 = __gc_alloc(16)
__gc_write(c3, 0, 1004)
__gc_list_ptr_set(list, 0, c0)
__gc_list_ptr_set(list, 1, c1)
__gc_list_ptr_set(list, 2, c2)
__gc_list_ptr_set(list, 3, c3)
__gc_pin(list)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
println(__gc_list_ptr_len(list))
println(__gc_read(__gc_list_ptr_get(list, 0), 0))
println(__gc_read(__gc_list_ptr_get(list, 1), 0))
println(__gc_read(__gc_list_ptr_get(list, 2), 0))
println(__gc_read(__gc_list_ptr_get(list, 3), 0))
__gc_unpin(list)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc list-ptr build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "4\n1001\n1002\n1003\n1004\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_list_int_round_trip() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-listint-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-listint-{unique}"));
    // Allocate a heap-backed Int list, populate it, force a heap pressure
    // collection while the list is rooted only by its stack slot, then
    // verify both the per-element get path and the println path read the
    // original values back.
    fs::write(
        &source_path,
        r#"val xs = __gc_list_int(5)
__gc_list_int_set(xs, 0, 10)
__gc_list_int_set(xs, 1, 20)
__gc_list_int_set(xs, 2, 30)
__gc_list_int_set(xs, 3, 40)
__gc_list_int_set(xs, 4, 50)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
println(__gc_list_int_get(xs, 0))
println(__gc_list_int_get(xs, 4))
__gc_list_int_println(xs)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc list-int build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "10\n50\n[10, 20, 30, 40, 50]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_concat_under_heap_pressure() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-str-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-str-{unique}"));
    // Bind two heap-allocated strings, concat them into a third heap
    // string, drop the originals from explicit reach by overwriting their
    // mutable slots with shorter throwaway strings, and stress the heap
    // hard enough that the bump path overflows. The auto-rooted slots for
    // the two latest values plus the concatenation result must keep their
    // heap objects alive across the collection cycle.
    fs::write(
        &source_path,
        r#"val a = __gc_string("hello")
val b = __gc_string(", world!")
val c = __gc_string_concat(a, b)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_string(",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,")
}
__gc_collect()
__gc_string_println(c)
val d = __gc_string_concat(c, __gc_string(" again"))
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
  __gc_alloc(150000)
}
__gc_collect()
__gc_string_println(d)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc string build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "hello, world!\nhello, world! again\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_concat_size_overflow() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-str-overflow-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-str-overflow-{unique}"));
    fs::write(
        &source_path,
        r#"val a = __gc_string_alloc(0)
__gc_write(a, 0, 9223372036854771666)
val b = __gc_string("x")
__gc_string_concat(a, b)
println("not reached")
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");
    assert!(
        build.status.success(),
        "gc string concat overflow build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(1));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        format!(
            "{}:4:1: __gc_string_concat allocation size overflow\n",
            source_path.display()
        )
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_from_runtime_file_input() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-runtime-str-{unique}.kl"));
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-gc-runtime-str-{unique}.txt"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-runtime-str-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"val raw = FileInput#all("{}")
val heap = __gc_string(raw)
__gc_string_println(heap)
println(__gc_string_len(heap))
val tagged = __gc_string_concat(__gc_string("got:"), heap)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {{
  __gc_alloc(150000)
}}
__gc_collect()
__gc_string_println(tagged)
"#,
            input_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc runtime string build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    fs::write(&input_path, "dynamic text").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "gc runtime string run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "dynamic text\n12\ngot:dynamic text\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_plus() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-str-plus-{unique}.kl"));
    let input_path = std::env::temp_dir().join(format!("klassic-native-gc-str-plus-{unique}.txt"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-str-plus-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"val a = __gc_string("hello")
val b = __gc_string(", ")
val c = a + b + __gc_string("world") + __gc_string("!")
__gc_string_println(c)
val raw = FileInput#all("{}")
val lifted = __gc_string(raw)
val d = c + __gc_string(" / ") + lifted
val e = __gc_string("prefix: ") + raw
val f = "static " + c
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {{
  __gc_alloc(150000)
}}
__gc_collect()
__gc_string_println(d)
println(e)
println(f)
assertResult("prefix: runtime")(toString(e))
assertResult("static hello, world!")(toString(f))
"#,
            input_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc string plus build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    fs::write(&input_path, "runtime").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "gc string plus run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "hello, world!\nhello, world! / runtime\nprefix: runtime\nstatic hello, world!\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_gc_heap_values_in_high_level_collection_literals() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-list-literal-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-list-literal-{unique}"));
    fs::write(
        &source_path,
        r#"println([__gc_string("a"), __gc_string("b")])"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("high-level collection literals containing GC heap pointers"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_plain_ints_for_gc_address_builtins() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-plain-int-addr-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-gc-plain-int-addr-{unique}"));
    fs::write(&source_path, "__gc_string_println(42)\n").expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("native __gc_string_println for non-address argument"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_plain_ints_for_gc_string_concat() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-plain-int-concat-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-gc-plain-int-concat-{unique}"));
    fs::write(
        &source_path,
        "__gc_string_println(__gc_string_concat(42, __gc_string(\"x\")))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("native __gc_string_concat for non-address first argument"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_plain_ints_for_gc_raw_memory_helpers() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-plain-int-read-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-gc-plain-int-read-{unique}"));
    fs::write(&source_path, "println(__gc_read(42, 0))\n").expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("native __gc_read for non-address argument"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_plain_ints_for_gc_pointer_reads() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-plain-int-read-ptr-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-gc-plain-int-read-ptr-{unique}"));
    fs::write(&source_path, "__gc_string_println(__gc_read_ptr(42, 0))\n")
        .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("native __gc_read_ptr for non-address argument"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_plain_ints_for_gc_string_reads() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-gc-plain-int-read-string-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-gc-plain-int-read-string-{unique}"));
    fs::write(&source_path, "println(__gc_read_string(42, 0))\n").expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("native __gc_read_string for non-address argument"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_plain_ints_for_gc_string_index_of_from() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-gc-plain-int-string-index-from-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-gc-plain-int-string-index-from-{unique}"
    ));
    fs::write(
        &source_path,
        "println(__gc_string_index_of_from(42, 97, 0))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("native __gc_string_index_of_from for non-address source"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_plain_ints_for_gc_string_last_index_of() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-gc-plain-int-string-last-index-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-gc-plain-int-string-last-index-{unique}"
    ));
    fs::write(&source_path, "println(__gc_string_last_index_of(42, 97))\n")
        .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("native __gc_string_last_index_of for non-address source"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_plain_ints_for_gc_list_int_len() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-gc-plain-int-list-int-len-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-gc-plain-int-list-int-len-{unique}"));
    fs::write(&source_path, "println(__gc_list_int_len(42))\n").expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("native __gc_list_int_len for non-address list argument"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_plain_ints_for_gc_list_ptr_get_string() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-gc-plain-int-list-get-string-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-gc-plain-int-list-get-string-{unique}"
    ));
    fs::write(&source_path, "println(__gc_list_ptr_get_string(42, 0))\n")
        .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("native __gc_list_ptr_get_string for non-address list argument"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_plain_ints_for_gc_smap_get_string() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-gc-plain-int-smap-get-string-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-gc-plain-int-smap-get-string-{unique}"
    ));
    fs::write(
        &source_path,
        "println(__gc_smap_get_string(42, __gc_string(\"x\")))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr)
            .contains("native __gc_smap_get_string for non-address map"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_to_string_bridge() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-str-tostring-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-str-tostring-{unique}"));
    fs::write(
        &source_path,
        r#"val h = __gc_string("heap")
val s = toString(h)
val method = h.toString()
println(s)
println(method)
println(length(s))
val shouted = toString(h + __gc_string("!"))
println(shouted)
assertResult("heap")(s)
assertResult("heap")(method)
assertResult("heap!")(shouted)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc string toString build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "gc string toString run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "heap\nheap\n4\nheap!\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_interpolation_value() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-str-interp-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-str-interp-{unique}"));
    fs::write(
        &source_path,
        r#"val h = __gc_string("heap")
val combined = h + __gc_string("!")
val message = "value=#{h}; combined=#{combined}; len=#{__gc_string_len(combined)}"
println(message)
assertResult("value=heap; combined=heap!; len=5")(message)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc string interpolation build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "gc string interpolation run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "value=heap; combined=heap!; len=5\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_equality_operators() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-str-eq-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-str-eq-{unique}"));
    fs::write(
        &source_path,
        r#"val a = __gc_string("same")
val b = __gc_string("same")
val c = a + __gc_string("!")
println(a == b)
println(a != b)
println(a == c)
println(a != c)
assert(a == b)
assert(a != c)
assertResult(a)(b)
assertResult(__gc_string("same!"))(c)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc string equality build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "gc string equality run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "true\nfalse\nfalse\ntrue\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_string_equality_roots_left_temporaries() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-gc-str-eq-root-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-str-eq-root-{unique}"));
    fs::write(
        &source_path,
        r#"println(__gc_string("same") == {
  foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
    __gc_alloc(150000)
  }
  __gc_collect()
  __gc_string("same")
})
println(__gc_string_eq(__gc_string("same"), {
  foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
    __gc_alloc(150000)
  }
  __gc_collect()
  __gc_string("same")
}))
assertResult(__gc_string("same"))({
  foreach(i in [1, 2, 3, 4, 5, 6, 7, 8]) {
    __gc_alloc(150000)
  }
  __gc_collect()
  __gc_string("same")
})
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc string equality rooting build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "gc string equality rooting run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "true\ntrue\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_array_traces_packed_pointer_payload() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-array-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-array-{unique}"));
    // __gc_array(n) allocates n pointer slots tagged GC_TYPE_POINTER_ARRAY.
    // The mark phase must walk every payload qword identically to a record:
    // pin only the array, drop the direct child slots, and push the heap
    // hard so a collection runs. Both children must still hold their
    // sentinels afterwards, proving tag-3 trace coverage.
    fs::write(
        &source_path,
        r#"val arr = __gc_array(3)
val c0 = __gc_alloc(16)
__gc_write(c0, 0, 100)
val c1 = __gc_alloc(16)
__gc_write(c1, 0, 200)
val c2 = __gc_alloc(16)
__gc_write(c2, 0, 300)
__gc_write(arr, 0, c0)
__gc_write(arr, 8, c1)
__gc_write(arr, 16, c2)
__gc_pin(arr)
foreach(i in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
  __gc_alloc(150000)
}
__gc_collect()
println(__gc_read(c0, 0))
println(__gc_read(c1, 0))
println(__gc_read(c2, 0))
__gc_unpin(arr)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc array build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "100\n200\n300\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_gc_heap_growth_when_roots_outlive_initial_segment() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-gc-grow-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-gc-grow-{unique}"));
    // Eight 150_000-byte allocations totalling ~1.2 MiB cannot fit in the
    // initial 1 MiB segment. Each one is bound to a stack-rooted slot, so
    // a collection cycle reclaims nothing and the bump allocator must grow
    // the heap by mmapping a fresh segment. After the second collection,
    // both segments must still be walked correctly during sweep.
    fs::write(
        &source_path,
        r#"val a0 = __gc_alloc(150000)
val a1 = __gc_alloc(150000)
val a2 = __gc_alloc(150000)
val a3 = __gc_alloc(150000)
val a4 = __gc_alloc(150000)
val a5 = __gc_alloc(150000)
val a6 = __gc_alloc(150000)
val a7 = __gc_alloc(150000)
__gc_write(a0, 0, 1)
__gc_write(a3, 0, 30)
__gc_write(a7, 0, 700)
__gc_collect()
println(__gc_read(a0, 0))
println(__gc_read(a3, 0))
println(__gc_read(a7, 0))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "gc heap-growth build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "1\n30\n700\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_list_of_maps_branch() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-list-of-maps-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-list-of-maps-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val xs = if(arg == "a") [%["x": 1, "y": 2], %["z": 9]] else [%["a": 100, "b": 200, "c": 300]]
println(xs)
println(size(xs))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "list of maps branch build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&a_run.stdout),
        "[%[x: 1, y: 2], %[z: 9]]\n2\n"
    );
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&b_run.stdout),
        "[%[a: 100, b: 200, c: 300]]\n1\n"
    );
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_list_of_sets_branch() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-list-of-sets-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-list-of-sets-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val xs = if(arg == "a") [%(1 2 3), %(4 5)] else [%(99 100 200 300)]
println(xs)
println(size(xs))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "list of sets branch build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&a_run.stdout),
        "[%(1, 2, 3), %(4, 5)]\n2\n"
    );
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&b_run.stdout),
        "[%(99, 100, 200, 300)]\n1\n"
    );
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_collection_display() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-display-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-display-{unique}"));
    fs::write(
        &source_path,
        r#"val arg = head(args())
val xs = if(arg == "a") [1, 2, 3] else [10, 20]
val s = if(arg == "a") %(1 2 3) else %(10 20)
val m = if(arg == "a") %["x": 1, "y": 2] else %["a": 99]
println(xs)
println(s)
println(m)
println("xs=" + xs)
println("s=" + s)
println("m=" + m)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime collection display build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let a_run = Command::new(&output_path)
        .arg("a")
        .output()
        .expect("generated executable should run a");
    let b_run = Command::new(&output_path)
        .arg("b")
        .output()
        .expect("generated executable should run b");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(a_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&a_run.stdout),
        "[1, 2, 3]\n%(1, 2, 3)\n%[x: 1, y: 2]\nxs=[1, 2, 3]\ns=%(1, 2, 3)\nm=%[x: 1, y: 2]\n"
    );
    assert!(a_run.stderr.is_empty());

    assert!(b_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&b_run.stdout),
        "[10, 20]\n%(10, 20)\n%[a: 99]\nxs=[10, 20]\ns=%(10, 20)\nm=%[a: 99]\n"
    );
    assert!(b_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_with_method_map_results() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-method-map-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-method-map-{unique}"));
    fs::write(
        &source_path,
        r#"val key = head(args())
val xs = if(key == "left") [1, 2].map((x) => x + 1) else [10, 20, 30].map((x) => x * 2)
println(xs)
println(size(xs))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if method map results build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(String::from_utf8_lossy(&left_run.stdout), "[2, 3]\n2\n");
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "[20, 40, 60]\n3\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_nested_static_int_list_branches() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-nested-int-list-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-nested-int-list-{unique}"
    ));
    fs::write(
        &source_path,
        r#"val key = head(args())
val same: List<List<Int>> = if(key == "left") [[1, 2], [3, 4]] else [[5, 6], [7, 8]]
val differentOuter: List<List<Int>> = if(key == "left") [[10, 20], [30, 40]] else [[50, 60]]
println(head(same))
println(head(tail(same)))
println(size(same))
println(head(differentOuter))
println(size(differentOuter))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if nested int-list build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&left_run.stdout),
        "[1, 2]\n[3, 4]\n2\n[10, 20]\n2\n"
    );
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "[5, 6]\n[7, 8]\n2\n[50, 60]\n1\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_nested_int_list_branches_with_different_inner_lengths() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-nested-inner-len-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-nested-inner-len-{unique}"
    ));
    fs::write(
        &source_path,
        r#"val key = head(args())
val xs: List<List<Int>> = if(key == "left") [[1, 2, 3], [4]] else [[5], [6, 7, 8]]
println(head(xs))
println(head(tail(xs)))
println(size(xs))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if nested-inner-len build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&left_run.stdout),
        "[1, 2, 3]\n[4]\n2\n"
    );
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "[5]\n[6, 7, 8]\n2\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_three_level_nested_list_branches() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-3level-nested-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-3level-nested-{unique}"));
    fs::write(
        &source_path,
        r#"val key = head(args())
val xs: List<List<List<Int>>> = if(key == "left") [[[1, 2]]] else [[[3, 4, 5]]]
val ys: List<List<List<Int>>> = if(key == "left") [[[1], [2, 3]]] else [[[4, 5], [6]]]
println(head(head(xs)))
println(head(head(ys)))
println(head(tail(head(ys))))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if 3-level nested build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&left_run.stdout),
        "[1, 2]\n[1]\n[2, 3]\n"
    );
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "[3, 4, 5]\n[4, 5]\n[6]\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_record_list_branches_with_different_lengths() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-record-list-len-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-record-list-len-{unique}"
    ));
    fs::write(
        &source_path,
        r#"record Box {
  count: Int
  label: String
}
val key = head(args())
val rs = if(key == "left") [#Box(1, "alpha")] else [#Box(2, "beta"), #Box(3, "gamma")]
println(size(rs))
println(head(rs).count)
println(head(rs).label)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if record-list-len build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(String::from_utf8_lossy(&left_run.stdout), "1\n1\nalpha\n");
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(String::from_utf8_lossy(&right_run.stdout), "2\n2\nbeta\n");
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_record_list_branches() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-record-list-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-record-list-{unique}"));
    fs::write(
        &source_path,
        r#"record Pt {
  x: Int
  y: Int
}
val key = head(args())
val rs = if(key == "left") [#Pt(1, 2), #Pt(3, 4)] else [#Pt(5, 6), #Pt(7, 8)]
val first = head(rs)
val second = head(tail(rs))
println(first.x)
println(first.y)
println(second.x)
println(second.y)
println(size(rs))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if record-list build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(String::from_utf8_lossy(&left_run.stdout), "1\n2\n3\n4\n2\n");
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "5\n6\n7\n8\n2\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_static_string_list_branches_with_different_lengths() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-static-string-list-len-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-static-string-list-len-{unique}"
    ));
    fs::write(
        &source_path,
        r#"val key = head(args())
val xs: List<String> = if(key == "left") ["alpha", "beta"] else ["gamma", "delta", "epsilon"]
println(size(xs))
println(head(xs))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if differing-length string-list build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let left_run = Command::new(&output_path)
        .arg("left")
        .output()
        .expect("generated executable should run left");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(left_run.status.success());
    assert_eq!(String::from_utf8_lossy(&left_run.stdout), "2\nalpha\n");
    assert!(left_run.stderr.is_empty());

    assert!(right_run.status.success());
    assert_eq!(String::from_utf8_lossy(&right_run.stdout), "3\ngamma\n");
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_string_helpers() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-string-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-string-{unique}"));
    fs::write(
        &source_path,
        "val text = \"  Abc123  \"\nval parts = split(\"a,b,c\", \",\")\nval label = \"count=\" + 3\nval decorated = \"parts=\" + parts\nval row = record { label: \"user=\" + \"Alice\", size: \"n=\" + size(parts), methodSize: \"m=\" + parts.size(), first: \"first=\" + head(parts) }\nval folded = foldLeft([\"A\", \"B\", \"C\"])(\"\")((x, y) => x + y)\nval exclaimed = map(parts)((x) => x + \"!\")\nprintln(substring(\"abcdef\", 1, 4))\nprintln(at(\"abc\", 1))\nprintln(trim(text))\nprintln(trimLeft(text))\nprintln(trimRight(text))\nprintln(replace(\"abab\", \"a\", \"x\"))\nprintln(replaceAll(\"a1b2\", \"[0-9]\", \"?\"))\nprintln(toLowerCase(\"AbC\"))\nprintln(toUpperCase(\"AbC\"))\nprintln(\"starts? \" + startsWith(\"hello\", \"he\"))\nprintln(\"ends? \" + endsWith(\"hello\", \"lo\"))\nprintln(\"contains? \" + \"hello\".contains(\"ell\"))\nprintln(\"matches? \" + matches(\"123\", \"[0-9]+\"))\nprintln(\"empty? \" + isEmptyString(\"\"))\nprintln(\"index = \" + indexOf(\"hello world\", \"world\"))\nprintln(\"last = \" + lastIndexOf(\"ababa\", \"ba\"))\nprintln(\"length = \" + length(\"hé\"))\nprintln(repeat(\"ha\", 3))\nprintln(reverse(\"abc\"))\nprintln(parts)\nprintln(join(parts, \"-\"))\nprintln(parts.join(\"|\"))\nprintln(label)\nprintln(decorated)\nprintln(row.label)\nprintln(row.size)\nprintln(row.methodSize)\nprintln(row.first)\nprintln(folded)\nprintln(exclaimed)\nprintln(\"chars = \" + split(\"ab\", \"\"))\nprintln(\"head = \" + head(parts))\nprintln(\"tail = \" + tail(parts))\nprintln(\"parts size = \" + size(parts))\nprintln(\"method size = \" + parts.size())\nmutable seen = 0\nforeach(part in parts) {\n  seen += 1\n  println(\"part = \" + part)\n}\nprintln(\"seen = \" + seen)\nassertResult(\"bcd\")(substring(\"abcdef\", 1, 4))\nassertResult(\"b\")(at(\"abc\", 1))\nassertResult(\"Abc123\")(trim(text))\nassertResult(\"xbab\")(replace(\"abab\", \"a\", \"x\"))\nassertResult(\"a?b?\")(replaceAll(\"a1b2\", \"[0-9]\", \"?\"))\nassertResult(true)(\"hello\".contains(\"ell\"))\nassertResult(true)(matches(\"123\", \"[0-9]+\"))\nassertResult(6)(indexOf(\"hello world\", \"world\"))\nassertResult(3)(lastIndexOf(\"ababa\", \"ba\"))\nassertResult(2)(length(\"hé\"))\nassertResult(\"hahaha\")(repeat(\"ha\", 3))\nassertResult(\"cba\")(reverse(\"abc\"))\nassertResult([\"a\", \"b\", \"c\"])(parts)\nassertResult(\"a-b-c\")(join(parts, \"-\"))\nassertResult(\"a|b|c\")(parts.join(\"|\"))\nassertResult(\"count=3\")(label)\nassertResult(\"parts=[a, b, c]\")(decorated)\nassertResult(\"user=Alice\")(row.label)\nassertResult(\"n=3\")(row.size)\nassertResult(\"m=3\")(row.methodSize)\nassertResult(\"first=a\")(row.first)\nassertResult(\"ABC\")(folded)\nassertResult([\"a!\", \"b!\", \"c!\"])(exclaimed)\nassertResult([\"b\", \"c\"])(tail(parts))\nassertResult(\"a\")(head(parts))\nassertResult(3)(size(parts))\nassertResult(3)(parts.size())\nassertResult(3)(seen)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "bcd\nb\nAbc123\nAbc123  \n  Abc123\nxbab\na?b?\nabc\nABC\nstarts? true\nends? true\ncontains? true\nmatches? true\nempty? true\nindex = 6\nlast = 3\nlength = 2\nhahaha\ncba\n[a, b, c]\na-b-c\na|b|c\ncount=3\nparts=[a, b, c]\nuser=Alice\nn=3\nm=3\nfirst=a\nABC\n[a!, b!, c!]\nchars = [a, b]\nhead = a\ntail = [b, c]\nparts size = 3\nmethod size = 3\npart = a\npart = b\npart = c\nseen = 3\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_string_dynamic_indices() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-static-string-dynamic-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-static-string-dynamic-{unique}"));
    fs::write(
        &source_path,
        r#"val text = "abacad"
mutable i = 0
mutable count = 0
while(i < length(text)) {
  if(text.at(i) == "a") { count += 1 } else { count += 0 }
  i += 1
}
mutable start = 1
mutable end = 4
val direct = substring("abcdef", start, end)
val method = "abcdef".substring(start, end)
println(count)
println(direct)
println(method)
println("xy".at(start))
assertResult(3)(count)
assertResult("bcd")(direct)
assertResult("bcd")(method)
assertResult("y")("xy".at(start))
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "static string dynamic index build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "static string dynamic index run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\nbcd\nbcd\ny\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_split_join_runtime_delimiters() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let delimiter_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-delimiter-{unique}.txt"));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-delimiter-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-delimiter-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"val delimiter = FileInput#all("{}")
val parts = "a,b,c".split(delimiter)
val joined = join(["a", "b", "c"], delimiter)
val methodJoined = ["x", "y"].join(delimiter)
println(parts)
println(join(parts, "|"))
println(joined)
println(methodJoined)
assertResult(["a", "b", "c"])(parts)
assertResult("a,b,c")(joined)
assertResult("x,y")(methodJoined)
"#,
            delimiter_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime delimiter build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&delimiter_path, ",").expect("delimiter should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&delimiter_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime delimiter run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[a, b, c]\na|b|c\na,b,c\nx,y\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_replace_runtime_operands() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let from_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-replace-from-{unique}.txt"));
    let to_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-replace-to-{unique}.txt"));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-replace-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-replace-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"val from = FileInput#all("{}")
val to = FileInput#all("{}")
val direct = replace("a-b-a", from, to)
val method = "left-right-left".replace(from, to)
println(direct)
println(method)
assertResult("a_b-a")(direct)
assertResult("left_right-left")(method)
"#,
            from_path.display(),
            to_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime replace operand build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&from_path, "-").expect("from should write after native build");
    fs::write(&to_path, "_").expect("to should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&from_path);
    let _ = fs::remove_file(&to_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime replace operand run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "a_b-a\nleft_right-left\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_repeat_runtime_count() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let count_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-repeat-count-{unique}.txt"));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-repeat-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-repeat-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"val count = length(FileInput#all("{}"))
val direct = repeat("ha", count)
val method = "ho".repeat(count)
println(direct)
println(method)
assertResult("hahaha")(direct)
assertResult("hohoho")(method)
"#,
            count_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime repeat count build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&count_path, "xxx").expect("count source should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&count_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime repeat count run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "hahaha\nhohoho\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_replace_all_runtime_replacement() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-replace-all-input-{unique}.txt"));
    let replacement_path = std::env::temp_dir().join(format!(
        "klassic-native-replace-all-replacement-{unique}.txt"
    ));
    let digit_pattern_path = std::env::temp_dir().join(format!(
        "klassic-native-replace-all-digit-pattern-{unique}.txt"
    ));
    let literal_pattern_path = std::env::temp_dir().join(format!(
        "klassic-native-replace-all-literal-pattern-{unique}.txt"
    ));
    let empty_pattern_path = std::env::temp_dir().join(format!(
        "klassic-native-replace-all-empty-pattern-{unique}.txt"
    ));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-replace-all-dynamic-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-replace-all-dynamic-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"val input = FileInput#all("{}")
val replacement = FileInput#all("{}")
val digitPattern = FileInput#all("{}")
val literalPattern = FileInput#all("{}")
val emptyPattern = FileInput#all("{}")
val dynamicInput = replaceAll(input, "[0-9]", replacement)
val staticInput = replaceAll("a1b2", "[0-9]", replacement)
val methodInput = "c3d4".replaceAll("[0-9]", replacement)
val runtimePattern = replaceAll(input, digitPattern, replacement)
val staticRuntimePattern = replaceAll("e5f6", digitPattern, replacement)
val literalRuntimePattern = replaceAll("ab_ab", literalPattern, replacement)
val emptyRuntimePattern = replaceAll("hé", emptyPattern, "-")
println(dynamicInput)
println(staticInput)
println(methodInput)
println(runtimePattern)
println(staticRuntimePattern)
println(literalRuntimePattern)
println(emptyRuntimePattern)
assertResult("aXbX")(dynamicInput)
assertResult("aXbX")(staticInput)
assertResult("cXdX")(methodInput)
assertResult("aXbX")(runtimePattern)
assertResult("eXfX")(staticRuntimePattern)
assertResult("X_X")(literalRuntimePattern)
assertResult("-h-é-")(emptyRuntimePattern)
"#,
            input_path.display(),
            replacement_path.display(),
            digit_pattern_path.display(),
            literal_pattern_path.display(),
            empty_pattern_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime replaceAll replacement build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path, "a1b2").expect("input source should write after native build");
    fs::write(&replacement_path, "X").expect("replacement source should write after native build");
    fs::write(&digit_pattern_path, "[0-9]").expect("digit pattern should write after native build");
    fs::write(&literal_pattern_path, "ab")
        .expect("literal pattern should write after native build");
    fs::write(&empty_pattern_path, "").expect("empty pattern should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&replacement_path);
    let _ = fs::remove_file(&digit_pattern_path);
    let _ = fs::remove_file(&literal_pattern_path);
    let _ = fs::remove_file(&empty_pattern_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime replaceAll replacement run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "aXbX\naXbX\ncXdX\naXbX\neXfX\nX_X\n-h-é-\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_builtin_function_aliases() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-builtin-alias-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-builtin-alias-{unique}"));
    fs::write(
        &source_path,
        "val sub = substring\nval lower = toLowerCase\nval lengthOf = length\nval sameSub = sub\nval make = cons\nval mapper = map\nval folder = foldLeft\nval check = assertResult\nval fs = [substring]\nval r = record { f: substring }\nmutable dyn = substring\ndyn = sameSub\nval xs = make(0)([1])\nval ys = mapper([1, 2])((x) => x + 1)\nval trimmed = mapper([\" a \", \" b \"])(trim)\nval lengths = mapper([\"ab\", \"cd\"])(length)\nval total = folder([1, 2, 3])(0)((acc, x) => acc + x)\nprintln(substring)\nprintln(sub)\nprintln(fs)\nprintln(r)\nprintln(r.f(\"abcd\", 1, 3))\nprintln(head(fs)(\"abcd\", 1, 3))\nprintln(({ println(\"pick builtin\"); sub })(\"abcd\", 1, 3))\nprintln(dyn)\nprintln(dyn(\"abcd\", 1, 3))\nprintln(sub(\"BAR\", 0, 1))\nprintln(lower(\"ABC\"))\nprintln(lengthOf(\"hé\"))\nprintln(sameSub(\"abc\", 1, 3))\nprintln(xs)\nprintln(ys)\nprintln(trimmed)\nprintln(lengths)\nprintln(total)\nassertResult(\"bc\")(r.f(\"abcd\", 1, 3))\nassertResult(\"bc\")(head(fs)(\"abcd\", 1, 3))\nassertResult(\"bc\")(dyn(\"abcd\", 1, 3))\nassertResult(\"B\")(sub(\"BAR\", 0, 1))\nassertResult(\"abc\")(lower(\"ABC\"))\nassertResult(2)(lengthOf(\"hé\"))\nassertResult(\"bc\")(sameSub(\"abc\", 1, 3))\ncheck([0, 1])(xs)\ncheck([2, 3])(ys)\ncheck([\"a\", \"b\"])(trimmed)\ncheck([2, 2])(lengths)\ncheck(6)(total)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "<builtin:substring>\n<builtin:substring>\n[<builtin:substring>]\n#(<builtin:substring>)\nbc\nbc\npick builtin\nbc\n<builtin:substring>\nbc\nB\nabc\n2\nbc\n[0, 1]\n[2, 3]\n[a, b]\n[2, 2]\n6\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_conditional_builtin_function_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-conditional-builtin-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-conditional-builtin-{unique}"));
    fs::write(
        &source_path,
        "def chooseCase() = {\n  val nestedFlag = size(CommandLine#args()) == 0\n  if(nestedFlag) toLowerCase else toUpperCase\n}\nval flag = size(CommandLine#args()) == 0\nval lower = toLowerCase\nval upper = toUpperCase\nval pickCase = if(flag) toLowerCase else toUpperCase\nval aliasPick = if(flag) lower else upper\nval fns = [if(flag) lower else upper]\nval rec = record { f: if(flag) toLowerCase else toUpperCase }\nval mapFns = %[\"case\": if(flag) lower else upper]\nval mapKeyFns = %[if(flag) lower else upper: \"case\"]\nval setFns = %(if(flag) lower else upper)\nval aggregateText = \"fns=#{fns}; rec=#{rec}; map=#{mapFns}; mapKey=#{mapKeyFns}; set=#{setFns}\"\nval aggregateConcat = \"concat=\" + fns + \"; rec=\" + rec + \"; map=\" + mapFns + \"; mapKey=\" + mapKeyFns + \"; set=\" + setFns\nval directConcat = \"picked+\" + pickCase\nval returnedPick = chooseCase()\nprintln(if(flag) toLowerCase else toUpperCase)\nprintln(pickCase)\nprintln(aliasPick)\nprintln(head(fns))\nprintln(rec.f)\nprintln(Map#get(mapFns, \"case\"))\nprintln(returnedPick)\nprintln(toString(fns))\nprintln(toString(rec))\nprintln(toString(mapFns))\nprintln(toString(mapKeyFns))\nprintln(toString(setFns))\nprintln(aggregateText)\nprintln(aggregateConcat)\nprintln(directConcat)\nprintln(\"picked=#{aliasPick}\")\nprintln(toString(pickCase))\nprintln(toString(returnedPick))\nprintln((if(flag) toLowerCase else toUpperCase)(\"AbC\"))\nprintln(pickCase(\"AbC\"))\nprintln(aliasPick(\"AbC\"))\nprintln(head(fns)(\"AbC\"))\nprintln(rec.f(\"AbC\"))\nprintln(Map#get(mapFns, \"case\")(\"AbC\"))\nprintln(returnedPick(\"AbC\"))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "conditional builtin function value build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let lower_run = Command::new(&output_path)
        .output()
        .expect("generated executable should run without args");
    let upper_run = Command::new(&output_path)
        .arg("upper")
        .output()
        .expect("generated executable should run with args");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        lower_run.status.success(),
        "conditional builtin function value lower run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&lower_run.stdout),
        String::from_utf8_lossy(&lower_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&lower_run.stdout),
        "<builtin:toLowerCase>\n<builtin:toLowerCase>\n<builtin:toLowerCase>\n<builtin:toLowerCase>\n<builtin:toLowerCase>\n<builtin:toLowerCase>\n<builtin:toLowerCase>\n[<builtin:toLowerCase>]\n#(<builtin:toLowerCase>)\n%[case: <builtin:toLowerCase>]\n%[<builtin:toLowerCase>: case]\n%(<builtin:toLowerCase>)\nfns=[<builtin:toLowerCase>]; rec=#(<builtin:toLowerCase>); map=%[case: <builtin:toLowerCase>]; mapKey=%[<builtin:toLowerCase>: case]; set=%(<builtin:toLowerCase>)\nconcat=[<builtin:toLowerCase>]; rec=#(<builtin:toLowerCase>); map=%[case: <builtin:toLowerCase>]; mapKey=%[<builtin:toLowerCase>: case]; set=%(<builtin:toLowerCase>)\npicked+<builtin:toLowerCase>\npicked=<builtin:toLowerCase>\n<builtin:toLowerCase>\n<builtin:toLowerCase>\nabc\nabc\nabc\nabc\nabc\nabc\nabc\n"
    );
    assert!(lower_run.stderr.is_empty());

    assert!(
        upper_run.status.success(),
        "conditional builtin function value upper run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&upper_run.stdout),
        String::from_utf8_lossy(&upper_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&upper_run.stdout),
        "<builtin:toUpperCase>\n<builtin:toUpperCase>\n<builtin:toUpperCase>\n<builtin:toUpperCase>\n<builtin:toUpperCase>\n<builtin:toUpperCase>\n<builtin:toUpperCase>\n[<builtin:toUpperCase>]\n#(<builtin:toUpperCase>)\n%[case: <builtin:toUpperCase>]\n%[<builtin:toUpperCase>: case]\n%(<builtin:toUpperCase>)\nfns=[<builtin:toUpperCase>]; rec=#(<builtin:toUpperCase>); map=%[case: <builtin:toUpperCase>]; mapKey=%[<builtin:toUpperCase>: case]; set=%(<builtin:toUpperCase>)\nconcat=[<builtin:toUpperCase>]; rec=#(<builtin:toUpperCase>); map=%[case: <builtin:toUpperCase>]; mapKey=%[<builtin:toUpperCase>: case]; set=%(<builtin:toUpperCase>)\npicked+<builtin:toUpperCase>\npicked=<builtin:toUpperCase>\n<builtin:toUpperCase>\n<builtin:toUpperCase>\nABC\nABC\nABC\nABC\nABC\nABC\nABC\n"
    );
    assert!(upper_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_thread_builtin_function_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-thread-function-value-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-thread-function-value-{unique}"));
    fs::write(
        &source_path,
        "val fs = [thread]\n({ println(\"pick thread\"); thread })(() => println(\"inline value\"))\nhead(fs)(() => println(\"list value\"))\nprintln(\"main\")\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "pick thread\nmain\ninline value\nlist value\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_side_effect_builtin_function_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-side-effect-function-value-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-side-effect-function-value-{unique}"
    ));
    fs::write(
        &source_path,
        "val printers = [println]\nval word = head(args())\nval parts = ({ println(\"pick split\"); split })(word, \",\")\n({ println(\"pick println\"); println })(\"inline print\")\nhead(printers)(\"list print\")\n({ println(\"pick sleep\"); sleep })(0)\n({ println(\"pick assert\"); assert })(true)\nprintln(({ println(\"pick upper\"); toUpperCase })(word))\nprintln(({ println(\"pick length\"); length })(word))\nprintln(({ println(\"pick join\"); join })(parts, \"|\"))\nprintln(({ println(\"pick contains\"); contains })(parts)(\"bc\"))\nprintln(\"done\")\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .arg("ab,bc")
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "pick split\npick println\ninline print\nlist print\npick sleep\npick assert\npick upper\nAB,BC\npick length\n5\npick join\nab|bc\npick contains\ntrue\ndone\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_curried_builtin_function_values_with_effectful_callees() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-curried-function-value-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-curried-function-value-{unique}"));
    fs::write(
        &source_path,
        "import Set.{contains}\n({ println(\"pick assertResult\"); assertResult })([1])([1])\nval xs = ({ println(\"pick cons\"); cons })(0)([1])\nval ys = ({ println(\"pick map\"); map })([\"a\", \"bb\"])((x) => length(x))\nval total = ({ println(\"pick foldLeft\"); foldLeft })([1, 2])(0)((acc, x) => acc + x)\nval hasRed = ({ println(\"pick contains\"); contains })(%(\"red\", \"blue\"))(\"red\")\nprintln(xs)\nprintln(ys)\nprintln(total)\nprintln(hasRed)\nassertResult([0, 1])(xs)\nassertResult([1, 2])(ys)\nassertResult(3)(total)\nassert(hasRed)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "pick assertResult\npick cons\npick map\npick foldLeft\npick contains\n[0, 1]\n[1, 2]\n3\ntrue\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_collection_builtin_function_values_with_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-collection-function-value-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-collection-function-value-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval n = ({ println(\"pick size\"); size })({ hits += 1; [1, 2] })\nval first = ({ println(\"pick head\"); head })({ hits += 1; [9, 10] })\nval rest = ({ println(\"pick tail\"); tail })({ hits += 1; [9, 10] })\nval empty = ({ println(\"pick empty\"); isEmpty })({ hits += 1; [] })\nval mapSize = ({ println(\"pick map size\"); Map#size })({ hits += 1; %[\"a\": 1] })\nval hasKey = ({ println(\"pick map key\"); Map#containsKey })({ hits += 1; %[\"a\": 1] }, { hits += 1; \"a\" })\nval hasValue = ({ println(\"pick map value\"); Map#containsValue })({ hits += 1; %[\"a\": 1] }, { hits += 1; 1 })\nval got = ({ println(\"pick map get\"); Map#get })({ hits += 1; %[\"a\": 1] }, { hits += 1; \"a\" })\nval setHas = ({ println(\"pick set contains\"); Set#contains })({ hits += 1; %(\"x\", \"y\") }, { hits += 1; \"x\" })\nprintln(hits)\nprintln(n)\nprintln(first)\nprintln(rest)\nprintln(empty)\nprintln(mapSize)\nprintln(hasKey)\nprintln(hasValue)\nprintln(got)\nprintln(setHas)\nassertResult(13)(hits)\nassertResult(2)(n)\nassertResult(9)(first)\nassertResult([10])(rest)\nassert(empty)\nassertResult(1)(mapSize)\nassert(hasKey)\nassert(hasValue)\nassertResult(1)(got)\nassert(setHas)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "pick size\npick head\npick tail\npick empty\npick map size\npick map key\npick map value\npick map get\npick set contains\n13\n2\n9\n[10]\ntrue\n1\ntrue\ntrue\n1\ntrue\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_stopwatch_builtin_function_value() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-stopwatch-function-value-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-stopwatch-function-value-{unique}"));
    fs::write(
        &source_path,
        "val elapsed = ({ println(\"pick stopwatch\"); stopwatch })(() => 1)\nassert(elapsed >= 0)\nprintln(\"ok\")\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "pick stopwatch\nok\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_println_error_builtin_function_value() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-println-error-value-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-println-error-value-{unique}"));
    fs::write(
        &source_path,
        "({ println(\"pick error\"); printlnError })(\"err value\")\nprintln(\"out value\")\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "pick error\nout value\n"
    );
    assert_eq!(String::from_utf8_lossy(&run.stderr), "err value\n");
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_builtin_functions_sample() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test-programs/builtin_functions.kl");
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-builtin-functions-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&output_path);

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_ignores_unreferenced_function_bodies() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test-programs/cleanup-expression.kl");
    let output_path = std::env::temp_dir().join(format!("klassic-native-unused-fn-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_record_function_calls() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test-programs/distance.kl");
    let output_path = std::env::temp_dir().join(format!("klassic-native-distance-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_inline_lambda_mutable_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test-programs/function-params-evaluation-count.kl");
    let output_path = std::env::temp_dir().join(format!("klassic-native-eval-count-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_typeclass_methods() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test-programs/typeclass-example.kl");
    let output_path = std::env::temp_dir().join(format!("klassic-native-typeclass-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "Int: 42\nString: Hello\nList[3 elements]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_advanced_typeclass_dictionary_examples() {
    let cases = [
        (
            "examples/typeclass-final-example.kl",
            "Int(42)\nString(\"Hello\")\n[Int(1), Int(2), Int(3)]\nInt(5) is equal to Int(5)\nString(\"foo\") is not equal to String(\"bar\")\nInt(42)\nString(\"Hello\")\n[Int(1), Int(2), Int(3)]\nInt(5) is equal to Int(5)\nString(\"foo\") is not equal to String(\"bar\")\n",
        ),
        (
            "test-programs/future-features/typeclass-polymorphic.kl",
            "=== Testing polymorphic display ===\nDisplaying: Int(42)\nDisplaying: \"Hello, Klassic!\"\nDisplaying: true\n\n=== Testing showIfEqual ===\nThey are equal: Int(10)\nInt(10) != Int(20)\nThey are equal: \"foo\"\n\"foo\" != \"bar\"\n\n=== Testing showList ===\nOriginal: [Int(1),Int(2),Int(3),Int(4),Int(5)]\nAs strings: Int(1), Int(2), Int(3), Int(4), Int(5)\n\n=== Testing with custom type ===\nDisplaying: Person(name=\"Alice\", age=Int(30))\nDisplaying: Person(name=\"Bob\", age=Int(25))\nThey are equal: Person(name=\"Alice\", age=Int(30))\nPerson(name=\"Alice\", age=Int(30)) != Person(name=\"Bob\", age=Int(25))\n",
        ),
    ];

    for (index, (program, expected_stdout)) in cases.iter().enumerate() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(program);
        let output_path =
            std::env::temp_dir().join(format!("klassic-native-typeclass-dict-{unique}-{index}"));

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");

        assert!(
            build.status.success(),
            "{program} build failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );
        assert!(build.stdout.is_empty());
        assert!(build.stderr.is_empty());

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&output_path);

        assert!(
            run.status.success(),
            "{program} run failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&run.stdout),
            String::from_utf8_lossy(&run.stderr)
        );
        assert_eq!(String::from_utf8_lossy(&run.stdout), *expected_stdout);
        assert!(run.stderr.is_empty());
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_higher_kinded_list_calls() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test-programs/hkt-no-constraints.kl");
    let output_path = std::env::temp_dir().join(format!("klassic-native-hkt-list-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "Original: [1, 2, 3, 4, 5]\nDoubled: [2, 4, 6, 8, 10]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_list_monad_calls() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test-programs/working-monad-example.kl");
    let output_path = std::env::temp_dir().join(format!("klassic-native-list-monad-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "[2]\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_file_output_sample() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let work_dir = std::env::temp_dir().join(format!("klassic-native-file-output-{unique}"));
    fs::create_dir(&work_dir).expect("temp work dir should be created");
    let source_path =
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test-programs/file-output.kl");
    let output_path = work_dir.join("file-output");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .current_dir(&work_dir)
        .output()
        .expect("generated executable should run");

    let leftover_file = work_dir.join("test-output.txt").exists();
    let leftover_lines = work_dir.join("test-lines.txt").exists();
    let _ = fs::remove_dir_all(&work_dir);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "File written successfully\nContent appended successfully\nFile exists: true\nMultiple lines written\nFile content: Hello, Klassic!\nAppended line\nLines read: [Line 1, Line 2, Line 3]\nTest files cleaned up\n"
    );
    assert!(run.stderr.is_empty());
    assert!(!leftover_file);
    assert!(!leftover_lines);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_file_helper_argument_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let work_dir = std::env::temp_dir().join(format!("klassic-native-file-effect-{unique}"));
    fs::create_dir(&work_dir).expect("temp work dir should be created");
    let source_path = work_dir.join("file-effect.kl");
    let output_path = work_dir.join("file-effect");
    fs::write(
        &source_path,
        "mutable hits = 0\nFileOutput#write({ hits += 1; \"effect.txt\" }, { hits += 1; \"hello\" })\nFileOutput#append({ hits += 1; \"effect.txt\" }, { hits += 1; \"!\" })\nval exists = FileOutput#exists({ hits += 1; \"effect.txt\" })\nval content = FileInput#all({ hits += 1; \"effect.txt\" })\nFileOutput#writeLines({ hits += 1; \"lines.txt\" }, { hits += 1; [\"a\", \"b\"] })\nval lines = FileInput#lines({ hits += 1; \"lines.txt\" })\nFileOutput#delete({ hits += 1; \"effect.txt\" })\nFileOutput#delete({ hits += 1; \"lines.txt\" })\nprintln(hits)\nassertResult(11)(hits)\nassert(exists)\nassertResult(\"hello!\")(content)\nassertResult([\"a\", \"b\"])(lines)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .current_dir(&work_dir)
        .output()
        .expect("generated executable should run");

    let leftover_effect = work_dir.join("effect.txt").exists();
    let leftover_lines = work_dir.join("lines.txt").exists();
    let _ = fs::remove_dir_all(&work_dir);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "11\n");
    assert!(run.stderr.is_empty());
    assert!(!leftover_effect);
    assert!(!leftover_lines);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_file_builtin_function_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let work_dir = std::env::temp_dir().join(format!("klassic-native-file-values-{unique}"));
    fs::create_dir(&work_dir).expect("temp work dir should be created");
    let source_path = work_dir.join("file-values.kl");
    let output_path = work_dir.join("file-values");
    fs::write(
        &source_path,
        "val path = \"value.txt\"\nval linesPath = \"lines.txt\"\n({ println(\"pick write\"); FileOutput#write })(path, \"hello\")\nval appenders = [FileOutput#append]\nhead(appenders)(path, \"!\")\nprintln(({ println(\"pick all\"); FileInput#all })(path))\nprintln(({ println(\"pick open\"); FileInput#open })(path, (stream) => FileInput#readAll(stream)))\n({ println(\"pick writeLines\"); FileOutput#writeLines })(linesPath, [\"a\", \"b\"])\nprintln(({ println(\"pick lines\"); FileInput#lines })(linesPath))\n({ println(\"pick delete\"); FileOutput#delete })(path)\n({ println(\"pick delete lines\"); FileOutput#delete })(linesPath)\nprintln(({ println(\"pick exists\"); FileOutput#exists })(path))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "file builtin value build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .current_dir(&work_dir)
        .output()
        .expect("generated executable should run");

    let leftover_value = work_dir.join("value.txt").exists();
    let leftover_lines = work_dir.join("lines.txt").exists();
    let _ = fs::remove_dir_all(&work_dir);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "pick write\npick all\nhello!\npick open\nhello!\npick writeLines\npick lines\n[a, b]\npick delete\npick delete lines\npick exists\nfalse\n"
    );
    assert!(run.stderr.is_empty());
    assert!(!leftover_value);
    assert!(!leftover_lines);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_file_input_sample() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test-programs/file-input.kl");
    let output_path = std::env::temp_dir().join(format!("klassic-native-file-input-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_file_input_printing() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-input-{unique}.txt"));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-input-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-input-{unique}"));
    fs::write(
        &source_path,
        format!(
            "val path = \"{}\"\nval all = FileInput#all\nmutable hits = 0\nprintln(\"content=\" + all({{ hits += 1; path }}))\nprintln(hits)\n",
            input_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime file input build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path, "runtime file").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime file input run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "content=runtime file\n1\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_file_input_open_callback_body() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let input_path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-open-callback-holder-{unique}.txt"
    ));
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-open-callback-{unique}.txt"));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-open-callback-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-open-callback-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"val path = FileInput#all("{}")
mutable hits = 0
val openedPath = FileInput#open(path, (stream) => {{
  hits += 1
  stream
}})
val lengthViaOpen = FileInput#open(path, (stream) => {{
  hits += 1
  length(FileInput#readAll(stream))
}})
val linesViaOpen = FileInput#open(path, (stream) => {{
  hits += 1
  FileInput#readLines(stream)
}})
val cleanupText = FileInput#open(path, (stream) => {{
  FileInput#readAll(stream) cleanup {{ hits += 1 }}
}})
println(openedPath == path)
println(lengthViaOpen)
println(linesViaOpen)
println(join(linesViaOpen, "|"))
println(cleanupText)
println(hits)
assert(openedPath == path)
assertResult(11)(lengthViaOpen)
assertResult(["hello", "world"])(linesViaOpen)
assertResult("hello\nworld")(cleanupText)
assertResult(4)(hits)
"#,
            input_path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime FileInput#open callback build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path_holder, input_path.to_string_lossy().as_bytes())
        .expect("input path holder should write after native build");
    fs::write(&input_path, "hello\nworld").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&input_path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime FileInput#open callback run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "true\n11\n[hello, world]\nhello|world\nhello\nworld\n4\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_file_input_open_runtime_callback_body() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-static-open-runtime-callback-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-static-open-runtime-callback-{unique}"
    ));
    fs::write(
        &source_path,
        r#"val opened = FileInput#open("static-name", (stream) => {
  println("stream=" + stream)
  args()
})
println(opened)
println(join(opened, "|"))
assertResult(["alpha", "beta"])(opened)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "static FileInput#open runtime callback build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .arg("alpha")
        .arg("beta")
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "static FileInput#open runtime callback run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "stream=static-name\n[alpha, beta]\nalpha|beta\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_file_input_open_callable_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let input_path_holder =
        std::env::temp_dir().join(format!("klassic-native-open-callable-holder-{unique}.txt"));
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-open-callable-{unique}.txt"));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-open-callable-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-open-callable-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"val runtimePath = FileInput#all("{}")
val readAll = FileInput#readAll
val readLines = FileInput#readLines
println(FileInput#open("Cargo.toml", readAll).contains("klassic"))
println(FileInput#open(runtimePath, readAll))
println(join(FileInput#open(runtimePath, readLines), "|"))
println(FileInput#open(runtimePath, {{
  println("pick callback")
  FileInput#readAll
}}))
assert(FileInput#open("Cargo.toml", FileInput#readAll).contains("klassic"))
assertResult("dynamic callback")(FileInput#open(runtimePath, readAll))
assertResult(["dynamic callback"])(FileInput#open(runtimePath, FileInput#readLines))
"#,
            input_path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "FileInput#open callable value build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path_holder, input_path.to_string_lossy().as_bytes())
        .expect("input path holder should write after native build");
    fs::write(&input_path, "dynamic callback").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&input_path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "FileInput#open callable value run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "true\ndynamic callback\ndynamic callback\npick callback\ndynamic callback\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_file_input_binding() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-input-binding-{unique}.txt"));
    let empty_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-input-binding-empty-{unique}.txt"
    ));
    let unicode_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-input-binding-unicode-{unique}.txt"
    ));
    let needle_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-input-binding-needle-{unique}.txt"
    ));
    let padded_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-input-binding-padded-{unique}.txt"
    ));
    let mixed_case_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-input-binding-mixed-case-{unique}.txt"
    ));
    let digit_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-input-binding-digit-{unique}.txt"
    ));
    let digits_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-input-binding-digits-{unique}.txt"
    ));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-input-binding-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-input-binding-{unique}"));
    fs::write(
        &source_path,
        format!(
            "val path = \"{}\"\nval emptyPath = \"{}\"\nval unicodePath = \"{}\"\nval needlePath = \"{}\"\nval paddedPath = \"{}\"\nval mixedCasePath = \"{}\"\nval digitPath = \"{}\"\nval digitsPath = \"{}\"\nmutable hits = 0\nval text = FileInput#all({{ hits += 1; path }})\nval empty = FileInput#all({{ hits += 1; emptyPath }})\nval unicode = FileInput#all({{ hits += 1; unicodePath }})\nval needle = FileInput#all({{ hits += 1; needlePath }})\nval padded = FileInput#all({{ hits += 1; paddedPath }})\nval mixedCase = FileInput#all({{ hits += 1; mixedCasePath }})\nval digit = FileInput#all({{ hits += 1; digitPath }})\nval digits = FileInput#all({{ hits += 1; digitsPath }})\nval combined = \"content=\" + text\nval shouted = text + \"!\"\nval interpolated = \"message=#{{text}}!\"\nprintln(combined)\nprintln(shouted)\nprintln(interpolated)\nprintln(combined == \"content=bound runtime file\")\nprintln(text == \"bound runtime file\")\nprintln(text != \"other\")\nprintln(isEmptyString(text))\nprintln(isEmptyString(empty))\nprintln(empty.isEmpty())\nprintln(length(text))\nprintln(length(unicode))\nprintln(substring(text, 6, 13))\nprintln(text.substring(0, 5))\nprintln(at(text, 6))\nprintln(text.at(7))\nprintln(substring(unicode, 1, 2))\nprintln(at(unicode, 1))\nprintln(substring(unicode, length(digit), length(unicode)))\nprintln(at(unicode, length(digit)))\nprintln(startsWith(text, \"bound\"))\nprintln(endsWith(text, \"file\"))\nprintln(text.startsWith(\"bound\"))\nprintln(text.endsWith(\"file\"))\nprintln(text.contains(needle))\nprintln(indexOf(text, needle))\nprintln(text.indexOf(needle))\nprintln(lastIndexOf(text, \"i\"))\nprintln(text.lastIndexOf(\"i\"))\nprintln(indexOf(text, \"missing\"))\nprintln(repeat(digit, length(unicode)))\nprintln(digit.repeat(length(unicode)))\nprintln(hits)\nassertResult(\"bound runtime file\")(text)\nassertResult(\"bound runtime file\")(text.toString())\nassertResult(text)(\"bound runtime file\")\nassertResult(combined)(\"content=bound runtime file\")\nassertResult(\"message=bound runtime file!\")(interpolated)\nassertResult(\"runtime\")(substring(text, 6, 13))\nassertResult(\"bound\")(text.substring(0, 5))\nassertResult(\"r\")(at(text, 6))\nassertResult(\"u\")(text.at(7))\nassertResult(\"é\")(substring(unicode, 1, 2))\nassertResult(\"é\")(at(unicode, 1))\nassertResult(\"é\")(substring(unicode, length(digit), length(unicode)))\nassertResult(\"é\")(at(unicode, length(digit)))\nassertResult(\"spaced runtime\")(trim(padded))\nassertResult(\"spaced runtime  \")(trimLeft(padded))\nassertResult(\"  spaced runtime\")(trimRight(padded))\nassertResult(\"bound runtime filebound runtime file\")(repeat(text, 2))\nassertResult(\"bound runtime filebound runtime file\")(text.repeat(2))\nassertResult(\"\")(repeat(empty, 3))\nassertResult(\"77\")(repeat(digit, length(unicode)))\nassertResult(\"77\")(digit.repeat(length(unicode)))\nassertResult(\"abc-é\")(toLowerCase(mixedCase))\nassertResult(\"ABC-é\")(toUpperCase(mixedCase))\nassertResult(\"bound native file\")(replace(text, \"runtime\", \"native\"))\nassertResult(\"bound native file\")(text.replace(\"runtime\", \"native\"))\nassertResult(\"bound 7 file\")(replace(text, needle, digit))\nassertResult(\"bound 7 file\")(text.replace(needle, digit))\nassertResult(\"bound runtime file\")(replace(text, \"missing\", \"native\"))\nassertResult(\"xbound runtime file\")(replace(text, \"\", \"x\"))\nassertResult(\"bound runtXme fXle\")(replaceAll(text, \"i\", \"X\"))\nassertResult(\"bound runtXme fXle\")(text.replaceAll(\"i\", \"X\"))\nassertResult(\"?????\")(replaceAll(digits, \"[0-9]\", \"?\"))\nassertResult(\"-h-é-\")(replaceAll(unicode, \"\", \"-\"))\nassertResult(\"elif emitnur dnuob\")(reverse(text))\nassertResult(\"éh\")(reverse(unicode))\nassert(matches(digits, \"[0-9]+\"))\nassert(matches(digit, \"[0-9]\"))\nassert(matches(text, \".*\"))\nassert(matches(text, \"bound runtime file\"))\nassert(matches(digit, digit))\nassert(matches(text, replace(digit, digit, \".*\")))\nassert(matches(digits, replace(digit, digit, \"[0-9]+\")))\nassert(matches(digit, replace(digit, digit, \"[0-9]\")))\nassert(!matches(text, \"bound\"))\nassert(!matches(text, \"[0-9]+\"))\nassert(!matches(text, replace(digit, digit, \"[0-9]+\")))\nassert(!matches(digits, \"[0-9]\"))\nassert(startsWith(text, \"bound\"))\nassert(endsWith(text, \"file\"))\nassert(text.contains(needle))\nassertResult(6)(indexOf(text, needle))\nassertResult(15)(lastIndexOf(text, \"i\"))\n",
            input_path.display(),
            empty_path.display(),
            unicode_path.display(),
            needle_path.display(),
            padded_path.display(),
            mixed_case_path.display(),
            digit_path.display(),
            digits_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime file input binding build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path, "bound runtime file").expect("input should write after native build");
    fs::write(&empty_path, "").expect("empty input should write after native build");
    fs::write(&unicode_path, "hé").expect("unicode input should write after native build");
    fs::write(&needle_path, "runtime").expect("needle input should write after native build");
    fs::write(&padded_path, "  spaced runtime  ")
        .expect("padded input should write after native build");
    fs::write(&mixed_case_path, "AbC-é").expect("mixed case input should write after native build");
    fs::write(&digit_path, "7").expect("digit input should write after native build");
    fs::write(&digits_path, "12345").expect("digits input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&empty_path);
    let _ = fs::remove_file(&unicode_path);
    let _ = fs::remove_file(&needle_path);
    let _ = fs::remove_file(&padded_path);
    let _ = fs::remove_file(&mixed_case_path);
    let _ = fs::remove_file(&digit_path);
    let _ = fs::remove_file(&digits_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime file input binding run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "content=bound runtime file\nbound runtime file!\nmessage=bound runtime file!\ntrue\ntrue\ntrue\nfalse\ntrue\ntrue\n18\n2\nruntime\nbound\nr\nu\né\né\né\né\ntrue\ntrue\ntrue\ntrue\ntrue\n6\n6\n15\n15\n-1\n77\n77\n8\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_runtime_top_level_captures() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-runtime-input-{unique}.txt"
    ));
    let input_path_holder = std::env::temp_dir().join(format!(
        "klassic-native-recursive-runtime-input-holder-{unique}.txt"
    ));
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-runtime-capture-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-recursive-runtime-capture-{unique}"));
    fs::write(
        &source_path,
        format!(
            "val path = FileInput#all(\"{}\")\nval text = FileInput#all(path)\nval lines = FileInput#lines(path)\nval sizes = [length(text), lines.size()]\nval variedKey = if(length(text) == 8) \"short\" else \"long\"\nval varied = Map#get(%[\n  \"short\": [length(text)],\n  \"long\": [length(text), lines.size()]\n], variedKey)\nval bag = record {{ sizes: sizes, first: lines.head() }}\ndef textLengthAfter(n: Int): Int = if(n == 0) length(text) else textLengthAfter(n - 1)\ndef lineCountAfter(n: Int): Int = if(n == 0) lines.size() else lineCountAfter(n - 1)\ndef firstLineLengthAfter(n: Int): Int = if(n == 0) length(lines.head()) else firstLineLengthAfter(n - 1)\ndef sizeSumAfter(n: Int): Int = if(n == 0) sizes.foldLeft(0, (acc, value) => acc + value) else sizeSumAfter(n - 1)\ndef variedSizeAfter(n: Int): Int = if(n == 0) varied.size() else variedSizeAfter(n - 1)\ndef variedSumAfter(n: Int): Int = if(n == 0) varied.foldLeft(0, (acc, value) => acc + value) else variedSumAfter(n - 1)\ndef bagFirstAfter(n: Int): String = if(n == 0) bag.first else bagFirstAfter(n - 1)\ndef bagHeadAfter(n: Int): Int = if(n == 0) head(bag.sizes) else bagHeadAfter(n - 1)\nprintln(textLengthAfter(2))\nprintln(lineCountAfter(3))\nprintln(firstLineLengthAfter(1))\nprintln(sizeSumAfter(1))\nprintln(variedSizeAfter(1))\nprintln(variedSumAfter(1))\nprintln(bagFirstAfter(1))\nprintln(bagHeadAfter(1))\nassertResult(8)(textLengthAfter(2))\nassertResult(3)(lineCountAfter(3))\nassertResult(3)(firstLineLengthAfter(1))\nassertResult(11)(sizeSumAfter(1))\nassertResult(1)(variedSizeAfter(1))\nassertResult(8)(variedSumAfter(1))\nassertResult(\"abc\")(bagFirstAfter(1))\nassertResult(8)(bagHeadAfter(1))\n",
            input_path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "recursive runtime capture build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "abc\nxy\nz").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&input_path_holder);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "recursive runtime capture run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "8\n3\n3\n11\n1\n8\nabc\n8\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_recursive_callable_dispatch_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-dispatch-capture-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-recursive-dispatch-capture-{unique}"
    ));
    fs::write(
        &source_path,
        "def plusOne(x: Int): Int = x + 1\n\
def plusTwo(x: Int): Int = x + 2\n\
val key = head(args())\n\
val selector = head(tail(args()))\n\
val f = Map#get(%[\"one\": plusOne, \"two\": plusTwo], key)\n\
val byLength = Map#get(%[3: plusOne, 5: plusTwo], length(selector))\n\
val byFlag = Map#get(%[false: plusOne, true: plusTwo], selector == \"wide!\")\n\
def applyMany(n: Int, x: Int): Int = if(n < 1) x else applyMany(n - 1, f(x))\n\
def applyLength(n: Int, x: Int): Int = if(n < 1) x else applyLength(n - 1, byLength(x))\n\
def applyFlag(n: Int, x: Int): Int = if(n < 1) x else applyFlag(n - 1, byFlag(x))\n\
def localPick(k: Int, n: Int): Int = if(n < 1) {\n\
  val picked = Map#get(%[1: plusOne, 2: plusTwo], k)\n\
  picked(0)\n\
} else {\n\
  val picked = Map#get(%[1: plusOne, 2: plusTwo], k)\n\
  localPick(2, n - 1) + picked(0)\n\
}\n\
val actual = applyMany(3, 0)\n\
val lengthActual = applyLength(2, 0)\n\
val flagActual = applyFlag(2, 0)\n\
val localActual = localPick(1, 1)\n\
println(actual)\n\
println(lengthActual)\n\
println(flagActual)\n\
println(localActual)\n\
if(key == \"one\") {\n\
  assertResult(3)(actual)\n\
} else {\n\
  assertResult(6)(actual)\n\
}\n\
if(length(selector) == 3) {\n\
  assertResult(2)(lengthActual)\n\
} else {\n\
  assertResult(4)(lengthActual)\n\
}\n\
if(selector == \"wide!\") {\n\
  assertResult(4)(flagActual)\n\
} else {\n\
  assertResult(2)(flagActual)\n\
}\n\
assertResult(3)(localActual)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "recursive callable dispatch capture build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let one_run = Command::new(&output_path)
        .arg("one")
        .arg("cat")
        .output()
        .expect("generated executable should run with one key");
    let two_run = Command::new(&output_path)
        .arg("two")
        .arg("wide!")
        .output()
        .expect("generated executable should run with two key");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        one_run.status.success(),
        "recursive callable dispatch capture one run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&one_run.stdout),
        String::from_utf8_lossy(&one_run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&one_run.stdout), "3\n2\n2\n3\n");
    assert!(one_run.stderr.is_empty());

    assert!(
        two_run.status.success(),
        "recursive callable dispatch capture two run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&two_run.stdout),
        String::from_utf8_lossy(&two_run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&two_run.stdout), "6\n4\n4\n3\n");
    assert!(two_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_file_output_content() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-output-input-{unique}.txt"));
    let output_content_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-output-content-{unique}.txt"
    ));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-output-content-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-output-content-{unique}"));
    fs::write(
        &source_path,
        format!(
            "val inputPath = \"{}\"\nval outputPath = \"{}\"\nval text = FileInput#all(inputPath)\nFileOutput#write(outputPath, text)\nFileOutput#append(outputPath, replaceAll(text, \"i\", \"!\"))\nprintln(FileInput#all(outputPath))\nprintln(FileOutput#exists(outputPath))\nprintln(Dir#isFile(outputPath))\n",
            input_path.display(),
            output_content_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime file output content build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path, "runtime").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");
    let output_content =
        fs::read_to_string(&output_content_path).expect("output content should be readable");

    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_content_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime file output content run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "runtimerunt!me\ntrue\ntrue\n"
    );
    assert!(run.stderr.is_empty());
    assert_eq!(output_content, "runtimerunt!me");
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_file_paths() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let input_path_holder =
        std::env::temp_dir().join(format!("klassic-native-runtime-path-holder-{unique}.txt"));
    let output_path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-output-path-holder-{unique}.txt"
    ));
    let target_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-path-target-{unique}.txt"));
    let output_content_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-path-output-content-{unique}.txt"
    ));
    let dynamic_dir_path =
        std::path::PathBuf::from(format!("{}.dir", output_content_path.display()));
    let dynamic_copy_path =
        std::path::PathBuf::from(format!("{}.copy", output_content_path.display()));
    let dynamic_moved_path =
        std::path::PathBuf::from(format!("{}.moved", output_content_path.display()));
    let dynamic_delete_path =
        std::path::PathBuf::from(format!("{}.delete", output_content_path.display()));
    let dynamic_lines_path =
        std::path::PathBuf::from(format!("{}.lines", output_content_path.display()));
    let dynamic_rewritten_lines_path =
        std::path::PathBuf::from(format!("{}.rewritten-lines", output_content_path.display()));
    let dynamic_parent_dir_path =
        std::path::PathBuf::from(format!("{}.nested", output_content_path.display()));
    let dynamic_nested_dir_path = dynamic_parent_dir_path.join("inner");
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-paths-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-paths-{unique}"));
    fs::write(
        &source_path,
        format!(
            "val pathHolder = \"{}\"\nval outputPathHolder = \"{}\"\nval targetPath = FileInput#all(pathHolder)\nval outputPath = FileInput#all(outputPathHolder)\nval dirPath = outputPath + \".dir\"\nval parentPath = outputPath + \".nested\"\nval nestedPath = parentPath + \"/inner\"\nval copyPath = outputPath + \".copy\"\nval movedPath = outputPath + \".moved\"\nval deletePath = outputPath + \".delete\"\nval linesPath = outputPath + \".lines\"\nval rewrittenLinesPath = outputPath + \".rewritten-lines\"\nprintln(FileInput#all(targetPath))\nprintln(FileInput#open(targetPath, (stream) => FileInput#readAll(stream)))\nprintln(FileOutput#exists(targetPath))\nprintln(Dir#isFile(targetPath))\nprintln(Dir#isDirectory(targetPath))\nFileOutput#write(outputPath, \"runtime path \")\nFileOutput#append(outputPath, FileInput#all(targetPath))\nprintln(FileInput#all(outputPath))\nval outputContent = FileInput#all(outputPath)\nval summary = \"len=#{{length(outputContent)}}, empty=#{{isEmptyString(outputContent)}}, exists=#{{FileOutput#exists(outputPath)}}\"\nval plusSummary = \"plus len=\" + length(outputContent) + \", empty=\" + isEmptyString(outputContent) + \", exists=\" + FileOutput#exists(outputPath)\nprintln(summary)\nprintln(plusSummary)\nFileOutput#writeLines(linesPath, [\"line-a\", \"line-b\"])\nprintln(FileInput#all(linesPath))\nprintln(FileInput#lines(linesPath))\nprintln(FileInput#open(linesPath, (stream) => FileInput#readLines(stream)))\nval runtimeLines = FileInput#lines(linesPath)\nval openLines = FileInput#open(linesPath, (stream) => FileInput#readLines(stream))\nprintln(runtimeLines)\nprintln(openLines)\nprintln(size(runtimeLines))\nprintln(isEmpty(runtimeLines))\nprintln(size(openLines))\nprintln(head(runtimeLines))\nprintln(head(openLines))\nprintln(tail(runtimeLines))\nprintln(size(tail(runtimeLines)))\nprintln(isEmpty(tail(tail(runtimeLines))))\nprintln(join(runtimeLines, \"|\"))\nprintln(tail(runtimeLines).join(\"/\"))\nprintln(runtimeLines == [\"line-a\", \"line-b\"])\nprintln([\"line-a\", \"line-b\"] == runtimeLines)\nprintln(runtimeLines != [\"line-a\"])\nprintln(runtimeLines == openLines)\nassertResult([\"line-a\", \"line-b\"])(runtimeLines)\nassertResult(runtimeLines)(openLines)\nassert(runtimeLines == [\"line-a\", \"line-b\"])\nassert([\"line-a\"] != runtimeLines)\nassert(runtimeLines == openLines)\nmutable lineCount = 0\nforeach(line in runtimeLines) {{\n  println(\"line \" + line)\n  lineCount += 1\n}}\nprintln(lineCount)\nassertResult(2)(lineCount)\nFileOutput#writeLines(rewrittenLinesPath, runtimeLines)\nprintln(FileInput#all(rewrittenLinesPath))\nassertResult(runtimeLines)(FileInput#lines(rewrittenLinesPath))\nprintln(FileOutput#exists(outputPath))\nprintln(Dir#isFile(outputPath))\nDir#mkdir(dirPath)\nprintln(Dir#isDirectory(dirPath))\nDir#delete(dirPath)\nprintln(Dir#exists(dirPath))\nDir#mkdirs(nestedPath)\nprintln(Dir#isDirectory(nestedPath))\nDir#delete(nestedPath)\nDir#delete(parentPath)\nprintln(Dir#exists(parentPath))\nDir#copy(outputPath, copyPath)\nprintln(FileInput#all(copyPath))\nDir#move(copyPath, movedPath)\nprintln(FileOutput#exists(copyPath))\nprintln(FileInput#all(movedPath))\nFileOutput#write(deletePath, \"bye\")\nprintln(FileOutput#exists(deletePath))\nFileOutput#delete(deletePath)\nprintln(FileOutput#exists(deletePath))\nFileOutput#delete(linesPath)\nFileOutput#delete(rewrittenLinesPath)\nFileOutput#delete(movedPath)\nprintln(FileOutput#exists(outputPath))\n",
            input_path_holder.display(),
            output_path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime file path build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path_holder, target_path.to_string_lossy().as_bytes())
        .expect("input path holder should write after native build");
    fs::write(
        &output_path_holder,
        output_content_path.to_string_lossy().as_bytes(),
    )
    .expect("output path holder should write after native build");
    fs::write(&target_path, "dynamic target").expect("target should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");
    let output_content =
        fs::read_to_string(&output_content_path).expect("output content should be readable");

    let _ = fs::remove_file(&input_path_holder);
    let _ = fs::remove_file(&output_path_holder);
    let _ = fs::remove_file(&target_path);
    let _ = fs::remove_file(&output_content_path);
    let _ = fs::remove_file(&dynamic_copy_path);
    let _ = fs::remove_file(&dynamic_moved_path);
    let _ = fs::remove_file(&dynamic_delete_path);
    let _ = fs::remove_file(&dynamic_lines_path);
    let _ = fs::remove_file(&dynamic_rewritten_lines_path);
    let _ = fs::remove_dir(&dynamic_dir_path);
    let _ = fs::remove_dir(&dynamic_nested_dir_path);
    let _ = fs::remove_dir(&dynamic_parent_dir_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime file path run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "dynamic target\ndynamic target\ntrue\ntrue\nfalse\nruntime path dynamic target\nlen=27, empty=false, exists=true\nplus len=27, empty=false, exists=true\nline-a\nline-b\n[line-a, line-b]\n[line-a, line-b]\n[line-a, line-b]\n[line-a, line-b]\n2\nfalse\n2\nline-a\nline-a\n[line-b]\n1\ntrue\nline-a|line-b\nline-b\ntrue\ntrue\ntrue\ntrue\nline line-a\nline line-b\n2\nline-a\nline-b\ntrue\ntrue\ntrue\nfalse\ntrue\nfalse\nruntime path dynamic target\nfalse\nruntime path dynamic target\ntrue\nfalse\ntrue\n"
    );
    assert!(run.stderr.is_empty());
    assert_eq!(output_content, "runtime path dynamic target");
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_dir_list_paths() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let dir_path_holder =
        std::env::temp_dir().join(format!("klassic-native-dir-list-path-holder-{unique}.txt"));
    let dir_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-dir-list-dir-{unique}"));
    let first_entry_path = dir_path.join("a.txt");
    let second_entry_path = dir_path.join("b.txt");
    let third_entry_path = dir_path.join("c.txt");
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-dir-list-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-dir-list-{unique}"));
    let source = format!(
        r#"val dirPathHolder = "{}"
val dirPath = FileInput#all(dirPathHolder)
Dir#mkdir(dirPath)
val secondEntryPath = dirPath + "/b.txt"
val firstEntryPath = dirPath + "/a.txt"
val thirdEntryPath = dirPath + "/c.txt"
FileOutput#write(secondEntryPath, "b")
FileOutput#write(firstEntryPath, "a")
FileOutput#write(thirdEntryPath, "c")
val entries = Dir#list(dirPath)
val fullEntries = Dir#listFull(dirPath)
println(entries)
println(size(entries))
println(head(entries))
println(join(entries, "|"))
println(fullEntries)
println(size(fullEntries))
println(head(fullEntries))
foreach(entry in entries) {{
  println("entry=" + entry)
}}
mutable seen = 0
foreach(entry in fullEntries) {{
  if(entry.endsWith("/a.txt") || entry.endsWith("/b.txt") || entry.endsWith("/c.txt")) {{
    seen += 1
  }}
}}
println(seen)
assertResult(["a.txt", "b.txt", "c.txt"])(entries)
assertResult(3)(size(fullEntries))
assert(endsWith(head(fullEntries), "/a.txt"))
assertResult(3)(seen)
FileOutput#delete(firstEntryPath)
FileOutput#delete(secondEntryPath)
FileOutput#delete(thirdEntryPath)
Dir#delete(dirPath)
"#,
        dir_path_holder.display()
    );
    fs::write(&source_path, source).expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime dir list build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&dir_path_holder, dir_path.to_string_lossy().as_bytes())
        .expect("dir path holder should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&dir_path_holder);
    let _ = fs::remove_file(&first_entry_path);
    let _ = fs::remove_file(&second_entry_path);
    let _ = fs::remove_file(&third_entry_path);
    let _ = fs::remove_dir(&dir_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime dir list run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        format!(
            "[a.txt, b.txt, c.txt]\n3\na.txt\na.txt|b.txt|c.txt\n[{}, {}, {}]\n3\n{}\nentry=a.txt\nentry=b.txt\nentry=c.txt\n3\n",
            first_entry_path.display(),
            second_entry_path.display(),
            third_entry_path.display(),
            first_entry_path.display()
        )
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_dir_current() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let run_dir = std::env::temp_dir().join(format!("klassic-native-current-dir-{unique}"));
    let marker_path = run_dir.join("marker.txt");
    let source_path = std::env::temp_dir().join(format!("klassic-native-current-dir-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-current-dir-bin-{unique}"));
    fs::create_dir_all(&run_dir).expect("run dir should be created");
    fs::write(&marker_path, "marker").expect("marker should write");
    fs::write(
        &source_path,
        "val here = Dir#current()\nprintln(here)\nprintln(Dir#isDirectory(here))\nprintln(Dir#list(here))\nassert(Dir#isDirectory(here))\nassertResult([\"marker.txt\"])(Dir#list(here))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime current dir build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .current_dir(&run_dir)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&marker_path);
    let _ = fs::remove_dir(&run_dir);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime current dir run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        format!("{}\ntrue\n[marker.txt]\n", run_dir.display())
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_dir_home_and_temp() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let home_dir = std::env::temp_dir().join(format!("klassic-native-home-{unique}"));
    let temp_dir = std::env::temp_dir().join(format!("klassic-native-temp-{unique}"));
    let home_text = home_dir.display().to_string();
    let temp_text = temp_dir.display().to_string();
    let source_path = std::env::temp_dir().join(format!("klassic-native-home-temp-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-home-temp-{unique}"));
    fs::create_dir_all(&home_dir).expect("home dir should be created");
    fs::create_dir_all(&temp_dir).expect("temp dir should be created");
    fs::write(
        &source_path,
        format!(
            "val home = Dir#home()\nval temp = Dir#temp()\nprintln(home)\nprintln(temp)\nprintln(Dir#exists(home))\nprintln(Dir#isDirectory(temp))\nassertResult(\"{home_text}\")(home)\nassertResult(\"{temp_text}\")(temp)\nassert(Dir#exists(Dir#home()))\nassert(Dir#isDirectory(Dir#temp()))\n"
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime home/temp build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .env("HOME", &home_dir)
        .env("TMPDIR", &temp_dir)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_dir(&home_dir);
    let _ = fs::remove_dir(&temp_dir);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime home/temp run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        format!("{home_text}\n{temp_text}\ntrue\ntrue\n")
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_command_line_args() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-args-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-args-{unique}"));
    fs::write(
        &source_path,
        "def captured() = CommandLine#args()\nval getArgs = CommandLine#args\nval argsAlias = args\nval xs = getArgs()\nval ys = captured()\nval zs = args()\nval ws = argsAlias()\nprintln(xs)\nprintln(ys)\nprintln(zs)\nprintln(ws)\nprintln(size(xs))\nprintln(head(xs))\nprintln(join(xs, \"|\"))\nassertResult([\"alpha\", \"two words\", \"gamma\"])(xs)\nassertResult(xs)(ys)\nassertResult(xs)(zs)\nassertResult(xs)(ws)\nassertResult(3)(size(ys))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "command line args build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .args(["alpha", "two words", "gamma"])
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "command line args run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[alpha, two words, gamma]\n[alpha, two words, gamma]\n[alpha, two words, gamma]\n[alpha, two words, gamma]\n3\nalpha\nalpha|two words|gamma\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_process_exit() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-process-exit-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-process-exit-{unique}"));
    fs::write(
        &source_path,
        "val direct = Process#exit\nval quit = exit\nprintln(\"before exit\")\nquit({ println(\"code path\"); 7 })\nprintln(\"after exit\")\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "process exit build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert_eq!(run.status.code(), Some(7));
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "before exit\ncode path\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_standard_input_all() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-stdin-all-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-stdin-all-{unique}"));
    fs::write(
        &source_path,
        "val read = StandardInput#all\nval text = read()\nprintln(trimRight(text))\nprintln(length(text))\nassertResult(\"alpha\\nbeta\\n\")(text)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "standard input build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let mut child = Command::new(&output_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("generated executable should run");

    {
        let mut stdin = child.stdin.take().expect("stdin should be piped");
        stdin
            .write_all(b"alpha\nbeta\n")
            .expect("stdin should accept input");
    }

    let run = child
        .wait_with_output()
        .expect("generated executable should finish after stdin closes");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "standard input run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "alpha\nbeta\n11\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_standard_input_lines() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-stdin-lines-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-stdin-lines-{unique}"));
    fs::write(
        &source_path,
        "val readLines = StandardInput#lines\nval lines = readLines()\nval trimmed = map(lines)(trim)\nprintln(lines)\nprintln(trimmed)\nprintln(join(lines, \"|\"))\nassertResult([\" alpha \", \" beta \"])(lines)\nassertResult([\"alpha\", \"beta\"])(trimmed)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "standard input lines build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let mut child = Command::new(&output_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("generated executable should run");

    {
        let mut stdin = child.stdin.take().expect("stdin should be piped");
        stdin
            .write_all(b" alpha \n beta \n")
            .expect("stdin should accept input");
    }

    let run = child
        .wait_with_output()
        .expect("generated executable should finish after stdin closes");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "standard input lines run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[ alpha ,  beta ]\n[alpha, beta]\n alpha | beta \n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_environment_vars() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-env-vars-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-env-vars-{unique}"));
    fs::write(
        &source_path,
        "def capturedEnv() = env()\nval getVars = Environment#vars\nval envAlias = env\nval vars = getVars()\nval aliasVars = envAlias()\nval captured = capturedEnv()\nmutable found = false\nforeach(entry in aliasVars) {\n  if(entry == \"KLASSIC_NATIVE_ENV_TEST=alpha\") {\n    found = true\n  }\n}\nprintln(found)\nprintln(vars == aliasVars)\nassert(found)\nassertResult(vars)(aliasVars)\nassertResult(vars)(captured)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "environment vars build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .env("KLASSIC_NATIVE_ENV_TEST", "alpha")
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "environment vars run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "true\ntrue\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_environment_get_and_exists() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-env-get-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-env-get-{unique}"));
    fs::write(
        &source_path,
        "val get = Environment#get\nval exists = hasEnv\nprintln(get(\"KLASSIC_NATIVE_ENV_GET_TEST\"))\nprintln(exists(\"KLASSIC_NATIVE_ENV_GET_TEST\"))\nprintln(Environment#exists(\"KLASSIC_NATIVE_ENV_GET_MISSING\"))\nassertResult(\"alpha\")(getEnv(\"KLASSIC_NATIVE_ENV_GET_TEST\"))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "environment get build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .env("KLASSIC_NATIVE_ENV_GET_TEST", "alpha")
        .env_remove("KLASSIC_NATIVE_ENV_GET_MISSING")
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "environment get run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "alpha\ntrue\nfalse\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_environment_key_lookup() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-env-key-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-env-key-{unique}"));
    fs::write(
        &source_path,
        "val key = head(args())\nval missing = \"KLASSIC_NATIVE_ENV_DYNAMIC_MISSING\"\nprintln(Environment#get(key))\nprintln(Environment#exists(key))\nprintln(hasEnv(missing))\nassertResult(\"beta\")(getEnv(key))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime environment key build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .arg("KLASSIC_NATIVE_ENV_DYNAMIC_KEY")
        .env("KLASSIC_NATIVE_ENV_DYNAMIC_KEY", "beta")
        .env_remove("KLASSIC_NATIVE_ENV_DYNAMIC_MISSING")
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime environment key run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "beta\ntrue\nfalse\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_to_string_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-to-string-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-to-string-{unique}"));
    fs::write(
        &source_path,
        "val n = size(args())\nval ok = Environment#exists(head(args()))\nval nt = toString(n)\nval okt = toString(ok)\nval dynamicUnit = if(ok) () else ()\nval dynamicNull = if(ok) null else null\nval unitText = toString(dynamicUnit)\nval nullText = toString(dynamicNull)\nprintln(nt)\nprintln(okt)\nprintln(\"n=\" + nt)\nprintln(\"ok=\" + okt)\nprintln(unitText)\nprintln(nullText)\nprintln(\"unit=\" + unitText)\nprintln(\"null=\" + nullText)\nassertResult(\"1\")(nt)\nassertResult(\"true\")(okt)\nassertResult(\"()\")(unitText)\nassertResult(\"null\")(nullText)\nassertResult(())(dynamicUnit)\nassertResult(null)(dynamicNull)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime toString build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .arg("KLASSIC_NATIVE_TOSTRING_ENV")
        .env("KLASSIC_NATIVE_TOSTRING_ENV", "1")
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime toString run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "1\ntrue\nn=1\nok=true\n()\nnull\nunit=()\nnull=null\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_line_cons() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let lines_path_holder =
        std::env::temp_dir().join(format!("klassic-native-lines-path-holder-{unique}.txt"));
    let rewritten_path_holder = std::env::temp_dir().join(format!(
        "klassic-native-rewritten-lines-path-holder-{unique}.txt"
    ));
    let prefix_path_holder =
        std::env::temp_dir().join(format!("klassic-native-prefix-path-holder-{unique}.txt"));
    let lines_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-cons-lines-{unique}.txt"));
    let rewritten_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-cons-rewritten-{unique}.txt"
    ));
    let prefix_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-cons-prefix-{unique}.txt"));
    let source_path = std::env::temp_dir().join(format!("klassic-native-runtime-cons-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-cons-{unique}"));
    let source = format!(
        r#"val linesPathHolder = "{}"
val rewrittenPathHolder = "{}"
val prefixPathHolder = "{}"
val linesPath = FileInput#all(linesPathHolder)
val rewrittenPath = FileInput#all(rewrittenPathHolder)
val prefixPath = FileInput#all(prefixPathHolder)
FileOutput#writeLines(linesPath, ["line-a", "line-b"])
val runtimeLines = FileInput#lines(linesPath)
val prefixedLines = cons("line-0")(runtimeLines)
println(prefixedLines)
println(join(prefixedLines, "|"))
println(size(prefixedLines))
println(head(prefixedLines))
println(tail(prefixedLines))
val shoutedLines = map(runtimeLines)((line) => line + "!")
val upperLines = runtimeLines.map((line) => toUpperCase(line))
val foldedLines = foldLeft(runtimeLines)("")((acc, line) => acc + "[" + line + "]")
val totalChars = foldLeft(runtimeLines)(0)((acc, line) => acc + length(line))
val longLines = foldLeft(runtimeLines)(0)((acc, line) => if(length(line) > 5) acc + 1 else acc)
val hasLineB = foldLeft(runtimeLines)(false)((acc, line) => acc || line.contains("line-b"))
val allLinePrefixed = foldLeft(runtimeLines)(true)((acc, line) => acc && startsWith(line, "line-"))
val left = "<"
val right = ">"
val decorate = (line) => left + line + right
val reduceJoin = (acc, line) => acc + left + line + right
val decoratedLines = map(runtimeLines)(decorate)
val foldedViaAlias = foldLeft(runtimeLines)("")(reduceJoin)
val runtimeLeft = FileInput#all(prefixPath)
val decorateRuntime = (line) => runtimeLeft + line + right
val runtimeDecoratedLines = runtimeLines.map(decorateRuntime)
val runtimeFoldedViaAlias = foldLeft(runtimeLines)("")((acc, line) => acc + runtimeLeft + line + right)
val reversedLines = foldLeft(runtimeLines)([])((acc, line) => cons(line)(acc))
val seededReversedLines = runtimeLines.foldLeft(["line-z"], (acc, line) => cons(line)(acc))
val summary = foldLeft(runtimeLines)(record {{ count: 0, text: "", rows: [] }})((acc, line) => record {{ count: acc.count + 1, text: acc.text + "[" + line + "]", rows: cons(line)(acc.rows) }})
println(shoutedLines)
println(join(shoutedLines, "|"))
println(upperLines)
println(foldedLines)
println(totalChars)
println(longLines)
println(hasLineB)
println(allLinePrefixed)
println(decoratedLines)
println(foldedViaAlias)
println(runtimeDecoratedLines)
println(runtimeFoldedViaAlias)
println(reversedLines)
println(seededReversedLines)
println(summary)
println(summary.count)
println(summary.text)
println(summary.rows)
assertResult(["line-0", "line-a", "line-b"])(prefixedLines)
assertResult(["line-a!", "line-b!"])(shoutedLines)
assertResult(["LINE-A", "LINE-B"])(upperLines)
assertResult("[line-a][line-b]")(foldedLines)
assertResult(12)(totalChars)
assertResult(2)(longLines)
assert(hasLineB)
assert(allLinePrefixed)
assertResult(["<line-a>", "<line-b>"])(decoratedLines)
assertResult("<line-a><line-b>")(foldedViaAlias)
assertResult(["<line-a>", "<line-b>"])(runtimeDecoratedLines)
assertResult("<line-a><line-b>")(runtimeFoldedViaAlias)
assertResult(["line-b", "line-a"])(reversedLines)
assertResult(["line-b", "line-a", "line-z"])(seededReversedLines)
assertResult(record {{ count: 2, text: "[line-a][line-b]", rows: ["line-b", "line-a"] }})(summary)
val visitResult = foldLeft(runtimeLines)(())((acc, line) => println("visit " + line))
assertResult(())(visitResult)
val nullFold = foldLeft(runtimeLines)(null)((acc, line) => null)
assertResult(null)(nullFold)
assert(prefixedLines == ["line-0", "line-a", "line-b"])
FileOutput#writeLines(rewrittenPath, prefixedLines)
println(FileInput#all(rewrittenPath))
assertResult(prefixedLines)(FileInput#lines(rewrittenPath))
FileOutput#delete(linesPath)
FileOutput#delete(rewrittenPath)
"#,
        lines_path_holder.display(),
        rewritten_path_holder.display(),
        prefix_path_holder.display()
    );
    fs::write(&source_path, source).expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime line cons build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&lines_path_holder, lines_path.to_string_lossy().as_bytes())
        .expect("lines path holder should write after native build");
    fs::write(
        &rewritten_path_holder,
        rewritten_path.to_string_lossy().as_bytes(),
    )
    .expect("rewritten path holder should write after native build");
    fs::write(
        &prefix_path_holder,
        prefix_path.to_string_lossy().as_bytes(),
    )
    .expect("prefix path holder should write after native build");
    fs::write(&prefix_path, b"<").expect("prefix file should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&lines_path_holder);
    let _ = fs::remove_file(&rewritten_path_holder);
    let _ = fs::remove_file(&prefix_path_holder);
    let _ = fs::remove_file(&lines_path);
    let _ = fs::remove_file(&rewritten_path);
    let _ = fs::remove_file(&prefix_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime line cons run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[line-0, line-a, line-b]\nline-0|line-a|line-b\n3\nline-0\n[line-a, line-b]\n[line-a!, line-b!]\nline-a!|line-b!\n[LINE-A, LINE-B]\n[line-a][line-b]\n12\n2\ntrue\ntrue\n[<line-a>, <line-b>]\n<line-a><line-b>\n[<line-a>, <line-b>]\n<line-a><line-b>\n[line-b, line-a]\n[line-b, line-a, line-z]\n#(2, [line-a][line-b], [line-b, line-a])\n2\n[line-a][line-b]\n[line-b, line-a]\nvisit line-a\nvisit line-b\nline-0\nline-a\nline-b\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_line_map_builtin_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let input_path_holder = std::env::temp_dir().join(format!(
        "klassic-native-line-map-builtin-holder-{unique}.txt"
    ));
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-line-map-builtin-{unique}.txt"));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-line-map-builtin-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-line-map-builtin-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"val path = FileInput#all("{}")
val lines = FileInput#lines(path)
val trimLine = trim
val mapAlias = map
val pickedUpper = {{
  println("pick mapper")
  toUpperCase
}}
val trimmed = lines.map(trimLine)
val aliasTrimmed = mapAlias(lines)(trimLine)
val directUpper = map(lines)(toUpperCase)
val pickedUpperLines = lines.map(pickedUpper)
println(trimmed)
println(aliasTrimmed)
println(directUpper)
println(pickedUpperLines)
assertResult(["alpha", "beta"])(trimmed)
assertResult(["alpha", "beta"])(aliasTrimmed)
assertResult(["  ALPHA  ", "BETA"])(directUpper)
assertResult(["  ALPHA  ", "BETA"])(pickedUpperLines)
"#,
            input_path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime line map builtin value build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path_holder, input_path.to_string_lossy().as_bytes())
        .expect("input path holder should write after native build");
    fs::write(&input_path, "  alpha  \nbeta").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&input_path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime line map builtin value run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "pick mapper\n[alpha, beta]\n[alpha, beta]\n[  ALPHA  , BETA]\n[  ALPHA  , BETA]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_method_style_fold_left() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let input_path_holder =
        std::env::temp_dir().join(format!("klassic-native-method-fold-holder-{unique}.txt"));
    let input_path = std::env::temp_dir().join(format!("klassic-native-method-fold-{unique}.txt"));
    let source_path = std::env::temp_dir().join(format!("klassic-native-method-fold-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-method-fold-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"val path = FileInput#all("{}")
val lines = FileInput#lines(path)
val staticSum = [1, 2, 3].foldLeft(0, (acc, item) => acc + item)
val staticText = ["a", "b"].foldLeft("", (acc, item) => acc + item)
val folded = lines.foldLeft("", (acc, line) => acc + "<" + line + ">")
val foldAlias = foldLeft
val aliasFolded = foldAlias(lines)("")((acc, line) => acc + "[" + line + "]")
val totalChars = lines.foldLeft(0, (acc, line) => acc + length(line))
val allLinePrefixed = lines.foldLeft(true, (acc, line) => acc && startsWith(line, "line-"))
println(staticSum)
println(staticText)
println(folded)
println(aliasFolded)
println(totalChars)
println(allLinePrefixed)
assertResult(6)(staticSum)
assertResult("ab")(staticText)
assertResult("<line-a><line-b>")(folded)
assertResult("[line-a][line-b]")(aliasFolded)
assertResult(12)(totalChars)
assert(allLinePrefixed)
"#,
            input_path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "method-style foldLeft build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path_holder, input_path.to_string_lossy().as_bytes())
        .expect("input path holder should write after native build");
    fs::write(&input_path, "line-a\nline-b").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&input_path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "method-style foldLeft run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "6\nab\n<line-a><line-b>\n[line-a][line-b]\n12\ntrue\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_line_to_string() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-line-to-string-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-line-to-string-{unique}"));
    fs::write(
        &source_path,
        "val probe = head(args())\nval lines = tail(args())\nval trailing = (probe + \",\").split(\",\")\nval empty = split(\"\", \",\")\nprintln(toString(lines))\nprintln(\"lines=\" + lines)\nprintln(toString(trailing))\nprintln(\"empty=\" + empty)\nprintln(lines.contains(\"beta\"))\nprintln(contains(lines)(\"gamma\"))\nprintln(lines.contains(probe))\nprintln(lines.contains(length(probe)))\nassertResult(\"[beta, gamma]\")(toString(lines))\nassertResult(\"lines=[beta, gamma]\")(\"lines=\" + lines)\nassertResult(\"[alpha, ]\")(toString(trailing))\nassertResult(\"empty=[]\")(\"empty=\" + empty)\nassert(lines.contains(\"beta\"))\nassert(contains(lines)(\"gamma\"))\nassert(!lines.contains(probe))\nassert(!lines.contains(length(probe)))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime line toString build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .arg("alpha")
        .arg("beta")
        .arg("gamma")
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime line toString run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[beta, gamma]\nlines=[beta, gamma]\n[alpha, ]\nempty=[]\ntrue\ntrue\nfalse\nfalse\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_line_csv_processing() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let csv_path_holder =
        std::env::temp_dir().join(format!("klassic-native-csv-path-holder-{unique}.txt"));
    let multi_path_holder =
        std::env::temp_dir().join(format!("klassic-native-multi-path-holder-{unique}.txt"));
    let delimiter_path_holder =
        std::env::temp_dir().join(format!("klassic-native-delimiter-path-holder-{unique}.txt"));
    let chars_path_holder =
        std::env::temp_dir().join(format!("klassic-native-chars-path-holder-{unique}.txt"));
    let empty_path_holder =
        std::env::temp_dir().join(format!("klassic-native-empty-path-holder-{unique}.txt"));
    let csv_path = std::env::temp_dir().join(format!("klassic-native-runtime-csv-{unique}.csv"));
    let multi_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-multi-{unique}.txt"));
    let delimiter_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-delimiter-{unique}.txt"));
    let chars_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-chars-{unique}.txt"));
    let empty_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-empty-{unique}.txt"));
    let source_path = std::env::temp_dir().join(format!("klassic-native-runtime-csv-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-csv-{unique}"));
    let source = format!(
        r#"val csvPathHolder = "{}"
val multiPathHolder = "{}"
val delimiterPathHolder = "{}"
val charsPathHolder = "{}"
val emptyPathHolder = "{}"
val csvPath = FileInput#all(csvPathHolder)
val multiPath = FileInput#all(multiPathHolder)
val delimiterPath = FileInput#all(delimiterPathHolder)
val charsPath = FileInput#all(charsPathHolder)
val emptyPath = FileInput#all(emptyPathHolder)
val lines = FileInput#lines(csvPath)
val data = tail(lines)
val formatRow = (row) => {{
  val fields = split(row, ",")
  val name = head(fields)
  val age = head(tail(fields))
  val city = head(tail(tail(fields)))
  name + ":" + age + "@" + city
}}
val results = map(data)(formatRow)
println(results)
println(join(results, "|"))
assertResult(["Alice:30@Tokyo", "Bob:25@Kyoto"])(results)
val words = split(FileInput#all(multiPath), "--")
println(words)
println(join(words, "|"))
println(size(words))
assertResult(["red", "green", "", "blue", ""])(words)
assertResult("red|green||blue|")(join(words, "|"))
assertResult(5)(size(words))
val wordTail = tail(words)
val decoratedWords = map(words)((word) => "<" + word + ">")
val foldedWords = foldLeft(words)("")((acc, word) => acc + "[" + word + "]")
println(wordTail)
println(decoratedWords)
println(foldedWords)
println(join(wordTail, ":"))
println(size(wordTail))
assertResult(["green", "", "blue", ""])(wordTail)
assertResult(["<red>", "<green>", "<>", "<blue>", "<>"])(decoratedWords)
assertResult("[red][green][][blue][]")(foldedWords)
assertResult("green::blue:")(join(wordTail, ":"))
assertResult(4)(size(wordTail))
val dynamicWords = split(FileInput#all(multiPath), FileInput#all(delimiterPath))
println(dynamicWords)
println(join(dynamicWords, "/"))
println(join(dynamicWords, FileInput#all(delimiterPath)))
println(join(dynamicWords, FileInput#all(emptyPath)))
assertResult(["red", "green", "", "blue", ""])(dynamicWords)
assertResult("red/green//blue/")(join(dynamicWords, "/"))
assertResult("red--green----blue--")(join(dynamicWords, FileInput#all(delimiterPath)))
assertResult("redgreenblue")(join(dynamicWords, FileInput#all(emptyPath)))
val chars = split(FileInput#all(charsPath), "")
println(chars)
assertResult(["h", "é", "!"])(chars)
val dynamicChars = split(FileInput#all(charsPath), FileInput#all(emptyPath))
println(dynamicChars)
assertResult(["h", "é", "!"])(dynamicChars)
val emptySplit = split(FileInput#all(emptyPath), "--")
val emptyChars = split(FileInput#all(emptyPath), "")
println(size(emptySplit))
println(size(emptyChars))
assertResult([""])(emptySplit)
assertResult(1)(size(emptySplit))
assertResult("")(head(emptySplit))
assertResult(0)(size(emptyChars))
"#,
        csv_path_holder.display(),
        multi_path_holder.display(),
        delimiter_path_holder.display(),
        chars_path_holder.display(),
        empty_path_holder.display()
    );
    fs::write(&source_path, source).expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime CSV build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&csv_path_holder, csv_path.to_string_lossy().as_bytes())
        .expect("csv path holder should write after native build");
    fs::write(&multi_path_holder, multi_path.to_string_lossy().as_bytes())
        .expect("multi path holder should write after native build");
    fs::write(
        &delimiter_path_holder,
        delimiter_path.to_string_lossy().as_bytes(),
    )
    .expect("delimiter path holder should write after native build");
    fs::write(&chars_path_holder, chars_path.to_string_lossy().as_bytes())
        .expect("chars path holder should write after native build");
    fs::write(&empty_path_holder, empty_path.to_string_lossy().as_bytes())
        .expect("empty path holder should write after native build");
    fs::write(&csv_path, b"name,age,city\nAlice,30,Tokyo\nBob,25,Kyoto")
        .expect("csv file should write after native build");
    fs::write(&multi_path, b"red--green----blue--")
        .expect("multi delimiter file should write after native build");
    fs::write(&delimiter_path, b"--").expect("delimiter file should write after native build");
    fs::write(&chars_path, "hé!".as_bytes()).expect("chars file should write after native build");
    fs::write(&empty_path, b"").expect("empty file should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&csv_path_holder);
    let _ = fs::remove_file(&multi_path_holder);
    let _ = fs::remove_file(&delimiter_path_holder);
    let _ = fs::remove_file(&chars_path_holder);
    let _ = fs::remove_file(&empty_path_holder);
    let _ = fs::remove_file(&csv_path);
    let _ = fs::remove_file(&multi_path);
    let _ = fs::remove_file(&delimiter_path);
    let _ = fs::remove_file(&chars_path);
    let _ = fs::remove_file(&empty_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime CSV run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[Alice:30@Tokyo, Bob:25@Kyoto]\nAlice:30@Tokyo|Bob:25@Kyoto\n[red, green, , blue, ]\nred|green||blue|\n5\n[green, , blue, ]\n[<red>, <green>, <>, <blue>, <>]\n[red][green][][blue][]\ngreen::blue:\n4\n[red, green, , blue, ]\nred/green//blue/\nred--green----blue--\nredgreenblue\n[h, é, !]\n[h, é, !]\n1\n0\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_file_input_errors() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let missing_path =
        std::env::temp_dir().join(format!("klassic-native-missing-input-{unique}.txt"));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-input-error-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-input-error-{unique}"));
    fs::write(
        &source_path,
        format!("println(FileInput#all(\"{}\"))\n", missing_path.display()),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime file input error build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!run.status.success());
    assert!(run.stdout.is_empty());
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        format!(
            "{}:1:1: FileInput#all failed to open file\n",
            source_path.display()
        )
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_file_input_binding_overflow_error() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-input-overflow-{unique}.txt"
    ));
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-input-overflow-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-input-overflow-{unique}"));
    fs::write(
        &source_path,
        format!(
            "val text = FileInput#all(\"{}\")\nprintln(text)\n",
            input_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime file input overflow build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    fs::write(&input_path, "x".repeat(65_537)).expect("large input should write after build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!run.status.success());
    assert!(run.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&run.stderr)
            .contains("FileInput#all runtime string exceeds 65536 bytes")
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_cleanup_return_value_preservation() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-cleanup-value-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-cleanup-value-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval x = { hits += 1; 10 } cleanup { hits += 10 }\nval ok = { hits += 1; true } cleanup { hits += 10 }\nprintln(x)\nprintln(ok)\nprintln(hits)\nassertResult(10)(x)\nassert(ok)\nassertResult(22)(hits)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "10\ntrue\n22\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_file_input_open_preserves_side_effecting_callback() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-file-input-effect-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-file-input-effect-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval text = \"src/test/resources/hello.txt\" FileInput#open {(stream) =>\n  FileInput#readAll(stream) cleanup { hits += 1 }\n}\nprintln(hits)\nprintln(text)\nassertResult(1)(hits)\nassertResult(\"Hello, World!\")(text)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "1\nHello, World!\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_dir_helpers() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let base = std::env::temp_dir().join(format!("klassic-native-dir-{unique}-work"));
    let nested = base.join("nested");
    let file = nested.join("a.txt");
    let copied = nested.join("b.txt");
    let moved = nested.join("c.txt");
    let source_path = std::env::temp_dir().join(format!("klassic-native-dir-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-dir-{unique}-bin"));
    fs::write(
        &source_path,
        format!(
            "val base = \"{}\"\nval nested = \"{}\"\nval file = \"{}\"\nval copied = \"{}\"\nval moved = \"{}\"\nprintln(Dir#exists(base))\nDir#mkdir(base)\nprintln(Dir#isDirectory(base))\nDir#mkdirs(nested)\nFileOutput#write(file, \"hello\")\nprintln(Dir#isFile(file))\nprintln(Dir#list(nested))\nprintln(Dir#listFull(nested))\nDir#copy(file, copied)\nprintln(FileInput#all(copied))\nDir#move(copied, moved)\nprintln(Dir#isFile(moved))\nFileOutput#delete(file)\nFileOutput#delete(moved)\nDir#delete(nested)\nDir#delete(base)\nprintln(Dir#exists(base))\n",
            base.display(),
            nested.display(),
            file.display(),
            copied.display(),
            moved.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dir helper build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);
    let _ = fs::remove_dir_all(&base);

    assert!(
        run.status.success(),
        "dir helper run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        format!(
            "false\ntrue\ntrue\n[a.txt]\n[{}]\nhello\ntrue\nfalse\n",
            file.display()
        )
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dir_builtin_function_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let base = std::env::temp_dir().join(format!("klassic-native-dir-values-{unique}-work"));
    let nested = base.join("nested");
    let file = nested.join("a.txt");
    let source_path = std::env::temp_dir().join(format!("klassic-native-dir-values-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-dir-values-{unique}-bin"));
    fs::write(
        &source_path,
        format!(
            "val base = \"{}\"\nval nested = \"{}\"\nval file = \"{}\"\n({{ println(\"pick mkdirs\"); Dir#mkdirs }})(nested)\n({{ println(\"pick write\"); FileOutput#write }})(file, \"hello\")\nprintln(({{ println(\"pick list\"); Dir#list }})(nested))\nprintln(({{ println(\"pick listFull\"); Dir#listFull }})(nested))\nprintln(({{ println(\"pick isDir\"); Dir#isDirectory }})(nested))\nprintln(({{ println(\"pick isFile\"); Dir#isFile }})(file))\n({{ println(\"pick delete file\"); FileOutput#delete }})(file)\n({{ println(\"pick delete nested\"); Dir#delete }})(nested)\n({{ println(\"pick delete base\"); Dir#delete }})(base)\nprintln(({{ println(\"pick exists\"); Dir#exists }})(base))\n",
            base.display(),
            nested.display(),
            file.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dir builtin value build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);
    let _ = fs::remove_dir_all(&base);

    assert!(
        run.status.success(),
        "dir builtin value run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        format!(
            "pick mkdirs\npick write\npick list\n[a.txt]\npick listFull\n[{}]\npick isDir\ntrue\npick isFile\ntrue\npick delete file\npick delete nested\npick delete base\npick exists\nfalse\n",
            file.display()
        )
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_mutable_loop_then_static_ternary() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test-programs/ternary-expression.kl");
    let output_path = std::env::temp_dir().join(format!("klassic-native-ternary-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_record_sample() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test-programs/record.kl");
    let output_path = std::env::temp_dir().join(format!("klassic-native-record-sample-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_thread_sample() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test-programs/builtin_functions-thread.kl");
    let output_path = std::env::temp_dir().join(format!("klassic-native-thread-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "Hello from main thread.\nHello from another thread.\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_thread_and_stopwatch_lambda_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-thread-lambda-value-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-thread-lambda-value-{unique}"));
    fs::write(
        &source_path,
        r#"mutable hits = 0
val job = () => {
  hits += 1
  println("job " + hits)
}
val pickedThread = {
  println("pick thread")
  () => {
    hits += 10
    println("picked " + hits)
  }
}
thread(job)
thread(pickedThread)
val measured = () => {
  hits += 100
  hits
}
val elapsed = stopwatch(measured)
val pickedElapsed = stopwatch({
  println("pick stopwatch")
  () => {
    hits += 1000
    hits
  }
})
println(elapsed >= 0)
println(pickedElapsed >= 0)
println(hits)
assert(elapsed >= 0)
assert(pickedElapsed >= 0)
assertResult(1100)(hits)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "thread/stopwatch lambda value build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "thread/stopwatch lambda value run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "pick thread\npick stopwatch\ntrue\ntrue\n1100\njob 1101\npicked 1111\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_thread_block_local_mutable_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-thread-capture-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-thread-capture-{unique}"));
    fs::write(
        &source_path,
        "println({\n  mutable x = 0\n  thread(() => {\n    x = x + 2\n    println(x)\n  })\n  thread(() => {\n    x = x + 3\n    println(x)\n  })\n  0\n})\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "0\n2\n5\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_thread_function_local_mutable_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-thread-fn-capture-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-thread-fn-capture-{unique}"));
    fs::write(
        &source_path,
        "def enqueue() = {\n  mutable x = 10\n  thread(() => {\n    x = x + 1\n    println(x)\n  })\n  thread(() => {\n    x = x + 2\n    println(x)\n  })\n  0\n}\nprintln(enqueue())\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "0\n11\n13\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_thread_alias_inside_function() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-thread-alias-fn-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-thread-alias-fn-{unique}"));
    fs::write(
        &source_path,
        "val spawn = thread\nval spawnLater = spawn\ndef enqueue() = {\n  mutable x = 10\n  spawnLater(() => {\n    x = x + 7\n    println(x)\n  })\n  0\n}\nprintln(enqueue())\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "0\n17\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_thread_alias_inside_lambda_value() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-thread-alias-lambda-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-thread-alias-lambda-{unique}"));
    fs::write(
        &source_path,
        "val spawn = thread\nval enqueue = () => {\n  mutable x = 3\n  spawn(() => {\n    x = x * 5\n    println(x)\n  })\n  0\n}\nprintln(enqueue())\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "0\n15\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_thread_foreach_iteration_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-thread-foreach-capture-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-thread-foreach-capture-{unique}"));
    fs::write(
        &source_path,
        "foreach(i in [1, 2, 3]) {\n  thread(() => println(i))\n}\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "1\n2\n3\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_curried_fold_sample() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test-programs/functions.kl");
    let output_path = std::env::temp_dir().join(format!("klassic-native-functions-{unique}"));

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_int_lists() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-list-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-list-{unique}"));
    fs::write(
        &source_path,
        "println([1, 2, -3])\nprintln(\"size = \" + size([1 2 3]))\nprintln(\"head = \" + head([42, 100]))\nprintln(\"tail = \" + tail([1, 2, 3]))\nprintln(\"cons = \" + cons(0)([1, 2]))\nprintln(\"words = \" + cons(\"a\")([\"b\", \"c\"]))\nprintln(\"infix = \" + (9 #cons [10]))\nprintln(\"reverse = \" + foldLeft([1, 2, 3])([])((acc, e) => e #cons acc))\nprintln(\"map = \" + map([1, 2, 3])((x) => x * 2 + 1))\nprintln(\"sum = \" + foldLeft([1, 2, 3])(0)((r, e) => r + e))\nprintln(\"empty? \" + isEmpty([1]))\nassertResult([2, 3])(tail([1, 2, 3]))\nassertResult([\"a\", \"b\", \"c\"])(cons(\"a\")([\"b\", \"c\"]))\nassertResult([9, 10])(9 #cons [10])\nassertResult([3, 2, 1])(foldLeft([1, 2, 3])([])((acc, e) => e #cons acc))\nassertResult([3, 5, 7])(map([1, 2, 3])((x) => x * 2 + 1))\nassertResult(6)(foldLeft([1, 2, 3])(0)((r, e) => r + e))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[1, 2, -3]\nsize = 3\nhead = 42\ntail = [2, 3]\ncons = [0, 1, 2]\nwords = [a, b, c]\ninfix = [9, 10]\nreverse = [3, 2, 1]\nmap = [3, 5, 7]\nsum = 6\nempty? false\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_cons_argument_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-cons-side-effects-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-cons-side-effects-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval words = cons({ hits += 1; \"a\" })({ hits += 1; [\"b\"] })\nval nums = ({ hits += 1; 0 }) #cons ({ hits += 1; [1] })\nprintln(hits)\nprintln(words)\nprintln(nums)\nassertResult(4)(hits)\nassertResult([\"a\", \"b\"])(words)\nassertResult([0, 1])(nums)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "4\n[a, b]\n[0, 1]\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_list_and_record_access_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-access-side-effects-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-access-side-effects-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval size1 = size({ hits += 1; [1, 2] })\nval head1 = head({ hits += 1; [9, 10] })\nval tail1 = tail({ hits += 1; [9, 10] })\nval empty1 = isEmpty({ hits += 1; [] })\nval methodSize = ({ hits += 1; [3, 4] }).size()\nval methodHead = ({ hits += 1; [5, 6] }).head()\nval methodTail = ({ hits += 1; [7, 8] }).tail()\nval field = ({ hits += 1; record { x: 11 } }).x\nprintln(hits)\nprintln(size1)\nprintln(head1)\nprintln(tail1)\nprintln(empty1)\nprintln(methodSize)\nprintln(methodHead)\nprintln(methodTail)\nprintln(field)\nassertResult(8)(hits)\nassertResult(2)(size1)\nassertResult(9)(head1)\nassertResult([10])(tail1)\nassertResult(true)(empty1)\nassertResult(2)(methodSize)\nassertResult(5)(methodHead)\nassertResult([8])(methodTail)\nassertResult(11)(field)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "8\n2\n9\n[10]\ntrue\n2\n5\n[8]\n11\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_method_map_and_foreach_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-method-map-side-effects-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-method-map-side-effects-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nforeach(x in { hits += 1; [1, 2] }) {\n  hits += x\n  val pair = [x, x + 1]\n  val row = record { value: x, next: x + 1 }\n  println(pair)\n  assertResult([x, x + 1])(pair)\n  assertResult(x + 1)(row.next)\n}\nval ys = ({ hits += 1; [1, 2] }).map((x) => { hits += 1; x + 1 })\nprintln(hits)\nprintln(ys)\nassertResult(7)(hits)\nassertResult([2, 3])(ys)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[1, 2]\n[2, 3]\n7\n[2, 3]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_map_and_fold_lambda_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-map-fold-side-effects-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-map-fold-side-effects-{unique}"));
    fs::write(
        &source_path,
        "def makeAdder(n: Int) = (x: Int) => x + n\nval add2 = makeAdder(2)\nmutable hits = 0\nval ys = map([1, 2, 3])((x) => { hits += 1; x + 1 })\nval zs = map([1, 2])((x) => add2(x))\nval total = foldLeft([1, 2, 3])({ hits += 1; 0 })((acc, e) => { hits += 1; acc + e })\nprintln(hits)\nprintln(ys)\nprintln(zs)\nprintln(total)\nassertResult(7)(hits)\nassertResult([2, 3, 4])(ys)\nassertResult([3, 4])(zs)\nassertResult(6)(total)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "7\n[2, 3, 4]\n[3, 4]\n6\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_string_and_list_bindings() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-static-bind-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-static-bind-{unique}"));
    fs::write(
        &source_path,
        "val greeting = \"hello\"\nval xs = [5, 8, 13]\nval ys = cons(3)(tail(xs))\nval doubled = map(xs)((x) => x * 2)\nval sum = foldLeft(xs)(0)((r, e) => r + e)\nprintln(greeting)\nprintln(\"greeting = \" + greeting)\nprintln(xs)\nprintln(\"size = \" + size(xs))\nprintln(\"head = \" + head(xs))\nprintln(\"tail = \" + tail(xs))\nprintln(\"tail size = \" + size(tail(xs)))\nprintln(\"ys = \" + ys)\nprintln(\"doubled = \" + doubled)\nprintln(\"sum = \" + sum)\nassertResult(\"hello\")(greeting)\nassertResult([8, 13])(tail(xs))\nassertResult([3, 8, 13])(ys)\nassertResult([10, 16, 26])(doubled)\nassertResult(26)(sum)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "hello\ngreeting = hello\n[5, 8, 13]\nsize = 3\nhead = 5\ntail = [8, 13]\ntail size = 2\nys = [3, 8, 13]\ndoubled = [10, 16, 26]\nsum = 26\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_straight_line_mutable_static_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-static-mutable-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-static-mutable-{unique}"));
    fs::write(
        &source_path,
        "mutable s = \"FOO\"\ns = s + s\nmutable xs = [\"a\"]\nxs = cons(\"b\")(xs)\nprintln(s)\nprintln(xs)\nassertResult(\"FOOFOO\")(s)\nassertResult([\"b\", \"a\"])(xs)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "FOOFOO\n[b, a]\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_annotated_string_lambda_parameters() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-string-lambda-{unique}.kl"));
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-string-lambda-{unique}.txt"));
    let lines_path =
        std::env::temp_dir().join(format!("klassic-native-string-lambda-lines-{unique}.txt"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-string-lambda-{unique}"));
    fs::write(
        &source_path,
        format!(
            r#"val textLength = (s: String) => length(s)
val lineCount = (lines: List<String>) => lines.size()
val prefix = "p:"
val baseLines = ["root"]
def prefixedLength(s: String): Int = length(prefix + s)
def totalLines(lines: List<String>): Int = lineCount(lines) + baseLines.size()
val text = FileInput#all("{}")
val lines = FileInput#lines("{}")
println(textLength(text))
println(textLength("abc"))
println(lineCount(lines))
println(lineCount(["x", "y"]))
println(prefixedLength(text))
println(totalLines(lines))
assertResult(7)(textLength(text))
assertResult(3)(textLength("abc"))
assertResult(3)(lineCount(lines))
assertResult(2)(lineCount(["x", "y"]))
assertResult(9)(prefixedLength(text))
assertResult(4)(totalLines(lines))
"#,
            input_path.display(),
            lines_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "annotated string lambda build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&input_path, "dynamic").expect("input source should write after native build");
    fs::write(&lines_path, "a\nb\nc").expect("lines source should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&lines_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "annotated string lambda run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "7\n3\n3\n2\n9\n4\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_placeholder_callable_aliases() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-placeholder-alias-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-placeholder-alias-{unique}"));
    fs::write(
        &source_path,
        "val id = _\nval add = _ + _\ndef inc(x) = x + 1\ndef sum(acc, e) = acc + e\nprintln(map([1])(id))\nprintln(map([1, 2, 3])(inc))\nprintln(foldLeft([1 2 3])(0)(add))\nprintln(foldLeft([1 2 3])(0)(sum))\nassertResult([1])(map([1])(id))\nassertResult([2, 3, 4])(map([1, 2, 3])(inc))\nassertResult(6)(foldLeft([1 2 3])(0)(add))\nassertResult(6)(foldLeft([1 2 3])(0)(sum))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[1]\n[2, 3, 4]\n6\n6\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_lambda_return_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-lambda-return-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-lambda-return-{unique}"));
    fs::write(
        &source_path,
        "def f(x) = _\ndef makeAdder(n: Int) = (x: Int) => x + n\nval make = (x) => _\nval add2 = makeAdder(2)\nmutable hits = 0\nprintln(((x) => x + 1)(2))\nprintln(map([1])(f(1)))\nprintln(map([1])(make(1)))\nprintln(add2({ hits += 1; 3 }))\nassertResult(3)(((x) => x + 1)(2))\nassertResult([1])(map([1])(f(1)))\nassertResult([1])(map([1])(make(1)))\nassertResult(5)(add2({ hits += 1; 3 }))\nassertResult(2)(hits)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n[1]\n[1]\n5\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_inline_lambda_runtime_arguments() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-lambda-runtime-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-lambda-runtime-{unique}"));
    fs::write(
        &source_path,
        "val elapsed = stopwatch( => 1)\nassert(((x) => x + 1)(elapsed) >= 1)\nassert(((x) => x >= 0)(elapsed))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_unannotated_inline_runtime_values() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-unannotated-inline-runtime-{unique}.kl"
    ));
    let text_path = std::env::temp_dir().join(format!(
        "klassic-native-unannotated-inline-runtime-text-{unique}.txt"
    ));
    let lines_path = std::env::temp_dir().join(format!(
        "klassic-native-unannotated-inline-runtime-lines-{unique}.txt"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-unannotated-inline-runtime-{unique}"
    ));
    fs::write(
        &source_path,
        format!(
            r#"def id(x) = x
def first(xs) = head(xs)
def blockId(x) = {{
  val y = x
  y
}}
def render(x) = "value=" + x
def renderAnnotated(x): String = "typed=" + x
def restAnnotated(xs): List<String> = tail(xs)
val text = FileInput#all("{}")
val lines = FileInput#lines("{}")
println(id(text))
println(join(id(lines), "|"))
println(first(lines))
println(blockId(text))
println(render(text))
println(renderAnnotated(text))
println(join(restAnnotated(lines), ":"))
assertResult("omega")(id(text))
assertResult(["red", "blue"])(id(lines))
assertResult("red")(first(lines))
assertResult("omega")(blockId(text))
assertResult("value=omega")(render(text))
assertResult("typed=omega")(renderAnnotated(text))
assertResult(["blue"])(restAnnotated(lines))
"#,
            text_path.display(),
            lines_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "unannotated inline runtime build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&text_path, "omega").expect("text input should write after native build");
    fs::write(&lines_path, "red\nblue").expect("lines input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&text_path);
    let _ = fs::remove_file(&lines_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "unannotated inline runtime run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "omega\nred|blue\nred\nomega\nvalue=omega\ntyped=omega\nblue\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_inline_lambda_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-lambda-side-effect-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-lambda-side-effect-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval result = ((x) => {\n  hits += 1\n  x\n})(1)\nval elapsed = stopwatch( => 1)\nassert(hits + elapsed >= elapsed + 1)\nassertResult(1)(result)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_record_lambda_method_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-record-method-effect-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-record-method-effect-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval r = record { call: (x) => {\n  hits += 1\n  x\n}}\nval result = r.call(1)\nval result2 = ({ hits += 1; record { call: (x) => { hits += 1; x + 1 } } }).call({ hits += 1; 1 })\nprintln(hits)\nprintln(result2)\nassertResult(1)(result)\nassertResult(4)(hits)\nassertResult(2)(result2)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "4\n2\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_if_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-static-if-effect-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-static-if-effect-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nif(true) {\n  hits += 1\n}\nval xs = if(true) { hits += 1; [1, 2] } else { hits += 10; [9] }\nval s = if(false) { hits += 10; \"bad\" } else { hits += 1; \"ok\" }\nval n = if({ hits += 1; true }) { hits += 1; 7 } else { hits += 10; 0 }\nprintln(hits)\nprintln(xs)\nprintln(s)\nprintln(n)\nassertResult(5)(hits)\nassertResult([1, 2])(xs)\nassertResult(\"ok\")(s)\nassertResult(7)(n)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "5\n[1, 2]\nok\n7\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_branch_assignment_state() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-branch-state-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-branch-state-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval flag = stopwatch( => 1) >= 0\nif(flag) {\n  hits = 1\n} else {\n  hits = 2\n}\nprintln(hits)\nassertResult(1)(hits)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "1\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_static_value_merges() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-static-merge-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-static-merge-{unique}"));
    fs::write(
        &source_path,
        "val flag = stopwatch( => 1) >= 0\nval label = if(flag) {\n  println(\"then\")\n  \"same\"\n} else {\n  println(\"else\")\n  \"same\"\n}\nprintln(label)\nmutable alias = \"old\"\nif(flag) {\n  alias = \"merged\"\n} else {\n  alias = \"merged\"\n}\nprintln(alias)\nmutable xs = [0]\nif(flag) {\n  xs = [1, 2]\n} else {\n  xs = [1, 2]\n}\nprintln(xs)\nassertResult(\"same\")(label)\nassertResult(\"merged\")(alias)\nassertResult([1, 2])(xs)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "then\nsame\nmerged\n[1, 2]\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_string_branch_results() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-string-result-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-string-result-{unique}"));
    fs::write(
        &source_path,
        "val flag = head(args()) == \"then\"\nval label = if(flag) {\n  println(\"then branch\")\n  \"alpha\"\n} else {\n  println(\"else branch\")\n  \"beta\"\n}\nprintln(label)\nprintln(\"tag=\" + label)\nif(flag) {\n  assertResult(\"alpha\")(label)\n} else {\n  assertResult(\"beta\")(label)\n}\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if string result build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let then_run = Command::new(&output_path)
        .arg("then")
        .output()
        .expect("generated executable should run then branch");
    let else_run = Command::new(&output_path)
        .arg("else")
        .output()
        .expect("generated executable should run else branch");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        then_run.status.success(),
        "dynamic if string then run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&then_run.stdout),
        String::from_utf8_lossy(&then_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&then_run.stdout),
        "then branch\nalpha\ntag=alpha\n"
    );
    assert!(then_run.stderr.is_empty());

    assert!(
        else_run.status.success(),
        "dynamic if string else run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&else_run.stdout),
        String::from_utf8_lossy(&else_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&else_run.stdout),
        "else branch\nbeta\ntag=beta\n"
    );
    assert!(else_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_runtime_line_branch_results() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-runtime-lines-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-runtime-lines-{unique}"));
    fs::write(
        &source_path,
        "val chooseArgs = head(args()) == \"args\"\nval chooseStatic = head(args()) == \"static\"\nval lines = if(chooseArgs) {\n  tail(args())\n} else if(chooseStatic) {\n  [\"static\", \"branch\"]\n} else {\n  (toString(size(args())) + \"\\nblue\").split(\"\\n\")\n}\nval staticLines = if(chooseStatic) [\"static-only\", \"then\"] else [\"static-only\", \"else\"]\nprintln(size(lines))\nprintln(lines.head())\nprintln(lines.join(\"|\"))\nprintln(staticLines.join(\"/\"))\nif(chooseArgs) {\n  assertResult(\"first\")(lines.head())\n  assertResult(\"first|second\")(lines.join(\"|\"))\n  assertResult(\"static-only/else\")(staticLines.join(\"/\"))\n} else if(chooseStatic) {\n  assertResult(\"static\")(lines.head())\n  assertResult(\"static|branch\")(lines.join(\"|\"))\n  assertResult(\"static-only/then\")(staticLines.join(\"/\"))\n} else {\n  assertResult(\"1\")(lines.head())\n  assertResult(\"1|blue\")(lines.join(\"|\"))\n  assertResult(\"static-only/else\")(staticLines.join(\"/\"))\n}\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic if runtime line result build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let args_run = Command::new(&output_path)
        .arg("args")
        .arg("first")
        .arg("second")
        .output()
        .expect("generated executable should run args branch");
    let split_run = Command::new(&output_path)
        .arg("split")
        .output()
        .expect("generated executable should run split branch");
    let static_run = Command::new(&output_path)
        .arg("static")
        .output()
        .expect("generated executable should run static branch");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        args_run.status.success(),
        "dynamic if runtime line args run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&args_run.stdout),
        String::from_utf8_lossy(&args_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&args_run.stdout),
        "2\nfirst\nfirst|second\nstatic-only/else\n"
    );
    assert!(args_run.stderr.is_empty());

    assert!(
        split_run.status.success(),
        "dynamic if runtime line split run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&split_run.stdout),
        String::from_utf8_lossy(&split_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&split_run.stdout),
        "2\n1\n1|blue\nstatic-only/else\n"
    );
    assert!(split_run.stderr.is_empty());

    assert!(
        static_run.status.success(),
        "dynamic if runtime line static run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&static_run.stdout),
        String::from_utf8_lossy(&static_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&static_run.stdout),
        "2\nstatic\nstatic|branch\nstatic-only/then\n"
    );
    assert!(static_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_function_value_merges() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-function-merge-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-function-merge-{unique}"));
    fs::write(
        &source_path,
        "val flag = stopwatch(() => 1) >= 0\nval f = if(flag) {\n  (x) => x + 1\n} else {\n  (x) => x + 1\n}\nprintln(f(4))\nval sub = if(flag) {\n  println(\"pick then\")\n  substring\n} else {\n  println(\"pick else\")\n  substring\n}\nprintln(sub(\"abcdef\", 1, 4))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "5\npick then\nbcd\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_branch_local_closure_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-closure-capture-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-closure-capture-{unique}"
    ));
    fs::write(
        &source_path,
        "val flag = stopwatch(() => 1) >= 0\nval f = if(flag) {\n  mutable x = 0\n  (y) => {\n    x = x + y\n    x\n  }\n} else {\n  mutable x = 0\n  (y) => {\n    x = x + y\n    x\n  }\n}\nprintln(f(2))\nprintln(f(3))\nassertResult(5)(f(0))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "2\n5\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_branch_local_thread_capture() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-thread-capture-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-thread-capture-{unique}"));
    fs::write(
        &source_path,
        "val flag = stopwatch(() => 1) >= 0\nif(flag) {\n  mutable x = 1\n  thread(() => {\n    x = x + 1\n    println(x)\n  })\n} else {\n  mutable x = 1\n  thread(() => {\n    x = x + 1\n    println(x)\n  })\n}\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "2\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_branch_local_record_closures() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-record-closures-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-record-closures-{unique}"
    ));
    fs::write(
        &source_path,
        "val flag = stopwatch(() => 1) >= 0\nval pair = if(flag) {\n  mutable x = 0\n  val inc = (y) => {\n    x = x + y\n    x\n  }\n  val get = () => x\n  record { inc: inc, get: get }\n} else {\n  mutable x = 0\n  val inc = (y) => {\n    x = x + y\n    x\n  }\n  val get = () => x\n  record { inc: inc, get: get }\n}\nprintln(pair.inc(2))\nprintln(pair.get())\nprintln(pair.inc(3))\nprintln(pair.get())\nassertResult(5)(pair.get())\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "2\n2\n5\n5\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_virtual_file_state_merges() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-file-merge-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-file-merge-{unique}"));
    let file_path = std::env::temp_dir().join(format!("klassic-native-merged-{unique}.txt"));
    let dir_path = std::env::temp_dir().join(format!("klassic-native-merged-dir-{unique}"));
    let dir_file_path = dir_path.join("merged.txt");
    fs::write(
        &source_path,
        format!(
            "val flag = stopwatch( => 1) < 0\nif(flag) {{\n  FileOutput#write(\"{}\", \"same\")\n}} else {{\n  FileOutput#write(\"{}\", \"same\")\n}}\nprintln(FileInput#all(\"{}\"))\nDir#mkdir(\"{}\")\nif(flag) {{\n  FileOutput#write(\"{}\", \"same\")\n}} else {{\n  FileOutput#write(\"{}\", \"same\")\n}}\nval entries = Dir#list(\"{}\")\nprintln(entries)\nassertResult([\"merged.txt\"])(entries)\nFileOutput#delete(\"{}\")\nDir#delete(\"{}\")\nFileOutput#delete(\"{}\")\n",
            file_path.display(),
            file_path.display(),
            file_path.display(),
            dir_path.display(),
            dir_file_path.display(),
            dir_file_path.display(),
            dir_path.display(),
            dir_file_path.display(),
            dir_path.display(),
            file_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);
    let _ = fs::remove_file(&file_path);
    let _ = fs::remove_file(&dir_file_path);
    let _ = fs::remove_dir(&dir_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "same\n[merged.txt]\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_branch_local_list_closure() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-if-list-closure-{unique}.kl"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-list-closure-{unique}"));
    fs::write(
        &source_path,
        "val flag = stopwatch(() => 1) >= 0\nval fs = if(flag) {\n  mutable x = 0\n  [(y) => {\n    x = x + y\n    x\n  }]\n} else {\n  mutable x = 0\n  [(y) => {\n    x = x + y\n    x\n  }]\n}\nval f = head(fs)\nprintln(f(2))\nprintln(f(3))\nassertResult(5)(f(0))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "2\n5\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_if_branch_local_map_closure() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-map-closure-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-if-map-closure-{unique}"));
    fs::write(
        &source_path,
        "val flag = stopwatch(() => 1) >= 0\nval table = if(flag) {\n  mutable x = 1\n  %[\"inc\": (y) => {\n    x = x + y\n    x\n  }]\n} else {\n  mutable x = 1\n  %[\"inc\": (y) => {\n    x = x + y\n    x\n  }]\n}\nval f = Map#get(table, \"inc\")\nprintln(f(2))\nprintln(f(3))\nassertResult(6)(f(0))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n6\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_uses_runtime_read_after_dynamic_if_virtual_file_state() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-file-leak-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-file-leak-{unique}"));
    let file_path = std::env::temp_dir().join(format!("klassic-native-leaked-{unique}.txt"));
    fs::write(
        &source_path,
        format!(
            "val flag = stopwatch( => 1) < 0\nif(flag) {{\n  FileOutput#write(\"{}\", \"then\")\n}} else {{\n  ()\n}}\nval text = FileInput#all(\"{}\")\nprintln(text)\n",
            file_path.display(),
            file_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic file runtime read build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);
    let _ = fs::remove_file(&file_path);

    assert!(!run.status.success());
    assert!(run.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&run.stderr).contains("FileInput#all failed to open file"),
        "{}",
        String::from_utf8_lossy(&run.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_uses_runtime_lines_after_dynamic_if_virtual_file_state() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-file-lines-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-file-lines-{unique}"));
    let file_path = std::env::temp_dir().join(format!("klassic-native-lines-{unique}.txt"));
    fs::write(
        &source_path,
        format!(
            "val flag = stopwatch( => 1) >= 0\nif(flag) {{\n  FileOutput#writeLines(\"{}\", [\"then\", \"branch\"])\n}} else {{\n  ()\n}}\nval lines = FileInput#lines(\"{}\")\nval readLines = FileInput#readLines(\"{}\")\nprintln(lines)\nprintln(join(readLines, \"|\"))\nassertResult([\"then\", \"branch\"])(lines)\nassertResult(lines)(readLines)\nFileOutput#delete(\"{}\")\n",
            file_path.display(),
            file_path.display(),
            file_path.display(),
            file_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "dynamic file runtime lines build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);
    let _ = fs::remove_file(&file_path);

    assert!(
        run.status.success(),
        "dynamic file runtime lines run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[then, branch]\nthen|branch\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_build_rejects_dynamic_if_divergent_thread_queues() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-thread-leak-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-thread-leak-{unique}"));
    fs::write(
        &source_path,
        "val flag = stopwatch(() => 1) < 0\nif(flag) {\n  thread(() => println(\"then\"))\n} else {\n  ()\n}\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!build.status.success());
    assert!(build.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&build.stderr).contains("divergent dynamic branches"),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_while_assignment_state() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-while-state-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-dynamic-while-state-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nmutable once = 0\nwhile(stopwatch( => 1) >= 0 && once == 0) {\n  hits = 1\n  once = 1\n}\nprintln(hits)\nassertResult(1)(hits)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "1\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_while_runtime_list_assignment() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-while-runtime-list-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-while-runtime-list-{unique}"
    ));
    fs::write(
        &source_path,
        "mutable xs = [1]\nmutable i = 0\nwhile(i < 2) {\n  xs = [i]\n  i += 1\n}\nprintln(xs)\nassertResult([1])(xs)\nmutable shortened = [0, 1, 2]\nmutable j = 0\nwhile(j < 1) {\n  shortened = [j]\n  j += 1\n}\nprintln(shortened)\nassertResult([0])(shortened)\nmutable untouched = [7]\nwhile(stopwatch(() => 1) < 0) {\n  untouched = [8]\n}\nprintln(untouched)\nassertResult([7])(untouched)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime-list while assignment build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime-list while assignment run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "[1]\n[0]\n[7]\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dynamic_while_condition_assignment_state() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-while-condition-state-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-dynamic-while-condition-state-{unique}"
    ));
    fs::write(
        &source_path,
        "mutable hits = 0\nmutable once = 0\nwhile(({ hits = 1; once == 0 }) && stopwatch( => 1) >= 0) {\n  once = 1\n}\nprintln(hits)\nassertResult(1)(hits)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "1\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_statically_skipped_while_body() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-skipped-while-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-skipped-while-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nwhile(false) {\n  mutable skipped = \"x\"\n  skipped = skipped + \"y\"\n  hits += 100\n}\nwhile({ hits += 1; false }) {\n  mutable skipped = [1]\n  skipped = [2]\n  hits += 100\n}\nprintln(hits)\nassertResult(1)(hits)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "skipped while build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "1\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_binary_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-binary-effect-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-binary-effect-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval sum = {\n  hits += 1\n  1\n} + 1\nval same = {\n  hits += 1\n  2\n} == 2\nval mixedSame = { hits += 1; 1 } == { hits += 1; 1.0 }\nval mixedDifferent = { hits += 1; 1 } != { hits += 1; 2.0F }\nval text = { hits += 1; \"a\" } + { hits += 1; \"b\" }\nval more = { hits += 1; 1 } + { hits += 1; 2 }\nval doubleSum = { hits += 1; 1.5 } + { hits += 1; 2.5 }\nval floatProduct = { hits += 1; 3.0F } * { hits += 1; 2.0F }\nval doubleGreater = { hits += 1; 4.0 } > { hits += 1; 3.0 }\nval skippedAnd = false && { hits += 1; true }\nval skippedOr = true || { hits += 1; false }\nval runAnd = true && { hits += 1; true }\nval runOr = false || { hits += 1; true }\nprintln(hits)\nprintln(text)\nprintln(more)\nprintln(doubleSum)\nprintln(floatProduct)\nprintln(doubleGreater)\nprintln(mixedSame)\nprintln(mixedDifferent)\nassertResult(2)(sum)\nassertResult(true)(same)\nassertResult(true)(mixedSame)\nassertResult(true)(mixedDifferent)\nassertResult(\"ab\")(text)\nassertResult(3)(more)\nassertResult(4.0)(doubleSum)\nassertResult(6.0F)(floatProduct)\nassertResult(true)(doubleGreater)\nassertResult(false)(skippedAnd)\nassertResult(true)(skippedOr)\nassertResult(true)(runAnd)\nassertResult(true)(runOr)\nassertResult(18)(hits)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "18\nab\n3\n4.0\n6.0\ntrue\ntrue\ntrue\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_call_argument_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-call-argument-effect-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-call-argument-effect-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval id = (x) => x\nval direct = ((x) => x)({\n  hits += 1\n  1\n})\nassertResult(1)({\n  hits += 1\n  1\n})\nval viaBinding = id({\n  hits += 1\n  1\n})\nprintln(hits)\nassertResult(1)(direct)\nassertResult(1)(viaBinding)\nassertResult(3)(hits)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "3\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dir_helper_argument_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let work_dir = std::env::temp_dir().join(format!("klassic-native-dir-effect-{unique}"));
    fs::create_dir(&work_dir).expect("temp work dir should be created");
    let source_path = work_dir.join("dir-effect.kl");
    let output_path = work_dir.join("dir-effect-bin");
    fs::write(
        &source_path,
        "mutable hits = 0\nDir#mkdir({ hits += 1; \"base\" })\nDir#mkdirs({ hits += 1; \"base/nested\" })\nFileOutput#write({ hits += 1; \"base/nested/a.txt\" }, { hits += 1; \"hello\" })\nval exists = Dir#exists({ hits += 1; \"base\" })\nval isDir = Dir#isDirectory({ hits += 1; \"base/nested\" })\nval isFile = Dir#isFile({ hits += 1; \"base/nested/a.txt\" })\nval listed = Dir#list({ hits += 1; \"base/nested\" })\nval listedFull = Dir#listFull({ hits += 1; \"base/nested\" })\nDir#copy({ hits += 1; \"base/nested/a.txt\" }, { hits += 1; \"base/nested/b.txt\" })\nDir#move({ hits += 1; \"base/nested/b.txt\" }, { hits += 1; \"base/nested/c.txt\" })\nFileOutput#delete({ hits += 1; \"base/nested/a.txt\" })\nFileOutput#delete({ hits += 1; \"base/nested/c.txt\" })\nDir#delete({ hits += 1; \"base/nested\" })\nDir#delete({ hits += 1; \"base\" })\nprintln(hits)\nassertResult(17)(hits)\nassert(exists)\nassert(isDir)\nassert(isFile)\nassertResult([\"a.txt\"])(listed)\nassert(size(listedFull) == 1)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .current_dir(&work_dir)
        .output()
        .expect("generated executable should run");

    let leftover_base = work_dir.join("base").exists();
    let _ = fs::remove_dir_all(&work_dir);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "17\n");
    assert!(run.stderr.is_empty());
    assert!(!leftover_base);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_records_and_field_access() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-record-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-record-{unique}"));
    fs::write(
        &source_path,
        "record Person {\n  name: String\n  age: Int\n  active: Boolean\n  scores: List<Int>\n}\nval p = #Person(\"Alice\", 30, true, [1, 2, 3])\nval point = record { x: 3, y: 4, label: \"P\" }\nprintln(p.name)\nprintln(\"age = \" + p.age)\nprintln(\"active = \" + p.active)\nprintln(\"scores = \" + p.scores)\nprintln(\"point = \" + point.label + \":\" + point.x)\nprintln(p)\nprintln(point)\nassertResult(\"Alice\")(p.name)\nassertResult(30)(p.age)\nassertResult(true)(p.active)\nassertResult([1, 2, 3])(p.scores)\nassertResult(record { x: 3, y: 4, label: \"P\" })(point)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "Alice\nage = 30\nactive = true\nscores = [1, 2, 3]\npoint = P:3\n#Person(Alice, 30, true, [1, 2, 3])\n#(3, 4, P)\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_record_fields() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-record-{unique}.kl"));
    let path_holder =
        std::env::temp_dir().join(format!("klassic-native-runtime-record-path-{unique}.txt"));
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-record-{unique}.txt"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-record-{unique}"));
    fs::write(
        &source_path,
        format!(
r##"record Box {{
  text: String
  lines: List<String>
  count: Int
  ok: Boolean
}}
record Outer {{
  box: #Box
  title: String
}}
val path = FileInput#all("{}")
val text = FileInput#all(path)
val literal = record {{
  text: text,
  lines: FileInput#lines(path),
  count: length(text),
  ok: text.contains("a"),
  label: "ok"
}}
val literalAgain = record {{
  text: FileInput#all(path),
  lines: FileInput#lines(path),
  count: length(FileInput#all(path)),
  ok: FileOutput#exists(path),
  label: "ok"
}}
val constructed = #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), FileOutput#exists(path))
val expectedLiteral = record {{
  text: "a\nb",
  lines: ["a", "b"],
  count: 3,
  ok: true,
  label: "ok"
}}
val expectedConstructed = #Box("a\nb", ["a", "b"], 3, true)
def textOf(b: #Box): String = b.text
def countOf(b) = b.count
def makeBox(): #Box = #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), FileOutput#exists(path))
def identityBox(b: #Box): #Box = b
def chooseBox(flag: Boolean, left: #Box, right: #Box): #Box = if(flag) left else right
def recursiveBox(n: Int): #Box = if(n <= 0) #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), true) else recursiveBox(n - 1)
val nested = #Outer(identityBox(constructed), "wrap")
val expectedNested = #Outer(expectedConstructed, "wrap")
val made = identityBox(makeBox())
val missing = path + ".missing"
val pickedExisting = if(FileOutput#exists(path)) #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), true) else #Box("missing", ["missing"], 7, false)
val pickedFallback = if(FileOutput#exists(missing)) #Box(FileInput#all(missing), FileInput#lines(missing), length(FileInput#all(missing)), true) else #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), false)
val chosen = chooseBox(FileOutput#exists(path), constructed, #Box("fallback", ["fallback"], 8, false))
val recursive = recursiveBox(2)
val mappedBox = Map#get(%["live": #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), true), "static": #Box("static", ["static"], 6, false)], "live")
val missingMappedBox = Map#get(%["live": #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), true)], "missing")
mutable listHits = 0
val headedBox = head([{{ listHits += 1; #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), true) }}, {{ listHits += 1; #Box("unused", ["unused"], 6, false) }}])
mutable mutableBox = #Box("start", ["start"], 5, false)
mutableBox = constructed
mutableBox = #Box("mut", ["mut"], 3, false)
val literalText = "literal=" + literal
val constructedText = "constructed=#{{constructed}}"
val againText = toString(literalAgain)
val nestedText = "nested=#{{nested}}"
println(literal.text)
println(join(literal.lines, "|"))
println(literal.count)
println("ok=" + literal.ok)
println(literal.label)
println(constructed.text)
println(join(constructed.lines, ":"))
println(constructed.count)
println("exists=" + constructed.ok)
println(constructed)
println(expectedLiteral == literal)
println(constructed == expectedConstructed)
println(literal == literalAgain)
println(literalText)
println(constructedText)
println(againText)
println(textOf(constructed))
println(countOf(constructed))
println(made.text)
println(made.count)
println(nested.box.text)
println(nested.box.count)
println(nested)
println(nestedText)
println(toString(nested))
println(nested == expectedNested)
println(pickedExisting)
println(pickedFallback)
println(pickedFallback.ok)
println(chosen)
println(recursive)
println(recursive.text)
println(mappedBox)
println(mappedBox.text)
println(missingMappedBox)
println(headedBox)
println(headedBox.text)
println(listHits)
println(mutableBox)
println(mutableBox.text)
println(mutableBox.ok)
assertResult("a\nb")(literal.text)
assertResult(["a", "b"])(literal.lines)
assertResult(3)(literal.count)
assertResult(true)(literal.ok)
assertResult("ok")(literal.label)
assertResult("a\nb")(constructed.text)
assertResult(["a", "b"])(constructed.lines)
assertResult(3)(constructed.count)
assertResult(true)(constructed.ok)
assertResult(expectedLiteral)(literal)
assertResult(expectedConstructed)(constructed)
assert(literal == literalAgain)
assertResult("literal=#(a\nb, [a, b], 3, true, ok)")(literalText)
assertResult("constructed=#Box(a\nb, [a, b], 3, true)")(constructedText)
assertResult("#(a\nb, [a, b], 3, true, ok)")(againText)
assertResult("a\nb")(textOf(constructed))
assertResult(3)(countOf(constructed))
assertResult("a\nb")(made.text)
assertResult(3)(made.count)
assertResult("a\nb")(nested.box.text)
assertResult(3)(nested.box.count)
assertResult(expectedNested)(nested)
assert(nested == expectedNested)
assertResult("nested=#Outer(#Box(a\nb, [a, b], 3, true), wrap)")(nestedText)
assertResult("#Outer(#Box(a\nb, [a, b], 3, true), wrap)")(toString(nested))
assertResult(expectedConstructed)(pickedExisting)
assertResult(#Box("a\nb", ["a", "b"], 3, false))(pickedFallback)
assertResult("a\nb")(pickedExisting.text)
assertResult(false)(pickedFallback.ok)
assertResult(expectedConstructed)(chosen)
assertResult(expectedConstructed)(recursive)
assertResult("a\nb")(recursive.text)
assertResult(expectedConstructed)(mappedBox)
assertResult("a\nb")(mappedBox.text)
assertResult(null)(missingMappedBox)
assertResult(expectedConstructed)(headedBox)
assertResult("a\nb")(headedBox.text)
assertResult(2)(listHits)
assertResult(#Box("mut", ["mut"], 3, false))(mutableBox)
assertResult("mut")(mutableBox.text)
assertResult(false)(mutableBox.ok)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime record build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "a\nb").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime record run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "a\nb\na|b\n3\nok=true\nok\na\nb\na:b\n3\nexists=true\n#Box(a\nb, [a, b], 3, true)\ntrue\ntrue\ntrue\nliteral=#(a\nb, [a, b], 3, true, ok)\nconstructed=#Box(a\nb, [a, b], 3, true)\n#(a\nb, [a, b], 3, true, ok)\na\nb\n3\na\nb\n3\na\nb\n3\n#Outer(#Box(a\nb, [a, b], 3, true), wrap)\nnested=#Outer(#Box(a\nb, [a, b], 3, true), wrap)\n#Outer(#Box(a\nb, [a, b], 3, true), wrap)\ntrue\n#Box(a\nb, [a, b], 3, true)\n#Box(a\nb, [a, b], 3, false)\nfalse\n#Box(a\nb, [a, b], 3, true)\n#Box(a\nb, [a, b], 3, true)\na\nb\n#Box(a\nb, [a, b], 3, true)\na\nb\nnull\n#Box(a\nb, [a, b], 3, true)\na\nb\n2\n#Box(mut, [mut], 3, false)\nmut\nfalse\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_literal_runtime_record_membership() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-record-membership-{unique}.kl"
    ));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-record-membership-path-{unique}.txt"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-record-membership-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-record-membership-{unique}"));
    fs::write(
        &source_path,
        format!(
            r##"record Box {{
  text: String
  lines: List<String>
  count: Int
  ok: Boolean
}}
record Summary {{
  text: String
  count: Int
  ok: Boolean
}}
val path = FileInput#all("{}")
val runtimeBox = #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), true)
mutable hits = 0
val listHit = [{{ hits += 1; #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), true) }}, {{ hits += 1; #Box("miss", ["miss"], 4, false) }}].contains(runtimeBox)
val listMiss = contains([{{ hits += 1; #Box("other", ["other"], 5, false) }}])(runtimeBox)
val setHit = Set#contains(%({{ hits += 1; #Box("miss", ["miss"], 4, false) }}, {{ hits += 1; #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), true) }}), runtimeBox)
val mapHit = Map#containsValue(%[{{ hits += 1; "live" }}: {{ hits += 1; #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), true) }}, {{ hits += 1; "other" }}: {{ hits += 1; #Box("other", ["other"], 5, false) }}], runtimeBox)
val listSize = size([{{ hits += 1; runtimeBox }}, {{ hits += 1; #Box("other", ["other"], 5, false) }}])
val listNonEmpty = isEmpty([{{ hits += 1; runtimeBox }}])
val mapSize = Map#size(%[{{ hits += 1; "live" }}: {{ hits += 1; runtimeBox }}, {{ hits += 1; "other" }}: {{ hits += 1; #Box("other", ["other"], 5, false) }}])
val mapNonEmpty = Map#isEmpty(%[{{ hits += 1; "live" }}: {{ hits += 1; runtimeBox }}])
val setNonEmpty = Set#isEmpty(%({{ hits += 1; runtimeBox }}))
val setRuntimeSize = Set#size(%({{ hits += 1; runtimeBox }}, {{ hits += 1; #Box("other", ["other"], 5, false) }}, {{ hits += 1; runtimeBox }}))
val keyHit = Map#containsKey(%[{{ hits += 1; FileInput#all(path) }}: {{ hits += 1; runtimeBox }}, {{ hits += 1; "other" }}: {{ hits += 1; runtimeBox }}], FileInput#all(path))
val keyMiss = %[{{ hits += 1; "other" }}: {{ hits += 1; runtimeBox }}].containsKey(FileInput#all(path))
val pickedRuntimeBox = Map#get(%[{{ hits += 1; FileInput#all(path) }}: {{ hits += 1; runtimeBox }}, {{ hits += 1; "other" }}: {{ hits += 1; #Box("other", ["other"], 5, false) }}], FileInput#all(path))
mutable foreachHits = 0
mutable foreachScore = 0
foreach(box in [{{ foreachHits += 1; #Box(FileInput#all(path), FileInput#lines(path), length(FileInput#all(path)), true) }}, {{ foreachHits += 1; #Box("other", ["other"], 5, false) }}]) {{
  println(foreachHits)
  println(box.count)
  foreachScore = foreachScore * 10 + box.count
}}
val foldedCount = foldLeft([{{ hits += 1; runtimeBox }}, {{ hits += 1; #Box("other", ["other"], 5, false) }}])(0)((acc, box) => acc + box.count)
val foldedSummary = foldLeft([{{ hits += 1; runtimeBox }}, {{ hits += 1; #Box("other", ["other"], 5, false) }}])(#Summary("", 0, true))((acc, box) => #Summary(acc.text + box.text, acc.count + box.count, acc.ok && box.ok))
val foldedText = foldLeft([{{ hits += 1; runtimeBox.text }}, {{ hits += 1; "!" }}])("")((acc, text) => acc + text)
val foldedLines = foldLeft([{{ hits += 1; substring(runtimeBox.text, 0, 1) }}, {{ hits += 1; "z" }}])([])((acc, text) => text #cons acc)
val literalTail = tail([{{ hits += 1; runtimeBox.text }}, {{ hits += 1; "tail" }}])
val mappedTexts = map([{{ hits += 1; substring(runtimeBox.text, 0, 1) }}, {{ hits += 1; "tail" }}])((text) => text + "!")
val joinedLiteral = join([{{ hits += 1; substring(runtimeBox.text, 0, 1) }}, {{ hits += 1; "tail" }}], "|")
val literalDisplay = toString([{{ hits += 1; runtimeBox.text }}, {{ hits += 1; "tail" }}])
println(listHit)
println(listMiss)
println(setHit)
println(mapHit)
println(listSize)
println(listNonEmpty)
println(mapSize)
println(mapNonEmpty)
println(setNonEmpty)
println(setRuntimeSize)
println(keyHit)
println(keyMiss)
println(hits)
println(foreachScore)
println(foldedCount)
println(foldedSummary.count)
println(foldedSummary.ok)
println(foldedSummary.text)
println(foldedText)
println(join(foldedLines, "|"))
println(join(literalTail, "|"))
println(join(mappedTexts, "|"))
println(joinedLiteral)
println(literalDisplay)
println([{{ hits += 1; substring(runtimeBox.text, 0, 1) }}, {{ hits += 1; "tail" }}])
println(pickedRuntimeBox.count)
println(pickedRuntimeBox.ok)
println(pickedRuntimeBox.text)
assert(listHit)
assert(!listMiss)
assert(setHit)
assert(mapHit)
assertResult(2)(listSize)
assert(!listNonEmpty)
assertResult(2)(mapSize)
assert(!mapNonEmpty)
assert(!setNonEmpty)
assert(keyHit)
assert(!keyMiss)
assertResult(50)(hits)
assertResult(2)(foreachHits)
assertResult(2)(setRuntimeSize)
assertResult(35)(foreachScore)
assertResult(8)(foldedCount)
assertResult(#Summary("a\nbother", 8, false))(foldedSummary)
assertResult("a\nb!")(foldedText)
assertResult(["z", "a"])(foldedLines)
assertResult(["tail"])(literalTail)
assertResult(["a!", "tail!"])(mappedTexts)
assertResult("a|tail")(joinedLiteral)
assertResult("[a\nb, tail]")(literalDisplay)
assertResult(#Box("a\nb", ["a", "b"], 3, true))(pickedRuntimeBox)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime record membership build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "a\nb").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime record membership run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "2\n3\n2\n5\ntrue\nfalse\ntrue\ntrue\n2\nfalse\n2\nfalse\nfalse\n2\ntrue\nfalse\n48\n35\n8\n8\nfalse\na\nbother\na\nb!\nz|a\ntail\na!|tail!\na|tail\n[a\nb, tail]\n[a, tail]\n3\ntrue\na\nb\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_literal_display_fragments() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-literal-display-{unique}.kl"
    ));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-literal-display-path-{unique}.txt"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-literal-display-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-literal-display-{unique}"));
    fs::write(
        &source_path,
        format!(
            r##"val path = FileInput#all("{}")
val runtime = FileInput#all(path)
mutable hits = 0
val listText = toString([{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}])
val listInterpolated = "list=#{{[{{ hits += 1; runtime }}, {{ hits += 1; \"tail\" }}]}}"
val listConcat = "list=" + [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}]
val mapText = toString(%[{{ hits += 1; runtime }}: {{ hits += 1; "value" }}])
val mapInterpolated = "map=#{{%[{{ hits += 1; runtime }}: {{ hits += 1; \"value\" }}]}}"
val mapConcat = "map=" + %[{{ hits += 1; runtime }}: {{ hits += 1; "value" }}]
val setText = toString(%({{ hits += 1; runtime }}, {{ hits += 1; "tail" }}, {{ hits += 1; runtime }}))
val setInterpolated = "set=#{{%({{ hits += 1; runtime }}, {{ hits += 1; \"tail\" }}, {{ hits += 1; runtime }})}}"
val setConcat = "set=" + %({{ hits += 1; runtime }}, {{ hits += 1; "tail" }}, {{ hits += 1; runtime }})
println(listText)
println(listInterpolated)
println(listConcat)
println(mapText)
println(mapInterpolated)
println(mapConcat)
println(setText)
println(setInterpolated)
println(setConcat)
println(%[{{ hits += 1; runtime }}: {{ hits += 1; "value" }}])
println(%({{ hits += 1; runtime }}, {{ hits += 1; "tail" }}, {{ hits += 1; runtime }}))
println(hits)
assertResult("[a\nb, tail]")(listText)
assertResult("list=[a\nb, tail]")(listInterpolated)
assertResult("list=[a\nb, tail]")(listConcat)
assertResult("%[a\nb: value]")(mapText)
assertResult("map=%[a\nb: value]")(mapInterpolated)
assertResult("map=%[a\nb: value]")(mapConcat)
assertResult("%(a\nb, tail)")(setText)
assertResult("set=%(a\nb, tail)")(setInterpolated)
assertResult("set=%(a\nb, tail)")(setConcat)
assertResult(26)(hits)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime literal display build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "a\nb").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime literal display run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[a\nb, tail]\nlist=[a\nb, tail]\nlist=[a\nb, tail]\n%[a\nb: value]\nmap=%[a\nb: value]\nmap=%[a\nb: value]\n%(a\nb, tail)\nset=%(a\nb, tail)\nset=%(a\nb, tail)\n%[a\nb: value]\n%(a\nb, tail)\n26\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_collection_literal_equality() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-literal-equality-{unique}.kl"
    ));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-literal-equality-path-{unique}.txt"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-literal-equality-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-literal-equality-{unique}"));
    fs::write(
	        &source_path,
	        format!(
	            r##"val path = FileInput#all("{}")
val runtime = FileInput#all(path)
val expectedList = ["a\nb", "tail"]
val expectedMap = %["a\nb": "value"]
val expectedSet = %("a\nb", "tail")
mutable hits = 0
val listOk = [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}] == ["a\nb", "tail"]
val listNe = [{{ hits += 1; runtime }}] != ["other"]
val mapOk = %[{{ hits += 1; runtime }}: {{ hits += 1; "value" }}] == %["a\nb": "value"]
val mapNe = %[{{ hits += 1; runtime }}: {{ hits += 1; "value" }}] != %["a\nb": "other"]
val setOk = %({{ hits += 1; runtime }}, {{ hits += 1; "tail" }}, {{ hits += 1; runtime }}) == %("a\nb", "tail")
val setNe = %({{ hits += 1; runtime }}) != %("other")
val listStaticLhsOk = expectedList == [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}]
val listStaticRhsOk = [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}] == expectedList
val mapStaticLhsOk = expectedMap == %[{{ hits += 1; runtime }}: {{ hits += 1; "value" }}]
val mapStaticRhsOk = %[{{ hits += 1; runtime }}: {{ hits += 1; "value" }}] == expectedMap
val setStaticLhsOk = expectedSet == %({{ hits += 1; runtime }}, {{ hits += 1; "tail" }}, {{ hits += 1; runtime }})
val setStaticRhsOk = %({{ hits += 1; runtime }}, {{ hits += 1; "tail" }}, {{ hits += 1; runtime }}) == expectedSet
println(listOk)
println(listNe)
println(mapOk)
println(mapNe)
println(setOk)
println(setNe)
println(listStaticLhsOk)
println(listStaticRhsOk)
println(mapStaticLhsOk)
println(mapStaticRhsOk)
println(setStaticLhsOk)
println(setStaticRhsOk)
println(hits)
assert(listOk)
assert(listNe)
assert(mapOk)
assert(mapNe)
assert(setOk)
assert(setNe)
assert(listStaticLhsOk)
assert(listStaticRhsOk)
assert(mapStaticLhsOk)
assert(mapStaticRhsOk)
assert(setStaticLhsOk)
assert(setStaticRhsOk)
assertResult(expectedList)([{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}])
assertResult(expectedMap)(%[{{ hits += 1; runtime }}: {{ hits += 1; "value" }}])
assertResult(expectedSet)(%({{ hits += 1; runtime }}, {{ hits += 1; "tail" }}, {{ hits += 1; runtime }}))
assertResult(32)(hits)
"##,
	            path_holder.display()
	        ),
	    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime literal equality build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "a\nb").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime literal equality run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\n25\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_list_literal_binding() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-list-binding-{unique}.kl"));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-path-{unique}.txt"
    ));
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-list-binding-{unique}.txt"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-list-binding-{unique}"));
    fs::write(
        &source_path,
        format!(
            r##"val path = FileInput#all("{}")
val runtime = FileInput#all(path)
mutable hits = 0
val xs = [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}]
val flags = [{{ hits += 1; runtime == "a\nb" }}, true]
val sizes = [{{ hits += 1; length(runtime) }}, 4]
println(xs)
println(toString(xs))
println("xs=" + xs)
println("xs=#{{xs}}")
println(flags)
println(sizes)
println(hits)
assertResult(["a\nb", "tail"])(xs)
assert(xs == ["a\nb", "tail"])
assert(["a\nb", "tail"] == xs)
assertResult([true, true])(flags)
assertResult([3, 4])(sizes)
assertResult(4)(hits)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime list binding build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "a\nb").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime list binding run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[a\nb, tail]\n[a\nb, tail]\nxs=[a\nb, tail]\nxs=[a\nb, tail]\n[true, true]\n[3, 4]\n4\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_list_literal_binding_helpers() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-helpers-{unique}.kl"
    ));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-helpers-path-{unique}.txt"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-helpers-{unique}.txt"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-helpers-{unique}"
    ));
    fs::write(
        &source_path,
        format!(
            r##"val path = FileInput#all("{}")
val runtime = FileInput#all(path)
mutable hits = 0
val xs = [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}]
val sizes = [{{ hits += 1; length(runtime) }}, 4]
val flags = [{{ hits += 1; runtime == "a\nb" }}, false]
val directSizeTail = tail([{{ hits += 1; length(runtime) }}, 4, 5])
println(head(xs))
println(xs.head())
println(head(tail(xs)))
println(tail(xs))
println(directSizeTail)
println(head(directSizeTail))
println(directSizeTail.contains(5))
println(size(xs))
println(xs.size())
println(isEmpty(xs))
println(tail(tail(xs)).isEmpty())
println(xs.contains("tail"))
println(contains(xs)(runtime))
println(sizes.contains(4))
println(contains(flags)(true))
foreach(x in xs) {{
  println("item=" + x)
}}
mutable total = 0
foreach(n in sizes) {{
  total += n
}}
println(total)
println(hits)
assertResult("a\nb")(head(xs))
assertResult("tail")(head(tail(xs)))
assertResult(["tail"])(tail(xs))
assertResult([4, 5])(directSizeTail)
assertResult(4)(head(directSizeTail))
assert(directSizeTail.contains(5))
assertResult(2)(size(xs))
assertResult(2)(xs.size())
assert(!isEmpty(xs))
assert(tail(tail(xs)).isEmpty())
assert(xs.contains("tail"))
assert(contains(xs)(runtime))
assert(sizes.contains(4))
assert(contains(flags)(true))
assertResult(7)(total)
assertResult(5)(hits)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime list binding helpers build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "a\nb").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime list binding helpers run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "a\nb\na\nb\ntail\n[tail]\n[4, 5]\n4\ntrue\n2\n2\nfalse\ntrue\ntrue\ntrue\ntrue\ntrue\nitem=a\nb\nitem=tail\n7\n5\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_list_literal_binding_map_and_fold() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-map-fold-{unique}.kl"
    ));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-map-fold-path-{unique}.txt"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-map-fold-{unique}.txt"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-map-fold-{unique}"
    ));
    fs::write(
        &source_path,
        format!(
            r##"val path = FileInput#all("{}")
val runtime = FileInput#all(path)
mutable hits = 0
val xs = [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}]
val sizes = [{{ hits += 1; length(runtime) }}, 4]
val mapped = xs.map((x) => x + "!")
val mappedViaFunction = map(xs)((x) => "[" + x + "]")
val bumped = sizes.map((n) => n + 1)
val directBumped = [{{ hits += 1; length(runtime) }}, 9].map((n) => n + 1)
val foldedText = xs.foldLeft("", (acc, x) => acc + "[" + x + "]")
val total = foldLeft(sizes)(0)((acc, n) => acc + n)
val allPositive = sizes.foldLeft(true, (acc, n) => acc && n > 0)
val reversed = xs.foldLeft([], (acc, x) => cons(x)(acc))
val reversedSizes = sizes.foldLeft([], (acc, n) => cons(n)(acc))
val seededSizes = sizes.foldLeft([99], (acc, n) => cons(n)(acc))
val directReversedSizes = [{{ hits += 1; length(runtime) }}, 9].foldLeft([], (acc, n) => cons(n)(acc))
println(mapped)
println(join(mapped, "|"))
println(mappedViaFunction)
println(join(mappedViaFunction, "|"))
println(bumped)
println(head(bumped))
println(directBumped)
println(foldedText)
println(total)
println(allPositive)
println(join(reversed, "|"))
println(reversedSizes)
println(seededSizes)
println(directReversedSizes)
println(hits)
assertResult(["ab!", "tail!"])(mapped)
assertResult(["[ab]", "[tail]"])(mappedViaFunction)
assertResult([3, 5])(bumped)
assertResult(3)(head(bumped))
assertResult([3, 10])(directBumped)
assertResult("[ab][tail]")(foldedText)
assertResult(6)(total)
assert(allPositive)
assertResult(["tail", "ab"])(reversed)
assertResult([4, 2])(reversedSizes)
assertResult([4, 2, 99])(seededSizes)
assertResult([9, 2])(directReversedSizes)
assertResult(5)(hits)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime list binding map/fold build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "ab").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime list binding map/fold run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[ab!, tail!]\nab!|tail!\n[[ab], [tail]]\n[ab]|[tail]\n[3, 5]\n3\n[3, 10]\n[ab][tail]\n6\ntrue\ntail|ab\n[4, 2]\n[4, 2, 99]\n[9, 2]\n5\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_list_literal_binding_join_and_write_lines() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-join-{unique}.kl"
    ));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-join-path-{unique}.txt"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-join-{unique}.txt"
    ));
    let delimiter_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-join-delimiter-{unique}.txt"
    ));
    let written_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-join-written-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-list-binding-join-{unique}"));
    fs::write(
        &source_path,
        format!(
            r##"val path = FileInput#all("{}")
val runtime = FileInput#all(path)
val delimiter = FileInput#all("{}")
mutable hits = 0
val xs = [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}]
val direct = join(xs, "|")
val method = xs.join("/")
val runtimeDelimited = join(xs, delimiter)
println(direct)
println(method)
println(runtimeDelimited)
FileOutput#writeLines("{}", xs)
val written = FileInput#all("{}")
println(written)
println(hits)
assertResult("ab|tail")(direct)
assertResult("ab/tail")(method)
assertResult("ab::tail")(runtimeDelimited)
assertResult("ab\ntail")(written)
assertResult(2)(hits)
"##,
            path_holder.display(),
            delimiter_path.display(),
            written_path.display(),
            written_path.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime list binding join build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "ab").expect("input should write after native build");
    fs::write(&delimiter_path, "::").expect("delimiter should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&delimiter_path);
    let _ = fs::remove_file(&written_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime list binding join run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "ab|tail\nab/tail\nab::tail\nab\ntail\n2\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_list_literal_binding_cons() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-cons-{unique}.kl"
    ));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-cons-path-{unique}.txt"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-binding-cons-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-list-binding-cons-{unique}"));
    fs::write(
        &source_path,
        format!(
            r##"val path = FileInput#all("{}")
val runtime = FileInput#all(path)
mutable hits = 0
val xs = [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}]
val ys = cons({{ hits += 1; "head" }})(xs)
val zs = cons({{ hits += 1; runtime + "!" }})(ys)
val direct = cons("direct")(xs)
val fromTail = cons("again")(tail(xs))
println(ys)
println(join(ys, "|"))
println(join(zs, "|"))
println(join(direct, "|"))
println(join(fromTail, "|"))
println(size(zs))
println(hits)
assertResult(["head", "ab", "tail"])(ys)
assertResult(["ab!", "head", "ab", "tail"])(zs)
assertResult(["direct", "ab", "tail"])(direct)
assertResult(["again", "tail"])(fromTail)
assertResult(4)(size(zs))
assertResult(4)(hits)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime list binding cons build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "ab").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime list binding cons run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[head, ab, tail]\nhead|ab|tail\nab!|head|ab|tail\ndirect|ab|tail\nagain|tail\n4\n4\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_list_literal_record_fields() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-record-field-{unique}.kl"
    ));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-record-field-path-{unique}.txt"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-record-field-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-list-record-field-{unique}"));
    fs::write(
        &source_path,
        format!(
            r##"record Bag {{
  items: List<String>
  label: String
}}
val path = FileInput#all("{}")
val runtime = FileInput#all(path)
mutable hits = 0
val xs = [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}]
val bag = #Bag(xs, "live")
val literal = record {{ items: xs, label: "live" }}
println(join(bag.items, "|"))
println(bag.items)
println(bag)
println(literal)
println(bag == #Bag(["ab", "tail"], "live"))
println(literal == record {{ items: ["ab", "tail"], label: "live" }})
println(hits)
assertResult(["ab", "tail"])(bag.items)
assertResult(#Bag(["ab", "tail"], "live"))(bag)
assertResult(record {{ items: ["ab", "tail"], label: "live" }})(literal)
assertResult(2)(hits)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime list record field build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "ab").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime list record field run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "ab|tail\n[ab, tail]\n#Bag([ab, tail], live)\n#([ab, tail], live)\ntrue\ntrue\n2\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_mutable_runtime_list_literal_binding() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-mutable-runtime-list-binding-{unique}.kl"
    ));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-mutable-runtime-list-binding-path-{unique}.txt"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-mutable-runtime-list-binding-{unique}.txt"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-mutable-runtime-list-binding-{unique}"
    ));
    fs::write(
        &source_path,
        format!(
            r##"val path = FileInput#all("{}")
val runtime = FileInput#all(path)
mutable hits = 0
mutable xs = [{{ hits += 1; runtime }}]
println(xs)
xs = cons({{ hits += 1; "head" }})(xs)
println(join(xs, "|"))
xs = cons({{ hits += 1; runtime + "!" }})(tail(xs))
println(join(xs, "|"))
xs = tail(xs)
println(xs)
println(hits)
assertResult(["head", "ab"])(cons("head")(tail(cons("skip")(xs))))
assertResult(["ab"])(xs)
assertResult(3)(hits)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "mutable runtime list binding build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "ab").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "mutable runtime list binding run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "[ab]\nhead|ab\nab!|ab\n[ab]\n3\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_list_literal_dynamic_if_results() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-dynamic-if-{unique}.kl"
    ));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-dynamic-if-path-{unique}.txt"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-dynamic-if-{unique}.txt"
    ));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-list-dynamic-if-{unique}"));
    fs::write(
        &source_path,
        format!(
            r##"record Bag {{
  items: List<String>
  label: String
}}
val path = FileInput#all("{}")
val runtime = FileInput#all(path)
mutable hits = 0
val chooseLeft = size(args()) == 0
val xs = [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}]
val dynamicKey = if(chooseLeft) "live" else "static"
val variedPrefix = Map#get(%[
  "live": [runtime],
  "static": [runtime, "tail"]
], dynamicKey)
val oneKey = if(chooseLeft) "live" else "live"
val onePrefix = Map#get(%[
  "live": [runtime],
  "static": [runtime, "tail"]
], oneKey)
val picked = if(chooseLeft) xs else cons("fallback")(tail(xs))
val prefixPicked = if(chooseLeft) variedPrefix else ["fallback", "tail"]
val staticThenPrefix = if(chooseLeft) ["fallback", "tail"] else onePrefix
val prefixBag = if(chooseLeft) #Bag(variedPrefix, "prefix") else #Bag(["fallback", "tail"], "fallback")
val staticPicked = if(chooseLeft) xs else ["static", "branch"]
val bag = if(chooseLeft) #Bag(xs, "left") else #Bag(cons("fallback")(tail(xs)), "right")
val staticBag = if(chooseLeft) #Bag(xs, "runtime") else #Bag(["static", "branch"], "static")
println(join(picked, "|"))
println(prefixPicked)
println(size(prefixPicked))
println(staticThenPrefix)
println(size(staticThenPrefix))
println(prefixBag)
println(size(prefixBag.items))
println(join(staticPicked, "|"))
println(join(bag.items, "|"))
println(bag)
println(staticBag)
println(hits)
if(chooseLeft) {{
  assertResult(["ab", "tail"])(picked)
  assertResult(["ab"])(prefixPicked)
  assertResult(1)(size(prefixPicked))
  assertResult(["fallback", "tail"])(staticThenPrefix)
  assertResult(2)(size(staticThenPrefix))
  assertResult(#Bag(["ab"], "prefix"))(prefixBag)
  assertResult(1)(size(prefixBag.items))
  assertResult(["ab", "tail"])(staticPicked)
  assertResult(#Bag(["ab", "tail"], "left"))(bag)
  assertResult(#Bag(["ab", "tail"], "runtime"))(staticBag)
}} else {{
  assertResult(["fallback", "tail"])(picked)
  assertResult(["fallback", "tail"])(prefixPicked)
  assertResult(2)(size(prefixPicked))
  assertResult(["ab"])(staticThenPrefix)
  assertResult(1)(size(staticThenPrefix))
  assertResult(#Bag(["fallback", "tail"], "fallback"))(prefixBag)
  assertResult(2)(size(prefixBag.items))
  assertResult(["static", "branch"])(staticPicked)
  assertResult(#Bag(["fallback", "tail"], "right"))(bag)
  assertResult(#Bag(["static", "branch"], "static"))(staticBag)
}}
assertResult(2)(hits)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime list dynamic if build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "ab").expect("input should write after native build");
    let left_run = Command::new(&output_path)
        .output()
        .expect("generated executable should run left branch");
    let right_run = Command::new(&output_path)
        .arg("right")
        .output()
        .expect("generated executable should run right branch");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        left_run.status.success(),
        "runtime list dynamic if left run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&left_run.stdout),
        String::from_utf8_lossy(&left_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&left_run.stdout),
        "ab|tail\n[ab]\n1\n[fallback, tail]\n2\n#Bag([ab], prefix)\n1\nab|tail\nab|tail\n#Bag([ab, tail], left)\n#Bag([ab, tail], runtime)\n2\n"
    );
    assert!(left_run.stderr.is_empty());

    assert!(
        right_run.status.success(),
        "runtime list dynamic if right run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&right_run.stdout),
        String::from_utf8_lossy(&right_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&right_run.stdout),
        "fallback|tail\n[fallback, tail]\n2\n[ab]\n1\n#Bag([fallback, tail], fallback)\n2\nstatic|branch\nfallback|tail\n#Bag([fallback, tail], right)\n#Bag([static, branch], static)\n2\n"
    );
    assert!(right_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_list_literal_map_get_results() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-list-map-get-{unique}.kl"));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-map-get-path-{unique}.txt"
    ));
    let input_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-list-map-get-{unique}.txt"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-list-map-get-{unique}"));
    fs::write(
        &source_path,
        format!(
            r##"record Bag {{
  items: List<String>
  label: String
}}
val path = FileInput#all("{}")
val runtime = FileInput#all(path)
val key = head(args())
mutable hits = 0
val xs = [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}]
val picked = Map#get(%["live": xs, "static": ["static", "branch"]], key)
val bag = Map#get(%[
  "live": #Bag(xs, "live"),
  "static": #Bag(["static", "branch"], "static")
], key)
val varied = Map#get(%[
  "live": [#Bag(xs, "live")],
  "static": [#Bag(["static"], "short"), #Bag(["branch"], "long")]
], key)
mutable seen = 0
foreach(item in varied) {{
  seen += 1
  println("seen " + item.label)
}}
val labels = map(varied)((item) => item.label)
val foldedItems = foldLeft(varied)(0)((acc, item) => acc + size(item.items))
val withHead = cons(#Bag(["head"], "head"))(varied)
println(join(picked, "|"))
println(picked)
println(join(bag.items, "|"))
println(bag)
println(varied)
println(size(varied))
println(head(varied).label)
println(join(head(varied).items, "|"))
println(tail(varied))
println(labels)
println(foldedItems)
println(varied.contains(#Bag(["branch"], "long")))
println(withHead)
println(withHead == withHead)
println(hits)
if(key == "live") {{
  assertResult(["ab", "tail"])(picked)
  assertResult(#Bag(["ab", "tail"], "live"))(bag)
  assertResult([#Bag(["ab", "tail"], "live")])(varied)
  assertResult(1)(size(varied))
  assertResult([])(tail(varied))
  assertResult(["live"])(labels)
  assertResult(2)(foldedItems)
  assertResult(false)(varied.contains(#Bag(["branch"], "long")))
  assertResult(1)(seen)
  assertResult([#Bag(["head"], "head"), #Bag(["ab", "tail"], "live")])(withHead)
}} else {{
  assertResult(["static", "branch"])(picked)
  assertResult(#Bag(["static", "branch"], "static"))(bag)
  assertResult([#Bag(["static"], "short"), #Bag(["branch"], "long")])(varied)
  assertResult(2)(size(varied))
  assertResult("long")(head(tail(varied)).label)
  assertResult(["short", "long"])(labels)
  assertResult(2)(foldedItems)
  assertResult(true)(varied.contains(#Bag(["branch"], "long")))
  assertResult(2)(seen)
  assertResult([#Bag(["head"], "head"), #Bag(["static"], "short"), #Bag(["branch"], "long")])(withHead)
}}
assertResult(2)(hits)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime list Map#get build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "ab").expect("input should write after native build");
    let live_run = Command::new(&output_path)
        .arg("live")
        .output()
        .expect("generated executable should run live branch");
    let static_run = Command::new(&output_path)
        .arg("static")
        .output()
        .expect("generated executable should run static branch");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        live_run.status.success(),
        "runtime list Map#get live run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&live_run.stdout),
        String::from_utf8_lossy(&live_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&live_run.stdout),
        "seen live\nab|tail\n[ab, tail]\nab|tail\n#Bag([ab, tail], live)\n[#Bag([ab, tail], live)]\n1\nlive\nab|tail\n[]\n[live]\n2\nfalse\n[#Bag([head], head), #Bag([ab, tail], live)]\ntrue\n2\n"
    );
    assert!(live_run.stderr.is_empty());

    assert!(
        static_run.status.success(),
        "runtime list Map#get static run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&static_run.stdout),
        String::from_utf8_lossy(&static_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&static_run.stdout),
        "seen short\nseen long\nstatic|branch\n[static, branch]\nstatic|branch\n#Bag([static, branch], static)\n[#Bag([static], short), #Bag([branch], long)]\n2\nshort\nstatic\n[#Bag([branch], long)]\n[short, long]\n2\ntrue\n[#Bag([head], head), #Bag([static], short), #Bag([branch], long)]\ntrue\n2\n"
    );
    assert!(static_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_variable_runtime_list_fold_left_list_accumulator() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-fold-list-acc-{unique}.kl"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-fold-list-acc-{unique}"
    ));
    fs::write(
        &source_path,
        r##"record Bag {
  items: List<String>
  label: String
}
val key = head(args())
val varied = Map#get(%[
  "live": [#Bag([key], "live")],
  "static": [#Bag(["static"], "short"), #Bag(["branch"], "long")]
], key)
val folded = foldLeft(varied)([#Bag(["seed"], "seed")])((acc, item) => cons(item)(acc))
println(folded)
if(key == "live") {
  assertResult([#Bag(["live"], "live"), #Bag(["seed"], "seed")])(folded)
} else {
  assertResult([#Bag(["branch"], "long"), #Bag(["static"], "short"), #Bag(["seed"], "seed")])(folded)
}
"##,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime-list foldLeft list accumulator build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let live_run = Command::new(&output_path)
        .arg("live")
        .output()
        .expect("generated executable should run live branch");
    let static_run = Command::new(&output_path)
        .arg("static")
        .output()
        .expect("generated executable should run static branch");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        live_run.status.success(),
        "runtime-list foldLeft list accumulator live run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&live_run.stdout),
        String::from_utf8_lossy(&live_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&live_run.stdout),
        "[#Bag([live], live), #Bag([seed], seed)]\n"
    );
    assert!(live_run.stderr.is_empty());

    assert!(
        static_run.status.success(),
        "runtime-list foldLeft list accumulator static run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&static_run.stdout),
        String::from_utf8_lossy(&static_run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&static_run.stdout),
        "[#Bag([branch], long), #Bag([static], short), #Bag([seed], seed)]\n"
    );
    assert!(static_run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_list_literal_function_line_list_interop() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-function-lines-{unique}.kl"
    ));
    let path_holder = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-function-lines-path-{unique}.txt"
    ));
    let input_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-function-lines-{unique}.txt"
    ));
    let output_path = std::env::temp_dir().join(format!(
        "klassic-native-runtime-list-function-lines-{unique}"
    ));
    fs::write(
        &source_path,
        format!(
            r##"record Bag {{
  items: List<String>
  label: String
}}
def lineCount(lines: List<String>): Int = lines.size()
def echoLines(lines: List<String>): List<String> = lines
val path = FileInput#all("{}")
val runtime = FileInput#all(path)
mutable hits = 0
val xs = [{{ hits += 1; runtime }}, {{ hits += 1; "tail" }}]
val selectedKey = if(length(runtime) == 2) "short" else "long"
val selected = Map#get(%[
  "short": [runtime],
  "long": [runtime, "tail"]
], selectedKey)
def makeLines(): List<String> = [runtime, "tail"]
def makeSelected(): List<String> = selected
def makeBag(): #Bag = #Bag([runtime, "tail"], "made")
def makeSelectedBag(): #Bag = #Bag(selected, "selected")
val echoed = echoLines(xs)
val selectedEchoed = echoLines(selected)
val made = makeLines()
val selectedMade = makeSelected()
val bag = makeBag()
val selectedBag = makeSelectedBag()
println(lineCount(xs))
println(join(echoed, "|"))
println(join(made, "|"))
println(bag)
println(selectedBag)
println(lineCount(selected))
println(join(selectedEchoed, "|"))
println(join(selectedMade, "|"))
println(hits)
assertResult(2)(lineCount(xs))
assertResult(1)(lineCount(selected))
assertResult(["ab", "tail"])(echoed)
assertResult(["ab"])(selectedEchoed)
assertResult(["ab", "tail"])(made)
assertResult(["ab"])(selectedMade)
assertResult(#Bag(["ab", "tail"], "made"))(bag)
assertResult(#Bag(["ab"], "selected"))(selectedBag)
assertResult(2)(hits)
"##,
            path_holder.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime list function line-list interop build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    fs::write(&path_holder, input_path.to_string_lossy().as_bytes())
        .expect("path holder should write after native build");
    fs::write(&input_path, "ab").expect("input should write after native build");
    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&path_holder);
    let _ = fs::remove_file(&input_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime list function line-list interop run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "2\nab|tail\nab|tail\n#Bag([ab, tail], made)\n#Bag([ab], selected)\n1\nab\nab\n2\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_literal_argument_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-literal-side-effects-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-literal-side-effects-{unique}"));
    fs::write(
        &source_path,
        "record User {\n  name: String\n  age: Int\n}\nmutable hits = 0\nval xs = [{ hits += 1; 1 }, { hits += 1; 2 }]\nval tags = %({ hits += 1; \"red\" }, { hits += 1; \"blue\" }, { hits += 1; \"red\" })\nval ages = %[{ hits += 1; \"Alice\" }: { hits += 1; 30 }]\nval rec = record { name: { hits += 1; \"Bob\" }, age: { hits += 1; 28 } }\nval user = #User({ hits += 1; \"Carol\" }, { hits += 1; 31 })\nprintln(hits)\nprintln(xs)\nprintln(tags)\nprintln(ages)\nprintln(rec.name)\nprintln(user.age)\nassertResult(11)(hits)\nassertResult([1, 2])(xs)\nassertResult(%(\"red\", \"blue\"))(tags)\nassertResult(%[\"Alice\": 30])(ages)\nassertResult(\"Bob\")(rec.name)\nassertResult(31)(user.age)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "11\n[1, 2]\n%(red, blue)\n%[Alice: 30]\nBob\n31\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_maps_and_sets() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-map-set-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-map-set-{unique}"));
    fs::write(
        &source_path,
        "val ages = %[\"Alice\": 30, \"Bob\": 28]\nval tags = %(\"red\", \"blue\", \"red\")\nval nested = %[\"xs\": [1, 2], \"empty\": []]\nprintln(ages)\nprintln(tags)\nprintln(nested)\nprintln(\"ages = \" + ages)\nprintln(\"tags = \" + tags)\nprintln(\"map size = \" + Map#size(ages))\nprintln(\"set size = \" + Set#size(tags))\nprintln(\"has Alice? \" + Map#containsKey(ages, \"Alice\"))\nprintln(\"has 30? \" + Map#containsValue(ages, 30))\nprintln(\"Alice = \" + Map#get(ages, \"Alice\"))\nprintln(\"missing = \" + Map#get(ages, \"Carol\"))\nprintln(\"has blue? \" + Set#contains(tags, \"blue\"))\nprintln(\"Bob = \" + ages.get(\"Bob\"))\nprintln(\"has red? \" + tags.contains(\"red\"))\nassertResult(%[\"Alice\": 30, \"Bob\": 28])(ages)\nassertResult(%(\"red\", \"blue\"))(tags)\nassertResult(%[\"xs\": [1, 2], \"empty\": []])(nested)\nassertResult(true)(Map#containsKey(ages, \"Alice\"))\nassertResult(false)(Map#containsKey(ages, \"Carol\"))\nassertResult(30)(Map#get(ages, \"Alice\"))\nassertResult(null)(Map#get(ages, \"Carol\"))\nassertResult(true)(Set#contains(tags, \"blue\"))\nassertResult(28)(ages.get(\"Bob\"))\nassertResult(true)(tags.contains(\"red\"))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "%[Alice: 30, Bob: 28]\n%(red, blue)\n%[xs: [1, 2], empty: []]\nages = %[Alice: 30, Bob: 28]\ntags = %(red, blue)\nmap size = 2\nset size = 2\nhas Alice? true\nhas 30? true\nAlice = 30\nmissing = null\nhas blue? true\nBob = 28\nhas red? true\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_map_get_null_checks() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-map-get-null-check-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-map-get-null-check-{unique}"));
    fs::write(
        &source_path,
        r#"val key = head(args())
val missing = key + "-missing"
val records = %[
  "hit": record { kind: "keyword", text: "if" },
  "other": record { kind: "operator", text: "+" }
]
mutable hits = 0
val hitRecordPresent = Map#get(records, key) != null
val missingRecordNull = records.get(missing) == null
val missingListNull = Map#get(%["hit": [1, 2], "other": [3]], missing) == null
val nullHit = Map#get(%["hit": null, "other": 2], key) == null
val nonNullHit = Map#get(%["hit": null, "other": 2], "other") != null
val symmetricMissingNull = null == Map#get(records, missing)
val methodPresent = null != records.get(key)
val callablePresent = Map#get(%["hit": length, "other": size], key) != null
val literalMissing = Map#get(%[
  { hits += 1; key }: { hits += 1; 1 },
  { hits += 1; "other" }: { hits += 1; null }
], missing) == null
val literalNullHit = Map#get(%[
  { hits += 1; key }: { hits += 1; 1 },
  { hits += 1; "other" }: { hits += 1; null }
], "other") == null
val branchText = if(Map#get(records, missing) == null) "missing" else "present"
val presentText = if(Map#get(records, key) != null) "present" else "missing"
mutable seen = 0
if(records.get(missing) == null) {
  seen += 1
}
if(Map#get(%["hit": null, "other": 2], "other") != null) {
  seen += 2
}
println(hitRecordPresent)
println(missingRecordNull)
println(missingListNull)
println(nullHit)
println(nonNullHit)
println(symmetricMissingNull)
println(methodPresent)
println(callablePresent)
println(literalMissing)
println(literalNullHit)
println(branchText)
println(presentText)
println(seen)
println(hits)
assert(hitRecordPresent)
assert(missingRecordNull)
assert(missingListNull)
assert(nullHit)
assert(nonNullHit)
assert(symmetricMissingNull)
assert(methodPresent)
assert(callablePresent)
assert(literalMissing)
assert(literalNullHit)
assertResult("missing")(branchText)
assertResult("present")(presentText)
assertResult(3)(seen)
assertResult(8)(hits)
"#,
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime Map#get null-check build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .arg("hit")
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime Map#get null-check run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\nmissing\npresent\n3\n8\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_string_collection_runtime_membership() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-membership-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-membership-{unique}"));
    fs::write(
        &source_path,
        "val keyword = head(args())\nval op = head(tail(args()))\nval kind = head(tail(tail(args())))\nval keywordLen = length(keyword)\nval keywordKnown = keyword == \"if\"\nval keywords = %(\"if\", \"else\", \"while\")\nval precedence = %[\"+\": 10, \"*\": 20]\nval kinds = %[\"if\": \"keyword\", \"+\": \"operator\"]\nval lengthKeys = %[2: \"short\", 5: \"wide\"]\nval lengthValues = %[\"short\": 2, \"kind\": 7]\nval boolKeys = %[true: \"known\", false: \"unknown\"]\nval boolValues = %[\"known\": true]\nval keywordTruthy = %[\"if\": true, \"while\": false]\nval nullValues = %[\"if\": null, \"while\": null]\nval unitValues = %[\"if\": (), \"while\": ()]\nval sameLists = %[\"if\": [1, 2], \"while\": [1, 2]]\nval listValues = %[\"if\": [1, 2], \"+\": [3, 4]]\nval tokenRecords = %[\"if\": record { kind: \"keyword\", text: \"if\", len: 2, ok: true }, \"+\": record { kind: \"operator\", text: \"+\", len: 1, ok: false }]\nval lengthRecords = %[2: record { kind: \"short\", text: \"two\", len: 2, ok: true }, 5: record { kind: \"wide\", text: \"five\", len: 5, ok: false }]\nval tokenSet = %(record { kind: \"keyword\", text: \"if\", len: 2, ok: true }, record { kind: \"operator\", text: \"+\", len: 1, ok: false })\nval tokenList = [record { kind: \"keyword\", text: \"if\", len: 2, ok: true }, record { kind: \"operator\", text: \"+\", len: 1, ok: false }]\nval token = Map#get(tokenRecords, keyword)\nval lengthRecord = Map#get(lengthRecords, keywordLen)\nval pickedList = Map#get(listValues, keyword)\nval pickedListByMethod = listValues.get(op)\nprintln(Set#contains(keywords, keyword))\nprintln(keywords.contains(keyword))\nprintln(keywords.contains(\"return\"))\nprintln(Map#containsKey(precedence, op))\nprintln(precedence.containsKey(\"/\"))\nprintln(Map#containsValue(kinds, kind))\nprintln(kinds.containsValue(kind))\nprintln([1, 2, 3].contains(keywordLen))\nprintln(%(1, 2, 3).contains(keywordLen))\nprintln(Map#containsKey(lengthKeys, keywordLen))\nprintln(Map#containsValue(lengthValues, keywordLen))\nprintln(Map#containsKey(boolKeys, keywordKnown))\nprintln(Map#containsValue(boolValues, keywordKnown))\nprintln(Map#get(precedence, op))\nprintln(kinds.get(op))\nprintln(Map#get(lengthKeys, keywordLen))\nprintln(Map#get(boolKeys, keywordKnown))\nprintln(Map#get(keywordTruthy, keyword))\nprintln(Map#get(nullValues, keyword))\nprintln(Map#get(nullValues, op))\nprintln(unitValues.get(keyword))\nprintln(Map#get(nullValues, keyword) == null)\nprintln(unitValues.get(keyword) == ())\nprintln(Map#get(sameLists, keyword))\nprintln(sameLists.containsValue(Map#get(sameLists, keyword)))\nprintln(pickedList)\nprintln(head(pickedList))\nprintln(pickedListByMethod)\nprintln(pickedListByMethod.contains(4))\nprintln(token)\nprintln(token.kind)\nprintln(lengthRecord.kind)\nprintln(Map#containsValue(tokenRecords, token))\nprintln(tokenRecords.containsValue(token))\nprintln(Set#contains(tokenSet, token))\nprintln(tokenSet.contains(token))\nprintln(tokenList.contains(token))\nprintln(tokenList.contains(lengthRecord))\nprintln(Map#get(lengthKeys, keyword))\nprintln(Map#get(kinds, keywordLen))\nassert(Set#contains(keywords, keyword))\nassert(keywords.contains(keyword))\nassert(!keywords.contains(\"return\"))\nassert(Map#containsKey(precedence, op))\nassert(!precedence.containsKey(\"/\"))\nassert(Map#containsValue(kinds, kind))\nassert(kinds.containsValue(kind))\nassert([1, 2, 3].contains(keywordLen))\nassert(%(1, 2, 3).contains(keywordLen))\nassert(Map#containsKey(lengthKeys, keywordLen))\nassert(Map#containsValue(lengthValues, keywordLen))\nassert(Map#containsKey(boolKeys, keywordKnown))\nassert(Map#containsValue(boolValues, keywordKnown))\nassertResult(10)(Map#get(precedence, op))\nassertResult(\"operator\")(kinds.get(op))\nassertResult(\"short\")(Map#get(lengthKeys, keywordLen))\nassertResult(\"known\")(Map#get(boolKeys, keywordKnown))\nassert(Map#get(keywordTruthy, keyword))\nassertResult(null)(Map#get(nullValues, keyword))\nassertResult(null)(Map#get(nullValues, op))\nassertResult(())(unitValues.get(keyword))\nassert(Map#get(nullValues, keyword) == null)\nassert(unitValues.get(keyword) == ())\nassertResult([1, 2])(Map#get(sameLists, keyword))\nassert(sameLists.containsValue(Map#get(sameLists, keyword)))\nassertResult([1, 2])(pickedList)\nassertResult(1)(head(pickedList))\nassertResult([3, 4])(pickedListByMethod)\nassert(pickedListByMethod.contains(4))\nassertResult(record { kind: \"keyword\", text: \"if\", len: 2, ok: true })(token)\nassertResult(\"keyword\")(token.kind)\nassertResult(\"short\")(lengthRecord.kind)\nassert(Map#containsValue(tokenRecords, token))\nassert(tokenRecords.containsValue(token))\nassert(Set#contains(tokenSet, token))\nassert(tokenSet.contains(token))\nassert(tokenList.contains(token))\nassert(!tokenList.contains(lengthRecord))\nassertResult(null)(Map#get(lengthKeys, keyword))\nassertResult(null)(Map#get(kinds, keywordLen))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime membership build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .arg("if")
        .arg("+")
        .arg("keyword")
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime membership run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "true\ntrue\nfalse\ntrue\nfalse\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\n10\noperator\nshort\nknown\ntrue\nnull\nnull\n()\ntrue\ntrue\n[1, 2]\ntrue\n[1, 2]\n1\n[3, 4]\ntrue\n#(keyword, if, 2, true)\nkeyword\nshort\ntrue\ntrue\ntrue\ntrue\ntrue\nfalse\nnull\nnull\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_module_import_aliases() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-import-alias-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-import-alias-{unique}"));
    fs::write(
        &source_path,
        "import Map as M\nimport Map.{size}\nimport Set as S\nimport Set.{contains}\nimport FileInput as FI\nval aliasedSize = M#size\nval readAll = FI#readAll\nprintln(M#size(%[\"a\": 1]))\nprintln(size(%[\"b\": 2, \"c\": 3]))\nprintln(aliasedSize(%[\"d\": 4]))\nprintln(S#size(%(\"x\")))\nprintln(contains(%(\"x\"))(\"x\"))\nprintln(readAll(\"src/test/resources/hello.txt\"))\nassertResult(1)(M#size(%[\"a\": 1]))\nassertResult(2)(size(%[\"b\": 2, \"c\": 3]))\nassertResult(1)(aliasedSize(%[\"d\": 4]))\nassertResult(1)(S#size(%(\"x\")))\nassert(contains(%(\"x\"))(\"x\"))\nassertResult(\"Hello, World!\")(readAll(\"src/test/resources/hello.txt\"))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "1\n2\n1\n1\ntrue\nHello, World!\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_helper_argument_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-helper-arg-effect-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-helper-arg-effect-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval tags = %(\"red\", \"blue\")\nval ages = %[\"Alice\": 30]\nval textOk = \"abc\".contains({ hits += 1; \"a\" })\nval setOk = tags.contains({ hits += 1; \"red\" })\nval moduleSetOk = Set#contains(tags, { hits += 1; \"blue\" })\nval age = Map#get(ages, { hits += 1; \"Alice\" })\nval hasAge = Map#containsValue(ages, { hits += 1; 30 })\nval piece = substring({ hits += 1; \"abcd\" }, { hits += 1; 1 }, { hits += 1; 3 })\nval shouted = replace({ hits += 1; \"aba\" }, { hits += 1; \"a\" }, { hits += 1; \"x\" })\nval repeated = repeat({ hits += 1; \"ha\" }, { hits += 1; 2 })\nval first = at({ hits += 1; \"xy\" }, { hits += 1; 0 })\nval matched = matches({ hits += 1; \"123\" }, { hits += 1; \"[0-9]+\" })\nval splitParts = split({ hits += 1; \"a,b\" }, { hits += 1; \",\" })\nval joined = join(splitParts, { hits += 1; \"-\" })\nval trimmed = trim({ hits += 1; \" ok \" })\nval lower = toLowerCase({ hits += 1; \"AB\" })\nval starts = startsWith({ hits += 1; \"abc\" }, { hits += 1; \"a\" })\nval idx = indexOf({ hits += 1; \"abc\" }, { hits += 1; \"b\" })\nval len = length({ hits += 1; \"hé\" })\nprintln(hits)\nassertResult(27)(hits)\nassert(textOk)\nassert(setOk)\nassert(moduleSetOk)\nassertResult(30)(age)\nassert(hasAge)\nassertResult(\"bc\")(piece)\nassertResult(\"xba\")(shouted)\nassertResult(\"haha\")(repeated)\nassertResult(\"x\")(first)\nassert(matched)\nassertResult([\"a\", \"b\"])(splitParts)\nassertResult(\"a-b\")(joined)\nassertResult(\"ok\")(trimmed)\nassertResult(\"ab\")(lower)\nassert(starts)\nassertResult(1)(idx)\nassertResult(2)(len)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "27\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_value_equality() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-equality-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-equality-{unique}"));
    fs::write(
        &source_path,
        "val parts = split(\"a,b\", \",\")\nval genericInts = map(parts)((x) => 1)\nval rec = record { name: \"Alice\", age: 30 }\nval map = %[\"a\": [1, 2], \"b\": [3]]\nval set = %(\"red\", \"blue\", \"red\")\nval unitText = \"unit=\" + ()\nmutable flag = true\nprintln(())\nprintln(unitText)\nprintln(\"unit eq = \" + (() == ()))\nprintln(\"string eq = \" + (\"a\" == \"a\"))\nprintln(\"list ne = \" + (parts != [\"a\", \"c\"]))\nprintln(\"record eq = \" + (rec == record { name: \"Alice\", age: 30 }))\nprintln(\"map eq = \" + (map == %[\"a\": [1, 2], \"b\": [3]]))\nprintln(\"set eq = \" + (set == %(\"red\", \"blue\")))\nprintln(\"null ne = \" + (null != Map#get(%[\"x\": 1], \"missing\")))\nprintln(\"flag eq = \" + (flag == true))\nassertResult(())(())\nassertResult(\"unit=()\")(unitText)\nassert(() == ())\nassert(\"a\" == \"a\")\nassert(parts == [\"a\", \"b\"])\nassertResult([1, 1])(genericInts)\nassert(genericInts == [1, 1])\nassert(rec != record { name: \"Alice\", age: 31 })\nassert(map == %[\"a\": [1, 2], \"b\": [3]])\nassert(set == %(\"red\", \"blue\"))\nassert(null == Map#get(%[\"x\": 1], \"missing\"))\nassert(flag == true)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "()\nunit=()\nunit eq = true\nstring eq = true\nlist ne = true\nrecord eq = true\nmap eq = true\nset eq = true\nnull ne = false\nflag eq = true\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_user_visible_function_equality() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-function-equality-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-function-equality-{unique}"));
    fs::write(
        &source_path,
        "val f = (x) => x\nprintln(f == f)\nprintln([f] == [f])\nprintln(println == println)\nprintln([println] == [println])\nassertResult(false)(f == f)\nassertResult(false)([f] == [f])\nassertResult(false)(println == println)\nassertResult(false)([println] == [println])\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "false\nfalse\nfalse\nfalse\n"
    );
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_todo_runtime_error() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-todo-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-todo-{unique}"));
    fs::write(&source_path, "ToDo()\n").expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(!run.status.success());
    assert!(run.stdout.is_empty());
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        format!("{}:1:1: not implemented yet\n", source_path.display())
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_assertion_runtime_errors() {
    let cases = [
        ("assert(false)\n", 1, 1, "assertion failed"),
        (
            "assertResult(1)(2)\n",
            1,
            1,
            "assertResult failed: expected 1 but got 2",
        ),
        (
            "mutable x = 1\nassertResult(x)(2)\n",
            2,
            1,
            "assertResult failed: expected 1 but got 2",
        ),
        (
            "assertResult([1])([2])\n",
            1,
            1,
            "assertResult failed: expected [1] but got [2]",
        ),
        (
            "val flag = size(CommandLine#args()) == 0\nval pick = if(flag) toLowerCase else toUpperCase\nassertResult(pick)(toUpperCase)\n",
            3,
            1,
            "assertResult failed: expected <builtin:toLowerCase> but got <builtin:toUpperCase>",
        ),
        ("head([])\n", 1, 1, "head expects a non-empty list"),
        (
            "val h = head\nh([])\n",
            2,
            1,
            "head expects a non-empty list",
        ),
        (
            "at(\"abc\", -1)\n",
            1,
            1,
            "at expects a non-negative integer index",
        ),
        (
            "substring(\"abc\", -1, 2)\n",
            1,
            1,
            "substring expects a non-negative integer index",
        ),
        (
            "repeat(\"a\", -1)\n",
            1,
            1,
            "repeat expects a non-negative integer index",
        ),
        (
            "sleep(-1)\n",
            1,
            1,
            "sleep expects a non-negative integer index",
        ),
        (
            "mutable millis = -1\nsleep(millis)\n",
            2,
            1,
            "sleep expects a non-negative integer index",
        ),
    ];
    for (index, (source, line, column, expected_message)) in cases.iter().enumerate() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path =
            std::env::temp_dir().join(format!("klassic-native-assert-error-{index}-{unique}.kl"));
        let output_path =
            std::env::temp_dir().join(format!("klassic-native-assert-error-{index}-{unique}"));
        fs::write(&source_path, source).expect("source should write");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");

        assert!(
            build.status.success(),
            "assertion error build failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );
        assert!(build.stdout.is_empty());
        assert!(build.stderr.is_empty());

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);

        assert!(!run.status.success());
        assert!(run.stdout.is_empty());
        assert_eq!(
            String::from_utf8_lossy(&run.stderr),
            format!(
                "{}:{line}:{column}: {expected_message}\n",
                source_path.display()
            )
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_file_output_runtime_errors() {
    let cases = [
        ("FileOutput#write", "FileOutput#write failed to open file"),
        ("FileOutput#append", "FileOutput#append failed to open file"),
        (
            "FileOutput#writeLines",
            "FileOutput#writeLines failed to open file",
        ),
    ];
    for (index, (helper, expected_message)) in cases.iter().enumerate() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path =
            std::env::temp_dir().join(format!("klassic-native-file-error-{index}-{unique}.kl"));
        let output_path =
            std::env::temp_dir().join(format!("klassic-native-file-error-{index}-{unique}"));
        let missing_parent =
            std::env::temp_dir().join(format!("klassic-native-missing-parent-{index}-{unique}"));
        let target_path = missing_parent.join("out.txt");
        let source = if *helper == "FileOutput#writeLines" {
            format!("{helper}(\"{}\", [\"x\"])\n", target_path.display())
        } else {
            format!("{helper}(\"{}\", \"x\")\n", target_path.display())
        };
        fs::write(&source_path, source).expect("source should write");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");

        assert!(
            build.status.success(),
            "file error build failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );
        assert!(build.stdout.is_empty());
        assert!(build.stderr.is_empty());

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);

        assert!(!run.status.success());
        assert!(run.stdout.is_empty());
        assert_eq!(
            String::from_utf8_lossy(&run.stderr),
            format!("{}:1:1: {expected_message}\n", source_path.display())
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_dir_runtime_errors() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let existing_dir = std::env::temp_dir().join(format!("klassic-native-existing-dir-{unique}"));
    let nonempty_dir = std::env::temp_dir().join(format!("klassic-native-nonempty-dir-{unique}"));
    let nonempty_file = nonempty_dir.join("inside.txt");
    let missing_source = std::env::temp_dir().join(format!("klassic-native-missing-move-{unique}"));
    let move_target = std::env::temp_dir().join(format!("klassic-native-move-target-{unique}"));
    let missing_copy_source =
        std::env::temp_dir().join(format!("klassic-native-missing-copy-{unique}"));
    let copy_target = std::env::temp_dir().join(format!("klassic-native-copy-target-{unique}"));
    fs::create_dir_all(&existing_dir).expect("existing dir should be created");

    let cases = [
        (
            format!("Dir#mkdir(\"{}\")\n", existing_dir.display()),
            1,
            "Dir#mkdir failed to create directory",
        ),
        (
            format!(
                "Dir#mkdirs(\"{}\")\nFileOutput#write(\"{}\", \"x\")\nDir#delete(\"{}\")\n",
                nonempty_dir.display(),
                nonempty_file.display(),
                nonempty_dir.display()
            ),
            3,
            "Dir#delete failed to delete directory",
        ),
        (
            format!(
                "Dir#move(\"{}\", \"{}\")\n",
                missing_source.display(),
                move_target.display()
            ),
            1,
            "Dir#move failed to move path",
        ),
        (
            format!(
                "Dir#copy(\"{}\", \"{}\")\n",
                missing_copy_source.display(),
                copy_target.display()
            ),
            1,
            "Dir#copy failed to open source file",
        ),
    ];

    for (index, (source, line, expected_message)) in cases.iter().enumerate() {
        let source_path =
            std::env::temp_dir().join(format!("klassic-native-dir-error-{index}-{unique}.kl"));
        let output_path =
            std::env::temp_dir().join(format!("klassic-native-dir-error-{index}-{unique}"));
        fs::write(&source_path, source).expect("source should write");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                output_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("klassic build should run");

        assert!(
            build.status.success(),
            "dir error build failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );
        assert!(build.stdout.is_empty());
        assert!(build.stderr.is_empty());

        let run = Command::new(&output_path)
            .output()
            .expect("generated executable should run");

        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&output_path);

        assert!(!run.status.success());
        assert!(run.stdout.is_empty());
        assert_eq!(
            String::from_utf8_lossy(&run.stderr),
            format!("{}:{line}:1: {expected_message}\n", source_path.display())
        );
    }

    let _ = fs::remove_dir_all(&existing_dir);
    let _ = fs::remove_dir_all(&nonempty_dir);
    let _ = fs::remove_file(&move_target);
    let _ = fs::remove_file(&copy_target);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_dir_copy() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_file = std::env::temp_dir().join(format!("klassic-native-copy-source-{unique}.txt"));
    let target_file = std::env::temp_dir().join(format!("klassic-native-copy-target-{unique}.txt"));
    let source_path = std::env::temp_dir().join(format!("klassic-native-runtime-copy-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-copy-{unique}"));
    fs::write(&source_file, "copy me").expect("source file should write");
    fs::write(
        &source_path,
        format!(
            "Dir#copy(\"{}\", \"{}\")\nprintln(\"copied\")\n",
            source_file.display(),
            target_file.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime copy build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let copied = fs::read_to_string(&target_file).unwrap_or_default();
    let _ = fs::remove_file(&source_file);
    let _ = fs::remove_file(&target_file);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime copy run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "copied\n");
    assert!(run.stderr.is_empty());
    assert_eq!(copied, "copy me");
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_dir_move() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_file = std::env::temp_dir().join(format!("klassic-native-move-source-{unique}.txt"));
    let target_file = std::env::temp_dir().join(format!("klassic-native-move-target-{unique}.txt"));
    let source_path = std::env::temp_dir().join(format!("klassic-native-runtime-move-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-move-{unique}"));
    fs::write(&source_file, "move me").expect("source file should write");
    fs::write(
        &source_path,
        format!(
            "Dir#move(\"{}\", \"{}\")\nprintln(\"moved\")\n",
            source_file.display(),
            target_file.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "runtime move build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let moved = fs::read_to_string(&target_file).unwrap_or_default();
    let source_still_exists = source_file.exists();
    let _ = fs::remove_file(&source_file);
    let _ = fs::remove_file(&target_file);
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(
        run.status.success(),
        "runtime move run failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "moved\n");
    assert!(run.stderr.is_empty());
    assert!(!source_still_exists);
    assert_eq!(moved, "move me");
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_file_delete_and_mkdirs_keep_evaluator_success_cases() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let existing_dir =
        std::env::temp_dir().join(format!("klassic-native-existing-mkdirs-{unique}"));
    let missing_file = existing_dir.join("missing.txt");
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-allowed-file-dir-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-allowed-file-dir-{unique}"));
    fs::create_dir_all(&existing_dir).expect("existing dir should be created");
    fs::write(
        &source_path,
        format!(
            "Dir#mkdirs(\"{}\")\nFileOutput#delete(\"{}\")\nprintln(\"ok\")\n",
            existing_dir.display(),
            missing_file.display()
        ),
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(
        build.status.success(),
        "allowed file/dir build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);
    let _ = fs::remove_dir_all(&existing_dir);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "ok\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_equality_side_effects() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-equality-side-effects-{unique}.kl"));
    let output_path =
        std::env::temp_dir().join(format!("klassic-native-equality-side-effects-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nassertResult({ hits += 1; [1, 2] })({ hits += 1; [1, 2] })\nassert({ hits += 1; \"a\" } == { hits += 1; \"a\" })\nassert({ hits += 1; record { x: 1 } } != { hits += 1; record { x: 2 } })\nassert({ hits += 1; %[\"a\": 1] } == { hits += 1; %[\"a\": 1] })\nassert({ hits += 1; %(\"x\") } == { hits += 1; %(\"x\") })\nprintln(hits)\nassertResult(10)(hits)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "10\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_static_sleep() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-sleep-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-sleep-{unique}"));
    fs::write(
        &source_path,
        "println(\"before\")\nsleep(0)\nprintln(\"after\")\nassertResult(())(sleep(0))\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "before\nafter\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_runtime_sleep_argument() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path =
        std::env::temp_dir().join(format!("klassic-native-runtime-sleep-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-runtime-sleep-{unique}"));
    fs::write(
        &source_path,
        "mutable hits = 0\nval ms = stopwatch( => 1)\nsleep(ms)\nsleep({ hits += 1; 0 })\nprintln(hits)\nassertResult(1)(hits)\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "1\n");
    assert!(run.stderr.is_empty());
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn builds_native_executable_for_stopwatch() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-native-stopwatch-{unique}.kl"));
    let output_path = std::env::temp_dir().join(format!("klassic-native-stopwatch-{unique}"));
    fs::write(
        &source_path,
        "val elapsed = stopwatch( => {\n  sleep(0)\n  42\n})\nassert(elapsed >= 0)\nprintln(\"elapsed ok\")\n",
    )
    .expect("source should write");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            output_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("klassic build should run");

    assert!(build.status.success());
    assert!(build.stdout.is_empty());
    assert!(build.stderr.is_empty());

    let run = Command::new(&output_path)
        .output()
        .expect("generated executable should run");

    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&output_path);

    assert!(run.status.success());
    assert_eq!(String::from_utf8_lossy(&run.stdout), "elapsed ok\n");
    assert!(run.stderr.is_empty());
}

#[test]
fn evaluates_expression_with_nested_comments() {
    let output = Command::new(klassic_bin())
        .args(["-e", "1 + /* outer /* inner */ outer */ 2"])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "3\n");
}

#[test]
fn executes_file_argument_without_printing_the_return_value() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let path = std::env::temp_dir().join(format!("klassic-cli-{unique}.kl"));
    fs::write(&path, "1 + 2").expect("temp file should be writable");

    let output = Command::new(klassic_bin())
        .arg(path.as_os_str())
        .output()
        .expect("binary should run");

    let _ = fs::remove_file(&path);

    assert!(output.status.success());
    assert!(output.stdout.is_empty());
    assert!(output.stderr.is_empty());
}

#[test]
fn executes_dash_f_file_argument_without_printing_the_return_value() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let path = std::env::temp_dir().join(format!("klassic-cli-dash-f-{unique}.kl"));
    fs::write(&path, "1 + 2").expect("temp file should be writable");

    let output = Command::new(klassic_bin())
        .args(["-f", path.to_string_lossy().as_ref()])
        .output()
        .expect("binary should run");

    let _ = fs::remove_file(&path);

    assert!(output.status.success());
    assert!(output.stdout.is_empty());
    assert!(output.stderr.is_empty());
}

#[test]
fn executes_multiline_bindings_and_control_flow() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let path = std::env::temp_dir().join(format!("klassic-cli-control-{unique}.kl"));
    fs::write(
        &path,
        "mutable i = 1\nwhile(i < 4) {\n  i += 1\n}\nif(i == 4) {\n  println(\"done\")\n}\n",
    )
    .expect("temp file should be writable");

    let output = Command::new(klassic_bin())
        .arg(path.as_os_str())
        .output()
        .expect("binary should run");

    let _ = fs::remove_file(&path);

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "done\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn evaluates_module_imports_via_cli() {
    let output = Command::new(klassic_bin())
        .args(["-e", "import Map\nsize(%[\"A\": 1, \"B\": 2])"])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "2\n");
    assert!(output.stderr.is_empty());

    let excluded = Command::new(klassic_bin())
        .args([
            "-e",
            "import Map.{size, get => _}\nsize(%[\"A\": 1, \"B\": 2])",
        ])
        .output()
        .expect("binary should run");

    assert!(excluded.status.success());
    assert_eq!(String::from_utf8_lossy(&excluded.stdout), "2\n");
    assert!(excluded.stderr.is_empty());
}

#[test]
fn evaluates_placeholder_and_cleanup_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "mutable i = 0\nwhile(i < 3) {\n  i += 1\n} cleanup {\n  i += 10\n}\nval xs = [1 2 3]\nprintln(map(xs)(_ + 1))\ni",
        ])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "[2, 3, 4]\n13\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn evaluates_list_map_and_reduce_syntax_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "println([1 2 3] map x => x + 1)\n[1 2 3 4] reduce 0 => r + e",
        ])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "[2, 3, 4]\n10\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn evaluates_typeclass_constrained_functions_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ntypeclass Eq<'a> where {\n  eq: ('a, 'a) => Boolean\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ninstance Eq<Int> where {\n  def eq(x: Int, y: Int): Boolean = x == y\n}\ndef display<'a>(x: 'a): String where Show<'a> = show(x)\ndef show_if_equal<'a>(x: 'a, y: 'a): String where (Show<'a>, Eq<'a>) = if(eq(x, y)) show(x) else show(y)\nprintln(display(42))\nshow_if_equal(1, 2)",
        ])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "Int(42)\nInt(2)\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn evaluates_fresh_constrained_instantiation_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int(\" + x + \")\"\n}\ninstance Show<String> where {\n  def show(x: String): String = \"Str(\" + x + \")\"\n}\nrecord Person {\n  name: String\n  age: Int\n}\ninstance Show<Person> where {\n  def show(p: Person): String = \"Person(\" + p.name + \")\"\n}\ndef display<'a>(x: 'a): String where Show<'a> = show(x)\nprintln(display(42))\nprintln(display(\"hello\"))\ndisplay(#Person(\"Alice\", 30))",
        ])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "Int(42)\nStr(hello)\nPerson(Alice)\n"
    );
    assert!(output.stderr.is_empty());
}

#[test]
fn evaluates_higher_kinded_constrained_functions_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "typeclass Functor<'f: * => *> where {\n  map: (('a) => 'b, 'f<'a>) => 'f<'b>\n}\ninstance Functor<List> where {\n  def map(f: ('a) => 'b, xs: List<'a>): List<'b> = xs.map(f)\n}\ndef liftTwice<'f, 'a, 'b, 'c>(xs: 'f<'a>, f: ('a) => 'b, g: ('b) => 'c): 'f<'c> where Functor<'f> = map(g, map(f, xs))\nliftTwice([1, 2, 3], (x) => x + 1, (y) => y * 2)",
        ])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "[4, 6, 8]\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn evaluates_forward_proof_references_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "theorem earlier(x: Int): { later(x) } = assert(true)\naxiom later(y: Int): { true }\nprintln(earlier(1))",
        ])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "true\n()\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn rejects_mismatched_proof_terms_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "axiom left(): { true }\naxiom right(): { false }\ntheorem bad(): { left } = right",
        ])
        .output()
        .expect("binary should run");

    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr)
            .contains("proof body does not establish declared proposition")
    );
}

#[test]
fn evaluates_higher_kinded_monad_constraints_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "typeclass Monad<'m: * => *> where {\n  bind: ('m<'a>, ('a) => 'm<'b>) => 'm<'b>;\n  unit: ('a) => 'm<'a>\n}\ninstance Monad<List> where {\n  def bind(xs: List<'a>, f: ('a) => List<'b>): List<'b> = f(head(xs))\n  def unit(x: 'a): List<'a> = [x]\n}\ndef pairWithNext<'m>(xs: 'm<Int>): 'm<Int> where Monad<'m> = bind(xs, (x) => unit(x + 1))\npairWithNext([1, 2, 3])",
        ])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "[2]\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn shares_mutable_thread_captures_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "mutable counter = 0\nthread(() => {\n  sleep(1)\n  counter = counter + 1\n})\nsleep(10)\ncounter",
        ])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "1\n");
    assert!(output.stderr.is_empty());
}

#[test]
fn warns_for_trusted_proofs_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "--warn-trust",
            "-e",
            "trust theorem foo(): { true } = assert(true)",
        ])
        .output()
        .expect("binary should run");

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "()\n");
    assert!(
        String::from_utf8_lossy(&output.stderr)
            .contains("[trust] proof 'foo' is trusted (level 1); depends on []")
    );
}

#[test]
fn warns_for_transitively_trusted_proofs_with_levels() {
    let output = Command::new(klassic_bin())
        .args([
            "--warn-trust",
            "-e",
            "axiom base(): { true }\ntheorem mid(): { base } = base\ntheorem top(): { mid } = mid",
        ])
        .output()
        .expect("binary should run");

    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "()\n");
    assert!(stderr.contains("[trust] proof 'base' is trusted (level 1); depends on []"));
    assert!(stderr.contains("[trust] proof 'mid' is trusted (level 2); depends on [base]"));
    assert!(stderr.contains("[trust] proof 'top' is trusted (level 3); depends on [mid]"));
}

#[test]
fn denies_trusted_proofs_via_cli() {
    let output = Command::new(klassic_bin())
        .args([
            "--deny-trust",
            "-e",
            "trust theorem foo(): { true } = assert(true)",
        ])
        .output()
        .expect("binary should run");

    assert!(!output.status.success());
    assert!(output.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&output.stderr)
            .contains("trusted proof 'foo' is not allowed (level 1)")
    );
}

#[test]
fn repl_supports_history_and_exit() {
    let mut child = Command::new(klassic_bin())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("binary should start repl");

    {
        let stdin = child.stdin.as_mut().expect("stdin should be piped");
        stdin
            .write_all(b"val answer = 42\nanswer\n:history\n:exit\n")
            .expect("repl input should be writable");
    }

    let output = child.wait_with_output().expect("repl should finish");
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(output.stderr.is_empty());
    assert!(stdout.contains("value = ()"));
    assert!(stdout.contains("value = 42"));
    assert!(stdout.contains("1: val answer = 42"));
    assert!(stdout.contains("2: answer"));
}

#[test]
fn repl_buffers_multiline_input_until_complete() {
    let mut child = Command::new(klassic_bin())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("binary should start repl");

    {
        let stdin = child.stdin.as_mut().expect("stdin should be piped");
        stdin
            .write_all(b"def addOne(x) = {\n  x + 1\n}\naddOne(2)\n:exit\n")
            .expect("repl input should be writable");
    }

    let output = child.wait_with_output().expect("repl should finish");
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success());
    assert!(output.stderr.is_empty());
    assert!(stdout.contains("value = ()"));
    assert!(stdout.contains("value = 3"));
}

/// Regression: the bundled stdlib prelude must NOT shift user line
/// numbers in either evaluator runtime errors or native-binary
/// runtime errors. Before the fix, `assert(false)` on user line 2
/// would surface as `:52:1:` (offset by the ~50-line prelude).
#[test]
fn evaluator_runtime_error_reports_user_line_numbers() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-prelude-eval-{unique}.kl"));
    fs::write(
        &source_path,
        "println(\"hello\")\nassert(false)\nprintln(\"unreached\")\n",
    )
    .expect("temp source should be writable");

    let output = Command::new(klassic_bin())
        .arg(&source_path)
        .output()
        .expect("binary should run");
    let _ = fs::remove_file(&source_path);

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(":2:1: assertion failed"),
        "stderr should pin the user's line 2; got:\n{stderr}"
    );
    assert!(
        !stderr.contains(":52:"),
        "stderr should not leak prelude-shifted line numbers; got:\n{stderr}"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_runtime_error_reports_user_line_numbers() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-prelude-native-{unique}.kl"));
    let exec_path = std::env::temp_dir().join(format!("klassic-prelude-native-bin-{unique}"));
    fs::write(
        &source_path,
        "println(\"hello\")\nassert(false)\nprintln(\"unreached\")\n",
    )
    .expect("temp source should be writable");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            exec_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("native build should run");
    assert!(
        build.status.success(),
        "native build failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let output = Command::new(&exec_path)
        .output()
        .expect("native executable should run");
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&exec_path);

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(":2:1: assertion failed"),
        "native stderr should pin the user's line 2; got:\n{stderr}"
    );
    assert!(
        !stderr.contains(":52:"),
        "native stderr should not leak prelude-shifted line numbers; got:\n{stderr}"
    );
}

/// `Time#nowMillis()` returns wall-clock milliseconds since the
/// UNIX epoch. The eval-mode and native-mode implementations share
/// a contract: a positive Int that is at least the test author's
/// hand-picked sentinel (a safely-in-the-past millisecond timestamp
/// that any non-clock-skewed system will exceed).
const TIME_NOW_MILLIS_SENTINEL: i64 = 1_700_000_000_000; // 2023-11-14T22:13:20Z

#[test]
fn evaluator_time_now_millis_is_recent() {
    let output = Command::new(klassic_bin())
        .args(["-e", "Time#nowMillis()"])
        .output()
        .expect("binary should run");
    assert!(
        output.status.success(),
        "Time#nowMillis() failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: i64 = stdout
        .trim()
        .parse()
        .unwrap_or_else(|_| panic!("expected an integer, got {stdout:?}"));
    assert!(
        value >= TIME_NOW_MILLIS_SENTINEL,
        "Time#nowMillis() returned {value} which is older than {TIME_NOW_MILLIS_SENTINEL}"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_time_now_millis_is_recent() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-time-now-millis-{unique}.kl"));
    let exec_path = std::env::temp_dir().join(format!("klassic-time-now-millis-bin-{unique}"));
    fs::write(&source_path, "println(Time#nowMillis())\n").expect("temp source should be writable");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            exec_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("native build should run");
    assert!(
        build.status.success(),
        "native build failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let output = Command::new(&exec_path)
        .output()
        .expect("native executable should run");
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&exec_path);

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: i64 = stdout
        .trim()
        .parse()
        .unwrap_or_else(|_| panic!("expected an integer, got {stdout:?}"));
    assert!(
        value >= TIME_NOW_MILLIS_SENTINEL,
        "native Time#nowMillis() returned {value} which is older than {TIME_NOW_MILLIS_SENTINEL}"
    );
}

#[test]
fn evaluator_math_pow_int_handles_common_cases() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "println(Math#powInt(2, 10))\n\
             println(Math#powInt(3, 4))\n\
             println(Math#powInt(7, 0))\n\
             println(Math#powInt(0, 5))\n\
             println(Math#powInt(-2, 3))",
        ])
        .output()
        .expect("binary should run");
    assert!(
        output.status.success(),
        "Math#powInt eval failed\nstderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    // The last expression's value (`()` from println) is also
    // printed in eval mode — see RunAction::EvaluateExpression.
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "1024\n81\n1\n0\n-8\n()\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_math_pow_int_handles_common_cases() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-math-pow-{unique}.kl"));
    let exec_path = std::env::temp_dir().join(format!("klassic-math-pow-bin-{unique}"));
    fs::write(
        &source_path,
        "println(Math#powInt(2, 10))\n\
         println(Math#powInt(3, 4))\n\
         println(Math#powInt(7, 0))\n\
         println(Math#powInt(0, 5))\n\
         println(Math#powInt(-2, 3))\n",
    )
    .expect("temp source should be writable");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            exec_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("native build should run");
    assert!(
        build.status.success(),
        "native build failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let output = Command::new(&exec_path)
        .output()
        .expect("native executable should run");
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&exec_path);

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "1024\n81\n1\n0\n-8\n"
    );
}

#[test]
fn evaluator_math_pow_int_rejects_negative_exponent() {
    let output = Command::new(klassic_bin())
        .args(["-e", "Math#powInt(2, -1)"])
        .output()
        .expect("binary should run");
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("Math#powInt expects a non-negative"),
        "stderr should explain the contract; got:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Determinism contract: `Random#seed(42)` followed by five
/// `Random#nextInt(...)` calls must produce this exact sequence in
/// both the evaluator and the native compiler. The constants are
/// Knuth's MMIX LCG (multiplier 6364136223846793005, increment
/// 1442695040888963407); changing them would silently break user
/// programs that rely on reproducibility.
const RANDOM_SEED_42_OUTPUT: &str = "69\n53\n77\n7\n588\n";

#[test]
fn evaluator_random_seed_produces_deterministic_sequence() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "Random#seed(42)\n\
             println(Random#nextInt(100))\n\
             println(Random#nextInt(100))\n\
             println(Random#nextInt(100))\n\
             println(Random#nextInt(1000))\n\
             println(Random#nextInt(1000))",
        ])
        .output()
        .expect("binary should run");
    assert!(
        output.status.success(),
        "Random eval failed\nstderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let body = stdout
        .strip_suffix("()\n")
        .expect("eval prints final () after the last expression");
    assert_eq!(body, RANDOM_SEED_42_OUTPUT);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_random_seed_matches_evaluator_sequence() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-random-{unique}.kl"));
    let exec_path = std::env::temp_dir().join(format!("klassic-random-bin-{unique}"));
    fs::write(
        &source_path,
        "Random#seed(42)\n\
         println(Random#nextInt(100))\n\
         println(Random#nextInt(100))\n\
         println(Random#nextInt(100))\n\
         println(Random#nextInt(1000))\n\
         println(Random#nextInt(1000))\n",
    )
    .expect("temp source should be writable");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            exec_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("native build should run");
    assert!(
        build.status.success(),
        "native build failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let output = Command::new(&exec_path)
        .output()
        .expect("native executable should run");
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&exec_path);

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        RANDOM_SEED_42_OUTPUT
    );
}

#[test]
fn evaluator_random_next_int_rejects_non_positive_bound() {
    for bad in ["0", "-1"] {
        let output = Command::new(klassic_bin())
            .args(["-e", &format!("Random#seed(1); Random#nextInt({bad})")])
            .output()
            .expect("binary should run");
        assert!(
            !output.status.success(),
            "bound {bad} should fail; stdout:\n{}",
            String::from_utf8_lossy(&output.stdout)
        );
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("Random#nextInt expects a positive"),
            "stderr should explain the contract; got:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn evaluator_string_parse_int_handles_typical_inputs() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "println(String#parseInt(\"0\"))\n\
             println(String#parseInt(\"42\"))\n\
             println(String#parseInt(\"-7\"))\n\
             println(String#parseInt(\"1234567890\"))",
        ])
        .output()
        .expect("binary should run");
    assert!(
        output.status.success(),
        "String#parseInt eval failed\nstderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "0\n42\n-7\n1234567890\n()\n"
    );
}

#[test]
fn evaluator_string_parse_int_rejects_garbage() {
    // We mirror Rust's `i64::parse` semantics on the eval side, so
    // leading "+" is accepted (e.g. `"+5" → 5`). Surrounding spaces
    // and any non-digit body are rejected.
    for bad in ["", "abc", "12abc", " 5", "5 "] {
        let output = Command::new(klassic_bin())
            .args(["-e", &format!("String#parseInt({bad:?})")])
            .output()
            .expect("binary should run");
        assert!(
            !output.status.success(),
            "input {bad:?} should fail; stdout:\n{}",
            String::from_utf8_lossy(&output.stdout)
        );
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("String#parseInt"),
            "stderr should mention String#parseInt; got:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_string_parse_int_handles_typical_inputs() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-parse-int-{unique}.kl"));
    let exec_path = std::env::temp_dir().join(format!("klassic-parse-int-bin-{unique}"));
    fs::write(
        &source_path,
        "println(String#parseInt(\"0\"))\n\
         println(String#parseInt(\"42\"))\n\
         println(String#parseInt(\"-7\"))\n\
         println(String#parseInt(\"1234567890\"))\n",
    )
    .expect("temp source should be writable");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            exec_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("native build should run");
    assert!(
        build.status.success(),
        "native build failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let output = Command::new(&exec_path)
        .output()
        .expect("native executable should run");
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&exec_path);

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "0\n42\n-7\n1234567890\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_string_parse_int_handles_runtime_strings() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-parse-int-runtime-{unique}.kl"));
    let exec_path = std::env::temp_dir().join(format!("klassic-parse-int-runtime-bin-{unique}"));
    fs::write(
        &source_path,
        "println(String#parseInt(\"\" + 42))\n\
         println(String#parseInt(\"-\" + 7))\n\
         println(String#parseInt(\"+\" + 5))\n\
         println(String#parseInt(\"922337203685477580\" + 7))\n\
         println(String#parseInt(\"-922337203685477580\" + 8))\n",
    )
    .expect("temp source should be writable");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            exec_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("native build should run");
    assert!(
        build.status.success(),
        "native build failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let output = Command::new(&exec_path)
        .output()
        .expect("native executable should run");
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&exec_path);

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "42\n-7\n5\n9223372036854775807\n-9223372036854775808\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_string_parse_int_rejects_bad_runtime_strings() {
    let cases = [
        (
            "klassic-parse-int-runtime-garbage",
            "String#parseInt(\"12\" + true)\n",
        ),
        (
            "klassic-parse-int-runtime-overflow",
            "String#parseInt(\"922337203685477580\" + 8)\n",
        ),
    ];
    for (prefix, source) in cases {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        let source_path = std::env::temp_dir().join(format!("{prefix}-{unique}.kl"));
        let exec_path = std::env::temp_dir().join(format!("{prefix}-bin-{unique}"));
        fs::write(&source_path, source).expect("temp source should be writable");

        let build = Command::new(klassic_bin())
            .args([
                "build",
                source_path.to_string_lossy().as_ref(),
                "-o",
                exec_path.to_string_lossy().as_ref(),
            ])
            .output()
            .expect("native build should run");
        assert!(
            build.status.success(),
            "native build failed:\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );

        let output = Command::new(&exec_path)
            .output()
            .expect("native executable should run");
        let _ = fs::remove_file(&source_path);
        let _ = fs::remove_file(&exec_path);

        assert!(
            !output.status.success(),
            "runtime parse should fail for {source:?}; stdout:\n{}",
            String::from_utf8_lossy(&output.stdout)
        );
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("String#parseInt"),
            "stderr should mention String#parseInt; got:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn evaluator_math_sqrt_int_floors_towards_zero() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "println(Math#sqrtInt(0))\n\
             println(Math#sqrtInt(1))\n\
             println(Math#sqrtInt(2))\n\
             println(Math#sqrtInt(15))\n\
             println(Math#sqrtInt(16))\n\
             println(Math#sqrtInt(99))\n\
             println(Math#sqrtInt(100))\n\
             println(Math#sqrtInt(1000000))",
        ])
        .output()
        .expect("binary should run");
    assert!(
        output.status.success(),
        "Math#sqrtInt eval failed\nstderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "0\n1\n1\n3\n4\n9\n10\n1000\n()\n"
    );
}

#[test]
fn evaluator_math_sqrt_int_rejects_negative() {
    let output = Command::new(klassic_bin())
        .args(["-e", "Math#sqrtInt(-1)"])
        .output()
        .expect("binary should run");
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("Math#sqrtInt expects a non-negative"),
        "stderr should explain the contract; got:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_math_sqrt_int_floors_towards_zero() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-sqrt-int-{unique}.kl"));
    let exec_path = std::env::temp_dir().join(format!("klassic-sqrt-int-bin-{unique}"));
    fs::write(
        &source_path,
        "println(Math#sqrtInt(0))\n\
         println(Math#sqrtInt(1))\n\
         println(Math#sqrtInt(2))\n\
         println(Math#sqrtInt(15))\n\
         println(Math#sqrtInt(16))\n\
         println(Math#sqrtInt(99))\n\
         println(Math#sqrtInt(100))\n\
         println(Math#sqrtInt(1000000))\n",
    )
    .expect("temp source should be writable");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            exec_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("native build should run");
    assert!(
        build.status.success(),
        "native build failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let output = Command::new(&exec_path)
        .output()
        .expect("native executable should run");
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&exec_path);

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "0\n1\n1\n3\n4\n9\n10\n1000\n"
    );
}

#[test]
fn evaluator_math_gcd_handles_typical_cases() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "println(Math#gcd(12, 18))\n\
             println(Math#gcd(18, 12))\n\
             println(Math#gcd(7, 13))\n\
             println(Math#gcd(0, 5))\n\
             println(Math#gcd(5, 0))\n\
             println(Math#gcd(0, 0))\n\
             println(Math#gcd(-12, 18))\n\
             println(Math#gcd(12, -18))",
        ])
        .output()
        .expect("binary should run");
    assert!(
        output.status.success(),
        "Math#gcd eval failed\nstderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "6\n6\n1\n5\n5\n0\n6\n6\n()\n"
    );
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn native_math_gcd_handles_typical_cases() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time should be monotonic")
        .as_nanos();
    let source_path = std::env::temp_dir().join(format!("klassic-gcd-{unique}.kl"));
    let exec_path = std::env::temp_dir().join(format!("klassic-gcd-bin-{unique}"));
    fs::write(
        &source_path,
        "println(Math#gcd(12, 18))\n\
         println(Math#gcd(18, 12))\n\
         println(Math#gcd(7, 13))\n\
         println(Math#gcd(0, 5))\n\
         println(Math#gcd(5, 0))\n\
         println(Math#gcd(0, 0))\n\
         println(Math#gcd(-12, 18))\n\
         println(Math#gcd(12, -18))\n",
    )
    .expect("temp source should be writable");

    let build = Command::new(klassic_bin())
        .args([
            "build",
            source_path.to_string_lossy().as_ref(),
            "-o",
            exec_path.to_string_lossy().as_ref(),
        ])
        .output()
        .expect("native build should run");
    assert!(
        build.status.success(),
        "native build failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let output = Command::new(&exec_path)
        .output()
        .expect("native executable should run");
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&exec_path);

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "6\n6\n1\n5\n5\n0\n6\n6\n"
    );
}

/// Regression: a recursive `def` declared in user code (in addition
/// to the prelude's recursive helpers) must coexist with `thread()`.
/// The fix this guards covered two distinct bugs:
///
/// * `snapshot_environment_for_thread` used to take a `borrow_mut`
///   that aliased itself when walking a recursive function value's
///   own env, panicking with "RefCell already borrowed".
/// * Even after that, snapshotting was O(N!) for N captured top-level
///   defs because each def's env was re-walked once per env slot.
///   The Arc-based cache plus restore-side dedup brings it back to
///   O(N), but only if the recursive cycle break stays correct.
#[test]
fn recursive_user_def_coexists_with_thread_spawn() {
    let output = Command::new(klassic_bin())
        .args([
            "-e",
            "def loop(n) = if(n <= 0) 0 else loop(n - 1)\n\
             mutable counter = 0\n\
             thread(() => {\n  sleep(1)\n  counter = counter + 1\n})\n\
             sleep(200)\n\
             counter",
        ])
        .output()
        .expect("binary should run");

    assert!(
        output.status.success(),
        "exit failure\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "1\n");
    assert!(
        output.stderr.is_empty(),
        "stderr should be empty; got:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}
