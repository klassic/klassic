//! Compile a real C caller of the embedding header and run it against the
//! emitted runtime, including the host C compiler's register/stack convention.
use super::{H, Src, run};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
use std::{path::Path, process::Command, sync::OnceLock};

static PROBE: OnceLock<Vec<u8>> = OnceLock::new();

fn command(program: &str, args: &[&str], directory: &Path) -> String {
    let output = Command::new(program)
        .args(args)
        .current_dir(directory)
        .env("LC_ALL", "C")
        .output()
        .unwrap_or_else(|e| panic!("{program}: {e}"));
    assert!(output.status.success(), "{program}: {output:?}");
    String::from_utf8(output.stdout).expect("tool output")
}

fn body(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.report("address_before", |h| h.value(Src::Root(0)));
    let entry = h.g.create_text_label();
    let callback = h.g.create_text_label();
    let after = h.g.create_text_label();
    h.g.jmp_label(after);
    h.g.bind_text_label(entry);
    h.g.asm.bytes(PROBE.get().unwrap());
    h.g.bind_text_label(callback);
    h.g.native_entry_begin(0);
    // C may use callee-saved registers internally. Rebind the primary runtime
    // before managed callback work; native_entry_end restores C's values.
    h.reload();
    request_full_collection(h.g);
    h.collect();
    h.finish_collection();
    h.g.mov_imm64(R::V0, 0);
    h.g.native_entry_end();
    h.g.ret();
    h.g.bind_text_label(after);
    let bootstrap = h.g.gc_gen.as_ref().unwrap().labels.native_api_bootstrap;
    for name in ["c_first", "c_reuse"] {
        h.report(name, |h| {
            h.g.mov_reg_reg(R::V7, R::OwnerContext);
            h.g.mov_text_addr(R::V11, bootstrap);
            h.g.call_native_unary(R::V11);
            h.g.mov_reg_reg(R::V7, R::V0);
            h.g.mov_text_addr(R::V6, callback);
            h.g.mov_text_addr(R::V11, entry);
            h.g.call_native_binary(R::V11);
        });
        h.report(&format!("{name}_value"), |h| h.load(Src::Root(0), 0));
        h.report(&format!("{name}_address"), |h| h.value(Src::Root(0)));
    }
    h.report("users", |h| tls_metric(h.g, true));
    h.report("key", |h| tls_metric(h.g, false));
    h.report("members", |h| {
        participant_metric(h.g, ParticipantMetric::Members)
    });
    h.report_counter("full", Counter::FullCollections);
    h.report("value", |h| h.load(Src::Root(0), 0));
}

#[test]
fn production_public_thread_embedding_header_runs_real_compiled_c_caller() {
    let source = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../runtime/gc/gc_native_thread_abi_probe.c")
        .canonicalize()
        .unwrap();
    let stamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let directory = std::env::temp_dir().join(format!(
        "klassic-thread-api-c-{}-{stamp}",
        std::process::id()
    ));
    std::fs::create_dir(&directory).unwrap();
    command(
        "clang",
        &[
            "-O2",
            "-std=c11",
            "-Wall",
            "-Wextra",
            "-Werror",
            "-fno-stack-protector",
            "-fno-pic",
            "-fno-pie",
            "-fno-asynchronous-unwind-tables",
            "-fno-unwind-tables",
            "-fno-jump-tables",
            "-ffreestanding",
            "-mno-sse",
            "-mno-sse2",
            "-c",
            source.to_str().unwrap(),
            "-o",
            "probe.o",
        ],
        &directory,
    );
    // Reject any dependency instead of silently dropping compiler relocations.
    let relocations = command("readelf", &["--wide", "-r", "probe.o"], &directory);
    assert!(
        relocations.contains("There are no relocations"),
        "{relocations}"
    );
    let symbols = command("readelf", &["--wide", "-s", "probe.o"], &directory);
    let entry = symbols
        .lines()
        .find(|line| line.split_whitespace().last() == Some("gc_native_thread_abi_probe"))
        .expect("C entry symbol");
    assert_eq!(
        u64::from_str_radix(entry.split_whitespace().nth(1).unwrap(), 16).unwrap(),
        0
    );
    command(
        "objcopy",
        &[
            "--only-section=.text",
            "-O",
            "binary",
            "probe.o",
            "probe.bin",
        ],
        &directory,
    );
    let code = std::fs::read(directory.join("probe.bin")).unwrap();
    assert!(!code.is_empty());
    PROBE.set(code).unwrap();
    let o = run(body, 0, true);
    for name in ["c_first", "c_reuse", "users", "key"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
    assert_eq!(o.get("members"), 1, "{}", o.text());
    assert_eq!(o.get("full"), 2, "{}", o.text());
    assert_eq!(o.get("c_first_value"), 84, "{}", o.text());
    assert_eq!(o.get("c_reuse_value"), 84, "{}", o.text());
    assert!(o.get("address_before") > 0, "{}", o.text());
    assert_ne!(
        o.get("address_before"),
        o.get("c_first_address"),
        "{}",
        o.text()
    );
    assert_ne!(
        o.get("c_first_address"),
        o.get("c_reuse_address"),
        "{}",
        o.text()
    );
    assert_eq!(o.get("value"), 84, "{}", o.text());
    std::fs::remove_dir_all(directory).unwrap();
}
