//! The emitted native boundary, including worker progress and failed writes.
use super::{H, Src, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};

fn metrics(h: &mut H<'_>) {
    for (name, i) in [("entries", 0), ("exits", 1), ("chunks", 2)] {
        h.report(name, |h| native_io_metric(h.g, i));
    }
    h.report("state", |h| native_context_state(h.g));
}
fn boundary(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 24, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.mark_start();
    h.g.call_label(h.l.native_enter);
    h.report("blocked", |h| native_context_state(h.g));
    // Drive the worker's real tracer in the deterministic fallback fixture.
    // It uses published RootCells without waiting for the blocked owner.
    h.drain_marks();
    h.report_counter("marked", Counter::Marked);
    h.g.call_label(h.l.native_exit);
    metrics(&mut h);
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
}
#[test]
fn production_native_boundary_publishes_state_and_allows_worker_marking() {
    for worker in [false, true] {
        let o = run(boundary, 0, worker);
        assert_eq!(o.get("blocked"), 2, "{}", o.text());
        assert!(o.get("marked") > 0, "{}", o.text());
        assert_eq!(o.get("entries"), 1);
        assert_eq!(o.get("exits"), 1);
        assert_eq!(o.get("chunks"), 0);
        assert_eq!(o.get("state"), 0);
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("id"), 1);
    }
}
fn output(g: &mut NativeCodeGenerator, bytes: u64) {
    let mut h = H::begin(g);
    let failed_write = bytes == u64::MAX;
    let bytes = if failed_write { 4097 } else { bytes };
    h.alloc(1, bytes + 8, 0);
    h.set_scalar(Src::Raw(0), 0, bytes);
    let text = h.g.asm.data_label_with_bytes(&vec![b'x'; bytes as usize]);
    h.value(Src::Raw(0));
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.add_reg_imm32(R::V7, 8);
    h.g.mov_data_addr(R::V6, text);
    h.g.mov_imm64(R::V1, bytes);
    h.g.rep_movsb();
    h.root(0);
    h.value(Src::Root(0));
    h.g.mov_imm64(R::V7, if failed_write { u64::MAX } else { 1 });
    h.g.call_label(h.l.write_heap_string);
    let newline = h.g.asm.data_label_with_bytes(b"\n");
    h.g.plat_write_data(1, newline, 1);
    metrics(&mut h);
    h.collect();
    h.finish_collection();
    h.report("length", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
}
#[test]
fn production_native_string_copyout_chunks_and_write_errors_restore_running() {
    for worker in [false, true] {
        for bytes in [0, 4096, 4097, 8193, u64::MAX] {
            let o = run(output, bytes, worker);
            let failed = bytes == u64::MAX;
            let count = if failed { 4097 } else { bytes };
            let chunks = count.div_ceil(4096) as i64;
            assert!(o.text().starts_with(&format!(
                "{}\n",
                "x".repeat(if failed { 0 } else { bytes as usize })
            )));
            assert_eq!(o.get("entries"), chunks, "{}", o.text());
            assert_eq!(o.get("exits"), chunks);
            assert_eq!(o.get("chunks"), chunks);
            assert_eq!(o.get("state"), 0);
            assert_eq!(o.get("length"), count as i64);
            assert_eq!(o.get("id"), 1);
        }
    }
}
fn invalid(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 8, 0);
    h.root(0);
    match mode {
        0 | 1 => native_private_probe(h.g, mode == 0),
        2 => h.g.call_label(h.l.native_enter),
        3 => {
            h.g.call_label(h.l.native_enter);
            h.value(Src::Root(0));
        }
        _ => unreachable!(),
    }
    h.g.call_label(h.l.native_enter);
}

fn sleeping(g: &mut NativeCodeGenerator, millis: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 8, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.mark_start();
    h.g.mov_imm64(R::V0, millis);
    h.g.call_label(h.l.sleep_millis);
    metrics(&mut h);
    h.drain_marks();
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
}

#[test]
fn production_native_sleep_preserves_roots_and_restores_running() {
    for worker in [false, true] {
        for millis in [0, 1, 20] {
            let o = run(sleeping, millis, worker);
            assert_eq!(o.get("entries"), 1, "{}", o.text());
            assert_eq!(o.get("exits"), 1);
            assert_eq!(o.get("chunks"), 0);
            assert_eq!(o.get("state"), 0);
            assert_eq!(o.get("value"), 84);
            assert_eq!(o.get("id"), 1);
        }
    }
}

fn rooted_output(g: &mut NativeCodeGenerator, arg: u64) {
    let mut h = H::begin(g);
    let failed = arg == u64::MAX;
    let bytes = if failed { 4097 } else { arg };
    let count = rooted_write_collect_between_chunks(h.g);
    h.alloc(1, bytes + 8, 0);
    h.set_scalar(Src::Raw(0), 0, bytes);
    let text = h.g.asm.data_label_with_bytes(&output_pattern(bytes));
    h.value(Src::Raw(0));
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.add_reg_imm32(R::V7, 8);
    h.g.mov_data_addr(R::V6, text);
    h.g.mov_imm64(R::V1, bytes);
    h.g.rep_movsb();
    // The writer must own its result root, even when no caller frame roots it.
    let invalid_stdout = (failed && h.g.is_windows).then(|| h.g.windows_runtime().stdout_handle);
    if let Some(handle) = invalid_stdout {
        // Win64's print shim selects cached standard handles, not arbitrary
        // fd numbers. Pass INVALID_HANDLE_VALUE to the real WriteFile leaf;
        // restore the original handle before printing the test observations.
        h.g.mov_data_addr(R::V1, handle);
        h.g.load_ptr_disp32(R::V0, R::V1, 0);
        h.g.store_rbp_slot(1856, R::V0);
        h.g.mov_imm64(R::V0, u64::MAX);
        h.g.store_ptr_disp32(R::V1, 0, R::V0);
    }
    h.value(Src::Raw(0));
    h.g.mov_imm64(R::V7, if failed { u64::MAX } else { 1 });
    rooted_write_call(h.g);
    if let Some(handle) = invalid_stdout {
        h.g.mov_data_addr(R::V1, handle);
        h.g.load_rbp_slot(R::V0, 1856);
        h.g.store_ptr_disp32(R::V1, 0, R::V0);
    }
    let newline = h.g.asm.data_label_with_bytes(b"\n");
    h.g.plat_write_data(1, newline, 1);
    h.report("moves", |h| {
        h.g.mov_data_addr(R::V1, count);
        h.g.load_ptr_disp32(R::V0, R::V1, 0);
    });
    h.report("depth", |h| rooted_write_depth(h.g));
    metrics(&mut h);
}
fn output_pattern(bytes: u64) -> Vec<u8> {
    (0..bytes)
        .map(|i| b'a' + ((i / 4096 + i % 17) % 26) as u8)
        .collect()
}
#[test]
fn production_rooted_output_reloads_sole_root_after_full_moves_between_chunks() {
    for worker in [false, true] {
        for bytes in [0, 1, 4096, 4097, 8193, u64::MAX] {
            let o = run(rooted_output, bytes, worker);
            let failed = bytes == u64::MAX;
            let length = if failed { 4097 } else { bytes };
            let chunks = length.div_ceil(4096) as i64;
            let mut expected = if failed {
                vec![]
            } else {
                output_pattern(bytes)
            };
            expected.push(b'\n');
            assert!(o.text().as_bytes().starts_with(&expected), "{}", o.text());
            assert_eq!(o.get("moves"), chunks, "{}", o.text());
            assert_eq!(o.get("depth"), 0);
            assert_eq!(o.get("entries"), chunks);
            assert_eq!(o.get("exits"), chunks);
            assert_eq!(o.get("chunks"), chunks);
            assert_eq!(o.get("state"), 0);
        }
    }
}

fn native_buffer_output(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    let invalid_stdout = (mode == 2 && h.g.is_windows).then(|| h.g.windows_runtime().stdout_handle);
    if let Some(handle) = invalid_stdout {
        h.g.mov_data_addr(R::V1, handle);
        h.g.load_ptr_disp32(R::V0, R::V1, 0);
        h.g.store_rbp_slot(1856, R::V0);
        h.g.mov_imm64(R::V0, u64::MAX);
        h.g.store_ptr_disp32(R::V1, 0, R::V0);
    }
    let bytes = if mode == 1 {
        b"".as_slice()
    } else {
        b"native-buffer\n".as_slice()
    };
    let data = h.g.asm.data_label_with_bytes(bytes);
    h.g.mov_imm64(R::V7, if mode == 2 { u64::MAX } else { 1 });
    h.g.mov_data_addr(R::V6, data);
    h.g.mov_imm64(R::V2, bytes.len() as u64);
    native_buffer_call(h.g);
    h.keep(0);
    if let Some(handle) = invalid_stdout {
        h.g.mov_data_addr(R::V1, handle);
        h.g.load_rbp_slot(R::V0, 1856);
        h.g.store_ptr_disp32(R::V1, 0, R::V0);
    }
    h.report("result", |h| h.value(Src::Raw(0)));
    h.report("buffers", |h| native_io_metric(h.g, 3));
    h.report("borrows", |h| root_native_borrows(h.g));
    metrics(&mut h);
}
#[test]
fn production_native_buffer_returns_os_result_and_restores_state_for_empty_and_error_writes() {
    for worker in [false, true] {
        for mode in 0..3 {
            let o = run(native_buffer_output, mode, worker);
            assert_eq!(o.get("result"), [14, 0, -9][mode as usize], "{}", o.text());
            assert_eq!(o.get("buffers"), 1);
            assert_eq!(o.get("borrows"), 0);
            assert_eq!(o.get("entries"), 1);
            assert_eq!(o.get("exits"), 1);
            assert_eq!(o.get("chunks"), 0);
            assert_eq!(o.get("state"), 0);
        }
    }
}

#[test]
fn production_native_buffer_win64_returns_counts_and_normalizes_real_write_failure() {
    use std::fmt::Write;
    let target = crate::NativeTargetContext::for_target(crate::NativeTarget::WindowsX86_64);
    for mode in 0..3 {
        let expr = klassic_syntax::parse_inline_expression("<native-buffer>", "1").unwrap();
        let mut g = NativeCodeGenerator::new(
            crate::SourceFile::new("<native-buffer>", "1"),
            None,
            target.platform,
        );
        g.gc_generational = true;
        g.gc_max_heap = 16 << 20;
        g.gc_min_heap = 128 << 10;
        g.gc_test = Some(crate::GcTestProgram {
            body: native_buffer_output,
            arg: mode,
            no_worker_thread: false,
        });
        let image = crate::write_executable_for_target(target, g.compile(&expr).unwrap());
        let expected = format!(
            "{}result={}\nbuffers=1\nborrows=0\nentries=1\nexits=1\nchunks=0\nstate=0\n",
            if mode == 0 { "native-buffer\n" } else { "" },
            [14, 0, -1][mode as usize]
        );
        if let Some(directory) = std::env::var_os("KLASSIC_GC_BUFFER_NATIVE_WINDOWS_DIR") {
            let directory = std::path::PathBuf::from(directory);
            std::fs::create_dir_all(&directory).unwrap();
            std::fs::write(directory.join(format!("buffer-{mode}.exe")), &image).unwrap();
            std::fs::write(directory.join(format!("buffer-{mode}.expected")), &expected).unwrap();
            let script = include_str!("../../tests/pin_native_windows_runner.ps1")
                .replace("pin-", "buffer-")
                .replace("$mode in 0..3", "$mode in 0..2")
                .replace("native pin callback", "native buffer");
            let mut encoded = vec![0xff, 0xfe];
            encoded.extend(script.encode_utf16().flat_map(u16::to_le_bytes));
            std::fs::write(directory.join("run-native.ps1"), encoded).unwrap();
        }
        let mut header = String::from(
            "#define GC_WINDOWS_PRODUCTION_FIXTURE 1\n#define GC_WINDOWS_PRODUCTION_MODE 0\nstatic const unsigned char production_fixture[]={",
        );
        for byte in image {
            write!(header, "{byte},").unwrap();
        }
        header.push_str("};\n");
        let output = crate::gc_atomic_tests::run_c_driver(
            "windows_production_fixture.h",
            &header,
            "windows_production_harness.c",
        );
        assert_eq!(output, expected, "mode={mode}");
    }
}

#[test]
fn production_rooted_output_win64_moves_between_chunks_and_preserves_error_cleanup() {
    use std::fmt::Write;
    let target = crate::NativeTargetContext::for_target(crate::NativeTarget::WindowsX86_64);
    for (mode, bytes) in [8193, u64::MAX, 4096, 0].into_iter().enumerate() {
        let expr = klassic_syntax::parse_inline_expression("<rooted-output>", "1").unwrap();
        let mut g = NativeCodeGenerator::new(
            crate::SourceFile::new("<rooted-output>", "1"),
            None,
            target.platform,
        );
        g.gc_generational = true;
        g.gc_max_heap = 16 << 20;
        g.gc_min_heap = 128 << 10;
        g.gc_test = Some(crate::GcTestProgram {
            body: rooted_output,
            arg: bytes,
            no_worker_thread: false,
        });
        let image = crate::write_executable_for_target(target, g.compile(&expr).unwrap());
        let failed = bytes == u64::MAX;
        let chunks = (if failed { 4097 } else { bytes }).div_ceil(4096);
        let mut expected = if failed {
            vec![]
        } else {
            output_pattern(bytes)
        };
        expected.extend(format!("\nmoves={chunks}\ndepth=0\nentries={chunks}\nexits={chunks}\nchunks={chunks}\nstate=0\n").as_bytes());
        if let Some(directory) = std::env::var_os("KLASSIC_GC_ROOTED_WRITE_NATIVE_WINDOWS_DIR") {
            let directory = std::path::PathBuf::from(directory);
            std::fs::create_dir_all(&directory).unwrap();
            std::fs::write(directory.join(format!("rooted-{mode}.exe")), &image).unwrap();
            std::fs::write(directory.join(format!("rooted-{mode}.expected")), &expected).unwrap();
            let script = include_str!("../../tests/pin_native_windows_runner.ps1")
                .replace("pin-", "rooted-")
                .replace("native pin callback", "native rooted output");
            let mut encoded = vec![0xff, 0xfe];
            encoded.extend(script.encode_utf16().flat_map(u16::to_le_bytes));
            std::fs::write(directory.join("run-native.ps1"), encoded).unwrap();
        }
        let mut header = String::from(
            "#define GC_WINDOWS_PRODUCTION_FIXTURE 1\n#define GC_WINDOWS_PRODUCTION_MODE 0\nstatic const unsigned char production_fixture[]={",
        );
        for byte in image {
            write!(header, "{byte},").unwrap();
        }
        header.push_str("};\n");
        let output = crate::gc_atomic_tests::run_c_driver(
            "windows_production_fixture.h",
            &header,
            "windows_production_harness.c",
        );
        assert!(
            output.as_bytes() == expected,
            "mode={mode}, actual bytes={}, expected bytes={}",
            output.len(),
            expected.len()
        );
    }
}
#[test]
fn production_native_boundary_rejects_private_access_nested_entry_and_heap_reads() {
    for mode in 0..4 {
        let o = run_failure(invalid, mode);
        assert!(!o.status.success());
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
    }
}
