//! Execute carry, zero and signed boundary checks through the real FlagAsm.
use super::*;
use std::process::Command;

#[test]
fn generational_arm64_arithmetic_flags_match_unsigned_and_signed_boundaries() {
    let mut a = Assembler::default();
    let failed = a.new_label();
    let values = [0, 1, u64::MAX, i64::MAX as u64, 1 << 63];
    for op in 0..8 {
        for x in values {
            for y in values {
                a.mov_imm64(Reg::X0, x);
                a.mov_imm64(Reg::X1, y);
                let mut f = FlagAsm(&mut a);
                let (r, carry, overflow) = match op {
                    0 => {
                        f.add_reg_reg(P::V0, P::V1);
                        let (r, c) = x.overflowing_add(y);
                        (r, c, ((!(x ^ y) & (x ^ r)) >> 63) != 0)
                    }
                    1 => {
                        f.sub_reg_reg(P::V0, P::V1);
                        let (r, c) = x.overflowing_sub(y);
                        (r, c, (((x ^ y) & (x ^ r)) >> 63) != 0)
                    }
                    2 => {
                        f.and_reg_reg(P::V0, P::V1);
                        (x & y, false, false)
                    }
                    3 => {
                        f.or_reg_reg(P::V0, P::V1);
                        (x | y, false, false)
                    }
                    4 => {
                        f.xor_reg_reg(P::V0, P::V1);
                        (x ^ y, false, false)
                    }
                    5 => {
                        f.test_reg_reg(P::V0, P::V1);
                        (x & y, false, false)
                    }
                    6 => {
                        let y = y as i32;
                        f.add_reg_imm32(P::V0, y);
                        let y = y as i64 as u64;
                        let (r, c) = x.overflowing_add(y);
                        (r, c, ((!(x ^ y) & (x ^ r)) >> 63) != 0)
                    }
                    _ => {
                        let y = y as i32;
                        f.sub_reg_imm32(P::V0, y);
                        let y = y as i64 as u64;
                        let (r, c) = x.overflowing_sub(y);
                        (r, c, (((x ^ y) & (x ^ r)) >> 63) != 0)
                    }
                };
                let zero = r == 0;
                let less = (r >> 63 != 0) ^ overflow;
                for (condition, expected) in [
                    (Condition::Equal, zero),
                    (Condition::NotEqual, !zero),
                    (Condition::Below, carry),
                    (Condition::Above, !carry && !zero),
                    (Condition::AboveOrEqual, !carry),
                    (Condition::Less, less),
                    (Condition::LessEqual, less || zero),
                    (Condition::Greater, !less && !zero),
                    (Condition::GreaterEqual, !less),
                ] {
                    if expected {
                        let next = f.create_text_label();
                        f.jcc_label(condition, next);
                        f.jmp_label(failed);
                        f.bind_text_label(next);
                    } else {
                        f.jcc_label(condition, failed);
                    }
                }
            }
        }
    }
    a.emit_write_rodata(1, b"flags-ok\n");
    a.emit_exit(0);
    a.bind(failed);
    a.emit_write_rodata(2, b"arithmetic flags mismatch\n");
    a.emit_exit(1);
    let _ = run_fixture(a, "flags", b"flags-ok\n");
}

fn run_fixture(mut a: Assembler, tag: &str, expected: &[u8]) -> Option<std::process::Output> {
    a.finish();
    let image = crate::macho::write_executable(
        a.code,
        a.rodata,
        &a.fixups,
        "flags",
        a.bss_len as u64,
        &a.imports,
    );
    let directory =
        std::env::temp_dir().join(format!("klassic-macos-{tag}-{}", std::process::id()));
    std::fs::create_dir_all(&directory).unwrap();
    let path = directory.join("flags.macho");
    std::fs::write(&path, image).unwrap();
    let mut candidates: Vec<std::path::PathBuf> = std::env::var_os("KLASSIC_UNICORN_PYTHON")
        .map(Into::into)
        .into_iter()
        .collect();
    candidates.push("python3".into());
    if let Some(home) = std::env::var_os("HOME")
        && let Ok(entries) =
            std::fs::read_dir(std::path::PathBuf::from(home).join(".pyenv/versions"))
    {
        candidates.extend(
            entries
                .filter_map(Result::ok)
                .map(|p| p.path().join("bin/python3")),
        );
    }
    let output = if let Some(python) = candidates.into_iter().find(|p| {
        Command::new(p)
            .args(["-c", "import unicorn"])
            .output()
            .is_ok_and(|o| o.status.success())
    }) {
        let script = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../tools/run-macho-aarch64.py");
        let output = Command::new(python)
            .arg(script)
            .arg(&path)
            .output()
            .unwrap();
        assert!(output.status.success(), "{output:?}");
        assert_eq!(output.stdout, expected);
        Some(output)
    } else {
        eprintln!("no unicorn interpreter; arithmetic fixture built but not executed");
        None
    };
    std::fs::remove_dir_all(directory).unwrap();
    output
}

#[test]
fn generational_arm64_worker_park_handles_sequence_wrap_and_timeout() {
    use super::super::{BranchKind as B, Cond};
    use crate::gc_codegen::production::WorkerHooks;
    let mut a = Assembler::default();
    let storage = a.reserve_data_cells(17);
    let sequence = a.reserve_data_cells(1);
    let stop = a.reserve_data_cells(1);
    let child = a.new_label();
    let child_loop = a.new_label();
    let child_done = a.new_label();
    let failed = a.new_label();
    let h = WorkerHooks {
        spawn: a.new_label(),
        park: a.new_label(),
        unpark: a.new_label(),
        relax: a.new_label(),
        clock: a.new_label(),
        join: Some(a.new_label()),
    };
    a.load_data_address(Reg::X1, sequence);
    a.mov_imm64(Reg::X0, 0x1_0000_0005);
    a.str_imm(Reg::X0, Reg::X1, 0);
    a.mov_imm64(Reg::X3, 0x12345678);
    a.call_label(h.spawn);
    a.mov_imm64(Reg::X9, 1);
    a.cmp_reg(Reg::X0, Reg::X9);
    a.branch(failed, B::Conditional(Cond::Ne));
    a.load_data_address(Reg::X5, sequence);
    a.mov_imm64(Reg::X4, 0x1_0000_0005);
    a.call_label(h.park);
    a.mov_imm64(Reg::X9, 0x12345678);
    a.cmp_reg(Reg::X3, Reg::X9);
    a.branch(failed, B::Conditional(Cond::Ne));
    a.load_data_address(Reg::X1, stop);
    a.mov_imm64(Reg::X0, 1);
    a.str_imm(Reg::X0, Reg::X1, 0);
    a.call_label(h.unpark);
    a.call_label(h.join.unwrap());
    a.emit_write_rodata(1, b"wrap-ok\n");
    a.emit_exit(0);
    a.bind(failed);
    a.emit_exit(1);
    a.bind(child);
    a.push_frame_record();
    a.bind(child_loop);
    a.load_data_address(Reg::X1, stop);
    a.ldr_imm(Reg::X0, Reg::X1, 0);
    a.branch(child_done, B::CompareNonZero(Reg::X0));
    a.load_data_address(Reg::X5, sequence);
    a.mov_imm64(Reg::X4, 0x1_0000_0005);
    a.call_label(h.park);
    a.branch(child_loop, B::Unconditional);
    a.bind(child_done);
    a.pop_frame_record();
    a.ret();
    super::super::gc_macos_production::emit(&mut a, &h, child, storage);
    if let Some(output) = run_fixture(a, "wrap", b"wrap-ok\n") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let line = stderr
            .lines()
            .find(|s| s.starts_with("macos-worker-model:"))
            .unwrap();
        assert!(
            line.split_whitespace().any(|s| s == "owner_waits=1"),
            "{stderr}"
        );
        assert!(
            !line.split_whitespace().any(|s| s == "timeouts=0"),
            "{stderr}"
        );
    }
}

#[test]
fn generational_arm64_pthread_callback_restores_every_native_nonvolatile_register() {
    use super::super::{BranchKind as B, Cond};
    use crate::gc_codegen::production::WorkerHooks;
    let mut a = Assembler::default();
    let storage = a.reserve_data_cells(17);
    let child = a.new_label();
    let failed = a.new_label();
    let h = WorkerHooks {
        spawn: a.new_label(),
        park: a.new_label(),
        unpark: a.new_label(),
        relax: a.new_label(),
        clock: a.new_label(),
        join: Some(a.new_label()),
    };
    a.mov_imm64(Reg::X3, 0x1234_5678);
    a.call_label(h.spawn);
    a.mov_imm64(Reg::X9, 1);
    a.cmp_reg(Reg::X0, Reg::X9);
    a.branch(failed, B::Conditional(Cond::Ne));
    a.call_label(h.join.unwrap());
    a.mov_imm64(Reg::X9, 0x1234_5678);
    a.cmp_reg(Reg::X3, Reg::X9);
    a.branch(failed, B::Conditional(Cond::Ne));
    a.emit_write_rodata(1, b"callback-ok\n");
    a.emit_exit(0);
    a.bind(failed);
    a.emit_exit(1);
    a.bind(child);
    // The internal convention may destroy every native nonvolatile register.
    // The model seeds distinct values and checks the callback's actual return.
    for n in 19..=29 {
        a.word(0xaa1f_03e0 | n); // mov xn,xzr
    }
    a.mov_imm64(Reg::X0, 0);
    for n in 8..=15 {
        a.word(0x9e67_0000 | n); // fmov dn,x0
    }
    a.mov_imm64(Reg::X0, 0xdead_beef); // native callback must replace this with NULL
    a.ret();
    super::super::gc_macos_production::emit(&mut a, &h, child, storage);
    if let Some(output) = run_fixture(a, "callback", b"callback-ok\n") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        for counter in [
            "returns=1",
            "join_calls=1",
            "joins=1",
            "stack_releases=1",
            "cond_destroy=1",
            "mutex_destroy=1",
        ] {
            assert!(stderr.split_whitespace().any(|s| s == counter), "{stderr}");
        }
    }
}

#[test]
fn generational_arm64_production_stop_join_is_idempotent_and_releases_the_event() {
    let mut e = Emitter::default();
    e.asm.gc_max_heap = Some(16 << 20);
    e.asm.gc_min_heap = Some(131072);
    let gc = e.reserve_gc_state();
    e.prepare_generational();
    e.emit_generational_startup(gc.shadow_stack);
    for _ in 0..3 {
        e.emit_generational_worker_shutdown();
    }
    e.asm.emit_write_rodata(1, b"stop-ok\n");
    e.asm.emit_exit(0);
    e.emit_generational_runtime(gc.shadow_stack, gc.shadow_stack_top);
    e.emit_gc_call_trampolines();
    if let Some(output) = run_fixture(e.asm, "stop", b"stop-ok\n") {
        let stderr = String::from_utf8_lossy(&output.stderr);
        for counter in [
            "threads=1",
            "returns=1",
            "join_calls=1",
            "joins=1",
            "stack_releases=1",
            "cond_destroy=1",
            "mutex_destroy=1",
        ] {
            assert!(stderr.split_whitespace().any(|s| s == counter), "{stderr}");
        }
    }
}

#[test]
fn generational_arm64_public_pin_scope_executes_compiled_c_native_callbacks_and_error_cleanup() {
    use crate::gc_abi::{
        NativeApiBinding, ScopedPinApi, ScopedPinCall as Call, ScopedPinScope as Scope,
    };
    use crate::gc_codegen::production::test_support::{bind_primary_owner, pin_abi_emit_bridges};
    use std::mem::{offset_of, size_of};
    let directory = std::env::temp_dir().join(format!("klassic-arm-pin-c-{}", std::process::id()));
    std::fs::create_dir_all(&directory).unwrap();
    let manifest = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let object = directory.join("body.o");
    let compiler = std::env::var("KLASSIC_CLANG").unwrap_or_else(|_| {
        ["clang-15", "clang"]
            .into_iter()
            .find(|cc| {
                Command::new(cc)
                    .arg("--version")
                    .output()
                    .is_ok_and(|o| o.status.success())
            })
            .expect("AArch64 C conformance requires Clang")
            .to_owned()
    });
    let compile = Command::new(compiler)
        .args([
            "--target=arm64-apple-macos11",
            "-std=c11",
            "-ffreestanding",
            "-O2",
            "-fno-stack-protector",
            "-fno-unwind-tables",
            "-fno-asynchronous-unwind-tables",
            "-fno-vectorize",
            "-fno-slp-vectorize",
            "-c",
        ])
        .arg(manifest.join("tests/pin_abi_freestanding_body.c"))
        .arg("-I")
        .arg(manifest.join("../../runtime/gc"))
        .arg("-o")
        .arg(&object)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let bytes = std::fs::read(object).unwrap();
    let u32_at = |off: usize| u32::from_le_bytes(bytes[off..off + 4].try_into().unwrap()) as usize;
    let u64_at = |off: usize| u64::from_le_bytes(bytes[off..off + 8].try_into().unwrap()) as usize;
    assert_eq!(u32_at(0), 0xfeedfacf);
    let mut command = 32;
    let mut code = None;
    for _ in 0..u32_at(16) {
        if u32_at(command) == 0x19 {
            for i in 0..u32_at(command + 64) {
                let section = command + 72 + i * 80;
                if &bytes[section..section + 7] == b"__text\0" {
                    assert_eq!(
                        u32_at(section + 60),
                        0,
                        "callback must have no external relocations"
                    );
                    let off = u32_at(section + 48);
                    let n = u64_at(section + 40);
                    code = Some(bytes[off..off + n].to_vec());
                }
            }
        }
        command += u32_at(command + 4);
    }
    let code = code.expect("compiled native callback text");
    assert!(!code.is_empty() && code.len().is_multiple_of(4));
    for mode in [0, 1, 2, 3] {
        let mut e = Emitter::default();
        e.asm.gc_max_heap = Some(16 << 20);
        e.asm.gc_min_heap = Some(131072);
        let gc = e.reserve_gc_state();
        e.prepare_generational();
        e.emit_generational_startup(gc.shadow_stack);
        let root = e.asm.reserve_data_cells(1);
        let scope = e.asm.reserve_data_cells(size_of::<Scope>() / 8);
        let user = e.asm.reserve_data_cells(8);
        let callback = e.asm.new_label();
        let managed = e.asm.new_label();
        let fault = e.asm.new_label();
        let failed = e.asm.new_label();
        let l = e.gen_labels();
        let (alloc, adopt, store, enable, bootstrap) = (
            l.alloc_actor,
            l.root_adopt_actor,
            l.root_store_actor,
            l.moving_enable_actor,
            l.native_api_bootstrap,
        );
        {
            let mut f = FlagAsm(&mut e.asm);
            bind_primary_owner(&mut f);
            f.mov_imm64(P::V7, 16);
            f.mov_imm64(P::V6, 1);
            f.call_label(alloc);
            f.mov_imm64(P::V1, 77);
            f.store_ptr_disp32(P::V0, 0, P::V1);
            f.mov_data_addr(P::V1, PortDataAddr::Bss(root));
            f.store_ptr_disp32(P::V1, 0, P::V0);
            f.mov_reg_reg(P::V0, P::V1);
            f.call_label(adopt);
            f.mov_reg_reg(P::V7, P::OwnerContext);
            f.mov_text_addr(P::V11, bootstrap);
            f.call_native_unary(P::V11);
            f.test_reg_reg(P::V0, P::V0);
            f.jcc_label(Condition::Equal, failed);
            f.mov_reg_reg(P::V8, P::V0);
            f.mov_data_addr(P::V9, PortDataAddr::Bss(user));
            f.store_ptr_disp32(P::V9, 56, P::V8);
            f.mov_text_addr(P::V0, bootstrap);
            f.store_ptr_disp32(P::V9, 48, P::V0);
            f.mov_data_addr(P::V9, PortDataAddr::Bss(scope));
            f.load_ptr_disp32(P::V0, P::V8, offset_of!(NativeApiBinding, context) as i32);
            f.store_ptr_disp32(P::V9, offset_of!(Call, context) as i32, P::V0);
            f.load_ptr_disp32(P::V0, P::V8, offset_of!(NativeApiBinding, pins) as i32);
            f.store_ptr_disp32(P::V9, offset_of!(Scope, api) as i32, P::V0);
            f.mov_data_addr(P::V1, PortDataAddr::Bss(root));
            f.load_ptr_disp32(P::V0, P::V1, 0);
            f.store_ptr_disp32(P::V9, offset_of!(Call, source) as i32, P::V0);
            f.mov_text_addr(P::V0, callback);
            f.store_ptr_disp32(P::V9, offset_of!(Scope, callback) as i32, P::V0);
            f.mov_data_addr(P::V0, PortDataAddr::Bss(user));
            f.store_ptr_disp32(P::V9, offset_of!(Scope, user) as i32, P::V0);
            f.mov_reg_reg(P::V9, P::V0);
            f.mov_text_addr(P::V0, managed);
            f.store_ptr_disp32(P::V9, 0, P::V0);
            f.mov_text_addr(P::V0, fault);
            f.store_ptr_disp32(P::V9, 8, P::V0);
            f.mov_imm64(P::V0, mode);
            f.store_ptr_disp32(P::V9, 24, P::V0);
            f.call_label(enable);
            f.mov_data_addr(P::V7, PortDataAddr::Bss(scope));
            f.load_ptr_disp32(P::V0, P::V7, offset_of!(Scope, api) as i32);
            f.load_ptr_disp32(P::V11, P::V0, offset_of!(ScopedPinApi, scope) as i32);
            f.call_native_unary(P::V11);
            f.cmp_reg_imm8(P::V0, 2);
            f.jcc_label(Condition::NotEqual, failed);
            f.mov_data_addr(P::V1, PortDataAddr::Bss(root));
            f.mov_imm64(P::V0, 0);
            f.call_label(store);
            let again = f.create_text_label();
            f.bind_text_label(again);
            f.mov_data_addr(P::V7, PortDataAddr::Bss(scope));
            f.load_ptr_disp32(P::V0, P::V7, offset_of!(Scope, api) as i32);
            f.load_ptr_disp32(P::V11, P::V0, offset_of!(ScopedPinApi, scope) as i32);
            f.call_native_unary(P::V11);
            f.cmp_reg_imm8(P::V0, 2);
            f.jcc_label(Condition::Equal, again);
            let finished = f.create_text_label();
            f.cmp_reg_imm8(P::V0, 6);
            f.jcc_label(Condition::NotEqual, finished);
            f.mov_data_addr(P::V9, PortDataAddr::Bss(user));
            f.load_ptr_disp32(P::V0, P::V9, 32);
            f.add_reg_imm32(P::V0, 1);
            f.store_ptr_disp32(P::V9, 32, P::V0);
            f.jmp_label(again);
            f.bind_text_label(finished);
            f.test_reg_reg(P::V0, P::V0);
            f.jcc_label(Condition::NotEqual, failed);
            f.mov_data_addr(P::V9, PortDataAddr::Bss(scope));
            for (off, value) in [
                (offset_of!(Scope, result), 77),
                (offset_of!(Scope, stage), 5),
                (offset_of!(Scope, error), 0),
                (offset_of!(Call, address), 0),
            ] {
                f.load_ptr_disp32(P::V0, P::V9, off as i32);
                f.cmp_reg_imm32(P::V0, value);
                f.jcc_label(Condition::NotEqual, failed);
            }
            f.mov_data_addr(P::V9, PortDataAddr::Bss(user));
            f.load_ptr_disp32(P::V0, P::V9, 16);
            f.cmp_reg_imm8(P::V0, 1);
            f.jcc_label(Condition::NotEqual, failed);
            f.load_ptr_disp32(P::V0, P::V9, 32);
            f.cmp_reg_imm32(P::V0, i32::from(mode == 2));
            f.jcc_label(Condition::NotEqual, failed);
        }
        e.emit_generational_worker_shutdown();
        e.asm.emit_write_rodata(1, b"pin-c-ok\n");
        e.asm.emit_exit(0);
        e.asm.bind(failed);
        e.asm.emit_exit(1);
        e.asm.bind(callback);
        for word in code.chunks_exact(4) {
            e.asm.word(u32::from_le_bytes(word.try_into().unwrap()));
        }
        {
            let runtime = e.gc_gen.take().unwrap();
            pin_abi_emit_bridges(&mut FlagAsm(&mut e.asm), &runtime.labels, managed, fault);
            e.gc_gen = Some(runtime);
        }
        e.emit_generational_runtime(gc.shadow_stack, gc.shadow_stack_top);
        e.emit_gc_call_trampolines();
        assert!(
            run_fixture(e.asm, &format!("pin-c-{mode}"), b"pin-c-ok\n").is_some(),
            "native callback must execute in the AArch64 API model"
        );
    }
    std::fs::remove_dir_all(directory).unwrap();
}

#[test]
fn generational_arm64_rooted_output_reloads_after_full_movement_between_chunks() {
    use crate::gc_codegen::production::test_support::{
        bind_primary_owner, rooted_write_emit_collection_hook,
    };
    for bytes in [0_u64, 4097, 8193] {
        let mut e = Emitter::default();
        e.asm.gc_max_heap = Some(16 << 20);
        e.asm.gc_min_heap = Some(131072);
        let gc = e.reserve_gc_state();
        e.prepare_generational();
        e.emit_generational_startup(gc.shadow_stack);
        let count = e.asm.reserve_data_cells(1);
        let text: Vec<u8> = (0..bytes)
            .map(|i| b'a' + ((i / 4096 + i % 17) % 26) as u8)
            .collect();
        let data = e.asm.intern_rodata(&text);
        let failed = e.asm.new_label();
        let runtime = e.gc_gen.take().unwrap();
        let l = &runtime.labels;
        {
            let mut f = FlagAsm(&mut e.asm);
            bind_primary_owner(&mut f);
            rooted_write_emit_collection_hook(&mut f, l, PortDataAddr::Bss(count));
            f.mov_imm64(P::V7, bytes + 8);
            f.mov_imm64(P::V6, 1);
            f.call_label(l.alloc_actor);
            f.mov_reg_reg(P::V9, P::V0);
            f.mov_imm64(P::V1, bytes);
            f.store_ptr_disp32(P::V9, 0, P::V1);
            f.mov_reg_reg(P::V7, P::V9);
            f.add_reg_imm32(P::V7, 8);
            f.mov_data_addr(P::V6, PortDataAddr::Rodata(data));
            f.rep_movsb();
            f.mov_reg_reg(P::V0, P::V9);
            f.mov_imm64(P::V7, 1);
            f.call_label(l.write_rooted_string);
            f.mov_data_addr(P::V1, PortDataAddr::Bss(count));
            f.load_ptr_disp32(P::V0, P::V1, 0);
            f.cmp_reg_imm32(P::V0, bytes.div_ceil(4096) as i32);
            f.jcc_label(Condition::NotEqual, failed);
        }
        e.gc_gen = Some(runtime);
        e.emit_generational_worker_shutdown();
        e.asm.emit_write_rodata(1, b"\nrooted-output-ok\n");
        e.asm.emit_exit(0);
        e.asm.bind(failed);
        e.asm.emit_exit(1);
        e.emit_generational_runtime(gc.shadow_stack, gc.shadow_stack_top);
        e.emit_gc_call_trampolines();
        let mut expected = text;
        expected.extend_from_slice(b"\nrooted-output-ok\n");
        assert!(run_fixture(e.asm, "rooted-output", &expected).is_some());
    }
}

#[test]
fn generational_arm64_nested_renderers_keep_real_parent_roots_through_full_moves() {
    use crate::aarch64::{LoweredEnumTable, collect_functions, collect_records};
    use crate::gc_barrier_harness_tests::nested_output::{EXPECTED, SOURCE, lowered};
    use crate::gc_codegen::production::test_support::{
        bind_primary_owner, rooted_write_depth, rooted_write_emit_collection_hook,
    };
    use std::collections::HashMap;
    for stress in [false, true] {
        let source = klassic_syntax::parse_inline_expression("<nested-output>", SOURCE).unwrap();
        let mut declarations = Vec::new();
        crate::collect_enum_decls(&source, &mut declarations);
        let owners: HashMap<_, _> = declarations
            .into_iter()
            .flat_map(|(name, _, variants)| {
                variants.into_iter().map(move |v| (v.name, name.clone()))
            })
            .collect();
        let (expr, names, _, variants, annotations) = lowered();
        let mut e = Emitter {
            gc_stress: stress,
            lowered_enums: names,
            lowered_enum_table: LoweredEnumTable::from_shapes(&variants, &annotations, &owners),
            ..Emitter::default()
        };
        e.asm.gc_max_heap = Some(16 << 20);
        e.asm.gc_min_heap = Some(131072);
        e.scopes.push(HashMap::new());
        e.lambda_bindings.push(HashMap::new());
        e.dict_bindings.push(HashMap::new());
        e.alias_bindings.push(HashMap::new());
        e.scope_root_slots.push(Vec::new());
        collect_records(&expr, &mut e);
        collect_functions(&expr, &mut e);
        let gc = e.reserve_gc_state();
        e.prepare_generational();
        e.emit_generational_startup(gc.shadow_stack);
        let count = e.asm.reserve_data_cells(1);
        let frame = e.asm.sub_sp_placeholder();
        e.asm.mov_fp_sp();
        {
            let rt = e.gc_gen.take().unwrap();
            let mut f = FlagAsm(&mut e.asm);
            bind_primary_owner(&mut f);
            rooted_write_emit_collection_hook(&mut f, &rt.labels, PortDataAddr::Bss(count));
            e.gc_gen = Some(rt);
        }
        e.statement(&expr).unwrap();
        let failed = e.asm.new_label();
        {
            let mut f = FlagAsm(&mut e.asm);
            f.mov_data_addr(P::V1, PortDataAddr::Bss(count));
            f.load_ptr_disp32(P::V0, P::V1, 0);
            f.cmp_reg_imm32(P::V0, 19);
            f.jcc_label(Condition::NotEqual, failed);
            rooted_write_depth(&mut f);
            f.test_reg_reg(P::V0, P::V0);
            f.jcc_label(Condition::NotEqual, failed);
        }
        e.asm
            .patch_sub_sp(frame, e.max_local_offset.div_ceil(16) * 16);
        e.emit_generational_worker_shutdown();
        e.asm.emit_exit(0);
        e.asm.bind(failed);
        e.asm.emit_exit(1);
        let mut emitted = vec![false; e.functions.len()];
        while let Some(index) = e.pending.pop() {
            if emitted.len() < e.functions.len() {
                emitted.resize(e.functions.len(), false);
            }
            if !emitted[index] {
                emitted[index] = true;
                e.emit_function(index).unwrap();
            }
        }
        if let Some(label) = e.member_scalar_label {
            e.emit_member_scalar_routine(label);
        }
        if let Some(label) = e.member_string_label {
            e.emit_member_string_routine(label);
        }
        if let Some(label) = e.member_deep_label {
            e.emit_member_deep_routine(label);
        }
        if let Some(label) = e.list_reverse_label {
            e.emit_list_reverse_routine(label);
        }
        if let Some(label) = e.shadow_push_label {
            e.emit_shadow_push_routine(label);
        }
        if let Some(label) = e.shadow_pop_label {
            e.emit_shadow_pop_routine(label);
        }
        e.emit_generational_runtime(gc.shadow_stack, gc.shadow_stack_top);
        e.emit_gc_call_trampolines();
        assert!(run_fixture(e.asm, "nested-output", EXPECTED).is_some());
    }
}

#[test]
fn generational_arm64_precise_frame_root_entries_preserve_registers_and_nzcv() {
    use super::super::{BranchKind as B, Cond};
    let mut e = Emitter::default();
    let gc = e.reserve_gc_state();
    e.prepare_generational();
    e.emit_generational_startup(gc.shadow_stack);
    e.asm.sub_sp_imm(512);
    e.asm.mov_fp_sp();
    e.scope_root_slots.push(Vec::new());
    let failed = e.asm.new_label();
    let registers = [
        Reg::X0,
        Reg::X1,
        Reg::X2,
        Reg::X3,
        Reg::X4,
        Reg::X5,
        Reg::X6,
        Reg::X7,
        Reg::X8,
        Reg::X9,
        Reg::X10,
        Reg::X11,
        Reg::X12,
        Reg::X16,
        Reg::X17,
    ];
    // Actual production null RootCells suffice to exercise adoption, handle
    // validation, journal publication, load and LIFO clearing. Every NZCV
    // combination is observed after every operation, before comparisons run.
    for flags in 0..16u64 {
        e.scope_root_slots.push(Vec::new());
        e.asm.mov_imm64(Reg::X0, 0);
        e.asm.store_local(Reg::X0, 0);
        for operation in 0..4 {
            for (i, reg) in registers.into_iter().enumerate() {
                e.asm.mov_imm64(
                    reg,
                    if reg == Reg::X2 {
                        0
                    } else if reg == Reg::X17 {
                        flags << 28
                    } else {
                        0x1000 + i as u64
                    },
                );
            }
            e.asm.word(0xd51b_4200 | 17); // msr NZCV,x17
            match operation {
                0 => e.emit_root_frame_slot(0),
                1 => e.load_local(Reg::X7, 0),
                2 => e.store_local(Reg::X2, 0),
                _ => e.close_root_scope(),
            }
            for (i, reg) in registers.into_iter().enumerate() {
                e.asm.store_local(reg, 32 + i as u32 * 8);
            }
            e.asm.word(0xd53b_4200 | 17); // mrs x17,NZCV
            e.asm.store_local(Reg::X17, 256);
            for (i, reg) in registers.into_iter().enumerate() {
                let expected = if reg == Reg::X2 || operation == 1 && reg == Reg::X7 {
                    0
                } else if reg == Reg::X17 {
                    flags << 28
                } else {
                    0x1000 + i as u64
                };
                e.asm.load_local(Reg::X0, 32 + i as u32 * 8);
                e.asm.mov_imm64(Reg::X1, expected);
                e.asm.cmp_reg(Reg::X0, Reg::X1);
                e.asm.branch(failed, B::Conditional(Cond::Ne));
            }
            e.asm.load_local(Reg::X0, 256);
            e.asm.mov_imm64(Reg::X1, flags << 28);
            e.asm.cmp_reg(Reg::X0, Reg::X1);
            e.asm.branch(failed, B::Conditional(Cond::Ne));
        }
    }
    e.emit_generational_worker_shutdown();
    e.asm.emit_write_rodata(1, b"frame-roots-registers-ok\n");
    e.asm.emit_exit(0);
    e.asm.bind(failed);
    e.asm
        .emit_write_rodata(2, b"frame root register/NZCV mismatch\n");
    e.asm.emit_exit(1);
    e.emit_generational_runtime(gc.shadow_stack, gc.shadow_stack_top);
    e.emit_gc_call_trampolines();
    assert!(run_fixture(e.asm, "frame-registers", b"frame-roots-registers-ok\n").is_some());
}

#[test]
fn generational_arm64_fresh_owner_attaches_during_mark_and_keeps_root_through_full() {
    let mut e = Emitter::default();
    e.asm.gc_max_heap = Some(16 << 20);
    let gc = e.reserve_gc_state();
    e.prepare_generational();
    e.emit_generational_startup(gc.shadow_stack);
    // Labels are consumed during emission, so keep the mutable assembler
    // separate from Emitter's immutable runtime-label borrow.
    let mut assembler = std::mem::take(&mut e.asm);
    {
        let mut f = FlagAsm(&mut assembler);
        crate::gc_codegen::production::test_support::bind_primary_owner(&mut f);
        crate::gc_codegen::production::test_support::emit_mark_attach_probe(&mut f, e.gen_labels());
    }
    e.asm = assembler;
    e.emit_generational_worker_shutdown();
    e.asm.emit_write_rodata(1, b"mark-attach-ok\n");
    e.asm.emit_exit(0);
    e.emit_generational_runtime(gc.shadow_stack, gc.shadow_stack_top);
    e.emit_gc_call_trampolines();
    assert!(run_fixture(e.asm, "mark-attach", b"mark-attach-ok\n").is_some());
}

#[test]
fn generational_arm64_attached_owner_detaches_and_releases_context_after_full() {
    let mut e = Emitter::default();
    e.asm.gc_max_heap = Some(16 << 20);
    let gc = e.reserve_gc_state();
    e.prepare_generational();
    e.emit_generational_startup(gc.shadow_stack);
    // Labels are consumed during emission, so keep the mutable assembler
    // separate from Emitter's immutable runtime-label borrow.
    let mut assembler = std::mem::take(&mut e.asm);
    {
        let mut f = FlagAsm(&mut assembler);
        crate::gc_codegen::production::test_support::bind_primary_owner(&mut f);
        crate::gc_codegen::production::test_support::emit_mark_attach_detach_probe(
            &mut f,
            e.gen_labels(),
        );
    }
    e.asm = assembler;
    e.emit_generational_worker_shutdown();
    e.asm.emit_write_rodata(1, b"mark-detach-ok\n");
    e.asm.emit_exit(0);
    e.emit_generational_runtime(gc.shadow_stack, gc.shadow_stack_top);
    e.emit_gc_call_trampolines();
    assert!(run_fixture(e.asm, "mark-detach", b"mark-detach-ok\n").is_some());
}

#[test]
fn generational_arm64_actual_pthread_tls_retains_key_until_detach_clear_and_context_release() {
    let mut e = Emitter::default();
    e.asm.gc_max_heap = Some(16 << 20);
    let gc = e.reserve_gc_state();
    e.prepare_generational();
    e.emit_generational_startup(gc.shadow_stack);
    // Labels are consumed during emission, so keep the mutable assembler
    // separate from Emitter's immutable runtime-label borrow.
    let mut assembler = std::mem::take(&mut e.asm);
    {
        let mut f = FlagAsm(&mut assembler);
        crate::gc_codegen::production::test_support::bind_primary_owner(&mut f);
        crate::gc_codegen::production::test_support::emit_mark_attach_tls_probe(
            &mut f,
            e.gen_labels(),
        );
    }
    e.asm = assembler;
    e.emit_generational_worker_shutdown();
    e.asm.emit_write_rodata(1, b"thread-tls-ok\n");
    e.asm.emit_exit(0);
    e.emit_generational_runtime(gc.shadow_stack, gc.shadow_stack_top);
    e.emit_gc_call_trampolines();
    assert!(run_fixture(e.asm, "thread-tls", b"thread-tls-ok\n").is_some());
}

#[test]
fn generational_arm64_public_thread_api_allocates_adopts_full_and_cleans_its_context() {
    let mut e = Emitter::default();
    e.asm.gc_max_heap = Some(16 << 20);
    let gc = e.reserve_gc_state();
    e.prepare_generational();
    e.emit_generational_startup(gc.shadow_stack);
    let mut assembler = std::mem::take(&mut e.asm);
    {
        let mut f = FlagAsm(&mut assembler);
        crate::gc_codegen::production::test_support::bind_primary_owner(&mut f);
        crate::gc_codegen::production::test_support::emit_public_thread_api_probe(
            &mut f,
            e.gen_labels(),
        );
    }
    e.asm = assembler;
    e.emit_generational_worker_shutdown();
    e.asm.emit_write_rodata(1, b"thread-api-ok\n");
    e.asm.emit_exit(0);
    e.emit_generational_runtime(gc.shadow_stack, gc.shadow_stack_top);
    e.emit_gc_call_trampolines();
    assert!(run_fixture(e.asm, "thread-api", b"thread-api-ok\n").is_some());
}
