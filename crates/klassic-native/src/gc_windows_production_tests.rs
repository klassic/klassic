//! Focused production park protocol through the complete generated PE.
use crate::{
    Condition, GcTestProgram, NativeCodeGenerator, NativeTarget, NativeTargetContext, Reg,
    SourceFile,
};
use std::fmt::Write;
fn wrap_body(g: &mut NativeCodeGenerator, _: u64) {
    crate::gc_codegen::production::test_support::park_wrapped_work_sequence(g);
    let text = b"wrap-ok\n";
    let data = g.asm.data_label_with_bytes(text);
    g.emit_write_data(1, data, text.len());
}
#[test]
fn gc_windows_production_park_compares_the_low_sequence_word_after_wrap() {
    run_fixture(wrap_body, 10, "wrap-ok\n");
}
fn run_fixture(body: fn(&mut NativeCodeGenerator, u64), mode: u32, expected: &str) {
    let target = NativeTargetContext::for_target(NativeTarget::WindowsX86_64);
    let source = SourceFile::new("<win-park>", "1");
    let expr = klassic_syntax::parse_inline_expression("<win-park>", "1").unwrap();
    let mut g = NativeCodeGenerator::new(source, None, target.platform);
    g.gc_generational = true;
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = 128 << 10;
    g.gc_test = Some(GcTestProgram {
        body,
        arg: 0,
        no_worker_thread: false,
    });
    let object = g.compile(&expr).unwrap();
    let image = crate::write_executable_for_target(target, object);
    if let Some(directory) = std::env::var_os("KLASSIC_GC_WINDOWS_PRODUCTION_FIXTURE_DIR") {
        let directory = std::path::PathBuf::from(directory);
        std::fs::create_dir_all(&directory).unwrap();
        std::fs::write(directory.join(format!("fixture-{mode}.exe")), &image).unwrap();
    }
    let mut header = format!(
        "#define GC_WINDOWS_PRODUCTION_FIXTURE 1\n#define GC_WINDOWS_PRODUCTION_MODE {mode}\nstatic const unsigned char production_fixture[]={{"
    );
    for byte in image {
        write!(header, "{byte},").unwrap();
    }
    header.push_str("};\n");
    let stdout = crate::gc_atomic_tests::run_c_driver(
        "windows_production_fixture.h",
        &header,
        "windows_production_harness.c",
    );
    assert_eq!(stdout, expected);
}

fn xmm_stack(g: &mut NativeCodeGenerator, load: bool) {
    for xmm in 0..7_u8 {
        g.asm.bytes(&[
            0xf3,
            0x0f,
            if load { 0x6f } else { 0x7f },
            0x84 | (xmm << 3),
            0x24,
        ]);
        g.asm.bytes(&(i32::from(xmm) * 16).to_le_bytes());
    }
}
fn yield_body(g: &mut NativeCodeGenerator, _: u64) {
    let hook = g
        .gc_gen
        .as_ref()
        .unwrap()
        .labels
        .worker_hooks()
        .unwrap()
        .relax;
    let failed = g.asm.create_text_label();
    let regs = [
        Reg::Rax,
        Reg::Rbx,
        Reg::Rcx,
        Reg::Rdx,
        Reg::Rsi,
        Reg::Rdi,
        Reg::R8,
        Reg::R9,
        Reg::R10,
        Reg::R11,
        Reg::R12,
        Reg::R13,
        Reg::R14,
        Reg::R15,
    ];
    for reg in regs {
        g.asm.push_reg(reg);
    }
    g.asm.sub_reg_imm32(Reg::Rsp, 112);
    xmm_stack(g, false);
    let patterns: Vec<_> = (0..6_u8)
        .map(|i| g.asm.data_label_with_bytes(&[0x11 + i; 16]))
        .collect();
    for (xmm, pattern) in patterns.iter().enumerate() {
        g.asm.mov_data_addr(Reg::Rax, *pattern);
        g.asm.bytes(&[0xf3, 0x0f, 0x6f, (xmm as u8) << 3]);
    }
    g.asm.mov_imm64(Reg::Rax, 0);
    g.asm.cmp_reg_imm32(Reg::Rax, 1); // CF/PF/AF/SF set; ZF/OF clear
    for (i, reg) in regs.iter().enumerate() {
        g.asm.mov_imm64(*reg, 0x61 + i as u64);
    }
    g.asm.call_label(hook);
    g.asm.push_reg(Reg::Rax);
    g.asm.bytes(&[0x9c]);
    g.asm.pop_reg(Reg::Rax);
    g.asm.and_reg_imm32(Reg::Rax, 0x8d5);
    g.asm.cmp_reg_imm32(Reg::Rax, 0x95);
    g.asm.jcc_label(Condition::NotEqual, failed);
    g.asm.pop_reg(Reg::Rax);
    for (i, reg) in regs.iter().enumerate() {
        g.asm.cmp_reg_imm32(*reg, 0x61 + i as i32);
        g.asm.jcc_label(Condition::NotEqual, failed);
    }
    for (xmm, pattern) in patterns.iter().enumerate() {
        g.asm.mov_data_addr(Reg::Rax, *pattern);
        g.asm.bytes(&[0xf3, 0x0f, 0x6f, 0x30]); // movdqu xmm6,[rax]
        g.asm.bytes(&[0x66, 0x0f, 0x74, 0xc6 | ((xmm as u8) << 3)]); // pcmpeqb xmm,xmm6
        g.asm.bytes(&[0x66, 0x0f, 0xd7, 0xd0 | xmm as u8]); // pmovmskb edx,xmm
        g.asm.cmp_reg_imm32(Reg::Rdx, 0xffff);
        g.asm.jcc_label(Condition::NotEqual, failed);
    }
    xmm_stack(g, true);
    g.asm.add_reg_imm32(Reg::Rsp, 112);
    for reg in regs.into_iter().rev() {
        g.asm.pop_reg(reg);
    }
    let text = b"yield-ok\n";
    let data = g.asm.data_label_with_bytes(text);
    g.emit_write_data(1, data, text.len());
    let done = g.asm.create_text_label();
    g.asm.jmp_label(done);
    g.asm.bind_text_label(failed);
    g.emit_runtime_error_text("Windows yield clobbered live state\n");
    g.asm.bind_text_label(done);
}
#[test]
fn gc_windows_production_yield_preserves_gprs_flags_and_volatile_simd() {
    run_fixture(yield_body, 12, "yield-ok\n");
}

fn stop_body(g: &mut NativeCodeGenerator, _: u64) {
    crate::gc_codegen::production::test_support::park_wrapped_work_sequence(g);
    let runtime = g.gc_gen.take().unwrap();
    for _ in 0..2 {
        crate::gc_codegen::production::shutdown_worker(g, &runtime.labels, runtime.data.runtime);
    }
    g.gc_gen = Some(runtime);
    let text = b"stop-ok\n";
    let data = g.asm.data_label_with_bytes(text);
    g.emit_write_data(1, data, text.len());
}
#[test]
fn gc_windows_production_stop_join_is_idempotent_and_releases_idle_worker() {
    // The compiler emits a third shutdown at normal completion. The OS model
    // requires precisely one join, one returned callback and two closes.
    run_fixture(stop_body, 16, "stop-ok\n");
}

fn callback_simd(g: &mut NativeCodeGenerator, xmm: u8, load: bool) {
    g.asm.bytes(&[0xf3]);
    if xmm >= 8 {
        g.asm.bytes(&[0x44]);
    }
    g.asm.bytes(&[
        0x0f,
        if load { 0x6f } else { 0x7f },
        0x84 | ((xmm & 7) << 3),
        0x24,
    ]);
    g.asm.bytes(&(i32::from(xmm - 6) * 16).to_le_bytes());
}
fn callback_body(g: &mut NativeCodeGenerator, _: u64) {
    use crate::gc_codegen::{production::WorkerHooks, windows_production};
    let resume = g.asm.create_text_label();
    let entry = g.asm.create_text_label();
    let failed = g.asm.create_text_label();
    let regs = [
        Reg::Rbx,
        Reg::Rbp,
        Reg::Rdi,
        Reg::Rsi,
        Reg::R12,
        Reg::R13,
        Reg::R14,
        Reg::R15,
    ];
    let patterns: Vec<_> = (6..16_u8)
        .map(|i| g.asm.data_label_with_bytes(&[0x21 + i; 16]))
        .collect();
    g.asm.jmp_label(resume);
    g.asm.bind_text_label(entry);
    for reg in regs {
        g.asm.mov_imm64(reg, 0);
    }
    for xmm in 6..16_u8 {
        g.asm.bytes(&[0x66]);
        if xmm >= 8 {
            g.asm.bytes(&[0x45]);
        }
        g.asm
            .bytes(&[0x0f, 0xef, 0xc0 | ((xmm & 7) << 3) | (xmm & 7)]); // pxor xmm,xmm
    }
    g.asm.ret();
    let hooks = WorkerHooks {
        spawn: g.asm.create_text_label(),
        park: g.asm.create_text_label(),
        unpark: g.asm.create_text_label(),
        relax: g.asm.create_text_label(),
        clock: g.asm.create_text_label(),
        join: Some(g.asm.create_text_label()),
    };
    let callback = windows_production::emit(g, &hooks, entry);
    g.asm.bind_text_label(resume);
    for reg in regs {
        g.asm.push_reg(reg);
    }
    g.asm.sub_reg_imm32(Reg::Rsp, 160);
    for xmm in 6..16_u8 {
        callback_simd(g, xmm, false);
    }
    for (i, pattern) in patterns.iter().enumerate() {
        let xmm = i as u8 + 6;
        g.asm.mov_data_addr(Reg::Rax, *pattern);
        g.asm.bytes(&[0xf3]);
        if xmm >= 8 {
            g.asm.bytes(&[0x44]);
        }
        g.asm.bytes(&[0x0f, 0x6f, (xmm & 7) << 3]);
    }
    for (i, reg) in regs.iter().enumerate() {
        g.asm.mov_imm64(*reg, 0x51 + i as u64);
    }
    g.asm.sub_reg_imm32(Reg::Rsp, 32);
    g.asm.call_label(callback);
    g.asm.add_reg_imm32(Reg::Rsp, 32);
    g.asm.test_reg_reg(Reg::Rax, Reg::Rax);
    g.asm.jcc_label(Condition::NotEqual, failed);
    for (i, reg) in regs.iter().enumerate() {
        g.asm.cmp_reg_imm32(*reg, 0x51 + i as i32);
        g.asm.jcc_label(Condition::NotEqual, failed);
    }
    for (i, pattern) in patterns.iter().enumerate() {
        let xmm = i as u8 + 6;
        g.asm.mov_data_addr(Reg::Rax, *pattern);
        g.asm.bytes(&[0xf3, 0x0f, 0x6f, 0x00]); // movdqu xmm0,[rax]
        g.asm.bytes(&[0x66]);
        if xmm >= 8 {
            g.asm.bytes(&[0x44]);
        }
        g.asm.bytes(&[0x0f, 0x74, 0xc0 | ((xmm & 7) << 3)]); // pcmpeqb xmm,xmm0
        g.asm.bytes(&[0x66]);
        if xmm >= 8 {
            g.asm.bytes(&[0x41]);
        }
        g.asm.bytes(&[0x0f, 0xd7, 0xc0 | (xmm & 7)]); // pmovmskb eax,xmm
        g.asm.cmp_reg_imm32(Reg::Rax, 0xffff);
        g.asm.jcc_label(Condition::NotEqual, failed);
    }
    for xmm in 6..16_u8 {
        callback_simd(g, xmm, true);
    }
    g.asm.add_reg_imm32(Reg::Rsp, 160);
    for reg in regs.into_iter().rev() {
        g.asm.pop_reg(reg);
    }
    let text = b"callback-ok\n";
    let data = g.asm.data_label_with_bytes(text);
    g.emit_write_data(1, data, text.len());
    let done = g.asm.create_text_label();
    g.asm.jmp_label(done);
    g.asm.bind_text_label(failed);
    g.emit_exit_code(1);
    g.asm.bind_text_label(done);
}
#[test]
fn gc_windows_production_callback_restores_all_native_nonvolatile_gprs_and_simd() {
    run_fixture(callback_body, 17, "callback-ok\n");
}

fn tls_context_body(g: &mut NativeCodeGenerator, _: u64) {
    let runtime = g.gc_gen.as_ref().unwrap().data.runtime;
    crate::gc_codegen::production::test_support::begin_primary_owner_fixture(g, runtime);
    let rt = g.gc_gen.take().unwrap();
    crate::gc_codegen::production::test_support::bind_primary_owner(g);
    crate::gc_codegen::production::test_support::emit_mark_attach_tls_probe(g, &rt.labels);
    g.gc_gen = Some(rt);
    let text = b"tls-context-ok\n";
    let data = g.asm.data_label_with_bytes(text);
    g.emit_write_data(1, data, text.len());
}
#[test]
fn gc_windows_production_actual_tls_retains_key_until_detach_clear_and_context_release() {
    run_fixture(tls_context_body, 0, "tls-context-ok\n");
}

fn public_thread_api_body(g: &mut NativeCodeGenerator, _: u64) {
    let runtime = g.gc_gen.as_ref().unwrap().data.runtime;
    crate::gc_codegen::production::test_support::begin_primary_owner_fixture(g, runtime);
    let rt = g.gc_gen.take().unwrap();
    crate::gc_codegen::production::test_support::bind_primary_owner(g);
    crate::gc_codegen::production::test_support::emit_public_thread_api_probe(g, &rt.labels);
    g.gc_gen = Some(rt);
    let text = b"thread-api-ok\n";
    let data = g.asm.data_label_with_bytes(text);
    g.emit_write_data(1, data, text.len());
}
#[test]
fn gc_windows_production_public_thread_api_allocates_adopts_full_and_cleans_its_context() {
    run_fixture(public_thread_api_body, 0, "thread-api-ok\n");
}
