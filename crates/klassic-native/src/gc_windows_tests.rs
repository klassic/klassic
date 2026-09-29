//! Win64 emission tests. Linux execution uses explicit OS API models; it is
//! not evidence of Windows kernel/loader behavior.
use crate::gc_codegen::windows;
use crate::portable_asm::{PortableAsm, Reg as P};
use crate::{
    Condition, ImportSymbol as I, NativeCodeGenerator, ObjectFile, Reg, RelocTarget, TextLabel,
};

fn emit_fixture_entry(g: &mut NativeCodeGenerator) {
    let a = &mut g.asm;
    a.store_ptr_disp32(Reg::Rcx, 48, Reg::Rsp);
    for reg in [Reg::Rbx, Reg::Rsi, Reg::Rdi] {
        a.push_reg(reg);
    }
    a.sub_reg_imm32(Reg::Rsp, 32);
    a.mov_reg_reg(Reg::Rbx, Reg::Rcx);
    a.load_u32_ptr_disp32(Reg::Rcx, Reg::Rbx, 8);
    a.call_import(I::TlsGetValue);
    a.store_ptr_disp32(Reg::Rbx, 16, Reg::Rax);
    let wait = a.create_text_label();
    let ready = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(wait);
    a.load_ptr_disp32(Reg::Rax, Reg::Rbx, 56);
    a.load_ptr_disp32(Reg::Rax, Reg::Rax, 0); // acquire on x86
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::NotEqual, ready);
    a.load_ptr_disp32(Reg::Rcx, Reg::Rbx, 0);
    a.mov_imm64(Reg::Rdx, u32::MAX as u64);
    a.call_import(I::WaitForSingleObject);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::NotEqual, done);
    a.jmp_label(wait);
    a.bind_text_label(ready);
    a.load_ptr_disp32(Reg::Rdi, Reg::Rbx, 24);
    a.load_ptr_disp32(Reg::Rsi, Reg::Rbx, 32);
    g.atomic_fetch_or(P::V7, P::V6);
    let a = &mut g.asm;
    a.store_ptr_disp32(Reg::Rbx, 40, Reg::Rax);
    a.mov_imm64(Reg::Rax, 0);
    a.bind_text_label(done);
    a.add_reg_imm32(Reg::Rsp, 32);
    for reg in [Reg::Rdi, Reg::Rsi, Reg::Rbx] {
        a.pop_reg(reg);
    }
    a.ret();
}

fn emit_native_lock_call(g: &mut NativeCodeGenerator) {
    for reg in [Reg::Rbp, Reg::Rdi, Reg::Rsi] {
        g.asm.push_reg(reg);
    }
    g.asm.mov_reg_reg(Reg::Rdi, Reg::Rcx);
    g.asm.mov_reg_reg(Reg::R10, Reg::Rdx);
    let previous = g.is_windows;
    g.is_windows = true;
    g.call_native_unary(P::V10);
    g.is_windows = previous;
    for reg in [Reg::Rsi, Reg::Rdi, Reg::Rbp] {
        g.asm.pop_reg(reg);
    }
    g.asm.mov_imm64(Reg::Rax, 0);
    g.asm.ret();
}

fn object() -> (ObjectFile, Vec<(&'static str, TextLabel)>) {
    let mut g = crate::gc_atomic_tests::generator();
    let thunk = g.asm.create_text_label();
    let mut functions = Vec::new();
    let start = g.asm.create_text_label();
    g.asm.bind_text_label(start);
    windows::emit_worker_start(&mut g, thunk);
    functions.push(("start", start));
    g.asm.bind_text_label(thunk);
    windows::emit_worker_thunk(&mut g);
    for (name, emit) in [
        (
            "join",
            windows::emit_worker_join as fn(&mut NativeCodeGenerator),
        ),
        (
            "metadata_map",
            crate::gc_codegen::native_metadata::emit_windows_map,
        ),
        (
            "metadata_unmap",
            crate::gc_codegen::native_metadata::emit_windows_unmap,
        ),
        (
            "heap_decommit",
            crate::gc_codegen::native_metadata::emit_windows_decommit,
        ),
        ("event", windows::emit_event_create),
        ("clock", windows::emit_monotonic_ns),
        ("entry", emit_fixture_entry),
        ("native_lock_call", emit_native_lock_call),
    ] {
        let label = g.asm.create_text_label();
        g.asm.bind_text_label(label);
        emit(&mut g);
        functions.push((name, label));
    }
    for (name, api, u32_result) in [
        ("tls_alloc", I::TlsAlloc, true),
        ("tls_free", I::TlsFree, true),
        ("tls_get", I::TlsGetValue, false),
        ("tls_set", I::TlsSetValue, true),
        ("park", I::WaitForSingleObject, true),
        ("wake", I::SetEvent, true),
        ("close", I::CloseHandle, true),
    ] {
        let label = g.asm.create_text_label();
        g.asm.bind_text_label(label);
        windows::emit_import_leaf(&mut g, api, u32_result);
        functions.push((name, label));
    }
    (g.asm.finish(), functions)
}

#[test]
fn gc_windows_worker_pe_imports_and_relocations() {
    let (object, _) = object();
    for import in [
        I::VirtualAlloc,
        I::VirtualFree,
        I::CreateThread,
        I::WaitForSingleObject,
        I::GetExitCodeThread,
        I::TlsAlloc,
        I::TlsFree,
        I::TlsSetValue,
        I::TlsGetValue,
        I::CreateEventA,
        I::SetEvent,
    ] {
        assert!(
            object
                .relocs
                .iter()
                .any(|r| r.target == RelocTarget::Import(import))
        );
    }
    let pe = crate::pe::write_executable(object.clone(), crate::NativeTarget::WindowsX86_64.spec());
    assert_eq!(&pe[..2], b"MZ");
    for import in I::ALL {
        assert!(
            pe.windows(import.name().len())
                .any(|s| s == import.name().as_bytes())
        );
    }
    // Follow the image's section and import tables, then verify every patched
    // displacement targets its actual code label or named IAT slot. Presence
    // of an import string alone would not prove correct calls after IAT growth.
    let u16_at = |p| u16::from_le_bytes(pe[p..p + 2].try_into().unwrap()) as usize;
    let u32_at = |p| u32::from_le_bytes(pe[p..p + 4].try_into().unwrap()) as usize;
    let u64_at = |p| u64::from_le_bytes(pe[p..p + 8].try_into().unwrap()) as usize;
    let coff = u32_at(0x3c) + 4;
    let optional = coff + 20;
    let sections = optional + u16_at(coff + 16);
    let file_offset = |rva: usize| {
        for i in 0..u16_at(coff + 2) {
            let section = sections + i * 40;
            let base = u32_at(section + 12);
            let size = u32_at(section + 16);
            if (base..base + size).contains(&rva) {
                return u32_at(section + 20) + rva - base;
            }
        }
        panic!("RVA outside image sections: {rva:x}");
    };
    let cstr = |offset: usize| {
        let end = pe[offset..].iter().position(|&c| c == 0).unwrap();
        std::str::from_utf8(&pe[offset..offset + end]).unwrap()
    };
    let descriptor = file_offset(u32_at(optional + 112 + 8)); // import directory
    assert_eq!(cstr(file_offset(u32_at(descriptor + 12))), "KERNEL32.DLL");
    let iat_rva = u32_at(descriptor + 16);
    let iat_file = file_offset(iat_rva);
    let text_rva = u32_at(sections + 12);
    let text_file = file_offset(text_rva);
    for r in &object.relocs {
        let displacement = i32::from_le_bytes(
            pe[text_file + r.pos..text_file + r.pos + 4]
                .try_into()
                .unwrap(),
        );
        let actual_rva = (text_rva as i64 + r.pos as i64 + 4 + displacement as i64) as usize;
        let expected_rva = match r.target {
            RelocTarget::Text(t) => text_rva + object.text_labels[t.0],
            RelocTarget::Import(symbol) => {
                let slot = symbol.iat_index() * 8;
                assert_eq!(
                    cstr(file_offset(u64_at(iat_file + slot)) + 2),
                    symbol.name()
                );
                iat_rva + slot
            }
            RelocTarget::Data(_) => panic!("unexpected fixture data"),
        };
        assert_eq!(actual_rva, expected_rva, "relocation at {}", r.pos);
    }
}

// ARM hosts still generate/check the PE above. Execute these x86 instructions
// only on x86 hosts; Windows execution and API-model execution are reported
// separately by the driver, never as a skipped success for the other platform.
#[cfg(target_arch = "x86_64")]
#[test]
fn gc_windows_worker_emitted_code() {
    use crate::RelocKind;
    use std::fmt::Write;
    let (mut object, functions) = object();
    let iat = object.text.len().next_multiple_of(8);
    object.text.resize(iat + I::ALL.len() * 8, 0);
    for r in &object.relocs {
        assert_eq!(r.kind, RelocKind::Rel32);
        let target = match r.target {
            RelocTarget::Text(t) => object.text_labels[t.0],
            RelocTarget::Import(i) => iat + i.iat_index() * 8,
            RelocTarget::Data(_) => panic!("fixture must not use absolute data"),
        };
        let delta = i32::try_from(target as i64 - (r.pos + 4) as i64).unwrap();
        object.text[r.pos..r.pos + 4].copy_from_slice(&delta.to_le_bytes());
    }
    let mut header = String::from("static const unsigned char win_code[] = {");
    for byte in object.text {
        write!(header, "{byte},").unwrap();
    }
    header.push_str("};\n");
    for (name, label) in functions {
        writeln!(header, "#define WIN_{name} {}", object.text_labels[label.0]).unwrap();
    }
    header.push_str("static const struct {size_t offset; const char *name;} win_imports[] = {\n");
    for symbol in I::ALL {
        if object
            .relocs
            .iter()
            .any(|r| r.target == RelocTarget::Import(symbol))
        {
            writeln!(
                header,
                "{{{},\"{}\"}},",
                iat + symbol.iat_index() * 8,
                symbol.name()
            )
            .unwrap();
        }
    }
    header.push_str("};\n");
    header.push_str("#define GC_MODEL_WIN_METADATA 1\n");
    crate::gc_atomic_tests::append_windows_metadata_header(&mut header);
    let stdout = crate::gc_atomic_tests::run_c_driver(
        "gc_windows_code.h",
        &header,
        "windows_worker_harness.c",
    );
    let expected = if cfg!(target_os = "windows") {
        "Windows OS worker execution PASS"
    } else {
        "Win64 worker execution with POSIX API model PASS"
    };
    assert!(stdout.contains(expected), "{stdout}");
    assert!(
        stdout.contains("Win64 native unary callback ABI PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains("Win64 paired startup preparation PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains("Win64 native root preparation PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains("Win64 metadata backing lifecycle PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains("Win64 payload decommit: retained reservation and normalized BOOL PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains("Win64 locally relocated metadata payload PASS"),
        "{stdout}"
    );
    eprint!("{stdout}");
}

#[test]
fn gc_windows_worker_launch_layout() {
    use std::mem::{align_of, offset_of, size_of};
    assert_eq!(size_of::<windows::WorkerLaunch>(), 24);
    assert_eq!(align_of::<windows::WorkerLaunch>(), 8);
    assert_eq!(offset_of!(windows::WorkerLaunch, entry), 0);
    assert_eq!(offset_of!(windows::WorkerLaunch, context), 8);
    assert_eq!(offset_of!(windows::WorkerLaunch, tls_index), 16);
}

#[test]
fn gc_windows_gc_clock_uses_native_shim() {
    let mut g = NativeCodeGenerator::new(
        crate::SourceFile::new("<gc-windows-clock>", ""),
        None,
        crate::TargetPlatform::for_target(crate::NativeTarget::WindowsX86_64),
    );
    let before = g.asm.text.len();
    g.emit_read_monotonic_ns_to_rax();
    assert!(g.asm.text.len() > before);
    assert!(
        !g.asm.text[before..]
            .windows(2)
            .any(|bytes| bytes == [0x0f, 0x05])
    );
    g.gc_log = true;
    let clock = RelocTarget::Text(g.windows_runtime().win_monotonic_ns);
    for emit in [
        NativeCodeGenerator::emit_gc_collect_runtime as fn(&mut NativeCodeGenerator),
        NativeCodeGenerator::emit_gc_mark_start_runtime,
        NativeCodeGenerator::emit_gc_mark_end_runtime,
    ] {
        let before = g.asm.relocs.len();
        emit(&mut g);
        assert_eq!(
            g.asm.relocs[before..]
                .iter()
                .filter(|r| r.target == clock)
                .count(),
            2,
            "each stopped phase must emit both clock reads on Windows"
        );
    }
}
