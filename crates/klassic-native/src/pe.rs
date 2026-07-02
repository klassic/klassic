//! Minimal PE32+ (PE64) console executable writer for the
//! `x86_64-pc-windows-msvc` direct backend. Unlike the Mach-O arm64
//! backend, Windows does not get its own codegen module: it reuses
//! the *entire* `DirectX86_64` (Linux) code generator (see
//! `NativeCodeGenerator`), which threads an `is_windows` flag through
//! every OS-boundary site (writing to stdout/stderr, growing the GC
//! heap via `mmap`, and process exit) so those sites call a small set
//! of Win64 import-call shims (`__win_write`, `__win_mmap`,
//! `__win_exit`, `__win_init`) instead of emitting a bare Linux
//! `syscall` instruction. This module's only job is serializing the
//! resulting `ObjectFile` (flat `text`/`data` byte buffers plus a
//! label-based relocation list -- the exact same shape the ELF writer
//! in `mod elf` consumes) as a valid, unsigned, import-only-from-
//! `kernel32.dll` PE64 image: no linker, no CRT, no `.reloc` section
//! (fixed `ImageBase`, `IMAGE_FILE_RELOCS_STRIPPED`).
//!
//! Layout follows the standard three-section shape a real linker
//! would produce (`.text` RX, `.rdata` R holding the import
//! directory/ILT/IAT/hint-name table, `.data` RW holding the
//! generator's `object.data` blob verbatim -- it mixes read-only
//! string constants with mutable globals like the GC heap pointers,
//! so it must be writable as a whole). `AddressOfEntryPoint` is the
//! very first byte of `.text`, exactly like the ELF writer's
//! `_start`.

use super::{ImportSymbol, NativeTargetSpec, ObjectFile, RelocKind, RelocTarget};

const IMAGE_BASE: u64 = 0x1_4000_0000;
const SECTION_ALIGNMENT: u64 = 0x1000;
const FILE_ALIGNMENT: u64 = 0x200;

const DOS_HEADER_SIZE: u64 = 64;
const PE_SIGNATURE_SIZE: u64 = 4;
const COFF_HEADER_SIZE: u64 = 20;
/// Fixed PE32+ optional header: 112 bytes of scalar fields plus 16
/// data directories * 8 bytes = 240. Must match
/// `COFF.SizeOfOptionalHeader` exactly (checked by
/// `build_target_windows_x86_64_emits_pe`).
const OPTIONAL_HEADER_SIZE: u64 = 112 + 16 * 8;
const SECTION_HEADER_SIZE: u64 = 40;
const SECTION_COUNT: u16 = 3; // .text, .rdata, .data
const NUMBER_OF_RVA_AND_SIZES: u32 = 16;

const IMAGE_FILE_MACHINE_AMD64: u16 = 0x8664;
const IMAGE_FILE_RELOCS_STRIPPED: u16 = 0x0001;
const IMAGE_FILE_EXECUTABLE_IMAGE: u16 = 0x0002;
const IMAGE_FILE_LARGE_ADDRESS_AWARE: u16 = 0x0020;

const OPTIONAL_HEADER_MAGIC_PE32_PLUS: u16 = 0x020b;
const IMAGE_SUBSYSTEM_WINDOWS_CUI: u16 = 3;
/// `NX_COMPAT` only. Deliberately does *not* set `DYNAMIC_BASE` or
/// `HIGH_ENTROPY_VA`: both require a real `.reloc` table, which this
/// design omits (fixed `ImageBase`, `RELOCS_STRIPPED`).
const IMAGE_DLLCHARACTERISTICS_NX_COMPAT: u16 = 0x0100;

const IMAGE_SCN_CNT_CODE: u32 = 0x0000_0020;
const IMAGE_SCN_CNT_INITIALIZED_DATA: u32 = 0x0000_0040;
const IMAGE_SCN_MEM_EXECUTE: u32 = 0x2000_0000;
const IMAGE_SCN_MEM_READ: u32 = 0x4000_0000;
const IMAGE_SCN_MEM_WRITE: u32 = 0x8000_0000;

/// 8 MiB -- Klassic programs recurse deeply (the native backend's own
/// stack-overflow probe assumes real headroom); the default MSVC 1 MiB
/// reserve is too small.
const SIZE_OF_STACK_RESERVE: u64 = 0x0080_0000;
const SIZE_OF_STACK_COMMIT: u64 = 0x0001_0000;
/// The program never uses the default process heap (all dynamic
/// allocation goes through `VirtualAlloc` directly), so these are
/// harmless linker-style defaults.
const SIZE_OF_HEAP_RESERVE: u64 = 0x0010_0000;
const SIZE_OF_HEAP_COMMIT: u64 = 0x0000_1000;

const KERNEL32_DLL: &str = "KERNEL32.DLL\0";

fn align_to(value: u64, alignment: u64) -> u64 {
    value.div_ceil(alignment) * alignment
}

struct Writer {
    bytes: Vec<u8>,
}

impl Writer {
    fn u8(&mut self, value: u8) {
        self.bytes.push(value);
    }

    fn u16(&mut self, value: u16) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    fn u32(&mut self, value: u32) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    fn u64(&mut self, value: u64) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    fn push_bytes(&mut self, data: &[u8]) {
        self.bytes.extend_from_slice(data);
    }

    fn pad_to(&mut self, offset: usize) {
        debug_assert!(self.bytes.len() <= offset);
        self.bytes.resize(offset, 0);
    }
}

/// Serialize `object` (the same flat text/data/relocs shape
/// `elf::write_executable` consumes) as a complete PE32+ executable.
/// `spec` is accepted for signature parity with the ELF writer but
/// unused: every field this function needs (machine type, subsystem,
/// section layout) is fixed for this single supported Windows target.
pub(crate) fn write_executable(mut object: ObjectFile, _spec: &NativeTargetSpec) -> Vec<u8> {
    let section_headers_size = SECTION_HEADER_SIZE * u64::from(SECTION_COUNT);
    let headers_end = DOS_HEADER_SIZE
        + PE_SIGNATURE_SIZE
        + COFF_HEADER_SIZE
        + OPTIONAL_HEADER_SIZE
        + section_headers_size;
    let size_of_headers = align_to(headers_end, FILE_ALIGNMENT);

    // ---- .text ----
    let text_rva = SECTION_ALIGNMENT;
    let text_file_offset = size_of_headers;
    let text_virtual_size = object.text.len() as u64;
    let text_raw_size = align_to(text_virtual_size, FILE_ALIGNMENT);

    // ---- .rdata: import directory table + ILT + IAT + hint/name + DLL name ----
    let rdata_rva = align_to(text_rva + text_virtual_size, SECTION_ALIGNMENT);
    let rdata_file_offset = align_to(text_file_offset + text_raw_size, FILE_ALIGNMENT);

    let import_dir_off: u64 = 0;
    let import_dir_size: u64 = 20 * 2; // one DLL descriptor + a zero terminator
    let ilt_off = import_dir_off + import_dir_size;
    let ilt_size = 8 * (ImportSymbol::ALL.len() as u64 + 1); // + zero terminator
    let iat_off = ilt_off + ilt_size;
    let iat_size = ilt_size;

    let mut hint_name_offsets = Vec::with_capacity(ImportSymbol::ALL.len());
    let mut cursor = iat_off + iat_size;
    for symbol in ImportSymbol::ALL {
        hint_name_offsets.push(cursor);
        // WORD Hint + ASCIIZ name, padded to an even total length.
        let entry_len = 2 + symbol.name().len() + 1;
        cursor += align_to(entry_len as u64, 2);
    }
    let dll_name_off = cursor;
    let rdata_virtual_size = dll_name_off + KERNEL32_DLL.len() as u64;
    let rdata_raw_size = align_to(rdata_virtual_size, FILE_ALIGNMENT);

    // ---- .data: the generator's object.data verbatim (RW: mixes
    // read-only string constants with mutable globals) ----
    let data_rva = align_to(rdata_rva + rdata_virtual_size, SECTION_ALIGNMENT);
    let data_file_offset = align_to(rdata_file_offset + rdata_raw_size, FILE_ALIGNMENT);
    let data_virtual_size = object.data.len() as u64;
    let data_raw_size = align_to(data_virtual_size, FILE_ALIGNMENT);

    let size_of_image = align_to(data_rva + data_virtual_size, SECTION_ALIGNMENT);

    // ---- patch relocations now that every section's VA is final ----
    let text_va = IMAGE_BASE + text_rva;
    let data_va = IMAGE_BASE + data_rva;
    let iat_va = IMAGE_BASE + rdata_rva + iat_off;
    patch_relocations(&mut object, text_va, data_va, iat_va);

    // ---- build .rdata content ----
    let mut rdata = Writer {
        bytes: Vec::with_capacity(rdata_virtual_size as usize),
    };
    // IMAGE_IMPORT_DESCRIPTOR for KERNEL32.DLL.
    rdata.u32((rdata_rva + ilt_off) as u32); // OriginalFirstThunk (-> ILT)
    rdata.u32(0); // TimeDateStamp
    rdata.u32(0); // ForwarderChain
    rdata.u32((rdata_rva + dll_name_off) as u32); // Name
    rdata.u32((rdata_rva + iat_off) as u32); // FirstThunk (-> IAT)
    rdata.push_bytes(&[0u8; 20]); // null descriptor terminator
    debug_assert_eq!(rdata.bytes.len() as u64, ilt_off);
    // ILT: RVAs into the hint/name table, non-ordinal (bit 63 clear).
    for &offset in &hint_name_offsets {
        rdata.u64(rdata_rva + offset);
    }
    rdata.u64(0);
    debug_assert_eq!(rdata.bytes.len() as u64, iat_off);
    // IAT: identical initial content to the ILT; the loader overwrites
    // this array in place with resolved addresses. Generated `call
    // [rip+disp32]` instructions (via `call_import`) target these
    // slots, never the ILT.
    for &offset in &hint_name_offsets {
        rdata.u64(rdata_rva + offset);
    }
    rdata.u64(0);
    debug_assert_eq!(rdata.bytes.len() as u64, hint_name_offsets[0]);
    for (symbol, &offset) in ImportSymbol::ALL.iter().zip(hint_name_offsets.iter()) {
        debug_assert_eq!(rdata.bytes.len() as u64, offset);
        rdata.u16(0); // Hint
        rdata.push_bytes(symbol.name().as_bytes());
        rdata.u8(0); // NUL terminator
        if !rdata.bytes.len().is_multiple_of(2) {
            rdata.u8(0); // pad to an even offset for the next entry
        }
    }
    debug_assert_eq!(rdata.bytes.len() as u64, dll_name_off);
    rdata.push_bytes(KERNEL32_DLL.as_bytes());
    debug_assert_eq!(rdata.bytes.len() as u64, rdata_virtual_size);
    rdata.pad_to(rdata_raw_size as usize);

    // ---- assemble the full file ----
    let mut out = Writer {
        bytes: Vec::with_capacity(size_of_image as usize),
    };

    // DOS header: the Windows loader only reads e_magic and follows
    // e_lfanew straight to the PE signature, so a stub-less "tiny PE"
    // (zero bytes between the two) loads fine.
    out.push_bytes(b"MZ");
    out.pad_to(0x3c);
    out.u32(DOS_HEADER_SIZE as u32); // e_lfanew
    debug_assert_eq!(out.bytes.len() as u64, DOS_HEADER_SIZE);

    out.push_bytes(b"PE\0\0");

    // COFF file header.
    out.u16(IMAGE_FILE_MACHINE_AMD64);
    out.u16(SECTION_COUNT);
    out.u32(0); // TimeDateStamp
    out.u32(0); // PointerToSymbolTable
    out.u32(0); // NumberOfSymbols
    out.u16(OPTIONAL_HEADER_SIZE as u16);
    out.u16(
        IMAGE_FILE_RELOCS_STRIPPED | IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE,
    );

    // Optional header (IMAGE_OPTIONAL_HEADER64 / PE32+).
    out.u16(OPTIONAL_HEADER_MAGIC_PE32_PLUS);
    out.u8(14); // MajorLinkerVersion (arbitrary, unchecked by the loader)
    out.u8(0); // MinorLinkerVersion
    out.u32(text_raw_size as u32); // SizeOfCode
    out.u32((rdata_raw_size + data_raw_size) as u32); // SizeOfInitializedData
    out.u32(0); // SizeOfUninitializedData (no .bss; heap comes from VirtualAlloc)
    out.u32(text_rva as u32); // AddressOfEntryPoint
    out.u32(text_rva as u32); // BaseOfCode
    out.u64(IMAGE_BASE);
    out.u32(SECTION_ALIGNMENT as u32);
    out.u32(FILE_ALIGNMENT as u32);
    out.u16(6); // MajorOperatingSystemVersion
    out.u16(0); // MinorOperatingSystemVersion
    out.u16(0); // MajorImageVersion
    out.u16(0); // MinorImageVersion
    out.u16(6); // MajorSubsystemVersion
    out.u16(0); // MinorSubsystemVersion
    out.u32(0); // Win32VersionValue (reserved, must be zero)
    out.u32(size_of_image as u32);
    out.u32(size_of_headers as u32);
    out.u32(0); // CheckSum (not enforced for ordinary console EXEs)
    out.u16(IMAGE_SUBSYSTEM_WINDOWS_CUI);
    out.u16(IMAGE_DLLCHARACTERISTICS_NX_COMPAT);
    out.u64(SIZE_OF_STACK_RESERVE);
    out.u64(SIZE_OF_STACK_COMMIT);
    out.u64(SIZE_OF_HEAP_RESERVE);
    out.u64(SIZE_OF_HEAP_COMMIT);
    out.u32(0); // LoaderFlags (obsolete, must be zero)
    out.u32(NUMBER_OF_RVA_AND_SIZES);
    // Data directories: only Export(0) and Import(1) are populated
    // (IAT(12) is deliberately left {0,0} -- the loader resolves
    // imports from the Import Directory Table, not this entry).
    out.u32(0);
    out.u32(0); // 0: Export
    out.u32((rdata_rva + import_dir_off) as u32);
    out.u32(import_dir_size as u32); // 1: Import
    for _ in 2..16 {
        out.u32(0);
        out.u32(0);
    }
    debug_assert_eq!(
        out.bytes.len() as u64,
        DOS_HEADER_SIZE + PE_SIGNATURE_SIZE + COFF_HEADER_SIZE + OPTIONAL_HEADER_SIZE
    );

    write_section_header(
        &mut out,
        b".text",
        text_virtual_size,
        text_rva,
        text_raw_size,
        text_file_offset,
        IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ,
    );
    write_section_header(
        &mut out,
        b".rdata",
        rdata_virtual_size,
        rdata_rva,
        rdata_raw_size,
        rdata_file_offset,
        IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ,
    );
    write_section_header(
        &mut out,
        b".data",
        data_virtual_size,
        data_rva,
        data_raw_size,
        data_file_offset,
        IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE,
    );
    debug_assert_eq!(out.bytes.len() as u64, headers_end);

    out.pad_to(size_of_headers as usize);
    debug_assert_eq!(out.bytes.len() as u64, text_file_offset);
    out.push_bytes(&object.text);
    out.pad_to((text_file_offset + text_raw_size) as usize);

    debug_assert_eq!(out.bytes.len() as u64, rdata_file_offset);
    out.push_bytes(&rdata.bytes);
    out.pad_to((rdata_file_offset + rdata_raw_size) as usize);

    debug_assert_eq!(out.bytes.len() as u64, data_file_offset);
    out.push_bytes(&object.data);
    out.pad_to((data_file_offset + data_raw_size) as usize);

    out.bytes
}

fn write_section_header(
    out: &mut Writer,
    name: &[u8],
    virtual_size: u64,
    virtual_address: u64,
    size_of_raw_data: u64,
    pointer_to_raw_data: u64,
    characteristics: u32,
) {
    let mut name_field = [0u8; 8];
    name_field[..name.len()].copy_from_slice(name);
    out.push_bytes(&name_field);
    out.u32(virtual_size as u32);
    out.u32(virtual_address as u32);
    out.u32(size_of_raw_data as u32);
    out.u32(pointer_to_raw_data as u32);
    out.u32(0); // PointerToRelocations
    out.u32(0); // PointerToLinenumbers
    out.u16(0); // NumberOfRelocations
    out.u16(0); // NumberOfLinenumbers
    out.u32(characteristics);
}

fn patch_relocations(object: &mut ObjectFile, text_va: u64, data_va: u64, iat_va: u64) {
    for reloc in &object.relocs {
        match reloc.kind {
            RelocKind::Rel32 => {
                let target = match reloc.target {
                    RelocTarget::Text(label) => text_va + object.text_labels[label.0] as u64,
                    RelocTarget::Data(label) => data_va + object.data_labels[label.0] as u64,
                    RelocTarget::Import(symbol) => iat_va + (symbol.iat_index() as u64) * 8,
                };
                let next = text_va + reloc.pos as u64 + 4;
                let value = (target as i64 - next as i64) as i32;
                object.text[reloc.pos..reloc.pos + 4].copy_from_slice(&value.to_le_bytes());
            }
            RelocKind::Abs64 => {
                let target = match reloc.target {
                    RelocTarget::Text(label) => text_va + object.text_labels[label.0] as u64,
                    RelocTarget::Data(label) => data_va + object.data_labels[label.0] as u64,
                    RelocTarget::Import(_) => {
                        unreachable!("no codegen path emits an Abs64 relocation against an import")
                    }
                };
                object.text[reloc.pos..reloc.pos + 8].copy_from_slice(&target.to_le_bytes());
            }
        }
    }
}
