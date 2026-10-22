//! Mach-O arm64 executable writer for the aarch64-apple-darwin
//! direct backend. Mirrors the ELF writer's role: codegen hands over
//! flat code/rodata blobs plus adrp/add data fixups and this module
//! serializes a complete, runnable executable. Darwin-specific
//! obligations live here: the Apple Silicon kernel only executes
//! arm64 mains that are dyld-linked (`MH_DYLDLINK` +
//! `LC_LOAD_DYLINKER` + `LC_MAIN`; fully static `LC_UNIXTHREAD`
//! images are rejected with EBADEXEC) and carry a valid embedded
//! ad-hoc code signature (a SHA-256 CodeDirectory). dyld also needs
//! `LC_SYMTAB` / `LC_DYSYMTAB`. Optional libSystem imports use eager dyld
//! bindings into writable GOT slots. No external `codesign` / `cc` / `ld`
//! is involved, keeping the direct backend's no-toolchain policy.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ImportSymbol {
    ClockGettime,
    Mmap,
    Munmap,
    Mprotect,
    Madvise,
    PthreadCreate,
    PthreadJoin,
    PthreadKeyCreate,
    PthreadKeyDelete,
    PthreadSetSpecific,
    PthreadGetSpecific,
    PthreadMutexInit,
    PthreadMutexDestroy,
    PthreadMutexLock,
    PthreadMutexUnlock,
    PthreadCondInit,
    PthreadCondDestroy,
    PthreadCondWait,
    PthreadCondSignal,
}

impl ImportSymbol {
    pub(crate) fn name(self) -> &'static str {
        match self {
            Self::ClockGettime => "_clock_gettime",
            Self::Mmap => "_mmap",
            Self::Munmap => "_munmap",
            Self::Mprotect => "_mprotect",
            Self::Madvise => "_madvise",
            Self::PthreadCreate => "_pthread_create",
            Self::PthreadJoin => "_pthread_join",
            Self::PthreadKeyCreate => "_pthread_key_create",
            Self::PthreadKeyDelete => "_pthread_key_delete",
            Self::PthreadSetSpecific => "_pthread_setspecific",
            Self::PthreadGetSpecific => "_pthread_getspecific",
            Self::PthreadMutexInit => "_pthread_mutex_init",
            Self::PthreadMutexDestroy => "_pthread_mutex_destroy",
            Self::PthreadMutexLock => "_pthread_mutex_lock",
            Self::PthreadMutexUnlock => "_pthread_mutex_unlock",
            Self::PthreadCondInit => "_pthread_cond_init",
            Self::PthreadCondDestroy => "_pthread_cond_destroy",
            Self::PthreadCondWait => "_pthread_cond_wait",
            Self::PthreadCondSignal => "_pthread_cond_signal",
        }
    }
}

/// Which segment a `DataFixup` addresses: the read-only `__TEXT,__const`
/// (constants, strings) or the writable `__DATA,__bss` (the GC's cells).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum FixupSection {
    Rodata,
    Data,
    Imports,
}

/// An `adrp`+`add` pair in `code` that must be patched to address a byte
/// in `section` (at `data_offset` within it) once the final image layout
/// is known.
#[derive(Clone, Copy, Debug)]
pub(crate) struct DataFixup {
    pub adrp_offset: usize,
    pub add_offset: usize,
    pub data_offset: usize,
    pub section: FixupSection,
}

const PAGE_SIZE: u64 = 16384;
const SIGN_PAGE_SIZE: usize = 4096;
const TEXT_BASE: u64 = 0x1_0000_0000;

const MH_MAGIC_64: u32 = 0xfeed_facf;
const CPU_TYPE_ARM64: u32 = 0x0100_000c;
const MH_EXECUTE: u32 = 2;
const MH_NOUNDEFS: u32 = 0x1;
const MH_DYLDLINK: u32 = 0x4;
const MH_PIE: u32 = 0x0020_0000;

const LC_SEGMENT_64: u32 = 0x19;
const LC_SYMTAB: u32 = 0x2;
const LC_DYSYMTAB: u32 = 0xb;
const LC_LOAD_DYLINKER: u32 = 0xe;
const LC_MAIN: u32 = 0x8000_0028;
const LC_BUILD_VERSION: u32 = 0x32;
const LC_CODE_SIGNATURE: u32 = 0x1d;
const LC_LOAD_DYLIB: u32 = 0xc;
const LC_DYLD_INFO_ONLY: u32 = 0x8000_0022;
const LIBSYSTEM: &[u8] = b"/usr/lib/libSystem.B.dylib\0";
const LOAD_DYLIB_COMMAND_SIZE: u32 = 56;
const DYLD_INFO_COMMAND_SIZE: u32 = 48;

const VM_PROT_READ: u32 = 0x1;
const VM_PROT_WRITE: u32 = 0x2;
const VM_PROT_EXECUTE: u32 = 0x4;

/// `S_ZEROFILL` section type: no file backing, zeroed at map time. The
/// GC's mutable global cells live in a `__DATA,__bss` section of this
/// type, so they add no on-disk bytes and fall outside the code
/// signature's `codeLimit`.
const S_ZEROFILL: u32 = 0x1;

const SEGMENT_COMMAND_SIZE: u32 = 72;
const SECTION_SIZE: u32 = 80;
/// cmd + cmdsize + name offset + "/usr/lib/dyld\0" padded to 8.
const LOAD_DYLINKER_COMMAND_SIZE: u32 = 32;
const MAIN_COMMAND_SIZE: u32 = 24;
const SYMTAB_COMMAND_SIZE: u32 = 24;
const DYSYMTAB_COMMAND_SIZE: u32 = 80;
const BUILD_VERSION_COMMAND_SIZE: u32 = 24;
const CODE_SIGNATURE_COMMAND_SIZE: u32 = 16;
const PLATFORM_MACOS: u32 = 1;
/// 11.0.0 encoded as xxxx.yy.zz nibbles.
const MACOS_11_0: u32 = 0x000b_0000;

const CSMAGIC_EMBEDDED_SIGNATURE: u32 = 0xfade_0cc0;
const CSMAGIC_CODEDIRECTORY: u32 = 0xfade_0c02;
const CSSLOT_CODEDIRECTORY: u32 = 0;
const CD_VERSION_EXEC_SEG: u32 = 0x2_0400;
const CS_ADHOC: u32 = 0x2;
const CS_HASHTYPE_SHA256: u8 = 2;
const CS_EXECSEG_MAIN_BINARY: u64 = 0x1;
const CODE_DIRECTORY_HEADER_SIZE: usize = 88;
const SHA256_SIZE: usize = 32;

fn align_to(value: u64, alignment: u64) -> u64 {
    value.div_ceil(alignment) * alignment
}

struct Writer {
    bytes: Vec<u8>,
}

impl Writer {
    fn u32(&mut self, value: u32) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    fn u64(&mut self, value: u64) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    fn name16(&mut self, name: &str) {
        let mut field = [0u8; 16];
        field[..name.len()].copy_from_slice(name.as_bytes());
        self.bytes.extend_from_slice(&field);
    }

    fn pad_to(&mut self, offset: u64) {
        debug_assert!(self.bytes.len() as u64 <= offset);
        self.bytes.resize(offset as usize, 0);
    }
}

/// Serialize a complete signed Mach-O arm64 executable. `code` starts
/// executing at its first byte; `fixups` are resolved against the
/// final layout before signing.
pub(crate) fn write_executable(
    mut code: Vec<u8>,
    rodata: Vec<u8>,
    fixups: &[DataFixup],
    identifier: &str,
    bss_size: u64,
    imports: &[ImportSymbol],
) -> Vec<u8> {
    // Imports require a file-backed GOT; GC globals require zero-fill BSS.
    // Omit both sections when unused. Without imports, preserve the existing
    // zero-fill-only layout so non-importing executables keep their format.
    let has_imports = !imports.is_empty();
    let has_data = bss_size > 0 || has_imports;
    let data_sections = u32::from(bss_size > 0) + u32::from(has_imports);
    let data_cmd_size = if has_data {
        SEGMENT_COMMAND_SIZE + data_sections * SECTION_SIZE
    } else {
        0
    };
    let sizeofcmds = SEGMENT_COMMAND_SIZE          // __PAGEZERO
        + SEGMENT_COMMAND_SIZE + 2 * SECTION_SIZE  // __TEXT (__text, __const)
        + data_cmd_size                            // __DATA (__bss), if any
        + SEGMENT_COMMAND_SIZE                     // __LINKEDIT
        + LOAD_DYLINKER_COMMAND_SIZE
        + MAIN_COMMAND_SIZE
        + SYMTAB_COMMAND_SIZE
        + DYSYMTAB_COMMAND_SIZE
        + BUILD_VERSION_COMMAND_SIZE
        + CODE_SIGNATURE_COMMAND_SIZE
        + if has_imports { LOAD_DYLIB_COMMAND_SIZE + DYLD_INFO_COMMAND_SIZE } else { 0 };
    let ncmds = 9 + u32::from(has_data) + 2 * u32::from(has_imports);
    let header_end = 32 + u64::from(sizeofcmds);
    let code_offset = align_to(header_end, 16);
    let rodata_offset = align_to(code_offset + code.len() as u64, 8);
    let text_filesize = align_to(rodata_offset + rodata.len() as u64, PAGE_SIZE);
    // Signed file bytes include the GOT and binding/symbol tables, but no BSS.
    let data_filesize = align_to(imports.len() as u64 * 8, PAGE_SIZE);
    let linkedit_offset = text_filesize + data_filesize;
    let linkedit = ImportTables::new(imports, linkedit_offset);
    let signature_offset = linkedit_offset + linkedit.bytes.len() as u64;
    // __DATA maps in VM immediately after __TEXT; __LINKEDIT then moves
    // past __DATA's (page-rounded) vm size.
    let data_vmaddr = TEXT_BASE + text_filesize;
    let data_vmsize = align_to(data_filesize + bss_size, PAGE_SIZE);
    let linkedit_vmaddr = if has_data {
        data_vmaddr + data_vmsize
    } else {
        TEXT_BASE + text_filesize
    };

    for fixup in fixups {
        let section_base = match fixup.section {
            FixupSection::Rodata => TEXT_BASE + rodata_offset,
            // BSS follows any file-backed GOT pages in __DATA.
            FixupSection::Data => data_vmaddr + data_filesize,
            FixupSection::Imports => {
                assert!(
                    fixup.data_offset.is_multiple_of(8) && fixup.data_offset / 8 < imports.len()
                );
                data_vmaddr
            }
        };
        patch_data_fixup(&mut code, code_offset, section_base, fixup);
    }

    let identifier_bytes = identifier.as_bytes();
    let code_slots = (signature_offset as usize).div_ceil(SIGN_PAGE_SIZE);
    let directory_length =
        CODE_DIRECTORY_HEADER_SIZE + identifier_bytes.len() + 1 + code_slots * SHA256_SIZE;
    // SuperBlob header (12) + one blob index (8) + the CodeDirectory.
    let signature_size = (12 + 8 + directory_length) as u64;

    let mut out = Writer {
        bytes: Vec::with_capacity((signature_offset + signature_size) as usize),
    };

    // mach_header_64
    out.u32(MH_MAGIC_64);
    out.u32(CPU_TYPE_ARM64);
    out.u32(0); // CPU_SUBTYPE_ARM64_ALL
    out.u32(MH_EXECUTE);
    out.u32(ncmds);
    out.u32(sizeofcmds);
    out.u32(MH_NOUNDEFS | MH_DYLDLINK | MH_PIE | if has_imports { 0x80 } else { 0 }); // MH_TWOLEVEL
    out.u32(0); // reserved

    // __PAGEZERO
    out.u32(LC_SEGMENT_64);
    out.u32(SEGMENT_COMMAND_SIZE);
    out.name16("__PAGEZERO");
    out.u64(0);
    out.u64(TEXT_BASE);
    out.u64(0);
    out.u64(0);
    out.u32(0);
    out.u32(0);
    out.u32(0);
    out.u32(0);

    // __TEXT: covers the header, the code, and the read-only data.
    out.u32(LC_SEGMENT_64);
    out.u32(SEGMENT_COMMAND_SIZE + 2 * SECTION_SIZE);
    out.name16("__TEXT");
    out.u64(TEXT_BASE);
    out.u64(text_filesize);
    out.u64(0);
    out.u64(text_filesize);
    out.u32(VM_PROT_READ | VM_PROT_EXECUTE);
    out.u32(VM_PROT_READ | VM_PROT_EXECUTE);
    out.u32(2);
    out.u32(0);

    // __TEXT,__text
    out.name16("__text");
    out.name16("__TEXT");
    out.u64(TEXT_BASE + code_offset);
    out.u64(code.len() as u64);
    out.u32(code_offset as u32);
    out.u32(2); // 2^2 alignment
    out.u32(0);
    out.u32(0);
    out.u32(0x8000_0400); // S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS
    out.u32(0);
    out.u32(0);
    out.u32(0);

    // __TEXT,__const
    out.name16("__const");
    out.name16("__TEXT");
    out.u64(TEXT_BASE + rodata_offset);
    out.u64(rodata.len() as u64);
    out.u32(rodata_offset as u32);
    out.u32(3); // 2^3 alignment
    out.u32(0);
    out.u32(0);
    out.u32(0);
    out.u32(0);
    out.u32(0);
    out.u32(0);

    // The GOT is file-backed; BSS begins after its padded file page. This
    // keeps zero-fill globals out of the signed bytes while binding locations
    // are mapped writable before dyld applies eager binds.
    if has_data {
        out.u32(LC_SEGMENT_64);
        out.u32(SEGMENT_COMMAND_SIZE + data_sections * SECTION_SIZE);
        out.name16("__DATA");
        out.u64(data_vmaddr);
        out.u64(data_vmsize);
        out.u64(text_filesize);
        out.u64(data_filesize);
        out.u32(VM_PROT_READ | VM_PROT_WRITE);
        out.u32(VM_PROT_READ | VM_PROT_WRITE);
        out.u32(data_sections);
        out.u32(0);
        if has_imports {
            out.name16("__got");
            out.name16("__DATA");
            out.u64(data_vmaddr);
            out.u64(imports.len() as u64 * 8);
            out.u32(text_filesize as u32);
            out.u32(3);
            out.u32(0);
            out.u32(0);
            out.u32(6); // S_NON_LAZY_SYMBOL_POINTERS
            out.u32(0); // first indirect-symbol-table index
            out.u32(0);
            out.u32(0);
        }
        if bss_size > 0 {
            out.name16("__bss");
            out.name16("__DATA");
            out.u64(data_vmaddr + data_filesize);
            out.u64(bss_size);
            out.u32(0);
            out.u32(3);
            out.u32(0);
            out.u32(0);
            out.u32(S_ZEROFILL);
            out.u32(0);
            out.u32(0);
            out.u32(0);
        }
    }

    // __LINKEDIT holds binding/symbol tables (if any) and the signature.
    let linkedit_size = linkedit.bytes.len() as u64 + signature_size;
    out.u32(LC_SEGMENT_64);
    out.u32(SEGMENT_COMMAND_SIZE);
    out.name16("__LINKEDIT");
    out.u64(linkedit_vmaddr);
    out.u64(align_to(linkedit_size, PAGE_SIZE));
    out.u64(linkedit_offset);
    out.u64(linkedit_size);
    out.u32(VM_PROT_READ);
    out.u32(VM_PROT_READ);
    out.u32(0);
    out.u32(0);

    // LC_LOAD_DYLINKER: the Apple Silicon kernel refuses arm64 main
    // executables that are not dyld-linked (MH_DYLDLINK + dyld are
    // load-time requirements even for images using only raw syscalls).
    out.u32(LC_LOAD_DYLINKER);
    out.u32(LOAD_DYLINKER_COMMAND_SIZE);
    out.u32(12); // name offset
    out.bytes.extend_from_slice(b"/usr/lib/dyld\0\0\0\0\0\0\0");

    // LC_MAIN: dyld jumps to this file offset; the program exits via
    // the exit syscall and never returns into dyld's glue.
    out.u32(LC_MAIN);
    out.u32(MAIN_COMMAND_SIZE);
    out.u64(code_offset); // entryoff
    out.u64(0); // stacksize: default

    if has_imports {
        out.u32(LC_LOAD_DYLIB);
        out.u32(LOAD_DYLIB_COMMAND_SIZE);
        out.u32(24); // dylib name offset
        out.u32(0); // timestamp
        out.u32(0x10000); // current version 1.0.0
        out.u32(0x10000); // compatibility version 1.0.0
        out.bytes.extend_from_slice(LIBSYSTEM);
        out.bytes.resize(
            out.bytes.len() + LOAD_DYLIB_COMMAND_SIZE as usize - 24 - LIBSYSTEM.len(),
            0,
        );

        out.u32(LC_DYLD_INFO_ONLY);
        out.u32(DYLD_INFO_COMMAND_SIZE);
        out.u32(0);
        out.u32(0); // no rebase stream: all internal references are PC-relative
        out.u32(linkedit_offset as u32);
        out.u32(linkedit.bind_size);
        for _ in 0..6 {
            out.u32(0);
        } // no weak/lazy bindings or exports
    }

    // The dynamic symbol table describes exactly the eager import slots.
    out.u32(LC_SYMTAB);
    out.u32(SYMTAB_COMMAND_SIZE);
    out.u32(linkedit.symoff);
    out.u32(imports.len() as u32);
    out.u32(linkedit.stroff);
    out.u32(linkedit.strsize);
    out.u32(LC_DYSYMTAB);
    out.u32(DYSYMTAB_COMMAND_SIZE);
    for field in 0..18 {
        out.u32(match field {
            5 | 13 => imports.len() as u32, // nundefsym, nindirectsyms
            12 => linkedit.indirectoff,
            _ => 0,
        });
    }

    // LC_BUILD_VERSION
    out.u32(LC_BUILD_VERSION);
    out.u32(BUILD_VERSION_COMMAND_SIZE);
    out.u32(PLATFORM_MACOS);
    out.u32(MACOS_11_0); // minos
    out.u32(MACOS_11_0); // sdk
    out.u32(0); // ntools

    // LC_CODE_SIGNATURE
    out.u32(LC_CODE_SIGNATURE);
    out.u32(CODE_SIGNATURE_COMMAND_SIZE);
    out.u32(signature_offset as u32);
    out.u32(signature_size as u32);

    out.pad_to(code_offset);
    out.bytes.extend_from_slice(&code);
    out.pad_to(rodata_offset);
    out.bytes.extend_from_slice(&rodata);
    out.pad_to(linkedit_offset);
    out.bytes.extend_from_slice(&linkedit.bytes);
    out.pad_to(signature_offset);

    let signature = build_adhoc_signature(
        &out.bytes,
        identifier_bytes,
        code_slots,
        directory_length,
        text_filesize,
    );
    debug_assert_eq!(signature.len() as u64, signature_size);
    out.bytes.extend_from_slice(&signature);
    out.bytes
}

fn patch_data_fixup(code: &mut [u8], code_offset: u64, section_base: u64, fixup: &DataFixup) {
    let place = TEXT_BASE + code_offset + fixup.adrp_offset as u64;
    let target = section_base + fixup.data_offset as u64;
    let page_delta = ((target >> 12) as i64 - (place >> 12) as i64) as u64;
    let immlo = (page_delta & 0x3) as u32;
    let immhi = ((page_delta >> 2) & 0x7_ffff) as u32;
    patch_instruction(code, fixup.adrp_offset, |word| {
        word | (immlo << 29) | (immhi << 5)
    });
    let page_offset = (target & 0xfff) as u32;
    patch_instruction(code, fixup.add_offset, |word| word | (page_offset << 10));
}

#[derive(Default)]
struct ImportTables {
    bytes: Vec<u8>,
    bind_size: u32,
    symoff: u32,
    stroff: u32,
    strsize: u32,
    indirectoff: u32,
}

impl ImportTables {
    fn new(imports: &[ImportSymbol], file_offset: u64) -> Self {
        if imports.is_empty() {
            return Self::default();
        }
        let mut bytes = vec![0x11, 0x51]; // dylib ordinal 1; BIND_TYPE_POINTER
        for (index, symbol) in imports.iter().enumerate() {
            assert!(!imports[..index].contains(symbol), "duplicate import slot");
            bytes.push(0x40); // SET_SYMBOL_TRAILING_FLAGS_IMM, no weak flags
            bytes.extend_from_slice(symbol.name().as_bytes());
            bytes.push(0);
            bytes.push(0x72); // SET_SEGMENT_AND_OFFSET_ULEB: __DATA is segment 2
            push_uleb(&mut bytes, index as u64 * 8);
            bytes.push(0x90); // DO_BIND, advances the location by a pointer
        }
        bytes.push(0); // DONE
        let bind_size = bytes.len() as u32;
        bytes.resize(bytes.len().next_multiple_of(8), 0);
        let symoff =
            u32::try_from(file_offset + bytes.len() as u64).expect("Mach-O symoff overflow");
        let mut strings = vec![0];
        for symbol in imports {
            bytes.extend_from_slice(&(strings.len() as u32).to_le_bytes());
            bytes.extend_from_slice(&[1, 0]); // N_UNDF | N_EXT, NO_SECT
            bytes.extend_from_slice(&0x100_u16.to_le_bytes()); // dylib ordinal 1, non-lazy
            bytes.extend_from_slice(&0_u64.to_le_bytes());
            strings.extend_from_slice(symbol.name().as_bytes());
            strings.push(0);
        }
        let stroff =
            u32::try_from(file_offset + bytes.len() as u64).expect("Mach-O stroff overflow");
        let strsize = strings.len() as u32;
        bytes.extend_from_slice(&strings);
        bytes.resize(bytes.len().next_multiple_of(4), 0);
        let indirectoff =
            u32::try_from(file_offset + bytes.len() as u64).expect("Mach-O indirectoff overflow");
        for index in 0..imports.len() {
            bytes.extend_from_slice(&(index as u32).to_le_bytes());
        }
        bytes.resize(bytes.len().next_multiple_of(16), 0); // signature alignment
        Self {
            bytes,
            bind_size,
            symoff,
            stroff,
            strsize,
            indirectoff,
        }
    }
}

fn push_uleb(bytes: &mut Vec<u8>, mut value: u64) {
    loop {
        let low = (value & 0x7f) as u8;
        value >>= 7;
        bytes.push(low | if value == 0 { 0 } else { 0x80 });
        if value == 0 {
            break;
        }
    }
}

fn patch_instruction(code: &mut [u8], offset: usize, patch: impl FnOnce(u32) -> u32) {
    let mut word = [0u8; 4];
    word.copy_from_slice(&code[offset..offset + 4]);
    let patched = patch(u32::from_le_bytes(word));
    code[offset..offset + 4].copy_from_slice(&patched.to_le_bytes());
}

/// Embedded ad-hoc signature: a SuperBlob holding one CodeDirectory
/// whose slots are SHA-256 hashes of each 4096-byte page of the file
/// up to the signature itself. Fields inside the signature blobs are
/// big-endian, unlike the rest of the file.
fn build_adhoc_signature(
    file: &[u8],
    identifier: &[u8],
    code_slots: usize,
    directory_length: usize,
    text_filesize: u64,
) -> Vec<u8> {
    let mut blob = Vec::with_capacity(12 + 8 + directory_length);
    let push32 = |blob: &mut Vec<u8>, value: u32| blob.extend_from_slice(&value.to_be_bytes());
    let push64 = |blob: &mut Vec<u8>, value: u64| blob.extend_from_slice(&value.to_be_bytes());

    push32(&mut blob, CSMAGIC_EMBEDDED_SIGNATURE);
    push32(&mut blob, (12 + 8 + directory_length) as u32);
    push32(&mut blob, 1); // blob count
    push32(&mut blob, CSSLOT_CODEDIRECTORY);
    push32(&mut blob, 20); // CodeDirectory offset within the SuperBlob

    let ident_offset = CODE_DIRECTORY_HEADER_SIZE as u32;
    let hash_offset = ident_offset + identifier.len() as u32 + 1;
    push32(&mut blob, CSMAGIC_CODEDIRECTORY);
    push32(&mut blob, directory_length as u32);
    push32(&mut blob, CD_VERSION_EXEC_SEG);
    push32(&mut blob, CS_ADHOC);
    push32(&mut blob, hash_offset);
    push32(&mut blob, ident_offset);
    push32(&mut blob, 0); // nSpecialSlots
    push32(&mut blob, code_slots as u32);
    push32(&mut blob, file.len() as u32); // codeLimit
    blob.push(SHA256_SIZE as u8);
    blob.push(CS_HASHTYPE_SHA256);
    blob.push(0); // platform
    blob.push(12); // log2(4096) signing page size
    push32(&mut blob, 0); // spare2
    push32(&mut blob, 0); // scatterOffset
    push32(&mut blob, 0); // teamOffset
    push32(&mut blob, 0); // spare3
    push64(&mut blob, 0); // codeLimit64
    push64(&mut blob, 0); // execSegBase (__TEXT starts at file offset 0)
    push64(&mut blob, text_filesize); // execSegLimit
    push64(&mut blob, CS_EXECSEG_MAIN_BINARY);
    blob.extend_from_slice(identifier);
    blob.push(0);
    for chunk in file.chunks(SIGN_PAGE_SIZE) {
        blob.extend_from_slice(&sha256(chunk));
    }
    blob
}

/// FIPS 180-4 SHA-256. The native crates avoid external dependencies
/// (the same reason the ELF writer is hand-rolled), so the digest is
/// implemented here with the standard constants and verified against
/// the FIPS test vectors below.
pub(crate) fn sha256(data: &[u8]) -> [u8; 32] {
    const K: [u32; 64] = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
        0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
        0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
        0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
        0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
        0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
        0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
        0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
        0xc67178f2,
    ];
    let mut state: [u32; 8] = [
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
        0x5be0cd19,
    ];
    let mut message = data.to_vec();
    let bit_length = (data.len() as u64) * 8;
    message.push(0x80);
    while message.len() % 64 != 56 {
        message.push(0);
    }
    message.extend_from_slice(&bit_length.to_be_bytes());

    for block in message.as_chunks::<64>().0 {
        let mut w = [0u32; 64];
        for (index, word) in block.as_chunks::<4>().0.iter().enumerate() {
            w[index] = u32::from_be_bytes([word[0], word[1], word[2], word[3]]);
        }
        for index in 16..64 {
            let s0 = w[index - 15].rotate_right(7)
                ^ w[index - 15].rotate_right(18)
                ^ (w[index - 15] >> 3);
            let s1 = w[index - 2].rotate_right(17)
                ^ w[index - 2].rotate_right(19)
                ^ (w[index - 2] >> 10);
            w[index] = w[index - 16]
                .wrapping_add(s0)
                .wrapping_add(w[index - 7])
                .wrapping_add(s1);
        }
        let [mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut h] = state;
        for index in 0..64 {
            let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let ch = (e & f) ^ ((!e) & g);
            let temp1 = h
                .wrapping_add(s1)
                .wrapping_add(ch)
                .wrapping_add(K[index])
                .wrapping_add(w[index]);
            let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let temp2 = s0.wrapping_add(maj);
            h = g;
            g = f;
            f = e;
            e = d.wrapping_add(temp1);
            d = c;
            c = b;
            b = a;
            a = temp1.wrapping_add(temp2);
        }
        state[0] = state[0].wrapping_add(a);
        state[1] = state[1].wrapping_add(b);
        state[2] = state[2].wrapping_add(c);
        state[3] = state[3].wrapping_add(d);
        state[4] = state[4].wrapping_add(e);
        state[5] = state[5].wrapping_add(f);
        state[6] = state[6].wrapping_add(g);
        state[7] = state[7].wrapping_add(h);
    }
    let mut digest = [0u8; 32];
    for (index, word) in state.iter().enumerate() {
        digest[index * 4..index * 4 + 4].copy_from_slice(&word.to_be_bytes());
    }
    digest
}

#[cfg(test)]
mod tests {
    #[test]
    fn gc_macho_clock_is_connected_to_all_logged_phases() {
        for enabled in [false, true] {
            let image = crate::compile_source_for_target(
                "<gc-clock>",
                "1",
                crate::NativeCompilerConfig {
                    target: crate::NativeTarget::MacosAarch64,
                    gc_log: enabled,
                    ..Default::default()
                },
            )
            .unwrap();
            let ncmds = u32::from_le_bytes(image[16..20].try_into().unwrap());
            let text =
                find_segment(&image, ncmds, "__TEXT").unwrap() + SEGMENT_COMMAND_SIZE as usize;
            let offset =
                u32::from_le_bytes(image[text + 48..text + 52].try_into().unwrap()) as usize;
            let size = u64::from_le_bytes(image[text + 40..text + 48].try_into().unwrap()) as usize;
            let calls = image[offset..offset + size]
                .as_chunks::<4>()
                .0
                .iter()
                .filter(|w| **w == 0xd63f_0200_u32.to_le_bytes())
                .count();
            assert_eq!(calls, if enabled { 6 } else { 0 }); // start/end for each of three phases
            let symbol = b"_clock_gettime\0";
            assert_eq!(image.windows(symbol.len()).any(|s| s == symbol), enabled);
        }
    }
    #[test]
    fn gc_macho_clock_fixture_has_real_import() {
        let image = crate::aarch64::gc_clock_test_image();
        let symbol = b"_clock_gettime\0";
        assert!(image.windows(symbol.len()).any(|s| s == symbol));
        // Optional artifact export for external LLVM/dyld inspection. Tests do
        // not mistake structural parsing on Linux for execution on macOS.
        if let Some(path) = std::env::var_os("KLASSIC_GC_CLOCK_IMAGE") {
            std::fs::write(path, image).unwrap();
        }
    }

    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    #[test]
    fn gc_macho_clock_executes_10000_monotonic_samples() {
        use std::os::unix::fs::PermissionsExt;
        let path = std::env::temp_dir().join(format!(
            "klassic-clock-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::write(&path, crate::aarch64::gc_clock_test_image()).unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
        let output = crate::gc_atomic_tests::run_with_deadline(&path);
        std::fs::remove_file(path).unwrap();
        assert!(
            output.status.success(),
            "macOS clock/loader execution {:?}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }
    #[test]
    fn gc_macho_import_layout_bindings_fixups_and_signature_agree() {
        for bss_size in [0, PAGE_SIZE * 2] {
            let code: Vec<u8> = [
                0x90000010_u32,
                0x91000210,
                0xf9400210,
                0xd63f0200,
                0x90000001,
                0x91000021,
            ]
            .into_iter()
            .flat_map(u32::to_le_bytes)
            .collect();
            let mut fixups = vec![DataFixup {
                adrp_offset: 0,
                add_offset: 4,
                data_offset: 0,
                section: FixupSection::Imports,
            }];
            if bss_size != 0 {
                fixups.push(DataFixup {
                    adrp_offset: 16,
                    add_offset: 20,
                    data_offset: 8,
                    section: FixupSection::Data,
                });
            }
            let image = write_executable(
                code,
                vec![],
                &fixups,
                "binding-test",
                bss_size,
                &[ImportSymbol::ClockGettime],
            );
            let u32_at = |p| u32::from_le_bytes(image[p..p + 4].try_into().unwrap()) as usize;
            let u64_at = |p| u64::from_le_bytes(image[p..p + 8].try_into().unwrap());
            let ncmds = u32_at(16) as u32;
            let command = |kind| {
                let mut p = 32;
                for _ in 0..ncmds {
                    if u32_at(p) == kind as usize {
                        return p;
                    }
                    p += u32_at(p + 4);
                }
                panic!("missing load command {kind:x}");
            };
            let data = find_segment(&image, ncmds, "__DATA").unwrap();
            let linkedit = find_segment(&image, ncmds, "__LINKEDIT").unwrap();
            let got = data + 72;
            let got_vm = u64_at(got + 32);
            let got_file = u32_at(got + 48);
            assert_eq!(&image[got..got + 5], b"__got");
            assert_eq!(u32_at(got + 64), 6);
            assert_eq!(u64_at(got + 40), 8);
            assert_eq!(u64_at(data + 48), PAGE_SIZE);
            assert_eq!(u32_at(data + 60), 3); // writable, not executable
            assert_eq!(u32_at(data + 64), 1 + usize::from(bss_size != 0));
            assert_eq!(&image[got_file..got_file + 8], &[0; 8]);
            assert_eq!(u64_at(linkedit + 24), u64_at(data + 24) + u64_at(data + 32));
            let entry = u64_at(command(LC_MAIN) + 8) as usize;
            assert_eq!(decode_adrp_add(&image, entry), got_vm);
            if bss_size != 0 {
                let bss = got + 80;
                assert_eq!(u32_at(bss + 64), S_ZEROFILL as usize);
                assert_eq!(u64_at(bss + 32), got_vm + PAGE_SIZE);
                assert_eq!(decode_adrp_add(&image, entry + 16), u64_at(bss + 32) + 8);
            }
            let dyld = command(LC_DYLD_INFO_ONLY);
            assert_eq!(u32_at(dyld + 8), 0); // no rebases of zero initial GOT entries
            let bind = &image[u32_at(dyld + 16)..u32_at(dyld + 16) + u32_at(dyld + 20)];
            assert_eq!(&bind[..3], &[0x11, 0x51, 0x40]); // ordinal, type, symbol
            let end = bind[3..].iter().position(|&b| b == 0).unwrap() + 3;
            assert_eq!(&bind[3..end], b"_clock_gettime");
            assert_eq!(&bind[end + 1..], &[0x72, 0, 0x90, 0]); // __DATA+0, bind, done
            let symtab = command(LC_SYMTAB);
            let symbol = u32_at(symtab + 8);
            let strings = u32_at(symtab + 16);
            assert_eq!(u32_at(symtab + 12), 1);
            assert_eq!(&image[symbol + 4..symbol + 8], &[1, 0, 0, 1]);
            assert_eq!(
                &image[strings + u32_at(symbol)..strings + u32_at(symtab + 20)],
                b"_clock_gettime\0"
            );
            let dynamic = command(LC_DYSYMTAB);
            assert_eq!(u32_at(dynamic + 28), 1); // nundefsym
            assert_eq!(u32_at(dynamic + 60), 1); // nindirectsyms
            assert_eq!(u32_at(u32_at(dynamic + 56)), 0); // symbol index for GOT[0]
            let sigcmd = command(LC_CODE_SIGNATURE);
            let signature = u32_at(sigcmd + 8);
            let directory = signature + 20;
            let be32 = |p| u32::from_be_bytes(image[p..p + 4].try_into().unwrap()) as usize;
            assert_eq!(be32(directory + 32), signature);
            assert_eq!(be32(directory + 28), signature.div_ceil(SIGN_PAGE_SIZE));
            assert_eq!(
                u64_at(linkedit + 40) + u64_at(linkedit + 48),
                image.len() as u64
            );
            let hashes = directory + be32(directory + 16);
            for (i, page) in image[..signature].chunks(SIGN_PAGE_SIZE).enumerate() {
                assert_eq!(
                    &image[hashes + i * 32..hashes + (i + 1) * 32],
                    &sha256(page)
                );
            }
        }
    }
    #[test]
    fn imports_have_dyld_binding_metadata() {
        let image = super::write_executable(
            vec![0xc0, 0x03, 0x5f, 0xd6],
            vec![],
            &[],
            "imports",
            16,
            &[super::ImportSymbol::ClockGettime],
        );
        assert!(
            image
                .windows(super::LIBSYSTEM.len())
                .any(|s| s == super::LIBSYSTEM)
        );
        let symbol = b"_clock_gettime\0";
        assert!(image.windows(symbol.len()).any(|s| s == symbol));
    }
    use super::*;

    fn hex(bytes: &[u8]) -> String {
        bytes.iter().map(|byte| format!("{byte:02x}")).collect()
    }

    #[test]
    fn sha256_matches_fips_vectors() {
        assert_eq!(
            hex(&sha256(b"")),
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        );
        assert_eq!(
            hex(&sha256(b"abc")),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );
        assert_eq!(
            hex(&sha256(
                b"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
            )),
            "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
        );
        // Cross the one-block boundary (padding path).
        assert_eq!(
            hex(&sha256(&[0x61u8; 64])),
            "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"
        );
    }

    #[test]
    fn executable_layout_is_structurally_sound() {
        let code = vec![0xd2, 0x80, 0x00, 0x40]; // mov x0, #2
        let rodata = b"hi\n".to_vec();
        let image = write_executable(code, rodata, &[], "klassic", 0, &[]);

        assert_eq!(&image[0..4], &MH_MAGIC_64.to_le_bytes());
        assert_eq!(&image[4..8], &CPU_TYPE_ARM64.to_le_bytes());
        assert_eq!(&image[12..16], &MH_EXECUTE.to_le_bytes());
        let ncmds = u32::from_le_bytes(image[16..20].try_into().unwrap());
        assert_eq!(ncmds, 9);
        let flags = u32::from_le_bytes(image[24..28].try_into().unwrap());
        assert_eq!(flags, MH_NOUNDEFS | MH_DYLDLINK | MH_PIE);
        assert!(
            image.windows(14).any(|window| window == b"/usr/lib/dyld\0"),
            "LC_LOAD_DYLINKER path missing"
        );

        // The signature begins at the 16K-aligned end of __TEXT and
        // opens with the SuperBlob magic (big-endian).
        let signature_offset = PAGE_SIZE as usize;
        assert_eq!(
            &image[signature_offset..signature_offset + 4],
            &CSMAGIC_EMBEDDED_SIGNATURE.to_be_bytes()
        );
        let directory = &image[signature_offset + 20..];
        assert_eq!(&directory[0..4], &CSMAGIC_CODEDIRECTORY.to_be_bytes());
        let code_slots = u32::from_be_bytes(directory[28..32].try_into().unwrap());
        assert_eq!(code_slots as usize, signature_offset / SIGN_PAGE_SIZE);
        let code_limit = u32::from_be_bytes(directory[32..36].try_into().unwrap());
        assert_eq!(code_limit as usize, signature_offset);

        // First page hash must match a recomputation over the file.
        let hash_offset = u32::from_be_bytes(directory[16..20].try_into().unwrap()) as usize;
        let first_hash = &directory[hash_offset..hash_offset + SHA256_SIZE];
        assert_eq!(first_hash, &sha256(&image[..SIGN_PAGE_SIZE]));
    }

    /// Walk the load commands and return the file offset of the
    /// `LC_SEGMENT_64` whose segname matches, or `None`.
    fn find_segment(image: &[u8], ncmds: u32, segname: &str) -> Option<usize> {
        let mut off = 32usize;
        for _ in 0..ncmds {
            let cmd = u32::from_le_bytes(image[off..off + 4].try_into().unwrap());
            let cmdsize = u32::from_le_bytes(image[off + 4..off + 8].try_into().unwrap()) as usize;
            if cmd == LC_SEGMENT_64 {
                let name_end = image[off + 8..off + 24]
                    .iter()
                    .position(|&b| b == 0)
                    .unwrap_or(16);
                if &image[off + 8..off + 8 + name_end] == segname.as_bytes() {
                    return Some(off);
                }
            }
            off += cmdsize;
        }
        None
    }

    #[test]
    fn writable_data_segment_is_gated_and_structurally_sound() {
        let code = vec![0xd2, 0x80, 0x00, 0x40]; // mov x0, #2
        let bss_size = 30 * 8; // ~30 zero-init GC cells
        let image = write_executable(code, b"hi\n".to_vec(), &[], "klassic", bss_size, &[]);

        // The data segment bumps the command count.
        let ncmds = u32::from_le_bytes(image[16..20].try_into().unwrap());
        assert_eq!(ncmds, 10);

        // __TEXT gives us its (page-aligned) vmsize = text_filesize.
        let text = find_segment(&image, ncmds, "__TEXT").expect("__TEXT present");
        let text_filesize = u64::from_le_bytes(image[text + 32..text + 40].try_into().unwrap());

        // __DATA: writable, one section, mapped right after __TEXT.
        let data = find_segment(&image, ncmds, "__DATA").expect("__DATA present");
        let data_vmaddr = u64::from_le_bytes(image[data + 24..data + 32].try_into().unwrap());
        let data_vmsize = u64::from_le_bytes(image[data + 32..data + 40].try_into().unwrap());
        let data_filesize = u64::from_le_bytes(image[data + 48..data + 56].try_into().unwrap());
        let initprot = u32::from_le_bytes(image[data + 60..data + 64].try_into().unwrap());
        let nsects = u32::from_le_bytes(image[data + 64..data + 68].try_into().unwrap());
        assert_eq!(data_vmaddr, TEXT_BASE + text_filesize);
        assert_eq!(data_vmsize, align_to(bss_size, PAGE_SIZE));
        assert_eq!(data_filesize, 0, "zero-fill: no on-disk bytes");
        assert_eq!(initprot, VM_PROT_READ | VM_PROT_WRITE);
        assert_eq!(nsects, 1);

        // __DATA,__bss section: S_ZEROFILL, no file offset, correct size.
        let sect = data + SEGMENT_COMMAND_SIZE as usize;
        assert_eq!(&image[sect..sect + 5], b"__bss");
        let sect_size = u64::from_le_bytes(image[sect + 40..sect + 48].try_into().unwrap());
        let sect_offset = u32::from_le_bytes(image[sect + 48..sect + 52].try_into().unwrap());
        let sect_flags = u32::from_le_bytes(image[sect + 64..sect + 68].try_into().unwrap());
        assert_eq!(sect_size, bss_size);
        assert_eq!(sect_offset, 0);
        assert_eq!(sect_flags, S_ZEROFILL);

        // __LINKEDIT moves past __DATA in the address space.
        let linkedit = find_segment(&image, ncmds, "__LINKEDIT").expect("__LINKEDIT present");
        let le_vmaddr = u64::from_le_bytes(image[linkedit + 24..linkedit + 32].try_into().unwrap());
        assert_eq!(le_vmaddr, data_vmaddr + data_vmsize);

        // The zero-fill segment adds no file bytes, so the signature still
        // begins at text_filesize and its first-page hash is valid.
        let signature_offset = text_filesize as usize;
        assert_eq!(
            &image[signature_offset..signature_offset + 4],
            &CSMAGIC_EMBEDDED_SIGNATURE.to_be_bytes()
        );
        let directory = &image[signature_offset + 20..];
        let hash_offset = u32::from_be_bytes(directory[16..20].try_into().unwrap()) as usize;
        let first_hash = &directory[hash_offset..hash_offset + SHA256_SIZE];
        assert_eq!(first_hash, &sha256(&image[..SIGN_PAGE_SIZE]));
    }

    #[test]
    fn data_fixups_resolve_page_and_offset() {
        // adrp x1, <data>; add x1, x1, #<pageoff> with the data only a
        // few bytes after the code: same 4K page, so adrp imm stays 0
        // and add receives the in-page byte offset.
        let code = vec![
            0x01, 0x00, 0x00, 0x90, // adrp x1, 0 (placeholder)
            0x21, 0x00, 0x00, 0x91, // add x1, x1, #0 (placeholder)
        ];
        let fixups = [DataFixup {
            adrp_offset: 0,
            add_offset: 4,
            data_offset: 0,
            section: FixupSection::Rodata,
        }];
        let image = write_executable(code, b"x".to_vec(), &fixups, "klassic", 0, &[]);
        // Locate the code from the layout formula: header (32) +
        // sizeofcmds, aligned to 16.
        let sizeofcmds = u32::from_le_bytes(image[20..24].try_into().unwrap()) as u64;
        let code_offset = align_to(32 + sizeofcmds, 16) as usize;
        let adrp = u32::from_le_bytes(image[code_offset..code_offset + 4].try_into().unwrap());
        assert_eq!(adrp, 0x9000_0001, "same page → zero page delta");
        let add = u32::from_le_bytes(image[code_offset + 4..code_offset + 8].try_into().unwrap());
        let rodata_offset = align_to(code_offset as u64 + 8, 8) as u32;
        assert_eq!(add, 0x9100_0021 | (rodata_offset << 10));
    }

    /// Decode an `adrp xd, <page>` + `add xd, xd, #<off>` pair back to the
    /// absolute address it computes, independent of the writer's formula.
    fn decode_adrp_add(image: &[u8], adrp_at: usize) -> u64 {
        let adrp = u32::from_le_bytes(image[adrp_at..adrp_at + 4].try_into().unwrap());
        let add = u32::from_le_bytes(image[adrp_at + 4..adrp_at + 8].try_into().unwrap());
        let immlo = ((adrp >> 29) & 0x3) as u64;
        let immhi = ((adrp >> 5) & 0x7_ffff) as u64;
        let mut page_delta = (immhi << 2) | immlo; // 21-bit signed
        if page_delta & (1 << 20) != 0 {
            page_delta |= !0 << 21; // sign-extend
        }
        let place = TEXT_BASE + adrp_at as u64;
        let target_page = (place & !0xfff).wrapping_add(page_delta.wrapping_shl(12));
        let imm12 = ((add >> 10) & 0xfff) as u64;
        target_page + imm12
    }

    #[test]
    fn data_section_fixups_resolve_against_data_segment() {
        // adrp/add addressing the second GC cell (byte offset 8) in
        // __DATA,__bss. Its target is a full segment away from the code,
        // so the adrp page delta is non-zero (unlike the rodata case).
        let code = vec![
            0x01, 0x00, 0x00, 0x90, // adrp x1, 0 (placeholder)
            0x21, 0x00, 0x00, 0x91, // add x1, x1, #0 (placeholder)
        ];
        let fixups = [DataFixup {
            adrp_offset: 0,
            add_offset: 4,
            data_offset: 8,
            section: FixupSection::Data,
        }];
        let image = write_executable(code, b"x".to_vec(), &fixups, "klassic", 16, &[]);

        let ncmds = u32::from_le_bytes(image[16..20].try_into().unwrap());
        let text = find_segment(&image, ncmds, "__TEXT").unwrap();
        let text_filesize = u64::from_le_bytes(image[text + 32..text + 40].try_into().unwrap());
        let data_vmaddr = TEXT_BASE + text_filesize;

        let sizeofcmds = u32::from_le_bytes(image[20..24].try_into().unwrap()) as u64;
        let code_offset = align_to(32 + sizeofcmds, 16) as usize;
        let resolved = decode_adrp_add(&image, code_offset);
        assert_eq!(resolved, data_vmaddr + 8);
    }
}
