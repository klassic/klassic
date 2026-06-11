//! Mach-O arm64 executable writer for the aarch64-apple-darwin
//! direct backend. Mirrors the ELF writer's role: codegen hands over
//! flat code/rodata blobs plus adrp/add data fixups and this module
//! serializes a complete, runnable executable. Darwin-specific
//! obligations live here: the Apple Silicon kernel only executes
//! arm64 mains that are dyld-linked (`MH_DYLDLINK` +
//! `LC_LOAD_DYLINKER` + `LC_MAIN`; fully static `LC_UNIXTHREAD`
//! images are rejected with EBADEXEC) and carry a valid embedded
//! ad-hoc code signature (a SHA-256 CodeDirectory). dyld also needs
//! empty `LC_SYMTAB` / `LC_DYSYMTAB` to not crash. The generated
//! code still imports nothing — it talks to the kernel through
//! `svc #0x80` exactly like the ELF backend talks through `syscall`
//! — and no external `codesign` / `cc` / `ld` is involved, keeping
//! the no-toolchain policy of the direct backends.

/// An `adrp`+`add` pair in `code` that must be patched to address a
/// byte in `rodata` once the final image layout is known.
#[derive(Clone, Copy, Debug)]
pub(crate) struct DataFixup {
    pub adrp_offset: usize,
    pub add_offset: usize,
    pub data_offset: usize,
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

const VM_PROT_READ: u32 = 0x1;
const VM_PROT_EXECUTE: u32 = 0x4;

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
) -> Vec<u8> {
    let sizeofcmds = SEGMENT_COMMAND_SIZE          // __PAGEZERO
        + SEGMENT_COMMAND_SIZE + 2 * SECTION_SIZE  // __TEXT (__text, __const)
        + SEGMENT_COMMAND_SIZE                     // __LINKEDIT
        + LOAD_DYLINKER_COMMAND_SIZE
        + MAIN_COMMAND_SIZE
        + SYMTAB_COMMAND_SIZE
        + DYSYMTAB_COMMAND_SIZE
        + BUILD_VERSION_COMMAND_SIZE
        + CODE_SIGNATURE_COMMAND_SIZE;
    let ncmds = 9u32;
    let header_end = 32 + u64::from(sizeofcmds);
    let code_offset = align_to(header_end, 16);
    let rodata_offset = align_to(code_offset + code.len() as u64, 8);
    let text_filesize = align_to(rodata_offset + rodata.len() as u64, PAGE_SIZE);
    let signature_offset = text_filesize;

    for fixup in fixups {
        patch_data_fixup(&mut code, code_offset, rodata_offset, fixup);
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
    out.u32(MH_NOUNDEFS | MH_DYLDLINK | MH_PIE);
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

    // __LINKEDIT: holds exactly the code signature.
    out.u32(LC_SEGMENT_64);
    out.u32(SEGMENT_COMMAND_SIZE);
    out.name16("__LINKEDIT");
    out.u64(TEXT_BASE + text_filesize);
    out.u64(align_to(signature_size, PAGE_SIZE));
    out.u64(signature_offset);
    out.u64(signature_size);
    out.u32(VM_PROT_READ);
    out.u32(VM_PROT_READ);
    out.u32(0);
    out.u32(0);

    // LC_LOAD_DYLINKER: the Apple Silicon kernel refuses arm64 main
    // executables that are not dyld-linked (MH_DYLDLINK + dyld are
    // load-time requirements even though the generated code never
    // imports a dylib and talks to the kernel via raw syscalls).
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

    // Empty LC_SYMTAB / LC_DYSYMTAB: dyld dereferences both
    // unconditionally and crashes when they are absent.
    out.u32(LC_SYMTAB);
    out.u32(SYMTAB_COMMAND_SIZE);
    out.u32(0); // symoff
    out.u32(0); // nsyms
    out.u32(0); // stroff
    out.u32(0); // strsize

    out.u32(LC_DYSYMTAB);
    out.u32(DYSYMTAB_COMMAND_SIZE);
    for _ in 0..18 {
        out.u32(0);
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

fn patch_data_fixup(code: &mut [u8], code_offset: u64, rodata_offset: u64, fixup: &DataFixup) {
    let place = TEXT_BASE + code_offset + fixup.adrp_offset as u64;
    let target = TEXT_BASE + rodata_offset + fixup.data_offset as u64;
    let page_delta = ((target >> 12) as i64 - (place >> 12) as i64) as u64;
    let immlo = (page_delta & 0x3) as u32;
    let immhi = ((page_delta >> 2) & 0x7_ffff) as u32;
    patch_instruction(code, fixup.adrp_offset, |word| {
        word | (immlo << 29) | (immhi << 5)
    });
    let page_offset = (target & 0xfff) as u32;
    patch_instruction(code, fixup.add_offset, |word| word | (page_offset << 10));
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

    for block in message.chunks_exact(64) {
        let mut w = [0u32; 64];
        for (index, word) in block.chunks_exact(4).enumerate() {
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
        let image = write_executable(code, rodata, &[], "klassic");

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
        }];
        let image = write_executable(code, b"x".to_vec(), &fixups, "klassic");
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
}
