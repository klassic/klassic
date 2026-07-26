//! Direct AArch64 backend for `aarch64-apple-darwin` (Apple Silicon
//! macOS). Like the x86_64 ELF backend it emits machine code and the
//! executable container directly — no `cc` / `as` / `ld` / `codesign`
//! — and like that backend's early history it starts from a small
//! vertical slice and grows. The current subset covers top-level
//! `Int` / `Bool` expressions (arithmetic, comparisons, short-circuit
//! logic), `val` / `mutable` locals with assignment, `if` / `while`,
//! and `println` of runtime `Int` / `Bool` values plus string and
//! double literals. Everything else fails with a source-located
//! diagnostic, never wrong code. Generated code is
//! position-independent (`adrp`+`add` for data, relative branches)
//! because the kernel slides `MH_PIE` images.
//!
//! Darwin arm64 syscall convention: number in `x16`, arguments in
//! `x0..`, trap via `svc #0x80`. BSD numbers: `exit` = 1, `write` = 4.

use std::collections::HashMap;

use klassic_span::{Diagnostic, Span};
use klassic_syntax::{BinaryOp, Expr, MatchArm, Pattern, StringPart, UnaryOp};

use crate::macho::{self, DataFixup, FixupSection};
use crate::portable_asm;

const SYS_EXIT: u16 = 1;
const SYS_WRITE: u16 = 4;
/// Darwin `read`.
const SYS_READ: u16 = 3;
/// Darwin `open`.
const SYS_OPEN: u16 = 5;
/// Darwin `close`.
const SYS_CLOSE: u16 = 6;
/// Darwin `unlink`.
const SYS_UNLINK: u16 = 10;
/// Darwin `access(path, mode)`. `mode = 0` (`F_OK`) checks existence.
const SYS_ACCESS: u16 = 33;
/// Darwin `rmdir`.
const SYS_RMDIR: u16 = 137;
/// Darwin `mkdir`.
const SYS_MKDIR: u16 = 136;
/// Darwin `fstatat64` -- used with `AT_FDCWD` for a plain `stat`.
const SYS_FSTATAT64: u16 = 470;
/// Darwin `rename`.
const SYS_RENAME: u16 = 128;
/// Darwin `gettimeofday`. `syscalls.master` (apple-oss-distributions/xnu)
/// defines this as a **3-argument** syscall marked `NO_SYSCALL_STUB`:
/// `int gettimeofday(struct timeval *tp, struct timezone *tzp,
/// uint64_t *mach_absolute_time)`. An earlier attempt at this builtin
/// called it with only 2 arguments (leaving x2 whatever it happened
/// to hold), which produced inconsistent real-hardware behavior
/// across two CI runs of the same binary depending on call order --
/// a clean syscall-failure abort in one ordering, a SIGSEGV in
/// another, a hallmark of the kernel misinterpreting a garbage x2 as
/// a pointer to write `mach_absolute_time` through. This time x2 is
/// explicitly zeroed (NULL) before the trap, matching the real ABI.
const SYS_GETTIMEOFDAY: u16 = 116;
/// Darwin's `AT_FDCWD` is `-2`, not Linux's `-100` (bsd/sys/fcntl.h).
const AT_FDCWD: i64 = -2;
/// Darwin's `O_*` open flags (bsd/sys/fcntl.h) -- numerically
/// different from Linux's; reusing Linux values would silently
/// corrupt file-open semantics.
const O_RDONLY: u64 = 0;
const O_WRONLY: u64 = 1;
const O_APPEND: u64 = 0x8;
const O_CREAT: u64 = 0x200;
const O_TRUNC: u64 = 0x400;
/// `0o644`: rw-r--r--, the default mode for a newly created file.
const DEFAULT_FILE_MODE: u64 = 0o644;
/// `0o755`: rwxr-xr-x, the default mode for a newly created directory.
const DEFAULT_DIR_MODE: u64 = 0o755;
/// Darwin `mmap`. Heap segments come straight from the kernel.
/// M9: whether the collector stays non-moving. `false` turns on
/// evacuation -- `relocate_start` advances into the Relocate phase instead
/// of returning to Idle, sparse regions become from-space, the load
/// barrier's slow path evacuates on demand and self-heals, and the
/// forwarding-word paths in `mark_visit` / `gc_load_barrier_slow` (whose
/// bit test is the first real user of the AArch64 `bt` lowering) go live.
/// The mutator is ready for it: every routine re-derives interior pointers
/// from a rooted slot after an allocation rather than carrying them across,
/// and the shadow stack holds slot *addresses*, so `relocate_fix_roots`
/// rewrites a moved reference where the mutator will next read it.
const GC_EVAC_OFF: bool = false;

const SYS_MMAP: u16 = 197;
const STDOUT_FD: u64 = 1;
const STDERR_FD: u64 = 2;
/// One bump-allocator segment. Exhaustion mmaps a fresh segment (the
/// old one leaks until the backend grows a collector — same place the
/// x86_64 backend started).
/// Darwin MAP_ANON | MAP_PRIVATE.
const MMAP_ANON_PRIVATE: u64 = 0x1002;
const PROT_READ_WRITE: u64 = 3;

/// AArch64 general-purpose registers used by the subset. The numeric
/// value is the register number in instruction encodings; 31 encodes
/// `xzr` or `sp` depending on position and is handled inside the
/// emission helpers rather than exposed here.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Reg {
    X0 = 0,
    X1 = 1,
    X2 = 2,
    X3 = 3,
    X4 = 4,
    X5 = 5,
    X6 = 6,
    X7 = 7,
    X8 = 8,
    X9 = 9,
    X10 = 10,
    X11 = 11,
    X12 = 12,
    /// Darwin syscall number register.
    X16 = 16,
    /// Reserved (formerly the bump allocator's next/end pointers, which
    /// the garbage collector's `gc_alloc` and its `__DATA` cells replaced).
    #[allow(dead_code)]
    X19 = 19,
    /// Reserved; see `X19`.
    #[allow(dead_code)]
    X20 = 20,
    /// Callee-saved: `argc`, captured from dyld's `LC_MAIN` entry.
    X21 = 21,
    /// Callee-saved: `argv`, captured from dyld's `LC_MAIN` entry.
    X22 = 22,
    /// Callee-saved: `envp`, captured from dyld's `LC_MAIN` entry.
    X23 = 23,
    /// Callee-saved: GC load-barrier colour-strip mask (`ColorStrip`).
    /// Seeded once at startup; the portable collector keeps it live.
    #[allow(dead_code)]
    X24 = 24,
    /// Callee-saved: GC current good colour (`GoodColor`).
    #[allow(dead_code)]
    X25 = 25,
    /// Callee-saved: GC bad-colour test mask (`BadMask`).
    #[allow(dead_code)]
    X26 = 26,
    /// The frame pointer, as a base register for the GC's rbp-relative
    /// frame slots (`[x29, #-offset]`). The rest of the backend addresses
    /// the frame through the bare `FP` const and `mov_fp_sp`.
    #[allow(dead_code)]
    X29 = 29,
}

/// AAPCS64 integer argument registers, in order.
const ARG_REGS: [Reg; 8] = [
    Reg::X0,
    Reg::X1,
    Reg::X2,
    Reg::X3,
    Reg::X4,
    Reg::X5,
    Reg::X6,
    Reg::X7,
];

/// Frame pointer register number: locals live at `[x29, #offset]` so
/// the operand stack can move `sp` freely. Only ever a base register,
/// so it stays out of the `Reg` enum.
const FP: u32 = 29;

/// AArch64 condition codes (B.cond / CSINC encodings).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Cond {
    Eq = 0,
    Ne = 1,
    /// Unsigned "higher or same" (carry set). Also the GC port's
    /// `AboveOrEqual`. Same encoding as ARM `CS`. Inert until the
    /// PortableAsm condition mapping (M4) constructs it.
    #[allow(dead_code)]
    Hs = 2,
    /// Carry clear. Darwin's `svc #0x80` convention signals a
    /// *successful* syscall this way (carry set = failure, x0 holds
    /// the positive errno) — the mirror image of Linux's
    /// negative-rax convention. Also the GC port's unsigned `Below`.
    Cc = 3,
    /// After `fcmp`: strictly less, unordered fails — the float `<`.
    Mi = 4,
    /// Unsigned "higher" (strictly greater). The GC port's `Above`.
    /// Inert until the PortableAsm condition mapping (M4).
    #[allow(dead_code)]
    Hi = 8,
    /// After `fcmp`: less-or-equal, unordered fails — the float `<=`.
    Ls = 9,
    Ge = 10,
    Lt = 11,
    Gt = 12,
    Le = 13,
}

impl Cond {
    /// The AArch64 condition encoding flips the polarity in the low
    /// bit, which `cset` (alias of `csinc` with the inverted
    /// condition) relies on.
    fn inverted_bits(self) -> u32 {
        (self as u32) ^ 1
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Label(usize);

/// Handle to a zero-initialized cell in the writable `__DATA,__bss`
/// section; the value is the byte offset from the segment base. The
/// AArch64 analog of the x86-64 backend's data label.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct DataLabel(usize);

enum BranchKind {
    /// `b label` — 26-bit signed word offset.
    Unconditional,
    /// `bl label` — 26-bit signed word offset, link register.
    Link,
    /// `b.cond label` — 19-bit signed word offset.
    Conditional(Cond),
    /// `cbz xreg, label`.
    CompareZero(Reg),
    /// `cbnz xreg, label`.
    CompareNonZero(Reg),
}

struct BranchFixup {
    code_offset: usize,
    label: Label,
    kind: BranchKind,
}

#[derive(Default)]
struct Assembler {
    code: Vec<u8>,
    rodata: Vec<u8>,
    fixups: Vec<DataFixup>,
    labels: Vec<Option<usize>>,
    branches: Vec<BranchFixup>,
    /// Total bytes reserved in `__DATA,__bss` (the GC's zero-init cells).
    /// Zero for pre-GC programs, so no writable segment is emitted.
    bss_len: usize,
    /// Set by the PortableAsm `bt_reg_imm8` lowering (which becomes a
    /// flag-setting `tst reg, #(1<<bit)` → Z = (bit == 0)); the next
    /// `jcc_label` consumes it to remap the x86 carry-based condition to
    /// the AArch64 zero-based one. The GC always emits the branch
    /// immediately after the bit test.
    bt_pending: bool,
}

impl Assembler {
    fn word(&mut self, instruction: u32) {
        self.code.extend_from_slice(&instruction.to_le_bytes());
    }

    fn new_label(&mut self) -> Label {
        self.labels.push(None);
        Label(self.labels.len() - 1)
    }

    fn bind(&mut self, label: Label) {
        debug_assert!(self.labels[label.0].is_none(), "label bound twice");
        self.labels[label.0] = Some(self.code.len());
    }

    fn branch(&mut self, label: Label, kind: BranchKind) {
        self.branches.push(BranchFixup {
            code_offset: self.code.len(),
            label,
            kind,
        });
        // Placeholder opcodes; offsets are patched in `finish`.
        match self.branches.last().expect("just pushed").kind {
            BranchKind::Unconditional => self.word(0x1400_0000),
            BranchKind::Link => self.word(0x9400_0000),
            BranchKind::Conditional(cond) => self.word(0x5400_0000 | cond as u32),
            BranchKind::CompareZero(reg) => self.word(0xb400_0000 | reg as u32),
            BranchKind::CompareNonZero(reg) => self.word(0xb500_0000 | reg as u32),
        }
    }

    /// Resolve every recorded branch against its bound label.
    fn finish(&mut self) {
        for fixup in &self.branches {
            let target = self.labels[fixup.label.0].expect("branch target label never bound");
            let delta_words = (target as i64 - fixup.code_offset as i64) / 4;
            let mut word_bytes = [0u8; 4];
            word_bytes.copy_from_slice(&self.code[fixup.code_offset..fixup.code_offset + 4]);
            let word = u32::from_le_bytes(word_bytes);
            let patched = match fixup.kind {
                BranchKind::Unconditional | BranchKind::Link => {
                    word | ((delta_words as u32) & 0x03ff_ffff)
                }
                BranchKind::Conditional(_)
                | BranchKind::CompareZero(_)
                | BranchKind::CompareNonZero(_) => word | (((delta_words as u32) & 0x7_ffff) << 5),
            };
            self.code[fixup.code_offset..fixup.code_offset + 4]
                .copy_from_slice(&patched.to_le_bytes());
        }
        self.branches.clear();
    }

    /// `movz xd, #imm16, lsl #(16 * shift)`
    fn movz(&mut self, reg: Reg, imm16: u16, shift: u32) {
        self.word(0xd280_0000 | (shift << 21) | (u32::from(imm16) << 5) | reg as u32);
    }

    /// `movk xd, #imm16, lsl #(16 * shift)`
    fn movk(&mut self, reg: Reg, imm16: u16, shift: u32) {
        self.word(0xf280_0000 | (shift << 21) | (u32::from(imm16) << 5) | reg as u32);
    }

    /// Materialize an arbitrary 64-bit constant. Negative values get
    /// the movn-free simple form: movz/movk all four halfwords.
    fn mov_imm64(&mut self, reg: Reg, value: u64) {
        self.movz(reg, value as u16, 0);
        for shift in 1..4u32 {
            let part = (value >> (16 * shift)) as u16;
            if part != 0 {
                self.movk(reg, part, shift);
            }
        }
    }

    /// `mov xd, xm` (orr xd, xzr, xm)
    fn mov_reg(&mut self, dst: Reg, src: Reg) {
        self.word(0xaa00_03e0 | ((src as u32) << 16) | dst as u32);
    }

    fn add_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.word(0x8b00_0000 | ((rhs as u32) << 16) | ((lhs as u32) << 5) | dst as u32);
    }

    fn sub_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.word(0xcb00_0000 | ((rhs as u32) << 16) | ((lhs as u32) << 5) | dst as u32);
    }

    /// `mul xd, xn, xm` (madd with xzr accumulator)
    fn mul_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.word(0x9b00_7c00 | ((rhs as u32) << 16) | ((lhs as u32) << 5) | dst as u32);
    }

    fn sdiv_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.word(0x9ac0_0c00 | ((rhs as u32) << 16) | ((lhs as u32) << 5) | dst as u32);
    }

    /// `msub xd, xn, xm, xa` — xd = xa - xn * xm (remainder helper).
    fn msub_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg, acc: Reg) {
        self.word(
            0x9b00_8000
                | ((rhs as u32) << 16)
                | ((acc as u32) << 10)
                | ((lhs as u32) << 5)
                | dst as u32,
        );
    }

    /// `cmp xn, xm` (subs xzr, xn, xm)
    fn cmp_reg(&mut self, lhs: Reg, rhs: Reg) {
        self.word(0xeb00_001f | ((rhs as u32) << 16) | ((lhs as u32) << 5));
    }

    // The logical-register and unscaled load/store primitives below are
    // added for the AArch64 GC port and stay inert (dead code) until the
    // PortableAsm impl (M4) lowers `and/or/xor/test_reg_reg`,
    // `load/store_ptr_disp32`, and `bt_reg_imm8` onto them.

    /// `and xd, xn, xm` (logical AND, shifted register, shift 0).
    #[allow(dead_code)]
    fn and_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.word(0x8a00_0000 | ((rhs as u32) << 16) | ((lhs as u32) << 5) | dst as u32);
    }

    /// `orr xd, xn, xm` (logical OR). `mov xd, xm` is the `xn = xzr` case.
    #[allow(dead_code)]
    fn orr_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.word(0xaa00_0000 | ((rhs as u32) << 16) | ((lhs as u32) << 5) | dst as u32);
    }

    /// `eor xd, xn, xm` (logical XOR).
    #[allow(dead_code)]
    fn eor_reg(&mut self, dst: Reg, lhs: Reg, rhs: Reg) {
        self.word(0xca00_0000 | ((rhs as u32) << 16) | ((lhs as u32) << 5) | dst as u32);
    }

    /// `tst xn, xm` (ands xzr, xn, xm) — flag-setting AND that discards
    /// its result, setting Z = ((xn & xm) == 0). The GC port lowers
    /// `test_reg_reg` here, and its bit-test to `tst xn, scratch` with a
    /// materialized `1 << bit` mask (avoiding the logical-immediate
    /// encoder), then remaps the following conditional branch.
    #[allow(dead_code)]
    fn tst_reg(&mut self, lhs: Reg, rhs: Reg) {
        self.word(0xea00_001f | ((rhs as u32) << 16) | ((lhs as u32) << 5));
    }

    /// `cset xd, cond`
    fn cset(&mut self, dst: Reg, cond: Cond) {
        self.word(0x9a9f_07e0 | (cond.inverted_bits() << 12) | dst as u32);
    }

    /// `sub sp, sp, #imm12`
    fn sub_sp_imm(&mut self, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0xd100_03ff | (imm << 10));
    }

    /// Reserve the frame now and fill in its size later. How many slots a body
    /// needs is only known once it has been compiled -- temporaries for a fold,
    /// a map, a collection literal and an inlined lambda are all taken as the
    /// emission goes -- and predicting it from the syntax got the answer wrong
    /// in a way that overwrote the saved frame record. So the instruction is
    /// emitted with a zero immediate and patched from the high-water mark.
    fn sub_sp_placeholder(&mut self) -> usize {
        let at = self.code.len();
        self.sub_sp_imm(0);
        at
    }

    fn patch_sub_sp(&mut self, at: usize, imm: u32) {
        debug_assert!(imm < 4096);
        let instruction: u32 = 0xd100_03ff | (imm << 10);
        self.code[at..at + 4].copy_from_slice(&instruction.to_le_bytes());
    }

    /// `add sp, sp, #imm12`
    fn add_sp_imm(&mut self, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0x9100_03ff | (imm << 10));
    }

    /// `add xd, sp, #imm12`
    fn add_reg_sp_imm(&mut self, dst: Reg, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0x9100_0000 | (imm << 10) | (31 << 5) | dst as u32);
    }

    /// `add xd, xn, #imm12`
    fn add_reg_imm(&mut self, dst: Reg, src: Reg, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0x9100_0000 | (imm << 10) | ((src as u32) << 5) | dst as u32);
    }

    /// `mov x29, sp`
    fn mov_fp_sp(&mut self) {
        self.word(0x9100_03fd);
    }

    /// `str xt, [sp, #-16]!` — 16-byte stride keeps sp aligned, which
    /// Darwin arm64 enforces in hardware on sp-based accesses.
    fn push(&mut self, reg: Reg) {
        self.word(0xf81f_0fe0 | reg as u32);
    }

    /// `ldr xt, [sp], #16`
    fn pop(&mut self, reg: Reg) {
        self.word(0xf841_07e0 | reg as u32);
    }

    /// `str xt, [x29, #offset]` (unsigned, 8-byte scaled)
    fn store_local(&mut self, reg: Reg, offset: u32) {
        debug_assert!(offset.is_multiple_of(8) && offset / 8 < 4096);
        self.word(0xf900_0000 | ((offset / 8) << 10) | (FP << 5) | reg as u32);
    }

    /// `ldr xt, [x29, #offset]`
    fn load_local(&mut self, reg: Reg, offset: u32) {
        debug_assert!(offset.is_multiple_of(8) && offset / 8 < 4096);
        self.word(0xf940_0000 | ((offset / 8) << 10) | (FP << 5) | reg as u32);
    }

    /// `ldur xt, [xn, #simm9]` — unscaled load, signed 9-bit byte offset
    /// in `-256..=255`. The GC port needs this for the object header's
    /// `[base-16]` (word0) / `[base-8]` (type tag) accesses, which the
    /// scaled `ldr` immediate form cannot express (it is non-negative).
    #[allow(dead_code)]
    fn ldur(&mut self, reg: Reg, base: Reg, offset: i32) {
        debug_assert!((-256..=255).contains(&offset));
        let imm9 = (offset as u32) & 0x1ff;
        self.word(0xf840_0000 | (imm9 << 12) | ((base as u32) << 5) | reg as u32);
    }

    /// `stur xt, [xn, #simm9]` — unscaled store, signed 9-bit byte offset.
    #[allow(dead_code)]
    fn stur(&mut self, reg: Reg, base: Reg, offset: i32) {
        debug_assert!((-256..=255).contains(&offset));
        let imm9 = (offset as u32) & 0x1ff;
        self.word(0xf800_0000 | (imm9 << 12) | ((base as u32) << 5) | reg as u32);
    }

    /// `strb wt, [xn, #-1]!`
    fn store_byte_pre_decrement(&mut self, reg: Reg, base: Reg) {
        self.word(0x3800_0c00 | (0x1ff << 12) | ((base as u32) << 5) | reg as u32);
    }

    /// `adrp xd, <page>` + `add xd, xd, #<pageoff>` addressing a byte
    /// in rodata; both immediates are zero placeholders patched by the
    /// Mach-O writer once the image layout is final.
    fn load_rodata_address(&mut self, reg: Reg, data_offset: usize) {
        let adrp_offset = self.code.len();
        self.word(0x9000_0000 | reg as u32);
        let add_offset = self.code.len();
        let rn = reg as u32;
        self.word(0x9100_0000 | (rn << 5) | rn);
        self.fixups.push(DataFixup {
            adrp_offset,
            add_offset,
            data_offset,
            section: FixupSection::Rodata,
        });
    }

    /// Reserve `count` zero-initialized 8-byte cells in the writable
    /// `__DATA,__bss` section and return a handle to the first. The GC's
    /// mutable globals (heap pointers, phase, mark worklist, region
    /// tables, counters, ...) live here — the AArch64 analog of the
    /// x86-64 backend's `data_label_with_i64s(&[0; count])`. Inert until
    /// the GC runtime wiring (M5) reserves and addresses cells.
    #[allow(dead_code)]
    fn reserve_data_cells(&mut self, count: usize) -> DataLabel {
        let label = DataLabel(self.bss_len);
        self.bss_len += count * 8;
        label
    }

    /// `adrp xd, <page>` + `add xd, xd, #<pageoff>` addressing a cell in
    /// `__DATA,__bss`; the immediates are zero placeholders patched by the
    /// Mach-O writer against the segment vmaddr once the layout is final.
    /// The GC port lowers `mov_data_addr` here.
    #[allow(dead_code)]
    fn load_data_address(&mut self, reg: Reg, label: DataLabel) {
        let adrp_offset = self.code.len();
        self.word(0x9000_0000 | reg as u32);
        let add_offset = self.code.len();
        let rn = reg as u32;
        self.word(0x9100_0000 | (rn << 5) | rn);
        self.fixups.push(DataFixup {
            adrp_offset,
            add_offset,
            data_offset: label.0,
            section: FixupSection::Data,
        });
    }

    /// `svc #0x80` — Darwin syscall trap.
    fn svc_0x80(&mut self) {
        self.word(0xd400_1001);
    }

    fn intern_rodata(&mut self, bytes: &[u8]) -> usize {
        let offset = self.rodata.len();
        self.rodata.extend_from_slice(bytes);
        offset
    }

    /// Intern a string literal as a `[len: u64][bytes]` object —
    /// byte-identical to what the bump allocator produces, so every
    /// string value is one pointer regardless of where it lives.
    fn intern_string_object(&mut self, text: &str) -> usize {
        while !self.rodata.len().is_multiple_of(8) {
            self.rodata.push(0);
        }
        let offset = self.rodata.len();
        self.rodata
            .extend_from_slice(&(text.len() as u64).to_le_bytes());
        self.rodata.extend_from_slice(text.as_bytes());
        offset
    }

    /// Intern a NUL-terminated C string — the shape every Darwin
    /// path-taking syscall (`access`, and later `open`/`unlink`/...)
    /// requires, distinct from `intern_string_object`'s length-prefixed
    /// Klassic string layout.
    fn intern_nul_terminated(&mut self, text: &str) -> usize {
        let offset = self.rodata.len();
        self.rodata.extend_from_slice(text.as_bytes());
        self.rodata.push(0);
        offset
    }

    /// write(fd, <rodata bytes>, len) — clobbers x0/x1/x2/x16.
    fn emit_write_rodata(&mut self, fd: u64, bytes: &[u8]) {
        let data_offset = self.intern_rodata(bytes);
        self.mov_imm64(Reg::X0, fd);
        self.load_rodata_address(Reg::X1, data_offset);
        self.mov_imm64(Reg::X2, bytes.len() as u64);
        self.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
        self.svc_0x80();
    }

    /// `x0 = 1` if the preceding Darwin syscall succeeded (carry
    /// clear), `0` otherwise — the qword-per-value sentinel `Dir#exists`
    /// and friends return instead of aborting. Darwin's `svc #0x80`
    /// carry-flag convention is the mirror image of Linux's
    /// negative-rax convention (see issue #538, M11).
    fn cset_syscall_succeeded(&mut self) {
        self.cset(Reg::X0, Cond::Cc);
    }

    /// Abort with `message` to stderr and `exit(1)` if the preceding
    /// Darwin syscall's carry flag signals failure (carry set). The
    /// abort-on-failure counterpart to `cset_syscall_succeeded`,
    /// deferred from M11 until a caller (M14 file I/O) actually
    /// needed it.
    fn emit_abort_if_syscall_failed(&mut self, message: &[u8]) {
        let ok = self.new_label();
        self.branch(ok, BranchKind::Conditional(Cond::Cc));
        self.emit_write_rodata(STDERR_FD, message);
        self.emit_exit(1);
        self.bind(ok);
    }

    fn emit_exit(&mut self, status: u64) {
        self.mov_imm64(Reg::X0, status);
        self.mov_imm64(Reg::X16, u64::from(SYS_EXIT));
        self.svc_0x80();
    }

    /// Decompose the signed integer in x0 into decimal digits at the
    /// tail of a 32-byte stack buffer the caller has reserved
    /// (`sub sp, #32`). Leaves x1 = first byte, sp+32 = one past the
    /// end. Clobbers x0/x2-x5.
    fn emit_int_digits(&mut self, include_newline: bool) {
        self.add_reg_sp_imm(Reg::X1, 32); // one past the buffer end
        if include_newline {
            self.mov_imm64(Reg::X3, b'\n' as u64);
            self.store_byte_pre_decrement(Reg::X3, Reg::X1);
        }
        // x5 = 1 when negative, then continue with |value|.
        self.cmp_x0_zero();
        self.cset(Reg::X5, Cond::Lt);
        let non_negative = self.new_label();
        self.branch(non_negative, BranchKind::Conditional(Cond::Ge));
        self.neg_x0();
        self.bind(non_negative);
        // Digit loop: do { x4 = x0 / 10; digit = x0 - x4*10; } while x0.
        let digit_loop = self.new_label();
        self.bind(digit_loop);
        self.mov_imm64(Reg::X3, 10);
        self.sdiv_reg(Reg::X4, Reg::X0, Reg::X3);
        self.msub_reg(Reg::X2, Reg::X4, Reg::X3, Reg::X0);
        self.add_reg_imm(Reg::X2, Reg::X2, u32::from(b'0'));
        self.store_byte_pre_decrement(Reg::X2, Reg::X1);
        self.mov_reg(Reg::X0, Reg::X4);
        self.branch(digit_loop, BranchKind::CompareNonZero(Reg::X0));
        let no_sign = self.new_label();
        self.branch(no_sign, BranchKind::CompareZero(Reg::X5));
        self.mov_imm64(Reg::X3, b'-' as u64);
        self.store_byte_pre_decrement(Reg::X3, Reg::X1);
        self.bind(no_sign);
    }

    /// Print the signed integer in x0 followed by a newline with one
    /// write syscall. Clobbers x0-x5/x16.
    fn emit_print_int_line(&mut self) {
        self.sub_sp_imm(32);
        self.emit_int_digits(true);
        // write(1, x1, (sp+32) - x1)
        self.add_reg_sp_imm(Reg::X2, 32);
        self.sub_reg(Reg::X2, Reg::X2, Reg::X1);
        self.mov_imm64(Reg::X0, STDOUT_FD);
        self.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
        self.svc_0x80();
        self.add_sp_imm(32);
    }

    /// `cmp x0, #0`
    fn cmp_x0_zero(&mut self) {
        self.word(0xf100_001f);
    }

    /// `neg x0, x0` (sub x0, xzr, x0)
    fn neg_x0(&mut self) {
        self.word(0xcb00_03e0);
    }

    // --- scalar double FP (the value lives in a GP register as raw
    // bits; these move it through d0/d1 for the actual operation) ---

    /// `fmov dd, xn` — reinterpret the GP bits as a double in dN.
    fn fmov_d_from_x(&mut self, d: u32, x: Reg) {
        self.word(0x9e67_0000 | ((x as u32) << 5) | d);
    }

    /// `fmov xd, dn` — reinterpret the double bits back into a GP reg.
    fn fmov_x_from_d(&mut self, x: Reg, d: u32) {
        self.word(0x9e66_0000 | (d << 5) | x as u32);
    }

    fn fadd_d(&mut self, dd: u32, dn: u32, dm: u32) {
        self.word(0x1e60_2800 | (dm << 16) | (dn << 5) | dd);
    }

    fn fsub_d(&mut self, dd: u32, dn: u32, dm: u32) {
        self.word(0x1e60_3800 | (dm << 16) | (dn << 5) | dd);
    }

    fn fmul_d(&mut self, dd: u32, dn: u32, dm: u32) {
        self.word(0x1e60_0800 | (dm << 16) | (dn << 5) | dd);
    }

    fn fdiv_d(&mut self, dd: u32, dn: u32, dm: u32) {
        self.word(0x1e60_1800 | (dm << 16) | (dn << 5) | dd);
    }

    /// `fsqrt dd, dn`
    fn fsqrt_d(&mut self, dd: u32, dn: u32) {
        self.word(0x1e61_c000 | (dn << 5) | dd);
    }

    /// `fneg dd, dn`
    fn fneg_d(&mut self, dd: u32, dn: u32) {
        self.word(0x1e61_4000 | (dn << 5) | dd);
    }

    /// `fabs dd, dn`
    fn fabs_d(&mut self, dd: u32, dn: u32) {
        self.word(0x1e60_c000 | (dn << 5) | dd);
    }

    /// `scvtf dd, xn` — signed 64-bit integer to double.
    fn scvtf_d_from_x(&mut self, dd: u32, x: Reg) {
        self.word(0x9e62_0000 | ((x as u32) << 5) | dd);
    }

    /// `fcvtzs xd, dn` — double to signed 64-bit integer, truncating.
    fn fcvtzs_x_from_d(&mut self, x: Reg, dn: u32) {
        self.word(0x9e78_0000 | (dn << 5) | x as u32);
    }

    /// `frintm dd, dn` — round toward minus infinity. Unused: this language's
    /// `floor` truncates toward zero (see the `floor` builtin), so `fcvtzs`
    /// alone is the whole operation. Kept because it is the natural companion
    /// to `frintp` and the next rounding builtin will want it.
    #[allow(dead_code)]
    fn frintm_d(&mut self, dd: u32, dn: u32) {
        self.word(0x1e65_4000 | (dn << 5) | dd);
    }

    /// `frintp dd, dn` — round toward plus infinity (ceil).
    fn frintp_d(&mut self, dd: u32, dn: u32) {
        self.word(0x1e64_c000 | (dn << 5) | dd);
    }

    /// `fcmp dn, dm` — sets NZCV for a conditional-set.
    fn fcmp_d(&mut self, dn: u32, dm: u32) {
        self.word(0x1e60_2000 | (dm << 16) | (dn << 5));
    }

    /// `sub xd, xn, #imm12`
    fn sub_reg_imm(&mut self, dst: Reg, src: Reg, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0xd100_0000 | (imm << 10) | ((src as u32) << 5) | dst as u32);
    }

    /// `cmp xn, #imm12` (subs xzr, xn, #imm)
    fn cmp_imm(&mut self, reg: Reg, imm: u32) {
        debug_assert!(imm < 4096);
        self.word(0xf100_001f | (imm << 10) | ((reg as u32) << 5));
    }

    /// `dst = 1` if the byte in `byte_reg` is ASCII whitespace
    /// (space/tab/LF/CR/VT/FF, matching the x86_64 backend's
    /// `emit_jump_if_ascii_whitespace` set), `0` otherwise.
    fn is_ascii_whitespace_into(&mut self, byte_reg: Reg, dst: Reg) {
        let is_ws = self.new_label();
        let done = self.new_label();
        for byte in [b' ', b'\t', b'\n', b'\r', 0x0b, 0x0c] {
            self.cmp_imm(byte_reg, u32::from(byte));
            self.branch(is_ws, BranchKind::Conditional(Cond::Eq));
        }
        self.mov_imm64(dst, 0);
        self.branch(done, BranchKind::Unconditional);
        self.bind(is_ws);
        self.mov_imm64(dst, 1);
        self.bind(done);
    }

    /// `dst = 1` if the `len` bytes at `a_ptr` equal the `len` bytes
    /// at `b_ptr`, `0` otherwise. Consumes `len`/`a_ptr`/`b_ptr`
    /// (post-increments through them) -- callers that need the
    /// originals intact afterward should pass working copies.
    /// `scratch1`/`scratch2` hold one byte from each side per
    /// iteration.
    fn bytes_equal(
        &mut self,
        len: Reg,
        a_ptr: Reg,
        b_ptr: Reg,
        dst: Reg,
        scratch1: Reg,
        scratch2: Reg,
    ) {
        let differ = self.new_label();
        let same = self.new_label();
        let done = self.new_label();
        let loop_start = self.new_label();
        self.bind(loop_start);
        self.branch(same, BranchKind::CompareZero(len));
        self.ldrb_post_increment(scratch1, a_ptr);
        self.ldrb_post_increment(scratch2, b_ptr);
        self.cmp_reg(scratch1, scratch2);
        self.branch(differ, BranchKind::Conditional(Cond::Ne));
        self.sub_reg_imm(len, len, 1);
        self.branch(loop_start, BranchKind::Unconditional);
        self.bind(same);
        self.mov_imm64(dst, 1);
        self.branch(done, BranchKind::Unconditional);
        self.bind(differ);
        self.mov_imm64(dst, 0);
        self.bind(done);
    }

    /// `lsr xd, xn, #shift`
    fn lsr_imm(&mut self, dst: Reg, src: Reg, shift: u32) {
        debug_assert!(shift < 64);
        self.word(0xd340_0000 | (shift << 16) | (63 << 10) | ((src as u32) << 5) | dst as u32);
    }

    /// `lsl xd, xn, #shift`
    fn lsl_imm(&mut self, dst: Reg, src: Reg, shift: u32) {
        debug_assert!(0 < shift && shift < 64);
        let immr = 64 - shift;
        let imms = 63 - shift;
        self.word(0xd340_0000 | (immr << 16) | (imms << 10) | ((src as u32) << 5) | dst as u32);
    }

    /// `ldr xt, [xn, #imm]` (unsigned, 8-byte scaled)
    fn ldr_imm(&mut self, dst: Reg, base: Reg, imm: u32) {
        debug_assert!(imm.is_multiple_of(8) && imm / 8 < 4096);
        self.word(0xf940_0000 | ((imm / 8) << 10) | ((base as u32) << 5) | dst as u32);
    }

    /// `ldr wt, [xn, #imm]` (unsigned, 4-byte scaled, 32-bit --
    /// zero-extends into the full 64-bit register per AAPCS64
    /// W-register write semantics). Used where a field is genuinely
    /// 4 bytes and a 64-bit load would pull in unrelated trailing
    /// bytes (e.g. Darwin's `struct timeval.tv_usec`, a
    /// `__int32_t` immediately followed by 4 bytes of padding).
    fn ldr_imm32(&mut self, dst: Reg, base: Reg, imm: u32) {
        debug_assert!(imm.is_multiple_of(4) && imm / 4 < 4096);
        self.word(0xb940_0000 | ((imm / 4) << 10) | ((base as u32) << 5) | dst as u32);
    }

    /// `str xt, [xn, #imm]` (unsigned, 8-byte scaled)
    fn str_imm(&mut self, src: Reg, base: Reg, imm: u32) {
        debug_assert!(imm.is_multiple_of(8) && imm / 8 < 4096);
        self.word(0xf900_0000 | ((imm / 8) << 10) | ((base as u32) << 5) | src as u32);
    }

    /// `ldrb wt, [xn]` — peek without advancing.
    fn ldrb(&mut self, dst: Reg, base: Reg) {
        self.word(0x3940_0000 | ((base as u32) << 5) | dst as u32);
    }

    /// `ldrh wt, [xn, #imm]` (unsigned, 2-byte scaled) -- used to read
    /// Darwin's `stat64.st_mode`, a halfword field.
    fn ldrh_imm(&mut self, dst: Reg, base: Reg, imm: u32) {
        debug_assert!(imm.is_multiple_of(2) && imm / 2 < 4096);
        self.word(0x7940_0000 | ((imm / 2) << 10) | ((base as u32) << 5) | dst as u32);
    }

    /// `ldr xt, [xn, xm]` — register-offset load.
    fn ldr_reg_offset(&mut self, dst: Reg, base: Reg, offset: Reg) {
        self.word(0xf860_6800 | ((offset as u32) << 16) | ((base as u32) << 5) | dst as u32);
    }

    /// `str xt, [xn, xm]` — register-offset store.
    fn str_reg_offset(&mut self, src: Reg, base: Reg, offset: Reg) {
        self.word(0xf820_6800 | ((offset as u32) << 16) | ((base as u32) << 5) | src as u32);
    }

    /// `ldrb wt, [xn, xm]` — register-offset byte load, no advance.
    /// Same size-field flip (`11` -> `00`) from `ldr_reg_offset` that
    /// `ldrb_post_increment` applies to `ldr_post_increment`.
    fn ldrb_reg_offset(&mut self, dst: Reg, base: Reg, offset: Reg) {
        self.word(0x3860_6800 | ((offset as u32) << 16) | ((base as u32) << 5) | dst as u32);
    }

    /// `strb wt, [xn, xm]` — register-offset byte store, no advance.
    fn strb_reg_offset(&mut self, src: Reg, base: Reg, offset: Reg) {
        self.word(0x3820_6800 | ((offset as u32) << 16) | ((base as u32) << 5) | src as u32);
    }

    /// `ldrb wt, [xn], #1`
    fn ldrb_post_increment(&mut self, dst: Reg, base: Reg) {
        self.word(0x3840_1400 | ((base as u32) << 5) | dst as u32);
    }

    /// `strb wt, [xn], #1`
    fn strb_post_increment(&mut self, src: Reg, base: Reg) {
        self.word(0x3800_1400 | ((base as u32) << 5) | src as u32);
    }

    /// `stp x29, x30, [sp, #-16]!` — function prologue link save.
    fn push_frame_record(&mut self) {
        self.word(0xa9bf_7bfd);
    }

    /// `ldp x29, x30, [sp], #16` — function epilogue link restore.
    fn pop_frame_record(&mut self) {
        self.word(0xa8c1_7bfd);
    }

    /// `ret`
    fn ret(&mut self) {
        self.word(0xd65f_03c0);
    }
}

/// Address of a datum the portable GC codegen references through
/// `mov_data_addr`: a byte in read-only `__TEXT,__const` (constants,
/// diagnostic strings) or a cell in writable `__DATA,__bss` (the
/// collector's mutable globals). The AArch64 `PortableAsm::DataLabel`.
#[derive(Clone, Copy)]
#[allow(dead_code)]
enum PortDataAddr {
    Rodata(usize),
    Bss(DataLabel),
}

/// Scratch register for immediate materialization and large-displacement
/// forms. It sits outside the `V0..V11` mapping, so the portable
/// collector never holds a live value in it.
const PORT_SCRATCH: Reg = Reg::X10;

/// Map a portable register to its AArch64 home. `V4`/`V5` (the x86 rsp/
/// rbp roles) are the stack and frame pointers; the frame-shaped methods
/// handle them inline and never route them through here.
fn port_reg(r: portable_asm::Reg) -> Reg {
    use portable_asm::Reg as P;
    match r {
        P::V0 => Reg::X0,
        P::V1 => Reg::X1,
        P::V2 => Reg::X2,
        P::V3 => Reg::X3,
        P::V6 => Reg::X4,
        P::V7 => Reg::X5,
        P::V8 => Reg::X6,
        P::V9 => Reg::X7,
        P::V10 => Reg::X8,
        P::V11 => Reg::X9,
        P::ColorStrip => Reg::X24,
        P::GoodColor => Reg::X25,
        P::BadMask => Reg::X26,
        P::V4 | P::V5 => {
            unreachable!("V4/V5 (sp/fp) are handled by the frame-shaped methods")
        }
    }
}

fn port_cond(c: portable_asm::Condition) -> Cond {
    use portable_asm::Condition as P;
    match c {
        P::Equal => Cond::Eq,
        P::NotEqual => Cond::Ne,
        P::Below => Cond::Cc,
        P::Above => Cond::Hi,
        P::AboveOrEqual => Cond::Hs,
        P::Less => Cond::Lt,
        P::LessEqual => Cond::Le,
        P::Greater => Cond::Gt,
        P::GreaterEqual => Cond::Ge,
        P::NoOverflow | P::Parity => {
            unreachable!("condition not emitted by any GC runtime routine")
        }
    }
}

/// AArch64 realization of the portable ZGC emitter. Every method lowers
/// one x86-flavored `PortableAsm` operation to AArch64, so the ~24
/// architecture-independent `portable_asm::emit_gc_*` routines emit into
/// this `Assembler` unchanged. Two conventions differ from x86 and are
/// absorbed here: the frame (x86 `push rbp` leaves the return address on
/// the stack, but AArch64 `bl` leaves it in x30, so the prologue saves a
/// full frame record x29/x30), and rbp-relative slots (x29 sits at the
/// frame top, so a slot is `[x29, #-offset]`).
impl portable_asm::PortableAsm for Assembler {
    type TextLabel = Label;
    type DataLabel = PortDataAddr;

    fn create_text_label(&mut self) -> Label {
        self.new_label()
    }
    fn bind_text_label(&mut self, label: Label) {
        self.bind(label);
    }
    fn jmp_label(&mut self, label: Label) {
        self.branch(label, BranchKind::Unconditional);
    }
    fn jcc_label(&mut self, cond: portable_asm::Condition, label: Label) {
        use portable_asm::Condition as P;
        let cond = if self.bt_pending {
            self.bt_pending = false;
            // The bit test lowered to `tst reg, #(1<<bit)`, setting
            // Z = (bit == 0). x86 `Below` (bit set) becomes `Ne`;
            // `AboveOrEqual` (bit clear) becomes `Eq`.
            match cond {
                P::Below => Cond::Ne,
                P::AboveOrEqual => Cond::Eq,
                other => port_cond(other),
            }
        } else {
            port_cond(cond)
        };
        self.branch(label, BranchKind::Conditional(cond));
    }
    /// A call that cannot destroy the caller's return address.
    ///
    /// The portable routines were written against x86-64, where `call`
    /// pushes the return address onto the stack, so a nested call is
    /// invisible to the caller's own return. AArch64's `bl` instead
    /// overwrites x30, so a routine that calls another without having saved
    /// the link register can never return -- `gc_load_barrier_slow` (which
    /// calls `mark_visit` and `evacuate`) is exactly that shape, and it
    /// jumped to a clobbered x30 the first time a mark cycle made the
    /// barrier take its slow path.
    ///
    /// Bracketing every portable call with a frame-record save/restore
    /// gives back the x86 property globally, instead of auditing each
    /// routine's frame discipline: two extra instructions per call, and
    /// x29 is restored too, so a callee that repoints the frame pointer
    /// cannot leak it either.
    fn call_label(&mut self, label: Label) {
        self.push_frame_record(); // stp x29, x30, [sp, #-16]!
        self.branch(label, BranchKind::Link);
        self.pop_frame_record(); // ldp x29, x30, [sp], #16
    }
    fn ret(&mut self) {
        Assembler::ret(self);
    }
    fn leave(&mut self) {
        self.word(0x9100_03bf); // mov sp, x29 (add sp, x29, #0)
        self.pop_frame_record(); // ldp x29, x30, [sp], #16
    }

    fn push_reg(&mut self, r: portable_asm::Reg) {
        match r {
            portable_asm::Reg::V5 => self.push_frame_record(), // save fp+lr
            other => self.push(port_reg(other)),
        }
    }
    fn pop_reg(&mut self, r: portable_asm::Reg) {
        match r {
            portable_asm::Reg::V5 => self.pop_frame_record(),
            other => self.pop(port_reg(other)),
        }
    }
    fn mov_reg_reg(&mut self, dst: portable_asm::Reg, src: portable_asm::Reg) {
        use portable_asm::Reg as P;
        match (dst, src) {
            (P::V5, P::V4) => self.mov_fp_sp(), // mov x29, sp
            _ => self.mov_reg(port_reg(dst), port_reg(src)),
        }
    }
    fn mov_imm64(&mut self, dst: portable_asm::Reg, imm: u64) {
        Assembler::mov_imm64(self, port_reg(dst), imm);
    }
    fn mov_data_addr(&mut self, dst: portable_asm::Reg, label: PortDataAddr) {
        let reg = port_reg(dst);
        match label {
            PortDataAddr::Rodata(off) => self.load_rodata_address(reg, off),
            PortDataAddr::Bss(dl) => self.load_data_address(reg, dl),
        }
    }

    fn load_ptr_disp32(&mut self, dst: portable_asm::Reg, base: portable_asm::Reg, disp: i32) {
        let (d, b) = (port_reg(dst), port_reg(base));
        if disp >= 0 && disp % 8 == 0 && disp / 8 < 4096 {
            self.ldr_imm(d, b, disp as u32);
        } else if (-256..=255).contains(&disp) {
            self.ldur(d, b, disp);
        } else {
            Assembler::mov_imm64(self, PORT_SCRATCH, disp as i64 as u64);
            self.ldr_reg_offset(d, b, PORT_SCRATCH);
        }
    }
    fn store_ptr_disp32(&mut self, base: portable_asm::Reg, disp: i32, src: portable_asm::Reg) {
        let (s, b) = (port_reg(src), port_reg(base));
        if disp >= 0 && disp % 8 == 0 && disp / 8 < 4096 {
            self.str_imm(s, b, disp as u32);
        } else if (-256..=255).contains(&disp) {
            self.stur(s, b, disp);
        } else {
            Assembler::mov_imm64(self, PORT_SCRATCH, disp as i64 as u64);
            self.str_reg_offset(s, b, PORT_SCRATCH);
        }
    }
    fn load_rbp_slot(&mut self, dst: portable_asm::Reg, offset: i32) {
        // x86 `[rbp - offset]`; x29 sits at the frame top, so `[x29, -off]`.
        self.ldur(port_reg(dst), Reg::X29, -offset);
    }
    fn store_rbp_slot(&mut self, offset: i32, src: portable_asm::Reg) {
        self.stur(port_reg(src), Reg::X29, -offset);
    }

    fn add_reg_reg(&mut self, dst: portable_asm::Reg, src: portable_asm::Reg) {
        let (d, s) = (port_reg(dst), port_reg(src));
        self.add_reg(d, d, s);
    }
    fn sub_reg_reg(&mut self, dst: portable_asm::Reg, src: portable_asm::Reg) {
        let (d, s) = (port_reg(dst), port_reg(src));
        self.sub_reg(d, d, s);
    }
    fn and_reg_reg(&mut self, dst: portable_asm::Reg, src: portable_asm::Reg) {
        let (d, s) = (port_reg(dst), port_reg(src));
        self.and_reg(d, d, s);
    }
    fn or_reg_reg(&mut self, dst: portable_asm::Reg, src: portable_asm::Reg) {
        let (d, s) = (port_reg(dst), port_reg(src));
        self.orr_reg(d, d, s);
    }
    fn xor_reg_reg(&mut self, dst: portable_asm::Reg, src: portable_asm::Reg) {
        let (d, s) = (port_reg(dst), port_reg(src));
        self.eor_reg(d, d, s);
    }
    fn cmp_reg_reg(&mut self, a: portable_asm::Reg, b: portable_asm::Reg) {
        self.cmp_reg(port_reg(a), port_reg(b));
    }
    fn test_reg_reg(&mut self, a: portable_asm::Reg, b: portable_asm::Reg) {
        self.tst_reg(port_reg(a), port_reg(b));
    }

    fn add_reg_imm32(&mut self, dst: portable_asm::Reg, imm: i32) {
        if dst == portable_asm::Reg::V4 {
            debug_assert!((0..4096).contains(&imm));
            self.add_sp_imm(imm as u32);
        } else {
            let d = port_reg(dst);
            if (0..4096).contains(&imm) {
                self.add_reg_imm(d, d, imm as u32);
            } else if (-4095..0).contains(&imm) {
                self.sub_reg_imm(d, d, (-imm) as u32);
            } else {
                Assembler::mov_imm64(self, PORT_SCRATCH, imm as i64 as u64);
                self.add_reg(d, d, PORT_SCRATCH);
            }
        }
    }
    fn sub_reg_imm8(&mut self, r: portable_asm::Reg, imm: i8) {
        if r == portable_asm::Reg::V4 {
            self.sub_sp_imm(imm as u32);
        } else {
            let d = port_reg(r);
            self.sub_reg_imm(d, d, imm as u32);
        }
    }
    fn sub_reg_imm32(&mut self, r: portable_asm::Reg, imm: i32) {
        let d = port_reg(r);
        if (0..4096).contains(&imm) {
            self.sub_reg_imm(d, d, imm as u32);
        } else {
            Assembler::mov_imm64(self, PORT_SCRATCH, imm as i64 as u64);
            self.sub_reg(d, d, PORT_SCRATCH);
        }
    }
    fn and_reg_imm32(&mut self, r: portable_asm::Reg, imm: i32) {
        // Materialize the mask and AND register-register, sidestepping the
        // AArch64 logical-immediate (N:immr:imms) encoder.
        let d = port_reg(r);
        Assembler::mov_imm64(self, PORT_SCRATCH, imm as i64 as u64);
        self.and_reg(d, d, PORT_SCRATCH);
    }
    fn cmp_reg_imm8(&mut self, r: portable_asm::Reg, imm: i8) {
        let d = port_reg(r);
        if imm >= 0 {
            self.cmp_imm(d, imm as u32);
        } else {
            Assembler::mov_imm64(self, PORT_SCRATCH, imm as i64 as u64);
            self.cmp_reg(d, PORT_SCRATCH);
        }
    }
    fn cmp_reg_imm32(&mut self, r: portable_asm::Reg, imm: i32) {
        let d = port_reg(r);
        if (0..4096).contains(&imm) {
            self.cmp_imm(d, imm as u32);
        } else {
            Assembler::mov_imm64(self, PORT_SCRATCH, imm as i64 as u64);
            self.cmp_reg(d, PORT_SCRATCH);
        }
    }
    fn shl_reg_imm8(&mut self, r: portable_asm::Reg, imm: u8) {
        let d = port_reg(r);
        self.lsl_imm(d, d, u32::from(imm));
    }
    fn shr_reg_imm8(&mut self, r: portable_asm::Reg, imm: u8) {
        let d = port_reg(r);
        self.lsr_imm(d, d, u32::from(imm));
    }
    fn bt_reg_imm8(&mut self, r: portable_asm::Reg, bit: u8) {
        // Flag-setting `tst reg, #(1<<bit)` via a materialized mask;
        // `jcc_label` remaps the following branch (see `bt_pending`).
        Assembler::mov_imm64(self, PORT_SCRATCH, 1u64 << bit);
        self.tst_reg(port_reg(r), PORT_SCRATCH);
        self.bt_pending = true;
    }
    fn rep_movsb(&mut self) {
        // Copy V1 (rcx=x1) bytes from [V6 (rsi=x4)] to [V7 (rdi=x5)].
        let (count, src, dst) = (Reg::X1, Reg::X4, Reg::X5);
        let copy_loop = self.new_label();
        let done = self.new_label();
        self.bind(copy_loop);
        self.branch(done, BranchKind::CompareZero(count));
        self.ldrb_post_increment(PORT_SCRATCH, src);
        self.strb_post_increment(PORT_SCRATCH, dst);
        self.sub_reg_imm(count, count, 1);
        self.branch(copy_loop, BranchKind::Unconditional);
        self.bind(done);
    }

    fn plat_write_data(&mut self, fd: u64, data: PortDataAddr, len: usize) {
        Assembler::mov_imm64(self, Reg::X0, fd);
        match data {
            PortDataAddr::Rodata(off) => self.load_rodata_address(Reg::X1, off),
            PortDataAddr::Bss(dl) => self.load_data_address(Reg::X1, dl),
        }
        Assembler::mov_imm64(self, Reg::X2, len as u64);
        Assembler::mov_imm64(self, Reg::X16, u64::from(SYS_WRITE));
        self.svc_0x80();
    }
    fn plat_exit(&mut self, code: u64) {
        Assembler::mov_imm64(self, Reg::X0, code);
        Assembler::mov_imm64(self, Reg::X16, u64::from(SYS_EXIT));
        self.svc_0x80();
    }
    fn plat_read_monotonic_ns(&mut self) {
        // Minimal: report 0 ns (like the x86 Windows path, whose pauses
        // also read 0). The GC wiring keeps `--gc-log` timing disabled on
        // AArch64, so this is never emitted; a real monotonic clock can
        // replace it later.
        Assembler::mov_imm64(self, Reg::X0, 0);
    }
}

fn unsupported(span: Span, feature: &str) -> Diagnostic {
    Diagnostic::compile(
        span,
        format!("{feature} is not supported by the aarch64-apple-darwin backend yet"),
    )
}

/// Element types a list can carry in the current subset.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum ListElem {
    /// A `Double`, carried as its raw bits like every other Double here.
    /// Printing one is still unsupported (that needs the shortest-round-trip
    /// formatter the x86-64 backend has), but folding and comparing work.
    Double,
    Int,
    Bool,
    Str,
    /// An element that is one of this backend's own enums, which is a pointer
    /// like a string is -- `JArr(items: List<Json>)` is a list of them.
    Enum(u32),
    /// An element that is a record, which is a pointer too. A list of them is
    /// what `toPairs()` hands back.
    Record(u32),
    /// An element that is itself a list: `depth` levels of list over `base`.
    /// `List<List<Int>>`'s element is `Nested { depth: 1, base: Int }` and
    /// `List<List<List<Int>>>`'s is `depth: 2`.
    ///
    /// Encoding the nesting as a depth rather than a boxed inner element keeps
    /// `ListElem` `Copy` -- which everything from the specialisation keys to
    /// the codegen relies on -- while still describing any depth.
    Nested {
        depth: u8,
        base: ScalarElem,
    },
}

/// The scalar element types, i.e. `ListElem` minus the nested case. Only used
/// as `Nested`'s base so the nesting can be a plain depth.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum ScalarElem {
    Double,
    Int,
    Bool,
    Str,
}

impl ScalarElem {
    fn as_elem(self) -> ListElem {
        match self {
            ScalarElem::Double => ListElem::Double,
            ScalarElem::Int => ListElem::Int,
            ScalarElem::Bool => ListElem::Bool,
            ScalarElem::Str => ListElem::Str,
        }
    }
}

impl ListElem {
    fn as_scalar(self) -> Option<ScalarElem> {
        match self {
            ListElem::Double => Some(ScalarElem::Double),
            ListElem::Int => Some(ScalarElem::Int),
            ListElem::Bool => Some(ScalarElem::Bool),
            ListElem::Str => Some(ScalarElem::Str),
            ListElem::Enum(_) | ListElem::Record(_) | ListElem::Nested { .. } => None,
        }
    }

    /// For a nested element, the element type of the list it *is*.
    fn nested_inner(self) -> Option<ListElem> {
        match self {
            ListElem::Nested { depth: 1, base } => Some(base.as_elem()),
            ListElem::Nested { depth, base } => Some(ListElem::Nested {
                depth: depth - 1,
                base,
            }),
            _ => None,
        }
    }
}

/// The value types the current subset can hold in a register / local.
/// `Str` is a pointer to a `[len: u64][bytes]` object in rodata or on
/// the bump heap; `List` is a pointer to a `[value: u64][next: ptr]`
/// cons cell (nil is the null pointer).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum ValueType {
    Int,
    /// An IEEE 754 double held as raw bits in a GP register / frame
    /// slot, exactly like an Int; arithmetic and comparison bounce
    /// through the FP registers, everything else (storage, calls,
    /// locals) treats it as an opaque qword.
    Double,
    Bool,
    Str,
    List(ListElem),
    /// The type of `[]` before any element pins it down; assignable
    /// to every list type (it is the null pointer).
    EmptyList,
    /// An insertion-ordered, de-duplicated set, stored as the same
    /// cons-cell list as `List` (membership and dedup are linear
    /// scans).
    Set(ListElem),
    /// An insertion-ordered map, stored as a cons-cell chain of two-slot
    /// entries `[key][value]` -- the same shape as a cons cell, so the
    /// collector traces an entry with no new case. Lookup is a linear scan,
    /// like `Set`'s membership.
    Map(ListElem, ListElem),
    /// The type of `%[]` before any entry pins it down.
    EmptyMap,
    /// The type of `%()` before any element pins it down.
    EmptySet,
    /// A record value: index into the emitter's record table (nominal
    /// declarations and interned structural shapes).
    Record(u32),
    /// An untyped heap pointer: lowered enum values and the box cells
    /// the enum desugaring builds around scalars.
    Ptr,
    Unit,
    /// A generic enum's value: an index into the emitter's shape table, which
    /// records what each variant carries. The shared lowering only erases
    /// *monomorphic* enums, so a generic one arrives here as constructor calls
    /// and `match` expressions, and what a variant's payload is has to be
    /// tracked per value -- `Some(42)` and `Some("x")` are the same enum and
    /// not the same shape.
    Enum(u32),
    /// `null` itself, before anything pins down what it stands in for. It is
    /// the null pointer, so it goes into any reference slot; joined with a
    /// value in an `if` it becomes a `Nullable`.
    Null,
    /// `null` *or* a value: the prelude's `stdlibFind` returns the element it
    /// found or nothing, and one machine word has to carry both. Null is 0 and
    /// the value is boxed exactly the way a list element is, so a null test
    /// tells them apart -- which is also why the collector can trace this
    /// like any other reference.
    Nullable(ListElem),
    /// The "type" of diverging expressions (`__match_fail()`): merges
    /// with anything in an if join.
    Never,
}

/// Merge the types of two branches of an `if` expression: a diverging
/// branch adopts the other branch's type, and an empty-list branch
/// adopts the other branch's list type.
fn merge_branch_types(then_ty: ValueType, else_ty: ValueType) -> Option<ValueType> {
    match (then_ty, else_ty) {
        (ValueType::Never, other) | (other, ValueType::Never) => Some(other),
        // `null` joined with a value. The value's branch boxes it on the way
        // out (see the `if` codegen), so the join is one word either way.
        (ValueType::Null, other) | (other, ValueType::Null) if nullable_elem(other).is_some() => {
            nullable_elem(other).map(ValueType::Nullable)
        }
        (ValueType::Nullable(elem), other) | (other, ValueType::Nullable(elem))
            if nullable_elem(other) == Some(elem) =>
        {
            Some(ValueType::Nullable(elem))
        }
        (ValueType::EmptyList, other @ ValueType::List(_))
        | (other @ ValueType::List(_), ValueType::EmptyList) => Some(other),
        (ValueType::EmptySet, other @ ValueType::Set(_))
        | (other @ ValueType::Set(_), ValueType::EmptySet) => Some(other),
        (ValueType::EmptyMap, other @ ValueType::Map(..))
        | (other @ ValueType::Map(..), ValueType::EmptyMap) => Some(other),
        (left, right) if left == right => Some(left),
        _ => None,
    }
}

/// Whether a value of type `actual` can flow into a slot of type
/// `expected` (locals, arguments, returns): exact match, or the
/// polymorphic empty list into any list slot.
fn assignable(actual: ValueType, expected: ValueType) -> bool {
    actual == expected
        // `null` into any reference slot, including one that may hold nothing.
        || (actual == ValueType::Null && is_heap_pointer(expected))
        || (actual == ValueType::EmptyList
            && matches!(expected, ValueType::List(_) | ValueType::EmptyList))
        || (actual == ValueType::EmptySet
            && matches!(expected, ValueType::Set(_) | ValueType::EmptySet))
        || (actual == ValueType::EmptyMap
            && matches!(expected, ValueType::Map(..) | ValueType::EmptyMap))
}

/// One nominal record declaration or interned structural shape:
/// values are heap objects with one qword per field in declaration
/// order. A structural shape has an empty name.
struct RecordInfo {
    name: String,
    fields: Vec<(String, ValueType)>,
    /// Fields whose type is a function, as (name, position among *all* the
    /// declared fields). A function has no runtime representation here, so such
    /// a field is not part of the object -- it is resolved at compile time from
    /// the binding that constructed it, exactly like a dictionary's.
    lambda_fields: Vec<(String, usize)>,
    /// False when the declaration could not be typed (generics,
    /// function-typed fields): the entry stays so `Record` indices
    /// remain stable, but every lookup skips it.
    usable: bool,
}

/// The value type of one list element of element type `elem`.
/// A scalar value held directly in a GP register (not a heap pointer).
/// The GC's uniform pointer representation requires these to be boxed
/// into a single-slot RAW_BYTES object when stored in a POINTER_RECORD
/// slot (a record field or a cons-cell value), and unboxed on read.
fn is_boxed_scalar(ty: ValueType) -> bool {
    matches!(ty, ValueType::Int | ValueType::Double | ValueType::Bool)
}

/// Does a value of this type occupy its slot as a heap reference the
/// collector must trace? `EmptyList`/`EmptySet` are the nil pointer (0),
/// which the root scan and the barriers both pass through untouched, so
/// rooting them is harmless and keeps the classification purely by type.
fn is_heap_pointer(ty: ValueType) -> bool {
    matches!(
        ty,
        ValueType::Str
            | ValueType::List(_)
            | ValueType::EmptyList
            | ValueType::Set(_)
            | ValueType::EmptySet
            | ValueType::Map(..)
            | ValueType::EmptyMap
            | ValueType::Record(_)
            | ValueType::Ptr
            | ValueType::Null
            | ValueType::Nullable(_)
            | ValueType::Enum(_)
    )
}

fn elem_value_type(elem: ListElem) -> ValueType {
    match elem {
        ListElem::Int => ValueType::Int,
        ListElem::Double => ValueType::Double,
        ListElem::Bool => ValueType::Bool,
        ListElem::Str => ValueType::Str,
        ListElem::Enum(shape) => ValueType::Enum(shape),
        ListElem::Record(index) => ValueType::Record(index),
        ListElem::Nested { .. } => ValueType::List(
            elem.nested_inner()
                .expect("a nested element is a list of its inner element"),
        ),
    }
}

/// The element type a value of type `ty` can be a list element of.
/// Replace whole-identifier occurrences of each type parameter in an
/// annotation with its argument: `Chain<a>` with `a = Int` becomes
/// `Chain<Int>`, while `Char` is left alone.
/// The annotation a type would be written as, for the types a type variable
/// can be solved to from an argument. Anything else answers `None`, which
/// leaves the variable unsolved rather than guessing.
fn type_annotation_name(ty: ValueType) -> Option<String> {
    Some(
        match ty {
            ValueType::Int => "Int",
            ValueType::Str => "String",
            ValueType::Bool => "Boolean",
            ValueType::Double => "Double",
            _ => return None,
        }
        .to_string(),
    )
}

fn substitute_type_params(text: &str, params: &[String], arguments: &[String]) -> String {
    let mut out = String::with_capacity(text.len());
    let mut token = String::new();
    let flush = |token: &mut String, out: &mut String| {
        if token.is_empty() {
            return;
        }
        match params.iter().position(|param| param == token) {
            Some(at) => out.push_str(&arguments[at]),
            None => out.push_str(token),
        }
        token.clear();
    };
    for ch in text.chars() {
        if ch.is_alphanumeric() || ch == '_' || ch == '\'' {
            token.push(ch);
        } else {
            flush(&mut token, &mut out);
            out.push(ch);
        }
    }
    flush(&mut token, &mut out);
    out
}

/// Every value assigned to `name` inside this expression. A partial walk of
/// the shapes assignments actually appear in; anything it does not look inside
/// simply leaves the binding typed by its initial value, which is the old
/// behaviour.
fn collect_assignments(expr: &Expr, name: &str, out: &mut Vec<Expr>) {
    match expr {
        Expr::Assign {
            name: target,
            value,
            ..
        } => {
            if target == name {
                out.push((**value).clone());
            }
            collect_assignments(value, name, out);
        }
        Expr::Block { expressions, .. } => {
            for expression in expressions {
                collect_assignments(expression, name, out);
            }
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_assignments(condition, name, out);
            collect_assignments(then_branch, name, out);
            if let Some(branch) = else_branch {
                collect_assignments(branch, name, out);
            }
        }
        Expr::While {
            condition, body, ..
        } => {
            collect_assignments(condition, name, out);
            collect_assignments(body, name, out);
        }
        Expr::Foreach { iterable, body, .. } => {
            collect_assignments(iterable, name, out);
            collect_assignments(body, name, out);
        }
        Expr::Match {
            scrutinee, arms, ..
        } => {
            collect_assignments(scrutinee, name, out);
            for arm in arms {
                collect_assignments(&arm.body, name, out);
            }
        }
        Expr::VarDecl { value, .. } => collect_assignments(value, name, out),
        _ => {}
    }
}

/// A lambda written as the only thing in a block is that lambda.
fn peel_block(expr: &Expr) -> &Expr {
    match expr {
        Expr::Block { expressions, .. } if expressions.len() == 1 => peel_block(&expressions[0]),
        other => other,
    }
}

/// `Name<A, B>` split into its head and its arguments, if it is written that
/// way. Nesting is respected, so `Pair<List<Int>, Int>` gives two arguments.
fn split_type_arguments(text: &str) -> Option<(&str, Vec<String>)> {
    let open = text.find('<')?;
    let inner = text.strip_suffix('>')?.get(open + 1..)?;
    let mut arguments = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (at, ch) in inner.char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                arguments.push(inner[start..at].trim().to_string());
                start = at + 1;
            }
            _ => {}
        }
    }
    arguments.push(inner[start..].trim().to_string());
    if arguments.iter().any(|argument| argument.is_empty()) {
        return None;
    }
    Some((&text[..open], arguments))
}

/// The compile-time identity of a literal map key, if the expression is one.
/// Only used to resolve duplicate keys in a map literal, so it just has to
/// tell two different keys apart.
fn literal_key(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Int { value, .. } => Some(format!("i{value}")),
        Expr::String { value, .. } => Some(format!("s{value}")),
        Expr::Bool { value, .. } => Some(format!("b{value}")),
        _ => None,
    }
}

/// The value a nullable join carries, if this type can be one half of one.
/// A nullable is already that join; anything a list can hold can become one.
fn nullable_elem(ty: ValueType) -> Option<ListElem> {
    match ty {
        ValueType::Nullable(elem) => Some(elem),
        other => list_elem_of(other),
    }
}

/// A `Double` the way the evaluator displays it: a whole number keeps one
/// decimal (`3.0`), anything else takes Rust's shortest round-trip form. The
/// x86-64 backend formats static doubles by the same rule.
fn format_double(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{value:.1}")
    } else {
        value.to_string()
    }
}

fn list_elem_of(ty: ValueType) -> Option<ListElem> {
    match ty {
        ValueType::Int => Some(ListElem::Int),
        ValueType::Double => Some(ListElem::Double),
        ValueType::Bool => Some(ListElem::Bool),
        ValueType::Str => Some(ListElem::Str),
        ValueType::Enum(shape) => Some(ListElem::Enum(shape)),
        ValueType::Record(index) => Some(ListElem::Record(index)),
        // A list of lists: one more level over whatever the inner list holds.
        ValueType::List(inner) => Some(match inner.as_scalar() {
            Some(base) => ListElem::Nested { depth: 1, base },
            None => match inner {
                ListElem::Nested { depth, base } => ListElem::Nested {
                    depth: depth + 1,
                    base,
                },
                _ => unreachable!("as_scalar covers every non-nested element"),
            },
        }),
        _ => None,
    }
}

/// A top-level annotated `def` compiled as a real AAPCS64 function:
/// arguments in x0..x7, result in x0, frame record saved by the
/// callee prologue (which is what makes recursion per-frame safe).
/// A def whose signature is not fully concrete, kept whole so each call site
/// can specialise it.
#[derive(Clone)]
struct GenericFunction {
    name: String,
    params: Vec<String>,
    body: Expr,
    /// The return type when the annotation names one this backend models.
    declared_return: Option<ValueType>,
    /// The annotations as written. A return type this backend cannot model on
    /// its own -- a type variable, or one applied to something like `'m<Int>`
    /// -- is still pinned down when it is *the same annotation* as a
    /// parameter's: the result is then that argument's type, which is exactly
    /// what `def pairWithNext<'m>(xs: 'm<Int>): 'm<Int>` says.
    param_annotations: Vec<Option<String>>,
    return_annotation: Option<String>,
}

struct FunctionInfo {
    label: Label,
    params: Vec<(String, ValueType)>,
    /// Parameters bound to a lambda rather than to a runtime slot. A lambda
    /// has no representation at run time in this backend -- it is compiled
    /// where it is called -- so a higher-order def is specialised for the
    /// particular lambda it was handed, and inside the body the parameter
    /// name simply resolves to that lambda.
    lambda_params: Vec<(String, usize)>,
    /// Parameters bound to a dictionary of lambdas, for the same reason.
    dict_params: Vec<(String, Vec<(String, usize)>)>,
    ret: ValueType,
    body: Expr,
}

/// A record declaration with no layout of its own: either generic, or with a
/// field whose type cannot be written down (`name: *`). Its constructor is what
/// pins the fields down, so the declaration is kept as text and instantiated
/// per set of argument types.
#[derive(Clone)]
struct GenericRecord {
    name: String,
    params: Vec<String>,
    /// Field names with their annotations, as written.
    fields: Vec<(String, String)>,
}

/// The compile-time bindings a lambda was written under.
///
/// A lambda has no runtime representation in this backend -- its body is
/// compiled afresh at each call -- so anything it closes over that exists only
/// at compile time has to travel with it. Value bindings are frame slots and
/// stay where they are; another lambda, or a dictionary of them, is a name
/// resolved at compile time, and without carrying those the body would resolve
/// them at the *call* site instead. That is what makes a dictionary-returning
/// function work: the record it answers with holds lambdas that close over the
/// dictionary it was given.
#[derive(Clone, Default)]
struct CapturedEnv {
    lambdas: Vec<(String, usize)>,
    dicts: Vec<(String, Vec<(String, usize)>)>,
}

#[derive(Default)]
struct Emitter {
    asm: Assembler,
    functions: Vec<(String, FunctionInfo)>,
    /// Indices of functions reached from compiled code; only these
    /// are emitted (the stdlib prelude declares many functions the
    /// subset cannot compile — they only fail if actually called).
    pending: Vec<usize>,
    /// Nominal record declarations plus interned structural shapes;
    /// `ValueType::Record` carries an index into this table.
    records: Vec<RecordInfo>,
    /// The records whose rendering is currently being emitted, so a record
    /// that holds itself is rejected instead of printed forever.
    printing_records: Vec<u32>,
    /// The same, for enums: a cons-style one holds its own shape.
    printing_enums: Vec<u32>,
    /// The pseudo-random generator's state, reserved the first time a program
    /// asks for a random number. Zero until seeded, which is where the
    /// evaluator's own generator starts.
    random_state_cell: Option<DataLabel>,
    /// Lazily emitted set helpers (called via `bl`): scalar / string
    /// membership scans and a cons-list reverse.
    member_scalar_label: Option<Label>,
    member_string_label: Option<Label>,
    list_reverse_label: Option<Label>,
    /// Label of the GC load barrier's slow path (`gc_load_barrier_slow`).
    /// Created lazily so the first user -- a barriered read or
    /// `reserve_gc_state`, whichever comes first -- owns it; the routine
    /// body itself is always emitted with the rest of the GC runtime.
    gc_load_barrier_label: Option<Label>,
    /// `__bss` cells for the GC shadow stack (base pointer, top index),
    /// shared by `reserve_gc_state` and the mutator's root pushes.
    shadow_stack_cells: Option<(DataLabel, DataLabel)>,
    /// Labels of the two tiny shadow-stack helpers, emitted on demand.
    shadow_push_label: Option<Label>,
    shadow_pop_label: Option<Label>,
    /// Label of `gc_alloc`, shared between the mutator's allocation sites
    /// and the GC runtime that defines it.
    gc_alloc_label: Option<Label>,
    /// `--gc-log` / `--gc-stress` / `--gc-poison`, threaded in from the CLI.
    gc_log: bool,
    gc_stress: bool,
    gc_poison: bool,
    /// Number of shadow-stack roots pushed in each open scope, so leaving
    /// a scope pops exactly its own.
    scope_root_counts: Vec<usize>,
    /// Names of enums `desugar_enums` lowered to `__gc_record` shape;
    /// annotations naming them type as plain heap pointers.
    lowered_enums: std::collections::HashSet<String>,
    scopes: Vec<HashMap<String, (u32, ValueType)>>,
    /// Lambda bodies interned by `val f = (x) => ...`, and the names bound to
    /// them, scoped like ordinary locals. A lambda has no runtime
    /// representation here: it is compiled where it is *called*, with the
    /// parameter types taken from the arguments at that site, so no closure
    /// object or indirect call is needed for the cases the language's own
    /// programs use.
    /// Defs whose signature is not fully concrete -- `def fact(n) = ...`, or
    /// a generic one like `def display<'a>(x: 'a): Unit where Show<'a>`.
    /// They are compiled once per distinct tuple of argument types, keyed in
    /// `specializations`, because the parameter types only exist at the call
    /// site.
    generic_functions: Vec<GenericFunction>,
    specializations: HashMap<(usize, Vec<ValueType>), usize>,
    lambdas: Vec<(Vec<String>, Expr)>,
    /// The environment each lambda was interned under, indexed as `lambdas`.
    lambda_envs: Vec<CapturedEnv>,
    lambda_bindings: Vec<HashMap<String, usize>>,
    /// Names bound to another name -- `val sub = substring` binds a builtin
    /// (or any function) as a value. There is no function value at run time
    /// here, so the binding is compile-time and `sub(...)` compiles as a call
    /// to what it names.
    alias_bindings: Vec<HashMap<String, String>>,
    /// Names bound to a *record of lambdas* -- the dictionary-passing shape
    /// `val Show_Int_dict = record { show: (x: Int) => ... }`. Like a lambda,
    /// such a record has no runtime representation here: `dict.show(x)`
    /// resolves the field to its lambda and compiles the body at the call.
    dict_bindings: Vec<HashMap<String, Vec<(String, usize)>>>,
    /// Generic record declarations, kept as text: `record GPoint<'x, 'y>` has
    /// no single layout, so an instantiation is interned when one is named --
    /// `#GPoint<Int, Int>` in a field's type, or a constructor whose arguments
    /// pin the parameters down.
    generic_records: Vec<GenericRecord>,
    /// Generic enum declarations: the enum's name and its variants in
    /// declaration order, each with how many fields it carries. The order is
    /// the tag, so it has to be the declaration's.
    generic_enums: Vec<(String, Vec<(String, usize)>)>,
    /// Whether each enum has type parameters, i.e. whether its payloads vary
    /// per value rather than being fixed by the declaration.
    enum_is_generic: Vec<bool>,
    /// Each enum's type parameters, for instantiating an applied one.
    enum_type_params: Vec<Vec<String>>,
    /// Interned shapes for applied generic enums, keyed by the enum and its
    /// type arguments, so `Chain<Int>` is one shape however often it is named.
    applied_enum_shapes: HashMap<(usize, Vec<String>), u32>,
    /// Frame slots that a `match` has already been compiled against. Widening
    /// one of those in place would be wrong -- the match decided which arms
    /// were reachable from the shape it saw -- so those assignments are still
    /// reported instead.
    matched_enum_slots: std::collections::HashSet<u32>,
    /// Payload annotations per enum, per variant, as written -- what a
    /// canonical shape is built from.
    enum_annotations: Vec<Vec<Vec<String>>>,
    /// The canonical shape of each enum that has one (no type parameters).
    canonical_enum_shapes: Vec<Option<u32>>,
    /// Interned shapes: the enum's index in `generic_enums`, and what each
    /// variant carries here. `Never` means "not known from this value", which
    /// is what a nullary constructor says about the other variants' payloads.
    enum_shapes: Vec<(usize, Vec<Vec<ValueType>>)>,
    /// Bodies handed to `thread`, run after the program's own statements.
    /// Not a thread: the hand-emitted x86-64 backend makes the same
    /// approximation, and matching it keeps the targets' output identical.
    queued_threads: Vec<(usize, Span)>,
    /// Values assigned to a `mutable` binding later in the block it is
    /// declared in, collected before the block is compiled so the binding's
    /// slot can be typed for all of them.
    pending_widenings: HashMap<String, Vec<Expr>>,
    /// Names bound to a string literal. Some builtins need the *text* of an
    /// argument at compile time -- `Dir#mkdirs` creates each parent directory,
    /// so it splits the path here -- and a literal put in a `val` first is the
    /// ordinary way to write one.
    literal_strings: Vec<HashMap<String, String>>,
    /// `val`-bound doubles whose value is known while compiling, which is the
    /// only kind this backend can print -- the same limit the x86-64 backend
    /// has, reached a different way. A `mutable` is never recorded: what it
    /// holds at a given point depends on the paths taken to get there.
    literal_doubles: Vec<HashMap<String, f64>>,
    next_local_offset: u32,
    /// The deepest `next_local_offset` reached in the function being emitted.
    /// Slots are handed back when an inlined lambda's parameters die, so the
    /// current offset is not the frame's size -- this is.
    max_local_offset: u32,
}

impl Emitter {
    fn lookup(&self, name: &str) -> Option<(u32, ValueType)> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    /// Resolve a type annotation against scalars, list types, lowered
    /// enums, and declared records (`#Point` or `Point`).
    fn annotation_type(&mut self, text: &str, span: Span) -> Result<ValueType, Diagnostic> {
        let trimmed = text.trim();
        let bare = trimmed.trim_start_matches('#');
        // Lowered monomorphic enum values travel as plain heap
        // pointers.
        if self.lowered_enums.contains(bare) {
            return Ok(ValueType::Ptr);
        }
        if let Some(index) = self
            .records
            .iter()
            .position(|record| record.usable && !record.name.is_empty() && record.name == bare)
        {
            return Ok(ValueType::Record(index as u32));
        }
        // An enum this backend compiles itself, named by its declaration.
        if let Some(enum_index) = self
            .generic_enums
            .iter()
            .position(|(declared, _)| declared == bare)
            && !self.enum_is_generic[enum_index]
            && let Some(shape) = self.canonical_enum_shape(enum_index, span)
        {
            return Ok(ValueType::Enum(shape));
        }
        // `Result<JsonStep, String>`: a generic enum applied to concrete types.
        if let Some((head, arguments)) = split_type_arguments(bare)
            && let Some(shape) = self.instantiate_generic_enum(head, &arguments, span)
        {
            return Ok(ValueType::Enum(shape));
        }
        // `List<X>` / `Set<X>` over anything nameable, not just the three
        // spellings written out below.
        if let Some((head, arguments)) = split_type_arguments(trimmed)
            && arguments.len() == 1
            && (head == "List" || head == "Set")
            && let Ok(inner) = self.annotation_type(&arguments[0], span)
            && let Some(elem) = list_elem_of(inner)
        {
            return Ok(if head == "List" {
                ValueType::List(elem)
            } else {
                ValueType::Set(elem)
            });
        }
        if bare == "Point" {
            let point = self.instantiate_builtin_point();
            return Ok(ValueType::Record(point));
        }
        // `#GPoint<Int, Int>`: a generic record named with its arguments. The
        // parameters are substituted into the field types and the result is
        // interned under the same name, so two mentions of the same
        // instantiation are the same type.
        if let Some((head, arguments)) = split_type_arguments(bare) {
            return self.instantiate_generic_record(head, &arguments, span);
        }
        match trimmed {
            "Int" | "Long" | "Short" | "Byte" => Ok(ValueType::Int),
            "Double" | "Float" => Ok(ValueType::Double),
            "Bool" | "Boolean" => Ok(ValueType::Bool),
            "String" => Ok(ValueType::Str),
            "List<Int>" => Ok(ValueType::List(ListElem::Int)),
            "List<Bool>" => Ok(ValueType::List(ListElem::Bool)),
            "List<String>" => Ok(ValueType::List(ListElem::Str)),
            "Set<Int>" => Ok(ValueType::Set(ListElem::Int)),
            "Set<Bool>" => Ok(ValueType::Set(ListElem::Bool)),
            "Set<String>" => Ok(ValueType::Set(ListElem::Str)),
            "Unit" => Ok(ValueType::Unit),
            other => Err(unsupported(span, &format!("type annotation `{other}`"))),
        }
    }

    /// Intern a structural record shape, reusing an existing entry so
    /// equal shapes share one `ValueType::Record` index.
    /// Intern the instantiation of a generic record for these type arguments,
    /// answering with its type. Interned by name *and* field types, so the
    /// same instantiation named twice is one record.
    fn instantiate_generic_record(
        &mut self,
        name: &str,
        arguments: &[String],
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        let Some(GenericRecord { params, fields, .. }) = self
            .generic_records
            .iter()
            .find(|record| record.name == name && record.params.len() == arguments.len())
            .cloned()
        else {
            return Err(unsupported(span, &format!("record `{name}`")));
        };
        let mut typed = Vec::with_capacity(fields.len());
        for (field, annotation) in &fields {
            // A field named by a parameter takes that argument's type; any
            // other annotation resolves as it stands (including a nested
            // instantiation, which recurses through here).
            let text = match params.iter().position(|param| param == annotation.trim()) {
                Some(at) => arguments[at].clone(),
                None => annotation.clone(),
            };
            typed.push((field.clone(), self.annotation_type(&text, span)?));
        }
        if let Some(index) = self
            .records
            .iter()
            .position(|record| record.usable && record.name == name && record.fields == typed)
        {
            return Ok(ValueType::Record(index as u32));
        }
        self.records.push(RecordInfo {
            name: name.to_string(),
            fields: typed,
            lambda_fields: Vec::new(),
            usable: true,
        });
        Ok(ValueType::Record((self.records.len() - 1) as u32))
    }

    /// The canonical shape of an enum with no type parameters, whose payloads
    /// come from its declaration rather than from each value.
    ///
    /// A recursive enum needs this: `JArr(items: List<Json>)` describes itself,
    /// so shapes taken per value would grow one level deeper on every merge and
    /// never settle. With no type parameters there is nothing to vary anyway --
    /// the declaration says everything -- so the shape is allocated first,
    /// empty, and then filled in, which lets a payload name the enum it is
    /// part of.
    fn canonical_enum_shape(&mut self, enum_index: usize, span: Span) -> Option<u32> {
        if let Some(shape) = self.canonical_enum_shapes[enum_index] {
            return Some(shape);
        }
        let variants = self.generic_enums[enum_index].1.clone();
        let empty: Vec<Vec<ValueType>> = variants.iter().map(|_| Vec::new()).collect();
        self.enum_shapes.push((enum_index, empty));
        let shape = (self.enum_shapes.len() - 1) as u32;
        self.canonical_enum_shapes[enum_index] = Some(shape);
        let annotations = self.enum_annotations[enum_index].clone();
        let mut payloads = Vec::with_capacity(variants.len());
        for fields in &annotations {
            let mut typed = Vec::with_capacity(fields.len());
            for annotation in fields {
                match self.annotation_type(annotation, span) {
                    Ok(ty) => typed.push(ty),
                    Err(_) => {
                        // A payload this backend cannot name: leave the shape
                        // unfilled and let the constructor report it.
                        self.canonical_enum_shapes[enum_index] = None;
                        return None;
                    }
                }
            }
            payloads.push(typed);
        }
        self.enum_shapes[shape as usize].1 = payloads;
        Some(shape)
    }

    /// The shape of a generic enum named with its arguments -- `Result<JsonStep,
    /// String>`. Applied to concrete types the declaration says everything, so
    /// the shape is built once from it rather than accumulated per value, which
    /// is what lets an annotated function's return type be complete instead of
    /// as much as the first constructor happened to reveal.
    fn instantiate_generic_enum(
        &mut self,
        name: &str,
        arguments: &[String],
        span: Span,
    ) -> Option<u32> {
        let enum_index = self
            .generic_enums
            .iter()
            .position(|(declared, _)| declared == name)?;
        if !self.enum_is_generic[enum_index] {
            return None;
        }
        let params = self.enum_type_params[enum_index].clone();
        if params.len() != arguments.len() {
            return None;
        }
        let key = (enum_index, arguments.to_vec());
        if let Some(shape) = self.applied_enum_shapes.get(&key) {
            return Some(*shape);
        }
        // Allocated empty and recorded first, so a payload that names this
        // very instantiation -- `Cons(head: 'a, tail: Chain<'a>)` -- finds it
        // instead of recursing forever.
        let variants = self.generic_enums[enum_index].1.len();
        self.enum_shapes
            .push((enum_index, vec![Vec::new(); variants]));
        let shape = (self.enum_shapes.len() - 1) as u32;
        self.applied_enum_shapes.insert(key, shape);
        let annotations = self.enum_annotations[enum_index].clone();
        let mut payloads = Vec::with_capacity(annotations.len());
        for fields in &annotations {
            let mut typed = Vec::with_capacity(fields.len());
            for annotation in fields {
                // Substitute the arguments into the annotation, so both `a`
                // and `Chain<a>` resolve. Whole identifiers only: a plain
                // `replace` would rewrite the `a` inside `Char`.
                let text = substitute_type_params(annotation.trim(), &params, arguments);
                match self.annotation_type(&text, span) {
                    Ok(ty) => typed.push(ty),
                    Err(_) => {
                        self.applied_enum_shapes
                            .remove(&(enum_index, arguments.to_vec()));
                        return None;
                    }
                }
            }
            payloads.push(typed);
        }
        self.enum_shapes[shape as usize].1 = payloads;
        Some(shape)
    }

    /// The enum and variant a constructor name belongs to, with the variant's
    /// tag and how many fields it carries.
    fn enum_variant(&self, name: &str) -> Option<(usize, usize, usize)> {
        self.generic_enums
            .iter()
            .enumerate()
            .find_map(|(enum_index, (_, variants))| {
                variants
                    .iter()
                    .position(|(variant, _)| variant == name)
                    .map(|tag| (enum_index, tag, variants[tag].1))
            })
    }

    /// The annotation text for a type, for the cases that have a name. Used to
    /// turn a constructor's argument types back into the enum's type arguments.
    fn annotation_of(&self, ty: ValueType) -> Option<String> {
        Some(match ty {
            ValueType::Int => "Int".to_string(),
            ValueType::Double => "Double".to_string(),
            ValueType::Bool => "Boolean".to_string(),
            ValueType::Str => "String".to_string(),
            ValueType::List(elem) => {
                format!("List<{}>", self.annotation_of(elem_value_type(elem))?)
            }
            ValueType::Set(elem) => {
                format!("Set<{}>", self.annotation_of(elem_value_type(elem))?)
            }
            ValueType::Record(index) => {
                let record = self.records.get(index as usize)?;
                if record.name.is_empty() {
                    return None;
                }
                record.name.clone()
            }
            ValueType::Enum(shape) => {
                let (enum_index, _) = self.enum_shapes.get(shape as usize)?;
                self.generic_enums.get(*enum_index)?.0.clone()
            }
            _ => return None,
        })
    }

    /// The canonical shape for a *generic* enum whose type arguments this
    /// constructor pins down: `Cons(1, rest)` on `Chain<'a>` says `'a` is Int,
    /// so the value has the shape of `Chain<Int>`.
    ///
    /// Without this, a self-referential constructor's shape describes one more
    /// level on every use -- `Cons(Int, <shape of the tail>)` -- and a loop
    /// that rebuilds the value never settles on a type.
    fn canonical_applied_enum_shape(
        &mut self,
        enum_index: usize,
        tag: usize,
        payload: &[ValueType],
        span: Span,
    ) -> Option<u32> {
        let params = self.enum_type_params.get(enum_index)?.clone();
        if params.is_empty() {
            return None;
        }
        let annotations = self.enum_annotations.get(enum_index)?.get(tag)?.clone();
        if annotations.len() != payload.len() {
            return None;
        }
        let mut arguments: Vec<Option<String>> = vec![None; params.len()];
        for (annotation, ty) in annotations.iter().zip(payload.iter()) {
            // Only a payload that *is* a parameter says what that parameter is.
            if let Some(at) = params.iter().position(|param| param == annotation.trim()) {
                let named = self.annotation_of(*ty)?;
                match &arguments[at] {
                    Some(existing) if *existing != named => return None,
                    _ => arguments[at] = Some(named),
                }
            }
        }
        let arguments: Vec<String> = arguments.into_iter().collect::<Option<Vec<_>>>()?;
        self.instantiate_generic_enum(&self.generic_enums[enum_index].0.clone(), &arguments, span)
    }

    /// Intern the shape of an enum whose `tag` variant carries `payload` and
    /// whose other variants are not described by this value.
    fn intern_enum_shape(&mut self, enum_index: usize, tag: usize, payload: Vec<ValueType>) -> u32 {
        let variants = self.generic_enums[enum_index].1.len();
        let mut shape: Vec<Vec<ValueType>> = (0..variants)
            .map(|which| {
                if which == tag {
                    payload.clone()
                } else {
                    vec![ValueType::Never; self.generic_enums[enum_index].1[which].1]
                }
            })
            .collect();
        shape.shrink_to_fit();
        if let Some(index) = self
            .enum_shapes
            .iter()
            .position(|(which, known)| *which == enum_index && *known == shape)
        {
            return index as u32;
        }
        self.enum_shapes.push((enum_index, shape));
        (self.enum_shapes.len() - 1) as u32
    }

    /// Build a generic enum's value: `[tag][field...]`, every slot a pointer so
    /// the collector traces it like any other record.
    fn emit_enum_value(
        &mut self,
        enum_index: usize,
        tag: usize,
        arguments: &[Expr],
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        self.asm.mov_imm64(Reg::X0, tag as u64);
        self.emit_box_scalar();
        self.push_rooted(Reg::X0);
        let mut payload = Vec::with_capacity(arguments.len());
        for argument in arguments {
            let ty = self.expression(argument)?;
            if ty == ValueType::Unit || ty == ValueType::Never {
                return Err(unsupported(argument.span(), "an enum field of this type"));
            }
            if is_boxed_scalar(ty) {
                self.emit_box_scalar();
            }
            self.push_rooted(Reg::X0);
            payload.push(ty);
        }
        self.emit_record_object(arguments.len() + 1);
        // An enum with no type parameters has one shape for every value, taken
        // from its declaration; only a generic one varies per value.
        if !self.enum_is_generic[enum_index]
            && let Some(shape) = self.canonical_enum_shape(enum_index, span)
        {
            return Ok(ValueType::Enum(shape));
        }
        // A generic enum whose arguments this constructor pins down has a
        // canonical shape too, which is what keeps a self-referential one from
        // describing one more level on every use.
        if let Some(shape) = self.canonical_applied_enum_shape(enum_index, tag, &payload, span) {
            return Ok(ValueType::Enum(shape));
        }
        let shape = self.intern_enum_shape(enum_index, tag, payload);
        Ok(ValueType::Enum(shape))
    }

    /// `f(a)(b)(c)` where `f` is a def whose body is `(b) => (c) => ...`.
    ///
    /// The curried levels are parameters like any other, so the whole chain is
    /// compiled as one call taking all of them. That is what lets such a def
    /// *recurse*: inlining the lambdas one level at a time would have to unroll
    /// the recursion, and how many times is a run-time answer.
    ///
    /// Answers `None` when the shape does not fit, so the ordinary paths run.
    fn curried_generic_call(
        &mut self,
        callee: &Expr,
        arguments: &[Expr],
        span: Span,
    ) -> Option<Result<ValueType, Diagnostic>> {
        let mut spine: Vec<&[Expr]> = vec![arguments];
        let mut head = callee;
        while let Expr::Call {
            callee, arguments, ..
        } = head
        {
            spine.push(arguments);
            head = callee;
        }
        if spine.len() < 2 {
            return None;
        }
        spine.reverse();
        let Expr::Identifier { name, .. } = head else {
            return None;
        };
        let generic = self
            .generic_functions
            .iter()
            .rposition(|generic| generic.name == *name && generic.params.len() == spine[0].len())?;
        // Peel one lambda per applied level, collecting their parameters.
        let mut params = self.generic_functions[generic].params.clone();
        let mut body = self.generic_functions[generic].body.clone();
        for group in &spine[1..] {
            let Expr::Lambda {
                params: level,
                body: inner,
                ..
            } = peel_block(&body)
            else {
                return None;
            };
            if level.len() != group.len() {
                return None;
            }
            params.extend(level.iter().cloned());
            body = (**inner).clone();
        }
        let flattened: Vec<Expr> = spine
            .iter()
            .flat_map(|group| group.iter().cloned())
            .collect();
        let curried = format!("{name}#curried{}", params.len());
        let index = match self
            .generic_functions
            .iter()
            .position(|generic| generic.name == curried && generic.params.len() == params.len())
        {
            Some(index) => index,
            None => {
                let annotations = vec![None; params.len()];
                self.generic_functions.push(GenericFunction {
                    name: curried,
                    params,
                    body,
                    declared_return: None,
                    param_annotations: annotations,
                    return_annotation: None,
                });
                self.generic_functions.len() - 1
            }
        };
        Some(self.emit_generic_function_call(index, &flattened, span))
    }

    /// Note, for every `mutable` declared in this block, what is assigned to
    /// it later on, so its slot can be typed for all of them.
    fn collect_block_widenings(&mut self, expressions: &[Expr]) {
        for (position, expression) in expressions.iter().enumerate() {
            let Expr::VarDecl {
                name,
                mutable: true,
                ..
            } = expression
            else {
                continue;
            };
            let mut assigned = Vec::new();
            for later in &expressions[position + 1..] {
                collect_assignments(later, name, &mut assigned);
            }
            self.pending_widenings.insert(name.clone(), assigned);
        }
    }

    /// The type a `mutable` binding's slot needs: its initial type merged with
    /// every type assigned to it later in the statement being compiled.
    ///
    /// Only widening merges apply -- two shapes of one enum, an empty list and
    /// a list -- so a genuine type change is still reported at the assignment.
    fn widened_mutable_type(&mut self, name: &str, initial: ValueType) -> ValueType {
        let assigned = self
            .pending_widenings
            .get(name)
            .cloned()
            .unwrap_or_default();
        if assigned.is_empty() {
            return initial;
        }
        // The binding is not in scope yet, and what is assigned to it usually
        // mentions it -- `c = Cons(v, c)` -- so the name is bound to what the
        // slot holds so far and the whole thing settles by repetition.
        let mut widened = initial;
        for _ in 0..4 {
            let mut grew = false;
            for value in &assigned {
                let mut locals = vec![(name.to_string(), widened)];
                let Some(ty) = self.static_type_under(value, &mut locals, 0) else {
                    continue;
                };
                let Some(merged) = self.merge_types(widened, ty) else {
                    return initial;
                };
                if merged != widened {
                    widened = merged;
                    grew = true;
                }
            }
            if !grew {
                break;
            }
        }
        widened
    }

    /// Whether two types are the same, comparing enum shapes by what they say
    /// rather than by which entry they are.
    ///
    /// One shape can be interned more than once -- an annotation names an
    /// instantiation, a constructor builds one, a merge produces one -- and
    /// two entries with identical contents describe the same values, so an
    /// index comparison would reject a value that fits perfectly.
    fn same_shape(&self, left: ValueType, right: ValueType) -> bool {
        match (left, right) {
            (ValueType::Enum(left), ValueType::Enum(right)) => {
                left == right || self.enum_shapes[left as usize] == self.enum_shapes[right as usize]
            }
            (left, right) => left == right,
        }
    }

    /// Merge two branch types, including two shapes of the same enum -- which
    /// the free function cannot do, since what a shape knows lives in a table
    /// here. `Never` in a payload means "not known from that value", so the two
    /// merge field by field and a nullary constructor learns the other's
    /// payload.
    fn merge_types(&mut self, left: ValueType, right: ValueType) -> Option<ValueType> {
        if let (ValueType::Enum(left), ValueType::Enum(right)) = (left, right)
            && left != right
        {
            let (left_enum, left_shape) = self.enum_shapes[left as usize].clone();
            let (right_enum, right_shape) = self.enum_shapes[right as usize].clone();
            if left_enum != right_enum {
                return None;
            }
            let mut merged = Vec::with_capacity(left_shape.len());
            for (left_payload, right_payload) in left_shape.iter().zip(right_shape.iter()) {
                let mut fields = Vec::with_capacity(left_payload.len());
                for (left_field, right_field) in left_payload.iter().zip(right_payload.iter()) {
                    // Payloads can be enums themselves -- `Ok(Step(...))` --
                    // so this recurses. It terminates because a payload's
                    // shape is always interned before the shape holding it.
                    fields.push(self.merge_types(*left_field, *right_field)?);
                }
                merged.push(fields);
            }
            if let Some(index) = self
                .enum_shapes
                .iter()
                .position(|(which, known)| *which == left_enum && *known == merged)
            {
                return Some(ValueType::Enum(index as u32));
            }
            self.enum_shapes.push((left_enum, merged));
            return Some(ValueType::Enum((self.enum_shapes.len() - 1) as u32));
        }
        merge_branch_types(left, right)
    }

    /// The bindings a pattern makes, as types: the inference's half of
    /// `emit_pattern`. Answers false when the pattern cannot match this shape,
    /// which is how an unreachable arm is skipped rather than typed.
    fn bind_pattern_types(
        &mut self,
        ty: ValueType,
        pattern: &Pattern,
        locals: &mut Vec<(String, ValueType)>,
    ) -> bool {
        match pattern {
            Pattern::Wildcard { .. } => true,
            Pattern::Variable { name, .. } => {
                locals.push((name.clone(), ty));
                true
            }
            Pattern::Constructor { name, args, .. } => {
                let ValueType::Enum(shape) = ty else {
                    return false;
                };
                let (enum_index, payloads) = self.enum_shapes[shape as usize].clone();
                let Some((which, tag, arity)) = self.enum_variant(name) else {
                    return false;
                };
                if which != enum_index || arity != args.len() {
                    return false;
                }
                if payloads[tag].contains(&ValueType::Never) && !args.is_empty() {
                    return false;
                }
                for (position, argument) in args.iter().enumerate() {
                    if !self.bind_pattern_types(payloads[tag][position], argument, locals) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }

    /// Test one pattern against the value in `slot`, jumping to `next` when it
    /// does not match, and answer with the slots it bound.
    ///
    /// Recursive, because patterns are: `case Ok(Step(value, next))` tests two
    /// tags and binds from the inner value. Nothing is rooted here -- a test
    /// that fails jumps out, and a root pushed on the way would never be
    /// dropped -- so the caller roots the slots once the whole pattern held.
    /// Between the tests only loads happen, so nothing can move in between.
    fn emit_pattern(
        &mut self,
        slot: u32,
        ty: ValueType,
        pattern: &Pattern,
        next: Label,
        bound: &mut Vec<(String, u32, ValueType)>,
        span: Span,
    ) -> Result<bool, Diagnostic> {
        match pattern {
            Pattern::Wildcard { .. } => Ok(true),
            Pattern::Variable { name, .. } => {
                bound.push((name.clone(), slot, ty));
                Ok(true)
            }
            Pattern::Constructor { name, args, .. } => {
                let ValueType::Enum(shape) = ty else {
                    return Err(unsupported(span, "a constructor pattern on this value"));
                };
                let (enum_index, payloads) = self.enum_shapes[shape as usize].clone();
                let Some((which, tag, arity)) = self.enum_variant(name) else {
                    return Err(unsupported(span, "a pattern of this kind"));
                };
                if which != enum_index || arity != args.len() {
                    return Err(unsupported(span, "a pattern from another enum"));
                }
                // A payload this shape never carries means the arm is dead.
                if payloads[tag].contains(&ValueType::Never) && !args.is_empty() {
                    return Ok(false);
                }
                self.asm.load_local(Reg::X0, slot);
                self.emit_gc_load_ptr(Reg::X0, 0); // the boxed tag
                self.emit_unbox_scalar();
                self.asm.cmp_imm(Reg::X0, tag as u32);
                self.asm.branch(next, BranchKind::Conditional(Cond::Ne));
                for (position, argument) in args.iter().enumerate() {
                    let field_ty = payloads[tag][position];
                    if matches!(argument, Pattern::Wildcard { .. }) {
                        continue;
                    }
                    self.asm.load_local(Reg::X0, slot);
                    self.emit_gc_load_ptr(Reg::X0, (position as u32 + 1) * 8);
                    if is_boxed_scalar(field_ty) {
                        self.emit_unbox_scalar();
                    }
                    let field_slot = self.reserve_locals(8);
                    self.asm.store_local(Reg::X0, field_slot);
                    if !self.emit_pattern(field_slot, field_ty, argument, next, bound, span)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            _ => Err(unsupported(span, "a pattern of this kind")),
        }
    }

    /// `foreach (x in xs) { ... }`: walk a cons chain, binding each element and
    /// running the body for its effect.
    ///
    /// The cursor lives in a rooted slot and is re-read each turn, because the
    /// body allocates -- the collector may move the chain while the loop is
    /// halfway along it.
    fn emit_foreach(
        &mut self,
        binding: &str,
        iterable: &Expr,
        body: &Expr,
        span: Span,
    ) -> Result<(), Diagnostic> {
        let mark = self.open_temp_roots();
        let elem = match self.expression(iterable)? {
            ValueType::List(elem) | ValueType::Set(elem) => elem,
            // Nothing to walk, but the iterable still had to be evaluated.
            ValueType::EmptyList | ValueType::EmptySet => {
                self.close_temp_roots(mark);
                return Ok(());
            }
            _ => return Err(unsupported(iterable.span(), "iterating over this value")),
        };
        let cursor = self.reserve_locals(16);
        let element = cursor + 8;
        self.asm.store_local(Reg::X0, cursor);
        self.emit_root_frame_slot(cursor);
        let elem_ty = elem_value_type(elem);
        let loop_start = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.bind(loop_start);
        self.asm.load_local(Reg::X0, cursor);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X0));
        self.emit_gc_load_ptr(Reg::X0, 0);
        if is_boxed_scalar(elem_ty) {
            self.emit_unbox_scalar();
        }
        self.asm.store_local(Reg::X0, element);
        self.push_binding_scopes();
        self.scope_root_counts.push(0);
        self.scopes
            .last_mut()
            .expect("emitter scope")
            .insert(binding.to_string(), (element, elem_ty));
        if is_heap_pointer(elem_ty) {
            self.emit_root_frame_slot(element);
        }
        // The body's reads have to see what the body's writes will make of a
        // binding, because every turn after the first reads what the previous
        // one wrote.
        self.widen_loop_mutables(body);
        self.statement(body)?;
        self.pop_binding_scopes();
        let roots = self.scope_root_counts.pop().expect("emitter root scope");
        self.emit_shadow_pop(roots);
        self.asm.load_local(Reg::X0, cursor);
        self.emit_gc_load_ptr(Reg::X0, 8);
        self.asm.store_local(Reg::X0, cursor);
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(done);
        self.close_temp_roots(mark);
        let _ = span;
        Ok(())
    }

    /// `o match { case Some(v) => ...; case None => ... }` on an enum this
    /// backend compiles itself.
    ///
    /// The value carries its tag in its first slot, so an arm is a comparison
    /// against that and, when it matches, its fields bound to locals. The
    /// scrutinee lives in a rooted slot because the arms allocate.
    fn emit_enum_match(
        &mut self,
        scrutinee: &Expr,
        arms: &[MatchArm],
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        let mark = self.open_temp_roots();
        if let Expr::Identifier { name, .. } = scrutinee
            && let Some((offset, _)) = self.lookup(name)
        {
            self.matched_enum_slots.insert(offset);
        }
        let scrutinee_ty = self.expression(scrutinee)?;
        if !matches!(scrutinee_ty, ValueType::Enum(_)) {
            return Err(unsupported(scrutinee.span(), "matching on this value"));
        }
        let value = self.reserve_locals(8);
        self.asm.store_local(Reg::X0, value);
        self.emit_root_frame_slot(value);

        let end = self.asm.new_label();
        let mut result: Option<ValueType> = None;
        let mut exhaustive = false;
        for arm in arms {
            if arm.guard.is_some() {
                return Err(unsupported(arm.span, "a match arm with a guard"));
            }
            if matches!(
                arm.pattern,
                Pattern::Wildcard { .. } | Pattern::Variable { .. }
            ) {
                exhaustive = true;
            }
            let next = self.asm.new_label();
            let mut bound = Vec::new();
            let reachable = self.emit_pattern(
                value,
                scrutinee_ty,
                &arm.pattern,
                next,
                &mut bound,
                arm.span,
            )?;
            if !reachable {
                // An arm the shape says cannot be taken: its comparison stands
                // (it always falls through) and its body is left uncompiled.
                self.asm.bind(next);
                continue;
            }
            self.push_binding_scopes();
            self.scope_root_counts.push(0);
            for (name, slot, ty) in &bound {
                self.scopes
                    .last_mut()
                    .expect("emitter scope")
                    .insert(name.clone(), (*slot, *ty));
                if is_heap_pointer(*ty) {
                    self.emit_root_frame_slot(*slot);
                }
            }
            let arm_ty = self.expression(&arm.body)?;
            self.pop_binding_scopes();
            let roots = self.scope_root_counts.pop().expect("emitter root scope");
            self.emit_shadow_pop(roots);
            result = Some(match result {
                None => arm_ty,
                Some(previous) => self
                    .merge_types(previous, arm_ty)
                    .ok_or_else(|| unsupported(arm.span, "match arms with different types"))?,
            });
            self.asm.branch(end, BranchKind::Unconditional);
            self.asm.bind(next);
        }
        // Falling past every arm is the evaluator's match error.
        if !exhaustive {
            self.asm
                .emit_write_rodata(STDERR_FD, b"klassic: no match\n");
            self.asm.emit_exit(1);
        }
        self.asm.bind(end);
        self.close_temp_roots(mark);
        result.ok_or_else(|| unsupported(span, "a match with no arms"))
    }

    /// The built-in `Point`: two Int fields, known without a declaration.
    fn instantiate_builtin_point(&mut self) -> u32 {
        let fields = vec![
            ("x".to_string(), ValueType::Int),
            ("y".to_string(), ValueType::Int),
        ];
        if let Some(index) = self
            .records
            .iter()
            .position(|record| record.usable && record.name == "Point" && record.fields == fields)
        {
            return index as u32;
        }
        self.records.push(RecordInfo {
            name: "Point".to_string(),
            fields,
            lambda_fields: Vec::new(),
            usable: true,
        });
        (self.records.len() - 1) as u32
    }

    fn intern_structural_record(&mut self, fields: Vec<(String, ValueType)>) -> u32 {
        if let Some(index) = self
            .records
            .iter()
            .position(|record| record.usable && record.name.is_empty() && record.fields == fields)
        {
            return index as u32;
        }
        self.records.push(RecordInfo {
            name: String::new(),
            fields,
            lambda_fields: Vec::new(),
            usable: true,
        });
        (self.records.len() - 1) as u32
    }

    /// Hand out `bytes` of frame space, remembering the high-water mark so the
    /// prologue's reservation can be patched to the real size.
    fn reserve_locals(&mut self, bytes: u32) -> u32 {
        let offset = self.next_local_offset;
        self.next_local_offset += bytes;
        self.max_local_offset = self.max_local_offset.max(self.next_local_offset);
        offset
    }

    /// Hand slots back: the offset shrinks, the high-water mark does not.
    fn release_locals_to(&mut self, offset: u32) {
        self.next_local_offset = offset;
    }

    fn declare_local(&mut self, name: &str, ty: ValueType) -> u32 {
        let offset = self.reserve_locals(8);
        self.scopes
            .last_mut()
            .expect("emitter scope")
            .insert(name.to_string(), (offset, ty));
        offset
    }

    /// Compile `expr`, leaving its value in x0. Binary operands use
    /// the machine stack (16-byte strides keep sp aligned).
    fn expression(&mut self, expr: &Expr) -> Result<ValueType, Diagnostic> {
        match expr {
            Expr::Int { value, .. } => {
                self.asm.mov_imm64(Reg::X0, *value as u64);
                Ok(ValueType::Int)
            }
            // `expr cleanup { ... }`: the clause runs after the expression it
            // is attached to, and the expression's value is still the value.
            // A heap result is parked as a root, because the clause can
            // allocate.
            Expr::Cleanup { body, cleanup, .. } => {
                let body_ty = self.expression(body)?;
                let rooted = is_heap_pointer(body_ty);
                if rooted {
                    self.push_rooted(Reg::X0);
                } else {
                    self.asm.push(Reg::X0);
                }
                self.statement(cleanup)?;
                if rooted {
                    self.pop_rooted(Reg::X0);
                } else {
                    self.asm.pop(Reg::X0);
                }
                Ok(body_ty)
            }
            Expr::Double { value, .. } => {
                // The double travels as its raw IEEE 754 bits in x0.
                self.asm.mov_imm64(Reg::X0, value.to_bits());
                Ok(ValueType::Double)
            }
            Expr::Bool { value, .. } => {
                self.asm.mov_imm64(Reg::X0, u64::from(*value));
                Ok(ValueType::Bool)
            }
            Expr::String { value, span } => {
                if value.contains("#{") {
                    return Err(unsupported(*span, "string interpolation"));
                }
                let offset = self.asm.intern_string_object(value);
                self.asm.load_rodata_address(Reg::X0, offset);
                // Materialise the literal as a real heap object (see
                // emit_heap_string_copy) so `Str` is always traceable.
                self.emit_heap_string_copy();
                Ok(ValueType::Str)
            }
            // `#{...}` holes: fold parts left to right through the
            // existing `emit_str_concat`, converting each hole's value
            // to `Str` first (M12, issue #538). Aarch64 strings are
            // already exact-size heap objects, so unlike the x86_64
            // backend's fixed-buffer path there's no capacity to
            // track -- every intermediate concat result is a fresh,
            // correctly-sized allocation.
            Expr::StringInterpolation { parts, .. } => {
                let mut iter = parts.iter();
                match iter.next() {
                    None => {
                        let offset = self.asm.intern_string_object("");
                        self.asm.load_rodata_address(Reg::X0, offset);
                        self.emit_heap_string_copy();
                    }
                    Some(first) => self.emit_string_part(first)?,
                }
                for part in iter {
                    // The accumulated prefix waits while the next part is
                    // built (which allocates), so root its stack slot.
                    self.push_rooted(Reg::X0);
                    self.emit_string_part(part)?;
                    self.asm.mov_reg(Reg::X1, Reg::X0);
                    self.pop_rooted(Reg::X0);
                    self.emit_str_concat();
                }
                Ok(ValueType::Str)
            }
            Expr::Identifier { name, span } => {
                // A nullary constructor is a value, not a call: `None`.
                if self.lookup(name).is_none()
                    && let Some((enum_index, tag, 0)) = self.enum_variant(name)
                {
                    return self.emit_enum_value(enum_index, tag, &[], *span);
                }
                let Some((offset, ty)) = self.lookup(name) else {
                    return Err(unsupported(*span, &format!("identifier `{name}`")));
                };
                self.asm.load_local(Reg::X0, offset);
                Ok(ty)
            }
            Expr::Binary { lhs, op, rhs, span } => self.binary(lhs, *op, rhs, *span),
            // `null` is the null pointer. Typed as a heap reference so a
            // binding holding it is rooted like any other -- the root scan and
            // both barriers already treat 0 as "nothing here", so a null slot
            // costs nothing and needs no special case.
            Expr::Null { .. } => {
                self.asm.mov_imm64(Reg::X0, 0);
                Ok(ValueType::Null)
            }
            Expr::Unary { op, expr, span } => {
                let ty = self.expression(expr)?;
                match (op, ty) {
                    (UnaryOp::Plus, ValueType::Int | ValueType::Double) => Ok(ty),
                    (UnaryOp::Minus, ValueType::Double) => {
                        self.asm.fmov_d_from_x(0, Reg::X0);
                        self.asm.fneg_d(0, 0);
                        self.asm.fmov_x_from_d(Reg::X0, 0);
                        Ok(ValueType::Double)
                    }
                    (UnaryOp::Minus, ValueType::Int) => {
                        // 0 - value, which is also how the evaluator negates.
                        self.asm.mov_reg(Reg::X1, Reg::X0);
                        self.asm.mov_imm64(Reg::X0, 0);
                        self.asm.sub_reg(Reg::X0, Reg::X0, Reg::X1);
                        Ok(ValueType::Int)
                    }
                    (UnaryOp::Not, ValueType::Bool) => {
                        // Bools are 0/1, so flipping the low bit negates.
                        self.asm.cmp_imm(Reg::X0, 0);
                        self.asm.cset(Reg::X0, Cond::Eq);
                        Ok(ValueType::Bool)
                    }
                    _ => Err(unsupported(*span, "this unary operator")),
                }
            }
            Expr::ListLiteral { elements, span } => {
                // Build the cons chain back to front: all elements go
                // onto the machine stack first, then each pop becomes
                // the head of a fresh cell whose tail is in x0.
                let mut elem = None;
                for element in elements {
                    let ty = self.expression(element)?;
                    let Some(this_elem) = list_elem_of(ty) else {
                        return Err(unsupported(element.span(), "a list element of this type"));
                    };
                    if *elem.get_or_insert(this_elem) != this_elem {
                        return Err(unsupported(*span, "mixed list element types"));
                    }
                    // Box a scalar element so the cons cell's head slot is
                    // a pointer (string/list/record heads already are).
                    if is_boxed_scalar(ty) {
                        self.emit_box_scalar();
                    }
                    self.push_rooted(Reg::X0);
                }
                self.asm.mov_imm64(Reg::X0, 0); // nil
                for _ in elements {
                    self.emit_cons_cell();
                }
                Ok(match elem {
                    Some(elem) => ValueType::List(elem),
                    None => ValueType::EmptyList,
                })
            }
            Expr::SetLiteral { elements, span } => self.set_literal(elements, *span),
            Expr::Match {
                scrutinee,
                arms,
                span,
            } => self.emit_enum_match(scrutinee, arms, *span),
            Expr::MapLiteral { entries, span } => self.map_literal(entries, *span),
            // Nominal construction `#Point(3, 4)`.
            Expr::RecordConstructor {
                name,
                arguments,
                span,
            } => {
                // A declaration whose layout depends on its arguments always
                // goes through the inference below, even after one
                // instantiation has been interned under the same name --
                // otherwise `#Pair(true, 1)` would be checked against whatever
                // `#Pair(1.5, 2.5)` made of it.
                let inferred = self
                    .generic_records
                    .iter()
                    .any(|record| record.name == *name && record.fields.len() == arguments.len());
                let concrete = if inferred {
                    None
                } else {
                    self.records
                        .iter()
                        .position(|record| record.usable && record.name == *name)
                };
                let Some(index) = concrete else {
                    // A generic record's constructor pins its parameters down
                    // by what it is given: the arguments' own types become the
                    // instantiation's field types.
                    if let Some(GenericRecord {
                        fields: declared, ..
                    }) = self
                        .generic_records
                        .iter()
                        .find(|record| {
                            record.name == *name && record.fields.len() == arguments.len()
                        })
                        .cloned()
                    {
                        let mut typed = Vec::with_capacity(arguments.len());
                        for (argument, (field, _)) in arguments.iter().zip(declared.iter()) {
                            let ty = self.expression(argument)?;
                            if ty == ValueType::Unit || ty == ValueType::Never {
                                return Err(unsupported(
                                    argument.span(),
                                    "a record field of this type",
                                ));
                            }
                            typed.push((field.clone(), ty));
                            if is_boxed_scalar(ty) {
                                self.emit_box_scalar();
                            }
                            self.push_rooted(Reg::X0);
                        }
                        let count = arguments.len();
                        let index = match self.records.iter().position(|record| {
                            record.usable && record.name == *name && record.fields == typed
                        }) {
                            Some(index) => index,
                            None => {
                                self.records.push(RecordInfo {
                                    name: name.clone(),
                                    fields: typed,
                                    lambda_fields: Vec::new(),
                                    usable: true,
                                });
                                self.records.len() - 1
                            }
                        };
                        self.emit_record_object(count);
                        return Ok(ValueType::Record(index as u32));
                    }
                    // `#Point(x, y)` is built in: the evaluator and the type
                    // checker both know it without a declaration, so this
                    // backend has to as well.
                    if name == "Point" && arguments.len() == 2 {
                        let point = self.instantiate_builtin_point();
                        for argument in arguments {
                            if self.expression(argument)? != ValueType::Int {
                                return Err(unsupported(
                                    argument.span(),
                                    "a Point field that is not an Int",
                                ));
                            }
                            self.emit_box_scalar();
                            self.push_rooted(Reg::X0);
                        }
                        self.emit_record_object(2);
                        return Ok(ValueType::Record(point));
                    }
                    return Err(unsupported(*span, &format!("record `{name}`")));
                };
                let fields = self.records[index].fields.clone();
                let lambda_fields = self.records[index].lambda_fields.clone();
                let declared = fields.len() + lambda_fields.len();
                if declared != arguments.len() {
                    return Err(Diagnostic::compile(
                        *span,
                        format!(
                            "{name} expects {declared} fields but got {}",
                            arguments.len()
                        ),
                    ));
                }
                // The function-typed fields are not part of the object; they
                // are resolved from the binding that constructed it, so only
                // the stored fields are evaluated here -- in declaration order,
                // with the compile-time ones skipped.
                let stored = arguments
                    .iter()
                    .enumerate()
                    .filter(|(position, _)| !lambda_fields.iter().any(|(_, at)| at == position))
                    .map(|(_, argument)| argument);
                for (argument, (_, expected)) in stored.zip(fields.iter()) {
                    let ty = self.expression(argument)?;
                    if !assignable(ty, *expected) {
                        return Err(unsupported(argument.span(), "a record field of this type"));
                    }
                    if is_boxed_scalar(ty) {
                        self.emit_box_scalar();
                    }
                    self.push_rooted(Reg::X0);
                }
                self.emit_record_object(fields.len());
                Ok(ValueType::Record(index as u32))
            }
            // Structural literal `record { x: 1; y: 2 }`: the shape
            // is interned so equal shapes share one type.
            Expr::RecordLiteral { fields, .. } => {
                let mut typed = Vec::with_capacity(fields.len());
                for (field_name, value) in fields {
                    let ty = self.expression(value)?;
                    if ty == ValueType::Unit || ty == ValueType::Never {
                        return Err(unsupported(value.span(), "a record field of this type"));
                    }
                    typed.push((field_name.clone(), ty));
                    if is_boxed_scalar(ty) {
                        self.emit_box_scalar();
                    }
                    self.push_rooted(Reg::X0);
                }
                let count = fields.len();
                let index = self.intern_structural_record(typed);
                self.emit_record_object(count);
                Ok(ValueType::Record(index))
            }
            Expr::FieldAccess {
                target,
                field,
                span,
            } => {
                let target_ty = self.expression(target)?;
                let ValueType::Record(index) = target_ty else {
                    return Err(unsupported(*span, "field access on this value"));
                };
                let info = &self.records[index as usize];
                let Some(position) = info
                    .fields
                    .iter()
                    .position(|(field_name, _)| field_name == field)
                else {
                    return Err(unsupported(*span, &format!("field `{field}`")));
                };
                let ty = info.fields[position].1;
                // Every record slot is a heap pointer (M6), so the read goes
                // through the load barrier; the scalar inside its box is
                // then a plain unbarriered load.
                self.emit_gc_load_ptr(Reg::X0, (position * 8) as u32);
                if is_boxed_scalar(ty) {
                    self.emit_unbox_scalar();
                }
                Ok(ty)
            }
            Expr::Call {
                callee,
                arguments,
                span,
            } => {
                // Curried builtins: the callee is itself a call, so the
                // whole thing reads `f(a)(b)`. The language has no general
                // first-class application in this backend yet, but several
                // builtins are *defined* in curried form, and the x86-64
                // backend recognises them the same way -- by matching the
                // shape rather than by building closures.
                if arguments.len() == 1
                    && let Expr::Call {
                        callee: inner,
                        arguments: first_args,
                        ..
                    } = callee.as_ref()
                    && first_args.len() == 1
                    && let Expr::Identifier { name, .. } = inner.as_ref()
                    && name == "assertResult"
                {
                    return self.emit_assert_result(&first_args[0], &arguments[0], *span);
                }
                // `map(list, f)` -- the same operation as the curried form,
                // spelled the way the evaluator's own builtin takes it.
                if arguments.len() == 2
                    && let Expr::Identifier { name, .. } = callee.as_ref()
                    && name == "map"
                    // The builtin's shape is `map(list, f)`. A *function* first
                    // means this is something else called `map` -- a `Functor`
                    // instance's own `map(f, xs)`, say -- so leave it to the
                    // function resolution below.
                    && self.lambda_argument(&arguments[0]).is_none()
                    && let Some((params, body)) = self.lambda_argument(&arguments[1])
                    && params.len() == 1
                {
                    return self.emit_list_map(&arguments[0], &params[0], &body, *span);
                }
                // Curried `foldLeft(list)(init)(f)` -- which is also what
                // `xs reduce init => r + e` desugars to -- with a lambda
                // literal for f.
                if arguments.len() == 1
                    && let Expr::Call {
                        callee: with_initial,
                        arguments: initial_args,
                        ..
                    } = callee.as_ref()
                    && initial_args.len() == 1
                    && let Expr::Call {
                        callee: inner,
                        arguments: list_args,
                        ..
                    } = with_initial.as_ref()
                    && list_args.len() == 1
                    && let Expr::Identifier { name, .. } = inner.as_ref()
                    && name == "foldLeft"
                    && let Some((params, body)) = self.lambda_argument(&arguments[0])
                    && params.len() == 2
                {
                    return self.emit_list_fold_left(
                        &list_args[0],
                        &initial_args[0],
                        &params[0],
                        &params[1],
                        &body,
                        *span,
                    );
                }
                // Curried `map(list)(f)`: the list's element type gives the
                // lambda's parameter type, so the body can be compiled straight
                // into the loop. The lambda is either written here or passed in
                // under a name, which is how a higher-order def hands its own
                // function parameter on.
                if arguments.len() == 1
                    && let Expr::Call {
                        callee: inner,
                        arguments: list_args,
                        ..
                    } = callee.as_ref()
                    && list_args.len() == 1
                    && let Expr::Identifier { name, .. } = inner.as_ref()
                    && name == "map"
                    && let Some((params, body)) = self.lambda_argument(&arguments[0])
                    && params.len() == 1
                {
                    return self.emit_list_map(&list_args[0], &params[0], &body, *span);
                }
                // Curried `cons(head)(tail)` — the evaluator's list
                // prepend builtin.
                if arguments.len() == 1
                    && let Expr::Call {
                        callee: inner,
                        arguments: head_args,
                        ..
                    } = callee.as_ref()
                    && head_args.len() == 1
                    && matches!(inner.as_ref(), Expr::Identifier { name, .. } if name == "cons")
                {
                    let head_ty = self.expression(&head_args[0])?;
                    let Some(elem) = list_elem_of(head_ty) else {
                        return Err(unsupported(
                            head_args[0].span(),
                            "a list element of this type",
                        ));
                    };
                    // Box a scalar head so the cons cell holds a pointer.
                    if is_boxed_scalar(head_ty) {
                        self.emit_box_scalar();
                    }
                    self.push_rooted(Reg::X0);
                    let tail_ty = self.expression(&arguments[0])?;
                    if !assignable(tail_ty, ValueType::List(elem)) {
                        return Err(unsupported(arguments[0].span(), "consing onto this value"));
                    }
                    self.emit_cons_cell();
                    return Ok(ValueType::List(elem));
                }
                // A generic enum's constructor: `Some(v)`.
                if let Expr::Identifier { name, .. } = callee.as_ref()
                    && let Some((enum_index, tag, arity)) = self.enum_variant(name)
                {
                    if arity != arguments.len() {
                        return Err(Diagnostic::compile(
                            *span,
                            format!(
                                "{name} carries {arity} field(s) but got {}",
                                arguments.len()
                            ),
                        ));
                    }
                    return self.emit_enum_value(enum_index, tag, arguments, *span);
                }
                // Method-style builtin call `target.method(args)`:
                // dispatch as `method(target, args)`, the same way the
                // evaluator and C backend resolve value methods.
                // `dict.show(x)` on a dictionary-bound name: the field names a
                // lambda, so the call is that lambda inlined here.
                if let Expr::FieldAccess { target, field, .. } = callee.as_ref()
                    && let Expr::Identifier { name, .. } = target.as_ref()
                    && let Some(mapping) = self.lookup_dict(name)
                {
                    let Some((_, index)) = mapping.iter().find(|(f, _)| f == field) else {
                        return Err(unsupported(
                            *span,
                            &format!("field `{field}` on this dictionary"),
                        ));
                    };
                    let index = *index;
                    // A function-typed *record* field is called as a method:
                    // `s.to_string()` passes the record itself as the `this`
                    // the lambda declares. A dictionary's field takes only
                    // what the call site writes, so the receiver is prepended
                    // exactly when the arities say it is expected.
                    if self.lambdas[index].0.len() == arguments.len() + 1 {
                        let mut with_receiver = Vec::with_capacity(arguments.len() + 1);
                        with_receiver.push((**target).clone());
                        with_receiver.extend(arguments.iter().cloned());
                        return self.emit_inline_lambda_call(index, &with_receiver, *span);
                    }
                    return self.emit_inline_lambda_call(index, arguments, *span);
                }
                if let Expr::FieldAccess { target, field, .. } = callee.as_ref() {
                    let mut all = Vec::with_capacity(arguments.len() + 1);
                    all.push((**target).clone());
                    all.extend(arguments.iter().cloned());
                    if let Some(ty) = self.builtin_call(field, &all, *span)? {
                        return Ok(ty);
                    }
                    // A method that is not a builtin is a function taking the
                    // receiver first -- which is what an extension method on
                    // an enum is left as when its name is also a builtin's
                    // (`Result`'s `getOrElse` and a map's are both spelled
                    // that way, with different arities).
                    if self.function_exists(field, all.len()) {
                        return self.function_call(field, &all, *span);
                    }
                    // An extension method declared on more than one type is
                    // left as a method call by the shared desugar, because a
                    // name alone does not say which extension is meant. The
                    // receiver's type does: `Result`'s `getOrElse` and
                    // `Option`'s are two functions, and this picks between
                    // them the way the checker does.
                    if let Some(key) = self.extension_type_key_of(target) {
                        let mangled = format!("__ext_{key}_{field}");
                        if self.function_exists(&mangled, all.len()) {
                            return self.function_call(&mangled, &all, *span);
                        }
                    }
                    return Err(unsupported(*span, &format!("method `{field}`")));
                }
                // `myFoldLeft(list)(z)(f)`: a def whose body is a chain of
                // lambdas, applied all the way down.
                if let Some(result) = self.curried_generic_call(callee, arguments, *span) {
                    return result;
                }
                // An immediately applied lambda literal, `((x) => x + 1)(3)`.
                if let Expr::Lambda { params, body, .. } = callee.as_ref() {
                    let index = self.intern_lambda(params.clone(), body.as_ref().clone());
                    return self.emit_inline_lambda_call(index, arguments, *span);
                }
                let Expr::Identifier { name, .. } = callee.as_ref() else {
                    return Err(unsupported(*span, "calling a non-identifier"));
                };
                // A name bound to a lambda shadows the function table, the
                // same way a local shadows a top-level definition.
                if let Some(index) = self.lookup_lambda(name) {
                    return self.emit_inline_lambda_call(index, arguments, *span);
                }
                // An alias stands for whatever name it was bound to.
                if let Some(target) = self.lookup_alias(name) {
                    let callee = Expr::Identifier {
                        name: target,
                        span: *span,
                    };
                    return self.expression(&Expr::Call {
                        callee: Box::new(callee),
                        arguments: arguments.clone(),
                        span: *span,
                    });
                }
                // `__enum_shape_named(value, "Variant")` and
                // `__enum_shape_hint(value, id)` are shape aids the shared
                // enum-lowering pass wraps around values for the x86_64
                // display/dispatch paths. The aarch64 backend has no use
                // for the shape, so it compiles the wrapped value
                // transparently and drops the marker.
                if (name == "__enum_shape_named" || name == "__enum_shape_hint")
                    && let Some(value) = arguments.first()
                {
                    return self.expression(value);
                }
                // Builtins first, mirroring the C backend's dispatch.
                if let Some(ty) = self.builtin_call(name, arguments, *span)? {
                    return Ok(ty);
                }
                self.function_call(name, arguments, *span)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let Some(else_branch) = else_branch else {
                    return Err(unsupported(*span, "if without else as an expression"));
                };
                let condition_ty = self.expression(condition)?;
                if condition_ty != ValueType::Bool {
                    return Err(unsupported(condition.span(), "a non-Bool condition"));
                }
                let else_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm
                    .branch(else_label, BranchKind::CompareZero(Reg::X0));
                // Whether this join is a nullable has to be decided *before*
                // either branch is compiled: a branch yielding a bare scalar
                // boxes on its way out so the join is one word either way, and
                // once its code is emitted there is no going back. So both
                // branches are predicted here, and the decision is checked
                // against what they actually turn out to be below.
                let predicted = match (
                    self.static_type_under(then_branch, &mut Vec::new(), 0),
                    self.static_type_under(else_branch, &mut Vec::new(), 0),
                ) {
                    (Some(then_ty), Some(else_ty)) => self.merge_types(then_ty, else_ty),
                    _ => None,
                };
                let nullable_join = matches!(predicted, Some(ValueType::Nullable(_)));
                let then_ty = self.expression(then_branch)?;
                let boxed_then = nullable_join && is_boxed_scalar(then_ty);
                if boxed_then {
                    self.emit_box_scalar();
                }
                self.asm.branch(end_label, BranchKind::Unconditional);
                self.asm.bind(else_label);
                let else_ty = self.expression(else_branch)?;
                let boxed_else = nullable_join && is_boxed_scalar(else_ty);
                if boxed_else {
                    self.emit_box_scalar();
                }
                self.asm.bind(end_label);
                let merged = self
                    .merge_types(then_ty, else_ty)
                    .ok_or_else(|| unsupported(*span, "if branches with different types"))?;
                // The prediction has to have been right about the boxing, or
                // the value and its type disagree: a nullable holding an
                // unboxed scalar gets dereferenced as a pointer at the far end,
                // and an unboxed type holding a box reads the box's address as
                // the value. Neither is something to compile, so where the
                // inference could not see the join coming, say so.
                let boxing_matches = if matches!(merged, ValueType::Nullable(_)) {
                    (!is_boxed_scalar(then_ty) || boxed_then)
                        && (!is_boxed_scalar(else_ty) || boxed_else)
                } else {
                    !boxed_then && !boxed_else
                };
                if !boxing_matches {
                    return Err(unsupported(
                        *span,
                        "an if whose branches join as a nullable the inference could not predict",
                    ));
                }
                Ok(merged)
            }
            // A block in expression position: statements, then the
            // value of the final expression (the enum lowering leans
            // on this shape heavily).
            Expr::Block { expressions, .. } => {
                let Some((last, init)) = expressions.split_last() else {
                    self.asm.mov_imm64(Reg::X0, 0);
                    return Ok(ValueType::Unit);
                };
                self.push_binding_scopes();
                self.scope_root_counts.push(0);
                for expression in init {
                    self.statement(expression)?;
                }
                let ty = self.expression(last)?;
                self.pop_binding_scopes();
                // The block's value is already in x0 and nothing allocates
                // between here and its use, so dropping the block's roots
                // now is safe (and the slots themselves are dead).
                let roots = self.scope_root_counts.pop().expect("emitter root scope");
                self.emit_shadow_pop(roots);
                Ok(ty)
            }
            other => Err(unsupported(other.span(), "this expression")),
        }
    }

    /// One `#{...}` interpolation part, leaving a `Str` value in x0.
    /// A literal segment interns directly; a hole is evaluated and
    /// converted to `Str` (`Int`/`Bool` supported for now — a nested
    /// interpolation or enum/record hole is deferred, matching the
    /// x86_64 backend's own incremental history).
    fn emit_string_part(&mut self, part: &StringPart) -> Result<(), Diagnostic> {
        match part {
            StringPart::Literal(text) => {
                let offset = self.asm.intern_string_object(text);
                self.asm.load_rodata_address(Reg::X0, offset);
                self.emit_heap_string_copy();
                Ok(())
            }
            StringPart::Interpolation(hole) => {
                if let Some(number) = self.static_double_of(hole) {
                    let offset = self.asm.intern_string_object(&format_double(number));
                    self.asm.load_rodata_address(Reg::X0, offset);
                    self.emit_heap_string_copy();
                    return Ok(());
                }
                match self.expression(hole)? {
                    ValueType::Str => {}
                    ValueType::Int => self.emit_int_to_str(),
                    ValueType::Bool => self.emit_bool_to_str(),
                    // A collection renders into the hole the way `println`
                    // renders it, through the same builder.
                    ValueType::List(elem) => {
                        self.emit_list_to_str(elem, "[", "]", hole.span())?;
                    }
                    ValueType::Set(elem) => {
                        self.emit_list_to_str(elem, "%(", ")", hole.span())?;
                    }
                    ValueType::EmptyList => {
                        let offset = self.asm.intern_string_object("[]");
                        self.asm.load_rodata_address(Reg::X0, offset);
                        self.emit_heap_string_copy();
                    }
                    ValueType::EmptySet => {
                        let offset = self.asm.intern_string_object("%()");
                        self.asm.load_rodata_address(Reg::X0, offset);
                        self.emit_heap_string_copy();
                    }
                    ValueType::EmptyMap => {
                        let offset = self.asm.intern_string_object("%[]");
                        self.asm.load_rodata_address(Reg::X0, offset);
                        self.emit_heap_string_copy();
                    }
                    ValueType::Enum(shape) => {
                        self.emit_enum_to_str(shape, hole.span())?;
                    }
                    other => {
                        return Err(unsupported(
                            hole.span(),
                            &format!("string interpolation of {other:?}"),
                        ));
                    }
                }
                Ok(())
            }
        }
    }

    /// `FileOutput#write`/`FileOutput#append`(path, content): `path`
    /// is a compile-time string literal already interned as a
    /// NUL-terminated rodata blob at `path_offset`; the content
    /// string object (`[len][bytes]`) is expected in x0. Opens with
    /// `O_WRONLY|O_CREAT|` (`O_TRUNC` or `O_APPEND`), writes the
    /// content bytes, then closes -- aborting with a source-located
    /// message on any syscall failure (M14, issue #538).
    fn emit_file_write(&mut self, append: bool) {
        let flags = if append {
            O_WRONLY | O_CREAT | O_APPEND
        } else {
            O_WRONLY | O_CREAT | O_TRUNC
        };
        self.asm.mov_imm64(Reg::X1, flags);
        self.asm.mov_imm64(Reg::X2, DEFAULT_FILE_MODE);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_OPEN));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: FileOutput#write failed to open file\n");
        self.asm.mov_reg(Reg::X3, Reg::X0); // fd
        self.pop_rooted(Reg::X4); // content object

        self.asm.ldr_imm(Reg::X2, Reg::X4, 0); // content len
        self.asm.add_reg_imm(Reg::X1, Reg::X4, 8); // content bytes
        self.asm.mov_reg(Reg::X0, Reg::X3); // fd
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: FileOutput#write failed to write file\n");

        self.asm.mov_reg(Reg::X0, Reg::X3); // fd
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_CLOSE));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: FileOutput#write failed to close file\n");

        self.asm.mov_imm64(Reg::X0, 0); // Unit
    }

    /// `FileInput#all`(path): opens the NUL-terminated path (rodata,
    /// `path_offset`) with `O_RDONLY`, reads up to `READ_CAP` bytes
    /// into a scratch heap buffer, closes, then copies exactly the
    /// bytes actually read into a fresh, exact-size string object
    /// (M14, issue #538). Every value that must survive an
    /// `emit_alloc` call is pushed/popped explicitly rather than kept
    /// resident in a register, since `emit_alloc` unconditionally
    /// clobbers x6 and only preserves x0-x5 across its heap-grow
    /// path -- the same lesson #563's bug taught for `trim`.
    fn emit_file_read_all(&mut self) {
        self.asm.mov_imm64(Reg::X1, O_RDONLY);
        self.asm.mov_imm64(Reg::X2, 0);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_OPEN));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: FileInput#all failed to open file\n");
        self.emit_read_fd_to_string();
    }

    /// Read everything left on the descriptor in x0 into a fresh string
    /// object, left in x0, and close it. Standard input is a descriptor
    /// nobody opened, which is why this is separate from opening a file.
    fn emit_read_fd_to_string(&mut self) {
        const READ_CAP: u64 = 1_048_576;

        self.asm.push(Reg::X0); // fd

        // A megabyte is too big for the machine stack, so the read buffer is
        // a real GC object (a header + RAW_BYTES payload the sweep can walk)
        // shaped like a string: length slot at [x5], bytes from [x5 + 8].
        self.asm.mov_imm64(Reg::X2, READ_CAP);
        self.emit_alloc_raw_string(Reg::X2); // x5 = buffer
        self.asm.pop(Reg::X3); // fd

        self.asm.mov_reg(Reg::X0, Reg::X3);
        self.asm.add_reg_imm(Reg::X1, Reg::X5, 8);
        self.asm.mov_imm64(Reg::X2, READ_CAP);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_READ));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: FileInput#all failed to read file\n");
        // x0 = bytes actually read
        self.asm.push(Reg::X0); // bytes_read
        self.asm.push(Reg::X5); // scratch buffer ptr
        self.asm.push(Reg::X3); // fd

        self.asm.mov_reg(Reg::X0, Reg::X3);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_CLOSE));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: FileInput#all failed to close file\n");

        self.asm.pop(Reg::X3); // fd, discarded
        self.asm.pop(Reg::X5); // scratch buffer ptr
        self.asm.pop(Reg::X2); // bytes_read (content length)

        // The buffer is a heap object waiting across the result's
        // allocation, so its stack slot is a root for that window.
        self.push_rooted(Reg::X5); // read buffer
        self.asm.push(Reg::X2); // bytes_read
        self.emit_alloc_raw_string(Reg::X2); // x5 = result object
        self.asm.pop(Reg::X2); // bytes_read
        self.pop_rooted(Reg::X6); // read buffer

        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8);
        self.asm.add_reg_imm(Reg::X6, Reg::X6, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X6, Reg::X7, Reg::X3);
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// `FileOutput#delete`(path): unlinks the NUL-terminated path
    /// (rodata, `path_offset`). Tolerates a missing file (`ENOENT`,
    /// errno 2) as success, matching the evaluator's
    /// `std::io::ErrorKind::NotFound` leniency; any other failure
    /// aborts with a source-located message (M14, issue #538).
    fn emit_file_delete(&mut self) {
        const ENOENT: u32 = 2;
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_UNLINK));
        self.asm.svc_0x80();
        let ok = self.asm.new_label();
        self.asm.branch(ok, BranchKind::Conditional(Cond::Cc));
        self.asm.cmp_imm(Reg::X0, ENOENT);
        self.asm.branch(ok, BranchKind::Conditional(Cond::Eq));
        self.asm
            .emit_write_rodata(STDERR_FD, b"klassic: FileOutput#delete failed\n");
        self.asm.emit_exit(1);
        self.asm.bind(ok);
        self.asm.mov_imm64(Reg::X0, 0); // Unit
    }

    /// `Dir#mkdir`(path): mkdir syscall on the NUL-terminated path
    /// (rodata, `path_offset`) with mode `0o755`. Aborts with a
    /// source-located message on any failure, matching the
    /// evaluator's `fs::create_dir` (which errors on an
    /// already-existing path) (M15, issue #538).
    fn emit_dir_mkdir(&mut self) {
        self.asm.mov_imm64(Reg::X1, DEFAULT_DIR_MODE);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_MKDIR));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: Dir#mkdir failed\n");
        self.asm.mov_imm64(Reg::X0, 0); // Unit
    }

    /// `Dir#mkdirs` per-prefix step: mkdir syscall on the
    /// NUL-terminated path (rodata, `path_offset`), tolerating
    /// `EEXIST` (errno 17) as success -- an intermediate or final
    /// directory may already exist, matching the evaluator's
    /// `fs::create_dir_all` leniency. Aborts with a source-located
    /// message on any other failure. Does not set a return value;
    /// callers run this once per `/`-separated prefix and set the
    /// `Unit` result after the last one (M15, issue #538).
    fn emit_dir_mkdir_tolerating_eexist(&mut self) {
        const EEXIST: u32 = 17;
        self.asm.mov_imm64(Reg::X1, DEFAULT_DIR_MODE);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_MKDIR));
        self.asm.svc_0x80();
        let ok = self.asm.new_label();
        self.asm.branch(ok, BranchKind::Conditional(Cond::Cc));
        self.asm.cmp_imm(Reg::X0, EEXIST);
        self.asm.branch(ok, BranchKind::Conditional(Cond::Eq));
        self.asm
            .emit_write_rodata(STDERR_FD, b"klassic: Dir#mkdirs failed\n");
        self.asm.emit_exit(1);
        self.asm.bind(ok);
    }

    /// `Dir#delete`(path): rmdir syscall on the NUL-terminated path
    /// (rodata, `path_offset`). Aborts with a source-located message
    /// on any failure, matching the evaluator's `fs::remove_dir`
    /// (errors on a non-empty or missing directory) (M15, issue
    /// #538).
    fn emit_dir_delete(&mut self) {
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_RMDIR));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: Dir#delete failed\n");
        self.asm.mov_imm64(Reg::X0, 0); // Unit
    }

    /// `Dir#isDirectory`(path): `fstatat64(AT_FDCWD, path, &buf, 0)`
    /// then tests whether `buf.st_mode`'s file-type bits equal
    /// `S_IFDIR`. Darwin's `stat64` places `st_mode` as a 2-byte
    /// halfword at offset 4 (not 8 bytes at offset 24 like Linux); the
    /// 144-byte buffer is a fresh bump-heap allocation per call (no
    /// GC exists yet on this backend to worry about tracing raw stat
    /// bytes as pointers -- directory checks are not hot-path enough
    /// to justify a reusable scratch slot). Shifts `st_mode` right by
    /// 12 bits to compare only the file-type nibble against `S_IFDIR
    /// >> 12` rather than emitting an `AND`-immediate (Darwin's
    /// `S_IFMT` bitmask encoding is annoying to construct; a value
    /// with a clean high nibble and zero low bits is bit-identical to
    /// its own top bits after this shift). A failed stat (e.g. a
    /// missing path) reports `false` rather than aborting, matching
    /// the evaluator's `Path::is_dir()` (M15, issue #538).
    fn emit_dir_is_directory(&mut self) {
        const STAT_BUF_SIZE: u64 = 144;
        const S_IFDIR_SHIFTED: u32 = 0o4; // S_IFDIR (0o040000) >> 12

        self.asm.mov_reg(Reg::X7, Reg::X0); // the path, past the buffer setup

        // A transient syscall buffer, so it lives on the machine stack, not
        // in the GC heap: the collector's sweep walks a region block by
        // block through object headers, and a raw header-less block would
        // desynchronise that walk.
        self.asm.sub_sp_imm(STAT_BUF_SIZE as u32);
        self.asm.add_reg_sp_imm(Reg::X6, 0);

        self.asm.mov_imm64(Reg::X0, AT_FDCWD as u64);
        self.asm.mov_reg(Reg::X1, Reg::X7);
        self.asm.mov_reg(Reg::X2, Reg::X6);
        self.asm.mov_imm64(Reg::X3, 0);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_FSTATAT64));
        self.asm.svc_0x80();

        let success = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.branch(success, BranchKind::Conditional(Cond::Cc));
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.branch(done, BranchKind::Unconditional);
        self.asm.bind(success);
        self.asm.ldrh_imm(Reg::X0, Reg::X6, 4);
        self.asm.lsr_imm(Reg::X0, Reg::X0, 12);
        self.asm.cmp_imm(Reg::X0, S_IFDIR_SHIFTED);
        self.asm.cset(Reg::X0, Cond::Eq);
        self.asm.bind(done);
        self.asm.add_sp_imm(STAT_BUF_SIZE as u32);
    }

    /// `Dir#move`(source, target): rename syscall on the two
    /// NUL-terminated paths (rodata, `source_offset`/`target_offset`).
    /// Aborts with a source-located message on any failure, matching
    /// the evaluator's `fs::rename` (M15, issue #538).
    fn emit_dir_move(&mut self) {
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_RENAME));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: Dir#move failed\n");
        self.asm.mov_imm64(Reg::X0, 0); // Unit
    }

    /// `Environment#exists`(key): walks the NUL-terminated `envp`
    /// array (x23, captured from dyld's `LC_MAIN` entry) looking for
    /// an entry whose `"KEY="` prefix matches `key`. Each `envp`
    /// entry is a NUL-terminated C string `"KEY=VALUE"`; a match
    /// means the first `key.len()` bytes equal `key` and the very
    /// next byte is `'='` (so `"FOO"` doesn't spuriously match an
    /// entry for `"FOOBAR"`), matching the evaluator's
    /// `env::var_os(key).is_some()` (M16, issue #538).
    /// Find the environment entry whose name is the `x10` bytes at `x9`,
    /// leaving a pointer to the byte after its `=` in x0, or 0 when there is
    /// no such variable.
    ///
    /// The key comes in as a pointer and a length rather than as text, so a
    /// name computed at run time -- `env_or(name)` reading its own parameter --
    /// searches the same way a literal one does.
    fn emit_environment_lookup(&mut self) {
        self.asm.mov_reg(Reg::X6, Reg::X23); // envp cursor
        self.asm.mov_reg(Reg::X4, Reg::X9); // key bytes, kept for each entry
        self.asm.mov_reg(Reg::X5, Reg::X10); // key length
        let loop_start = self.asm.new_label();
        let try_match = self.asm.new_label();
        let check_equals = self.asm.new_label();
        let advance_entry = self.asm.new_label();
        let found = self.asm.new_label();
        let not_found = self.asm.new_label();
        let done = self.asm.new_label();

        self.asm.bind(loop_start);
        self.asm.ldr_imm(Reg::X7, Reg::X6, 0); // entry ptr
        self.asm.branch(not_found, BranchKind::CompareZero(Reg::X7));
        self.asm.mov_reg(Reg::X8, Reg::X7); // entry byte cursor
        self.asm.mov_reg(Reg::X9, Reg::X4); // key byte cursor
        self.asm.mov_reg(Reg::X10, Reg::X5); // remaining length

        self.asm.bind(try_match);
        self.asm
            .branch(check_equals, BranchKind::CompareZero(Reg::X10));
        self.asm.ldrb_post_increment(Reg::X11, Reg::X8);
        self.asm.ldrb_post_increment(Reg::X12, Reg::X9);
        self.asm.cmp_reg(Reg::X11, Reg::X12);
        self.asm
            .branch(advance_entry, BranchKind::Conditional(Cond::Ne));
        self.asm.sub_reg_imm(Reg::X10, Reg::X10, 1);
        self.asm.branch(try_match, BranchKind::Unconditional);

        self.asm.bind(check_equals);
        self.asm.ldrb(Reg::X11, Reg::X8); // peek, no advance
        self.asm.cmp_imm(Reg::X11, u32::from(b'='));
        self.asm.branch(found, BranchKind::Conditional(Cond::Eq));

        self.asm.bind(advance_entry);
        self.asm.add_reg_imm(Reg::X6, Reg::X6, 8); // next envp slot
        self.asm.branch(loop_start, BranchKind::Unconditional);

        self.asm.bind(found);
        // x8 points at the `=`; the value starts after it.
        self.asm.add_reg_imm(Reg::X0, Reg::X8, 1);
        self.asm.branch(done, BranchKind::Unconditional);
        self.asm.bind(not_found);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.bind(done);
    }

    /// Leave the key of an `Environment#*` call in x9 (bytes) and x10 (length),
    /// whether it is written at the call site or computed.
    fn emit_environment_key(&mut self, key: &Expr) -> Result<(), Diagnostic> {
        if let Expr::String { value, span } = key {
            if value.contains("#{") {
                return Err(unsupported(*span, "string interpolation"));
            }
            let offset = self.asm.intern_rodata(value.as_bytes());
            self.asm.load_rodata_address(Reg::X9, offset);
            self.asm.mov_imm64(Reg::X10, value.len() as u64);
            return Ok(());
        }
        if self.expression(key)? != ValueType::Str {
            return Err(unsupported(
                key.span(),
                "an environment variable name that is not a string",
            ));
        }
        self.asm.ldr_imm(Reg::X10, Reg::X0, 0); // length
        self.asm.add_reg_imm(Reg::X9, Reg::X0, 8); // bytes
        Ok(())
    }

    /// `Time#nowMillis`(): `gettimeofday(&buf, NULL, NULL)` (all
    /// three arguments -- see the `SYS_GETTIMEOFDAY` doc comment for
    /// why the third one, previously omitted, is believed to be the
    /// actual cause of this builtin's prior real-hardware crash) into
    /// a fresh 16-byte bump-heap buffer laid out as Darwin's actual
    /// `struct timeval` (`tv_sec: i64` at offset 0, `tv_usec: i32` at
    /// offset 8 followed by 4 bytes of padding -- read with a 32-bit
    /// load so the result never depends on those padding bytes being
    /// zero), then computes `tv_sec*1000 + tv_usec/1000` (M16, issue
    /// #538 / #570).
    fn emit_time_now_millis(&mut self) {
        // Transient syscall scratch: the machine stack, not the GC heap
        // (see emit_dir_is_directory).
        self.asm.sub_sp_imm(16);
        self.asm.add_reg_sp_imm(Reg::X6, 0);

        self.asm.mov_reg(Reg::X0, Reg::X6);
        self.asm.mov_imm64(Reg::X1, 0); // tzp = NULL
        self.asm.mov_imm64(Reg::X2, 0); // mach_absolute_time = NULL
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_GETTIMEOFDAY));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic: Time#nowMillis failed\n");

        self.asm.ldr_imm(Reg::X0, Reg::X6, 0); // tv_sec
        self.asm.ldr_imm32(Reg::X1, Reg::X6, 8); // tv_usec (32-bit field)
        self.asm.mov_imm64(Reg::X2, 1000);
        self.asm.mul_reg(Reg::X0, Reg::X0, Reg::X2); // tv_sec * 1000
        self.asm.sdiv_reg(Reg::X1, Reg::X1, Reg::X2); // tv_usec / 1000
        self.asm.add_reg(Reg::X0, Reg::X0, Reg::X1);
        self.asm.add_sp_imm(16);
    }

    fn binary(
        &mut self,
        lhs: &Expr,
        op: BinaryOp,
        rhs: &Expr,
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        // Short-circuit logic first: rhs must not evaluate eagerly.
        if matches!(op, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) {
            let lhs_ty = self.expression(lhs)?;
            let end_label = self.asm.new_label();
            match op {
                BinaryOp::LogicalAnd => {
                    self.asm.branch(end_label, BranchKind::CompareZero(Reg::X0))
                }
                _ => self
                    .asm
                    .branch(end_label, BranchKind::CompareNonZero(Reg::X0)),
            }
            let rhs_ty = self.expression(rhs)?;
            self.asm.bind(end_label);
            if lhs_ty != ValueType::Bool || rhs_ty != ValueType::Bool {
                return Err(unsupported(span, "logical operator on non-Bool operands"));
            }
            return Ok(ValueType::Bool);
        }

        let lhs_ty = self.expression(lhs)?;
        // A heap-reference left operand (a string being concatenated, a
        // collection being compared) waits while the right operand is
        // evaluated, which can allocate -- root its slot for that window.
        let root_lhs = is_heap_pointer(lhs_ty);
        if root_lhs {
            self.push_rooted(Reg::X0);
        } else {
            self.asm.push(Reg::X0);
        }
        let mut rhs_ty = self.expression(rhs)?;
        // `"n=" + 42` and `"ok=" + true`: appending to a string converts the
        // right operand, the way the evaluator and the x86-64 backend do. The
        // conversion allocates, so it happens while the left operand is still
        // parked as a root, before it is taken back.
        if lhs_ty == ValueType::Str && op == BinaryOp::Add && rhs_ty != ValueType::Str {
            match rhs_ty {
                ValueType::Int => {
                    self.emit_int_to_str();
                    rhs_ty = ValueType::Str;
                }
                ValueType::Bool => {
                    self.emit_bool_to_str();
                    rhs_ty = ValueType::Str;
                }
                ValueType::List(elem) => {
                    self.emit_list_to_str(elem, "[", "]", rhs.span())?;
                    rhs_ty = ValueType::Str;
                }
                ValueType::Set(elem) => {
                    self.emit_list_to_str(elem, "%(", ")", rhs.span())?;
                    rhs_ty = ValueType::Str;
                }
                ValueType::EmptyList => {
                    let offset = self.asm.intern_string_object("[]");
                    self.asm.load_rodata_address(Reg::X0, offset);
                    self.emit_heap_string_copy();
                    rhs_ty = ValueType::Str;
                }
                ValueType::EmptySet => {
                    let offset = self.asm.intern_string_object("%()");
                    self.asm.load_rodata_address(Reg::X0, offset);
                    self.emit_heap_string_copy();
                    rhs_ty = ValueType::Str;
                }
                // A value that may not be there appends as the word `null`
                // when it is not, and as itself when it is -- which is what
                // reading a map entry's value into a message does.
                ValueType::Nullable(elem) => {
                    let null_label = self.asm.new_label();
                    let end_label = self.asm.new_label();
                    self.asm
                        .branch(null_label, BranchKind::CompareZero(Reg::X0));
                    if is_boxed_scalar(elem_value_type(elem)) {
                        self.emit_unbox_scalar();
                    }
                    match elem {
                        ListElem::Int => self.emit_int_to_str(),
                        ListElem::Bool => self.emit_bool_to_str(),
                        ListElem::Str => self.emit_heap_string_copy(),
                        _ => {
                            return Err(unsupported(rhs.span(), "appending a value of this type"));
                        }
                    }
                    self.asm.branch(end_label, BranchKind::Unconditional);
                    self.asm.bind(null_label);
                    let offset = self.asm.intern_string_object("null");
                    self.asm.load_rodata_address(Reg::X0, offset);
                    self.emit_heap_string_copy();
                    self.asm.bind(end_label);
                    rhs_ty = ValueType::Str;
                }
                _ => return Err(unsupported(rhs.span(), "appending a value of this type")),
            }
        }
        self.asm.mov_reg(Reg::X1, Reg::X0);
        if root_lhs {
            self.pop_rooted(Reg::X0);
        } else {
            self.asm.pop(Reg::X0);
        }
        if lhs_ty != rhs_ty {
            return Err(unsupported(span, "mixed operand types"));
        }
        if lhs_ty == ValueType::Str {
            return match op {
                BinaryOp::Add => {
                    self.emit_str_concat();
                    Ok(ValueType::Str)
                }
                BinaryOp::Equal => {
                    self.emit_str_eq();
                    Ok(ValueType::Bool)
                }
                BinaryOp::NotEqual => {
                    self.emit_str_eq();
                    self.asm.cmp_imm(Reg::X0, 0);
                    self.asm.cset(Reg::X0, Cond::Eq);
                    Ok(ValueType::Bool)
                }
                _ => Err(unsupported(span, "this string operator")),
            };
        }
        if lhs_ty == ValueType::Double {
            // Both operands are raw double bits in x0 / x1; move them
            // into d0 / d1 for the FP unit. IEEE division by zero is
            // defined (it yields an infinity), so no zero guard.
            self.asm.fmov_d_from_x(0, Reg::X0);
            self.asm.fmov_d_from_x(1, Reg::X1);
            return match op {
                BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                    match op {
                        BinaryOp::Add => self.asm.fadd_d(0, 0, 1),
                        BinaryOp::Subtract => self.asm.fsub_d(0, 0, 1),
                        BinaryOp::Multiply => self.asm.fmul_d(0, 0, 1),
                        _ => self.asm.fdiv_d(0, 0, 1),
                    }
                    self.asm.fmov_x_from_d(Reg::X0, 0);
                    Ok(ValueType::Double)
                }
                BinaryOp::Less
                | BinaryOp::LessEqual
                | BinaryOp::Greater
                | BinaryOp::GreaterEqual
                | BinaryOp::Equal
                | BinaryOp::NotEqual => {
                    let cond = match op {
                        BinaryOp::Less => Cond::Mi,
                        BinaryOp::LessEqual => Cond::Ls,
                        BinaryOp::Greater => Cond::Gt,
                        BinaryOp::GreaterEqual => Cond::Ge,
                        BinaryOp::Equal => Cond::Eq,
                        _ => Cond::Ne,
                    };
                    self.asm.fcmp_d(0, 1);
                    self.asm.cset(Reg::X0, cond);
                    Ok(ValueType::Bool)
                }
                _ => Err(unsupported(span, "this double operator")),
            };
        }
        match op {
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                if lhs_ty != ValueType::Int {
                    return Err(unsupported(span, "arithmetic on non-Int operands"));
                }
                match op {
                    BinaryOp::Add => self.asm.add_reg(Reg::X0, Reg::X0, Reg::X1),
                    BinaryOp::Subtract => self.asm.sub_reg(Reg::X0, Reg::X0, Reg::X1),
                    BinaryOp::Multiply => self.asm.mul_reg(Reg::X0, Reg::X0, Reg::X1),
                    _ => {
                        // Match the evaluator: division by zero is a
                        // runtime error, not an arm64 zero result.
                        let ok_label = self.asm.new_label();
                        self.asm
                            .branch(ok_label, BranchKind::CompareNonZero(Reg::X1));
                        self.asm
                            .emit_write_rodata(STDERR_FD, b"klassic: division by zero\n");
                        self.asm.emit_exit(1);
                        self.asm.bind(ok_label);
                        self.asm.sdiv_reg(Reg::X0, Reg::X0, Reg::X1);
                    }
                }
                Ok(ValueType::Int)
            }
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                if lhs_ty != ValueType::Int {
                    return Err(unsupported(span, "comparison on non-Int operands"));
                }
                let cond = match op {
                    BinaryOp::Less => Cond::Lt,
                    BinaryOp::LessEqual => Cond::Le,
                    BinaryOp::Greater => Cond::Gt,
                    _ => Cond::Ge,
                };
                self.asm.cmp_reg(Reg::X0, Reg::X1);
                self.asm.cset(Reg::X0, cond);
                Ok(ValueType::Bool)
            }
            BinaryOp::Equal | BinaryOp::NotEqual => {
                // Two collections are equal when their *contents* are, which
                // is what the evaluator says and what `assertResult([1 2 3])(xs)`
                // in the sample programs relies on. Comparing the pointers
                // would call two separately built lists different.
                if let ValueType::List(elem) | ValueType::Set(elem) = lhs_ty {
                    self.emit_list_eq(elem, span)?;
                    if op == BinaryOp::NotEqual {
                        self.asm.cmp_imm(Reg::X0, 0);
                        self.asm.cset(Reg::X0, Cond::Eq);
                    }
                    return Ok(ValueType::Bool);
                }
                let cond = if op == BinaryOp::Equal {
                    Cond::Eq
                } else {
                    Cond::Ne
                };
                self.asm.cmp_reg(Reg::X0, Reg::X1);
                self.asm.cset(Reg::X0, cond);
                Ok(ValueType::Bool)
            }
            _ => Err(unsupported(span, "this binary operator")),
        }
    }

    /// `assertResult(expected)(actual)`: the test-program assertion. Equal
    /// values yield Unit; unequal ones abort with the evaluator's message
    /// prefix on stderr, which is what the sample programs rely on to fail
    /// loudly rather than print a wrong answer.
    ///
    /// The comparison reuses the ordinary `==` path, so it inherits its type
    /// checking and its per-type semantics (byte-wise for strings, value for
    /// scalars) instead of duplicating them.
    fn emit_assert_result(
        &mut self,
        expected: &Expr,
        actual: &Expr,
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        let ty = self.binary(expected, BinaryOp::Equal, actual, span)?;
        if ty != ValueType::Bool {
            return Err(unsupported(span, "assertResult on values of this type"));
        }
        let ok = self.asm.new_label();
        self.asm.branch(ok, BranchKind::CompareNonZero(Reg::X0));
        self.asm
            .emit_write_rodata(STDERR_FD, b"klassic: assertResult failed\n");
        self.asm.emit_exit(1);
        self.asm.bind(ok);
        Ok(ValueType::Unit)
    }

    fn gc_alloc_entry_label(&mut self) -> Label {
        match self.gc_alloc_label {
            Some(label) => label,
            None => {
                let label = self.asm.new_label();
                self.gc_alloc_label = Some(label);
                label
            }
        }
    }

    /// M7 (go-live): call the collector's `gc_alloc` -- payload size in x5,
    /// type tag in x4, user pointer (block + 16) back in x0.
    ///
    /// This replaces the old bump allocator, so an allocation is now a
    /// *collection point*: it can mark, sweep and (from M9) relocate. Two
    /// consequences shape every caller. The portable routine works in
    /// x0-x9 plus the x10 immediate scratch, so this saves x1-x3 and x6-x12
    /// around it (x4/x5 are the arguments, x0 the result) -- callers keep
    /// their scalars and interior pointers. And a heap *reference* the
    /// caller still needs must be rooted across the call, because being in
    /// a register (or on the invisible machine stack) is not reachability:
    /// callers park those with `push_rooted` and take them back afterwards,
    /// which also picks up the new address once the collector moves objects.
    fn emit_gc_alloc_call(&mut self) {
        const SAVED: [Reg; 10] = [
            Reg::X1,
            Reg::X2,
            Reg::X3,
            Reg::X6,
            Reg::X7,
            Reg::X8,
            Reg::X9,
            Reg::X10,
            Reg::X11,
            Reg::X12,
        ];
        let entry = self.gc_alloc_entry_label();
        self.asm.push_frame_record(); // the bl clobbers x30
        for reg in SAVED {
            self.asm.push(reg);
        }
        self.asm.branch(entry, BranchKind::Link);
        for reg in SAVED.into_iter().rev() {
            self.asm.pop(reg);
        }
        self.asm.pop_frame_record();
    }

    /// Allocate a GC-shaped heap object: a 16-byte header
    /// `[size|mark][type_tag]` followed by `words` 8-byte payload slots.
    /// `gc_alloc` writes the header and hands back the *user* pointer
    /// (block + 16) in x0, so every `[x0 + off]` payload access is
    /// unchanged. The tag distinguishes RAW_BYTES (no inner pointers) from
    /// POINTER_RECORD (every payload slot a heap pointer), so the collector
    /// traces only the latter.
    fn emit_gc_alloc_object(&mut self, words: usize, tag: u64) {
        self.asm.mov_imm64(Reg::X5, (words * 8) as u64); // payload bytes
        self.asm.mov_imm64(Reg::X4, tag);
        self.emit_gc_alloc_call(); // x0 = user pointer
    }

    /// Box a scalar (in x0) into its own single-slot `RAW_BYTES` object,
    /// leaving the box's user pointer in x0. Every heap slot of a
    /// `POINTER_RECORD` must be a pointer, so scalar record fields and
    /// list elements are boxed on write and unboxed on read (the uniform
    /// representation the shared enum lowering already uses).
    fn emit_box_scalar(&mut self) {
        self.asm.push(Reg::X0); // preserve the scalar across the alloc
        self.emit_gc_alloc_object(1, crate::gc_layout::GC_TYPE_RAW_BYTES);
        self.asm.pop(Reg::X1);
        self.asm.str_imm(Reg::X1, Reg::X0, 0); // [box] = scalar
    }

    /// Unbox a scalar: load it from the single-slot box whose user
    /// pointer is in x0.
    fn emit_unbox_scalar(&mut self) {
        self.asm.ldr_imm(Reg::X0, Reg::X0, 0);
    }

    /// M7: colour a heap reference on its way into a heap slot --
    /// `dst = src == 0 ? 0 : src | GoodColor`.
    ///
    /// The write half of the mutator contract, and the dual of the load
    /// barrier. Stored references carry the cycle's good colour, so when
    /// MarkStart flips the colours every reference already in the heap
    /// becomes "bad" and the next load of it takes the slow path, which is
    /// how the collector observes -- and marks -- what the mutator reaches.
    /// A raw, uncoloured pointer would always look good and never be
    /// marked, so this is what makes the barrier work at all.
    ///
    /// Null stays raw zero: `0 | colour` would be a bogus pointer, and the
    /// root scan and barriers all treat 0 as "nothing here".
    fn emit_color_ptr(&mut self, dst: Reg, src: Reg) {
        let raw = self.asm.new_label();
        if dst != src {
            self.asm.mov_reg(dst, src);
        }
        self.asm.branch(raw, BranchKind::CompareZero(dst));
        self.asm.orr_reg(dst, dst, Reg::X25); // | good colour
        self.asm.bind(raw);
    }

    /// The two shadow-stack cells, reserved on first use.
    fn shadow_cells(&mut self) -> (DataLabel, DataLabel) {
        match self.shadow_stack_cells {
            Some(cells) => cells,
            None => {
                let cells = (
                    self.asm.reserve_data_cells(1),
                    self.asm.reserve_data_cells(1),
                );
                self.shadow_stack_cells = Some(cells);
                cells
            }
        }
    }

    fn shadow_push_routine_label(&mut self) -> Label {
        match self.shadow_push_label {
            Some(label) => label,
            None => {
                let label = self.asm.new_label();
                self.shadow_push_label = Some(label);
                label
            }
        }
    }

    fn shadow_pop_routine_label(&mut self) -> Label {
        match self.shadow_pop_label {
            Some(label) => label,
            None => {
                let label = self.asm.new_label();
                self.shadow_pop_label = Some(label);
                label
            }
        }
    }

    /// M7: root the stack slot whose address is `[x29 + offset]`'s home --
    /// i.e. make the collector treat that frame slot as a live reference.
    /// The shadow stack holds slot *addresses*, so a moving collector can
    /// rewrite the slot in place and the mutator observes the new address.
    ///
    /// Only x0 and x30 are touched at the call site (x0 is restored), so
    /// this is safe to emit anywhere regardless of what is live.
    /// Note where the current scope's roots stand, so temporaries rooted from
    /// here can be dropped again by `close_temp_roots`.
    ///
    /// A helper called *inside* an expression has to leave the shadow stack
    /// exactly as it found it. The stack is strictly last-in-first-out and its
    /// entries are slot *addresses*, so a frame-slot root left behind is popped
    /// by whatever `pop_rooted` comes next -- which then leaves that machine
    /// stack slot rooted after its value is gone, and the next collection reads
    /// whatever has since been pushed there as a heap reference.
    fn open_temp_roots(&self) -> usize {
        *self.scope_root_counts.last().expect("emitter root scope")
    }

    /// Drop every root taken since `mark`. Preserves x0, so a result being
    /// returned survives.
    fn close_temp_roots(&mut self, mark: usize) {
        let count = self.open_temp_roots();
        debug_assert!(count >= mark);
        self.emit_shadow_pop(count - mark);
        *self
            .scope_root_counts
            .last_mut()
            .expect("emitter root scope") = mark;
    }

    fn emit_root_frame_slot(&mut self, offset: u32) {
        let push = self.shadow_push_routine_label();
        self.asm.push(Reg::X0); // x0 is the argument register below
        self.asm.push_frame_record(); // the bl clobbers x30
        self.asm.add_reg_imm(Reg::X0, Reg::X29, offset);
        self.asm.branch(push, BranchKind::Link);
        self.asm.pop_frame_record();
        self.asm.pop(Reg::X0);
        *self
            .scope_root_counts
            .last_mut()
            .expect("emitter root scope") += 1;
    }

    /// M9: root the stack slot at `base + offset`, which must hold a heap
    /// reference (or null).
    ///
    /// This is what makes a scratch struct relocation-proof. A routine that
    /// caches an interior pointer (a string's payload base) across an
    /// allocation would be left holding a dangling address once the
    /// collector starts moving objects. Storing the *object* in a rooted
    /// slot instead lets the collector rewrite it in place, so every
    /// interior pointer can be re-derived from the slot and is always
    /// current.
    fn emit_root_address(&mut self, base: Reg, offset: u32) {
        let push = self.shadow_push_routine_label();
        self.asm.push(Reg::X0); // the argument register below
        self.asm.push_frame_record(); // the bl clobbers x30
        self.asm.add_reg_imm(Reg::X0, base, offset);
        self.asm.branch(push, BranchKind::Link);
        self.asm.pop_frame_record();
        self.asm.pop(Reg::X0);
    }

    /// Push `reg` onto the machine stack *and* root that stack slot, for a
    /// heap pointer that must stay live while a later subexpression
    /// allocates. The machine stack is invisible to the collector, so a
    /// plain `push` would leave the value unreachable at a collection --
    /// this makes the temporary a precise root for exactly its lifetime.
    /// Pair with `pop_rooted`. Preserves every register.
    fn push_rooted(&mut self, reg: Reg) {
        let push = self.shadow_push_routine_label();
        self.asm.push(reg); // [sp] = the value, and the slot to root
        self.asm.push_frame_record(); // the bl clobbers x30
        self.asm.push(Reg::X0); // x0 is the argument register below
        self.asm.add_reg_sp_imm(Reg::X0, 32); // &value slot
        self.asm.branch(push, BranchKind::Link);
        self.asm.pop(Reg::X0);
        self.asm.pop_frame_record();
    }

    /// Pop a `push_rooted` temporary into `reg`, dropping its root.
    fn pop_rooted(&mut self, reg: Reg) {
        self.emit_shadow_pop(1);
        self.asm.pop(reg);
    }

    /// Drop `count` shadow-stack roots. Preserves every register.
    fn emit_shadow_pop(&mut self, count: usize) {
        if count == 0 {
            return;
        }
        let pop = self.shadow_pop_routine_label();
        self.asm.push_frame_record(); // the bl clobbers x30
        for _ in 0..count {
            self.asm.branch(pop, BranchKind::Link);
        }
        self.asm.pop_frame_record();
    }

    /// `bl`-called: root the slot whose address is in x0. Preserves every
    /// register (x1-x3 are saved). Overflowing the shadow stack is fatal --
    /// the same diagnostic the portable routines use for their tables.
    fn emit_shadow_push_routine(&mut self, label: Label) {
        let (base, top) = self.shadow_cells();
        self.asm.bind(label);
        self.asm.push(Reg::X1);
        self.asm.push(Reg::X2);
        self.asm.push(Reg::X3);
        self.asm.load_data_address(Reg::X2, top);
        self.asm.ldr_imm(Reg::X3, Reg::X2, 0); // top
        let ok = self.asm.new_label();
        self.asm
            .mov_imm64(Reg::X1, crate::gc_layout::GC_SHADOW_STACK_LEN as u64);
        self.asm.cmp_reg(Reg::X3, Reg::X1);
        self.asm.branch(ok, BranchKind::Conditional(Cond::Lt));
        self.asm
            .emit_write_rodata(STDERR_FD, b"klassic gc: shadow stack overflow\n");
        self.asm.emit_exit(1);
        self.asm.bind(ok);
        self.asm.load_data_address(Reg::X1, base);
        self.asm.ldr_imm(Reg::X1, Reg::X1, 0); // shadow stack base
        self.asm.lsl_imm(Reg::X3, Reg::X3, 3); // top * 8
        self.asm.add_reg(Reg::X1, Reg::X1, Reg::X3);
        self.asm.str_imm(Reg::X0, Reg::X1, 0); // base[top] = slot address
        self.asm.ldr_imm(Reg::X3, Reg::X2, 0);
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.str_imm(Reg::X3, Reg::X2, 0); // top += 1
        self.asm.pop(Reg::X3);
        self.asm.pop(Reg::X2);
        self.asm.pop(Reg::X1);
        self.asm.ret();
    }

    /// `bl`-called: drop one shadow-stack root. Preserves every register.
    ///
    /// The underflow check is a deliberate self-test of the root
    /// bookkeeping: pushes and pops must balance on every path, and an
    /// imbalance is otherwise silent (a leaked root, or -- worse -- a
    /// negative top that makes the next push write outside the table).
    /// Aborting here turns any mismatch into an immediate, obvious CI
    /// failure on arm64 instead of a rare corruption once the collector is
    /// live.
    fn emit_shadow_pop_routine(&mut self, label: Label) {
        let (_, top) = self.shadow_cells();
        self.asm.bind(label);
        self.asm.push(Reg::X0);
        self.asm.push(Reg::X1);
        self.asm.load_data_address(Reg::X0, top);
        self.asm.ldr_imm(Reg::X1, Reg::X0, 0);
        let ok = self.asm.new_label();
        self.asm.branch(ok, BranchKind::CompareNonZero(Reg::X1));
        self.asm
            .emit_write_rodata(STDERR_FD, b"klassic gc: shadow stack underflow\n");
        self.asm.emit_exit(1);
        self.asm.bind(ok);
        self.asm.sub_reg_imm(Reg::X1, Reg::X1, 1);
        self.asm.str_imm(Reg::X1, Reg::X0, 0);
        self.asm.pop(Reg::X1);
        self.asm.pop(Reg::X0);
        self.asm.ret();
    }

    /// M7: copy the string object in x0 onto the GC heap, leaving the copy
    /// in x0.
    ///
    /// String *literals* live in the read-only `__TEXT,__const` image, so
    /// they have no object header: the collector cannot size them, and
    /// marking one would try to write a mark bit into read-only memory.
    /// Every literal is therefore materialised as a heap copy, which makes
    /// `Str` mean "heap string" everywhere and keeps every reference the
    /// mutator can store or root a real, traceable object. (The x86-64
    /// backend gets the same guarantee by distinguishing StaticString from
    /// HeapString in its value model and copying at the boundary.)
    ///
    /// The source is in the immovable image, so it is parked on the machine
    /// stack *without* a root -- rooting a non-heap pointer would send the
    /// collector into `__const` looking for a header.
    /// A heap string holding the NUL-terminated bytes at the pointer in x0.
    /// The length is measured first, since a C string does not carry one.
    fn emit_heap_string_from_cstring(&mut self) {
        // x9 = the bytes; x2 = length, counted to the terminator.
        self.asm.mov_reg(Reg::X9, Reg::X0);
        self.asm.mov_imm64(Reg::X2, 0);
        let measure = self.asm.new_label();
        let measured = self.asm.new_label();
        self.asm.bind(measure);
        self.asm.ldrb_reg_offset(Reg::X11, Reg::X9, Reg::X2);
        self.asm.branch(measured, BranchKind::CompareZero(Reg::X11));
        self.asm.add_reg_imm(Reg::X2, Reg::X2, 1);
        self.asm.branch(measure, BranchKind::Unconditional);
        self.asm.bind(measured);
        // The source is outside the heap (it is argv), so it cannot move
        // during the allocation and needs no root.
        self.asm.push(Reg::X9);
        self.asm.push(Reg::X2);
        self.emit_alloc_raw_string(Reg::X2); // x5 = object, x2 preserved
        self.asm.pop(Reg::X2);
        self.asm.pop(Reg::X6);
        self.asm.str_imm(Reg::X2, Reg::X5, 0); // length
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8); // destination bytes
        self.emit_copy_bytes(Reg::X2, Reg::X6, Reg::X7, Reg::X3);
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// A NUL-terminated copy of the string object in x0, left as a byte pointer
    /// in x0: the syscalls take C strings, and a Klassic string is
    /// length-prefixed without a terminator. Used for a path that is only known
    /// at run time -- a literal one interns into rodata already terminated.
    fn emit_cstring_copy(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0); // length
        self.push_rooted(Reg::X0); // the source survives the allocation
        self.asm.add_reg_imm(Reg::X2, Reg::X2, 1); // room for the terminator
        self.emit_alloc_raw_string(Reg::X2); // x5 = buffer, x2 preserved
        self.pop_rooted(Reg::X6); // source
        self.asm.str_imm(Reg::X2, Reg::X5, 0); // the buffer's own length
        self.asm.sub_reg_imm(Reg::X2, Reg::X2, 1); // bytes to copy
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8); // dst payload
        self.asm.add_reg_imm(Reg::X6, Reg::X6, 8); // src payload
        // The copy advances x7 past the last byte, which is where the
        // terminator goes.
        self.emit_copy_bytes(Reg::X2, Reg::X6, Reg::X7, Reg::X3);
        self.asm.mov_imm64(Reg::X3, 0);
        self.asm.strb_post_increment(Reg::X3, Reg::X7);
        self.asm.add_reg_imm(Reg::X0, Reg::X5, 8);
    }

    /// Leave a C string for `path` in x0, whether it is a literal or computed.
    fn emit_path_cstring(&mut self, path: &Expr) -> Result<(), Diagnostic> {
        if let Expr::String { value, span } = path {
            if value.contains("#{") {
                return Err(unsupported(*span, "string interpolation"));
            }
            let offset = self.asm.intern_nul_terminated(value);
            self.asm.load_rodata_address(Reg::X0, offset);
            return Ok(());
        }
        if self.expression(path)? != ValueType::Str {
            return Err(unsupported(path.span(), "a path that is not a string"));
        }
        self.emit_cstring_copy();
        Ok(())
    }

    fn emit_heap_string_copy(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0); // length
        self.asm.push(Reg::X0); // source: rodata never moves
        self.emit_alloc_raw_string(Reg::X2); // x5 = copy, x2 preserved
        self.asm.pop(Reg::X6); // source
        self.asm.str_imm(Reg::X2, Reg::X5, 0); // copy's length
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8); // dst payload
        self.asm.add_reg_imm(Reg::X6, Reg::X6, 8); // src payload
        self.emit_copy_bytes(Reg::X2, Reg::X6, Reg::X7, Reg::X3);
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    fn load_barrier_label(&mut self) -> Label {
        match self.gc_load_barrier_label {
            Some(label) => label,
            None => {
                let label = self.asm.new_label();
                self.gc_load_barrier_label = Some(label);
                label
            }
        }
    }

    /// M7: load a heap *pointer* out of `[base + offset]` through the GC
    /// load barrier, leaving the raw (colour-stripped) pointer in x0.
    ///
    /// This is the read half of the collector's mutator contract, and it is
    /// what makes incremental marking sound: a load of a
    /// not-currently-good-coloured reference takes the slow path, which
    /// follows forwarding, self-heals the field, and -- during Mark -- marks
    /// the loaded object. Without it a mutator could move the last reference
    /// to an object from an untraced slot into an already-traced one and
    /// hide it from the marker.
    ///
    /// Fast path (4 instructions):
    /// ```text
    ///   add x8, base, #offset   ; field address, also the slow-path arg
    ///   ldr x0, [x8]            ; the colour-tagged value
    ///   tst x0, x26             ; bad colour? (BadMask)
    ///   b.eq fast               ; good / raw / null -> no slow path
    ///   ...slow...
    /// fast:
    ///   and x0, x0, x24         ; strip the colour (ColorStrip)
    /// ```
    /// Until go-live nothing colours a stored pointer, so `tst` always sees
    /// zero and `and` changes nothing: the barrier is a semantic no-op that
    /// can land and be CI-validated ahead of the flip.
    ///
    /// Contract: result in x0; clobbers x0 and x8 only. The rare slow path
    /// saves x29/x30 (it makes a `bl`) plus every caller-saved register the
    /// portable routine may touch, so leaf helpers can barrier without
    /// building a frame.
    fn emit_gc_load_ptr(&mut self, base: Reg, offset: u32) {
        self.asm.add_reg_imm(Reg::X8, base, offset);
        self.emit_gc_load_barriered();
    }

    /// `emit_gc_load_ptr` for a *dynamic* offset: the field address is
    /// `base + offset` with both in registers.
    fn emit_gc_load_ptr_reg_offset(&mut self, base: Reg, offset: Reg) {
        self.asm.add_reg(Reg::X8, base, offset);
        self.emit_gc_load_barriered();
    }

    /// The barrier proper: x8 = field address on entry, x0 = the raw
    /// pointer on exit.
    fn emit_gc_load_barriered(&mut self) {
        const SAVED: [Reg; 11] = [
            Reg::X1,
            Reg::X2,
            Reg::X3,
            Reg::X4,
            Reg::X5,
            Reg::X6,
            Reg::X7,
            Reg::X9,
            Reg::X10,
            Reg::X11,
            Reg::X12,
        ];
        self.asm.ldr_imm(Reg::X0, Reg::X8, 0);
        let fast = self.asm.new_label();
        self.asm.tst_reg(Reg::X0, Reg::X26);
        self.asm.branch(fast, BranchKind::Conditional(Cond::Eq));
        let slow = self.load_barrier_label();
        self.asm.push_frame_record(); // the bl below clobbers x30
        for reg in SAVED {
            self.asm.push(reg);
        }
        self.asm.branch(slow, BranchKind::Link); // x0 = value, x8 = field
        for reg in SAVED.into_iter().rev() {
            self.asm.pop(reg);
        }
        self.asm.pop_frame_record();
        self.asm.bind(fast);
        self.asm.and_reg(Reg::X0, Reg::X0, Reg::X24);
    }

    /// Allocate a GC heap string whose payload is `[len][bytes...]`, with
    /// the character count in `len` (any register other than x4/x5). The
    /// object gets the usual 16-byte header from `gc_alloc` and the *user*
    /// pointer lands in x5, so callers keep writing the length at
    /// `[x5 + 0]` and the bytes from `[x5 + 8]`. A string has no inner
    /// pointers, hence RAW_BYTES.
    ///
    /// `len` survives the call unless it is x0 (the result register the
    /// allocator returns in). Callers holding a heap reference across this
    /// must root it -- allocating can now collect.
    fn emit_alloc_raw_string(&mut self, len: Reg) {
        // payload = align8(len + 8): the 8-byte length field plus the bytes.
        self.asm.add_reg_imm(Reg::X5, len, 15);
        self.asm.lsr_imm(Reg::X5, Reg::X5, 3);
        self.asm.lsl_imm(Reg::X5, Reg::X5, 3);
        self.asm
            .mov_imm64(Reg::X4, crate::gc_layout::GC_TYPE_RAW_BYTES);
        self.emit_gc_alloc_call(); // x0 = user pointer
        self.asm.mov_reg(Reg::X5, Reg::X0); // the documented result register
    }

    /// Copy `[count]` bytes between the byte pointers in `src`/`dst`;
    /// `count` reaches zero, `src`/`dst` advance, `scratch` clobbered.
    fn emit_copy_bytes(&mut self, count: Reg, src: Reg, dst: Reg, scratch: Reg) {
        let copy_loop = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.bind(copy_loop);
        self.asm.branch(done, BranchKind::CompareZero(count));
        self.asm.ldrb_post_increment(scratch, src);
        self.asm.strb_post_increment(scratch, dst);
        self.asm.sub_reg_imm(count, count, 1);
        self.asm.branch(copy_loop, BranchKind::Unconditional);
        self.asm.bind(done);
    }

    /// String concatenation: a in x0, b in x1 → fresh heap object in
    /// x0. Layout is `[len][a bytes][b bytes]`.
    fn emit_str_concat(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.ldr_imm(Reg::X3, Reg::X1, 0);
        self.asm.add_reg(Reg::X2, Reg::X2, Reg::X3);
        // Allocating can collect, so both operands are parked as roots and
        // taken back (with their post-collection addresses) afterwards.
        self.push_rooted(Reg::X0);
        self.push_rooted(Reg::X1);
        self.emit_alloc_raw_string(Reg::X2);
        self.pop_rooted(Reg::X1);
        self.pop_rooted(Reg::X0);
        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8);
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.add_reg_imm(Reg::X6, Reg::X0, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X6, Reg::X7, Reg::X3);
        self.asm.ldr_imm(Reg::X2, Reg::X1, 0);
        self.asm.add_reg_imm(Reg::X6, Reg::X1, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X6, Reg::X7, Reg::X3);
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// `startsWith(s, prefix)`: s in x0, prefix in x1 -> Bool in x0.
    fn emit_str_starts_with(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.ldr_imm(Reg::X3, Reg::X1, 0);
        let fail = self.asm.new_label();
        let ok = self.asm.new_label();
        let end = self.asm.new_label();
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm.branch(fail, BranchKind::Conditional(Cond::Gt));
        self.asm.add_reg_imm(Reg::X6, Reg::X0, 8);
        self.asm.add_reg_imm(Reg::X7, Reg::X1, 8);
        let byte_loop = self.asm.new_label();
        self.asm.bind(byte_loop);
        self.asm.branch(ok, BranchKind::CompareZero(Reg::X3));
        self.asm.ldrb_post_increment(Reg::X4, Reg::X6);
        self.asm.ldrb_post_increment(Reg::X5, Reg::X7);
        self.asm.cmp_reg(Reg::X4, Reg::X5);
        self.asm.branch(fail, BranchKind::Conditional(Cond::Ne));
        self.asm.sub_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.branch(byte_loop, BranchKind::Unconditional);
        self.asm.bind(ok);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.branch(end, BranchKind::Unconditional);
        self.asm.bind(fail);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.bind(end);
    }

    /// `indexOf(s, needle)` / `lastIndexOf(s, needle)`: s in x0, needle in x1
    /// -> the *byte* offset in x0, or -1. Byte rather than character, because
    /// the evaluator answers with what Rust's `find`/`rfind` answer with.
    /// Scanning forward and keeping the last match is `rfind`.
    fn emit_str_index_of(&mut self, last: bool) {
        let not_found = self.asm.new_label();
        let done = self.asm.new_label();
        let end = self.asm.new_label();
        self.asm.ldr_imm(Reg::X3, Reg::X0, 0); // haystack length
        self.asm.ldr_imm(Reg::X5, Reg::X1, 0); // needle length
        self.asm.add_reg_imm(Reg::X2, Reg::X0, 8); // haystack bytes
        self.asm.add_reg_imm(Reg::X4, Reg::X1, 8); // needle bytes
        self.asm.cmp_reg(Reg::X5, Reg::X3);
        self.asm
            .branch(not_found, BranchKind::Conditional(Cond::Gt));
        self.asm.mov_imm64(Reg::X6, 0); // start offset
        self.asm.mov_imm64(Reg::X7, u64::MAX); // best so far = -1
        let outer = self.asm.new_label();
        let inner = self.asm.new_label();
        let matched = self.asm.new_label();
        let advance = self.asm.new_label();
        self.asm.bind(outer);
        // Stop once fewer bytes are left than the needle needs.
        self.asm.sub_reg(Reg::X11, Reg::X3, Reg::X6);
        self.asm.cmp_reg(Reg::X11, Reg::X5);
        self.asm.branch(done, BranchKind::Conditional(Cond::Lt));
        self.asm.add_reg(Reg::X10, Reg::X2, Reg::X6); // haystack cursor
        self.asm.mov_reg(Reg::X11, Reg::X4); // needle cursor
        self.asm.mov_reg(Reg::X8, Reg::X5); // bytes left to compare
        self.asm.bind(inner);
        self.asm.branch(matched, BranchKind::CompareZero(Reg::X8));
        self.asm.ldrb_post_increment(Reg::X9, Reg::X10);
        self.asm.ldrb_post_increment(Reg::X12, Reg::X11);
        self.asm.cmp_reg(Reg::X9, Reg::X12);
        self.asm.branch(advance, BranchKind::Conditional(Cond::Ne));
        self.asm.sub_reg_imm(Reg::X8, Reg::X8, 1);
        self.asm.branch(inner, BranchKind::Unconditional);
        self.asm.bind(matched);
        if last {
            self.asm.mov_reg(Reg::X7, Reg::X6);
        } else {
            self.asm.mov_reg(Reg::X0, Reg::X6);
            self.asm.branch(end, BranchKind::Unconditional);
        }
        self.asm.bind(advance);
        self.asm.add_reg_imm(Reg::X6, Reg::X6, 1);
        self.asm.branch(outer, BranchKind::Unconditional);
        self.asm.bind(done);
        self.asm.mov_reg(Reg::X0, Reg::X7);
        self.asm.branch(end, BranchKind::Unconditional);
        self.asm.bind(not_found);
        self.asm.mov_imm64(Reg::X0, u64::MAX);
        self.asm.bind(end);
    }

    /// `endsWith(s, suffix)`: s in x0, suffix in x1 -> Bool in x0.
    fn emit_str_ends_with(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.ldr_imm(Reg::X3, Reg::X1, 0);
        let fail = self.asm.new_label();
        let ok = self.asm.new_label();
        let end = self.asm.new_label();
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm.branch(fail, BranchKind::Conditional(Cond::Gt));
        self.asm.sub_reg(Reg::X8, Reg::X2, Reg::X3);
        self.asm.add_reg_imm(Reg::X6, Reg::X0, 8);
        self.asm.add_reg(Reg::X6, Reg::X6, Reg::X8);
        self.asm.add_reg_imm(Reg::X7, Reg::X1, 8);
        let byte_loop = self.asm.new_label();
        self.asm.bind(byte_loop);
        self.asm.branch(ok, BranchKind::CompareZero(Reg::X3));
        self.asm.ldrb_post_increment(Reg::X4, Reg::X6);
        self.asm.ldrb_post_increment(Reg::X5, Reg::X7);
        self.asm.cmp_reg(Reg::X4, Reg::X5);
        self.asm.branch(fail, BranchKind::Conditional(Cond::Ne));
        self.asm.sub_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.branch(byte_loop, BranchKind::Unconditional);
        self.asm.bind(ok);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.branch(end, BranchKind::Unconditional);
        self.asm.bind(fail);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.bind(end);
    }

    /// String equality: a in x0, b in x1 → Bool in x0.
    fn emit_str_eq(&mut self) {
        let differ = self.asm.new_label();
        let same = self.asm.new_label();
        let end = self.asm.new_label();
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.ldr_imm(Reg::X3, Reg::X1, 0);
        self.asm.cmp_reg(Reg::X2, Reg::X3);
        self.asm.branch(differ, BranchKind::Conditional(Cond::Ne));
        self.asm.add_reg_imm(Reg::X6, Reg::X0, 8);
        self.asm.add_reg_imm(Reg::X7, Reg::X1, 8);
        let byte_loop = self.asm.new_label();
        self.asm.bind(byte_loop);
        self.asm.branch(same, BranchKind::CompareZero(Reg::X2));
        self.asm.ldrb_post_increment(Reg::X3, Reg::X6);
        self.asm.ldrb_post_increment(Reg::X4, Reg::X7);
        self.asm.cmp_reg(Reg::X3, Reg::X4);
        self.asm.branch(differ, BranchKind::Conditional(Cond::Ne));
        self.asm.sub_reg_imm(Reg::X2, Reg::X2, 1);
        self.asm.branch(byte_loop, BranchKind::Unconditional);
        self.asm.bind(same);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.branch(end, BranchKind::Unconditional);
        self.asm.bind(differ);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.bind(end);
    }

    /// `length(s)`: UTF-8 character count, matching the evaluator —
    /// bytes whose top two bits are not `10` start a character.
    /// `toUpperCase`/`toLowerCase`: s in x0 -> fresh string object in
    /// x0, ASCII bytes shifted, everything else copied unchanged
    /// (matching the evaluator's and x86_64 backend's ASCII-only
    /// convention -- no full Unicode case tables).
    fn emit_str_ascii_case(&mut self, to_upper: bool) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        // The source is a heap object that must survive the allocation, and
        // its payload pointer is re-derived afterwards rather than carried
        // across (so a relocating collector cannot leave it dangling).
        self.push_rooted(Reg::X0);
        self.emit_alloc_raw_string(Reg::X2);
        self.pop_rooted(Reg::X0);
        self.asm.add_reg_imm(Reg::X3, Reg::X0, 8);
        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X6, Reg::X5, 8);
        self.asm.mov_reg(Reg::X7, Reg::X2);
        let loop_start = self.asm.new_label();
        let store = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.bind(loop_start);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X7));
        self.asm.ldrb_post_increment(Reg::X1, Reg::X3);
        let (lo, hi) = if to_upper { (b'a', b'z') } else { (b'A', b'Z') };
        self.asm.cmp_imm(Reg::X1, u32::from(lo));
        self.asm.branch(store, BranchKind::Conditional(Cond::Lt));
        self.asm.cmp_imm(Reg::X1, u32::from(hi));
        self.asm.branch(store, BranchKind::Conditional(Cond::Gt));
        if to_upper {
            self.asm.sub_reg_imm(Reg::X1, Reg::X1, 32);
        } else {
            self.asm.add_reg_imm(Reg::X1, Reg::X1, 32);
        }
        self.asm.bind(store);
        self.asm.strb_post_increment(Reg::X1, Reg::X6);
        self.asm.sub_reg_imm(Reg::X7, Reg::X7, 1);
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(done);
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// UTF-8 aware `reverse`: s in x0 -> fresh string object in x0
    /// with characters in reverse order (bytes within each character
    /// preserved). Scans backward from the end to find each
    /// character's start byte (top bits != `10`), then copies that
    /// character's bytes forward into the output at the current write
    /// cursor -- characters are discovered in reverse order, so
    /// writing them in discovery order reverses the string.
    fn emit_str_reverse(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.push_rooted(Reg::X0); // survives the allocation
        self.emit_alloc_raw_string(Reg::X2);
        self.pop_rooted(Reg::X0);
        self.asm.add_reg_imm(Reg::X3, Reg::X0, 8); // re-derived payload
        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X6, Reg::X5, 8);
        self.asm.mov_reg(Reg::X8, Reg::X2);
        self.asm.mov_imm64(Reg::X9, 0);

        let outer_loop = self.asm.new_label();
        let find_start = self.asm.new_label();
        let copy_start = self.asm.new_label();
        let copy_loop = self.asm.new_label();
        let copy_done = self.asm.new_label();
        let done = self.asm.new_label();

        self.asm.bind(outer_loop);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X8));
        self.asm.mov_reg(Reg::X10, Reg::X8);

        self.asm.bind(find_start);
        self.asm.sub_reg_imm(Reg::X10, Reg::X10, 1);
        self.asm.ldrb_reg_offset(Reg::X11, Reg::X3, Reg::X10);
        self.asm.lsr_imm(Reg::X11, Reg::X11, 6);
        self.asm.cmp_imm(Reg::X11, 2);
        self.asm
            .branch(copy_start, BranchKind::Conditional(Cond::Ne));
        self.asm
            .branch(find_start, BranchKind::CompareNonZero(Reg::X10));

        self.asm.bind(copy_start);
        self.asm.mov_reg(Reg::X12, Reg::X10);
        self.asm.bind(copy_loop);
        self.asm.cmp_reg(Reg::X12, Reg::X8);
        self.asm
            .branch(copy_done, BranchKind::Conditional(Cond::Ge));
        self.asm.ldrb_reg_offset(Reg::X11, Reg::X3, Reg::X12);
        self.asm.strb_reg_offset(Reg::X11, Reg::X6, Reg::X9);
        self.asm.add_reg_imm(Reg::X12, Reg::X12, 1);
        self.asm.add_reg_imm(Reg::X9, Reg::X9, 1);
        self.asm.branch(copy_loop, BranchKind::Unconditional);

        self.asm.bind(copy_done);
        self.asm.mov_reg(Reg::X8, Reg::X10);
        self.asm.branch(outer_loop, BranchKind::Unconditional);

        self.asm.bind(done);
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// `trim`/`trimLeft`/`trimRight`: s in x0 -> fresh string object
    /// in x0 with leading/trailing ASCII whitespace stripped (ASCII
    /// only, matching the x86_64 backend's accepted precedent -- not
    /// Rust's Unicode-aware `str::trim`).
    fn emit_str_trim(&mut self, trim_left: bool, trim_right: bool) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.add_reg_imm(Reg::X3, Reg::X0, 8);
        self.asm.mov_imm64(Reg::X9, 0);

        if trim_left {
            let left_loop = self.asm.new_label();
            let left_done = self.asm.new_label();
            self.asm.bind(left_loop);
            self.asm.cmp_reg(Reg::X9, Reg::X2);
            self.asm
                .branch(left_done, BranchKind::Conditional(Cond::Ge));
            self.asm.ldrb_reg_offset(Reg::X4, Reg::X3, Reg::X9);
            self.asm.is_ascii_whitespace_into(Reg::X4, Reg::X5);
            self.asm.branch(left_done, BranchKind::CompareZero(Reg::X5));
            self.asm.add_reg_imm(Reg::X9, Reg::X9, 1);
            self.asm.branch(left_loop, BranchKind::Unconditional);
            self.asm.bind(left_done);
        }

        self.asm.mov_reg(Reg::X8, Reg::X2);
        if trim_right {
            let right_loop = self.asm.new_label();
            let right_done = self.asm.new_label();
            self.asm.bind(right_loop);
            self.asm.cmp_reg(Reg::X8, Reg::X9);
            self.asm
                .branch(right_done, BranchKind::Conditional(Cond::Le));
            self.asm.sub_reg_imm(Reg::X8, Reg::X8, 1);
            self.asm.ldrb_reg_offset(Reg::X4, Reg::X3, Reg::X8);
            self.asm.is_ascii_whitespace_into(Reg::X4, Reg::X5);
            self.asm
                .branch(right_loop, BranchKind::CompareNonZero(Reg::X5));
            self.asm.add_reg_imm(Reg::X8, Reg::X8, 1);
            self.asm.bind(right_done);
        }

        self.asm.sub_reg(Reg::X2, Reg::X8, Reg::X9);
        // X9 (trim start index) is outside the X0-X5 range
        // `emit_alloc`'s heap-grow path preserves, so it must be saved
        // across the call explicitly rather than folded into X6
        // beforehand (a prior version computed the slice pointer here
        // and lost it to a heap-grow mmap when the bump allocator's
        // segment was full).
        self.asm.push(Reg::X9); // a scalar index
        self.push_rooted(Reg::X0); // the source object
        self.emit_alloc_raw_string(Reg::X2);
        self.pop_rooted(Reg::X0);
        self.asm.pop(Reg::X9);
        self.asm.add_reg_imm(Reg::X3, Reg::X0, 8); // re-derived payload
        self.asm.add_reg(Reg::X6, Reg::X3, Reg::X9);
        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X6, Reg::X7, Reg::X8);
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// `join(list, separator)`: cons-list head in x0, separator
    /// string in x1 -> fresh string object in x0. Two passes: the
    /// first walks the list to total the element byte lengths and
    /// count them (so the separator's contribution, `sep_len` times
    /// `count minus one`, is known before allocating); the second
    /// walks it again, copying each element into a single exact-size
    /// allocation with the separator interspersed between elements
    /// (never before the first or after the last).
    fn emit_str_join(&mut self) {
        self.asm.mov_reg(Reg::X10, Reg::X0);
        // The barriered cell reads below clobber x0, so the list head is
        // parked for the second pass -- as a root, since a collection can
        // now happen while it waits.
        self.push_rooted(Reg::X0);
        self.asm.mov_imm64(Reg::X2, 0);
        self.asm.mov_imm64(Reg::X9, 0);
        let count_loop = self.asm.new_label();
        let count_done = self.asm.new_label();
        self.asm.bind(count_loop);
        self.asm
            .branch(count_done, BranchKind::CompareZero(Reg::X10));
        self.emit_gc_load_ptr(Reg::X10, 0); // x0 = element (barriered)
        self.asm.mov_reg(Reg::X11, Reg::X0);
        self.asm.ldr_imm(Reg::X12, Reg::X11, 0); // its length (a scalar)
        self.asm.add_reg(Reg::X2, Reg::X2, Reg::X12);
        self.asm.add_reg_imm(Reg::X9, Reg::X9, 1);
        self.emit_gc_load_ptr(Reg::X10, 8); // x0 = next (barriered)
        self.asm.mov_reg(Reg::X10, Reg::X0);
        self.asm.branch(count_loop, BranchKind::Unconditional);
        self.asm.bind(count_done);
        self.pop_rooted(Reg::X0); // the list head again

        let no_sep = self.asm.new_label();
        self.asm.cmp_imm(Reg::X9, 0);
        self.asm.branch(no_sep, BranchKind::Conditional(Cond::Eq));
        self.asm.ldr_imm(Reg::X11, Reg::X1, 0);
        self.asm.sub_reg_imm(Reg::X12, Reg::X9, 1);
        self.asm.mul_reg(Reg::X11, Reg::X11, Reg::X12);
        self.asm.add_reg(Reg::X2, Reg::X2, Reg::X11);
        self.asm.bind(no_sep);

        // X9 (count) is outside emit_alloc's x0-x5 preserved range, so
        // it must be saved across the call explicitly (see the
        // emit_str_trim fix for why this matters once a heap-grow
        // mmap actually fires).
        self.asm.push(Reg::X9); // the element count, a scalar
        self.push_rooted(Reg::X0); // the list
        self.push_rooted(Reg::X1); // the separator
        self.emit_alloc_raw_string(Reg::X2);
        self.pop_rooted(Reg::X1);
        self.pop_rooted(Reg::X0);
        self.asm.pop(Reg::X9);

        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X6, Reg::X5, 8);
        self.asm.mov_reg(Reg::X10, Reg::X0);
        self.asm.mov_imm64(Reg::X12, 0);

        let copy_loop = self.asm.new_label();
        let copy_done = self.asm.new_label();
        let skip_sep = self.asm.new_label();
        self.asm.bind(copy_loop);
        self.asm
            .branch(copy_done, BranchKind::CompareZero(Reg::X10));
        self.asm.cmp_imm(Reg::X12, 0);
        self.asm.branch(skip_sep, BranchKind::Conditional(Cond::Eq));
        self.asm.ldr_imm(Reg::X2, Reg::X1, 0);
        self.asm.add_reg_imm(Reg::X3, Reg::X1, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X3, Reg::X6, Reg::X4);
        self.asm.bind(skip_sep);
        self.emit_gc_load_ptr(Reg::X10, 0); // x0 = element (barriered)
        self.asm.mov_reg(Reg::X11, Reg::X0);
        self.asm.ldr_imm(Reg::X2, Reg::X11, 0);
        self.asm.add_reg_imm(Reg::X3, Reg::X11, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X3, Reg::X6, Reg::X4);
        self.asm.add_reg_imm(Reg::X12, Reg::X12, 1);
        self.emit_gc_load_ptr(Reg::X10, 8); // x0 = next (barriered)
        self.asm.mov_reg(Reg::X10, Reg::X0);
        self.asm.branch(copy_loop, BranchKind::Unconditional);
        self.asm.bind(copy_done);

        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// `replaceAll`(input, pattern, replacement): literal-substring
    /// replacement (not the evaluator's `"[0-9]"` pseudo-regex special
    /// case, and an empty pattern aborts rather than replicating
    /// Rust's between-every-character `str::replace("")` semantics --
    /// both explicit, documented scope reductions from x86_64
    /// parity). Non-overlapping matches, scanning left to right.
    ///
    /// This needs six fixed values alive across the whole two-pass
    /// operation (each of input/pattern/replacement's byte-base
    /// pointer and length) plus a running match count and total
    /// result length -- more than fits resident in registers across
    /// two `emit_alloc` calls without a deliberate spill scheme (this
    /// is exactly the register-pressure wall an earlier attempt at
    /// this routine hit and abandoned). Instead of spilling to
    /// individual stack slots, the six fixed values are written once
    /// into a single 48-byte scratch heap object right after they're
    /// computed, and re-loaded from it by offset wherever needed --
    /// so the only value that must stay resident in a register across
    /// every subsequent `emit_alloc` call is the one pointer to that
    /// object (`x6`), the same one-value-survives-the-call discipline
    /// every other M13 routine already uses (M13 slice 1's `trim` bug
    /// was exactly a missed instance of it).
    fn emit_str_replace_all(&mut self) {
        // x0=input, x1=pattern, x2=replacement. The scratch struct below
        // holds all three as rooted slots, which is both what keeps them
        // alive across the result allocation and what keeps the payload
        // pointers derived from them current if the collector moves them.
        self.asm.ldr_imm(Reg::X3, Reg::X1, 0); // pattern_len
        let pattern_ok = self.asm.new_label();
        self.asm
            .branch(pattern_ok, BranchKind::CompareNonZero(Reg::X3));
        self.asm.emit_write_rodata(
            STDERR_FD,
            b"klassic: replaceAll pattern must not be empty\n",
        );
        self.asm.emit_exit(1);
        self.asm.bind(pattern_ok);

        // The scratch struct lives on the machine stack rather than the GC
        // heap: the collector's sweep walks each region block by block
        // through object headers, so a raw header-less block inside the heap
        // would desynchronise that walk.
        //
        // Slots 0/16/32 hold the three *objects* and are GC roots, so the
        // collector keeps them alive and rewrites them if it moves them;
        // every payload pointer below is re-derived as `object + 8` at the
        // point of use and can therefore never go stale.
        self.asm.sub_sp_imm(48);
        self.asm.add_reg_sp_imm(Reg::X6, 0);

        // Scratch layout: [0]=input_bytes_base [8]=input_len
        // [16]=pattern_bytes_base [24]=pattern_len
        // [32]=replacement_bytes_base [40]=replacement_len
        self.asm.ldr_imm(Reg::X7, Reg::X0, 0);
        self.asm.str_imm(Reg::X7, Reg::X6, 8); // input_len
        self.asm.str_imm(Reg::X0, Reg::X6, 0); // input object (rooted)
        self.asm.ldr_imm(Reg::X7, Reg::X1, 0);
        self.asm.str_imm(Reg::X7, Reg::X6, 24); // pattern_len
        self.asm.str_imm(Reg::X1, Reg::X6, 16); // pattern object (rooted)
        self.asm.ldr_imm(Reg::X7, Reg::X2, 0);
        self.asm.str_imm(Reg::X7, Reg::X6, 40); // replacement_len
        self.asm.str_imm(Reg::X2, Reg::X6, 32); // replacement object (rooted)
        self.emit_root_address(Reg::X6, 0);
        self.emit_root_address(Reg::X6, 16);
        self.emit_root_address(Reg::X6, 32);

        // Pass 1: count non-overlapping matches.
        self.asm.ldr_imm(Reg::X8, Reg::X6, 0); // input_bytes_base
        self.asm.add_reg_imm(Reg::X8, Reg::X8, 8); // payload of the rooted object
        self.asm.ldr_imm(Reg::X9, Reg::X6, 8); // input_len
        self.asm.mov_imm64(Reg::X10, 0); // pos
        self.asm.mov_imm64(Reg::X11, 0); // match_count

        let count_loop = self.asm.new_label();
        let count_done = self.asm.new_label();
        let count_is_match = self.asm.new_label();
        self.asm.bind(count_loop);
        self.asm.ldr_imm(Reg::X7, Reg::X6, 24); // pattern_len
        self.asm.add_reg(Reg::X0, Reg::X10, Reg::X7);
        self.asm.cmp_reg(Reg::X0, Reg::X9);
        self.asm
            .branch(count_done, BranchKind::Conditional(Cond::Gt));
        self.asm.add_reg(Reg::X0, Reg::X8, Reg::X10); // a_ptr
        self.asm.ldr_imm(Reg::X1, Reg::X6, 16); // b_ptr = pattern_bytes_base
        self.asm.add_reg_imm(Reg::X1, Reg::X1, 8); // payload of the rooted object
        self.asm.mov_reg(Reg::X2, Reg::X7); // len = pattern_len
        self.asm
            .bytes_equal(Reg::X2, Reg::X0, Reg::X1, Reg::X3, Reg::X4, Reg::X12);
        self.asm
            .branch(count_is_match, BranchKind::CompareNonZero(Reg::X3));
        self.asm.add_reg_imm(Reg::X10, Reg::X10, 1);
        self.asm.branch(count_loop, BranchKind::Unconditional);
        self.asm.bind(count_is_match);
        self.asm.add_reg_imm(Reg::X11, Reg::X11, 1);
        self.asm.add_reg(Reg::X10, Reg::X10, Reg::X7);
        self.asm.branch(count_loop, BranchKind::Unconditional);
        self.asm.bind(count_done);

        // total_content_len = input_len + match_count*(replacement_len - pattern_len)
        self.asm.ldr_imm(Reg::X0, Reg::X6, 40); // replacement_len
        self.asm.ldr_imm(Reg::X1, Reg::X6, 24); // pattern_len
        self.asm.sub_reg(Reg::X0, Reg::X0, Reg::X1);
        self.asm.mul_reg(Reg::X0, Reg::X0, Reg::X11);
        self.asm.add_reg(Reg::X0, Reg::X9, Reg::X0);

        self.asm.push(Reg::X6);
        self.asm.push(Reg::X0); // total_content_len
        self.emit_alloc_raw_string(Reg::X0); // x5 = result object user pointer
        self.asm.pop(Reg::X12); // total_content_len
        self.asm.pop(Reg::X6); // scratch struct ptr

        self.asm.str_imm(Reg::X12, Reg::X5, 0);

        // Pass 2: copy, substituting at each match.
        self.asm.mov_imm64(Reg::X10, 0); // pos
        self.asm.add_reg_imm(Reg::X11, Reg::X5, 8); // dst cursor

        let copy_loop = self.asm.new_label();
        let copy_no_match = self.asm.new_label();
        let copy_is_match = self.asm.new_label();
        let copy_done = self.asm.new_label();
        self.asm.bind(copy_loop);
        self.asm.ldr_imm(Reg::X0, Reg::X6, 8); // input_len
        self.asm.cmp_reg(Reg::X10, Reg::X0);
        self.asm
            .branch(copy_done, BranchKind::Conditional(Cond::Ge));
        self.asm.ldr_imm(Reg::X7, Reg::X6, 24); // pattern_len
        self.asm.add_reg(Reg::X0, Reg::X10, Reg::X7);
        self.asm.ldr_imm(Reg::X1, Reg::X6, 8); // input_len
        self.asm.cmp_reg(Reg::X0, Reg::X1);
        self.asm
            .branch(copy_no_match, BranchKind::Conditional(Cond::Gt));
        self.asm.ldr_imm(Reg::X0, Reg::X6, 0); // input_bytes_base
        self.asm.add_reg_imm(Reg::X0, Reg::X0, 8); // payload of the rooted object
        self.asm.add_reg(Reg::X0, Reg::X0, Reg::X10);
        self.asm.ldr_imm(Reg::X1, Reg::X6, 16); // pattern_bytes_base
        self.asm.add_reg_imm(Reg::X1, Reg::X1, 8); // payload of the rooted object
        self.asm.mov_reg(Reg::X2, Reg::X7);
        self.asm
            .bytes_equal(Reg::X2, Reg::X0, Reg::X1, Reg::X3, Reg::X4, Reg::X12);
        self.asm
            .branch(copy_is_match, BranchKind::CompareNonZero(Reg::X3));

        self.asm.bind(copy_no_match);
        self.asm.ldr_imm(Reg::X0, Reg::X6, 0); // input_bytes_base
        self.asm.add_reg_imm(Reg::X0, Reg::X0, 8); // payload of the rooted object
        self.asm.add_reg(Reg::X0, Reg::X0, Reg::X10);
        self.asm.ldrb_post_increment(Reg::X1, Reg::X0);
        self.asm.strb_post_increment(Reg::X1, Reg::X11);
        self.asm.add_reg_imm(Reg::X10, Reg::X10, 1);
        self.asm.branch(copy_loop, BranchKind::Unconditional);

        self.asm.bind(copy_is_match);
        self.asm.ldr_imm(Reg::X0, Reg::X6, 32); // replacement_bytes_base
        self.asm.add_reg_imm(Reg::X0, Reg::X0, 8); // payload of the rooted object
        self.asm.ldr_imm(Reg::X1, Reg::X6, 40); // replacement_len
        self.emit_copy_bytes(Reg::X1, Reg::X0, Reg::X11, Reg::X2);
        self.asm.ldr_imm(Reg::X7, Reg::X6, 24); // pattern_len
        self.asm.add_reg(Reg::X10, Reg::X10, Reg::X7);
        self.asm.branch(copy_loop, BranchKind::Unconditional);
        self.asm.bind(copy_done);

        self.asm.mov_reg(Reg::X0, Reg::X5);
        self.emit_shadow_pop(3); // the three rooted scratch slots
        self.asm.add_sp_imm(48); // then the scratch itself
    }

    /// Build a fresh string object holding `input_bytes_base[start..end]`
    /// (byte offsets, in x0/x1) and push it onto the machine stack.
    /// Expects the split scratch struct in x6, `input_bytes_base` in
    /// x8, `input_len` in x9, scan position in x10, segment-start
    /// offset in x11, and running segment count in x12 -- all six
    /// survive the `emit_alloc` call this needs, via an explicit
    /// push/pop bracket (the one-value-survives-the-call discipline
    /// `emit_str_replace_all` uses, just for six values at once
    /// instead of one).
    fn emit_split_build_segment_and_push(&mut self, start: Reg, end: Reg) {
        self.asm.sub_reg(Reg::X2, end, start); // len = end - start
        self.asm.push(Reg::X6);
        self.asm.push(Reg::X8);
        self.asm.push(Reg::X9);
        self.asm.push(Reg::X10);
        self.asm.push(Reg::X11);
        self.asm.push(Reg::X12);
        self.asm.push(Reg::X2); // len
        self.asm.push(start);
        self.emit_alloc_raw_string(Reg::X2); // x5 = new segment string object
        self.asm.pop(Reg::X0); // start offset
        self.asm.pop(Reg::X2); // len
        self.asm.pop(Reg::X12);
        self.asm.pop(Reg::X11);
        self.asm.pop(Reg::X10);
        self.asm.pop(Reg::X9);
        self.asm.pop(Reg::X8);
        self.asm.pop(Reg::X6);

        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        // Re-derive the input's payload from the rooted scratch slot: the
        // allocation above can collect, and once the collector moves objects
        // the x8 saved across it would be a stale interior pointer.
        self.asm.ldr_imm(Reg::X8, Reg::X6, 0);
        self.asm.add_reg_imm(Reg::X8, Reg::X8, 8);
        self.asm.add_reg(Reg::X3, Reg::X8, Reg::X0); // src = payload + start
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8); // dst = new object's payload
        self.emit_copy_bytes(Reg::X2, Reg::X3, Reg::X7, Reg::X1);
        self.asm.mov_reg(Reg::X0, Reg::X5);
        // The finished segments queue on the machine stack while the
        // remaining ones are built (each allocates) and are consumed by
        // emit_cons_cell, which pops them as roots -- so they are pushed as
        // roots here.
        self.push_rooted(Reg::X0);
        self.asm.add_reg_imm(Reg::X12, Reg::X12, 1);
    }

    /// `split`(input, delimiter): non-empty-delimiter path, `Rust
    /// str::split`-equivalent semantics (non-overlapping matches
    /// scanned left to right, an empty segment between adjacent
    /// delimiters or at a leading/trailing delimiter). Two-pass-plus-
    /// build design: scan once, building each segment string and
    /// pushing it onto the machine stack in left-to-right order, then
    /// pop them back off (right-to-left, i.e. last segment first) and
    /// `cons` each onto a growing list -- since `cons` prepends, this
    /// naturally reassembles the original left-to-right order.
    ///
    /// Uses the same scratch-heap-struct approach as `replaceAll` for
    /// the two fixed values needed throughout (input/delimiter's
    /// byte-base pointer and length), since that's what actually
    /// solved the register-pressure wall that stalled the earlier
    /// `replaceAll` attempt, generalized here to a 32-byte struct.
    fn emit_str_split_nonempty_delimiter(&mut self) {
        // x0 = input, x1 = delimiter. Machine-stack scratch, not a GC-heap
        // block, and slots 0/16 hold the two *objects* as GC roots: that is
        // what keeps them alive across the segment allocations and what lets
        // every payload pointer be re-derived as `object + 8`, so none can
        // dangle once the collector moves objects (see
        // emit_str_replace_all).
        self.asm.sub_sp_imm(32);
        self.asm.add_reg_sp_imm(Reg::X6, 0);

        // Scratch layout: [0]=input_bytes_base [8]=input_len
        // [16]=delimiter_bytes_base [24]=delimiter_len
        self.asm.ldr_imm(Reg::X7, Reg::X0, 0);
        self.asm.str_imm(Reg::X7, Reg::X6, 8); // input_len
        self.asm.str_imm(Reg::X0, Reg::X6, 0); // input object (rooted)
        self.asm.ldr_imm(Reg::X7, Reg::X1, 0);
        self.asm.str_imm(Reg::X7, Reg::X6, 24); // delimiter_len
        self.asm.str_imm(Reg::X1, Reg::X6, 16); // delimiter object (rooted)
        self.emit_root_address(Reg::X6, 0);
        self.emit_root_address(Reg::X6, 16);

        self.asm.ldr_imm(Reg::X8, Reg::X6, 0); // the rooted input object
        self.asm.add_reg_imm(Reg::X8, Reg::X8, 8); // its payload
        self.asm.ldr_imm(Reg::X9, Reg::X6, 8); // input_len
        self.asm.mov_imm64(Reg::X10, 0); // pos
        self.asm.mov_imm64(Reg::X11, 0); // segment_start
        self.asm.mov_imm64(Reg::X12, 0); // segment_count

        let scan_loop = self.asm.new_label();
        let scan_is_match = self.asm.new_label();
        let scan_done = self.asm.new_label();
        self.asm.bind(scan_loop);
        self.asm.ldr_imm(Reg::X0, Reg::X6, 24); // delimiter_len
        self.asm.add_reg(Reg::X1, Reg::X10, Reg::X0);
        self.asm.cmp_reg(Reg::X1, Reg::X9);
        self.asm
            .branch(scan_done, BranchKind::Conditional(Cond::Gt));
        self.asm.add_reg(Reg::X0, Reg::X8, Reg::X10); // a_ptr
        self.asm.ldr_imm(Reg::X1, Reg::X6, 16); // the rooted delimiter object
        self.asm.add_reg_imm(Reg::X1, Reg::X1, 8); // b_ptr = its payload
        self.asm.ldr_imm(Reg::X2, Reg::X6, 24); // len = delimiter_len
        self.asm
            .bytes_equal(Reg::X2, Reg::X0, Reg::X1, Reg::X3, Reg::X4, Reg::X7);
        self.asm
            .branch(scan_is_match, BranchKind::CompareNonZero(Reg::X3));
        self.asm.add_reg_imm(Reg::X10, Reg::X10, 1);
        self.asm.branch(scan_loop, BranchKind::Unconditional);

        self.asm.bind(scan_is_match);
        self.emit_split_build_segment_and_push(Reg::X11, Reg::X10);
        self.asm.ldr_imm(Reg::X7, Reg::X6, 24); // delimiter_len
        self.asm.add_reg(Reg::X10, Reg::X10, Reg::X7);
        self.asm.mov_reg(Reg::X11, Reg::X10);
        self.asm.branch(scan_loop, BranchKind::Unconditional);

        self.asm.bind(scan_done);
        self.emit_split_build_segment_and_push(Reg::X11, Reg::X9);

        // Pop segments back off (last-pushed first) and cons each
        // onto a growing list -- this reassembles left-to-right order.
        self.asm.mov_imm64(Reg::X0, 0); // list = nil
        let cons_loop = self.asm.new_label();
        let cons_done = self.asm.new_label();
        self.asm.bind(cons_loop);
        self.asm
            .branch(cons_done, BranchKind::CompareZero(Reg::X12));
        self.emit_cons_cell();
        self.asm.sub_reg_imm(Reg::X12, Reg::X12, 1);
        self.asm.branch(cons_loop, BranchKind::Unconditional);
        self.asm.bind(cons_done);
        self.emit_shadow_pop(2); // the two rooted scratch slots
        self.asm.add_sp_imm(32); // then the scratch itself
    }

    /// `split`(input, ""): each UTF-8 character becomes its own
    /// one-character list element. Scans left to right, treating any
    /// byte whose top two bits aren't `10` (a UTF-8 continuation
    /// byte) as the start of a new character -- the same detection
    /// `emit_str_char_count`/`emit_str_reverse` already use. Shares
    /// `emit_split_build_segment_and_push`'s scratch-object
    /// registers (x6/x8/x9/x10/x11/x12) even though there's no
    /// delimiter scratch struct here; x6 is simply an unused
    /// push/pop round-trip in this path (harmless, since nothing
    /// reads it back).
    fn emit_str_split_chars(&mut self) {
        // x0 = input. A one-slot machine-stack scratch holds the object as a
        // GC root, so the segment builder can re-derive the payload pointer
        // after each allocation instead of carrying a stale one (see
        // emit_str_split_nonempty_delimiter).
        self.asm.sub_sp_imm(16);
        self.asm.add_reg_sp_imm(Reg::X6, 0);
        self.asm.str_imm(Reg::X0, Reg::X6, 0);
        self.emit_root_address(Reg::X6, 0);
        self.asm.ldr_imm(Reg::X9, Reg::X0, 0); // input_len
        self.asm.add_reg_imm(Reg::X8, Reg::X0, 8); // input_bytes_base
        self.asm.mov_imm64(Reg::X10, 0); // pos
        self.asm.mov_imm64(Reg::X12, 0); // char_count

        let scan_loop = self.asm.new_label();
        let scan_done = self.asm.new_label();
        let continuation_loop = self.asm.new_label();
        let char_end_found = self.asm.new_label();
        self.asm.bind(scan_loop);
        self.asm.cmp_reg(Reg::X10, Reg::X9);
        self.asm
            .branch(scan_done, BranchKind::Conditional(Cond::Ge));
        self.asm.mov_reg(Reg::X11, Reg::X10); // char_start = pos
        self.asm.add_reg_imm(Reg::X10, Reg::X10, 1); // consume the lead byte

        self.asm.bind(continuation_loop);
        self.asm.cmp_reg(Reg::X10, Reg::X9);
        self.asm
            .branch(char_end_found, BranchKind::Conditional(Cond::Ge));
        self.asm.add_reg(Reg::X0, Reg::X8, Reg::X10);
        self.asm.ldrb(Reg::X1, Reg::X0);
        self.asm.lsr_imm(Reg::X1, Reg::X1, 6);
        self.asm.cmp_imm(Reg::X1, 2);
        self.asm
            .branch(char_end_found, BranchKind::Conditional(Cond::Ne));
        self.asm.add_reg_imm(Reg::X10, Reg::X10, 1); // consume continuation byte
        self.asm
            .branch(continuation_loop, BranchKind::Unconditional);

        self.asm.bind(char_end_found);
        self.emit_split_build_segment_and_push(Reg::X11, Reg::X10);
        self.asm.branch(scan_loop, BranchKind::Unconditional);
        self.asm.bind(scan_done);

        self.asm.mov_imm64(Reg::X0, 0); // list = nil
        let cons_loop = self.asm.new_label();
        let cons_done = self.asm.new_label();
        self.asm.bind(cons_loop);
        self.asm
            .branch(cons_done, BranchKind::CompareZero(Reg::X12));
        self.emit_cons_cell();
        self.asm.sub_reg_imm(Reg::X12, Reg::X12, 1);
        self.asm.branch(cons_loop, BranchKind::Unconditional);
        self.asm.bind(cons_done);
        self.emit_shadow_pop(1); // the rooted scratch slot
        self.asm.add_sp_imm(16); // then the scratch itself
    }

    /// `split`(input, delimiter): dispatches to the empty-delimiter
    /// (per-character) or non-empty-delimiter path.
    fn emit_str_split(&mut self) {
        // x0 = input, x1 = delimiter
        self.asm.push(Reg::X0);
        self.asm.push(Reg::X1);
        self.asm.ldr_imm(Reg::X2, Reg::X1, 0); // delimiter_len
        let non_empty = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm
            .branch(non_empty, BranchKind::CompareNonZero(Reg::X2));
        self.asm.pop(Reg::X1);
        self.asm.pop(Reg::X0);
        self.emit_str_split_chars();
        self.asm.branch(done, BranchKind::Unconditional);
        self.asm.bind(non_empty);
        self.asm.pop(Reg::X1);
        self.asm.pop(Reg::X0);
        self.emit_str_split_nonempty_delimiter();
        self.asm.bind(done);
    }

    fn emit_str_char_count(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.add_reg_imm(Reg::X3, Reg::X0, 8);
        self.asm.mov_imm64(Reg::X4, 0);
        let count_loop = self.asm.new_label();
        let done = self.asm.new_label();
        let continuation = self.asm.new_label();
        self.asm.bind(count_loop);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X2));
        self.asm.ldrb_post_increment(Reg::X5, Reg::X3);
        self.asm.lsr_imm(Reg::X5, Reg::X5, 6);
        self.asm.cmp_imm(Reg::X5, 2);
        self.asm
            .branch(continuation, BranchKind::Conditional(Cond::Eq));
        self.asm.add_reg_imm(Reg::X4, Reg::X4, 1);
        self.asm.bind(continuation);
        self.asm.sub_reg_imm(Reg::X2, Reg::X2, 1);
        self.asm.branch(count_loop, BranchKind::Unconditional);
        self.asm.bind(done);
        self.asm.mov_reg(Reg::X0, Reg::X4);
    }

    /// Advance over up to x5 UTF-8 characters: x3 = byte pointer
    /// (advanced), x4 = remaining bytes (decremented), x5 destroyed,
    /// x6 clobbered. Stops early when the bytes run out, which is
    /// what clamps out-of-range indices.
    fn emit_skip_chars(&mut self) {
        let scan_loop = self.asm.new_label();
        let done = self.asm.new_label();
        let char_done = self.asm.new_label();
        let continuation_loop = self.asm.new_label();
        self.asm.bind(scan_loop);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X5));
        self.asm.branch(done, BranchKind::CompareZero(Reg::X4));
        // Consume the lead byte, then any continuation bytes (top
        // bits `10`).
        self.asm.ldrb_post_increment(Reg::X6, Reg::X3);
        self.asm.sub_reg_imm(Reg::X4, Reg::X4, 1);
        self.asm.bind(continuation_loop);
        self.asm.branch(char_done, BranchKind::CompareZero(Reg::X4));
        self.asm.ldrb(Reg::X6, Reg::X3);
        self.asm.lsr_imm(Reg::X6, Reg::X6, 6);
        self.asm.cmp_imm(Reg::X6, 2);
        self.asm
            .branch(char_done, BranchKind::Conditional(Cond::Ne));
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.sub_reg_imm(Reg::X4, Reg::X4, 1);
        self.asm
            .branch(continuation_loop, BranchKind::Unconditional);
        self.asm.bind(char_done);
        self.asm.sub_reg_imm(Reg::X5, Reg::X5, 1);
        self.asm.branch(scan_loop, BranchKind::Unconditional);
        self.asm.bind(done);
    }

    /// `substring(s, start, end)` with the evaluator's semantics:
    /// char-indexed, negatives clamp to 0, everything clamps to the
    /// string, `end < start` yields the empty string. In: s = x0,
    /// start = x1, end = x2; out: fresh object pointer in x0.
    fn emit_substring(&mut self) {
        // Clamp negative indices to zero.
        for reg in [Reg::X1, Reg::X2] {
            let non_negative = self.asm.new_label();
            self.asm.cmp_imm(reg, 0);
            self.asm
                .branch(non_negative, BranchKind::Conditional(Cond::Ge));
            self.asm.mov_imm64(reg, 0);
            self.asm.bind(non_negative);
        }
        // First scan: skip `start` characters from the payload. x9 keeps
        // the source object itself (emit_skip_chars only touches x3-x6) so
        // that it stays reachable across the allocation below, from which
        // the slice pointer is re-derived.
        self.asm.mov_reg(Reg::X9, Reg::X0);
        self.asm.ldr_imm(Reg::X4, Reg::X0, 0);
        self.asm.add_reg_imm(Reg::X3, Reg::X0, 8);
        self.asm.mov_reg(Reg::X5, Reg::X1);
        // chars to take = max(end - start, 0); computed before the
        // scans clobber x1/x2.
        self.asm.sub_reg(Reg::X2, Reg::X2, Reg::X1);
        self.emit_skip_chars();
        // x0 is free now: keep the slice's first byte there.
        self.asm.mov_reg(Reg::X0, Reg::X3);
        let non_negative_take = self.asm.new_label();
        self.asm.cmp_imm(Reg::X2, 0);
        self.asm
            .branch(non_negative_take, BranchKind::Conditional(Cond::Ge));
        self.asm.mov_imm64(Reg::X2, 0);
        self.asm.bind(non_negative_take);
        // Second scan: advance over the characters being taken.
        self.asm.mov_reg(Reg::X5, Reg::X2);
        self.emit_skip_chars();
        // Slice byte length and its offset within the source object, then
        // allocate (which can collect) and copy from the re-derived slice.
        self.asm.sub_reg(Reg::X2, Reg::X3, Reg::X0);
        self.asm.sub_reg(Reg::X1, Reg::X0, Reg::X9); // slice offset
        self.asm.push(Reg::X1); // a scalar
        self.push_rooted(Reg::X9); // the source object
        self.emit_alloc_raw_string(Reg::X2);
        self.pop_rooted(Reg::X9);
        self.asm.pop(Reg::X1);
        self.asm.add_reg(Reg::X0, Reg::X9, Reg::X1); // re-derived slice
        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X0, Reg::X7, Reg::X3);
        self.asm.mov_reg(Reg::X0, Reg::X5);
    }

    /// `toString` of the integer in x0 → fresh string object in x0.
    fn emit_int_to_str(&mut self) {
        self.asm.sub_sp_imm(32);
        self.asm.emit_int_digits(false);
        self.asm.add_reg_sp_imm(Reg::X2, 32);
        self.asm.sub_reg(Reg::X2, Reg::X2, Reg::X1);
        self.emit_alloc_raw_string(Reg::X2);
        self.asm.str_imm(Reg::X2, Reg::X5, 0);
        self.asm.add_reg_imm(Reg::X7, Reg::X5, 8);
        self.emit_copy_bytes(Reg::X2, Reg::X1, Reg::X7, Reg::X3);
        self.asm.mov_reg(Reg::X0, Reg::X5);
        self.asm.add_sp_imm(32);
    }

    /// `toString` of the Bool in x0 → interned "true"/"false" object.
    fn emit_bool_to_str(&mut self) {
        let false_label = self.asm.new_label();
        let end_label = self.asm.new_label();
        let true_offset = self.asm.intern_string_object("true");
        let false_offset = self.asm.intern_string_object("false");
        self.asm
            .branch(false_label, BranchKind::CompareZero(Reg::X0));
        self.asm.load_rodata_address(Reg::X0, true_offset);
        self.asm.branch(end_label, BranchKind::Unconditional);
        self.asm.bind(false_label);
        self.asm.load_rodata_address(Reg::X0, false_offset);
        self.asm.bind(end_label);
        // Like any other literal, hand back a heap copy so the result is a
        // traceable object rather than an image address.
        self.emit_heap_string_copy();
    }

    /// Build a record object from `count` field values pushed onto
    /// the machine stack in declaration order → object pointer in x0.
    fn emit_record_object(&mut self, count: usize) {
        // M6: a GC-shaped POINTER_RECORD -- every field is a heap pointer
        // (scalar fields were boxed by the caller as they were evaluated),
        // so the collector can trace the payload uniformly. The 16-byte
        // header shifts the user pointer to block + 16, but fields still
        // live at [user_ptr + position*8], so field access is unchanged.
        self.emit_gc_alloc_object(count, crate::gc_layout::GC_TYPE_POINTER_RECORD); // x0 = user ptr
        for position in (0..count).rev() {
            // The caller parked each field as a rooted machine-stack slot.
            self.pop_rooted(Reg::X1);
            self.emit_color_ptr(Reg::X1, Reg::X1); // colour on store
            self.asm.str_imm(Reg::X1, Reg::X0, (position * 8) as u32);
        }
    }

    /// Prepend a cons cell: head value on top of the machine stack,
    /// tail pointer in x0 → fresh cell pointer in x0.
    ///
    /// M6: a GC-shaped `POINTER_RECORD` with two pointer slots
    /// `[head][next]`. The head on the stack is already a pointer -- a
    /// scalar head was boxed by the caller, a string/list/record head is
    /// a pointer already -- and `next` in x0 is a cell pointer or 0
    /// (nil), so the collector can trace both slots uniformly. The
    /// 16-byte header shifts the user pointer to block + 16, but the head
    /// and next still live at `[cell + 0]`/`[cell + 8]`, so every list
    /// read keeps the same offsets.
    fn emit_cons_cell(&mut self) {
        // Both `next` (parked here) and `head` (parked by the caller) are
        // heap references waiting across an allocation, so both machine-
        // stack slots are GC roots for that window.
        self.push_rooted(Reg::X0); // `next`
        self.emit_gc_alloc_object(2, crate::gc_layout::GC_TYPE_POINTER_RECORD); // x0 = user ptr
        self.pop_rooted(Reg::X1);
        self.emit_color_ptr(Reg::X1, Reg::X1); // colour on store
        self.asm.str_imm(Reg::X1, Reg::X0, 8); // [cell + 8] = next
        self.pop_rooted(Reg::X1);
        self.emit_color_ptr(Reg::X1, Reg::X1);
        self.asm.str_imm(Reg::X1, Reg::X0, 0); // [cell + 0] = head
    }

    /// Get/create the lazily-emitted membership-scan routine label for
    /// an element type. Both the routines take `x0` = list head and
    /// `x1` = candidate and return a Bool in `x0`.
    fn member_label(&mut self, elem: ListElem) -> Label {
        let slot = match elem {
            ListElem::Str => &mut self.member_string_label,
            _ => &mut self.member_scalar_label,
        };
        match slot {
            Some(label) => *label,
            None => {
                let label = self.asm.new_label();
                *slot = Some(label);
                label
            }
        }
    }

    fn reverse_label(&mut self) -> Label {
        match self.list_reverse_label {
            Some(label) => label,
            None => {
                let label = self.asm.new_label();
                self.list_reverse_label = Some(label);
                label
            }
        }
    }

    /// Compile a `%(...)` set literal: each element is added to a
    /// cons-list accumulator only if a membership scan does not find
    /// it, so duplicates collapse to their first occurrence. The
    /// accumulator ends up in reverse-insertion order, so a final
    /// reverse restores insertion order for printing.
    fn set_literal(&mut self, elements: &[Expr], span: Span) -> Result<ValueType, Diagnostic> {
        let mark = self.open_temp_roots();
        // Frame slot for the partial set; survives the bl calls below.
        // It holds a heap reference across the cons allocations, so it is
        // rooted like a named binding (its root is dropped with the
        // enclosing scope; the slot is never reused).
        let acc = self.reserve_locals(8);
        self.asm.mov_imm64(Reg::X0, 0); // nil
        self.asm.store_local(Reg::X0, acc);
        self.emit_root_frame_slot(acc);

        let mut elem_ty = None;
        for element in elements {
            let ty = self.expression(element)?;
            let Some(this) = list_elem_of(ty) else {
                return Err(unsupported(element.span(), "a set element of this type"));
            };
            if *elem_ty.get_or_insert(this) != this {
                return Err(unsupported(span, "mixed set element types"));
            }
            // Is the candidate (x0) already in the partial set?
            self.asm.mov_reg(Reg::X1, Reg::X0);
            self.push_rooted(Reg::X1); // candidate survives the bl
            self.asm.load_local(Reg::X0, acc);
            let member = self.member_label(this);
            self.asm.branch(member, BranchKind::Link);
            let skip = self.asm.new_label();
            self.asm.branch(skip, BranchKind::CompareNonZero(Reg::X0));
            // Absent: prepend the candidate to the accumulator. Box a
            // scalar candidate so the cons cell's head slot is a pointer.
            self.pop_rooted(Reg::X1);
            self.asm.mov_reg(Reg::X0, Reg::X1);
            if is_boxed_scalar(elem_value_type(this)) {
                self.emit_box_scalar();
            }
            self.push_rooted(Reg::X0); // head for emit_cons_cell
            self.asm.load_local(Reg::X0, acc);
            self.emit_cons_cell();
            self.asm.store_local(Reg::X0, acc);
            let after = self.asm.new_label();
            self.asm.branch(after, BranchKind::Unconditional);
            self.asm.bind(skip);
            self.pop_rooted(Reg::X1); // discard the candidate
            self.asm.bind(after);
        }

        self.asm.load_local(Reg::X0, acc);
        let reverse = self.reverse_label();
        self.asm.branch(reverse, BranchKind::Link);
        self.close_temp_roots(mark);
        Ok(match elem_ty {
            Some(elem) => ValueType::Set(elem),
            None => ValueType::EmptySet,
        })
    }

    /// `%["a": 1 "b": 2]`: an insertion-ordered chain of entries, each entry a
    /// two-slot cell `[key][value]`.
    ///
    /// Keys have to be literals. That is a semantic limit rather than a
    /// representational one: the evaluator lets a later entry overwrite an
    /// earlier one's value while keeping the earlier one's *position*, and with
    /// literal keys that resolution happens here at compile time instead of
    /// needing to mutate an entry. A computed key is reported as unsupported
    /// rather than quietly dropping the overwrite.
    fn map_literal(
        &mut self,
        entries: &[(Expr, Expr)],
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        let mark = self.open_temp_roots();
        let mut ordered: Vec<(&Expr, &Expr)> = Vec::new();
        let mut seen: Vec<String> = Vec::new();
        for (key, value) in entries {
            let Some(literal) = literal_key(key) else {
                return Err(unsupported(key.span(), "a map key that is not a literal"));
            };
            match seen.iter().position(|previous| *previous == literal) {
                Some(at) => ordered[at].1 = value,
                None => {
                    seen.push(literal);
                    ordered.push((key, value));
                }
            }
        }

        // Frame slot for the partial map, rooted like the set literal's: it
        // holds a heap reference across every allocation below.
        let acc = self.reserve_locals(8);
        self.asm.mov_imm64(Reg::X0, 0); // nil
        self.asm.store_local(Reg::X0, acc);
        self.emit_root_frame_slot(acc);

        let mut key_elem = None;
        let mut value_elem = None;
        for (key, value) in ordered {
            let key_ty = self.expression(key)?;
            let Some(this_key) = list_elem_of(key_ty) else {
                return Err(unsupported(key.span(), "a map key of this type"));
            };
            if *key_elem.get_or_insert(this_key) != this_key {
                return Err(unsupported(span, "mixed map key types"));
            }
            if is_boxed_scalar(key_ty) {
                self.emit_box_scalar();
            }
            // The key waits on the machine stack as a root while the value is
            // evaluated, then becomes the entry's head.
            self.push_rooted(Reg::X0);
            let value_ty = self.expression(value)?;
            let Some(this_value) = list_elem_of(value_ty) else {
                return Err(unsupported(value.span(), "a map value of this type"));
            };
            if *value_elem.get_or_insert(this_value) != this_value {
                return Err(unsupported(span, "mixed map value types"));
            }
            if is_boxed_scalar(value_ty) {
                self.emit_box_scalar();
            }
            self.emit_cons_cell(); // entry = [key][value]
            self.push_rooted(Reg::X0);
            self.asm.load_local(Reg::X0, acc);
            self.emit_cons_cell(); // chain cell = [entry][rest]
            self.asm.store_local(Reg::X0, acc);
        }

        self.asm.load_local(Reg::X0, acc);
        let reverse = self.reverse_label();
        self.asm.branch(reverse, BranchKind::Link);
        self.close_temp_roots(mark);
        Ok(match (key_elem, value_elem) {
            (Some(key), Some(value)) => ValueType::Map(key, value),
            _ => ValueType::EmptyMap,
        })
    }

    /// The length of the cons-cell chain in x0 -> x0. Lists, sets and maps are
    /// all the same chain, so they all count the same way.
    fn emit_chain_length(&mut self) {
        let count_loop = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.mov_reg(Reg::X1, Reg::X0);
        // The barriered next-reads clobber x0, so the running count lives in
        // x2 (which the barrier preserves) until the end.
        self.asm.mov_imm64(Reg::X2, 0);
        self.asm.bind(count_loop);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X1));
        self.emit_gc_load_ptr(Reg::X1, 8); // x0 = next (barriered)
        self.asm.mov_reg(Reg::X1, Reg::X0);
        self.asm.add_reg_imm(Reg::X2, Reg::X2, 1);
        self.asm.branch(count_loop, BranchKind::Unconditional);
        self.asm.bind(done);
        self.asm.mov_reg(Reg::X0, Reg::X2);
    }

    /// Prepend a fresh `[key][value]` entry, taken from those two slots, to
    /// the chain in `acc`.
    fn emit_put_entry(&mut self, key_slot: u32, value_slot: u32, acc: u32) {
        self.asm.load_local(Reg::X0, key_slot);
        self.push_rooted(Reg::X0);
        self.asm.load_local(Reg::X0, value_slot);
        self.emit_cons_cell(); // entry = [key][value]
        self.push_rooted(Reg::X0);
        self.asm.load_local(Reg::X0, acc);
        self.emit_cons_cell(); // chain cell = [entry][rest]
        self.asm.store_local(Reg::X0, acc);
    }

    /// The entry whose key equals the one in x1, or 0 when there is none:
    /// x0 = map chain, x1 = key (a boxed scalar or a string pointer)
    /// -> x0 = entry pointer or 0.
    ///
    /// The wanted key, the cursor and the candidate entry all live in a
    /// machine-stack scratch frame because the string comparison clobbers the
    /// low registers; they are rooted, since a comparison of string keys can
    /// be reached across a load barrier that relocates.
    fn emit_map_lookup(&mut self, key: ListElem) {
        let loop_start = self.asm.new_label();
        let found = self.asm.new_label();
        let done = self.asm.new_label();
        self.push_rooted(Reg::X1); // wanted key
        self.push_rooted(Reg::X0); // cursor
        self.asm.bind(loop_start);
        self.pop_rooted(Reg::X2);
        self.push_rooted(Reg::X2); // peek the cursor
        self.asm.branch(done, BranchKind::CompareZero(Reg::X2));
        self.emit_gc_load_ptr(Reg::X2, 0); // x0 = entry (barriered)
        self.push_rooted(Reg::X0); // the entry is the answer if the key matches
        self.emit_gc_load_ptr(Reg::X0, 0); // x0 = this entry's key
        if is_boxed_scalar(elem_value_type(key)) {
            self.emit_unbox_scalar();
        }
        // The wanted key sits two rooted slots below the entry, and each
        // pushed slot is 16 bytes wide here.
        self.asm.add_reg_sp_imm(Reg::X1, 32);
        self.asm.ldr_imm(Reg::X1, Reg::X1, 0);
        match key {
            ListElem::Str => self.emit_str_eq(),
            _ => {
                if is_boxed_scalar(elem_value_type(key)) {
                    self.asm.ldr_imm(Reg::X1, Reg::X1, 0); // unbox the wanted key
                }
                self.asm.cmp_reg(Reg::X0, Reg::X1);
                self.asm.cset(Reg::X0, Cond::Eq);
            }
        }
        self.asm.branch(found, BranchKind::CompareNonZero(Reg::X0));
        self.pop_rooted(Reg::X2); // discard this entry
        self.pop_rooted(Reg::X2); // the cursor
        self.emit_gc_load_ptr(Reg::X2, 8); // x0 = next (barriered)
        self.push_rooted(Reg::X0);
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(found);
        self.pop_rooted(Reg::X0); // the matching entry
        self.pop_rooted(Reg::X2); // the cursor
        self.pop_rooted(Reg::X2); // the wanted key
        let end = self.asm.new_label();
        self.asm.branch(end, BranchKind::Unconditional);
        self.asm.bind(done);
        self.pop_rooted(Reg::X2); // the cursor
        self.pop_rooted(Reg::X2); // the wanted key
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.bind(end);
    }

    /// println of the map in x0 in the evaluator's format: `%[k: v, k2: v2]`.
    fn emit_println_map(
        &mut self,
        key: ListElem,
        value: ListElem,
        span: Span,
    ) -> Result<(), Diagnostic> {
        let loop_start = self.asm.new_label();
        let close = self.asm.new_label();
        self.push_rooted(Reg::X0); // cursor
        self.asm.emit_write_rodata(STDOUT_FD, b"%[");
        self.pop_rooted(Reg::X3);
        self.push_rooted(Reg::X3);
        self.asm.branch(close, BranchKind::CompareZero(Reg::X3));
        self.asm.bind(loop_start);
        self.pop_rooted(Reg::X3);
        self.push_rooted(Reg::X3);
        self.emit_gc_load_ptr(Reg::X3, 0); // x0 = entry (barriered)
        self.push_rooted(Reg::X0); // the entry survives printing the key
        self.emit_gc_load_ptr(Reg::X0, 0); // x0 = key
        if is_boxed_scalar(elem_value_type(key)) {
            self.emit_unbox_scalar();
        }
        self.emit_print_elem(key, span)?;
        self.asm.emit_write_rodata(STDOUT_FD, b": ");
        self.pop_rooted(Reg::X3); // the entry
        self.emit_gc_load_ptr(Reg::X3, 8); // x0 = value
        if is_boxed_scalar(elem_value_type(value)) {
            self.emit_unbox_scalar();
        }
        self.emit_print_elem(value, span)?;
        self.pop_rooted(Reg::X3); // the cursor
        self.emit_gc_load_ptr(Reg::X3, 8); // x0 = next (barriered)
        self.asm.mov_reg(Reg::X3, Reg::X0);
        self.push_rooted(Reg::X3);
        self.asm.branch(close, BranchKind::CompareZero(Reg::X3));
        self.asm.emit_write_rodata(STDOUT_FD, b", ");
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(close);
        self.pop_rooted(Reg::X3); // discard the cursor
        self.asm.emit_write_rodata(STDOUT_FD, b"]\n");
        Ok(())
    }

    /// println of the set in x0 in the evaluator's format: `%(e1, e2)`.
    fn emit_println_set(&mut self, elem: ListElem, span: Span) -> Result<(), Diagnostic> {
        let loop_start = self.asm.new_label();
        let close = self.asm.new_label();
        self.asm.push(Reg::X0);
        self.asm.emit_write_rodata(STDOUT_FD, b"%(");
        self.asm.pop(Reg::X3);
        self.asm.push(Reg::X3);
        self.asm.branch(close, BranchKind::CompareZero(Reg::X3));
        self.asm.bind(loop_start);
        self.asm.pop(Reg::X3);
        self.asm.push(Reg::X3);
        self.emit_gc_load_ptr(Reg::X3, 0); // x0 = element (barriered)
        // Scalar elements are boxed in the cell; unbox before printing
        // (string elements are already the pointer).
        if is_boxed_scalar(elem_value_type(elem)) {
            self.emit_unbox_scalar();
        }
        self.emit_print_elem(elem, span)?;
        self.asm.pop(Reg::X3);
        self.emit_gc_load_ptr(Reg::X3, 8); // x0 = next (barriered)
        self.asm.mov_reg(Reg::X3, Reg::X0);
        self.asm.push(Reg::X3);
        self.asm.branch(close, BranchKind::CompareZero(Reg::X3));
        self.asm.emit_write_rodata(STDOUT_FD, b", ");
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(close);
        self.asm.pop(Reg::X3);
        self.asm.emit_write_rodata(STDOUT_FD, b")\n");
        Ok(())
    }

    /// `bl`-called scalar membership scan: x0 = list, x1 = candidate
    /// (a raw Int/Bool qword) → x0 = Bool. No frame: it calls nothing.
    fn emit_member_scalar_routine(&mut self, label: Label) {
        self.asm.bind(label);
        self.asm.mov_reg(Reg::X2, Reg::X0); // cursor
        let loop_start = self.asm.new_label();
        let found = self.asm.new_label();
        let not_found = self.asm.new_label();
        self.asm.bind(loop_start);
        self.asm.branch(not_found, BranchKind::CompareZero(Reg::X2));
        // Both cell slots are heap pointers, so both reads are barriered
        // (x0/x8 clobbered, the cursor x2 and candidate x1 preserved). The
        // scalar inside the box is a plain load.
        self.emit_gc_load_ptr(Reg::X2, 0); // x0 = boxed element pointer
        self.asm.ldr_imm(Reg::X3, Reg::X0, 0); // unbox to the raw scalar
        self.asm.cmp_reg(Reg::X3, Reg::X1);
        self.asm.branch(found, BranchKind::Conditional(Cond::Eq));
        self.emit_gc_load_ptr(Reg::X2, 8); // x0 = next
        self.asm.mov_reg(Reg::X2, Reg::X0);
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(found);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.ret();
        self.asm.bind(not_found);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.ret();
    }

    /// `bl`-called string membership scan: x0 = list, x1 = candidate
    /// (a string pointer) → x0 = Bool. The candidate and cursor live
    /// in a 16-byte stack scratch addressed off x5, which the inlined
    /// `str_eq` (using x0-x4/x6/x7) never touches.
    fn emit_member_string_routine(&mut self, label: Label) {
        self.asm.bind(label);
        self.asm.sub_sp_imm(16);
        self.asm.add_reg_sp_imm(Reg::X5, 0); // scratch base
        self.asm.str_imm(Reg::X1, Reg::X5, 0); // candidate
        self.asm.mov_reg(Reg::X2, Reg::X0); // cursor
        let loop_start = self.asm.new_label();
        let found = self.asm.new_label();
        let not_found = self.asm.new_label();
        self.asm.bind(loop_start);
        self.asm.branch(not_found, BranchKind::CompareZero(Reg::X2));
        self.asm.str_imm(Reg::X2, Reg::X5, 8); // save cursor
        self.emit_gc_load_ptr(Reg::X2, 0); // x0 = element (barriered)
        self.asm.ldr_imm(Reg::X1, Reg::X5, 0); // candidate
        self.emit_str_eq();
        self.asm.branch(found, BranchKind::CompareNonZero(Reg::X0));
        self.asm.ldr_imm(Reg::X2, Reg::X5, 8); // restore cursor
        self.emit_gc_load_ptr(Reg::X2, 8); // x0 = next (barriered)
        self.asm.mov_reg(Reg::X2, Reg::X0);
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(found);
        self.asm.add_sp_imm(16);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.ret();
        self.asm.bind(not_found);
        self.asm.add_sp_imm(16);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.ret();
    }

    /// `bl`-called cons-list reverse: x0 = list → x0 = fresh reversed
    /// list. Needs a frame record because it allocates (which can
    /// `bl` the heap-grow routine).
    fn emit_list_reverse_routine(&mut self, label: Label) {
        self.asm.bind(label);
        self.asm.push_frame_record();
        self.asm.mov_imm64(Reg::X1, 0); // acc = nil
        let loop_start = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.bind(loop_start);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X0));
        // Both cell slots are heap pointers: read them through the barrier,
        // holding the cursor in x9 (which the barrier preserves).
        self.asm.mov_reg(Reg::X9, Reg::X0); // cursor
        self.emit_gc_load_ptr(Reg::X9, 0); // x0 = head
        self.asm.mov_reg(Reg::X2, Reg::X0); // head
        self.emit_gc_load_ptr(Reg::X9, 8); // x0 = next -- advance input
        // Build the reversed cell. All three live values -- the input cursor
        // (x0), the accumulator (x1) and the head being moved (x2) -- are
        // heap references that have to survive an allocation that can
        // collect, so each is parked as a root and taken back afterwards
        // (which also picks up their post-relocation addresses).
        self.push_rooted(Reg::X0);
        self.push_rooted(Reg::X1);
        self.push_rooted(Reg::X2);
        self.emit_gc_alloc_object(2, crate::gc_layout::GC_TYPE_POINTER_RECORD);
        self.asm.mov_reg(Reg::X3, Reg::X0); // the fresh cell
        self.pop_rooted(Reg::X2); // head
        self.pop_rooted(Reg::X1); // acc
        self.pop_rooted(Reg::X0); // cursor
        self.emit_color_ptr(Reg::X4, Reg::X2);
        self.asm.str_imm(Reg::X4, Reg::X3, 0); // [cell + 0] = head
        self.emit_color_ptr(Reg::X4, Reg::X1);
        self.asm.str_imm(Reg::X4, Reg::X3, 8); // [cell + 8] = next = acc
        self.asm.mov_reg(Reg::X1, Reg::X3); // acc = the new cell (raw)
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(done);
        self.asm.mov_reg(Reg::X0, Reg::X1);
        self.asm.pop_frame_record();
        self.asm.ret();
    }

    /// Print one element value in x0 without a newline; clobbers
    /// x0-x5/x16.
    fn emit_print_elem(&mut self, elem: ListElem, span: Span) -> Result<(), Diagnostic> {
        match elem {
            // Rendering a Double needs the shortest-round-trip formatter this
            // backend does not have yet; arithmetic and comparison on Doubles
            // (including inside a list) work regardless.
            ListElem::Double => {
                return Err(unsupported(span, "printing a Double element"));
            }
            ListElem::Enum(shape) => {
                self.emit_print_enum_inline(shape, span)?;
            }
            ListElem::Record(index) => {
                self.emit_print_record_inline(index, span)?;
            }
            // A nested element is a list: build its text with the same
            // renderer and write that. The recursion is at codegen time and
            // terminates because the depth decreases.
            ListElem::Nested { .. } => {
                let inner = elem
                    .nested_inner()
                    .expect("a nested element is a list of its inner element");
                self.emit_list_to_str(inner, "[", "]", span)?;
                self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
                self.asm.add_reg_imm(Reg::X1, Reg::X0, 8);
                self.asm.mov_imm64(Reg::X0, STDOUT_FD);
                self.asm.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
                self.asm.svc_0x80();
            }
            ListElem::Int => {
                self.asm.sub_sp_imm(32);
                self.asm.emit_int_digits(false);
                self.asm.add_reg_sp_imm(Reg::X2, 32);
                self.asm.sub_reg(Reg::X2, Reg::X2, Reg::X1);
                self.asm.mov_imm64(Reg::X0, STDOUT_FD);
                self.asm.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
                self.asm.svc_0x80();
                self.asm.add_sp_imm(32);
            }
            ListElem::Str => {
                self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
                self.asm.add_reg_imm(Reg::X1, Reg::X0, 8);
                self.asm.mov_imm64(Reg::X0, STDOUT_FD);
                self.asm.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
                self.asm.svc_0x80();
            }
            ListElem::Bool => {
                let false_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm
                    .branch(false_label, BranchKind::CompareZero(Reg::X0));
                self.asm.emit_write_rodata(STDOUT_FD, b"true");
                self.asm.branch(end_label, BranchKind::Unconditional);
                self.asm.bind(false_label);
                self.asm.emit_write_rodata(STDOUT_FD, b"false");
                self.asm.bind(end_label);
            }
        }
        Ok(())
    }

    /// println of the record object in x0 in the evaluator's format:
    /// `#Name(v1, v2)` for nominal records, `#(v1, v2)` for
    /// structural shapes. Scalar and string fields only for now.
    fn emit_println_record(&mut self, index: u32, span: Span) -> Result<(), Diagnostic> {
        self.emit_print_record_inline(index, span)?;
        self.asm.emit_write_rodata(STDOUT_FD, b"\n");
        Ok(())
    }

    /// The same rendering without the newline, which is what a record nested
    /// inside a list or another record needs. A record that (transitively)
    /// holds itself would print forever, so the stack of records being
    /// rendered is checked rather than recursed into.
    fn emit_print_record_inline(&mut self, index: u32, span: Span) -> Result<(), Diagnostic> {
        if self.printing_records.contains(&index) {
            return Err(unsupported(span, "printing a record that holds itself"));
        }
        self.printing_records.push(index);
        let result = self.emit_print_record_fields(index, span);
        self.printing_records.pop();
        result
    }

    fn emit_print_record_fields(&mut self, index: u32, span: Span) -> Result<(), Diagnostic> {
        let info = &self.records[index as usize];
        let opener = format!("#{}(", info.name);
        let fields = info.fields.clone();
        for (_, ty) in &fields {
            if list_elem_of(*ty).is_none() && !matches!(ty, ValueType::Nullable(_)) {
                return Err(unsupported(span, "printing a record with this field type"));
            }
        }
        self.asm.push(Reg::X0); // the object survives the writes
        self.asm.emit_write_rodata(STDOUT_FD, opener.as_bytes());
        for (position, (_, ty)) in fields.iter().enumerate() {
            if position > 0 {
                self.asm.emit_write_rodata(STDOUT_FD, b", ");
            }
            self.asm.pop(Reg::X0);
            self.asm.push(Reg::X0);
            self.emit_gc_load_ptr(Reg::X0, (position * 8) as u32); // barriered
            // A field that may not be there -- `Map#get`'s answer is the one
            // every map entry carries -- prints the word `null` when it is
            // not, which is what the evaluator prints.
            if let ValueType::Nullable(elem) = *ty {
                let null_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm
                    .branch(null_label, BranchKind::CompareZero(Reg::X0));
                if is_boxed_scalar(elem_value_type(elem)) {
                    self.emit_unbox_scalar();
                }
                self.emit_print_elem(elem, span)?;
                self.asm.branch(end_label, BranchKind::Unconditional);
                self.asm.bind(null_label);
                self.asm.emit_write_rodata(STDOUT_FD, b"null");
                self.asm.bind(end_label);
                continue;
            }
            // Scalar fields are boxed in the POINTER_RECORD; unbox before
            // printing (string fields are already the pointer).
            if is_boxed_scalar(*ty) {
                self.emit_unbox_scalar();
            }
            self.emit_print_elem(list_elem_of(*ty).expect("checked above"), span)?;
        }
        self.asm.pop(Reg::X0); // discard the saved object
        self.asm.emit_write_rodata(STDOUT_FD, b")");
        Ok(())
    }

    /// The evaluator's rendering of an enum value in x0 -- `Some(3)`, `None`,
    /// `Ok(1)` -- without a newline, so it reads the same alone, inside a list,
    /// or inside a record. The value is `[boxed tag][payload...]`, so this is a
    /// comparison per variant against the tag the constructor stored.
    fn emit_print_enum_inline(&mut self, shape: u32, span: Span) -> Result<(), Diagnostic> {
        if self.printing_enums.contains(&shape) {
            return Err(unsupported(span, "printing an enum that holds itself"));
        }
        self.printing_enums.push(shape);
        let result = self.emit_print_enum_variants(shape, span);
        self.printing_enums.pop();
        result
    }

    fn emit_print_enum_variants(&mut self, shape: u32, span: Span) -> Result<(), Diagnostic> {
        let (enum_index, payloads) = self.enum_shapes[shape as usize].clone();
        let variants = self.generic_enums[enum_index].1.clone();
        let end = self.asm.new_label();
        self.asm.push(Reg::X0); // the value survives the writes below
        for (tag, (variant, arity)) in variants.iter().enumerate() {
            // A payload this shape never carries means no value of it is this
            // variant, which is the same reading `match` takes of `Never`.
            if payloads[tag].contains(&ValueType::Never) {
                continue;
            }
            let next = self.asm.new_label();
            self.asm.pop(Reg::X0);
            self.asm.push(Reg::X0);
            self.emit_gc_load_ptr(Reg::X0, 0); // the boxed tag
            self.emit_unbox_scalar();
            self.asm.cmp_imm(Reg::X0, tag as u32);
            self.asm.branch(next, BranchKind::Conditional(Cond::Ne));
            self.asm.emit_write_rodata(STDOUT_FD, variant.as_bytes());
            if *arity > 0 {
                self.asm.emit_write_rodata(STDOUT_FD, b"(");
                let fields = payloads[tag].clone();
                for (position, field_ty) in fields.iter().copied().take(*arity).enumerate() {
                    if position > 0 {
                        self.asm.emit_write_rodata(STDOUT_FD, b", ");
                    }
                    self.asm.pop(Reg::X0);
                    self.asm.push(Reg::X0);
                    self.emit_gc_load_ptr(Reg::X0, (position as u32 + 1) * 8);
                    if is_boxed_scalar(field_ty) {
                        self.emit_unbox_scalar();
                    }
                    match field_ty {
                        ValueType::Enum(inner) => self.emit_print_enum_inline(inner, span)?,
                        ValueType::Record(inner) => self.emit_print_record_inline(inner, span)?,
                        other => {
                            let Some(elem) = list_elem_of(other) else {
                                return Err(unsupported(
                                    span,
                                    "printing an enum payload of this type",
                                ));
                            };
                            self.emit_print_elem(elem, span)?;
                        }
                    }
                }
                self.asm.emit_write_rodata(STDOUT_FD, b")");
            }
            self.asm.branch(end, BranchKind::Unconditional);
            self.asm.bind(next);
        }
        self.asm.bind(end);
        self.asm.pop(Reg::X0); // discard the saved value
        Ok(())
    }

    /// println of the list in x0 in the evaluator's format:
    /// `[e1, e2, ...]` (strings unquoted).
    fn emit_println_list(&mut self, elem: ListElem, span: Span) -> Result<(), Diagnostic> {
        let loop_start = self.asm.new_label();
        let close = self.asm.new_label();
        self.asm.push(Reg::X0); // cursor survives the writes below
        self.asm.emit_write_rodata(STDOUT_FD, b"[");
        self.asm.pop(Reg::X3);
        self.asm.push(Reg::X3);
        self.asm.branch(close, BranchKind::CompareZero(Reg::X3));
        self.asm.bind(loop_start);
        self.asm.pop(Reg::X3);
        self.asm.push(Reg::X3);
        self.emit_gc_load_ptr(Reg::X3, 0); // x0 = element (barriered)
        // Scalar elements are boxed in the cell; unbox before printing
        // (string elements are already the pointer).
        if is_boxed_scalar(elem_value_type(elem)) {
            self.emit_unbox_scalar();
        }
        self.emit_print_elem(elem, span)?;
        self.asm.pop(Reg::X3);
        self.emit_gc_load_ptr(Reg::X3, 8); // x0 = next (barriered)
        self.asm.mov_reg(Reg::X3, Reg::X0);
        self.asm.push(Reg::X3);
        self.asm.branch(close, BranchKind::CompareZero(Reg::X3));
        self.asm.emit_write_rodata(STDOUT_FD, b", ");
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(close);
        self.asm.pop(Reg::X3); // discard the cursor slot
        self.asm.emit_write_rodata(STDOUT_FD, b"]\n");
        Ok(())
    }

    /// println of the string object in x0: one write for the payload,
    /// one for the newline.
    fn emit_println_str(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
        self.asm.add_reg_imm(Reg::X1, Reg::X0, 8);
        self.asm.mov_imm64(Reg::X0, STDOUT_FD);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
        self.asm.svc_0x80();
        self.asm.emit_write_rodata(STDOUT_FD, b"\n");
    }

    /// The shared heap-grow routine: mmap a fresh segment into
    /// x19/x20. Preserves x0-x5 because allocation sites have live
    /// String / display builtins, mirroring the C backend's surface.
    /// Returns Ok(None) when `name` is not a builtin.
    fn builtin_call(
        &mut self,
        name: &str,
        arguments: &[Expr],
        span: Span,
    ) -> Result<Option<ValueType>, Diagnostic> {
        match (name, arguments.len()) {
            ("length", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "length of a non-string"));
                }
                self.emit_str_char_count();
                Ok(Some(ValueType::Int))
            }
            ("isEmptyString", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "isEmptyString of a non-string"));
                }
                self.asm.ldr_imm(Reg::X1, Reg::X0, 0);
                self.asm.cmp_imm(Reg::X1, 0);
                // Reuse the comparison: x0 = (byte length == 0).
                self.asm.cset(Reg::X0, Cond::Eq);
                Ok(Some(ValueType::Bool))
            }
            ("toString", 1) => {
                match self.expression(&arguments[0])? {
                    ValueType::Int => self.emit_int_to_str(),
                    ValueType::Bool => self.emit_bool_to_str(),
                    ValueType::Str => {}
                    other => {
                        return Err(unsupported(span, &format!("toString of {other:?}")));
                    }
                }
                Ok(Some(ValueType::Str))
            }
            ("toUpperCase", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "toUpperCase of a non-string"));
                }
                self.emit_str_ascii_case(true);
                Ok(Some(ValueType::Str))
            }
            ("toLowerCase", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "toLowerCase of a non-string"));
                }
                self.emit_str_ascii_case(false);
                Ok(Some(ValueType::Str))
            }
            ("reverse", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "reverse of a non-string"));
                }
                self.emit_str_reverse();
                Ok(Some(ValueType::Str))
            }
            ("trim", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "trim of a non-string"));
                }
                self.emit_str_trim(true, true);
                Ok(Some(ValueType::Str))
            }
            ("trimLeft", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "trimLeft of a non-string"));
                }
                self.emit_str_trim(true, false);
                Ok(Some(ValueType::Str))
            }
            ("trimRight", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "trimRight of a non-string"));
                }
                self.emit_str_trim(false, true);
                Ok(Some(ValueType::Str))
            }
            ("substring", 3) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "substring of a non-string"));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Int {
                    return Err(unsupported(span, "substring with a non-Int start"));
                }
                self.asm.push(Reg::X0);
                if self.expression(&arguments[2])? != ValueType::Int {
                    return Err(unsupported(span, "substring with a non-Int end"));
                }
                self.asm.mov_reg(Reg::X2, Reg::X0);
                self.asm.pop(Reg::X1);
                self.pop_rooted(Reg::X0);
                self.emit_substring();
                Ok(Some(ValueType::Str))
            }
            ("at", 2) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "at of a non-string"));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Int {
                    return Err(unsupported(span, "at with a non-Int index"));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.pop_rooted(Reg::X0);
                // at(s, i) = substring(s, i, i + 1)
                self.asm.add_reg_imm(Reg::X2, Reg::X1, 1);
                self.emit_substring();
                Ok(Some(ValueType::Str))
            }
            // `println` in expression position, which is where it lands when it
            // is the last thing in a block -- the body of a timed or mapped
            // block, say. It answers with unit, like every other statement.
            ("println", 1) => {
                self.println_call(arguments, span)?;
                self.asm.mov_imm64(Reg::X0, 0);
                Ok(Some(ValueType::Unit))
            }
            // The same line on standard error.
            ("printlnError", 1) => {
                if let Some(line) = Self::literal_line(&arguments[0])? {
                    self.asm.emit_write_rodata(STDERR_FD, line.as_bytes());
                    self.asm.mov_imm64(Reg::X0, 0);
                    return Ok(Some(ValueType::Unit));
                }
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(
                        arguments[0].span(),
                        "printing this to standard error",
                    ));
                }
                self.asm.ldr_imm(Reg::X2, Reg::X0, 0); // length
                self.asm.add_reg_imm(Reg::X1, Reg::X0, 8); // bytes
                self.asm.mov_imm64(Reg::X0, STDERR_FD);
                self.asm.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
                self.asm.svc_0x80();
                self.asm.emit_write_rodata(STDERR_FD, b"\n");
                self.asm.mov_imm64(Reg::X0, 0);
                Ok(Some(ValueType::Unit))
            }
            ("assert", 1) => {
                if self.expression(&arguments[0])? != ValueType::Bool {
                    return Err(unsupported(span, "assert of a non-Bool"));
                }
                let ok = self.asm.new_label();
                self.asm.branch(ok, BranchKind::CompareNonZero(Reg::X0));
                self.asm
                    .emit_write_rodata(STDERR_FD, b"klassic: assertion failed\n");
                self.asm.emit_exit(1);
                self.asm.bind(ok);
                Ok(Some(ValueType::Unit))
            }
            ("matches", 2) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "matches on a non-string"));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Str {
                    return Err(unsupported(span, "matches with a non-string pattern"));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.pop_rooted(Reg::X0);
                self.emit_str_matches();
                Ok(Some(ValueType::Bool))
            }
            // `String#isInt` / `String#isDouble`: folded when the text is known
            // while compiling (Rust's own parse decides, so the answer is the
            // evaluator's by construction), scanned when it is not.
            ("String#isInt", 1) | ("String#isDouble", 1) => {
                if let Some(text) = self.literal_string(&arguments[0]) {
                    let answer = if name == "String#isInt" {
                        text.trim().parse::<i64>().is_ok()
                    } else {
                        text.trim().parse::<f64>().is_ok()
                    };
                    self.asm.mov_imm64(Reg::X0, u64::from(answer));
                    return Ok(Some(ValueType::Bool));
                }
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "a string test of a non-string"));
                }
                if name == "String#isInt" {
                    self.emit_str_is_int();
                } else {
                    self.emit_str_is_double();
                }
                Ok(Some(ValueType::Bool))
            }
            ("indexOf", 2) | ("lastIndexOf", 2) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "indexOf of a non-string"));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Str {
                    return Err(unsupported(span, "indexOf with a non-string needle"));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.pop_rooted(Reg::X0);
                self.emit_str_index_of(name == "lastIndexOf");
                Ok(Some(ValueType::Int))
            }
            // `repeat(s, n)`: n copies of s, built by appending. A count that
            // is not positive answers with the empty string, which is what
            // `substring`'s clamping already does with an out-of-range index.
            ("repeat", 2) => {
                let mark = self.open_temp_roots();
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "repeat of a non-string"));
                }
                let source = self.reserve_locals(24);
                let acc = source + 8;
                let count = acc + 8;
                self.asm.store_local(Reg::X0, source);
                self.emit_root_frame_slot(source);
                let empty = self.asm.intern_string_object("");
                self.asm.load_rodata_address(Reg::X0, empty);
                self.emit_heap_string_copy();
                self.asm.store_local(Reg::X0, acc);
                self.emit_root_frame_slot(acc);
                if self.expression(&arguments[1])? != ValueType::Int {
                    return Err(unsupported(span, "repeat with a non-Int count"));
                }
                self.asm.store_local(Reg::X0, count);
                let loop_start = self.asm.new_label();
                let done = self.asm.new_label();
                self.asm.bind(loop_start);
                self.asm.load_local(Reg::X0, count);
                self.asm.cmp_imm(Reg::X0, 0);
                self.asm.branch(done, BranchKind::Conditional(Cond::Le));
                self.asm.load_local(Reg::X1, source);
                self.asm.load_local(Reg::X0, acc);
                self.emit_str_concat();
                self.asm.store_local(Reg::X0, acc);
                self.asm.load_local(Reg::X0, count);
                self.asm.sub_reg_imm(Reg::X0, Reg::X0, 1);
                self.asm.store_local(Reg::X0, count);
                self.asm.branch(loop_start, BranchKind::Unconditional);
                self.asm.bind(done);
                self.asm.load_local(Reg::X0, acc);
                self.close_temp_roots(mark);
                Ok(Some(ValueType::Str))
            }
            // `replace(s, from, to)`: the *first* occurrence, which is what the
            // evaluator's `replacen(from, to, 1)` does. `replaceAll` is the
            // pattern-matching one and lives elsewhere.
            ("replace", 3) => {
                let mark = self.open_temp_roots();
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "replace of a non-string"));
                }
                let source = self.reserve_locals(40);
                let from = source + 8;
                let to = from + 8;
                let head = to + 8;
                let at = head + 8;
                self.asm.store_local(Reg::X0, source);
                self.emit_root_frame_slot(source);
                // Every slot that will hold a string is rooted before the
                // branch below, so both paths have pushed the same roots.
                self.asm.mov_imm64(Reg::X0, 0);
                self.asm.store_local(Reg::X0, from);
                self.emit_root_frame_slot(from);
                self.asm.mov_imm64(Reg::X0, 0);
                self.asm.store_local(Reg::X0, to);
                self.emit_root_frame_slot(to);
                self.asm.mov_imm64(Reg::X0, 0);
                self.asm.store_local(Reg::X0, head);
                self.emit_root_frame_slot(head);
                if self.expression(&arguments[1])? != ValueType::Str {
                    return Err(unsupported(span, "replace with a non-string pattern"));
                }
                self.asm.store_local(Reg::X0, from);
                if self.expression(&arguments[2])? != ValueType::Str {
                    return Err(unsupported(span, "replace with a non-string replacement"));
                }
                self.asm.store_local(Reg::X0, to);
                self.asm.load_local(Reg::X1, from);
                self.asm.load_local(Reg::X0, source);
                self.emit_str_index_of(false);
                self.asm.store_local(Reg::X0, at);
                let absent = self.asm.new_label();
                let end = self.asm.new_label();
                self.asm.cmp_imm(Reg::X0, 0);
                self.asm.branch(absent, BranchKind::Conditional(Cond::Lt));
                // head = substring(source, 0, at)
                self.asm.load_local(Reg::X2, at);
                self.asm.mov_imm64(Reg::X1, 0);
                self.asm.load_local(Reg::X0, source);
                self.emit_substring();
                self.asm.store_local(Reg::X0, head);
                // head = head + to
                self.asm.load_local(Reg::X1, to);
                self.asm.load_local(Reg::X0, head);
                self.emit_str_concat();
                self.asm.store_local(Reg::X0, head);
                // tail = substring(source, at + len(from), len(source))
                self.asm.load_local(Reg::X0, from);
                self.asm.ldr_imm(Reg::X1, Reg::X0, 0);
                self.asm.load_local(Reg::X0, at);
                self.asm.add_reg(Reg::X1, Reg::X0, Reg::X1);
                self.asm.load_local(Reg::X0, source);
                self.asm.ldr_imm(Reg::X2, Reg::X0, 0);
                self.emit_substring();
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.asm.load_local(Reg::X0, head);
                self.emit_str_concat();
                self.asm.branch(end, BranchKind::Unconditional);
                self.asm.bind(absent);
                self.asm.load_local(Reg::X0, source);
                self.asm.bind(end);
                self.close_temp_roots(mark);
                Ok(Some(ValueType::Str))
            }
            // `Random#seed(n)` / `Random#nextInt(bound)`: the evaluator's
            // generator, which is a 64-bit LCG whose high half is the answer.
            // The same constants and the same shift, so a seeded program
            // prints the same numbers here as it does there.
            ("Random#seed", 1) => {
                if self.expression(&arguments[0])? != ValueType::Int {
                    return Err(unsupported(span, "Random#seed of a non-Int"));
                }
                let cell = self.random_state_cell();
                self.asm.load_data_address(Reg::X1, cell);
                self.asm.str_imm(Reg::X0, Reg::X1, 0);
                Ok(Some(ValueType::Unit))
            }
            ("Random#nextInt", 1) => {
                if self.expression(&arguments[0])? != ValueType::Int {
                    return Err(unsupported(span, "Random#nextInt of a non-Int"));
                }
                let cell = self.random_state_cell();
                self.asm.mov_reg(Reg::X4, Reg::X0); // bound
                self.asm.load_data_address(Reg::X1, cell);
                self.asm.ldr_imm(Reg::X0, Reg::X1, 0);
                self.asm.mov_imm64(Reg::X2, 6_364_136_223_846_793_005);
                self.asm.mul_reg(Reg::X0, Reg::X0, Reg::X2);
                self.asm.mov_imm64(Reg::X2, 1_442_695_040_888_963_407);
                self.asm.add_reg(Reg::X0, Reg::X0, Reg::X2);
                self.asm.str_imm(Reg::X0, Reg::X1, 0);
                self.asm.lsr_imm(Reg::X0, Reg::X0, 32);
                self.asm.sdiv_reg(Reg::X2, Reg::X0, Reg::X4);
                self.asm.msub_reg(Reg::X0, Reg::X2, Reg::X4, Reg::X0);
                Ok(Some(ValueType::Int))
            }
            // The `Math#*` integer helpers, by the evaluator's own algorithms:
            // Euclid on the absolute values, square-and-multiply, and Newton's
            // iteration. A negative exponent or radicand is a runtime error in
            // the evaluator; here they answer 1 and 0, since this backend has
            // no way to raise one.
            ("Math#gcd", 2) => {
                if self.expression(&arguments[0])? != ValueType::Int {
                    return Err(unsupported(span, "Math#gcd of a non-Int"));
                }
                self.asm.push(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Int {
                    return Err(unsupported(span, "Math#gcd of a non-Int"));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.asm.pop(Reg::X0);
                for reg in [Reg::X0, Reg::X1] {
                    let non_negative = self.asm.new_label();
                    self.asm.cmp_imm(reg, 0);
                    self.asm
                        .branch(non_negative, BranchKind::Conditional(Cond::Ge));
                    self.asm.mov_imm64(Reg::X2, 0);
                    self.asm.sub_reg(reg, Reg::X2, reg);
                    self.asm.bind(non_negative);
                }
                let loop_start = self.asm.new_label();
                let done = self.asm.new_label();
                self.asm.bind(loop_start);
                self.asm.branch(done, BranchKind::CompareZero(Reg::X1));
                self.asm.sdiv_reg(Reg::X2, Reg::X0, Reg::X1);
                self.asm.msub_reg(Reg::X2, Reg::X2, Reg::X1, Reg::X0); // a % b
                self.asm.mov_reg(Reg::X0, Reg::X1);
                self.asm.mov_reg(Reg::X1, Reg::X2);
                self.asm.branch(loop_start, BranchKind::Unconditional);
                self.asm.bind(done);
                Ok(Some(ValueType::Int))
            }
            ("Math#powInt", 2) => {
                if self.expression(&arguments[0])? != ValueType::Int {
                    return Err(unsupported(span, "Math#powInt of a non-Int"));
                }
                self.asm.push(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Int {
                    return Err(unsupported(span, "Math#powInt of a non-Int"));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0); // exponent
                self.asm.pop(Reg::X2); // base
                self.asm.mov_imm64(Reg::X0, 1); // result
                let loop_start = self.asm.new_label();
                let done = self.asm.new_label();
                let skip = self.asm.new_label();
                self.asm.bind(loop_start);
                self.asm.cmp_imm(Reg::X1, 0);
                self.asm.branch(done, BranchKind::Conditional(Cond::Le));
                self.asm.mov_imm64(Reg::X3, 1);
                self.asm.tst_reg(Reg::X1, Reg::X3);
                self.asm.branch(skip, BranchKind::Conditional(Cond::Eq));
                self.asm.mul_reg(Reg::X0, Reg::X0, Reg::X2);
                self.asm.bind(skip);
                self.asm.lsr_imm(Reg::X1, Reg::X1, 1);
                self.asm.mul_reg(Reg::X2, Reg::X2, Reg::X2);
                self.asm.branch(loop_start, BranchKind::Unconditional);
                self.asm.bind(done);
                Ok(Some(ValueType::Int))
            }
            ("Math#sqrtInt", 1) => {
                if self.expression(&arguments[0])? != ValueType::Int {
                    return Err(unsupported(span, "Math#sqrtInt of a non-Int"));
                }
                let done = self.asm.new_label();
                let positive = self.asm.new_label();
                self.asm.mov_reg(Reg::X1, Reg::X0); // n
                self.asm.cmp_imm(Reg::X1, 0);
                self.asm.branch(positive, BranchKind::Conditional(Cond::Gt));
                self.asm.mov_imm64(Reg::X0, 0);
                self.asm.branch(done, BranchKind::Unconditional);
                self.asm.bind(positive);
                // x = n; y = (x + 1) / 2; while y < x { x = y; y = (x + n/x)/2 }
                self.asm.mov_reg(Reg::X0, Reg::X1); // x
                self.asm.add_reg_imm(Reg::X2, Reg::X0, 1);
                self.asm.mov_imm64(Reg::X3, 2);
                self.asm.sdiv_reg(Reg::X2, Reg::X2, Reg::X3); // y
                let loop_start = self.asm.new_label();
                self.asm.bind(loop_start);
                self.asm.cmp_reg(Reg::X2, Reg::X0);
                self.asm.branch(done, BranchKind::Conditional(Cond::Ge));
                self.asm.mov_reg(Reg::X0, Reg::X2); // x = y
                self.asm.sdiv_reg(Reg::X4, Reg::X1, Reg::X0); // n / x
                self.asm.add_reg(Reg::X2, Reg::X0, Reg::X4);
                self.asm.mov_imm64(Reg::X3, 2);
                self.asm.sdiv_reg(Reg::X2, Reg::X2, Reg::X3);
                self.asm.branch(loop_start, BranchKind::Unconditional);
                self.asm.bind(done);
                Ok(Some(ValueType::Int))
            }
            ("startsWith", 2) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "startsWith of a non-string"));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Str {
                    return Err(unsupported(span, "startsWith with a non-string prefix"));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.pop_rooted(Reg::X0);
                self.emit_str_starts_with();
                Ok(Some(ValueType::Bool))
            }
            ("endsWith", 2) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "endsWith of a non-string"));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Str {
                    return Err(unsupported(span, "endsWith with a non-string suffix"));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.pop_rooted(Reg::X0);
                self.emit_str_ends_with();
                Ok(Some(ValueType::Bool))
            }
            ("join", 2) => {
                let list_ty = self.expression(&arguments[0])?;
                if !matches!(
                    list_ty,
                    ValueType::List(ListElem::Str) | ValueType::EmptyList
                ) {
                    return Err(unsupported(span, "join of a non-string list"));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Str {
                    return Err(unsupported(span, "join with a non-string separator"));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.pop_rooted(Reg::X0);
                self.emit_str_join();
                Ok(Some(ValueType::Str))
            }
            ("replaceAll", 3) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "replaceAll of a non-string"));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Str {
                    return Err(unsupported(span, "replaceAll with a non-string pattern"));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[2])? != ValueType::Str {
                    return Err(unsupported(
                        span,
                        "replaceAll with a non-string replacement",
                    ));
                }
                self.asm.mov_reg(Reg::X2, Reg::X0);
                self.pop_rooted(Reg::X1);
                self.pop_rooted(Reg::X0);
                self.emit_str_replace_all();
                Ok(Some(ValueType::Str))
            }
            ("split", 2) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "split of a non-string"));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Str {
                    return Err(unsupported(span, "split with a non-string delimiter"));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.pop_rooted(Reg::X0);
                self.emit_str_split();
                Ok(Some(ValueType::List(ListElem::Str)))
            }
            ("head", 1) => {
                let ty = self.expression(&arguments[0])?;
                let ValueType::List(elem) = ty else {
                    return Err(unsupported(span, "head of a non-list"));
                };
                let non_empty = self.asm.new_label();
                self.asm
                    .branch(non_empty, BranchKind::CompareNonZero(Reg::X0));
                self.asm
                    .emit_write_rodata(STDERR_FD, b"klassic: head expects a non-empty list\n");
                self.asm.emit_exit(1);
                self.asm.bind(non_empty);
                self.emit_gc_load_ptr(Reg::X0, 0); // barriered: head is a pointer
                // Scalar heads are boxed in the cell; unbox before use
                // (string/list/record heads are already the pointer).
                if is_boxed_scalar(elem_value_type(elem)) {
                    self.emit_unbox_scalar();
                }
                Ok(Some(elem_value_type(elem)))
            }
            ("tail", 1) => {
                let ty = self.expression(&arguments[0])?;
                if !matches!(ty, ValueType::List(_) | ValueType::EmptyList) {
                    return Err(unsupported(span, "tail of a non-list"));
                }
                // The evaluator's tail([]) is [] — nil stays nil.
                let end = self.asm.new_label();
                self.asm.branch(end, BranchKind::CompareZero(Reg::X0));
                self.emit_gc_load_ptr(Reg::X0, 8); // barriered: next is a pointer
                self.asm.bind(end);
                Ok(Some(ty))
            }
            // `Map#empty()`: the empty chain, which is the null pointer.
            ("Map#empty", 0) => {
                self.asm.mov_imm64(Reg::X0, 0);
                Ok(Some(ValueType::EmptyMap))
            }
            // `Map#put(m, key, value)`: a fresh map with that binding. An
            // existing key keeps its position and takes the new value; a new
            // one goes on the end, which is the evaluator's own order.
            ("Map#put", 3) => {
                let mark = self.open_temp_roots();
                let map_ty = self.expression(&arguments[0])?;
                let existing = match map_ty {
                    ValueType::Map(key, value) => Some((key, value)),
                    ValueType::EmptyMap => None,
                    _ => return Err(unsupported(span, "a map builtin on a non-map")),
                };
                let cursor = self.reserve_locals(24);
                let acc = cursor + 8;
                let replaced = cursor + 16;
                self.asm.store_local(Reg::X0, cursor);
                self.emit_root_frame_slot(cursor);
                self.asm.mov_imm64(Reg::X0, 0);
                self.asm.store_local(Reg::X0, acc);
                self.emit_root_frame_slot(acc);
                self.asm.store_local(Reg::X0, replaced);

                // The key and value are rooted for the whole walk: every entry
                // copied along the way allocates.
                let key_ty = self.expression(&arguments[1])?;
                let Some(key_elem) = list_elem_of(key_ty) else {
                    return Err(unsupported(arguments[1].span(), "a map key of this type"));
                };
                if let Some((existing_key, _)) = existing
                    && existing_key != key_elem
                {
                    return Err(unsupported(arguments[1].span(), "mixed map key types"));
                }
                if is_boxed_scalar(key_ty) {
                    self.emit_box_scalar();
                }
                let key_slot = self.reserve_locals(16);
                let value_slot = key_slot + 8;
                self.asm.store_local(Reg::X0, key_slot);
                self.emit_root_frame_slot(key_slot);
                let value_ty = self.expression(&arguments[2])?;
                let Some(value_elem) = list_elem_of(value_ty) else {
                    return Err(unsupported(arguments[2].span(), "a map value of this type"));
                };
                if let Some((_, existing_value)) = existing
                    && existing_value != value_elem
                {
                    return Err(unsupported(arguments[2].span(), "mixed map value types"));
                }
                if is_boxed_scalar(value_ty) {
                    self.emit_box_scalar();
                }
                self.asm.store_local(Reg::X0, value_slot);
                self.emit_root_frame_slot(value_slot);

                let loop_start = self.asm.new_label();
                let done = self.asm.new_label();
                let keep = self.asm.new_label();
                let next = self.asm.new_label();
                self.asm.bind(loop_start);
                self.asm.load_local(Reg::X0, cursor);
                self.asm.branch(done, BranchKind::CompareZero(Reg::X0));
                self.emit_gc_load_ptr(Reg::X0, 0); // this entry
                self.push_rooted(Reg::X0);
                self.emit_gc_load_ptr(Reg::X0, 0); // its key
                if is_boxed_scalar(elem_value_type(key_elem)) {
                    self.emit_unbox_scalar();
                }
                self.asm.load_local(Reg::X1, key_slot);
                match key_elem {
                    ListElem::Str => self.emit_str_eq(),
                    _ => {
                        if is_boxed_scalar(elem_value_type(key_elem)) {
                            self.asm.ldr_imm(Reg::X1, Reg::X1, 0);
                        }
                        self.asm.cmp_reg(Reg::X0, Reg::X1);
                        self.asm.cset(Reg::X0, Cond::Eq);
                    }
                }
                self.asm.branch(keep, BranchKind::CompareZero(Reg::X0));
                self.pop_rooted(Reg::X1); // the old entry, replaced
                self.asm.mov_imm64(Reg::X0, 1);
                self.asm.store_local(Reg::X0, replaced);
                self.emit_put_entry(key_slot, value_slot, acc);
                self.asm.branch(next, BranchKind::Unconditional);
                self.asm.bind(keep);
                self.pop_rooted(Reg::X0); // the entry, kept as it is
                self.push_rooted(Reg::X0);
                self.asm.load_local(Reg::X0, acc);
                self.emit_cons_cell();
                self.asm.store_local(Reg::X0, acc);
                self.asm.bind(next);
                self.asm.load_local(Reg::X0, cursor);
                self.emit_gc_load_ptr(Reg::X0, 8);
                self.asm.store_local(Reg::X0, cursor);
                self.asm.branch(loop_start, BranchKind::Unconditional);
                self.asm.bind(done);
                // A key that was not there goes on the end, which is the front
                // of the accumulator before it is reversed.
                let appended = self.asm.new_label();
                self.asm.load_local(Reg::X0, replaced);
                self.asm
                    .branch(appended, BranchKind::CompareNonZero(Reg::X0));
                self.emit_put_entry(key_slot, value_slot, acc);
                self.asm.bind(appended);
                self.asm.load_local(Reg::X0, acc);
                let reverse = self.reverse_label();
                self.asm.branch(reverse, BranchKind::Link);
                self.close_temp_roots(mark);
                Ok(Some(ValueType::Map(key_elem, value_elem)))
            }
            // `Map#keys(m)` / `Map#values(m)`: one half of every entry, in
            // insertion order.
            ("Map#keys", 1) | ("Map#values", 1) => {
                let mark = self.open_temp_roots();
                let map_ty = self.expression(&arguments[0])?;
                let (key_elem, value_elem) = match map_ty {
                    ValueType::Map(key, value) => (key, value),
                    ValueType::EmptyMap => {
                        self.close_temp_roots(mark);
                        self.asm.mov_imm64(Reg::X0, 0);
                        return Ok(Some(ValueType::EmptyList));
                    }
                    _ => return Err(unsupported(span, "a map builtin on a non-map")),
                };
                let wants_keys = name.ends_with("keys");
                let field = if wants_keys { 0 } else { 8 };
                let elem = if wants_keys { key_elem } else { value_elem };
                let cursor = self.reserve_locals(16);
                let acc = cursor + 8;
                self.asm.store_local(Reg::X0, cursor);
                self.emit_root_frame_slot(cursor);
                self.asm.mov_imm64(Reg::X0, 0);
                self.asm.store_local(Reg::X0, acc);
                self.emit_root_frame_slot(acc);
                let loop_start = self.asm.new_label();
                let done = self.asm.new_label();
                self.asm.bind(loop_start);
                self.asm.load_local(Reg::X0, cursor);
                self.asm.branch(done, BranchKind::CompareZero(Reg::X0));
                self.emit_gc_load_ptr(Reg::X0, 0); // entry
                self.emit_gc_load_ptr(Reg::X0, field); // its key or value
                self.push_rooted(Reg::X0);
                self.asm.load_local(Reg::X0, acc);
                self.emit_cons_cell();
                self.asm.store_local(Reg::X0, acc);
                self.asm.load_local(Reg::X0, cursor);
                self.emit_gc_load_ptr(Reg::X0, 8);
                self.asm.store_local(Reg::X0, cursor);
                self.asm.branch(loop_start, BranchKind::Unconditional);
                self.asm.bind(done);
                self.asm.load_local(Reg::X0, acc);
                let reverse = self.reverse_label();
                self.asm.branch(reverse, BranchKind::Link);
                self.close_temp_roots(mark);
                Ok(Some(ValueType::List(elem)))
            }
            ("isEmpty", 1) | ("Map#isEmpty", 1) | ("Set#isEmpty", 1) => {
                let ty = self.expression(&arguments[0])?;
                if !matches!(
                    ty,
                    ValueType::List(_)
                        | ValueType::EmptyList
                        | ValueType::Set(_)
                        | ValueType::EmptySet
                        | ValueType::Map(..)
                        | ValueType::EmptyMap
                ) {
                    return Err(unsupported(span, "isEmpty of a non-collection"));
                }
                self.asm.cmp_imm(Reg::X0, 0);
                self.asm.cset(Reg::X0, Cond::Eq);
                Ok(Some(ValueType::Bool))
            }
            // `xs.map(f)`: the method spelling is dispatched as a builtin
            // call, so it lands here rather than in the two-argument and
            // curried forms the function spelling matches.
            ("map", 2) => {
                // Declining rather than failing here matters: a `Functor`
                // instance declares its own `map(f, xs)`, and that call has to
                // reach the function resolution instead of stopping at the
                // builtin table.
                if self.lambda_argument(&arguments[0]).is_some() {
                    return Ok(None);
                }
                let Some((params, body)) = self.lambda_argument(&arguments[1]) else {
                    return Ok(None);
                };
                if params.len() != 1 {
                    return Err(unsupported(span, "mapping with a function of this arity"));
                }
                Ok(Some(self.emit_list_map(
                    &arguments[0],
                    &params[0],
                    &body,
                    span,
                )?))
            }
            ("size", 1) | ("Map#size", 1) | ("Set#size", 1) => {
                let ty = self.expression(&arguments[0])?;
                if !matches!(
                    ty,
                    ValueType::List(_)
                        | ValueType::EmptyList
                        | ValueType::Set(_)
                        | ValueType::EmptySet
                        | ValueType::Map(..)
                        | ValueType::EmptyMap
                ) {
                    return Err(unsupported(span, "size of a non-collection"));
                }
                // Lists, sets and maps are all cons-cell chains, so the
                // length walk is the same one.
                self.emit_chain_length();
                Ok(Some(ValueType::Int))
            }
            // The `Map#*` builtins the stdlib's `std.map` extension methods
            // delegate to. A map is a chain of `[key][value]` entries, so
            // looking one up is a linear scan and its size is the same walk a
            // list's is.
            // `m.getOrElse(key, fallback)`: the value if the key is there, the
            // fallback otherwise -- so unlike `get` this answers with the
            // value's own type. The fallback is only evaluated when it is
            // needed, which is what the stdlib's own definition does.
            ("getOrElse", 3) | ("Map#getOrElse", 3) => {
                let map_ty = self.expression(&arguments[0])?;
                let (key_elem, value_elem) = match map_ty {
                    ValueType::Map(key, value) => (key, value),
                    ValueType::EmptyMap => {
                        self.expression(&arguments[1])?;
                        return Ok(Some(self.expression(&arguments[2])?));
                    }
                    _ => return Err(unsupported(span, "a map builtin on a non-map")),
                };
                self.push_rooted(Reg::X0);
                let key_ty = self.expression(&arguments[1])?;
                if list_elem_of(key_ty) != Some(key_elem) {
                    return Err(unsupported(
                        arguments[1].span(),
                        "a key of a different type than the map's keys",
                    ));
                }
                if is_boxed_scalar(key_ty) {
                    self.emit_box_scalar();
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.pop_rooted(Reg::X0);
                self.emit_map_lookup(key_elem);
                let absent = self.asm.new_label();
                let end = self.asm.new_label();
                let value_ty = elem_value_type(value_elem);
                self.asm.branch(absent, BranchKind::CompareZero(Reg::X0));
                self.emit_gc_load_ptr(Reg::X0, 8);
                if is_boxed_scalar(value_ty) {
                    self.emit_unbox_scalar();
                }
                self.asm.branch(end, BranchKind::Unconditional);
                self.asm.bind(absent);
                let fallback_ty = self.expression(&arguments[2])?;
                if !assignable(fallback_ty, value_ty) {
                    return Err(unsupported(
                        arguments[2].span(),
                        "a fallback of a different type than the map's values",
                    ));
                }
                self.asm.bind(end);
                Ok(Some(value_ty))
            }
            // `CommandLine#args()`: the arguments this binary was run with,
            // as a list of strings, taken from the `argv` the entry point
            // stashed. Skips element 0, the program name, like the
            // evaluator's script arguments do.
            ("CommandLine#args", 0) => {
                let acc = self.reserve_locals(8);
                let mark = self.open_temp_roots();
                self.asm.mov_imm64(Reg::X0, 0); // nil
                self.asm.store_local(Reg::X0, acc);
                self.emit_root_frame_slot(acc);
                let index = self.reserve_locals(8);
                self.asm.mov_imm64(Reg::X0, 1);
                self.asm.store_local(Reg::X0, index);
                let loop_start = self.asm.new_label();
                let done = self.asm.new_label();
                self.asm.bind(loop_start);
                self.asm.load_local(Reg::X0, index);
                self.asm.cmp_reg(Reg::X0, Reg::X21); // argc
                self.asm.branch(done, BranchKind::Conditional(Cond::Ge));
                // argv[index]
                self.asm.lsl_imm(Reg::X0, Reg::X0, 3);
                self.asm.add_reg(Reg::X0, Reg::X22, Reg::X0);
                self.asm.ldr_imm(Reg::X0, Reg::X0, 0);
                self.emit_heap_string_from_cstring();
                self.push_rooted(Reg::X0);
                self.asm.load_local(Reg::X0, acc);
                self.emit_cons_cell();
                self.asm.store_local(Reg::X0, acc);
                self.asm.load_local(Reg::X0, index);
                self.asm.add_reg_imm(Reg::X0, Reg::X0, 1);
                self.asm.store_local(Reg::X0, index);
                self.asm.branch(loop_start, BranchKind::Unconditional);
                self.asm.bind(done);
                self.asm.load_local(Reg::X0, acc);
                let reverse = self.reverse_label();
                self.asm.branch(reverse, BranchKind::Link);
                self.close_temp_roots(mark);
                Ok(Some(ValueType::List(ListElem::Str)))
            }
            // `String#parseInt(text)`: a decimal integer, with an optional
            // leading `-`. Anything else stops the program with the same
            // message the evaluator gives, since there is no value to answer
            // with that would not be a lie.
            ("String#parseInt", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(arguments[0].span(), "parsing a non-string"));
                }
                let fail = self.asm.new_label();
                let done = self.asm.new_label();
                let digits = self.asm.new_label();
                let negative = self.asm.new_label();
                let signed = self.asm.new_label();
                self.asm.ldr_imm(Reg::X10, Reg::X0, 0); // length
                self.asm.add_reg_imm(Reg::X9, Reg::X0, 8); // bytes
                self.asm.mov_imm64(Reg::X11, 0); // index
                self.asm.mov_imm64(Reg::X12, 0); // accumulated value
                self.asm.mov_imm64(Reg::X1, 0); // negative?
                self.asm.cmp_imm(Reg::X10, 0);
                self.asm.branch(fail, BranchKind::Conditional(Cond::Eq));
                // A leading `-` shifts the first digit along.
                self.asm.ldrb_reg_offset(Reg::X2, Reg::X9, Reg::X11);
                self.asm.cmp_imm(Reg::X2, u32::from(b'-'));
                self.asm.branch(negative, BranchKind::Conditional(Cond::Eq));
                self.asm.branch(digits, BranchKind::Unconditional);
                self.asm.bind(negative);
                self.asm.mov_imm64(Reg::X1, 1);
                self.asm.mov_imm64(Reg::X11, 1);
                // `-` on its own is not a number.
                self.asm.cmp_reg(Reg::X11, Reg::X10);
                self.asm.branch(fail, BranchKind::Conditional(Cond::Ge));
                self.asm.bind(digits);
                let loop_start = self.asm.new_label();
                self.asm.bind(loop_start);
                self.asm.cmp_reg(Reg::X11, Reg::X10);
                self.asm.branch(signed, BranchKind::Conditional(Cond::Ge));
                self.asm.ldrb_reg_offset(Reg::X2, Reg::X9, Reg::X11);
                self.asm.cmp_imm(Reg::X2, u32::from(b'0'));
                self.asm.branch(fail, BranchKind::Conditional(Cond::Lt));
                self.asm.cmp_imm(Reg::X2, u32::from(b'9'));
                self.asm.branch(fail, BranchKind::Conditional(Cond::Gt));
                self.asm.mov_imm64(Reg::X3, 10);
                self.asm.mul_reg(Reg::X12, Reg::X12, Reg::X3);
                self.asm.sub_reg_imm(Reg::X2, Reg::X2, u32::from(b'0'));
                self.asm.add_reg(Reg::X12, Reg::X12, Reg::X2);
                self.asm.add_reg_imm(Reg::X11, Reg::X11, 1);
                self.asm.branch(loop_start, BranchKind::Unconditional);
                self.asm.bind(signed);
                self.asm.mov_reg(Reg::X0, Reg::X12);
                self.asm.cmp_imm(Reg::X1, 0);
                self.asm.branch(done, BranchKind::Conditional(Cond::Eq));
                self.asm.mov_imm64(Reg::X3, 0);
                self.asm.sub_reg(Reg::X0, Reg::X3, Reg::X0);
                self.asm.branch(done, BranchKind::Unconditional);
                self.asm.bind(fail);
                self.asm.emit_write_rodata(
                    STDERR_FD,
                    b"klassic: String#parseInt failed to parse a decimal integer\n",
                );
                self.asm.emit_exit(1);
                self.asm.bind(done);
                Ok(Some(ValueType::Int))
            }
            // `Process#exit(code)`: stop here with that status.
            ("Process#exit", 1) => {
                if self.expression(&arguments[0])? != ValueType::Int {
                    return Err(unsupported(arguments[0].span(), "exiting with a non-Int"));
                }
                self.asm.mov_reg(Reg::X0, Reg::X0);
                self.asm.mov_imm64(Reg::X16, u64::from(SYS_EXIT));
                self.asm.svc_0x80();
                // Never returns, so the value left behind is unreachable.
                self.asm.mov_imm64(Reg::X0, 0);
                Ok(Some(ValueType::Never))
            }
            // `StandardInput#lines()`: everything on standard input, split the
            // way `FileInput#lines` splits a file -- no trailing empty line.
            ("StandardInput#lines", 0) => {
                let mark = self.open_temp_roots();
                self.asm.mov_imm64(Reg::X0, 0); // fd 0
                self.emit_read_fd_to_string();
                let text = self.declare_local("__stdin_text", ValueType::Str);
                self.asm.store_local(Reg::X0, text);
                self.emit_root_frame_slot(text);
                let name_expr = |name: &str| Expr::Identifier {
                    name: name.to_string(),
                    span,
                };
                let call = |name: &str, args: Vec<Expr>| Expr::Call {
                    callee: Box::new(Expr::Identifier {
                        name: name.to_string(),
                        span,
                    }),
                    arguments: args,
                    span,
                };
                let split = call(
                    "split",
                    vec![
                        name_expr("__stdin_text"),
                        Expr::String {
                            value: "\n".to_string(),
                            span,
                        },
                    ],
                );
                let parts_ty = self.expression(&split)?;
                let parts = self.declare_local("__stdin_parts", parts_ty);
                self.asm.store_local(Reg::X0, parts);
                self.emit_root_frame_slot(parts);
                let trimmed = Expr::If {
                    condition: Box::new(call(
                        "isEmptyString",
                        vec![call("stdlibLast", vec![name_expr("__stdin_parts")])],
                    )),
                    then_branch: Box::new(call(
                        "stdlibTake",
                        vec![
                            name_expr("__stdin_parts"),
                            Expr::Binary {
                                op: BinaryOp::Subtract,
                                lhs: Box::new(call("size", vec![name_expr("__stdin_parts")])),
                                rhs: Box::new(Expr::Int {
                                    value: 1,
                                    kind: klassic_syntax::IntLiteralKind::Int,
                                    span,
                                }),
                                span,
                            },
                        ],
                    )),
                    else_branch: Some(Box::new(name_expr("__stdin_parts"))),
                    span,
                };
                let lines = self.expression(&trimmed)?;
                self.close_temp_roots(mark);
                Ok(Some(lines))
            }
            // `sleep(millis)` without a sleep syscall: this backend only issues
            // syscalls whose numbers it can verify, and Darwin does not publish
            // them in headers, so the wait is spun on the clock that is already
            // verified here. That costs the CPU it holds for the duration --
            // worth being explicit about -- but it is exact and it is honest,
            // where a guessed syscall number would break only on the machine
            // that runs the program.
            ("sleep", 1) => {
                if self.expression(&arguments[0])? != ValueType::Int {
                    return Err(unsupported(arguments[0].span(), "sleeping for a non-Int"));
                }
                self.asm.push(Reg::X0); // millis
                self.emit_time_now_millis(); // x0 = now
                self.asm.pop(Reg::X1); // millis
                // The evaluator rejects a negative duration at run time.
                let non_negative = self.asm.new_label();
                self.asm.cmp_imm(Reg::X1, 0);
                self.asm
                    .branch(non_negative, BranchKind::Conditional(Cond::Ge));
                self.asm
                    .emit_write_rodata(STDERR_FD, b"klassic: sleep expects a non-negative Int\n");
                self.asm.emit_exit(1);
                self.asm.bind(non_negative);
                self.asm.add_reg(Reg::X1, Reg::X0, Reg::X1); // deadline
                self.asm.push(Reg::X1);
                let wait = self.asm.new_label();
                let done = self.asm.new_label();
                self.asm.bind(wait);
                self.emit_time_now_millis(); // x0 = now
                self.asm.add_reg_sp_imm(Reg::X1, 0);
                self.asm.ldr_imm(Reg::X1, Reg::X1, 0); // deadline
                self.asm.cmp_reg(Reg::X0, Reg::X1);
                self.asm.branch(done, BranchKind::Conditional(Cond::Ge));
                self.asm.branch(wait, BranchKind::Unconditional);
                self.asm.bind(done);
                self.asm.pop(Reg::X1); // deadline, discarded
                self.asm.mov_imm64(Reg::X0, 0); // Unit
                Ok(Some(ValueType::Unit))
            }
            // `thread(block)` runs the block after the program's own statements
            // rather than beside them. Creating a thread on Darwin needs a
            // syscall whose number this host cannot verify, and the x86-64
            // backend queues them the same way, so the targets agree on what a
            // program prints -- which is what a program can observe here.
            ("thread", 1) => {
                let Some((params, body)) = self.lambda_argument(&arguments[0]) else {
                    return Err(unsupported(arguments[0].span(), "threading this argument"));
                };
                if !params.is_empty() {
                    return Err(unsupported(
                        arguments[0].span(),
                        "a thread body that takes arguments",
                    ));
                }
                let index = self.intern_lambda(params, body);
                self.queued_threads.push((index, span));
                self.asm.mov_imm64(Reg::X0, 0); // Unit
                Ok(Some(ValueType::Unit))
            }
            // `stopwatch(block)`: how many milliseconds the block took. Its
            // value is discarded, like the evaluator's.
            ("stopwatch", 1) => {
                let Some((params, body)) = self.lambda_argument(&arguments[0]) else {
                    return Err(unsupported(arguments[0].span(), "timing this argument"));
                };
                if !params.is_empty() {
                    return Err(unsupported(
                        arguments[0].span(),
                        "timing a block that takes arguments",
                    ));
                }
                self.emit_time_now_millis(); // x0 = start
                self.asm.push(Reg::X0);
                let index = self.intern_lambda(params, body);
                self.emit_inline_lambda_call(index, &[], span)?;
                self.emit_time_now_millis(); // x0 = end
                self.asm.pop(Reg::X1); // start
                self.asm.sub_reg(Reg::X0, Reg::X0, Reg::X1);
                Ok(Some(ValueType::Int))
            }
            ("Map#containsKey", 2) | ("containsKey", 2) | ("Map#get", 2) | ("get", 2) => {
                let map_ty = self.expression(&arguments[0])?;
                let (key_elem, value_elem) = match map_ty {
                    ValueType::Map(key, value) => (key, value),
                    // Nothing is in an empty map, whatever is asked of it --
                    // but the key still has to be evaluated for its effects.
                    ValueType::EmptyMap => {
                        self.expression(&arguments[1])?;
                        self.asm.mov_imm64(Reg::X0, 0);
                        return Ok(Some(if name.ends_with("containsKey") {
                            ValueType::Bool
                        } else {
                            ValueType::Null
                        }));
                    }
                    _ => return Err(unsupported(span, "a map builtin on a non-map")),
                };
                self.push_rooted(Reg::X0); // the map survives the key
                let key_ty = self.expression(&arguments[1])?;
                if list_elem_of(key_ty) != Some(key_elem) {
                    return Err(unsupported(
                        arguments[1].span(),
                        "a key of a different type than the map's keys",
                    ));
                }
                if is_boxed_scalar(key_ty) {
                    self.emit_box_scalar();
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.pop_rooted(Reg::X0);
                self.emit_map_lookup(key_elem);
                if name.ends_with("containsKey") {
                    self.asm.cmp_imm(Reg::X0, 0);
                    self.asm.cset(Reg::X0, Cond::Ne);
                    return Ok(Some(ValueType::Bool));
                }
                // `get` answers with the value or nothing, which is exactly a
                // nullable: the entry's value slot already holds it the way a
                // nullable wants it (boxed scalar or pointer).
                let absent = self.asm.new_label();
                let end = self.asm.new_label();
                self.asm.branch(absent, BranchKind::CompareZero(Reg::X0));
                self.emit_gc_load_ptr(Reg::X0, 8);
                self.asm.branch(end, BranchKind::Unconditional);
                self.asm.bind(absent);
                self.asm.mov_imm64(Reg::X0, 0);
                self.asm.bind(end);
                Ok(Some(ValueType::Nullable(value_elem)))
            }
            ("contains", 2) | ("Set#contains", 2) => {
                let set_ty = self.expression(&arguments[0])?;
                // `contains` reads as membership on a set and as substring
                // containment on a string -- the evaluator dispatches on the
                // receiver, and `std.string`'s `containsText` is the second.
                if set_ty == ValueType::Str {
                    self.push_rooted(Reg::X0);
                    if self.expression(&arguments[1])? != ValueType::Str {
                        return Err(unsupported(span, "contains with a non-string needle"));
                    }
                    self.asm.mov_reg(Reg::X1, Reg::X0);
                    self.pop_rooted(Reg::X0);
                    self.emit_str_index_of(false);
                    self.asm.cmp_imm(Reg::X0, 0);
                    self.asm.cset(Reg::X0, Cond::Ge);
                    return Ok(Some(ValueType::Bool));
                }
                let elem = match set_ty {
                    ValueType::Set(elem) => Some(elem),
                    ValueType::EmptySet => None,
                    _ => return Err(unsupported(span, "contains on a non-set")),
                };
                self.push_rooted(Reg::X0); // the set survives the candidate eval
                let candidate_ty = self.expression(&arguments[1])?;
                self.asm.mov_reg(Reg::X1, Reg::X0); // candidate
                self.pop_rooted(Reg::X0); // set head
                match elem {
                    Some(elem) => {
                        if list_elem_of(candidate_ty) != Some(elem) {
                            return Err(unsupported(span, "contains with a mismatched element"));
                        }
                        let member = self.member_label(elem);
                        self.asm.branch(member, BranchKind::Link);
                    }
                    // Nothing is a member of the empty set.
                    None => self.asm.mov_imm64(Reg::X0, 0),
                }
                Ok(Some(ValueType::Bool))
            }
            // The rest of the `Set#*` family `std.set`'s extension methods
            // delegate to. A set is an insertion-ordered chain with no
            // duplicates, which is the same chain a list is -- so reading one
            // as a list is a type change, and the two builders below are the
            // set literal's own loop over a chain that is only known while
            // running.
            ("Set#empty", 0) => {
                self.asm.mov_imm64(Reg::X0, 0);
                Ok(Some(ValueType::EmptySet))
            }
            ("Set#toList", 1) => {
                let ty = self.expression(&arguments[0])?;
                match ty {
                    ValueType::Set(elem) => Ok(Some(ValueType::List(elem))),
                    ValueType::EmptySet => Ok(Some(ValueType::EmptyList)),
                    ValueType::List(_) | ValueType::EmptyList => Ok(Some(ty)),
                    _ => Err(unsupported(span, "a set builtin on a non-set")),
                }
            }
            // `Set#add(s, x)`: the same set when `x` is already in it, and
            // otherwise a fresh chain holding `x` last -- built by copying the
            // old one backwards, prepending `x`, and reversing, so insertion
            // order comes out the way the evaluator prints it.
            ("Set#add", 2) => {
                let mark = self.open_temp_roots();
                let set_ty = self.expression(&arguments[0])?;
                let declared = match set_ty {
                    ValueType::Set(elem) => Some(elem),
                    ValueType::EmptySet => None,
                    _ => return Err(unsupported(span, "a set builtin on a non-set")),
                };
                let source = self.reserve_locals(8);
                self.asm.store_local(Reg::X0, source);
                self.emit_root_frame_slot(source);
                let value_ty = self.expression(&arguments[1])?;
                let Some(elem) = list_elem_of(value_ty) else {
                    return Err(unsupported(
                        arguments[1].span(),
                        "a set element of this type",
                    ));
                };
                if declared.is_some_and(|declared| declared != elem) {
                    return Err(unsupported(span, "adding an element of another type"));
                }
                let raw = self.reserve_locals(8);
                self.asm.store_local(Reg::X0, raw);
                if is_heap_pointer(value_ty) {
                    self.emit_root_frame_slot(raw);
                }
                let boxed = self.reserve_locals(8);
                if is_boxed_scalar(value_ty) {
                    self.emit_box_scalar();
                }
                self.asm.store_local(Reg::X0, boxed);
                self.emit_root_frame_slot(boxed);
                // Already a member? Then the set is its own answer.
                let present = self.asm.new_label();
                let end = self.asm.new_label();
                // The copy's two slots are rooted before the branch below, not
                // inside it. The shadow stack is popped by a count worked out
                // while compiling, so a root pushed on one path only leaves the
                // other path popping one that was never pushed -- which is what
                // `Set#add(s, x)` did when `x` was already there.
                let acc = self.reserve_locals(8);
                self.asm.mov_imm64(Reg::X0, 0);
                self.asm.store_local(Reg::X0, acc);
                self.emit_root_frame_slot(acc);
                let cursor = self.reserve_locals(8);
                self.asm.load_local(Reg::X0, source);
                self.asm.store_local(Reg::X0, cursor);
                self.emit_root_frame_slot(cursor);
                self.asm.load_local(Reg::X1, raw);
                self.asm.load_local(Reg::X0, source);
                let member = self.member_label(elem);
                self.asm.branch(member, BranchKind::Link);
                self.asm
                    .branch(present, BranchKind::CompareNonZero(Reg::X0));
                let loop_start = self.asm.new_label();
                let copied = self.asm.new_label();
                self.asm.bind(loop_start);
                self.asm.load_local(Reg::X0, cursor);
                self.asm.branch(copied, BranchKind::CompareZero(Reg::X0));
                self.emit_gc_load_ptr(Reg::X0, 0); // the element, already boxed
                self.push_rooted(Reg::X0); // head for the cons below
                self.asm.load_local(Reg::X0, acc);
                self.emit_cons_cell();
                self.asm.store_local(Reg::X0, acc);
                self.asm.load_local(Reg::X0, cursor);
                self.emit_gc_load_ptr(Reg::X0, 8);
                self.asm.store_local(Reg::X0, cursor);
                self.asm.branch(loop_start, BranchKind::Unconditional);
                self.asm.bind(copied);
                self.asm.load_local(Reg::X0, boxed);
                self.push_rooted(Reg::X0);
                self.asm.load_local(Reg::X0, acc);
                self.emit_cons_cell();
                let reverse = self.reverse_label();
                self.asm.branch(reverse, BranchKind::Link);
                self.asm.branch(end, BranchKind::Unconditional);
                self.asm.bind(present);
                self.asm.load_local(Reg::X0, source);
                self.asm.bind(end);
                self.close_temp_roots(mark);
                Ok(Some(ValueType::Set(elem)))
            }
            // `Set#fromList(xs)`: the set literal's loop, over a chain that is
            // only known while running -- keep the first occurrence of each
            // element, in the order they arrive.
            ("Set#fromList", 1) => {
                let mark = self.open_temp_roots();
                let ty = self.expression(&arguments[0])?;
                let elem = match ty {
                    ValueType::List(elem) | ValueType::Set(elem) => elem,
                    ValueType::EmptyList | ValueType::EmptySet => {
                        self.close_temp_roots(mark);
                        return Ok(Some(ValueType::EmptySet));
                    }
                    _ => return Err(unsupported(span, "a set builtin on a non-collection")),
                };
                let cursor = self.reserve_locals(8);
                self.asm.store_local(Reg::X0, cursor);
                self.emit_root_frame_slot(cursor);
                let acc = self.reserve_locals(8);
                self.asm.mov_imm64(Reg::X0, 0);
                self.asm.store_local(Reg::X0, acc);
                self.emit_root_frame_slot(acc);
                let loop_start = self.asm.new_label();
                let done = self.asm.new_label();
                self.asm.bind(loop_start);
                self.asm.load_local(Reg::X0, cursor);
                self.asm.branch(done, BranchKind::CompareZero(Reg::X0));
                self.emit_gc_load_ptr(Reg::X0, 0); // the element, already boxed
                self.push_rooted(Reg::X0); // survives the scan, and is the head
                if is_boxed_scalar(elem_value_type(elem)) {
                    self.emit_unbox_scalar();
                }
                self.asm.mov_reg(Reg::X1, Reg::X0); // candidate
                self.asm.load_local(Reg::X0, acc);
                let member = self.member_label(elem);
                self.asm.branch(member, BranchKind::Link);
                let skip = self.asm.new_label();
                let after = self.asm.new_label();
                self.asm.branch(skip, BranchKind::CompareNonZero(Reg::X0));
                self.asm.load_local(Reg::X0, acc);
                self.emit_cons_cell(); // takes the parked head
                self.asm.store_local(Reg::X0, acc);
                self.asm.branch(after, BranchKind::Unconditional);
                self.asm.bind(skip);
                self.pop_rooted(Reg::X0); // a duplicate: drop the parked head
                self.asm.bind(after);
                self.asm.load_local(Reg::X0, cursor);
                self.emit_gc_load_ptr(Reg::X0, 8);
                self.asm.store_local(Reg::X0, cursor);
                self.asm.branch(loop_start, BranchKind::Unconditional);
                self.asm.bind(done);
                self.asm.load_local(Reg::X0, acc);
                let reverse = self.reverse_label();
                self.asm.branch(reverse, BranchKind::Link);
                self.close_temp_roots(mark);
                Ok(Some(ValueType::Set(elem)))
            }
            // ---- enum-lowering primitives (`desugar_enums`) ----
            // `__gc_record(n)` / `__gc_alloc(bytes)`: zeroed heap
            // memory from the bump allocator (segments are fresh mmap
            // pages, so zeroing is free by construction).
            ("__gc_record", 1) | ("__gc_alloc", 1) => {
                if self.expression(&arguments[0])? != ValueType::Int {
                    return Err(unsupported(span, &format!("{name} with a non-Int size")));
                }
                // M6: a GC-shaped object with a 16-byte header. The enum
                // lowering boxes every scalar/bool/double field and the
                // discriminant, so a `__gc_record`'s n slots are all heap
                // pointers (POINTER_RECORD, size = 1 + field_count >= 1); a
                // `__gc_alloc` cell is raw bytes (RAW_BYTES). Both return the
                // user pointer (block + 16), so the `__gc_write`/`__gc_read`
                // payload offsets are unchanged.
                let tag = if name == "__gc_record" {
                    // n pointer slots -> n * 8 bytes of payload.
                    self.asm.lsl_imm(Reg::X5, Reg::X0, 3);
                    crate::gc_layout::GC_TYPE_POINTER_RECORD
                } else {
                    self.asm.add_reg_imm(Reg::X5, Reg::X0, 7);
                    self.asm.lsr_imm(Reg::X5, Reg::X5, 3);
                    self.asm.lsl_imm(Reg::X5, Reg::X5, 3);
                    crate::gc_layout::GC_TYPE_RAW_BYTES
                };
                self.asm.mov_imm64(Reg::X4, tag);
                self.emit_gc_alloc_call(); // x0 = user pointer
                Ok(Some(ValueType::Ptr))
            }
            ("__gc_write", 3) => {
                if self.expression(&arguments[0])? != ValueType::Ptr {
                    return Err(unsupported(span, "__gc_write to a non-pointer"));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Int {
                    return Err(unsupported(span, "__gc_write with a non-Int offset"));
                }
                self.asm.push(Reg::X0);
                let value_ty = self.expression(&arguments[2])?;
                if value_ty == ValueType::Unit {
                    return Err(unsupported(span, "__gc_write of unit"));
                }
                self.asm.mov_reg(Reg::X2, Reg::X0);
                self.asm.pop(Reg::X1);
                self.pop_rooted(Reg::X0);
                // Colour a stored heap reference. A boxed scalar's contents
                // goes through the same primitive and stays raw -- it is a
                // value, not a pointer.
                if is_heap_pointer(value_ty) {
                    self.emit_color_ptr(Reg::X2, Reg::X2);
                }
                self.asm.str_reg_offset(Reg::X2, Reg::X0, Reg::X1);
                Ok(Some(ValueType::Unit))
            }
            ("__gc_read", 2) | ("__gc_read_ptr", 2) | ("__gc_read_string", 2) => {
                if self.expression(&arguments[0])? != ValueType::Ptr {
                    return Err(unsupported(span, &format!("{name} of a non-pointer")));
                }
                self.push_rooted(Reg::X0);
                if self.expression(&arguments[1])? != ValueType::Int {
                    return Err(unsupported(span, &format!("{name} with a non-Int offset")));
                }
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.pop_rooted(Reg::X0);
                if name == "__gc_read" {
                    // A scalar out of a RAW_BYTES box: no barrier (the box
                    // pointer itself was read through one).
                    self.asm.ldr_reg_offset(Reg::X0, Reg::X0, Reg::X1);
                } else {
                    self.emit_gc_load_ptr_reg_offset(Reg::X0, Reg::X1);
                }
                Ok(Some(match name {
                    "__gc_read" => ValueType::Int,
                    "__gc_read_string" => ValueType::Str,
                    _ => ValueType::Ptr,
                }))
            }
            // On this backend rodata literals and heap strings are
            // already the same one-pointer layout; normalization is
            // the identity until a tracing collector exists.
            ("__gc_string", 1) => {
                if self.expression(&arguments[0])? != ValueType::Str {
                    return Err(unsupported(span, "__gc_string of a non-string"));
                }
                Ok(Some(ValueType::Str))
            }
            // ---- numeric builtins ----
            // Doubles travel as their raw bits in a general register, so each
            // of these moves through d0 and back. `abs` is the only one that
            // also has an integer form.
            ("double", 1) => {
                let ty = self.expression(&arguments[0])?;
                match ty {
                    ValueType::Int => {
                        self.asm.scvtf_d_from_x(0, Reg::X0);
                        self.asm.fmov_x_from_d(Reg::X0, 0);
                        Ok(Some(ValueType::Double))
                    }
                    ValueType::Double => Ok(Some(ValueType::Double)),
                    _ => Err(unsupported(span, "double of this type")),
                }
            }
            ("sqrt", 1) => {
                let ty = self.expression(&arguments[0])?;
                match ty {
                    ValueType::Double => {
                        self.asm.fmov_d_from_x(0, Reg::X0);
                        self.asm.fsqrt_d(0, 0);
                        self.asm.fmov_x_from_d(Reg::X0, 0);
                        Ok(Some(ValueType::Double))
                    }
                    // The evaluator's sqrt is Double-valued for an Int too.
                    ValueType::Int => {
                        self.asm.scvtf_d_from_x(0, Reg::X0);
                        self.asm.fsqrt_d(0, 0);
                        self.asm.fmov_x_from_d(Reg::X0, 0);
                        Ok(Some(ValueType::Double))
                    }
                    _ => Err(unsupported(span, "sqrt of this type")),
                }
            }
            // `int` and `floor` are the same builtin in this language and
            // both *truncate toward zero* -- `floor(-4.7)` is -4, not -5 --
            // which is exactly what `fcvtzs` does on its own. `ceil` is a true
            // ceiling, so it rounds up first. Both are Int-valued.
            ("floor", 1) | ("int", 1) | ("ceil", 1) => {
                let ty = self.expression(&arguments[0])?;
                match ty {
                    ValueType::Double => {}
                    // An integer is already the answer.
                    ValueType::Int => return Ok(Some(ValueType::Int)),
                    _ => return Err(unsupported(span, &format!("{name} of this type"))),
                }
                self.asm.fmov_d_from_x(0, Reg::X0);
                if name == "ceil" {
                    self.asm.frintp_d(0, 0);
                }
                self.asm.fcvtzs_x_from_d(Reg::X0, 0);
                Ok(Some(ValueType::Int))
            }
            ("abs", 1) => {
                let ty = self.expression(&arguments[0])?;
                match ty {
                    ValueType::Double => {
                        self.asm.fmov_d_from_x(0, Reg::X0);
                        self.asm.fabs_d(0, 0);
                        self.asm.fmov_x_from_d(Reg::X0, 0);
                        Ok(Some(ValueType::Double))
                    }
                    ValueType::Int => {
                        // negative ? 0 - value : value
                        let non_negative = self.asm.new_label();
                        self.asm.cmp_imm(Reg::X0, 0);
                        self.asm
                            .branch(non_negative, BranchKind::Conditional(Cond::Ge));
                        self.asm.mov_reg(Reg::X1, Reg::X0);
                        self.asm.mov_imm64(Reg::X0, 0);
                        self.asm.sub_reg(Reg::X0, Reg::X0, Reg::X1);
                        self.asm.bind(non_negative);
                        Ok(Some(ValueType::Int))
                    }
                    _ => Err(unsupported(span, "abs of this type")),
                }
            }
            ("__match_fail", 0) => {
                self.asm
                    .emit_write_rodata(STDERR_FD, b"klassic: match: no pattern matched\n");
                self.asm.emit_exit(1);
                Ok(Some(ValueType::Never))
            }
            // `access(path, F_OK)`: the carry flag (Cond::Cc = success)
            // becomes the Bool result directly — the first user of the
            // M11 syscall-failure convention (issue #538).
            ("Dir#exists", 1) | ("FileOutput#exists", 1) => {
                self.emit_path_cstring(&arguments[0])?;
                self.asm.mov_imm64(Reg::X1, 0); // F_OK
                self.asm.mov_imm64(Reg::X16, u64::from(SYS_ACCESS));
                self.asm.svc_0x80();
                self.asm.cset_syscall_succeeded();
                Ok(Some(ValueType::Bool))
            }
            ("FileOutput#write", 2) | ("FileOutput#append", 2) => {
                if self.expression(&arguments[1])? != ValueType::Str {
                    return Err(unsupported(
                        span,
                        &format!("{name} with non-string content"),
                    ));
                }
                // The content waits as a root while the path is built, since
                // building one from a run-time string allocates.
                self.push_rooted(Reg::X0);
                self.emit_path_cstring(&arguments[0])?;
                self.emit_file_write(name == "FileOutput#append");
                Ok(Some(ValueType::Unit))
            }
            // `writeLines(path, lines)` is the lines joined with newlines and
            // written -- no trailing one, which is what the evaluator's own
            // `join` does. Building that call rather than a second string
            // walker keeps the two spellings the same code.
            ("FileOutput#writeLines", 2) => {
                let joined = Expr::Call {
                    callee: Box::new(Expr::Identifier {
                        name: "join".to_string(),
                        span,
                    }),
                    arguments: vec![
                        arguments[1].clone(),
                        Expr::String {
                            value: "\n".to_string(),
                            span,
                        },
                    ],
                    span,
                };
                if self.expression(&joined)? != ValueType::Str {
                    return Err(unsupported(span, "writeLines of a non-string list"));
                }
                self.push_rooted(Reg::X0);
                self.emit_path_cstring(&arguments[0])?;
                self.emit_file_write(false);
                Ok(Some(ValueType::Unit))
            }
            // `lines(path)` splits the file on newlines *without* a trailing
            // empty line, which is what the evaluator gets from Rust's
            // `str::lines`. Built here as the same expression `std.string`'s
            // own `lines` is written as, over two hidden locals so the text and
            // the split are each computed once.
            ("FileInput#lines", 1) | ("FileInput#readLines", 1) => {
                let mark = self.open_temp_roots();
                self.emit_path_cstring(&arguments[0])?;
                self.emit_file_read_all();
                let text = self.declare_local("__file_lines_text", ValueType::Str);
                self.asm.store_local(Reg::X0, text);
                self.emit_root_frame_slot(text);
                let name_expr = |name: &str| Expr::Identifier {
                    name: name.to_string(),
                    span,
                };
                let call = |name: &str, args: Vec<Expr>| Expr::Call {
                    callee: Box::new(Expr::Identifier {
                        name: name.to_string(),
                        span,
                    }),
                    arguments: args,
                    span,
                };
                let split = call(
                    "split",
                    vec![
                        name_expr("__file_lines_text"),
                        Expr::String {
                            value: "\n".to_string(),
                            span,
                        },
                    ],
                );
                let parts_ty = self.expression(&split)?;
                let parts = self.declare_local("__file_lines_parts", parts_ty);
                self.asm.store_local(Reg::X0, parts);
                self.emit_root_frame_slot(parts);
                let trimmed = Expr::If {
                    condition: Box::new(call(
                        "isEmptyString",
                        vec![call("stdlibLast", vec![name_expr("__file_lines_parts")])],
                    )),
                    then_branch: Box::new(call(
                        "stdlibTake",
                        vec![
                            name_expr("__file_lines_parts"),
                            Expr::Binary {
                                op: BinaryOp::Subtract,
                                lhs: Box::new(call("size", vec![name_expr("__file_lines_parts")])),
                                rhs: Box::new(Expr::Int {
                                    value: 1,
                                    kind: klassic_syntax::IntLiteralKind::Int,
                                    span,
                                }),
                                span,
                            },
                        ],
                    )),
                    else_branch: Some(Box::new(name_expr("__file_lines_parts"))),
                    span,
                };
                let lines = self.expression(&trimmed)?;
                self.close_temp_roots(mark);
                Ok(Some(lines))
            }
            // `path FileInput#open { stream => ... }`: the evaluator hands the
            // block the path itself and answers with the block's value, so this
            // is the block inlined with its parameter bound to the path. There
            // is no descriptor to keep or close -- reading happens inside.
            ("FileInput#open", 2) => {
                let Some((params, body)) = self.lambda_argument(&arguments[1]) else {
                    return Err(unsupported(arguments[1].span(), "opening with this block"));
                };
                if params.len() != 1 {
                    return Err(unsupported(
                        arguments[1].span(),
                        "an open block taking other than one stream",
                    ));
                }
                let index = self.intern_lambda(params, body);
                let opened = self.emit_inline_lambda_call(index, &arguments[0..1], span)?;
                Ok(Some(opened))
            }
            ("FileInput#all", 1) | ("FileInput#readAll", 1) => {
                self.emit_path_cstring(&arguments[0])?;
                self.emit_file_read_all();
                Ok(Some(ValueType::Str))
            }
            ("FileOutput#delete", 1) => {
                self.emit_path_cstring(&arguments[0])?;
                self.emit_file_delete();
                Ok(Some(ValueType::Unit))
            }
            ("Dir#mkdir", 1) => {
                self.emit_path_cstring(&arguments[0])?;
                self.emit_dir_mkdir();
                Ok(Some(ValueType::Unit))
            }
            ("Dir#delete", 1) => {
                self.emit_path_cstring(&arguments[0])?;
                self.emit_dir_delete();
                Ok(Some(ValueType::Unit))
            }
            ("Dir#mkdirs", 1) => {
                // Each parent directory is created in turn, so this one needs
                // the path's text here rather than at run time -- a `val`
                // holding a literal counts.
                let Some(path) = self.literal_string(&arguments[0]) else {
                    return Err(unsupported(
                        arguments[0].span(),
                        "mkdirs of a path that is not a literal",
                    ));
                };
                // An empty path is a no-op, matching the evaluator's
                // `fs::create_dir_all("")` (which succeeds); `mkdir("")`
                // itself would fail with ENOENT, not EEXIST, and abort.
                let mut prefixes = Vec::new();
                if !path.is_empty() {
                    for (index, ch) in path.char_indices() {
                        if ch == '/' && index > 0 {
                            prefixes.push(path[..index].to_string());
                        }
                    }
                    prefixes.push(path.clone());
                }
                for prefix in prefixes {
                    let prefix_offset = self.asm.intern_nul_terminated(&prefix);
                    self.asm.load_rodata_address(Reg::X0, prefix_offset);
                    self.emit_dir_mkdir_tolerating_eexist();
                }
                self.asm.mov_imm64(Reg::X0, 0); // Unit
                Ok(Some(ValueType::Unit))
            }
            ("Dir#isDirectory", 1) => {
                self.emit_path_cstring(&arguments[0])?;
                self.emit_dir_is_directory();
                Ok(Some(ValueType::Bool))
            }
            ("Dir#move", 2) => {
                self.emit_path_cstring(&arguments[0])?;
                // The source waits as a root: a run-time target path allocates,
                // and a collection would move the buffer it is parked in.
                self.push_rooted(Reg::X0);
                self.emit_path_cstring(&arguments[1])?;
                self.asm.mov_reg(Reg::X1, Reg::X0);
                self.pop_rooted(Reg::X0);
                self.emit_dir_move();
                Ok(Some(ValueType::Unit))
            }
            ("Environment#exists", 1) => {
                self.emit_environment_key(&arguments[0])?;
                self.emit_environment_lookup();
                self.asm.cmp_imm(Reg::X0, 0);
                self.asm.cset(Reg::X0, Cond::Ne);
                Ok(Some(ValueType::Bool))
            }
            // `Environment#get(name)`: the variable's value, or the
            // evaluator's error when there is no such variable.
            ("Environment#get", 1) => {
                self.emit_environment_key(&arguments[0])?;
                self.emit_environment_lookup();
                let present = self.asm.new_label();
                self.asm
                    .branch(present, BranchKind::CompareNonZero(Reg::X0));
                self.asm.emit_write_rodata(
                    STDERR_FD,
                    b"klassic: failed to read environment variable\n",
                );
                self.asm.emit_exit(1);
                self.asm.bind(present);
                self.emit_heap_string_from_cstring();
                Ok(Some(ValueType::Str))
            }
            ("Time#nowMillis", 0) => {
                self.emit_time_now_millis();
                Ok(Some(ValueType::Int))
            }
            _ => Ok(None),
        }
    }

    /// Call a top-level annotated function: arguments are evaluated
    /// left to right onto the machine stack, then popped into the
    /// AAPCS64 argument registers right to left.
    /// The type of an expression, worked out *without* emitting code, for
    /// decisions that must happen before compiling anything -- currently
    /// choosing between same-named typeclass instance methods. Deliberately
    /// partial: `None` means "cannot tell cheaply", and every caller has a
    /// defined behaviour for that.
    fn static_type_of(&self, expr: &Expr) -> Option<ValueType> {
        match expr {
            Expr::Int { .. } => Some(ValueType::Int),
            Expr::Double { .. } => Some(ValueType::Double),
            Expr::Bool { .. } => Some(ValueType::Bool),
            Expr::String { .. } | Expr::StringInterpolation { .. } => Some(ValueType::Str),
            Expr::Identifier { name, .. } => self.lookup(name).map(|(_, ty)| ty),
            Expr::ListLiteral { elements, .. } => match elements.first() {
                None => Some(ValueType::EmptyList),
                Some(first) => list_elem_of(self.static_type_of(first)?).map(ValueType::List),
            },
            Expr::SetLiteral { elements, .. } => match elements.first() {
                None => Some(ValueType::EmptySet),
                Some(first) => list_elem_of(self.static_type_of(first)?).map(ValueType::Set),
            },
            Expr::RecordConstructor { name, .. } => self
                .records
                .iter()
                .position(|record| record.usable && &record.name == name)
                .map(|index| ValueType::Record(index as u32)),
            Expr::Call { callee, .. } => {
                let Expr::Identifier { name, .. } = callee.as_ref() else {
                    return None;
                };
                let mut matching = self.functions.iter().filter(|(n, _)| n == name);
                let first = matching.next()?;
                // Only unambiguous when the name has exactly one definition.
                matching.next().is_none().then_some(first.1.ret)
            }
            _ => None,
        }
    }

    /// Build the evaluator's textual form of a cons list as a heap string:
    /// `[1, 2, 3]` for a list, `%(1, 2)` for a set, `[]` when empty.
    ///
    /// Every step allocates -- each element's text, each concatenation -- so
    /// the input cursor and the string being built live in rooted slots and
    /// the loop re-reads them rather than holding them in registers.
    fn emit_list_to_str(
        &mut self,
        elem: ListElem,
        open: &str,
        close: &str,
        span: Span,
    ) -> Result<(), Diagnostic> {
        let mark = self.open_temp_roots();
        let cursor = self.reserve_locals(24);
        let acc = cursor + 8;
        let sep = acc + 8;
        self.asm.store_local(Reg::X0, cursor);
        self.emit_root_frame_slot(cursor);
        let open_offset = self.asm.intern_string_object(open);
        self.asm.load_rodata_address(Reg::X0, open_offset);
        self.emit_heap_string_copy();
        self.asm.store_local(Reg::X0, acc);
        self.emit_root_frame_slot(acc);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.store_local(Reg::X0, sep);

        let loop_start = self.asm.new_label();
        let done = self.asm.new_label();
        let no_sep = self.asm.new_label();
        self.asm.bind(loop_start);
        self.asm.load_local(Reg::X0, cursor);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X0));
        // Separator before every element but the first.
        self.asm.load_local(Reg::X0, sep);
        self.asm.branch(no_sep, BranchKind::CompareZero(Reg::X0));
        let comma_offset = self.asm.intern_string_object(", ");
        self.asm.load_rodata_address(Reg::X0, comma_offset);
        self.emit_heap_string_copy();
        self.asm.mov_reg(Reg::X1, Reg::X0);
        self.asm.load_local(Reg::X0, acc);
        self.emit_str_concat();
        self.asm.store_local(Reg::X0, acc);
        self.asm.bind(no_sep);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.store_local(Reg::X0, sep);
        // The element's own text.
        let elem_ty = elem_value_type(elem);
        self.asm.load_local(Reg::X0, cursor);
        self.emit_gc_load_ptr(Reg::X0, 0);
        if is_boxed_scalar(elem_ty) {
            self.emit_unbox_scalar();
        }
        match elem {
            ListElem::Int => self.emit_int_to_str(),
            ListElem::Bool => self.emit_bool_to_str(),
            ListElem::Str => {}
            ListElem::Enum(shape) => self.emit_enum_to_str(shape, span)?,
            ListElem::Record(_) => {
                return Err(unsupported(span, "rendering a record element"));
            }
            // Same renderer one level down; the element pointer is already in
            // x0, and the result replaces it.
            ListElem::Nested { .. } => {
                let inner = elem
                    .nested_inner()
                    .expect("a nested element is a list of its inner element");
                self.emit_list_to_str(inner, "[", "]", span)?;
            }
            // Guarded by the callers, which reject a Double element before
            // building any text (see emit_print_elem).
            ListElem::Double => return Err(unsupported(span, "rendering a Double element")),
        }
        self.asm.mov_reg(Reg::X1, Reg::X0);
        self.asm.load_local(Reg::X0, acc);
        self.emit_str_concat();
        self.asm.store_local(Reg::X0, acc);
        // Advance.
        self.asm.load_local(Reg::X0, cursor);
        self.emit_gc_load_ptr(Reg::X0, 8);
        self.asm.store_local(Reg::X0, cursor);
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(done);

        let close_offset = self.asm.intern_string_object(close);
        self.asm.load_rodata_address(Reg::X0, close_offset);
        self.emit_heap_string_copy();
        self.asm.mov_reg(Reg::X1, Reg::X0);
        self.asm.load_local(Reg::X0, acc);
        self.emit_str_concat();
        self.close_temp_roots(mark);
        Ok(())
    }

    /// Append the text of a literal to the string builder in `acc`.
    fn emit_append_literal(&mut self, acc: u32, text: &str) {
        let offset = self.asm.intern_string_object(text);
        self.asm.load_rodata_address(Reg::X0, offset);
        self.emit_heap_string_copy();
        self.asm.mov_reg(Reg::X1, Reg::X0);
        self.asm.load_local(Reg::X0, acc);
        self.emit_str_concat();
        self.asm.store_local(Reg::X0, acc);
    }

    /// The rendering `emit_print_enum_inline` writes to stdout, built as a heap
    /// string instead -- which is what `"#{result}"` and a list of enums need.
    /// The value is in x0 and the string replaces it.
    fn emit_enum_to_str(&mut self, shape: u32, span: Span) -> Result<(), Diagnostic> {
        if self.printing_enums.contains(&shape) {
            return Err(unsupported(span, "rendering an enum that holds itself"));
        }
        self.printing_enums.push(shape);
        let result = self.emit_enum_to_str_variants(shape, span);
        self.printing_enums.pop();
        result
    }

    fn emit_enum_to_str_variants(&mut self, shape: u32, span: Span) -> Result<(), Diagnostic> {
        let (enum_index, payloads) = self.enum_shapes[shape as usize].clone();
        let variants = self.generic_enums[enum_index].1.clone();
        let mark = self.open_temp_roots();
        let value = self.reserve_locals(16);
        let acc = value + 8;
        self.asm.store_local(Reg::X0, value);
        self.emit_root_frame_slot(value);
        // The builder starts empty, and is rooted before any branch so every
        // path through the switch below has pushed the same roots.
        let empty = self.asm.intern_string_object("");
        self.asm.load_rodata_address(Reg::X0, empty);
        self.emit_heap_string_copy();
        self.asm.store_local(Reg::X0, acc);
        self.emit_root_frame_slot(acc);
        let end = self.asm.new_label();
        for (tag, (variant, arity)) in variants.iter().enumerate() {
            if payloads[tag].contains(&ValueType::Never) {
                continue;
            }
            let next = self.asm.new_label();
            self.asm.load_local(Reg::X0, value);
            self.emit_gc_load_ptr(Reg::X0, 0); // the boxed tag
            self.emit_unbox_scalar();
            self.asm.cmp_imm(Reg::X0, tag as u32);
            self.asm.branch(next, BranchKind::Conditional(Cond::Ne));
            self.emit_append_literal(acc, variant);
            if *arity > 0 {
                self.emit_append_literal(acc, "(");
                let fields = payloads[tag].clone();
                for (position, field_ty) in fields.iter().copied().take(*arity).enumerate() {
                    if position > 0 {
                        self.emit_append_literal(acc, ", ");
                    }
                    self.asm.load_local(Reg::X0, value);
                    self.emit_gc_load_ptr(Reg::X0, (position as u32 + 1) * 8);
                    if is_boxed_scalar(field_ty) {
                        self.emit_unbox_scalar();
                    }
                    match field_ty {
                        ValueType::Int => self.emit_int_to_str(),
                        ValueType::Bool => self.emit_bool_to_str(),
                        ValueType::Str => {}
                        ValueType::Enum(inner) => self.emit_enum_to_str(inner, span)?,
                        ValueType::List(elem) => self.emit_list_to_str(elem, "[", "]", span)?,
                        ValueType::Set(elem) => self.emit_list_to_str(elem, "%(", ")", span)?,
                        other => {
                            return Err(unsupported(
                                span,
                                &format!("rendering an enum payload of type {other:?}"),
                            ));
                        }
                    }
                    self.asm.mov_reg(Reg::X1, Reg::X0);
                    self.asm.load_local(Reg::X0, acc);
                    self.emit_str_concat();
                    self.asm.store_local(Reg::X0, acc);
                }
                self.emit_append_literal(acc, ")");
            }
            self.asm.branch(end, BranchKind::Unconditional);
            self.asm.bind(next);
        }
        self.asm.bind(end);
        self.asm.load_local(Reg::X0, acc);
        self.close_temp_roots(mark);
        Ok(())
    }

    /// Leave the trimmed window of the string in x0 as `[x3, x2)` over the
    /// bytes at x1: the evaluator's `is_int`/`is_double` both start with
    /// `trim()`, and everything after reads that window.
    fn emit_trim_window(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0); // length
        self.asm.add_reg_imm(Reg::X1, Reg::X0, 8); // bytes
        self.asm.mov_imm64(Reg::X3, 0);
        let left_loop = self.asm.new_label();
        let left_done = self.asm.new_label();
        self.asm.bind(left_loop);
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm
            .branch(left_done, BranchKind::Conditional(Cond::Ge));
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X3);
        self.emit_branch_unless_ascii_space(Reg::X4, left_done);
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.branch(left_loop, BranchKind::Unconditional);
        self.asm.bind(left_done);
        let right_loop = self.asm.new_label();
        let right_done = self.asm.new_label();
        self.asm.bind(right_loop);
        self.asm.cmp_reg(Reg::X2, Reg::X3);
        self.asm
            .branch(right_done, BranchKind::Conditional(Cond::Le));
        self.asm.sub_reg_imm(Reg::X5, Reg::X2, 1);
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X5);
        self.emit_branch_unless_ascii_space(Reg::X4, right_done);
        self.asm.mov_reg(Reg::X2, Reg::X5);
        self.asm.branch(right_loop, BranchKind::Unconditional);
        self.asm.bind(right_done);
    }

    /// Branch to `label` unless the byte in `reg` is ASCII whitespace.
    fn emit_branch_unless_ascii_space(&mut self, reg: Reg, label: Label) {
        let is_space = self.asm.new_label();
        for byte in [b' ', b'\t', b'\n', b'\r', 0x0b, 0x0c] {
            self.asm.cmp_imm(reg, u32::from(byte));
            self.asm.branch(is_space, BranchKind::Conditional(Cond::Eq));
        }
        self.asm.branch(label, BranchKind::Unconditional);
        self.asm.bind(is_space);
    }

    /// `String#isInt(s)` for a string only known at run time: what
    /// `s.trim().parse::<i64>()` accepts, range included. Nineteen digits is
    /// the only length that needs comparing, and it compares digits, so no
    /// 128-bit arithmetic is involved.
    fn emit_str_is_int(&mut self) {
        let positive = self.asm.intern_rodata(b"9223372036854775807");
        let negative = self.asm.intern_rodata(b"9223372036854775808");
        let no = self.asm.new_label();
        let yes = self.asm.new_label();
        let done = self.asm.new_label();
        self.emit_trim_window();
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm.branch(no, BranchKind::Conditional(Cond::Ge));
        // An optional sign, remembered in x7.
        let signed = self.asm.new_label();
        let after_sign = self.asm.new_label();
        self.asm.mov_imm64(Reg::X7, 0);
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X3);
        self.asm.cmp_imm(Reg::X4, u32::from(b'+'));
        self.asm.branch(signed, BranchKind::Conditional(Cond::Eq));
        self.asm.cmp_imm(Reg::X4, u32::from(b'-'));
        self.asm
            .branch(after_sign, BranchKind::Conditional(Cond::Ne));
        self.asm.mov_imm64(Reg::X7, 1);
        self.asm.bind(signed);
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.bind(after_sign);
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm.branch(no, BranchKind::Conditional(Cond::Ge));
        // Leading zeros do not count towards the range; keep the last digit.
        let zero_loop = self.asm.new_label();
        let zeros_done = self.asm.new_label();
        self.asm.bind(zero_loop);
        self.asm.sub_reg(Reg::X5, Reg::X2, Reg::X3);
        self.asm.cmp_imm(Reg::X5, 1);
        self.asm
            .branch(zeros_done, BranchKind::Conditional(Cond::Le));
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X3);
        self.asm.cmp_imm(Reg::X4, u32::from(b'0'));
        self.asm
            .branch(zeros_done, BranchKind::Conditional(Cond::Ne));
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.branch(zero_loop, BranchKind::Unconditional);
        self.asm.bind(zeros_done);
        // Every remaining byte has to be a digit.
        let digit_loop = self.asm.new_label();
        let digits_done = self.asm.new_label();
        self.asm.mov_reg(Reg::X5, Reg::X3);
        self.asm.bind(digit_loop);
        self.asm.cmp_reg(Reg::X5, Reg::X2);
        self.asm
            .branch(digits_done, BranchKind::Conditional(Cond::Ge));
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X5);
        self.asm.cmp_imm(Reg::X4, u32::from(b'0'));
        self.asm.branch(no, BranchKind::Conditional(Cond::Lt));
        self.asm.cmp_imm(Reg::X4, u32::from(b'9'));
        self.asm.branch(no, BranchKind::Conditional(Cond::Gt));
        self.asm.add_reg_imm(Reg::X5, Reg::X5, 1);
        self.asm.branch(digit_loop, BranchKind::Unconditional);
        self.asm.bind(digits_done);
        self.asm.sub_reg(Reg::X5, Reg::X2, Reg::X3);
        self.asm.cmp_imm(Reg::X5, 19);
        self.asm.branch(no, BranchKind::Conditional(Cond::Gt));
        self.asm.branch(yes, BranchKind::Conditional(Cond::Lt));
        let use_negative = self.asm.new_label();
        let limit_ready = self.asm.new_label();
        self.asm.cmp_imm(Reg::X7, 0);
        self.asm
            .branch(use_negative, BranchKind::Conditional(Cond::Ne));
        self.asm.load_rodata_address(Reg::X6, positive);
        self.asm.branch(limit_ready, BranchKind::Unconditional);
        self.asm.bind(use_negative);
        self.asm.load_rodata_address(Reg::X6, negative);
        self.asm.bind(limit_ready);
        let compare_loop = self.asm.new_label();
        self.asm.mov_imm64(Reg::X5, 0);
        self.asm.bind(compare_loop);
        self.asm.cmp_imm(Reg::X5, 19);
        self.asm.branch(yes, BranchKind::Conditional(Cond::Ge));
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X3);
        self.asm.ldrb_reg_offset(Reg::X8, Reg::X6, Reg::X5);
        self.asm.cmp_reg(Reg::X4, Reg::X8);
        self.asm.branch(yes, BranchKind::Conditional(Cond::Lt));
        self.asm.branch(no, BranchKind::Conditional(Cond::Gt));
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.add_reg_imm(Reg::X5, Reg::X5, 1);
        self.asm.branch(compare_loop, BranchKind::Unconditional);
        self.asm.bind(yes);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.branch(done, BranchKind::Unconditional);
        self.asm.bind(no);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.bind(done);
    }

    /// `String#isDouble(s)` for a string only known at run time: what
    /// `s.trim().parse::<f64>()` accepts -- an optional sign, then `inf`,
    /// `infinity` or `nan` in either case, or digits with an optional point
    /// and an optional exponent. Too large to represent parses as infinity
    /// rather than failing, so there is no range check.
    fn emit_str_is_double(&mut self) {
        let no = self.asm.new_label();
        let yes = self.asm.new_label();
        let done = self.asm.new_label();
        self.emit_trim_window();
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm.branch(no, BranchKind::Conditional(Cond::Ge));
        let signed = self.asm.new_label();
        let after_sign = self.asm.new_label();
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X3);
        self.asm.cmp_imm(Reg::X4, u32::from(b'+'));
        self.asm.branch(signed, BranchKind::Conditional(Cond::Eq));
        self.asm.cmp_imm(Reg::X4, u32::from(b'-'));
        self.asm
            .branch(after_sign, BranchKind::Conditional(Cond::Ne));
        self.asm.bind(signed);
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.bind(after_sign);
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm.branch(no, BranchKind::Conditional(Cond::Ge));
        // `inf`, `infinity` and `nan`, in any case.
        for word in [b"infinity".as_slice(), b"inf".as_slice(), b"nan".as_slice()] {
            let offset = self.asm.intern_rodata(word);
            let mismatch = self.asm.new_label();
            let compare = self.asm.new_label();
            self.asm.sub_reg(Reg::X5, Reg::X2, Reg::X3);
            self.asm.cmp_imm(Reg::X5, word.len() as u32);
            self.asm.branch(mismatch, BranchKind::Conditional(Cond::Ne));
            self.asm.load_rodata_address(Reg::X6, offset);
            self.asm.mov_imm64(Reg::X5, 0);
            self.asm.bind(compare);
            self.asm.cmp_imm(Reg::X5, word.len() as u32);
            self.asm.branch(yes, BranchKind::Conditional(Cond::Ge));
            self.asm.add_reg(Reg::X9, Reg::X3, Reg::X5);
            self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X9);
            // Lower-case the input byte; the words above are lower-case.
            self.asm.mov_imm64(Reg::X10, 0x20);
            self.asm.orr_reg(Reg::X4, Reg::X4, Reg::X10);
            self.asm.ldrb_reg_offset(Reg::X8, Reg::X6, Reg::X5);
            self.asm.cmp_reg(Reg::X4, Reg::X8);
            self.asm.branch(mismatch, BranchKind::Conditional(Cond::Ne));
            self.asm.add_reg_imm(Reg::X5, Reg::X5, 1);
            self.asm.branch(compare, BranchKind::Unconditional);
            self.asm.bind(mismatch);
        }
        // Digits, an optional point with more digits, at least one of the two.
        self.asm.mov_imm64(Reg::X7, 0);
        let int_digits = self.asm.new_label();
        let int_done = self.asm.new_label();
        self.asm.bind(int_digits);
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm.branch(int_done, BranchKind::Conditional(Cond::Ge));
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X3);
        self.asm.cmp_imm(Reg::X4, u32::from(b'0'));
        self.asm.branch(int_done, BranchKind::Conditional(Cond::Lt));
        self.asm.cmp_imm(Reg::X4, u32::from(b'9'));
        self.asm.branch(int_done, BranchKind::Conditional(Cond::Gt));
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.add_reg_imm(Reg::X7, Reg::X7, 1);
        self.asm.branch(int_digits, BranchKind::Unconditional);
        self.asm.bind(int_done);
        let after_point = self.asm.new_label();
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm
            .branch(after_point, BranchKind::Conditional(Cond::Ge));
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X3);
        self.asm.cmp_imm(Reg::X4, u32::from(b'.'));
        self.asm
            .branch(after_point, BranchKind::Conditional(Cond::Ne));
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        let frac_digits = self.asm.new_label();
        self.asm.bind(frac_digits);
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm
            .branch(after_point, BranchKind::Conditional(Cond::Ge));
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X3);
        self.asm.cmp_imm(Reg::X4, u32::from(b'0'));
        self.asm
            .branch(after_point, BranchKind::Conditional(Cond::Lt));
        self.asm.cmp_imm(Reg::X4, u32::from(b'9'));
        self.asm
            .branch(after_point, BranchKind::Conditional(Cond::Gt));
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.add_reg_imm(Reg::X7, Reg::X7, 1);
        self.asm.branch(frac_digits, BranchKind::Unconditional);
        self.asm.bind(after_point);
        self.asm.cmp_imm(Reg::X7, 0);
        self.asm.branch(no, BranchKind::Conditional(Cond::Eq));
        // An optional exponent, which needs at least one digit of its own.
        let after_exponent = self.asm.new_label();
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm
            .branch(after_exponent, BranchKind::Conditional(Cond::Ge));
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X3);
        self.asm.mov_imm64(Reg::X10, 0x20);
        self.asm.orr_reg(Reg::X4, Reg::X4, Reg::X10);
        self.asm.cmp_imm(Reg::X4, u32::from(b'e'));
        self.asm
            .branch(after_exponent, BranchKind::Conditional(Cond::Ne));
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        let exponent_signed = self.asm.new_label();
        let exponent_digits = self.asm.new_label();
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm.branch(no, BranchKind::Conditional(Cond::Ge));
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X3);
        self.asm.cmp_imm(Reg::X4, u32::from(b'+'));
        self.asm
            .branch(exponent_signed, BranchKind::Conditional(Cond::Eq));
        self.asm.cmp_imm(Reg::X4, u32::from(b'-'));
        self.asm
            .branch(exponent_digits, BranchKind::Conditional(Cond::Ne));
        self.asm.bind(exponent_signed);
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.bind(exponent_digits);
        self.asm.mov_imm64(Reg::X7, 0);
        let exponent_loop = self.asm.new_label();
        let exponent_done = self.asm.new_label();
        self.asm.bind(exponent_loop);
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm
            .branch(exponent_done, BranchKind::Conditional(Cond::Ge));
        self.asm.ldrb_reg_offset(Reg::X4, Reg::X1, Reg::X3);
        self.asm.cmp_imm(Reg::X4, u32::from(b'0'));
        self.asm
            .branch(exponent_done, BranchKind::Conditional(Cond::Lt));
        self.asm.cmp_imm(Reg::X4, u32::from(b'9'));
        self.asm
            .branch(exponent_done, BranchKind::Conditional(Cond::Gt));
        self.asm.add_reg_imm(Reg::X3, Reg::X3, 1);
        self.asm.add_reg_imm(Reg::X7, Reg::X7, 1);
        self.asm.branch(exponent_loop, BranchKind::Unconditional);
        self.asm.bind(exponent_done);
        self.asm.cmp_imm(Reg::X7, 0);
        self.asm.branch(no, BranchKind::Conditional(Cond::Eq));
        self.asm.bind(after_exponent);
        // Anything left over means it was not a number.
        self.asm.cmp_reg(Reg::X3, Reg::X2);
        self.asm.branch(no, BranchKind::Conditional(Cond::Ne));
        self.asm.bind(yes);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.branch(done, BranchKind::Unconditional);
        self.asm.bind(no);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.bind(done);
    }

    /// Is every byte of the string in x0 an ASCII digit? x0 becomes the Bool.
    /// An empty string answers false, which is what both digit patterns want.
    fn emit_str_all_digits(&mut self) {
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0); // length
        self.asm.add_reg_imm(Reg::X3, Reg::X0, 8); // bytes
        let loop_start = self.asm.new_label();
        let no = self.asm.new_label();
        let yes = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.branch(no, BranchKind::CompareZero(Reg::X2));
        self.asm.bind(loop_start);
        self.asm.branch(yes, BranchKind::CompareZero(Reg::X2));
        self.asm.ldrb_post_increment(Reg::X4, Reg::X3);
        self.asm.cmp_imm(Reg::X4, u32::from(b'0'));
        self.asm.branch(no, BranchKind::Conditional(Cond::Lt));
        self.asm.cmp_imm(Reg::X4, u32::from(b'9'));
        self.asm.branch(no, BranchKind::Conditional(Cond::Gt));
        self.asm.sub_reg_imm(Reg::X2, Reg::X2, 1);
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(yes);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.branch(done, BranchKind::Unconditional);
        self.asm.bind(no);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.bind(done);
    }

    /// `matches(input, pattern)`: the language's deliberately tiny regex --
    /// `.*` matches anything, `[0-9]+` a non-empty run of digits, `[0-9]` a
    /// single digit, and any other pattern is a literal to compare against.
    ///
    /// Decided at run time by comparing the pattern string, where the x86-64
    /// backend folds it at compile time from a static string; doing it this way
    /// costs three string comparisons and works for a pattern that is not a
    /// literal.
    fn emit_str_matches(&mut self) {
        // x0 = input, x1 = pattern. Both are parked as roots: building each
        // literal below allocates.
        self.push_rooted(Reg::X0); // input, at [sp + 16] after the next push
        self.push_rooted(Reg::X1); // pattern, at [sp]
        let matched = self.asm.new_label();
        let unmatched = self.asm.new_label();
        let done = self.asm.new_label();
        let literal = self.asm.new_label();
        let digits_plus = self.asm.new_label();
        let single_digit = self.asm.new_label();

        // Each test compares the pattern against one of the three forms.
        for (text, target) in [
            (".*", matched),
            ("[0-9]+", digits_plus),
            ("[0-9]", single_digit),
        ] {
            self.asm.add_reg_sp_imm(Reg::X3, 0);
            self.asm.ldr_imm(Reg::X0, Reg::X3, 0); // the pattern
            let offset = self.asm.intern_string_object(text);
            self.asm.load_rodata_address(Reg::X1, offset);
            self.asm.push(Reg::X0);
            self.asm.mov_reg(Reg::X0, Reg::X1);
            self.emit_heap_string_copy(); // the literal, as a heap string
            self.asm.mov_reg(Reg::X1, Reg::X0);
            self.asm.pop(Reg::X0);
            self.emit_str_eq();
            self.asm.branch(target, BranchKind::CompareNonZero(Reg::X0));
        }
        self.asm.branch(literal, BranchKind::Unconditional);

        // `[0-9]+`: a non-empty run of digits.
        self.asm.bind(digits_plus);
        self.asm.add_reg_sp_imm(Reg::X3, 16);
        self.asm.ldr_imm(Reg::X0, Reg::X3, 0); // the input
        self.emit_str_all_digits();
        self.asm
            .branch(matched, BranchKind::CompareNonZero(Reg::X0));
        self.asm.branch(unmatched, BranchKind::Unconditional);

        // `[0-9]`: exactly one digit.
        self.asm.bind(single_digit);
        self.asm.add_reg_sp_imm(Reg::X3, 16);
        self.asm.ldr_imm(Reg::X0, Reg::X3, 0);
        self.asm.ldr_imm(Reg::X2, Reg::X0, 0); // length
        self.asm.cmp_imm(Reg::X2, 1);
        self.asm
            .branch(unmatched, BranchKind::Conditional(Cond::Ne));
        self.emit_str_all_digits();
        self.asm
            .branch(matched, BranchKind::CompareNonZero(Reg::X0));
        self.asm.branch(unmatched, BranchKind::Unconditional);

        // Anything else is a literal to equal.
        self.asm.bind(literal);
        self.asm.add_reg_sp_imm(Reg::X3, 16);
        self.asm.ldr_imm(Reg::X0, Reg::X3, 0); // the input
        self.asm.add_reg_sp_imm(Reg::X3, 0);
        self.asm.ldr_imm(Reg::X1, Reg::X3, 0); // the pattern
        self.emit_str_eq();
        self.asm
            .branch(matched, BranchKind::CompareNonZero(Reg::X0));

        self.asm.bind(unmatched);
        self.asm.mov_imm64(Reg::X2, 0);
        self.asm.branch(done, BranchKind::Unconditional);
        self.asm.bind(matched);
        self.asm.mov_imm64(Reg::X2, 1);
        self.asm.bind(done);
        self.pop_rooted(Reg::X1);
        self.pop_rooted(Reg::X0);
        self.asm.mov_reg(Reg::X0, Reg::X2);
    }

    /// Content equality for two cons chains: x0 and x1 are the heads, x0
    /// becomes the Bool.
    ///
    /// Walks both at once -- equal length, equal elements, in order. The
    /// element comparison is by type: a boxed scalar is unboxed and compared
    /// by value, a string by its bytes. Nothing here allocates, so no rooting
    /// is needed, but the string comparison clobbers x0-x7, so both cursors
    /// live on the machine stack across it.
    fn emit_list_eq(&mut self, elem: ListElem, span: Span) -> Result<(), Diagnostic> {
        let elem_ty = elem_value_type(elem);
        if matches!(elem, ListElem::Double) {
            // Comparing the raw bits is not IEEE equality, and no sample
            // program asks for it, so it is refused rather than quietly wrong.
            return Err(unsupported(span, "comparing lists of Double"));
        }
        let loop_start = self.asm.new_label();
        let equal = self.asm.new_label();
        let different = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.bind(loop_start);
        // Either chain ending decides it: both nil is equal, one nil is not.
        let lhs_more = self.asm.new_label();
        self.asm
            .branch(lhs_more, BranchKind::CompareNonZero(Reg::X0));
        self.asm.branch(equal, BranchKind::CompareZero(Reg::X1));
        self.asm.branch(different, BranchKind::Unconditional);
        self.asm.bind(lhs_more);
        self.asm.branch(different, BranchKind::CompareZero(Reg::X1));

        // Park both cursors: the element reads and the string comparison
        // below clobber the registers holding them.
        self.asm.push(Reg::X0); // lhs cursor, at [sp + 16] after the next push
        self.asm.push(Reg::X1); // rhs cursor, at [sp]
        self.asm.add_reg_sp_imm(Reg::X3, 16);
        self.asm.ldr_imm(Reg::X0, Reg::X3, 0);
        self.emit_gc_load_ptr(Reg::X0, 0);
        if is_boxed_scalar(elem_ty) {
            self.emit_unbox_scalar();
        }
        self.asm.mov_reg(Reg::X2, Reg::X0); // the left element
        self.asm.add_reg_sp_imm(Reg::X3, 0);
        self.asm.ldr_imm(Reg::X0, Reg::X3, 0);
        self.emit_gc_load_ptr(Reg::X0, 0);
        if is_boxed_scalar(elem_ty) {
            self.emit_unbox_scalar();
        }
        self.asm.mov_reg(Reg::X1, Reg::X0); // the right element
        self.asm.mov_reg(Reg::X0, Reg::X2);
        if elem_ty == ValueType::Str {
            self.emit_str_eq(); // x0 = Bool
        } else if let Some(inner) = elem.nested_inner() {
            // The elements are themselves lists, so they compare by content
            // too -- a pointer comparison here would call two separately built
            // inner lists different, which is what it did before the nested
            // element type existed. The recursion is at codegen time and
            // terminates because the depth decreases.
            self.emit_list_eq(inner, span)?;
        } else {
            self.asm.cmp_reg(Reg::X0, Reg::X1);
            self.asm.cset(Reg::X0, Cond::Eq);
        }
        // Take the cursors back on both paths, so the stack stays balanced.
        let same = self.asm.new_label();
        self.asm.branch(same, BranchKind::CompareNonZero(Reg::X0));
        self.asm.pop(Reg::X1);
        self.asm.pop(Reg::X0);
        self.asm.branch(different, BranchKind::Unconditional);
        self.asm.bind(same);
        self.asm.pop(Reg::X1);
        self.asm.pop(Reg::X0);

        // Advance both cursors. The barrier preserves x1-x7, so the right
        // cursor survives the left read.
        self.asm.push(Reg::X1);
        self.emit_gc_load_ptr(Reg::X0, 8);
        self.asm.mov_reg(Reg::X2, Reg::X0); // the left next
        self.asm.pop(Reg::X1);
        self.asm.push(Reg::X2);
        self.emit_gc_load_ptr(Reg::X1, 8);
        self.asm.mov_reg(Reg::X1, Reg::X0); // the right next
        self.asm.pop(Reg::X0);
        self.asm.branch(loop_start, BranchKind::Unconditional);

        self.asm.bind(equal);
        self.asm.mov_imm64(Reg::X0, 1);
        self.asm.branch(done, BranchKind::Unconditional);
        self.asm.bind(different);
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm.bind(done);
        Ok(())
    }

    /// `map(list)(f)`: a fresh list of `f` applied to each element.
    ///
    /// The lambda's body is compiled *once*, inside the loop, with its
    /// parameter bound to the current element -- so this is a real runtime
    /// map rather than an unrolling, and it works for a list of any length
    /// including one whose length is not known until run time. The result is
    /// built by prepending and then reversed, reusing the cons-list machinery
    /// the set literal already shares.
    fn emit_list_map(
        &mut self,
        list: &Expr,
        param: &str,
        body: &Expr,
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        let mark = self.open_temp_roots();
        let list_ty = self.expression(list)?;
        let elem = match list_ty {
            ValueType::List(elem) => elem,
            // Mapping over an empty list is an empty list, whatever f is.
            ValueType::EmptyList => return Ok(ValueType::EmptyList),
            _ => return Err(unsupported(list.span(), "mapping over this value")),
        };
        // Three frame slots: the input cursor, the accumulated output and the
        // element the lambda parameter names. The first two hold heap
        // references across the allocations the body and the cons perform, so
        // both are rooted; the parameter slot is rooted only when its type is
        // a reference.
        let cursor = self.reserve_locals(24);
        let acc = cursor + 8;
        let param_slot = acc + 8;
        self.asm.store_local(Reg::X0, cursor);
        self.emit_root_frame_slot(cursor);
        self.asm.mov_imm64(Reg::X0, 0); // acc = nil
        self.asm.store_local(Reg::X0, acc);
        self.emit_root_frame_slot(acc);

        let elem_ty = elem_value_type(elem);
        let loop_start = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.bind(loop_start);
        self.asm.load_local(Reg::X0, cursor);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X0));
        // element = [cursor + 0], unboxed when it is a scalar
        self.emit_gc_load_ptr(Reg::X0, 0);
        if is_boxed_scalar(elem_ty) {
            self.emit_unbox_scalar();
        }
        self.asm.store_local(Reg::X0, param_slot);
        self.push_binding_scopes();
        self.scope_root_counts.push(0);
        self.scopes
            .last_mut()
            .expect("emitter scope")
            .insert(param.to_string(), (param_slot, elem_ty));
        if is_heap_pointer(elem_ty) {
            self.emit_root_frame_slot(param_slot);
        }
        let mapped = self.expression(body)?;
        let Some(mapped_elem) = list_elem_of(mapped) else {
            return Err(unsupported(body.span(), "a mapped element of this type"));
        };
        if is_boxed_scalar(mapped) {
            self.emit_box_scalar();
        }
        // acc = cons(mapped, acc)
        self.push_rooted(Reg::X0);
        self.asm.load_local(Reg::X0, acc);
        self.emit_cons_cell();
        self.asm.store_local(Reg::X0, acc);
        self.pop_binding_scopes();
        let roots = self.scope_root_counts.pop().expect("emitter root scope");
        self.emit_shadow_pop(roots);
        // cursor = [cursor + 8]
        self.asm.load_local(Reg::X0, cursor);
        self.emit_gc_load_ptr(Reg::X0, 8);
        self.asm.store_local(Reg::X0, cursor);
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(done);

        // Prepending reversed the order; put it back.
        self.asm.load_local(Reg::X0, acc);
        let reverse = self.reverse_label();
        self.asm.push_frame_record(); // the bl clobbers x30
        self.asm.branch(reverse, BranchKind::Link);
        self.asm.pop_frame_record();
        self.close_temp_roots(mark);
        let _ = span;
        Ok(ValueType::List(mapped_elem))
    }

    /// The type of an expression under an overlay of extra bindings, worked
    /// out without emitting code. This is how a generic def's *return* type
    /// is found: bind its parameters to the argument types from the call
    /// site, then read the body's type. `None` means "cannot tell cheaply",
    /// and callers treat that as unsupported rather than guessing.
    ///
    /// The overlay grows as a block's `val`s are walked, so a body that
    /// computes through locals -- the usual shape -- is covered. A recursive
    /// call reads as `None`, which an `if` merges away as long as the other
    /// branch is concrete; that is exactly what makes `def fact(n) = if (n <
    /// 2) 1 else n * fact(n - 1)` work.
    fn static_type_under(
        &mut self,
        expr: &Expr,
        locals: &mut Vec<(String, ValueType)>,
        depth: u32,
    ) -> Option<ValueType> {
        // Inferring through a call means inferring that callee's body too, so
        // a bound keeps a recursive or mutually recursive chain finite. The
        // `if` merge below is what still gets a useful answer out of a
        // recursive function: the branch that bottoms out types, and the one
        // that recurses contributes nothing.
        if depth > 40 {
            return None;
        }
        match expr {
            Expr::Identifier { name, .. } => locals
                .iter()
                .rev()
                .find(|(n, _)| n == name)
                .map(|(_, ty)| *ty)
                .or_else(|| self.lookup(name).map(|(_, ty)| ty)),
            // Typed the way the emitter types it, so an `if` with a `null`
            // branch predicts the nullable join rather than the other branch.
            Expr::Null { .. } => Some(ValueType::Null),
            // `#Name(a, b)`. A generic record's fields take their types from
            // the arguments, which is the same rule compiling one uses -- so
            // predicting one interns the shape compilation would intern, and
            // both agree on the index.
            Expr::RecordConstructor {
                name, arguments, ..
            } => {
                if let Some(index) = self
                    .records
                    .iter()
                    .position(|record| record.usable && record.name == *name)
                    .filter(|_| {
                        !self.generic_records.iter().any(|record| {
                            record.name == *name && record.fields.len() == arguments.len()
                        })
                    })
                {
                    return Some(ValueType::Record(index as u32));
                }
                let declared = self
                    .generic_records
                    .iter()
                    .find(|record| record.name == *name && record.fields.len() == arguments.len())
                    .cloned()?;
                let mut typed = Vec::with_capacity(arguments.len());
                for (argument, (field, _)) in arguments.iter().zip(declared.fields.iter()) {
                    let ty = self.static_type_under(argument, locals, depth + 1)?;
                    if ty == ValueType::Unit || ty == ValueType::Never {
                        return None;
                    }
                    typed.push((field.clone(), ty));
                }
                let index = match self.records.iter().position(|record| {
                    record.usable && record.name == *name && record.fields == typed
                }) {
                    Some(index) => index,
                    None => {
                        self.records.push(RecordInfo {
                            name: name.clone(),
                            fields: typed,
                            lambda_fields: Vec::new(),
                            usable: true,
                        });
                        self.records.len() - 1
                    }
                };
                Some(ValueType::Record(index as u32))
            }
            // Collection literals over locals: the whole-program inference
            // cannot see these bindings, so the element type is inferred here.
            // `unit(x) = [x]` in a Monad instance is exactly this shape.
            Expr::ListLiteral { elements, .. } => match elements.first() {
                None => Some(ValueType::EmptyList),
                Some(first) => list_elem_of(self.static_type_under(first, locals, depth + 1)?)
                    .map(ValueType::List),
            },
            Expr::SetLiteral { elements, .. } => match elements.first() {
                None => Some(ValueType::EmptySet),
                Some(first) => list_elem_of(self.static_type_under(first, locals, depth + 1)?)
                    .map(ValueType::Set),
            },
            Expr::MapLiteral { entries, .. } => match entries.first() {
                None => Some(ValueType::EmptyMap),
                Some((key, value)) => {
                    let key = list_elem_of(self.static_type_under(key, locals, depth + 1)?)?;
                    let value = list_elem_of(self.static_type_under(value, locals, depth + 1)?)?;
                    Some(ValueType::Map(key, value))
                }
            },
            Expr::Block { expressions, .. } => {
                let mark = locals.len();
                let (last, init) = expressions.split_last()?;
                for expression in init {
                    if let Expr::VarDecl { name, value, .. } = expression
                        && let Some(ty) = self.static_type_under(value, locals, depth + 1)
                    {
                        locals.push((name.clone(), ty));
                    }
                }
                let ty = self.static_type_under(last, locals, depth + 1);
                locals.truncate(mark);
                ty
            }
            // A match answers with its arms merged, with each arm's bindings
            // taken from the scrutinee's shape. This is what types the option
            // helpers, whose bodies are nothing but a match.
            Expr::Match {
                scrutinee, arms, ..
            } => {
                let ValueType::Enum(shape) =
                    self.static_type_under(scrutinee, locals, depth + 1)?
                else {
                    return None;
                };
                let mut result: Option<ValueType> = None;
                for arm in arms {
                    if arm.guard.is_some() {
                        return None;
                    }
                    let mark = locals.len();
                    // Patterns nest, so the bindings are collected the way the
                    // emitter collects them; an arm the shape says cannot be
                    // taken contributes nothing, exactly as there.
                    if !self.bind_pattern_types(ValueType::Enum(shape), &arm.pattern, locals) {
                        locals.truncate(mark);
                        continue;
                    }
                    let arm_ty = self.static_type_under(&arm.body, locals, depth + 1);
                    locals.truncate(mark);
                    let arm_ty = arm_ty?;
                    result = Some(match result {
                        None => arm_ty,
                        Some(previous) => self.merge_types(previous, arm_ty)?,
                    });
                }
                result
            }
            Expr::If {
                then_branch,
                else_branch,
                ..
            } => {
                let then_ty = self.static_type_under(then_branch, locals, depth + 1);
                let else_ty = else_branch
                    .as_ref()
                    .and_then(|branch| self.static_type_under(branch, locals, depth + 1));
                match (then_ty, else_ty) {
                    // Merged, so `if (done) [] else cons(x)(rest)` is a list
                    // of x rather than the empty list the first branch shows.
                    (Some(then_ty), Some(else_ty)) => self.merge_types(then_ty, else_ty),
                    // A branch whose type is unknown -- the recursive one --
                    // does not veto the branch that is known.
                    (known, None) | (None, known) => known,
                }
            }
            Expr::Unary { op, expr, .. } => match op {
                UnaryOp::Not => Some(ValueType::Bool),
                _ => self.static_type_under(expr, locals, depth + 1),
            },
            Expr::Binary { lhs, op, rhs, .. } => match op {
                BinaryOp::Equal
                | BinaryOp::NotEqual
                | BinaryOp::Less
                | BinaryOp::LessEqual
                | BinaryOp::Greater
                | BinaryOp::GreaterEqual
                | BinaryOp::LogicalAnd
                | BinaryOp::LogicalOr => Some(ValueType::Bool),
                // Arithmetic keeps the operand type; appending to a string
                // yields a string whatever the right side was.
                _ => {
                    let lhs_ty = self.static_type_under(lhs, locals, depth + 1);
                    match lhs_ty {
                        Some(ValueType::Str) => Some(ValueType::Str),
                        Some(ty) => Some(ty),
                        None => self.static_type_under(rhs, locals, depth + 1),
                    }
                }
            },
            Expr::FieldAccess { target, field, .. } => {
                let ValueType::Record(index) = self.static_type_under(target, locals, depth + 1)?
                else {
                    return None;
                };
                let info = self.records.get(index as usize)?;
                info.fields
                    .iter()
                    .find(|(name, _)| name == field)
                    .map(|(_, ty)| *ty)
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                // Curried `foldLeft(list)(init)(f)` has the accumulator's type,
                // which is the initial value's -- this is what types the
                // prelude's fold-based helpers.
                if arguments.len() == 1
                    && let Expr::Call {
                        callee: with_initial,
                        arguments: initial_args,
                        ..
                    } = callee.as_ref()
                    && initial_args.len() == 1
                    && let Expr::Call {
                        callee: inner,
                        arguments: list_args,
                        ..
                    } = with_initial.as_ref()
                    && matches!(inner.as_ref(), Expr::Identifier { name, .. } if name == "foldLeft")
                {
                    let initial_ty = self.static_type_under(&initial_args[0], locals, depth + 1)?;
                    // ...but the body can widen it, exactly as the emitter's
                    // own fold does: starting from `[]` and appending lists
                    // ends up a list. Take the same fixpoint here so a
                    // fold-based function's return type is the settled one.
                    if let Some((params, body)) = self.lambda_argument(&arguments[0])
                        && params.len() == 2
                        && list_args.len() == 1
                        && let Some(ValueType::List(elem)) =
                            self.static_type_under(&list_args[0], locals, depth + 1)
                    {
                        let elem_ty = elem_value_type(elem);
                        let mut acc_ty = initial_ty;
                        for _ in 0..3 {
                            locals.push((params[0].clone(), acc_ty));
                            locals.push((params[1].clone(), elem_ty));
                            let folded = self.static_type_under(&body, locals, depth + 1);
                            locals.pop();
                            locals.pop();
                            let Some(merged) = folded.and_then(|f| self.merge_types(acc_ty, f))
                            else {
                                break;
                            };
                            if merged == acc_ty {
                                break;
                            }
                            acc_ty = merged;
                        }
                        return Some(acc_ty);
                    }
                    return Some(initial_ty);
                }
                // Curried `cons(head)(tail)` builds a list of the head's type.
                if arguments.len() == 1
                    && let Expr::Call {
                        callee: inner,
                        arguments: head_args,
                        ..
                    } = callee.as_ref()
                    && head_args.len() == 1
                    && matches!(inner.as_ref(), Expr::Identifier { name, .. } if name == "cons")
                {
                    let head_ty = self.static_type_under(&head_args[0], locals, depth + 1)?;
                    return list_elem_of(head_ty).map(ValueType::List);
                }
                // Curried `map(list)(f)`, the spelling `xs.map(f)` also
                // becomes: a list of whatever the lambda produces.
                if arguments.len() == 1
                    && let Expr::Call {
                        callee: inner,
                        arguments: list_args,
                        ..
                    } = callee.as_ref()
                    && list_args.len() == 1
                    && matches!(inner.as_ref(), Expr::Identifier { name, .. } if name == "map")
                {
                    return self.mapped_list_type(&list_args[0], &arguments[0], locals, depth);
                }
                // A curried call `f(a)(b)(c)`: the emitter compiles it as one
                // call to a flattened function, so predict that -- which is
                // what types such a function's *recursion*, and so what its
                // result widens to.
                if let Expr::Call { .. } = callee.as_ref() {
                    let mut spine: Vec<&[Expr]> = vec![arguments];
                    let mut head = callee.as_ref();
                    while let Expr::Call {
                        callee, arguments, ..
                    } = head
                    {
                        spine.push(arguments);
                        head = callee;
                    }
                    spine.reverse();
                    if let Expr::Identifier { name, .. } = head {
                        let flattened: Vec<Expr> = spine
                            .iter()
                            .flat_map(|group| group.iter().cloned())
                            .collect();
                        let curried = format!("{name}#curried{}", flattened.len());
                        if let Some(generic) = self.generic_functions.iter().rposition(|generic| {
                            generic.name == curried && generic.params.len() == flattened.len()
                        }) {
                            let (params, body) = {
                                let generic = &self.generic_functions[generic];
                                (generic.params.clone(), generic.body.clone())
                            };
                            let mark = locals.len();
                            self.lambda_bindings.push(HashMap::new());
                            for (param, argument) in params.iter().zip(flattened.iter()) {
                                // A function argument is bound as one; anything
                                // else contributes its type.
                                if let Some((lambda_params, lambda_body)) =
                                    self.lambda_argument(argument)
                                {
                                    let lambda = self.intern_lambda(lambda_params, lambda_body);
                                    self.lambda_bindings
                                        .last_mut()
                                        .expect("emitter lambda scope")
                                        .insert(param.clone(), lambda);
                                    continue;
                                }
                                if let Some(ty) =
                                    self.static_type_under(argument, locals, depth + 1)
                                {
                                    locals.push((param.clone(), ty));
                                }
                            }
                            let predicted = self.static_type_under(&body, locals, depth + 1);
                            self.lambda_bindings.pop();
                            locals.truncate(mark);
                            return predicted;
                        }
                    }
                }
                // Method-style `target.m(args)`: the emitter dispatches it as
                // `m(target, args)`, so predict that call instead. This is what
                // types an instance method whose body is `xs.map(f)`.
                if let Expr::FieldAccess {
                    target,
                    field,
                    span: access,
                } = callee.as_ref()
                {
                    let mut all = Vec::with_capacity(arguments.len() + 1);
                    all.push((**target).clone());
                    all.extend(arguments.iter().cloned());
                    let dispatched = Expr::Call {
                        callee: Box::new(Expr::Identifier {
                            name: field.clone(),
                            span: *access,
                        }),
                        arguments: all,
                        span: *access,
                    };
                    return self.static_type_under(&dispatched, locals, depth + 1);
                }
                let Expr::Identifier { name, .. } = callee.as_ref() else {
                    return None;
                };
                // A name bound to a lambda: its result is the lambda body's,
                // with the lambda's parameters bound to these arguments. This
                // is what types a higher-order def like
                // `def applyTwice(f, x) = f(f(x))`.
                if let Some(lambda) = self.lookup_lambda(name) {
                    let (lambda_params, lambda_body) = self.lambdas[lambda].clone();
                    if lambda_params.len() != arguments.len() {
                        return None;
                    }
                    let mut lambda_locals = Vec::with_capacity(lambda_params.len());
                    for (param, argument) in lambda_params.iter().zip(arguments.iter()) {
                        let ty = self.static_type_under(argument, locals, depth + 1)?;
                        lambda_locals.push((param.clone(), ty));
                    }
                    return self.static_type_under(&lambda_body, &mut lambda_locals, depth + 1);
                }
                // A builtin's result, for the handful whose type the inference
                // path actually meets. Anything else falls through to the
                // function table.
                let first_type = arguments
                    .first()
                    .and_then(|argument| self.static_type_under(argument, locals, depth + 1));
                match name.as_str() {
                    // head/tail matter more than they look: they are how the
                    // prelude's recursive list functions are written, so
                    // without them a body like `cons(head(xs))(rest)` cannot
                    // be typed and the whole inference collapses to the empty
                    // list its base case shows.
                    "head" => {
                        return arguments
                            .first()
                            .and_then(|argument| {
                                self.static_type_under(argument, locals, depth + 1)
                            })
                            .and_then(|ty| match ty {
                                ValueType::List(elem) | ValueType::Set(elem) => {
                                    Some(elem_value_type(elem))
                                }
                                _ => None,
                            });
                    }
                    "tail" => {
                        return arguments
                            .first()
                            .and_then(|argument| {
                                self.static_type_under(argument, locals, depth + 1)
                            })
                            .filter(|ty| matches!(ty, ValueType::List(_) | ValueType::EmptyList));
                    }
                    "println" | "print" | "assert" => return Some(ValueType::Unit),
                    "double" | "sqrt" => return Some(ValueType::Double),
                    "abs" => return first_type,
                    // `floor`/`int`/`ceil` are Int-valued here, like the
                    // evaluator's -- keeping this in step with the emitter
                    // matters, since a wrong guess is a rejected build.
                    "size" | "length" | "toInt" | "floor" | "int" | "ceil" | "Map#size"
                    | "Set#size" | "indexOf" | "lastIndexOf" | "Math#gcd" | "Math#powInt"
                    | "Math#sqrtInt" => {
                        return Some(ValueType::Int);
                    }
                    "isEmpty" | "contains" | "isEmptyString" | "matches" | "containsKey"
                    | "Map#containsKey" | "Map#isEmpty" | "Map#containsValue" | "Set#isEmpty"
                    | "Set#contains" | "startsWith" | "endsWith" | "String#isInt"
                    | "String#isDouble" => {
                        return Some(ValueType::Bool);
                    }
                    "String#parseInt" => return Some(ValueType::Int),
                    // The set builders, so `std.set`'s extension methods can
                    // be typed: two of them answer with a set, and reading one
                    // as a list keeps the element type.
                    "Set#empty" => return Some(ValueType::EmptySet),
                    "Set#toList" => {
                        return match first_type {
                            Some(ValueType::Set(elem)) => Some(ValueType::List(elem)),
                            Some(ValueType::EmptySet) => Some(ValueType::EmptyList),
                            other @ (Some(ValueType::List(_)) | Some(ValueType::EmptyList)) => {
                                other
                            }
                            _ => None,
                        };
                    }
                    "Set#add" | "Set#fromList" => {
                        return match first_type {
                            Some(ValueType::Set(elem)) | Some(ValueType::List(elem)) => {
                                Some(ValueType::Set(elem))
                            }
                            Some(ValueType::EmptySet) | Some(ValueType::EmptyList)
                                if name == "Set#fromList" =>
                            {
                                Some(ValueType::EmptySet)
                            }
                            _ => None,
                        };
                    }
                    // Splitting a string is how `std.string`'s `lines` and
                    // `words` start, so the inference has to get through it to
                    // type them at all.
                    "split" => return Some(ValueType::List(ListElem::Str)),
                    // `map(list, f)`, the spelling the evaluator's own builtin
                    // takes.
                    "map" if arguments.len() == 2 => {
                        return self.mapped_list_type(&arguments[0], &arguments[1], locals, depth);
                    }
                    // Looking a key up answers with the value or nothing, so
                    // the inference needs the map's own value type -- which is
                    // what types `std.map`'s `getOrElse`, whose body merges a
                    // lookup with its fallback.
                    "get" | "Map#get" => {
                        return match first_type {
                            Some(ValueType::Map(_, value)) => Some(ValueType::Nullable(value)),
                            _ => None,
                        };
                    }
                    // A map's keys and values are lists of its two halves,
                    // which is what types every fold that walks a map --
                    // `std.map`'s `toPairs` among them.
                    "Map#keys" | "keys" => {
                        return match first_type {
                            Some(ValueType::Map(key, _)) => Some(ValueType::List(key)),
                            _ => None,
                        };
                    }
                    "Map#values" | "values" => {
                        return match first_type {
                            Some(ValueType::Map(_, value)) => Some(ValueType::List(value)),
                            _ => None,
                        };
                    }
                    "getOrElse" | "Map#getOrElse" => {
                        return match first_type {
                            Some(ValueType::Map(_, value)) => Some(elem_value_type(value)),
                            // On a map with nothing in it the answer is the
                            // fallback, which is what the emitter does -- and
                            // typing it is what lets a `mutable` map that is
                            // grown in a loop settle on a type at all.
                            Some(ValueType::EmptyMap) if arguments.len() == 3 => {
                                self.static_type_under(&arguments[2], locals, depth + 1)
                            }
                            _ => None,
                        };
                    }
                    // `Map#put(m, k, v)`: a map of what the key and value are.
                    // Without this a map grown in a loop stays typed as the
                    // empty map it started as, and every lookup inside the loop
                    // compiles to its fallback.
                    "Map#put" if arguments.len() == 3 => {
                        let key = list_elem_of(self.static_type_under(
                            &arguments[1],
                            locals,
                            depth + 1,
                        )?)?;
                        let value = list_elem_of(self.static_type_under(
                            &arguments[2],
                            locals,
                            depth + 1,
                        )?)?;
                        return Some(ValueType::Map(key, value));
                    }
                    "Map#empty" => return Some(ValueType::EmptyMap),
                    // The enum lowering's allocators. A constructor call like
                    // `Some(x)` becomes one of these, so without them a
                    // function that just wraps a constructor cannot be typed.
                    "__gc_record" | "__gc_alloc" | "__gc_string" => {
                        return Some(ValueType::Ptr);
                    }
                    "__match_fail" => return Some(ValueType::Never),
                    "toString" | "substring" | "trim" | "toUpperCase" | "toLowerCase"
                    | "reverse" | "join" | "replaceAll" | "replace" | "repeat" | "at"
                    | "capitalize" | "stripPrefix" | "stripSuffix" | "trimLeft" | "trimRight" => {
                        return Some(ValueType::Str);
                    }
                    _ => {}
                }
                // A generic enum's constructor: the payload types the call
                // site gives are the shape the value carries.
                if let Some((enum_index, tag, arity)) = self.enum_variant(name)
                    && arity == arguments.len()
                {
                    let mut payload = Vec::with_capacity(arity);
                    for argument in arguments {
                        payload.push(self.static_type_under(argument, locals, depth + 1)?);
                    }
                    if !self.enum_is_generic[enum_index]
                        && let Some(shape) = self.canonical_enum_shape(enum_index, expr.span())
                    {
                        return Some(ValueType::Enum(shape));
                    }
                    if let Some(shape) =
                        self.canonical_applied_enum_shape(enum_index, tag, &payload, expr.span())
                    {
                        return Some(ValueType::Enum(shape));
                    }
                    let shape = self.intern_enum_shape(enum_index, tag, payload);
                    return Some(ValueType::Enum(shape));
                }
                // A generic def's result is its body's, inferred with the
                // parameters bound to these argument types -- the same thing
                // the specialisation itself will do.
                if let Some(generic) = self.generic_functions.iter().rposition(|generic| {
                    generic.name == *name && generic.params.len() == arguments.len()
                }) {
                    let specialised = self.generic_functions[generic].clone();
                    if let Some(declared) = specialised.declared_return {
                        return Some(declared);
                    }
                    // A specialisation already registered for these arguments
                    // answers with its own return type. This is what types a
                    // *recursive* call: the specialisation goes into the table
                    // before its body is compiled, so a call inside that body
                    // gets the real answer here instead of descending into the
                    // body again and running into the depth limit -- and the
                    // depth limit would answer with the base case alone, which
                    // for a function returning `null` or a value is exactly the
                    // half that leaves the other half unboxed.
                    let values: Vec<ValueType> = arguments
                        .iter()
                        .filter_map(|argument| self.static_type_under(argument, locals, depth + 1))
                        .collect();
                    let mut registered = self
                        .specializations
                        .iter()
                        .filter(|((which, _), _)| *which == generic)
                        .map(|(_, index)| &self.functions[*index].1)
                        .filter(|info| {
                            info.params
                                .iter()
                                .map(|(_, ty)| *ty)
                                .eq(values.iter().copied())
                        })
                        .map(|info| info.ret);
                    if let Some(first) = registered.next()
                        && registered.all(|ret| ret == first)
                    {
                        return Some(first);
                    }
                    let generic = specialised;
                    let params = &generic.params;
                    let body = &generic.body;
                    let mut callee_locals = Vec::with_capacity(params.len());
                    // A function argument is not a value, so it cannot be
                    // typed as one: it is bound as a lambda for the callee,
                    // the way compiling the specialisation binds it. Without
                    // this a def that hands a lambda on -- `words()` calling
                    // `stdlibFilter` -- could not be typed at all.
                    let mut callee_lambdas: Vec<(String, usize)> = Vec::new();
                    for (param, argument) in params.iter().zip(arguments.iter()) {
                        if let Expr::Lambda {
                            params: lambda_params,
                            body: lambda_body,
                            ..
                        } = argument
                        {
                            let index = self
                                .intern_lambda(lambda_params.clone(), lambda_body.as_ref().clone());
                            callee_lambdas.push((param.clone(), index));
                            continue;
                        }
                        if let Expr::Identifier { name, .. } = argument
                            && let Some(index) = self.lookup_lambda(name)
                        {
                            callee_lambdas.push((param.clone(), index));
                            continue;
                        }
                        let ty = self.static_type_under(argument, locals, depth + 1)?;
                        callee_locals.push((param.clone(), ty));
                    }
                    if callee_lambdas.is_empty() {
                        return self.static_type_under(body, &mut callee_locals, depth + 1);
                    }
                    self.push_binding_scopes();
                    for (param, lambda) in &callee_lambdas {
                        self.lambda_bindings
                            .last_mut()
                            .expect("emitter lambda scope")
                            .insert(param.clone(), *lambda);
                    }
                    let predicted = self.static_type_under(body, &mut callee_locals, depth + 1);
                    self.pop_binding_scopes();
                    return predicted;
                }
                self.static_type_of(expr)
            }
            _ => self.static_type_of(expr),
        }
    }

    /// Compile a call to a def whose signature was not concrete, by
    /// specialising it for the argument types at this site.
    fn emit_generic_function_call(
        &mut self,
        generic: usize,
        arguments: &[Expr],
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        let GenericFunction {
            name,
            params,
            body,
            declared_return,
            param_annotations,
            return_annotation,
        } = self.generic_functions[generic].clone();
        if params.len() != arguments.len() {
            return Err(Diagnostic::compile(
                span,
                format!(
                    "{name} expects {} argument(s) but got {}",
                    params.len(),
                    arguments.len()
                ),
            ));
        }
        if arguments.len() > ARG_REGS.len() {
            return Err(unsupported(span, "calls with more than 8 arguments"));
        }
        // Arguments first: their types are what selects the specialisation,
        // and a heap-reference one is rooted while the rest are evaluated.
        let mut argument_types = Vec::with_capacity(arguments.len());
        let mut rooted = Vec::with_capacity(arguments.len());
        let mut value_params: Vec<(String, ValueType)> = Vec::new();
        let mut lambda_params: Vec<(String, usize)> = Vec::new();
        let mut dict_params: Vec<(String, Vec<(String, usize)>)> = Vec::new();
        let mut value_annotations: Vec<Option<String>> = Vec::new();
        for (position, (param, argument)) in params.iter().zip(arguments.iter()).enumerate() {
            // A lambda argument is not a value: it is interned and the
            // parameter name is bound to it inside the specialisation, so a
            // higher-order def is specialised per lambda as well as per type.
            if let Expr::Lambda {
                params: lambda_args,
                body: lambda_body,
                ..
            } = argument
            {
                let index = self.intern_lambda(lambda_args.clone(), lambda_body.as_ref().clone());
                lambda_params.push((param.clone(), index));
                continue;
            }
            // A name that is itself bound to a lambda passes the binding
            // along rather than a value -- which is how the prelude's
            // recursive higher-order functions thread their function
            // argument (`stdlibFilter(tail(xs), p)`).
            if let Expr::Identifier { name, .. } = argument
                && let Some(index) = self.lookup_lambda(name)
            {
                lambda_params.push((param.clone(), index));
                continue;
            }
            // Likewise a dictionary of lambdas: `showValue(Show_Int_dict, 42)`
            // hands over the record's field-to-lambda mapping, not a value.
            if let Some(dict) = self.dict_value_of(argument) {
                dict_params.push((param.clone(), dict));
                continue;
            }
            let ty = self.expression(argument)?;
            if ty == ValueType::Unit {
                return Err(unsupported(argument.span(), "a unit-typed argument"));
            }
            if is_heap_pointer(ty) {
                self.push_rooted(Reg::X0);
                rooted.push(true);
            } else {
                self.asm.push(Reg::X0);
                rooted.push(false);
            }
            argument_types.push(ty);
            value_params.push((param.clone(), ty));
            value_annotations.push(param_annotations.get(position).cloned().flatten());
        }
        // Solve the definition's type variables from *all* the arguments, and
        // give an enum-typed parameter whose own argument left them open the
        // shape those solutions imply. `put(KMNil, "a", 1)` says nothing
        // through its first argument -- the empty case carries no payloads --
        // while `key: 'k` and `value: 'v` say everything, and without this the
        // body destructures the other variant against payloads it does not
        // know.
        let mut bindings: Vec<(String, String)> = Vec::new();
        for ((_, ty), annotation) in value_params.iter().zip(value_annotations.iter()) {
            let Some(annotation) = annotation else {
                continue;
            };
            let text = annotation.trim();
            if !text.starts_with('\'') {
                continue;
            }
            let Some(name) = type_annotation_name(*ty) else {
                continue;
            };
            if !bindings.iter().any(|(known, _)| known == text) {
                bindings.push((text.to_string(), name));
            }
        }
        if !bindings.is_empty() {
            let variables: Vec<String> = bindings.iter().map(|(name, _)| name.clone()).collect();
            let solutions: Vec<String> = bindings.iter().map(|(_, name)| name.clone()).collect();
            for position in 0..value_params.len() {
                let ValueType::Enum(shape) = value_params[position].1 else {
                    continue;
                };
                let open = self.enum_shapes[shape as usize]
                    .1
                    .iter()
                    .any(|payload| payload.contains(&ValueType::Never));
                if !open {
                    continue;
                }
                let Some(annotation) = value_annotations[position].clone() else {
                    continue;
                };
                let applied = substitute_type_params(&annotation, &variables, &solutions);
                if applied == annotation {
                    continue;
                }
                if let Ok(resolved @ ValueType::Enum(_)) = self.annotation_type(&applied, span) {
                    value_params[position].1 = resolved;
                    argument_types[position] = resolved;
                }
            }
        }
        // The lambda identities are part of what makes a specialisation
        // distinct, so they join the argument types in the key.
        let mut key_types = argument_types.clone();
        for (_, index) in &lambda_params {
            key_types.push(ValueType::Record(*index as u32));
        }
        for (_, fields) in &dict_params {
            for (_, index) in fields {
                key_types.push(ValueType::Set(ListElem::Nested {
                    depth: 1,
                    base: ScalarElem::Int,
                }));
                key_types.push(ValueType::Record(*index as u32));
            }
        }
        let key = (generic, key_types);
        let fresh = !self.specializations.contains_key(&key);
        let index = match self.specializations.get(&key) {
            Some(&index) => index,
            None => {
                let signature = value_params.clone();
                // A declared return type is the answer when there is one --
                // only the *parameters* were generic in that case.
                //
                // Failing that, a return annotation this backend cannot model
                // on its own is still decided when it is *the same
                // annotation* as one of the parameters: the result is that
                // argument's type. `def id(x: 'a): 'a` and
                // `def pairWithNext<'m>(xs: 'm<Int>): 'm<Int>` both say so
                // without naming a type this backend knows.
                let matched_return = return_annotation.as_ref().and_then(|wanted| {
                    param_annotations
                        .iter()
                        .zip(arguments.iter())
                        .position(|(annotation, _)| annotation.as_deref() == Some(wanted.as_str()))
                        .and_then(|position| argument_types.get(position).copied())
                });
                let mut locals = signature.clone();
                // The lambda parameters must be visible while the body's type
                // is worked out, or a higher-order def's result could not be
                // typed at all.
                // Both kinds of compile-time parameter have to be visible
                // while the body's type is worked out, and both scopes are
                // pushed so the pops below stay symmetric -- an unmatched pop
                // here silently discarded the *enclosing* scope, which is how
                // a second dictionary at top level stopped being found.
                self.lambda_bindings
                    .push(lambda_params.iter().cloned().collect());
                self.dict_bindings
                    .push(dict_params.iter().cloned().collect());
                self.alias_bindings.push(HashMap::new());
                let inferred = declared_return
                    .or(matched_return)
                    .or_else(|| self.static_type_under(&body, &mut locals, 0));
                self.lambda_bindings.pop();
                self.dict_bindings.pop();
                self.alias_bindings.pop();
                // No answer yet is not the same as no answer: a body that calls
                // *another* function which calls back into this one cannot be
                // typed until this specialisation exists to be found. So it is
                // registered with a provisional `Never` -- which merges with
                // anything, so a recursive call contributes nothing rather than
                // poisoning the merge -- and the fixpoint below settles it. If
                // it is still `Never` afterwards, the call really cannot be
                // typed and that is reported there.
                let ret = inferred.unwrap_or(ValueType::Never);
                let label = self.asm.new_label();
                // Registered *before* its body is emitted, so a recursive
                // call inside the body finds this specialisation instead of
                // specialising again forever.
                self.functions.push((
                    format!("{name}#{}", self.specializations.len()),
                    FunctionInfo {
                        label,
                        params: signature,
                        lambda_params: lambda_params.clone(),
                        dict_params: dict_params.clone(),
                        ret,
                        body,
                    },
                ));
                let index = self.functions.len() - 1;
                self.specializations.insert(key, index);
                index
            }
        };
        if fresh {
            // The first guess at the return type came from a body in which the
            // recursive call had nothing to resolve to yet, so it is the base
            // case's answer alone: `myFoldLeft` starting from `[]` looked like
            // it returned the empty list. Now that the specialisation is
            // registered the recursion resolves, so take the fixpoint and
            // correct the registration -- before the body is compiled, since
            // the body is checked against it.
            for _ in 0..8 {
                let (params, body, current) = {
                    let info = &self.functions[index].1;
                    (info.params.clone(), info.body.clone(), info.ret)
                };
                let mut locals: Vec<(String, ValueType)> = params
                    .iter()
                    .map(|(name, ty)| (name.clone(), *ty))
                    .collect();
                self.push_binding_scopes();
                for (param, lambda) in &lambda_params {
                    self.lambda_bindings
                        .last_mut()
                        .expect("emitter lambda scope")
                        .insert(param.clone(), *lambda);
                }
                for (param, fields) in &dict_params {
                    self.dict_bindings
                        .last_mut()
                        .expect("emitter dict scope")
                        .insert(param.clone(), fields.clone());
                }
                let predicted = self.static_type_under(&body, &mut locals, 0);
                self.pop_binding_scopes();
                let Some(predicted) = predicted else {
                    break;
                };
                let Some(merged) = self.merge_types(current, predicted) else {
                    break;
                };
                if merged == current {
                    break;
                }
                self.functions[index].1.ret = merged;
            }
        }
        let (label, ret) = {
            let info = &self.functions[index].1;
            (info.label, info.ret)
        };
        if ret == ValueType::Never {
            return Err(unsupported(
                span,
                &format!("a call to `{name}`, whose result type cannot be determined"),
            ));
        }
        for (position, register) in ARG_REGS.iter().take(argument_types.len()).enumerate().rev() {
            if rooted[position] {
                self.pop_rooted(*register);
            } else {
                self.asm.pop(*register);
            }
        }
        self.asm.branch(label, BranchKind::Link);
        self.pending.push(index);
        Ok(ret)
    }

    /// `foldLeft(list)(initial)(f)`: thread an accumulator through the list,
    /// `acc = f(acc, element)` per element.
    ///
    /// Like the map above, the lambda's body is compiled once inside the loop,
    /// so the fold runs at run time over a list of any length. The
    /// accumulator's type has to be stable across iterations -- it is the
    /// initial value's type, and a body that produced something else would be
    /// a different function on the second iteration -- so that is checked
    /// rather than assumed.
    fn emit_list_fold_left(
        &mut self,
        list: &Expr,
        initial: &Expr,
        acc_param: &str,
        elem_param: &str,
        body: &Expr,
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        let mark = self.open_temp_roots();
        let initial_ty = self.expression(initial)?;
        if initial_ty == ValueType::Unit {
            return Err(unsupported(initial.span(), "a unit-typed accumulator"));
        }
        let acc = self.reserve_locals(32);
        let cursor = acc + 8;
        let acc_slot = cursor + 8;
        let elem_slot = acc_slot + 8;
        self.asm.store_local(Reg::X0, acc);
        if is_heap_pointer(initial_ty) {
            self.emit_root_frame_slot(acc);
        }
        let list_ty = self.expression(list)?;
        let elem = match list_ty {
            ValueType::List(elem) => elem,
            // Folding an empty list is the initial value.
            ValueType::EmptyList => return Ok(initial_ty),
            _ => return Err(unsupported(list.span(), "folding over this value")),
        };
        self.asm.store_local(Reg::X0, cursor);
        self.emit_root_frame_slot(cursor);

        let elem_ty = elem_value_type(elem);
        // The accumulator's type can be *widened* by the body:
        // `foldLeft(xss)([])((acc, xs) => concat(acc, xs))` starts from the
        // polymorphic empty list and ends up holding a real list, so typing the
        // body against the initial value alone would reject the fold. Settle
        // the type first by inference -- this emits nothing -- and compile the
        // body once against the settled type. Widening only ever turns the
        // empty list/set into a list/set, both of which are heap pointers
        // already, so the accumulator's rooting above is unaffected.
        let mut acc_ty = initial_ty;
        for _ in 0..3 {
            let mut locals = vec![
                (acc_param.to_string(), acc_ty),
                (elem_param.to_string(), elem_ty),
            ];
            let Some(folded) = self.static_type_under(body, &mut locals, 0) else {
                break;
            };
            let Some(merged) = self.merge_types(acc_ty, folded) else {
                break;
            };
            if merged == acc_ty {
                break;
            }
            acc_ty = merged;
        }
        let loop_start = self.asm.new_label();
        let done = self.asm.new_label();
        self.asm.bind(loop_start);
        self.asm.load_local(Reg::X0, cursor);
        self.asm.branch(done, BranchKind::CompareZero(Reg::X0));
        self.emit_gc_load_ptr(Reg::X0, 0);
        if is_boxed_scalar(elem_ty) {
            self.emit_unbox_scalar();
        }
        self.asm.store_local(Reg::X0, elem_slot);
        self.asm.load_local(Reg::X0, acc);
        self.asm.store_local(Reg::X0, acc_slot);
        self.push_binding_scopes();
        self.scope_root_counts.push(0);
        {
            let scope = self.scopes.last_mut().expect("emitter scope");
            scope.insert(acc_param.to_string(), (acc_slot, acc_ty));
            scope.insert(elem_param.to_string(), (elem_slot, elem_ty));
        }
        if is_heap_pointer(acc_ty) {
            self.emit_root_frame_slot(acc_slot);
        }
        if is_heap_pointer(elem_ty) {
            self.emit_root_frame_slot(elem_slot);
        }
        let folded = self.expression(body)?;
        if !assignable(folded, acc_ty) {
            return Err(unsupported(
                body.span(),
                "a fold whose accumulator changes type",
            ));
        }
        self.asm.store_local(Reg::X0, acc);
        self.pop_binding_scopes();
        let roots = self.scope_root_counts.pop().expect("emitter root scope");
        self.emit_shadow_pop(roots);
        self.asm.load_local(Reg::X0, cursor);
        self.emit_gc_load_ptr(Reg::X0, 8);
        self.asm.store_local(Reg::X0, cursor);
        self.asm.branch(loop_start, BranchKind::Unconditional);
        self.asm.bind(done);
        self.asm.load_local(Reg::X0, acc);
        self.close_temp_roots(mark);
        let _ = span;
        Ok(acc_ty)
    }

    /// Open a scope for all four kinds of compile-time binding at once.
    ///
    /// These stacks have to move in lockstep: an unmatched pop discards the
    /// *enclosing* scope, and the symptom is a binding that silently stops
    /// existing (or a panic on an empty stack). Two bugs of exactly that shape
    /// happened while these were pushed one by one, so opening and closing
    /// them is a single operation now.
    fn push_binding_scopes(&mut self) {
        self.scopes.push(HashMap::new());
        self.lambda_bindings.push(HashMap::new());
        self.dict_bindings.push(HashMap::new());
        self.alias_bindings.push(HashMap::new());
        self.literal_strings.push(HashMap::new());
        self.literal_doubles.push(HashMap::new());
    }

    /// Close the scopes `push_binding_scopes` opened.
    fn pop_binding_scopes(&mut self) {
        self.scopes.pop();
        self.lambda_bindings.pop();
        self.dict_bindings.pop();
        self.alias_bindings.pop();
        self.literal_strings.pop();
        self.literal_doubles.pop();
    }

    /// The value of a `Double` expression, when it is one this backend can work
    /// out while compiling. Only pure shapes are folded -- literals, arithmetic
    /// on them, and names bound to one by a `val` -- so folding an expression
    /// away can never skip an effect.
    fn static_double_of(&self, expr: &Expr) -> Option<f64> {
        match expr {
            Expr::Double { value, .. } => Some(*value),
            Expr::Identifier { name, .. } => self
                .literal_doubles
                .iter()
                .rev()
                .find_map(|scope| scope.get(name))
                .copied(),
            Expr::Unary { op, expr, .. } => match op {
                UnaryOp::Minus => Some(-self.static_double_of(expr)?),
                UnaryOp::Plus => self.static_double_of(expr),
                _ => None,
            },
            Expr::Binary { lhs, op, rhs, .. } => {
                let left = self.static_double_of(lhs)?;
                let right = self.static_double_of(rhs)?;
                match op {
                    BinaryOp::Add => Some(left + right),
                    BinaryOp::Subtract => Some(left - right),
                    BinaryOp::Multiply => Some(left * right),
                    BinaryOp::Divide => Some(left / right),
                    _ => None,
                }
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                let Expr::Identifier { name, .. } = callee.as_ref() else {
                    return None;
                };
                if arguments.len() != 1 {
                    return None;
                }
                match name.as_str() {
                    "sqrt" => Some(self.static_double_of(&arguments[0])?.sqrt()),
                    "abs" => Some(self.static_double_of(&arguments[0])?.abs()),
                    // `double(n)` widens an Int, so its argument is one.
                    "double" => match &arguments[0] {
                        Expr::Int { value, .. } => Some(*value as f64),
                        other => self.static_double_of(other),
                    },
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// A name aliasing another name, innermost scope first.
    fn lookup_alias(&self, name: &str) -> Option<String> {
        self.alias_bindings
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }

    /// A name bound to a dictionary of lambdas, innermost scope first.
    /// Intern a lambda together with the compile-time environment it was
    /// written under, and answer with its index.
    fn intern_lambda(&mut self, params: Vec<String>, body: Expr) -> usize {
        // Outer scopes first, so an inner binding of the same name wins when
        // the environment is installed in order.
        let env = CapturedEnv {
            lambdas: self
                .lambda_bindings
                .iter()
                .flatten()
                .map(|(name, index)| (name.clone(), *index))
                .collect(),
            dicts: self
                .dict_bindings
                .iter()
                .flatten()
                .map(|(name, fields)| (name.clone(), fields.clone()))
                .collect(),
        };
        self.lambdas.push((params, body));
        self.lambda_envs.push(env);
        self.lambdas.len() - 1
    }

    /// The field-to-lambda mapping of an expression that denotes a dictionary
    /// at compile time.
    ///
    /// Three shapes, and the third is the point: a name bound to one, a record
    /// literal whose fields are all lambdas, and a *call* whose result is one
    /// -- `Show_List_dict(Show_Int_dict)`, a dictionary parameterised by
    /// another. That call is evaluated here rather than at run time, with the
    /// argument bound as a dictionary inside, so the lambdas in the record it
    /// answers with capture it.
    ///
    /// Interning lambdas is all this does; it emits no code, so asking whether
    /// an arbitrary argument happens to be a dictionary costs nothing but a few
    /// unused table entries.
    fn dict_value_of(&mut self, expr: &Expr) -> Option<Vec<(String, usize)>> {
        match expr {
            Expr::Identifier { name, .. } => self.lookup_dict(name),
            Expr::RecordLiteral { fields, .. }
                if !fields.is_empty()
                    && fields
                        .iter()
                        .all(|(_, value)| matches!(value, Expr::Lambda { .. })) =>
            {
                let mut mapping = Vec::with_capacity(fields.len());
                for (field, value) in fields {
                    let Expr::Lambda { params, body, .. } = value else {
                        unreachable!("checked in the guard");
                    };
                    let index = self.intern_lambda(params.clone(), body.as_ref().clone());
                    mapping.push((field.clone(), index));
                }
                Some(mapping)
            }
            Expr::Block { expressions, .. } if expressions.len() == 1 => {
                self.dict_value_of(&expressions[0])
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                let Expr::Identifier { name, .. } = callee.as_ref() else {
                    return None;
                };
                let index = self.lookup_lambda(name)?;
                let (params, body) = self.lambdas[index].clone();
                if params.len() != arguments.len() {
                    return None;
                }
                self.push_binding_scopes();
                self.install_captured_env(index);
                let mut bound = true;
                for (param, argument) in params.iter().zip(arguments.iter()) {
                    if let Some(dict) = self.dict_value_of(argument) {
                        self.dict_bindings
                            .last_mut()
                            .expect("emitter dict scope")
                            .insert(param.clone(), dict);
                        continue;
                    }
                    if let Expr::Identifier { name, .. } = argument
                        && let Some(lambda) = self.lookup_lambda(name)
                    {
                        self.lambda_bindings
                            .last_mut()
                            .expect("emitter lambda scope")
                            .insert(param.clone(), lambda);
                        continue;
                    }
                    // A value argument cannot be bound without emitting code,
                    // so this is not a compile-time dictionary after all.
                    bound = false;
                    break;
                }
                let mapping = if bound {
                    self.dict_value_of(&body)
                } else {
                    None
                };
                self.pop_binding_scopes();
                mapping
            }
            _ => None,
        }
    }

    /// Install a lambda's captured environment in the innermost scope. Adds to
    /// what is already visible rather than replacing it: the names a lambda
    /// closed over resolve to what they meant then, and nothing else moves.
    fn install_captured_env(&mut self, index: usize) {
        let env = self.lambda_envs[index].clone();
        for (name, lambda) in env.lambdas {
            self.lambda_bindings
                .last_mut()
                .expect("emitter lambda scope")
                .insert(name, lambda);
        }
        for (name, fields) in env.dicts {
            self.dict_bindings
                .last_mut()
                .expect("emitter dict scope")
                .insert(name, fields);
        }
    }

    fn lookup_dict(&self, name: &str) -> Option<Vec<(String, usize)>> {
        self.dict_bindings
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }

    /// A name bound to a lambda, innermost scope first.
    /// The text of an argument that is a string literal, directly or through a
    /// `val` bound to one. Interpolation is deliberately excluded: its value is
    /// not known until it runs.
    fn literal_string(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::String { value, .. } if !value.contains("#{") => Some(value.clone()),
            Expr::Identifier { name, .. } => self
                .literal_strings
                .iter()
                .rev()
                .find_map(|scope| scope.get(name))
                .cloned(),
            _ => None,
        }
    }

    fn lookup_lambda(&self, name: &str) -> Option<usize> {
        self.lambda_bindings
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    /// The parameters and body of a lambda argument, whether it was written
    /// out at the call site or passed in under a name -- a higher-order def
    /// hands its own lambda parameter on, so both spellings turn up.
    fn lambda_argument(&self, expr: &Expr) -> Option<(Vec<String>, Expr)> {
        match expr {
            Expr::Lambda { params, body, .. } => Some((params.clone(), (**body).clone())),
            Expr::Identifier { name, span } => {
                if let Some(index) = self.lookup_lambda(name) {
                    let (params, body) = &self.lambdas[index];
                    return Some((params.clone(), body.clone()));
                }
                // A *definition* passed where a function is expected --
                // `foldLeft(xs)([])(insertSorted)`, which is how `std.list`'s
                // sort is written. Wrapping it in a lambda that forwards its
                // arguments is what makes it a value here, since this backend
                // has no way to pass one.
                let arity = self
                    .functions
                    .iter()
                    .find(|(n, _)| n == name)
                    .map(|(_, info)| info.params.len())
                    .or_else(|| {
                        self.generic_functions
                            .iter()
                            .rev()
                            .find(|generic| generic.name == *name)
                            .map(|generic| generic.params.len())
                    })?;
                let params: Vec<String> = (0..arity).map(|i| format!("__arg{i}")).collect();
                let arguments = params
                    .iter()
                    .map(|param| Expr::Identifier {
                        name: param.clone(),
                        span: *span,
                    })
                    .collect();
                Some((
                    params,
                    Expr::Call {
                        callee: Box::new(Expr::Identifier {
                            name: name.clone(),
                            span: *span,
                        }),
                        arguments,
                        span: *span,
                    },
                ))
            }
            // `{ (x) => ... }` and `{x, y => ...}`: a braced block whose only
            // expression is the lambda, which is how a trailing block argument
            // is written.
            Expr::Block { expressions, .. } if expressions.len() == 1 => {
                self.lambda_argument(&expressions[0])
            }
            _ => None,
        }
    }

    /// The type of mapping `list` through `f`: a list of whatever the lambda's
    /// body produces with its parameter bound to the element type. Inference
    /// only -- nothing is emitted.
    fn mapped_list_type(
        &mut self,
        list: &Expr,
        f: &Expr,
        locals: &mut Vec<(String, ValueType)>,
        depth: u32,
    ) -> Option<ValueType> {
        let elem = match self.static_type_under(list, locals, depth + 1)? {
            ValueType::List(elem) => elem,
            _ => return None,
        };
        let (params, body) = self.lambda_argument(f)?;
        if params.len() != 1 {
            return None;
        }
        locals.push((params[0].clone(), elem_value_type(elem)));
        let result = self.static_type_under(&body, locals, depth + 1);
        locals.pop();
        list_elem_of(result?).map(ValueType::List)
    }

    /// Compile a call to a lambda-bound name by compiling the lambda's body
    /// here, with its parameters bound to the arguments' slots and types.
    ///
    /// This is why no closure object is needed: the body is compiled in the
    /// caller's frame, so a captured local is simply still in scope. The
    /// parameter slots are released afterwards, so N call sites reuse them
    /// rather than each growing the frame.
    fn emit_inline_lambda_call(
        &mut self,
        index: usize,
        arguments: &[Expr],
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        let (params, body) = self.lambdas[index].clone();
        if params.len() != arguments.len() {
            return Err(Diagnostic::compile(
                span,
                format!(
                    "this lambda expects {} argument(s) but got {}",
                    params.len(),
                    arguments.len()
                ),
            ));
        }
        // Evaluate the arguments in the caller's scope, before the parameters
        // shadow anything, and park each one in its own slot.
        let mut bound = Vec::with_capacity(params.len());
        let saved_offset = self.next_local_offset;
        for argument in arguments {
            let ty = self.expression(argument)?;
            if ty == ValueType::Unit {
                return Err(unsupported(argument.span(), "a unit-typed argument"));
            }
            let offset = self.reserve_locals(8);
            self.asm.store_local(Reg::X0, offset);
            bound.push((offset, ty));
        }
        self.push_binding_scopes();
        // What the lambda closed over, before its parameters shadow anything.
        self.install_captured_env(index);
        self.scope_root_counts.push(0);
        for (param, (offset, ty)) in params.iter().zip(bound.iter()) {
            self.scopes
                .last_mut()
                .expect("emitter scope")
                .insert(param.clone(), (*offset, *ty));
            // A heap-reference parameter is a root for the body's duration,
            // exactly like a binding of the same type would be.
            if is_heap_pointer(*ty) {
                self.emit_root_frame_slot(*offset);
            }
        }
        let result = self.expression(&body)?;
        self.pop_binding_scopes();
        let roots = self.scope_root_counts.pop().expect("emitter root scope");
        self.emit_shadow_pop(roots);
        // The parameter slots are dead now; let the next call reuse them.
        self.release_locals_to(saved_offset);
        Ok(result)
    }

    /// Widen the slots of any bindings a loop body assigns to, before that
    /// body is compiled.
    ///
    /// A `mutable` takes its type from what is assigned to it, and the
    /// declaration site can settle that for straight-line code. A loop cannot
    /// be settled there: `m = Map#put(m, k, ...)` mentions the loop's own
    /// variable, which does not exist yet at the declaration. So the same
    /// fixpoint is taken again here, with the loop variable bound -- otherwise
    /// the body's *reads* compile against the type the binding started with
    /// (`Map#empty()` is the empty map, and a lookup on one is its fallback),
    /// which is correct on the first turn and wrong on every turn after it.
    fn widen_loop_mutables(&mut self, body: &Expr) {
        let names: Vec<String> = self
            .scopes
            .iter()
            .flatten()
            .map(|(name, _)| name.clone())
            .collect();
        for name in names {
            let mut assigned = Vec::new();
            collect_assignments(body, &name, &mut assigned);
            if assigned.is_empty() {
                continue;
            }
            let Some((slot, current)) = self.lookup(&name) else {
                continue;
            };
            let mut widened = current;
            for _ in 0..4 {
                let mut grew = false;
                for value in &assigned {
                    let mut locals = Vec::new();
                    let Some(ty) = self.static_type_under(value, &mut locals, 0) else {
                        continue;
                    };
                    let Some(merged) = self.merge_types(widened, ty) else {
                        continue;
                    };
                    if merged != widened {
                        widened = merged;
                        grew = true;
                    }
                }
                if !grew {
                    break;
                }
            }
            if widened == current {
                continue;
            }
            // Both the old type and the new one are heap references, so the
            // root the slot already has still covers it.
            for scope in self.scopes.iter_mut().rev() {
                if let Some(entry) = scope.get_mut(&name) {
                    *entry = (slot, widened);
                    break;
                }
            }
        }
    }

    /// The generator's state cell, reserved on first use so a program that
    /// never asks for a random number carries no data section for one.
    fn random_state_cell(&mut self) -> DataLabel {
        match self.random_state_cell {
            Some(cell) => cell,
            None => {
                let cell = self.asm.reserve_data_cells(1);
                self.random_state_cell = Some(cell);
                cell
            }
        }
    }

    /// The name an `extension (this: T) { ... }` block is keyed by, worked out
    /// from what the receiver's type turns out to be. Typing it emits nothing,
    /// so asking costs only the inference.
    fn extension_type_key_of(&mut self, receiver: &Expr) -> Option<String> {
        let mut locals = Vec::new();
        let ty = self.static_type_under(receiver, &mut locals, 0)?;
        Some(match ty {
            ValueType::Str => "String".to_string(),
            ValueType::Int => "Int".to_string(),
            ValueType::Bool => "Boolean".to_string(),
            ValueType::Double => "Double".to_string(),
            ValueType::List(_) | ValueType::EmptyList => "List".to_string(),
            ValueType::Set(_) | ValueType::EmptySet => "Set".to_string(),
            ValueType::Map(..) | ValueType::EmptyMap => "Map".to_string(),
            ValueType::Enum(shape) => {
                let (enum_index, _) = self.enum_shapes.get(shape as usize)?;
                self.generic_enums.get(*enum_index)?.0.clone()
            }
            ValueType::Record(index) => self.records.get(index as usize)?.name.clone(),
            _ => return None,
        })
    }

    /// Is there a definition of this name that takes this many arguments,
    /// concrete or generic? Asked before turning a method call into one.
    fn function_exists(&self, name: &str, arity: usize) -> bool {
        self.functions
            .iter()
            .any(|(n, info)| n == name && info.params.len() == arity)
            || self
                .generic_functions
                .iter()
                .any(|generic| generic.name == name && generic.params.len() == arity)
    }

    fn function_call(
        &mut self,
        name: &str,
        arguments: &[Expr],
        span: Span,
    ) -> Result<ValueType, Diagnostic> {
        // rposition: a later (user) definition shadows an earlier
        // (prelude) one, matching evaluator scoping.
        //
        // Typeclass instances break the one-name-one-function assumption:
        // `instance Show<Int>` and `instance Show<String>` both declare
        // `show`, and which one a call means is decided by the argument's
        // type. So when a name has several definitions, prefer the one whose
        // parameters accept the arguments -- typed without emitting code, so
        // the choice happens before any argument is compiled. A single
        // definition keeps the old path exactly, and an unresolvable
        // overload falls back to the innermost one, whose parameter check
        // then produces the diagnostic.
        let candidates: Vec<usize> = self
            .functions
            .iter()
            .enumerate()
            .filter(|(_, (n, _))| n == name)
            .map(|(index, _)| index)
            .collect();
        let Some(&fallback) = candidates.last() else {
            // No concrete definition: a def whose signature was not fully
            // annotated is compiled per call site instead.
            if let Some(generic) = self.generic_functions.iter().rposition(|generic| {
                generic.name == name && generic.params.len() == arguments.len()
            }) {
                return self.emit_generic_function_call(generic, arguments, span);
            }
            return Err(unsupported(span, &format!("function `{name}`")));
        };
        let index = if candidates.len() == 1 {
            fallback
        } else {
            let argument_types: Vec<Option<ValueType>> =
                arguments.iter().map(|a| self.static_type_of(a)).collect();
            let accepted = candidates.iter().rev().copied().find(|&candidate| {
                let params = &self.functions[candidate].1.params;
                params.len() == arguments.len()
                    && params
                        .iter()
                        .zip(argument_types.iter())
                        .all(|((_, expected), actual)| {
                            actual.is_some_and(|actual| assignable(actual, *expected))
                        })
            });
            match accepted {
                Some(index) => index,
                // No concrete overload accepts these arguments. One of the
                // instances may still be the answer without being concrete:
                // `instance Show<List<'a>>` declares its `show` over a type
                // variable, so it is compiled per call site like any other
                // signature that is not fully annotated.
                None => {
                    if let Some(generic) = self.generic_functions.iter().rposition(|generic| {
                        generic.name == name && generic.params.len() == arguments.len()
                    }) {
                        return self.emit_generic_function_call(generic, arguments, span);
                    }
                    fallback
                }
            }
        };
        let (label, params, ret) = {
            let info = &self.functions[index].1;
            (info.label, info.params.clone(), info.ret)
        };
        if arguments.len() != params.len() {
            return Err(Diagnostic::compile(
                span,
                format!(
                    "{name} expects {} {} but got {}",
                    params.len(),
                    if params.len() == 1 {
                        "argument"
                    } else {
                        "arguments"
                    },
                    arguments.len()
                ),
            ));
        }
        if arguments.len() > ARG_REGS.len() {
            return Err(unsupported(span, "calls with more than 8 arguments"));
        }
        // Each evaluated argument waits on the machine stack while the
        // remaining ones are evaluated, and those can allocate -- so a
        // heap-reference argument is parked as a root for that window, the
        // same discipline every other multi-operand site follows.
        let mut rooted = Vec::with_capacity(arguments.len());
        for (argument, (_, expected)) in arguments.iter().zip(params.iter()) {
            let ty = self.expression(argument)?;
            if !assignable(ty, *expected) {
                return Err(unsupported(argument.span(), "an argument of this type"));
            }
            if is_heap_pointer(ty) {
                self.push_rooted(Reg::X0);
                rooted.push(true);
            } else {
                self.asm.push(Reg::X0);
                rooted.push(false);
            }
        }
        for (position, register) in ARG_REGS.iter().take(arguments.len()).enumerate().rev() {
            if rooted[position] {
                self.pop_rooted(*register);
            } else {
                self.asm.pop(*register);
            }
        }
        self.asm.branch(label, BranchKind::Link);
        self.pending.push(index);
        Ok(ret)
    }

    /// Emit one collected function: prologue saves the frame record
    /// and binds parameters to frame-pointer slots, the body is a
    /// single expression whose value stays in x0.
    fn emit_function(&mut self, index: usize) -> Result<(), Diagnostic> {
        let (label, params, lambda_params, dict_params, ret, body) = {
            let info = &self.functions[index].1;
            (
                info.label,
                info.params.clone(),
                info.lambda_params.clone(),
                info.dict_params.clone(),
                info.ret,
                info.body.clone(),
            )
        };
        self.asm.bind(label);
        self.asm.push_frame_record();
        let frame_patch = self.asm.sub_sp_placeholder();
        self.asm.mov_fp_sp();

        self.push_binding_scopes();
        self.scope_root_counts.push(0);
        for (param, lambda) in &lambda_params {
            self.lambda_bindings
                .last_mut()
                .expect("emitter lambda scope")
                .insert(param.clone(), *lambda);
        }
        for (param, fields) in &dict_params {
            self.dict_bindings
                .last_mut()
                .expect("emitter dict scope")
                .insert(param.clone(), fields.clone());
        }
        let saved_offset = self.next_local_offset;
        let saved_max = self.max_local_offset;
        self.next_local_offset = 0;
        self.max_local_offset = 0;
        let mut param_slots = Vec::new();
        for (position, (param, ty)) in params.iter().enumerate() {
            let offset = self.declare_local(param, *ty);
            self.asm.store_local(ARG_REGS[position], offset);
            if is_heap_pointer(*ty) {
                param_slots.push(offset);
            }
        }
        // Root the heap-reference parameters only after every argument
        // register has been spilled -- the root push itself uses x0.
        for offset in param_slots {
            self.emit_root_frame_slot(offset);
        }
        let body_ty = self.expression(&body)?;
        self.pop_binding_scopes();
        // Drop this activation's roots before returning; the result is in
        // x0, which the pop helper preserves.
        let roots = self.scope_root_counts.pop().expect("emitter root scope");
        self.emit_shadow_pop(roots);
        // The body is compiled, so the frame's real size is known: fill in the
        // reservation and take it back down the same amount.
        let frame_size = self.max_local_offset.div_ceil(16) * 16;
        if frame_size >= 4096 {
            return Err(unsupported(body.span(), "this many local variables"));
        }
        self.asm.patch_sub_sp(frame_patch, frame_size);
        self.next_local_offset = saved_offset;
        self.max_local_offset = saved_max;
        // A body narrower than the registered return type is fine: two shapes
        // of one enum have the same representation, and the wider one is what
        // callers were told. Merging is how "narrower" is decided, since what a
        // shape knows lives in a table here.
        let fits = assignable(body_ty, ret)
            || self
                .merge_types(body_ty, ret)
                .is_some_and(|merged| self.same_shape(merged, ret));
        if !fits {
            return Err(unsupported(
                body.span(),
                "a function body of a different type than its return annotation",
            ));
        }

        if frame_size > 0 {
            self.asm.add_sp_imm(frame_size);
        }
        self.asm.pop_frame_record();
        self.asm.ret();
        Ok(())
    }

    /// Render a `println` argument that is a compile-time literal to
    /// its evaluator-identical line. Doubles go through Rust's `f64`
    /// Display, the same formatter the evaluator and klassic_rt use.
    fn literal_line(argument: &Expr) -> Result<Option<String>, Diagnostic> {
        match argument {
            Expr::String { value, span } => {
                if value.contains("#{") {
                    return Err(unsupported(*span, "string interpolation"));
                }
                Ok(Some(format!("{value}\n")))
            }
            Expr::Double { value, .. } => Ok(Some(format!("{}\n", format_double(*value)))),
            _ => Ok(None),
        }
    }

    fn println_call(&mut self, arguments: &[Expr], span: Span) -> Result<(), Diagnostic> {
        if arguments.len() != 1 {
            return Err(Diagnostic::compile(
                span,
                format!("println expects 1 argument but got {}", arguments.len()),
            ));
        }
        let argument = &arguments[0];
        if let Some(line) = Self::literal_line(argument)? {
            self.asm.emit_write_rodata(STDOUT_FD, line.as_bytes());
            return Ok(());
        }
        if let Some(number) = self.static_double_of(argument) {
            let line = format!("{}\n", format_double(number));
            self.asm.emit_write_rodata(STDOUT_FD, line.as_bytes());
            return Ok(());
        }
        match self.expression(argument)? {
            ValueType::Int => {
                self.asm.emit_print_int_line();
                Ok(())
            }
            ValueType::Str => {
                self.emit_println_str();
                Ok(())
            }
            ValueType::List(elem) => {
                self.emit_println_list(elem, span)?;
                Ok(())
            }
            ValueType::EmptyList => {
                self.asm.emit_write_rodata(STDOUT_FD, b"[]\n");
                Ok(())
            }
            ValueType::Set(elem) => {
                self.emit_println_set(elem, span)?;
                Ok(())
            }
            ValueType::EmptySet => {
                self.asm.emit_write_rodata(STDOUT_FD, b"%()\n");
                Ok(())
            }
            ValueType::Map(key, value) => {
                self.emit_println_map(key, value, span)?;
                Ok(())
            }
            ValueType::EmptyMap => {
                self.asm.emit_write_rodata(STDOUT_FD, b"%[]\n");
                Ok(())
            }
            ValueType::Record(index) => {
                self.emit_println_record(index, argument.span())?;
                Ok(())
            }
            ValueType::Bool => {
                let false_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm
                    .branch(false_label, BranchKind::CompareZero(Reg::X0));
                self.asm.emit_write_rodata(STDOUT_FD, b"true\n");
                self.asm.branch(end_label, BranchKind::Unconditional);
                self.asm.bind(false_label);
                self.asm.emit_write_rodata(STDOUT_FD, b"false\n");
                self.asm.bind(end_label);
                Ok(())
            }
            ValueType::Null => {
                self.asm.emit_write_rodata(STDOUT_FD, b"null\n");
                Ok(())
            }
            // `null` or a value: the evaluator prints the word `null` for the
            // empty case, and the value itself otherwise.
            ValueType::Nullable(elem) => {
                let null_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm
                    .branch(null_label, BranchKind::CompareZero(Reg::X0));
                let inner = elem_value_type(elem);
                if is_boxed_scalar(inner) {
                    self.emit_unbox_scalar();
                }
                match elem {
                    ListElem::Int => self.asm.emit_print_int_line(),
                    ListElem::Str => self.emit_println_str(),
                    ListElem::Bool => {
                        let false_label = self.asm.new_label();
                        let bool_end = self.asm.new_label();
                        self.asm
                            .branch(false_label, BranchKind::CompareZero(Reg::X0));
                        self.asm.emit_write_rodata(STDOUT_FD, b"true\n");
                        self.asm.branch(bool_end, BranchKind::Unconditional);
                        self.asm.bind(false_label);
                        self.asm.emit_write_rodata(STDOUT_FD, b"false\n");
                        self.asm.bind(bool_end);
                    }
                    ListElem::Double
                    | ListElem::Enum(_)
                    | ListElem::Record(_)
                    | ListElem::Nested { .. } => {
                        return Err(unsupported(
                            argument.span(),
                            &format!("printing a nullable {elem:?}"),
                        ));
                    }
                }
                self.asm.branch(end_label, BranchKind::Unconditional);
                self.asm.bind(null_label);
                self.asm.emit_write_rodata(STDOUT_FD, b"null\n");
                self.asm.bind(end_label);
                Ok(())
            }
            ValueType::Enum(shape) => {
                self.emit_print_enum_inline(shape, argument.span())?;
                self.asm.emit_write_rodata(STDOUT_FD, b"\n");
                Ok(())
            }
            other => Err(unsupported(
                argument.span(),
                &format!("printing a {other:?} value"),
            )),
        }
    }

    fn statement(&mut self, expr: &Expr) -> Result<(), Diagnostic> {
        match expr {
            Expr::Block { expressions, .. } => {
                self.push_binding_scopes();
                self.scope_root_counts.push(0);
                self.collect_block_widenings(expressions);
                for expression in expressions {
                    self.statement(expression)?;
                }
                self.pop_binding_scopes();
                let roots = self.scope_root_counts.pop().expect("emitter root scope");
                self.emit_shadow_pop(roots);
                Ok(())
            }
            // Declarations have no runtime effect in the current
            // subset; calling a declared function is rejected at the
            // call site.
            // A typeclass declaration is type-level only, and an instance's
            // methods were already collected as functions, so neither emits
            // code here.
            // An enum declaration is likewise type-level by the time it gets
            // here: the lowering has already turned its constructors and
            // matches into records and calls.
            Expr::ModuleHeader { .. }
            | Expr::Import { .. }
            | Expr::DefDecl { .. }
            | Expr::RecordDeclaration { .. }
            | Expr::EnumDeclaration { .. }
            | Expr::TypeClassDeclaration { .. }
            | Expr::InstanceDeclaration { .. } => Ok(()),
            Expr::VarDecl {
                name,
                value,
                mutable,
                ..
            } => {
                // Remember a string literal's text for the builtins that need
                // it at compile time, and forget any older binding of this name
                // so a shadowing one does not inherit the wrong text.
                {
                    let literal = match value.as_ref() {
                        Expr::String { value, .. } if !value.contains("#{") => Some(value.clone()),
                        _ => None,
                    };
                    let scope = self
                        .literal_strings
                        .last_mut()
                        .expect("emitter literal scope");
                    match literal {
                        Some(text) => scope.insert(name.clone(), text),
                        None => scope.remove(name),
                    };
                }
                {
                    let folded = if *mutable {
                        None
                    } else {
                        self.static_double_of(value)
                    };
                    let scope = self
                        .literal_doubles
                        .last_mut()
                        .expect("emitter literal scope");
                    match folded {
                        Some(number) => scope.insert(name.clone(), number),
                        None => scope.remove(name),
                    };
                }
                // `val f = (x) => ...`: bind the name to the lambda body
                // instead of to a slot. There is nothing to store -- the body
                // is compiled at each call site, where the parameter types
                // are known from the arguments.
                if let Expr::Lambda { params, body, .. } = value.as_ref() {
                    let index = self.intern_lambda(params.clone(), body.as_ref().clone());
                    self.lambda_bindings
                        .last_mut()
                        .expect("emitter lambda scope")
                        .insert(name.clone(), index);
                    return Ok(());
                }
                // `val Show_Int_dict = record { show: (x: Int) => ... }`: a
                // record whose fields are all lambdas is the dictionary-passing
                // shape, and like a lambda it lives only at compile time --
                // the name is bound to its field-to-lambda mapping and
                // `dict.show(x)` compiles the body at the call.
                // `val sub = substring`: a bare name on the right that is not
                // a value in this backend -- a builtin or a function -- becomes
                // an alias resolved at each call.
                if let Expr::Identifier { name: target, .. } = value.as_ref()
                    && self.lookup(target).is_none()
                    && self.lookup_lambda(target).is_none()
                    && self.lookup_dict(target).is_none()
                    // A nullary constructor is a value, not another name.
                    && self.enum_variant(target).is_none()
                {
                    let target = self.lookup_alias(target).unwrap_or_else(|| target.clone());
                    self.alias_bindings
                        .last_mut()
                        .expect("emitter alias scope")
                        .insert(name.clone(), target);
                    return Ok(());
                }
                if let Expr::RecordLiteral { fields, .. } = value.as_ref()
                    && !fields.is_empty()
                    && fields
                        .iter()
                        .all(|(_, value)| matches!(value, Expr::Lambda { .. }))
                {
                    let mapping = self
                        .dict_value_of(value)
                        .expect("a record of lambdas is a dictionary");
                    self.dict_bindings
                        .last_mut()
                        .expect("emitter dict scope")
                        .insert(name.clone(), mapping);
                    return Ok(());
                }
                // A nominal record with function-typed fields: the stored
                // fields become the object, and the functions are remembered
                // against this name so `s.to_string()` can find one.
                if let Expr::RecordConstructor {
                    name: record,
                    arguments,
                    ..
                } = value.as_ref()
                    && let Some(index) = self
                        .records
                        .iter()
                        .position(|info| info.usable && info.name == *record)
                    && !self.records[index].lambda_fields.is_empty()
                {
                    let lambda_fields = self.records[index].lambda_fields.clone();
                    let mut mapping = Vec::with_capacity(lambda_fields.len());
                    for (field, position) in lambda_fields {
                        let Some((params, body)) = arguments
                            .get(position)
                            .and_then(|argument| self.lambda_argument(argument))
                        else {
                            return Err(unsupported(
                                value.span(),
                                "a function-typed field that is not a function",
                            ));
                        };
                        mapping.push((field, self.intern_lambda(params, body)));
                    }
                    self.dict_bindings
                        .last_mut()
                        .expect("emitter dict scope")
                        .insert(name.clone(), mapping);
                    // The object itself is still a value binding.
                }
                // ...and so is a call that answers with one, which is how a
                // dictionary parameterised by another is bound to a name.
                if matches!(value.as_ref(), Expr::Call { .. })
                    && let Some(mapping) = self.dict_value_of(value)
                {
                    self.dict_bindings
                        .last_mut()
                        .expect("emitter dict scope")
                        .insert(name.clone(), mapping);
                    return Ok(());
                }
                let ty = self.expression(value)?;
                if ty == ValueType::Unit {
                    return Err(unsupported(value.span(), "a unit-typed binding"));
                }
                // A `mutable` slot has to hold everything that will be
                // assigned to it, not just what it starts as: `mutable c =
                // Nil` followed by `c = Cons(v, c)` starts as an enum shape
                // that carries nothing and ends as one that does. The
                // assignments are already in the tree, so the slot's type is
                // the merge of them all.
                let ty = self.widened_mutable_type(name, ty);
                let offset = self.declare_local(name, ty);
                self.asm.store_local(Reg::X0, offset);
                // M7: a binding that holds a heap reference becomes a
                // precise GC root for the rest of its scope. Rooting after
                // the store (rather than zero-then-root) is safe because
                // nothing allocates in between.
                if is_heap_pointer(ty) {
                    self.emit_root_frame_slot(offset);
                }
                Ok(())
            }
            Expr::Assign { name, value, span } => {
                let Some((offset, expected)) = self.lookup(name) else {
                    return Err(unsupported(*span, &format!("assignment to `{name}`")));
                };
                let ty = self.expression(value)?;
                // A slot typed for everything assigned to it accepts a value
                // that is narrower than it: two shapes of one enum have the
                // same representation, and the slot was widened to hold both.
                let mut fits = assignable(ty, expected)
                    || self
                        .merge_types(ty, expected)
                        .is_some_and(|merged| self.same_shape(merged, expected));
                // A slot the declaration could not type for everything -- the
                // value assigned to it mentions a name that was not in scope
                // yet, a `foreach` binding say -- can still widen here, as long
                // as nothing has matched on it. A match settles which arms are
                // reachable from the shape it saw, so widening after one would
                // leave that decision behind.
                if !fits
                    && !self.matched_enum_slots.contains(&offset)
                    && let Some(merged) = self.merge_types(ty, expected)
                {
                    for scope in self.scopes.iter_mut().rev() {
                        if let Some(entry) = scope.get_mut(name) {
                            entry.1 = merged;
                            break;
                        }
                    }
                    fits = true;
                }
                if !fits {
                    return Err(unsupported(*span, "assignment changing a type"));
                }
                self.asm.store_local(Reg::X0, offset);
                Ok(())
            }
            Expr::While {
                condition, body, ..
            } => {
                let loop_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm.bind(loop_label);
                let condition_ty = self.expression(condition)?;
                if condition_ty != ValueType::Bool {
                    return Err(unsupported(condition.span(), "a non-Bool condition"));
                }
                self.asm.branch(end_label, BranchKind::CompareZero(Reg::X0));
                self.widen_loop_mutables(body);
                self.statement(body)?;
                self.asm.branch(loop_label, BranchKind::Unconditional);
                self.asm.bind(end_label);
                Ok(())
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let condition_ty = self.expression(condition)?;
                if condition_ty != ValueType::Bool {
                    return Err(unsupported(condition.span(), "a non-Bool condition"));
                }
                let else_label = self.asm.new_label();
                let end_label = self.asm.new_label();
                self.asm
                    .branch(else_label, BranchKind::CompareZero(Reg::X0));
                self.statement(then_branch)?;
                self.asm.branch(end_label, BranchKind::Unconditional);
                self.asm.bind(else_label);
                if let Some(else_branch) = else_branch {
                    self.statement(else_branch)?;
                }
                self.asm.bind(end_label);
                Ok(())
            }
            Expr::Foreach {
                binding,
                iterable,
                body,
                span,
            } => self.emit_foreach(binding, iterable, body, *span),
            Expr::Call {
                callee, arguments, ..
            } if matches!(callee.as_ref(), Expr::Identifier { name, .. } if name == "println") => {
                self.println_call(arguments, expr.span())
            }
            other => {
                // An expression in statement position: evaluate for
                // effect (the subset's expressions are pure, but a
                // discarded value is harmless and keeps parity with
                // the C backend).
                self.expression(other)?;
                Ok(())
            }
        }
    }
}

/// Register every monomorphic, fully annotated top-level record
/// declaration. Two passes so records can reference each other in any
/// order; declarations the subset cannot type (generics, function
/// fields) are skipped and produce a use-site diagnostic instead.
fn collect_records(expr: &Expr, emitter: &mut Emitter) {
    let Expr::Block { expressions, .. } = expr else {
        return;
    };
    // Pass 1: names, so field annotations can reference any record.
    for expression in expressions {
        if let Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            ..
        } = expression
            && !type_params.is_empty()
        {
            // No single layout, so nothing is registered yet: an instantiation
            // is interned when one is named.
            emitter.generic_records.push(GenericRecord {
                name: name.clone(),
                params: type_params.clone(),
                fields: fields
                    .iter()
                    .map(|field| {
                        (
                            field.name.clone(),
                            field
                                .annotation
                                .as_ref()
                                .map(|annotation| annotation.text.clone())
                                .unwrap_or_default(),
                        )
                    })
                    .collect(),
            });
        }
        if let Expr::RecordDeclaration {
            name, type_params, ..
        } = expression
            && type_params.is_empty()
        {
            // Provisionally usable so pass 2 can resolve mutual
            // references; refined below.
            emitter.records.push(RecordInfo {
                name: name.clone(),
                fields: Vec::new(),
                lambda_fields: Vec::new(),
                usable: true,
            });
        }
    }
    // Generic enums are not lowered by the shared pass, so their declarations
    // are still here: remember the variants in order, since that order is the
    // tag every value carries.
    for expression in expressions {
        // Every declaration that is still here is one the shared pass did not
        // lower -- a generic enum, or one whose payload names an applied type
        // like `List<Json>`. Either way this backend compiles it itself.
        if let Expr::EnumDeclaration {
            name,
            type_params,
            variants,
            ..
        } = expression
        {
            emitter.generic_enums.push((
                name.clone(),
                variants
                    .iter()
                    .map(|variant| (variant.name.clone(), variant.params.len()))
                    .collect(),
            ));
            emitter.enum_annotations.push(
                variants
                    .iter()
                    .map(|variant| {
                        variant
                            .params
                            .iter()
                            .map(|(_, annotation)| annotation.text.clone())
                            .collect()
                    })
                    .collect(),
            );
            // Only an enum with no parameters has one shape for every value.
            emitter.canonical_enum_shapes.push(None);
            emitter.enum_is_generic.push(!type_params.is_empty());
            emitter.enum_type_params.push(type_params.clone());
        }
    }
    // Pass 2: field types; undecodable declarations are marked
    // unusable (indices must stay stable).
    for expression in expressions {
        let Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            span,
        } = expression
        else {
            continue;
        };
        if !type_params.is_empty() {
            continue;
        }
        let mut typed = Vec::with_capacity(fields.len());
        let mut lambda_fields = Vec::new();
        let mut usable = true;
        for (position, field) in fields.iter().enumerate() {
            let Some(annotation) = &field.annotation else {
                usable = false;
                break;
            };
            // A function-typed field is compile-time only, so the record is
            // still perfectly usable -- that field simply is not stored.
            if annotation.text.contains("=>") {
                lambda_fields.push((field.name.clone(), position));
                continue;
            }
            let Ok(ty) = emitter.annotation_type(&annotation.text, *span) else {
                usable = false;
                break;
            };
            typed.push((field.name.clone(), ty));
        }
        let index = emitter
            .records
            .iter()
            .position(|record| record.name == *name)
            .expect("registered in pass 1");
        emitter.records[index].fields = typed;
        emitter.records[index].lambda_fields = lambda_fields;
        emitter.records[index].usable = usable;
        // A declaration whose field types cannot be written down -- `name: *`,
        // say -- has no layout of its own, but its *constructor* pins every
        // field down. So it is remembered the way a generic declaration is, and
        // instantiated from the arguments when one is built.
        if !usable {
            emitter.generic_records.push(GenericRecord {
                name: name.clone(),
                params: Vec::new(),
                fields: fields
                    .iter()
                    .map(|field| {
                        (
                            field.name.clone(),
                            field
                                .annotation
                                .as_ref()
                                .map(|annotation| annotation.text.clone())
                                .unwrap_or_default(),
                        )
                    })
                    .collect(),
            });
        }
    }
}

/// Register every fully annotated top-level `def` as a callable
/// function. Defs with missing or unsupported annotations are skipped
/// silently — the stdlib prelude is full of them — and calling one
/// produces the call-site diagnostic instead.
fn collect_functions(expr: &Expr, emitter: &mut Emitter) {
    let Expr::Block { expressions, .. } = expr else {
        return;
    };
    // A typeclass instance is a bundle of ordinary, fully annotated defs:
    // `instance Show<Int> where { def show(x: Int): String = ... }`. Treated
    // as plain functions they need no new machinery -- what makes them a
    // typeclass is that several instances declare the *same* method name for
    // different parameter types, which `function_call` resolves by argument
    // type. The `typeclass` declaration itself is type-level only and emits
    // nothing.
    let mut flattened: Vec<&Expr> = Vec::new();
    for expression in expressions {
        match expression {
            Expr::InstanceDeclaration { methods, .. } => flattened.extend(methods.iter()),
            other => flattened.push(other),
        }
    }
    for expression in flattened {
        let Expr::DefDecl {
            name,
            params,
            param_annotations,
            return_annotation,
            body,
            span,
            ..
        } = expression
        else {
            continue;
        };
        let mut signature = Vec::with_capacity(params.len());
        for (param, annotation) in params.iter().zip(param_annotations.iter()) {
            let Some(annotation) = annotation else {
                signature.clear();
                break;
            };
            let Ok(ty) = emitter.annotation_type(&annotation.text, *span) else {
                signature.clear();
                break;
            };
            signature.push((param.clone(), ty));
        }
        // A signature the backend cannot spell out concretely -- an
        // unannotated parameter, a type variable, a return type it does not
        // model -- becomes a *generic* def instead of being dropped. Its
        // parameter types exist only at the call site, so it is compiled
        // once per distinct tuple of argument types (see
        // emit_generic_function_call).
        let concrete_return = return_annotation
            .as_ref()
            .and_then(|annotation| emitter.annotation_type(&annotation.text, *span).ok());
        if signature.len() != params.len() || concrete_return.is_none() {
            emitter.generic_functions.push(GenericFunction {
                name: name.clone(),
                params: params.clone(),
                body: body.as_ref().clone(),
                declared_return: concrete_return,
                param_annotations: param_annotations
                    .iter()
                    .map(|annotation| {
                        annotation
                            .as_ref()
                            .map(|annotation| annotation.text.clone())
                    })
                    .collect(),
                return_annotation: return_annotation
                    .as_ref()
                    .map(|annotation| annotation.text.clone()),
            });
            continue;
        }
        let ret = concrete_return.expect("checked above");
        let label = emitter.asm.new_label();
        emitter.functions.push((
            name.clone(),
            FunctionInfo {
                label,
                params: signature,
                lambda_params: Vec::new(),
                dict_params: Vec::new(),
                ret,
                body: body.as_ref().clone(),
            },
        ));
    }
}

/// Compile the whole program to a signed Mach-O arm64 executable.
/// `lowered_enums` names the enums `desugar_enums` already lowered to
/// `__gc_record` shape.
/// Every data cell, diagnostic string, and subroutine label the portable
/// ZGC needs, reserved in one pass and threaded into the `emit_gc_*`
/// calls. Mirrors the x86-64 backend's GC field set (lib.rs) one-to-one.
/// The single-qword cells and the three 512-entry region arrays live in
/// `__DATA,__bss`; the diagnostic strings live in read-only `__const`.
struct GcState {
    heap_base: PortDataAddr,
    heap_top: PortDataAddr,
    heap_end: PortDataAddr,
    free_region_head: PortDataAddr,
    mark_worklist: PortDataAddr,
    mark_worklist_top: PortDataAddr,
    shadow_stack: PortDataAddr,
    shadow_stack_top: PortDataAddr,
    region_base: PortDataAddr,
    committed_count: PortDataAddr,
    budget_regions: PortDataAddr,
    phase: PortDataAddr,
    good_color: PortDataAddr,
    bad_mask: PortDataAddr,
    bytes_since_cycle: PortDataAddr,
    bytes_since_quantum: PortDataAddr,
    stw_fallback_pending: PortDataAddr,
    stw_fallbacks: PortDataAddr,
    header_mark: PortDataAddr,
    mark_color: PortDataAddr,
    evac_region_base: PortDataAddr,
    evac_top: PortDataAddr,
    evac_end: PortDataAddr,
    reloc_scan_idx: PortDataAddr,
    reloc_block: PortDataAddr,
    relocated_count: PortDataAddr,
    collect_counter: PortDataAddr,
    alloc_count: PortDataAddr,
    bytes_allocated: PortDataAddr,
    pause_max_ns: PortDataAddr,
    pause_total_ns: PortDataAddr,
    pause_start_ns: PortDataAddr,
    region_top: PortDataAddr,
    region_live: PortDataAddr,
    region_fromspace: PortDataAddr,
    oom: (PortDataAddr, usize),
    worklist_overflow: (PortDataAddr, usize),
    bounds_error_text: (PortDataAddr, usize),
    evac_exhausted: (PortDataAddr, usize),
    evac_oversized: (PortDataAddr, usize),
    l_alloc: Label,
    l_collect: Label,
    l_mark_roots: Label,
    l_trace: Label,
    l_sweep: Label,
    l_clear_all_marks: Label,
    l_stw_mark_complete: Label,
    l_drain: Label,
    l_evacuate: Label,
    l_acquire_evac_region: Label,
    l_relocate_start: Label,
    l_relocate_quantum: Label,
    l_relocate_finish: Label,
    l_free_ghost_regions: Label,
    l_relocate_fix_roots: Label,
    l_mark_start: Label,
    l_mark_end: Label,
    l_mark_visit: Label,
    l_deep_equal: Label,
    l_load_barrier_slow: Label,
    l_acquire_region: Label,
    l_alloc_large: Label,
    l_grow_budget: Label,
    l_bounds_error: Label,
}

impl Emitter {
    /// Store a register's value into a single-qword GC `__bss` cell,
    /// using X10 as the address scratch.
    fn emit_store_gc_cell_reg(&mut self, cell: PortDataAddr, value: Reg) {
        let PortDataAddr::Bss(label) = cell else {
            unreachable!("GC cells live in __bss");
        };
        self.asm.load_data_address(Reg::X10, label);
        self.asm.str_imm(value, Reg::X10, 0);
    }

    /// Store a 64-bit immediate into a single-qword GC `__bss` cell
    /// (X11 holds the value, X10 the address).
    fn emit_store_gc_cell_imm(&mut self, cell: PortDataAddr, value: u64) {
        self.asm.mov_imm64(Reg::X11, value);
        self.emit_store_gc_cell_reg(cell, Reg::X11);
    }

    /// M7 (infra): bring the GC region heap online at startup -- the
    /// AArch64 mirror of the x86-64 `emit_initialize_gc_heap`. mmaps the
    /// whole region reservation (demand-paged) and the runtime tables
    /// (shadow stack + mark worklist, zero-filled by mmap), then installs
    /// region-0 metadata. Runs before the bump heap is armed; while
    /// allocations still bump (pre go-live) these cells are written but
    /// unread, so this only exercises the startup path.
    fn emit_gc_init_heap(&mut self, gc: &GcState) {
        // mmap(NULL, GC_RESERVE_BYTES, RW, MAP_ANON|PRIVATE, -1, 0).
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm
            .mov_imm64(Reg::X1, crate::gc_layout::GC_RESERVE_BYTES);
        self.asm.mov_imm64(Reg::X2, PROT_READ_WRITE);
        self.asm.mov_imm64(Reg::X3, MMAP_ANON_PRIVATE);
        self.asm.mov_imm64(Reg::X4, u64::MAX); // fd = -1
        self.asm.mov_imm64(Reg::X5, 0);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_MMAP));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic gc: mmap failed\n");
        // region_base = heap_base = heap_top = region_top[0] = base (x0).
        self.emit_store_gc_cell_reg(gc.region_base, Reg::X0);
        self.emit_store_gc_cell_reg(gc.heap_base, Reg::X0);
        self.emit_store_gc_cell_reg(gc.heap_top, Reg::X0);
        self.emit_store_gc_cell_reg(gc.region_top, Reg::X0); // element 0
        // heap_end = base + GC_REGION_SIZE.
        self.asm
            .mov_imm64(Reg::X1, crate::gc_layout::GC_REGION_SIZE);
        self.asm.add_reg(Reg::X2, Reg::X0, Reg::X1);
        self.emit_store_gc_cell_reg(gc.heap_end, Reg::X2);
        // free_region_head = 0; committed_count = 1; budget = initial.
        self.emit_store_gc_cell_imm(gc.free_region_head, 0);
        self.emit_store_gc_cell_imm(gc.committed_count, 1);
        self.emit_store_gc_cell_imm(
            gc.budget_regions,
            crate::gc_layout::GC_INITIAL_BUDGET_REGIONS,
        );
        // mmap(NULL, GC_TABLES_BYTES, ...) for the shadow stack + worklist.
        self.asm.mov_imm64(Reg::X0, 0);
        self.asm
            .mov_imm64(Reg::X1, crate::gc_layout::GC_TABLES_BYTES);
        self.asm.mov_imm64(Reg::X2, PROT_READ_WRITE);
        self.asm.mov_imm64(Reg::X3, MMAP_ANON_PRIVATE);
        self.asm.mov_imm64(Reg::X4, u64::MAX);
        self.asm.mov_imm64(Reg::X5, 0);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_MMAP));
        self.asm.svc_0x80();
        self.asm
            .emit_abort_if_syscall_failed(b"klassic gc: mmap failed\n");
        // shadow_stack = tables base; mark_worklist = base + SHADOW_LEN*8.
        self.emit_store_gc_cell_reg(gc.shadow_stack, Reg::X0);
        self.asm
            .mov_imm64(Reg::X1, (crate::gc_layout::GC_SHADOW_STACK_LEN * 8) as u64);
        self.asm.add_reg(Reg::X0, Reg::X0, Reg::X1);
        self.emit_store_gc_cell_reg(gc.mark_worklist, Reg::X0);
    }

    /// M7 (infra): seed the callee-saved colour registers (X24 strip mask,
    /// X25 good colour, X26 bad-colour test mask) and the colour/mark
    /// cells -- the AArch64 mirror of `emit_initialize_color_registers`.
    /// X25/X26 cache the cells; MarkStart reloads them each cycle.
    ///
    /// With the moving collector (M9) Idle and Relocate use good = R and
    /// bad = M0|M1: a reference still carrying a mark colour may point at a
    /// ghost, so it is "bad" and its next load slow-paths and follows the
    /// forwarding word. The non-moving scheme keeps the last mark colour
    /// good instead, so Idle loads stay on the fast path.
    fn emit_gc_init_colors(&mut self, gc: &GcState, evac_off: bool) {
        self.asm
            .mov_imm64(Reg::X24, crate::gc_layout::GC_COLOR_STRIP);
        // --gc-poison makes every colour except the current good one bad, so
        // any reference read without the barrier -- or stored without being
        // coloured -- faults or slow-paths immediately. It is the coverage
        // canary for the two barriers.
        let good = match (self.gc_poison, evac_off) {
            (true, true) => crate::gc_layout::GC_COLOR_M1,
            (true, false) => crate::gc_layout::GC_COLOR_R,
            (false, true) => crate::gc_layout::GC_COLOR_M0,
            (false, false) => crate::gc_layout::GC_COLOR_R,
        };
        let bad = if self.gc_poison {
            crate::gc_layout::GC_COLOR_MASK
        } else if evac_off {
            crate::gc_layout::GC_COLOR_BAD_MASK
        } else {
            crate::gc_layout::GC_COLOR_M0 | crate::gc_layout::GC_COLOR_M1
        };
        self.emit_store_gc_cell_imm(gc.good_color, good);
        self.emit_store_gc_cell_imm(gc.bad_mask, bad);
        let PortDataAddr::Bss(good) = gc.good_color else {
            unreachable!("GC cells live in __bss");
        };
        self.asm.load_data_address(Reg::X10, good);
        self.asm.ldr_imm(Reg::X25, Reg::X10, 0);
        let PortDataAddr::Bss(bad) = gc.bad_mask else {
            unreachable!("GC cells live in __bss");
        };
        self.asm.load_data_address(Reg::X10, bad);
        self.asm.ldr_imm(Reg::X26, Reg::X10, 0);
        // Header-mark parity (nonzero so the first mark isn't a no-op) and
        // the initial mark colour.
        self.emit_store_gc_cell_imm(gc.header_mark, crate::gc_layout::GC_HMARK1);
        self.emit_store_gc_cell_imm(gc.mark_color, crate::gc_layout::GC_COLOR_M1);
    }

    /// `--gc-log`: write one `label=<value>` field of GC statistics to
    /// stderr, reading the counter out of its `__bss` cell.
    fn emit_gc_log_field(&mut self, label: &[u8], cell: PortDataAddr) {
        self.asm.emit_write_rodata(STDERR_FD, label);
        let PortDataAddr::Bss(dl) = cell else {
            unreachable!("GC cells live in __bss");
        };
        self.asm.load_data_address(Reg::X0, dl);
        self.asm.ldr_imm(Reg::X0, Reg::X0, 0);
        // Render the value with the shared decimal formatter (which writes
        // into a 32-byte stack buffer and leaves the first digit in x1 and
        // the buffer end in x2).
        self.asm.sub_sp_imm(32);
        self.asm.emit_int_digits(false);
        self.asm.add_reg_sp_imm(Reg::X2, 32);
        self.asm.sub_reg(Reg::X2, Reg::X2, Reg::X1);
        self.asm.mov_imm64(Reg::X0, STDERR_FD);
        self.asm.mov_imm64(Reg::X16, u64::from(SYS_WRITE));
        self.asm.svc_0x80();
        self.asm.add_sp_imm(32);
    }

    /// `--gc-log`: the one-line statistics report, written to stderr just
    /// before a normal exit. `collections` proving non-zero is what shows
    /// the collector actually ran, which is how the stress corpus asserts
    /// it is doing work rather than silently never collecting.
    fn emit_gc_log_report(&mut self, gc: &GcState) {
        self.emit_gc_log_field(b"gc: collections=", gc.collect_counter);
        self.emit_gc_log_field(b" allocs=", gc.alloc_count);
        self.emit_gc_log_field(b" bytes=", gc.bytes_allocated);
        self.emit_gc_log_field(b" committed=", gc.committed_count);
        self.emit_gc_log_field(b" relocated=", gc.relocated_count);
        self.emit_gc_log_field(b" stw_fallbacks=", gc.stw_fallbacks);
        self.asm.emit_write_rodata(STDERR_FD, b"\n");
    }

    /// Reserve all GC cells (single qwords + the three region arrays) in
    /// `__DATA,__bss`, intern the diagnostic strings, and create the
    /// subroutine labels.
    fn reserve_gc_state(&mut self) -> GcState {
        const REGIONS: usize = crate::gc_layout::GC_RESERVE_REGIONS as usize;
        // Shared with the mutator's root pushes (whichever side runs first
        // reserves them), so both address the same two cells.
        let (shadow_stack_cell, shadow_stack_top_cell) = self.shadow_cells();
        let mut cell = || PortDataAddr::Bss(self.asm.reserve_data_cells(1));
        let heap_base = cell();
        let heap_top = cell();
        let heap_end = cell();
        let free_region_head = cell();
        let mark_worklist = cell();
        let mark_worklist_top = cell();
        let shadow_stack = PortDataAddr::Bss(shadow_stack_cell);
        let shadow_stack_top = PortDataAddr::Bss(shadow_stack_top_cell);
        let region_base = cell();
        let committed_count = cell();
        let budget_regions = cell();
        let phase = cell();
        let good_color = cell();
        let bad_mask = cell();
        let bytes_since_cycle = cell();
        let bytes_since_quantum = cell();
        let stw_fallback_pending = cell();
        let stw_fallbacks = cell();
        let header_mark = cell();
        let mark_color = cell();
        let evac_region_base = cell();
        let evac_top = cell();
        let evac_end = cell();
        let reloc_scan_idx = cell();
        let reloc_block = cell();
        let relocated_count = cell();
        let collect_counter = cell();
        let alloc_count = cell();
        let bytes_allocated = cell();
        let pause_max_ns = cell();
        let pause_total_ns = cell();
        let pause_start_ns = cell();
        let region_top = PortDataAddr::Bss(self.asm.reserve_data_cells(REGIONS));
        let region_live = PortDataAddr::Bss(self.asm.reserve_data_cells(REGIONS));
        let region_fromspace = PortDataAddr::Bss(self.asm.reserve_data_cells(REGIONS));

        let mut string = |bytes: &[u8]| {
            (
                PortDataAddr::Rodata(self.asm.intern_rodata(bytes)),
                bytes.len(),
            )
        };
        let oom = string(b"klassic gc: out of memory\n");
        let worklist_overflow = string(b"klassic gc: mark worklist overflow\n");
        let bounds_error_text = string(b"klassic gc: index out of bounds\n");
        let evac_exhausted = string(b"klassic gc: evacuation exhausted the heap reservation\n");
        let evac_oversized = string(b"klassic gc: evacuation object exceeds a region\n");
        // Share one label with the mutator's barriered reads (whichever
        // side runs first creates it) so their `bl` lands on this body.
        let l_load_barrier_slow = self.load_barrier_label();
        // Likewise shared with every mutator allocation site.
        let l_alloc = self.gc_alloc_entry_label();

        GcState {
            heap_base,
            heap_top,
            heap_end,
            free_region_head,
            mark_worklist,
            mark_worklist_top,
            shadow_stack,
            shadow_stack_top,
            region_base,
            committed_count,
            budget_regions,
            phase,
            good_color,
            bad_mask,
            bytes_since_cycle,
            bytes_since_quantum,
            stw_fallback_pending,
            stw_fallbacks,
            header_mark,
            mark_color,
            evac_region_base,
            evac_top,
            evac_end,
            reloc_scan_idx,
            reloc_block,
            relocated_count,
            collect_counter,
            alloc_count,
            bytes_allocated,
            pause_max_ns,
            pause_total_ns,
            pause_start_ns,
            region_top,
            region_live,
            region_fromspace,
            oom,
            worklist_overflow,
            bounds_error_text,
            evac_exhausted,
            evac_oversized,
            l_alloc,
            l_collect: self.asm.new_label(),
            l_mark_roots: self.asm.new_label(),
            l_trace: self.asm.new_label(),
            l_sweep: self.asm.new_label(),
            l_clear_all_marks: self.asm.new_label(),
            l_stw_mark_complete: self.asm.new_label(),
            l_drain: self.asm.new_label(),
            l_evacuate: self.asm.new_label(),
            l_acquire_evac_region: self.asm.new_label(),
            l_relocate_start: self.asm.new_label(),
            l_relocate_quantum: self.asm.new_label(),
            l_relocate_finish: self.asm.new_label(),
            l_free_ghost_regions: self.asm.new_label(),
            l_relocate_fix_roots: self.asm.new_label(),
            l_mark_start: self.asm.new_label(),
            l_mark_end: self.asm.new_label(),
            l_mark_visit: self.asm.new_label(),
            l_deep_equal: self.asm.new_label(),
            l_load_barrier_slow,
            l_acquire_region: self.asm.new_label(),
            l_alloc_large: self.asm.new_label(),
            l_grow_budget: self.asm.new_label(),
            l_bounds_error: self.asm.new_label(),
        }
    }

    /// Emit all 24 portable GC runtime routines into the code buffer.
    /// M5: evacuation is off (non-moving) and pause timing is disabled
    /// (`plat_read_monotonic_ns` is a stub); the `--gc-log`/`--gc-stress`
    /// flags are threaded in at go-live (M7). Emitted after the program
    /// body and its exit, so the routines are reachable only once the
    /// mutator calls `gc_alloc` (M7) -- dead but linked until then.
    fn emit_gc_runtime(&mut self, gc: &GcState) {
        use portable_asm as pa;
        let asm = &mut self.asm;
        let evac_off = GC_EVAC_OFF;
        let poison = self.gc_poison;
        let timing = false;
        let stderr_fd = 2u64;
        let tables = pa::RegionTables {
            heap_base: gc.heap_base,
            heap_top: gc.heap_top,
            region_base: gc.region_base,
            region_top: gc.region_top,
            committed_count: gc.committed_count,
            region_fromspace: gc.region_fromspace,
        };
        let pause = pa::PauseCells {
            start_ns: gc.pause_start_ns,
            total_ns: gc.pause_total_ns,
            max_ns: gc.pause_max_ns,
        };

        pa::emit_gc_mark_visit(
            asm,
            gc.l_mark_visit,
            pa::MarkWorklist {
                header_mark: gc.header_mark,
                worklist: gc.mark_worklist,
                worklist_top: gc.mark_worklist_top,
                stw_fallback_pending: gc.stw_fallback_pending,
            },
        );
        pa::emit_gc_deep_equal(asm, gc.l_deep_equal);
        pa::emit_gc_load_barrier_slow(
            asm,
            gc.l_load_barrier_slow,
            gc.phase,
            gc.region_base,
            gc.region_fromspace,
            gc.l_evacuate,
            gc.l_mark_visit,
        );
        pa::emit_gc_alloc(
            asm,
            gc.l_alloc,
            pa::AllocCells {
                phase: gc.phase,
                bytes_since_cycle: gc.bytes_since_cycle,
                budget_regions: gc.budget_regions,
                bytes_since_quantum: gc.bytes_since_quantum,
                mark_worklist_top: gc.mark_worklist_top,
                header_mark: gc.header_mark,
                alloc_count: gc.alloc_count,
                bytes_allocated: gc.bytes_allocated,
                heap_top: gc.heap_top,
                heap_end: gc.heap_end,
                oom_text: gc.oom.0,
            },
            pa::AllocTargets {
                mark_start: gc.l_mark_start,
                trace: gc.l_trace,
                mark_end: gc.l_mark_end,
                relocate_quantum: gc.l_relocate_quantum,
                collect: gc.l_collect,
                grow_budget: gc.l_grow_budget,
                acquire_region: gc.l_acquire_region,
                alloc_large: gc.l_alloc_large,
            },
            pa::AllocFlags {
                stress: self.gc_stress,
                // Counting allocations and bytes is what makes the exit
                // report's `allocs=` / `bytes=` fields meaningful.
                log: self.gc_log,
            },
            gc.oom.1,
            stderr_fd,
        );
        pa::emit_gc_collect(
            asm,
            gc.l_collect,
            timing,
            pause,
            gc.phase,
            pa::CollectTargets {
                stw_mark_complete: gc.l_stw_mark_complete,
                free_ghost_regions: gc.l_free_ghost_regions,
                sweep: gc.l_sweep,
                relocate_quantum: gc.l_relocate_quantum,
                mark_end: gc.l_mark_end,
            },
        );
        pa::emit_gc_mark_roots(
            asm,
            gc.l_mark_roots,
            gc.shadow_stack,
            gc.shadow_stack_top,
            gc.l_mark_visit,
        );
        pa::emit_gc_trace(
            asm,
            gc.l_trace,
            gc.mark_worklist_top,
            gc.mark_worklist,
            gc.l_mark_visit,
        );
        pa::emit_gc_sweep(
            asm,
            gc.l_sweep,
            tables,
            gc.collect_counter,
            gc.free_region_head,
            gc.header_mark,
            gc.region_live,
        );
        pa::emit_gc_clear_all_marks(asm, gc.l_clear_all_marks, tables);
        pa::emit_gc_stw_mark_complete(
            asm,
            gc.l_stw_mark_complete,
            pa::StwMarkTargets {
                clear_all_marks: gc.l_clear_all_marks,
                mark_roots: gc.l_mark_roots,
                drain: gc.l_drain,
            },
            gc.stw_fallback_pending,
            gc.mark_worklist_top,
            gc.worklist_overflow.0,
            gc.worklist_overflow.1,
        );
        pa::emit_gc_drain(asm, gc.l_drain, gc.l_trace, gc.mark_worklist_top);
        pa::emit_gc_evacuate(
            asm,
            gc.l_evacuate,
            pa::EvacBumpCells {
                evac_top: gc.evac_top,
                evac_end: gc.evac_end,
                relocated_count: gc.relocated_count,
            },
            gc.l_acquire_evac_region,
            gc.evac_oversized.0,
            gc.evac_oversized.1,
            stderr_fd,
        );
        pa::emit_gc_acquire_evac_region(
            asm,
            gc.l_acquire_evac_region,
            pa::EvacRegionCells {
                evac_region_base: gc.evac_region_base,
                evac_top: gc.evac_top,
                evac_end: gc.evac_end,
                region_base: gc.region_base,
                region_top: gc.region_top,
                committed_count: gc.committed_count,
                free_region_head: gc.free_region_head,
            },
            gc.evac_exhausted.0,
            gc.evac_exhausted.1,
            stderr_fd,
        );
        pa::emit_gc_relocate_start(
            asm,
            gc.l_relocate_start,
            pa::RelocateStartCells {
                good_color: gc.good_color,
                bad_mask: gc.bad_mask,
                free_region_head: gc.free_region_head,
                budget_regions: gc.budget_regions,
                committed_count: gc.committed_count,
                heap_base: gc.heap_base,
                region_base: gc.region_base,
                region_top: gc.region_top,
                region_live: gc.region_live,
                region_fromspace: gc.region_fromspace,
                evac_region_base: gc.evac_region_base,
                evac_top: gc.evac_top,
                evac_end: gc.evac_end,
                reloc_scan_idx: gc.reloc_scan_idx,
                reloc_block: gc.reloc_block,
                bytes_since_quantum: gc.bytes_since_quantum,
                phase: gc.phase,
                bytes_since_cycle: gc.bytes_since_cycle,
            },
            gc.l_relocate_fix_roots,
            evac_off,
            poison,
        );
        pa::emit_gc_relocate_quantum(
            asm,
            gc.l_relocate_quantum,
            tables,
            pa::RelocQuantumCells {
                reloc_scan_idx: gc.reloc_scan_idx,
                reloc_block: gc.reloc_block,
                header_mark: gc.header_mark,
            },
            gc.l_evacuate,
            gc.l_relocate_finish,
        );
        pa::emit_gc_relocate_finish(
            asm,
            gc.l_relocate_finish,
            tables,
            gc.evac_region_base,
            gc.evac_top,
            gc.phase,
            gc.bytes_since_cycle,
        );
        pa::emit_gc_free_ghost_regions(
            asm,
            gc.l_free_ghost_regions,
            tables,
            gc.free_region_head,
            gc.region_live,
        );
        pa::emit_gc_relocate_fix_roots(
            asm,
            gc.l_relocate_fix_roots,
            gc.shadow_stack,
            gc.shadow_stack_top,
            gc.region_base,
            gc.region_fromspace,
            gc.l_evacuate,
        );
        pa::emit_gc_mark_start(
            asm,
            gc.l_mark_start,
            timing,
            pause,
            pa::MarkStartCells {
                header_mark: gc.header_mark,
                good_color: gc.good_color,
                bad_mask: gc.bad_mask,
                mark_color: gc.mark_color,
                worklist_top: gc.mark_worklist_top,
                phase: gc.phase,
            },
            gc.l_mark_roots,
            pa::MarkColorMode { evac_off, poison },
        );
        pa::emit_gc_mark_end(
            asm,
            gc.l_mark_end,
            timing,
            pause,
            pa::MarkEndTargets {
                drain: gc.l_drain,
                stw_mark_complete: gc.l_stw_mark_complete,
                free_ghost_regions: gc.l_free_ghost_regions,
                sweep: gc.l_sweep,
                relocate_start: gc.l_relocate_start,
            },
            gc.stw_fallback_pending,
            gc.stw_fallbacks,
        );
        pa::emit_gc_acquire_region(
            asm,
            gc.l_acquire_region,
            tables,
            gc.free_region_head,
            gc.budget_regions,
            gc.heap_end,
        );
        pa::emit_gc_alloc_large(asm, gc.l_alloc_large, tables, gc.budget_regions);
        pa::emit_gc_grow_budget(asm, gc.l_grow_budget, gc.committed_count, gc.budget_regions);
        pa::emit_gc_bounds_error(
            asm,
            gc.l_bounds_error,
            gc.bounds_error_text.0,
            gc.bounds_error_text.1,
        );
    }
}

pub(crate) fn emit_macho_program(
    expr: &Expr,
    lowered_enums: std::collections::HashSet<String>,
    gc_log: bool,
    gc_stress: bool,
    gc_poison: bool,
) -> Result<Vec<u8>, Diagnostic> {
    let mut emitter = Emitter {
        lowered_enums,
        gc_log,
        gc_stress,
        gc_poison,
        ..Emitter::default()
    };
    emitter.scopes.push(HashMap::new());
    emitter.lambda_bindings.push(HashMap::new());
    emitter.dict_bindings.push(HashMap::new());
    emitter.alias_bindings.push(HashMap::new());
    emitter.scope_root_counts.push(0);
    collect_records(expr, &mut emitter);
    collect_functions(expr, &mut emitter);

    // dyld calls the LC_MAIN entry as a plain AAPCS64 call:
    // x0=argc, x1=argv, x2=envp, x3=apple. Capture into callee-saved
    // registers before anything else can clobber x0-x2 (M16, issue
    // #538).
    emitter.asm.mov_reg(Reg::X21, Reg::X0);
    emitter.asm.mov_reg(Reg::X22, Reg::X1);
    emitter.asm.mov_reg(Reg::X23, Reg::X2);

    // M7 (infra): reserve the GC cells/labels and bring the region heap +
    // colour registers online at startup, before the bump heap is armed.
    // Allocations still bump below, so the collector stays inert -- this
    // only exercises the startup mmap/seeding path (validated live on
    // arm64 CI) ahead of the go-live flip. reserve_gc_state emits no code
    // (it only reserves __bss cells / rodata / labels), so calling it here
    // and emitting the routine bodies after main is unchanged.
    let gc = emitter.reserve_gc_state();
    emitter.emit_gc_init_heap(&gc);
    emitter.emit_gc_init_colors(&gc, GC_EVAC_OFF);

    // Reserved now, sized once the program has been compiled -- see
    // `sub_sp_placeholder`.
    let frame_patch = emitter.asm.sub_sp_placeholder();
    emitter.asm.mov_fp_sp();

    emitter.statement(expr)?;
    // The queued `thread` bodies, in the order they were handed over. They run
    // inside the program's own frame, so anything they closed over is still
    // where they left it.
    for (index, span) in std::mem::take(&mut emitter.queued_threads) {
        emitter.emit_inline_lambda_call(index, &[], span)?;
    }
    let frame_size = emitter.max_local_offset.div_ceil(16) * 16;
    if frame_size >= 4096 {
        return Err(unsupported(expr.span(), "this many local variables"));
    }
    emitter.asm.patch_sub_sp(frame_patch, frame_size);
    if gc_log {
        emitter.emit_gc_log_report(&gc);
    }
    emitter.asm.emit_exit(0);

    // Emit reached functions; their bodies may reach more.
    let mut emitted = vec![false; emitter.functions.len()];
    while let Some(index) = emitter.pending.pop() {
        // Emitting a body can *add* functions: a generic def reached from
        // here gets a fresh specialisation per argument-type tuple, appended
        // to `functions` and queued. So the seen-set has to grow with the
        // list rather than being sized once up front.
        if emitted.len() < emitter.functions.len() {
            emitted.resize(emitter.functions.len(), false);
        }
        if !emitted[index] {
            emitted[index] = true;
            emitter.emit_function(index)?;
        }
    }
    // Set helpers before heap-grow: the reverse routine allocates, so
    // it can be what first references the heap-grow routine.
    if let Some(label) = emitter.member_scalar_label {
        emitter.emit_member_scalar_routine(label);
    }
    if let Some(label) = emitter.member_string_label {
        emitter.emit_member_string_routine(label);
    }
    if let Some(label) = emitter.list_reverse_label {
        emitter.emit_list_reverse_routine(label);
    }
    // The shadow-stack helpers the mutator's root pushes call.
    if let Some(label) = emitter.shadow_push_label {
        emitter.emit_shadow_push_routine(label);
    }
    if let Some(label) = emitter.shadow_pop_label {
        emitter.emit_shadow_pop_routine(label);
    }

    // M5: emit all 24 portable ZGC runtime routines after the program's
    // exit (the cells were reserved and the region heap/colour registers
    // seeded at startup above). Nothing calls gc_alloc yet (the bump
    // allocator is still live), so the routines are dead but linked -- they
    // exercise the __DATA segment, the data fixups, and the whole
    // PortableAsm lowering end-to-end, and must encode validly. The go-live
    // (M7) routes the mutator through gc_alloc and adds roots/barriers.
    emitter.emit_gc_runtime(&gc);

    emitter.asm.finish();
    // `bss_len` is 0 for every program today (no codegen reserves GC
    // cells until M5), so no writable segment is emitted and the image
    // stays byte-for-byte the pre-GC layout. The data-label machinery
    // (reserve_data_cells / load_data_address) is wired to it and ready.
    let bss_len = emitter.asm.bss_len as u64;
    Ok(macho::write_executable(
        emitter.asm.code,
        emitter.asm.rodata,
        &emitter.asm.fixups,
        "klassic",
        bss_len,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn words(asm: &Assembler) -> Vec<u32> {
        asm.code
            .chunks_exact(4)
            .map(|chunk| u32::from_le_bytes(chunk.try_into().unwrap()))
            .collect()
    }

    #[test]
    fn encodes_reference_instructions() {
        let mut asm = Assembler::default();
        asm.movz(Reg::X0, 1, 0);
        asm.movz(Reg::X16, 4, 0);
        asm.movk(Reg::X2, 0xbeef, 1);
        asm.svc_0x80();
        asm.add_reg(Reg::X0, Reg::X0, Reg::X1);
        asm.sub_reg(Reg::X2, Reg::X3, Reg::X4);
        asm.mul_reg(Reg::X0, Reg::X0, Reg::X1);
        asm.sdiv_reg(Reg::X0, Reg::X0, Reg::X1);
        asm.cmp_reg(Reg::X0, Reg::X1);
        asm.cset(Reg::X0, Cond::Lt);
        asm.mov_reg(Reg::X1, Reg::X0);
        asm.push(Reg::X0);
        asm.pop(Reg::X1);
        asm.store_local(Reg::X0, 16);
        asm.load_local(Reg::X2, 8);
        assert_eq!(
            words(&asm),
            vec![
                0xd280_0020, // movz x0, #1
                0xd280_0090, // movz x16, #4
                0xf2b7_dde2, // movk x2, #0xbeef, lsl #16
                0xd400_1001, // svc #0x80
                0x8b01_0000, // add x0, x0, x1
                0xcb04_0062, // sub x2, x3, x4
                0x9b01_7c00, // mul x0, x0, x1
                0x9ac1_0c00, // sdiv x0, x0, x1
                0xeb01_001f, // cmp x0, x1
                0x9a9f_a7e0, // cset x0, lt
                0xaa00_03e1, // mov x1, x0
                0xf81f_0fe0, // str x0, [sp, #-16]!
                0xf841_07e1, // ldr x1, [sp], #16
                0xf900_0ba0, // str x0, [x29, #16]
                0xf940_07a2, // ldr x2, [x29, #8]
            ]
        );
    }

    #[test]
    fn encodes_gc_port_instructions() {
        // Golden words generated with keystone-engine (ARM64) and
        // round-tripped through capstone; see the aarch64 GC port plan.
        // These primitives are inert until the PortableAsm impl (M4)
        // references them.
        let mut asm = Assembler::default();
        asm.and_reg(Reg::X0, Reg::X1, Reg::X2);
        asm.orr_reg(Reg::X0, Reg::X1, Reg::X2);
        asm.eor_reg(Reg::X0, Reg::X1, Reg::X2);
        asm.tst_reg(Reg::X0, Reg::X1);
        asm.ldur(Reg::X0, Reg::X7, -16);
        asm.ldur(Reg::X0, Reg::X7, -8);
        asm.stur(Reg::X11, Reg::X10, -16);
        asm.ldur(Reg::X0, Reg::X7, 255);
        assert_eq!(
            words(&asm),
            vec![
                0x8a02_0020, // and x0, x1, x2
                0xaa02_0020, // orr x0, x1, x2
                0xca02_0020, // eor x0, x1, x2
                0xea01_001f, // tst x0, x1  (ands xzr, x0, x1)
                0xf85f_00e0, // ldur x0, [x7, #-16]
                0xf85f_80e0, // ldur x0, [x7, #-8]
                0xf81f_014b, // stur x11, [x10, #-16]
                0xf84f_f0e0, // ldur x0, [x7, #255]
            ]
        );
    }

    #[test]
    fn encodes_unsigned_conditional_branches() {
        // b.hi (Above) = cond 8, b.hs/b.cs (AboveOrEqual) = cond 2.
        let mut asm = Assembler::default();
        let target = asm.new_label();
        asm.branch(target, BranchKind::Conditional(Cond::Hi));
        asm.branch(target, BranchKind::Conditional(Cond::Hs));
        asm.bind(target);
        asm.finish();
        let w = words(&asm);
        // b.cond encodes cond in the low 4 bits; offset patched to +N words.
        assert_eq!(w[0] & 0xff00_001f, 0x5400_0008); // b.hi
        assert_eq!(w[1] & 0xff00_001f, 0x5400_0002); // b.hs
    }

    #[test]
    fn portable_gc_routines_emit_valid_aarch64() {
        // Emit whole architecture-independent GC routines through the
        // AArch64 PortableAsm impl and sanity-check the byte stream. Set
        // KLASSIC_AA_DUMP to a path to dump the code for capstone disasm.
        let mut asm = Assembler::default();

        let cc = PortDataAddr::Bss(asm.reserve_data_cells(1));
        let br = PortDataAddr::Bss(asm.reserve_data_cells(1));
        let grow = asm.new_label();
        crate::portable_asm::emit_gc_grow_budget(&mut asm, grow, cc, br);

        let bounds = asm.new_label();
        crate::portable_asm::emit_gc_bounds_error(&mut asm, bounds, PortDataAddr::Rodata(0), 32);

        let worklist_top = PortDataAddr::Bss(asm.reserve_data_cells(1));
        let drain = asm.new_label();
        let trace = asm.new_label();
        asm.bind(trace); // a stand-in body so `finish` can resolve the call
        asm.ret();
        crate::portable_asm::emit_gc_drain(&mut asm, drain, trace, worklist_top);

        // mark_roots exercises the rbp-relative frame slots ([x29,#-N]).
        let shadow = PortDataAddr::Bss(asm.reserve_data_cells(1));
        let shadow_top = PortDataAddr::Bss(asm.reserve_data_cells(1));
        let mark_visit = asm.new_label();
        asm.bind(mark_visit);
        asm.ret();
        let mark_roots = asm.new_label();
        crate::portable_asm::emit_gc_mark_roots(
            &mut asm, mark_roots, shadow, shadow_top, mark_visit,
        );

        // load_barrier_slow exercises the bit-test remap and the reserved
        // colour registers (x24/x25/x26) and the [base-16] header access.
        let phase = PortDataAddr::Bss(asm.reserve_data_cells(1));
        let region_base = PortDataAddr::Bss(asm.reserve_data_cells(1));
        let region_fromspace = PortDataAddr::Bss(asm.reserve_data_cells(1));
        let evacuate = asm.new_label();
        asm.bind(evacuate);
        asm.ret();
        let lbs = asm.new_label();
        crate::portable_asm::emit_gc_load_barrier_slow(
            &mut asm,
            lbs,
            phase,
            region_base,
            region_fromspace,
            evacuate,
            mark_visit,
        );

        asm.finish();
        assert!(!asm.code.is_empty());
        assert_eq!(asm.code.len() % 4, 0, "every instruction is a 32-bit word");
        if let Ok(path) = std::env::var("KLASSIC_AA_DUMP") {
            std::fs::write(path, &asm.code).unwrap();
        }
    }

    #[test]
    fn branches_resolve_forward_and_backward() {
        let mut asm = Assembler::default();
        let back = asm.new_label();
        let forward = asm.new_label();
        asm.bind(back);
        asm.movz(Reg::X0, 0, 0);
        asm.branch(forward, BranchKind::CompareZero(Reg::X0));
        asm.branch(back, BranchKind::Unconditional);
        asm.bind(forward);
        asm.movz(Reg::X0, 1, 0);
        asm.finish();
        let words = words(&asm);
        // cbz x0, +2 words
        assert_eq!(words[1], 0xb400_0000 | (2 << 5));
        // b -2 words (back to offset 0 from offset 8)
        assert_eq!(words[2], 0x1400_0000 | (0x03ff_ffff & (-2i32 as u32)));
    }

    #[test]
    fn mov_imm64_skips_zero_halfwords() {
        let mut asm = Assembler::default();
        asm.mov_imm64(Reg::X2, 13);
        assert_eq!(words(&asm), vec![0xd280_01a2]); // movz x2, #13 only

        let mut asm = Assembler::default();
        asm.mov_imm64(Reg::X0, 0x1_0000);
        assert_eq!(
            words(&asm),
            vec![0xd280_0000, 0xf2a0_0020] // movz x0, #0 ; movk x0, #1, lsl #16
        );
    }

    #[test]
    fn write_sequence_records_one_fixup_per_string() {
        let mut asm = Assembler::default();
        asm.emit_write_rodata(STDOUT_FD, b"hello\n");
        asm.emit_write_rodata(STDOUT_FD, b"bye\n");
        assert_eq!(asm.fixups.len(), 2);
        assert_eq!(asm.fixups[0].data_offset, 0);
        assert_eq!(asm.fixups[1].data_offset, 6);
        assert_eq!(asm.rodata, b"hello\nbye\n");
        // Each fixup names an adrp immediately followed by its add.
        for fixup in &asm.fixups {
            assert_eq!(fixup.add_offset, fixup.adrp_offset + 4);
            let adrp = u32::from_le_bytes(
                asm.code[fixup.adrp_offset..fixup.adrp_offset + 4]
                    .try_into()
                    .unwrap(),
            );
            assert_eq!(adrp & 0x9f00_001f, 0x9000_0001); // adrp x1, 0
        }
    }
}
