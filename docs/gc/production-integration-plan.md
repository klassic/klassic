# Production integration plan — M-G1 (2026-09-29)

Status: planned, not started. Branch `gc/generational-wip` (checkpoint
`0d674e8` holds the interrupted Codex session's component work).

## Why this milestone

The component work (C `runtime/gc/gc_*.h`, emitted `gc_codegen/*.rs`) is broad,
but nothing reaches ordinary compilation: `lib.rs`, `pe.rs`, `macho.rs` and
`aarch64.rs` never call `gc_codegen` outside `#[cfg(test)]`, and 211 emitters are
`#[allow(dead_code)]`. Adding more isolated controllers does not move G01-G15
forward. M-G1 builds the first production integration layer on one target.

Note: `runtime/gc/klassic_gc.c` is the **legacy** collector linked by the LLVM
backend (`llvm.rs:963-976`). The C form of the new ABI is the header-only
`runtime/gc/gc_*.h`.

## Scope

- Linux x86-64 ELF direct backend only, opt-in `--gc=generational`.
- Language subset: Int / Bool / Double, existing static/BSS lists, maps,
  records and lambdas (these never touch the GC heap on x86), and heap strings.
- Enums are rejected under the flag with a source-located diagnostic (the
  desugared program contains `__gc_read_ptr`, `__gc_write` or `__gc_record`).
- M-G1 does **not** reclaim. Heap exhaustion is an honest OOM exit. This is an
  integration scaffold, not a collector, and must not be reported as G-anything
  complete. Young-only collection is M-G2.

## Legacy touchpoints to replace (lib.rs, checkpoint 0d674e8)

| Entry | Def | Notes |
|---|---|---|
| `emit_initialize_gc_heap` | 39300 | called once at 5632 (startup) |
| `emit_initialize_color_registers` | 39243 | r13/r14/r15 reserved at 42689; not needed under new ABI |
| `compile_gc_alloc` / `compile_gc_record` | 14039 / 14074 | dispatchers at 8421, 9495 |
| string allocs (`compile_gc_string`, static/runtime -> heap string, `emit_heap_string_concat_from_slots`, `compile_gc_int_to_string`) | 14127-14600 | the 8 `portable_asm::emit_gc_alloc_dynamic` sites |
| `emit_gc_alloc_runtime` / `_large_runtime` | 39495 / 40045 | legacy allocator leaf; also runs mark quanta and `--gc-stress` |
| shadow stack: `register_heap_pointer_slot`, `root_initialized_heap_slot`, `emit_gc_shadow_push`, `emit_gc_shadow_pop_n` | 40306 / 40281 / 40174 / 40205 | used by `allocate_slot` (19) / `allocate_anonymous_slot` (25); args rooted at 37823; `pop_scope` 40128; function exit 37894 |
| `emit_literal_root_seed` / `literal_root_cells` | 40193 / 12391 | enum-only, out of scope for M-G1 |

Raw heap-string byte access (no barrier needed: RawBytes holds no references,
but the address must be re-derived from the root after every allocation point):
`emit_append_heap_string_to_runtime_buffer_offset_label` (36412),
`emit_heap_string_equality_from_regs` (14449), concat fragments, and
`emit_print_value_fragment`, which hands a raw managed pointer to a blocking
`write` syscall — copy out to BSS (or scoped pin) before the syscall.

Flag chain for the selector: `src/cli.rs:24-32` `ExecutionConfig`
(parsed 126-135, assigned 179-181) -> `src/main.rs:167-169` ->
`klassic_native::NativeCompilerConfig` (lib.rs:24-41) -> `NativeCodeGenerator`
fields (4522-4531, set at 705-707). Reject non-Linux-x86 targets and the
LLVM/C backends in `main.rs` next to the existing check at line 141.

## New-ABI pieces to reuse (gc_codegen, emitted, both ISAs)

- Reference: `(heap_offset << 16) | metadata16`, 0 = null; offset points at the
  payload, 16-byte aligned (`gc_abi.rs:47-135`, `:1008`). Address =
  `heap_base + (bits >> 16)` only after a committed-object-start check.
- Header: 32 bytes `{payload_bytes, descriptor_id, mark_record, state}`,
  state INITIALIZING=1 -> PUBLISHED=2 (`gc_abi.rs:1246-1247`, `:1324-1332`);
  minimum payload 16 bytes.
- RootHandle `{cell, reuse_generation, scope}` / RootCell 32 bytes (`gc_abi.rs:336,345`);
  `gc_codegen/roots.rs:20` handles RESERVE/STORE/RELEASE/READ_RAW/PREPARE_EMPTY.
- Allocation: `alloc_start.rs:8`, `heap.rs:13` (TLAB bind), `heap.rs:178`
  (try_tlab_allocate), `alloc_into.rs:23`, `initialization.rs:5` (publish).
- Descriptors: `descriptors.rs:86,321`, `native_builtin_registry.rs:17`;
  builtin ids in `gc_builtin_descriptors.h:10-12` (id 1 = raw bytes).
- Startup admission `native_startup.rs:9`; metadata map `native_metadata.rs`;
  context state `context.rs:15`.
- Calling convention: SysV-like, args in rdi/rsi/rdx (portable V7/V6/V2),
  result in rax (V0); args are pointers to `#[repr(C)]` request structs, result
  is a status (OK / REFILL / FLUSH / ...). Blobs only jump within themselves
  (`gc_atomic_tests.rs:618 finish_code`). The emitted-vs-C conformance pattern
  is `gc_atomic_tests.rs:239 emit_body` + `tests/atomic_harness.c`.

C-only today, must be emitted for M-G1 (minimal forms):
heap payload reservation (`gc_native_heap.h:24 gc_native_heap_reserve`),
runtime bootstrap/activation and initial mutator registration
(`gc_native_runtime.h:52,100`), and run provisioning / TLAB refill
(`gc_native_allocation.h`).

## Steps

1. Selector plumbing + rejection diagnostics (CLI test in `tests/cli_smoke.rs`).
2. Emitted startup: reserve heap, register descriptor id 1, attach the single
   mutator context, prepare root storage. Test: empty program builds and runs
   under the flag.
3. Roots: replace shadow-stack slot registration with RootCells in one step
   (retaining direct frame loads next to RootCells reads stale refs after a
   move — `compiler-migration.md:738-760`). Frame values are read via root
   READ_RAW.
4. Allocation: replace the 8 sites with reserve-root -> try_tlab_allocate ->
   initialize payload -> publish, with an emitted REFILL loop (run
   provisioning) and an honest OOM exit.
5. Raw byte access: re-derive addresses from roots after allocation points;
   copy out before `write`.
6. Differential tests: string-using `test-programs/` subset, evaluator output ==
   `--gc=generational` output, plus allocation-heavy concat loops up to OOM.
7. Docs: `docs/architecture-rust.md` addendum, update
   `default-integration-audit-2026-09-29.md` and `capabilities.md` without
   overstating (still no collection).

Each step: `cargo fmt --check`, clippy `-D warnings`, targeted tests, then the
existing legacy suites must stay green (default path unchanged).

## Next milestones (outline)

- M-G2: young-only collection for the M-G1 subset (roots are RootCells, so a
  moving young cycle can run at allocation-triggered safepoints first, then
  move toward concurrent phases with the existing mark/relocate emitters).
- M-G3: enums (reference descriptors ids 2/4, store barrier, load barrier,
  `gc_deep_equal`, literal roots).
- M-G4: PE and Mach-O parity, then workers/scheduler (G02, G12).
