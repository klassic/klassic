# Klassic Spec Matrix

Status values:
- `not started`
- `partial`
- `complete`
- `verified`

Classification values:
- `required`: part of the supported language/runtime surface
- `optional`: sample or roadmap behavior that is not yet part of the core contract
- `unknown`: needs more repository evidence before being promoted

Rust tests under `tests/` and crate-local unit tests are the executable
authority for this matrix.

| Feature | Classification | Evidence | Rust Status | Verification |
| --- | --- | --- | --- | --- |
| Native CLI entrypoint (`klassic`, `-e`, `-f`, positional `.kl`) | required | `README.md`, `tests/cli_smoke.rs` | verified | `cargo test`, CLI smoke, sample-program harness |
| Native x86_64 build command (`klassic build [--target linux-x86_64] <file> -o <output>`) | required | native compiler plan, `tests/cli_smoke.rs`, `tests/sample_programs.rs`, `test-programs/*.kl`, `examples/typeclass*.kl` | partial | `cargo test`; all 53 non-future top-level `test-programs/*.kl` native-build/run, and programs with existing golden stdout/stderr now compare native output against those expectations; all 4 promoted future-feature programs and all 3 typeclass examples native-build and run successfully; direct inline lambda calls, printable, callable, shadowing, and straight-line mutable top-level lambda, `def`, and builtin function values, runtime-argument calls through static lambda values, call-site inlined unannotated runtime string/list pass-through and string-literal concatenation functions with inferred or annotated runtime returns, block/function-local mutable closure captures including closures returned inside records/lists/maps and from unannotated functions plus queued thread bodies from blocks/functions/foreach, non-identifier callees returning function values with callee effects including side-effecting builtin function values, supported curried builtin helpers, first-class collection/Map/Set helper values, and first-class File/Dir helper values, runtime FileOutput write/append fixed-buffer RuntimeString content, runtime FileInput print streaming and immutable runtime FileInput bindings in print/string-concat/equality/assertResult/isEmptyString/length/toString/substring-at-with-static-or-runtime-indexes/trim/repeat-with-static-or-runtime-count/case-conversion/matches-with-static-or-runtime-literal-operands/replaceAll-with-static-or-runtime-pattern-and-replacement/reverse/startsWith/endsWith/method-contains/indexOf/lastIndexOf contexts, unknown virtual file state fallback for runtime `FileInput#all`, `FileInput#lines` / `readLines`, `FileOutput#exists`, `Dir#exists`, `Dir#isFile`, and `Dir#isDirectory`, runtime `Dir#current`/`Dir#home`/`Dir#temp`, runtime `CommandLine#args` line-list results through direct/unqualified/aliased/function-local calls, explicit `Process#exit` status codes, stdin runtime string and line-list input plus immutable source-helper aliases, environment variable line-list input through direct/aliased/function calls plus static/runtime key get/exists lookups, static and runtime string paths for file-input read/print and general `FileInput#open` callback bodies or callable values over the stream parameter plus direct `lines` / `readLines`, immutable printable runtime line bindings, mutable runtime string and line-list bindings with fixed-buffer assignment, runtime line-list `toString`/string concatenation, runtime line-list `map` with inline or aliased lambdas and builtin function values, String/Int/Bool/Null/Unit/List<String>-accumulator runtime line-list `foldLeft` with inline or aliased reducers, runtime `split` / `join` with static or runtime string delimiters, and open-readLines printing/bindings, FileOutput write/append/writeLines/exists/delete, Dir mkdir/mkdirs/delete/copy/move, Dir existence/type checks, and runtime Dir list/listFull line-list results, top-level binding captures in non-recursive functions and immutable static plus runtime string/line-list/selected-length-list/record top-level captures in recursive functions, annotated runtime `String` / `List<String>` parameters in scalar-returning recursive functions including reentrant calls and self-calls staged before shared parameter buffers are updated, annotated supported record parameters/returns with staged record arguments and call-site return copies including recursive runtime-record returns, direct/aliased `thread` calls in functions and lambda values plus zero-argument thread/stopwatch lambda-value arguments, dynamic `if` merges of divergent string and runtime line-list results including divergent static string-list and static/runtime line-list joins plus structurally equal lambda values and canonical builtin function values including equivalent branch-local mutable closure/record/list/map-closure and queued-thread captures, user-visible function equality matches the evaluator's false result, source-located assertion, `ToDo`, empty-head, negative `sleep`, negative string-helper index/count, FileOutput syscall failure, runtime `Dir#copy`, Dir mkdir/delete/move syscall failure, static-folded pure recursive helpers over static lists including static callable arguments, and recursive call-site-inline rejection diagnostics are emitted by native executables or native builds, static returned-lambda calls, static lambda bodies calling returned closures, static record lambda methods with effectful receivers/arguments, runtime `String` / `List<String>`, dynamic `Int` / `Boolean`, and nested runtime record fields with field selection through simple functions, printing, equality against compatible records, and runtime string display, stack-passed call arguments, static string `substring` / `at` with dynamic integer indexes, static string `split` and static string-list `join` with runtime delimiters, static string first-occurrence `replace` with runtime operands, static/runtime string `replaceAll` with static or runtime pattern and replacement strings, static string `repeat` with runtime integer counts, static-list `map` / `foldLeft` including method-style `map` / `foldLeft`, Int-list `foreach` static bindings, static literals/equality/assertResult including side-effecting mixed numeric equality, cleanup return preservation, statically selected `if` paths, statically skipped `while` bodies with condition effects, dynamic `if` static-value and virtual File/Dir state merges, numeric helper recovery, binary folds/string concat, logical short-circuit static fact merging, call-argument folds, static helper arguments, static `cons` arguments, File/Dir helper arguments, dynamic-control assignments, dynamic `while` static invalidation, and `FileInput#open` callback bodies/callable values preserve impure side effects and can return supported runtime values |
| Direct ELF64 writer / handwritten x64 emitter | required | native compiler plan, `klassic-native` tests | partial | `cargo test -p klassic-native`; generated executables run on Linux x86_64 |
| REPL with `:exit` and `:history` | required | `tests/cli_smoke.rs` | verified | `cargo test` |
| `--deny-trust` / `--warn-trust` | required | `tests/cli_smoke.rs`, evaluator tests | verified | `cargo test`, CLI smoke |
| Line comments | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Nested block comments | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Arithmetic precedence and associativity | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Unary `+` / `-` on integers | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Integer literals | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Long / float / double literals | required | `test-programs/numeric-literals.kl`, `tests/language_regressions.rs` | verified | `cargo test`, sample-program harness |
| Boolean / string / unit literals | required | `tests/language_regressions.rs` | verified | `cargo test` |
| List literals with comma / space / newline separators | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Map literals with comma / space / newline separators | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Set literals with comma / space / newline separators | required | `tests/language_regressions.rs` | verified | `cargo test` |
| String interpolation | required | `test-programs/string-interpolation.kl`, `tests/language_regressions.rs` | verified | `cargo test`, sample-program harness |
| `val` / `mutable` / assignment / compound assignment | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Lambdas and named recursive `def` | required | `README.md`, `tests/language_regressions.rs` | verified | `cargo test` |
| `if`, `while`, `foreach`, ternary | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Cleanup expressions | required | `test-programs/cleanup-expression.kl`, `tests/language_regressions.rs` | verified | `cargo test`, sample-program harness |
| Placeholder desugaring (`_`) | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Records and record field selection | required | `test-programs/record.kl`, `tests/language_regressions.rs` | verified | `cargo test` |
| Row polymorphism and record typing | required | `test-programs/future-features/record_inference.kl`, `tests/language_regressions.rs`, `crates/klassic-types/src/lib.rs` unit tests | verified | `cargo test`; nominal records can satisfy structural row field functions, and row field type mismatches after numeric use are rejected |
| Hindley-Milner inference / schemes / annotations | required | `tests/language_regressions.rs`, `crates/klassic-types/src/lib.rs` unit tests | verified | `cargo test`; generalized lambda results are resolved before annotation checks, contextual lambda checking handles `foldLeft` reducers, and polymorphic annotation mismatches are rejected |
| Dynamic escape hatch `*` | required | `test-programs/type-cast.kl`, `tests/language_regressions.rs` | verified | `cargo test`, sample-program harness |
| Type classes and instances | required | `tests/language_regressions.rs`, `test-programs/future-features/typeclass-*.kl` | verified | `cargo test`, sample-program harness |
| Higher-kinded type classes / kind annotations | required | `tests/language_regressions.rs`, `test-programs/higher-kinded-typeclass.kl` | verified | `cargo test`, sample-program harness |
| Modules / imports / aliases / selective imports | required | `tests/language_regressions.rs` | verified | `cargo test` |
| User-defined module persistence across evaluations | required | `tests/language_regressions.rs` | verified | `cargo test` |
| File input module | required | `test-programs/file-input.kl`, `tests/language_regressions.rs` | verified | `cargo test`, sample-program harness |
| File output module | required | `test-programs/file-output.kl`, `tests/language_regressions.rs` | verified | `cargo test`, sample-program harness |
| Directory module | required | `tests/language_regressions.rs` | verified | `cargo test` |
| String helper builtins | required | `tests/language_regressions.rs` | verified | `cargo test` |
| List / map / set helper builtins | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Thread / sleep / stopwatch builtins | required | `test-programs/builtin_functions-thread.kl` | verified | `cargo test`, sample-program harness |
| Runtime error helpers (`assert`, `assertResult`, `ToDo`) | required | `tests/language_regressions.rs` | verified | `cargo test` |
| Macro PEG subsystem | required | `klassic-macro-peg`, `tests/language_regressions.rs` | verified | `cargo test -p klassic-macro-peg`, `cargo test` |
| Theorem / trust / axiom surface | required | `tests/cli_smoke.rs`, evaluator tests | verified | `cargo test`, CLI smoke |
| Promoted future-feature programs | optional | `test-programs/future-features/*` | verified | `cargo test --test sample_programs`; native build/run coverage is included for all 4 promoted programs |

## Current Milestone

The repository contains a Rust-native language implementation with:

- native `klassic` binary via Cargo
- expression/file/REPL execution
- source spans and diagnostics
- parser, rewrite pass, typechecker, evaluator, builtins, modules, and macro PEG
- a first native compiler vertical slice for Linux x86_64 ELF64 executables,
  including annotated boolean function arguments/returns, stack-passed native
  function arguments beyond the first six integer/boolean parameters,
  annotated runtime `String` / `List<String>` parameters in scalar-returning
  recursive functions, including reentrant calls and self-calls staged before
  shared parameter buffers are updated,
  fixed-buffer annotated `String` / `List<String>` function returns copied into
  call-site buffers, annotated supported record parameters/returns with staged
  record arguments and call-site return copies including recursive runtime-record
  returns, function value aliases, static record fields, runtime
  `String` / `List<String>`, dynamic `Int` / `Boolean`, and nested runtime record fields with
  compatible equality/display, dynamic `if` branch merging, and mutable assignment
  from runtime or supported static initializers, runtime line-list `foldLeft`
  record accumulators, direct list-literal `head` returning runtime native values,
  direct list-literal `tail` returning runtime line-list values or runtime-list
  values for non-string tails,
  literal `contains` / `containsKey` / `containsValue` over runtime native values,
  literal list/map/set `size` and list/map/set `isEmpty` selectors preserving effects,
  list-literal `map` over runtime native values into runtime line-list or
  runtime-list results,
  list-literal `join` over runtime string values into runtime string results,
  list/map/set-literal display / `toString` / printing / interpolation /
  string concatenation over runtime native values into runtime strings,
  list/map/set-literal equality / `assertResult` over runtime native values
  and static collection bindings,
  immutable runtime list-literal bindings with display, equality, list-helper, `cons`, `map`, `foldLeft` including list-building accumulators, `join`, and `FileOutput#writeLines` support,
  fixed-capacity dynamic `if` merges for runtime-list values and record fields carrying them, with selected-length preservation whenever a runtime-list branch participates,
  annotated `List<String>` function argument/return interop for compatible runtime-list selected prefixes,
  runtime record fields carrying runtime-list selected prefixes through field access, display, equality, and annotated record returns,
  straight-line mutable runtime-list rebinding through list-helper chains and dynamic `while` runtime-list assignment with mutable selected-length storage,
  list-literal `foreach` and scalar/string/line-list/runtime-list/record-accumulator `foldLeft` over runtime native values,
  map-literal `Map#get` / `.get` returning runtime native values including variable-length runtime-list values from static or runtime keys, direct runtime-key `Map#get(...) == null` / `!= null` checks over static maps and map literals, with selected-prefix `contains`, `cons`, `foreach`, `map`, scalar/string/line-list/record `foldLeft`, runtime-list accumulator `foldLeft`, `join`, display/printing, and runtime-list equality,
  direct or method-style static-list `head` lookups including `tail` and
  `cons` chains, and static `Map#get` /
  `.get` lookups with literal or folded static keys plus runtime string/int/bool
  `Map#get` / `.get` keys for static maps with uniform
  string, string-list, int, boolean, supported static record, non-string static-list, including variable-length list storage, `null`, or `()` compatible values and
  equivalent static value compatible entries including callables, runtime record
  membership through static list/set `contains` and map `containsValue`,
  static-key map-literal lookups returning runtime native values, immediate
  runtime-key dispatch through static callable maps plus immutable runtime
  string/int/bool-key callable map lookup bindings with selected callable
  display printing/interpolation/string concatenation/`toString`, frame-independent
  recursive captures, and evaluator-style
  false function equality, runtime line-list
  membership against static string-list entries, static membership for
  effectful queries that settle back to static values, incompatible-key static
  `null` and all-null compatible runtime map lookups, preserving those runtime
  return hints plus block, cleanup, and
  same-runtime-return conditional callees preserving them, immediate calls on conditional function values,
  pure conditional callable branches in immutable bindings or static aggregate elements synthesized into branch-local-call lambdas for user callables and supported builtin function values with matching arity while preserving selected-branch builtin display through printing, interpolation, string concatenation, `toString`, and failing `assertResult` messages, including returned callables and aggregate display, effectful callee expressions returning string/list helper builtin values with runtime string or runtime line-list arguments,
  top-level and inline lambda calls with the same annotated parameter matching,
  call-site inlined unannotated pass-through and string-literal concatenation `def`s over runtime `String` / `List<String>` values with inferred or annotated runtime returns,
  immutable static string/list bindings, simple unannotated integer/boolean return inference,
  streamed `println` / `printlnError` string interpolation, compile-time folded
  interpolation for immutable static values with preserved fragment block effects,
  fixed-buffer runtime string interpolation for native runtime string and
  dynamic native `Int` / `Boolean` fragments,
  static string helpers including
  `split` / `join`, static string concatenation plus runtime string
  concatenation and `toString` with dynamic native `Int` / `Boolean` operands
  plus displayable static-native values after dynamic/effectful evaluation, static helper calls inside
  immutable static values, static and runtime integer-millisecond `sleep` via Linux `nanosleep`,
  zero-argument literal or lambda-value `stopwatch` via Linux `clock_gettime`, Int numeric
  helpers (`abs`, `int`, `floor`, `ceil`), static Double/Float literals and
  numeric helper folding (`double`, `sqrt`,
  `abs`, `floor`, `ceil`) with Float preserving f32 rounding/display and
  effectful block-prefix arguments preserved before static numeric recovery, static
  Int-list `foreach`, static Int list literals
  with constant arithmetic and bitwise elements, generic static list arenas,
  printable runtime line-list bindings with `size` / `isEmpty` / `head` /
  `tail` / `cons` / `contains` / `map` with inline or aliased lambdas and
  builtin function values /
  String/Int/Bool/Null/Unit/List<String>-accumulator direct or method-style `foldLeft` with inline or aliased reducers /
  runtime `split` / `join` with static or runtime string delimiters / runtime `foreach` /
  `toString` / string concatenation / static-list and runtime-list equality /
  `assertResult` / `FileOutput#writeLines` write-back, and
  `size` / `isEmpty` / `contains` / `head` / `tail` / static `cons` / static `map` / static
  direct or method-style `foldLeft`, with generic static `cons`, static generic-list `foreach`
  unrolling plus static mapper `map`, static numeric/string accumulator
  `foldLeft`, and Int-list `foldLeft` reducers that build static lists,
  static string `substring` / `at` with dynamic integer indexes,
  static string `split` and static string-list `join` with runtime delimiters,
  static string first-occurrence `replace` with runtime operands,
  static/runtime string `replaceAll` with static or runtime pattern and replacement strings,
  static string `repeat` with runtime integer counts,
  static `if` folding
  for aggregate values, plus ordinary static aggregate equality including compact
  Int-list versus generic static Int-list comparison,
  static nominal/structural record construction, field selection, printing, and
  static map/set/File/Dir helper calls including runtime string/int/boolean
  membership for static string-key/string-valued maps, string sets, and scalar
  list/set/map entries plus builtin module aliases/imports and helper value aliases,
  immutable static `null` / `()` bindings, and static string/list/record/map/
  set/null/unit `assertResult`, native `ToDo()` runtime failure emission, plus
  native `()` printing and equality, and immutable aliases to directly supported
  builtin functions and curried helpers such as `val sub = substring` and
  `val folder = foldLeft`, inline lambda calls with
  mutable captures, straight-line mutable static aggregate values, direct static
  typeclass methods, static higher-kinded List-style `map` / `bind` / `unit`,
  placeholder-derived callable aliases and returned static lambda values for
  `map` / `foldLeft`, static file helpers, static record lambda methods, queued
  native thread bodies for the sample surface, and fold-like three-stage curried
  calls over static lists, including captured dictionary records used by the
  full typeclass dictionary-passing examples, plus static `Dir` helpers for
  hermetic filesystem workflows
- sample-program and integration-test coverage for the supported language surface

Required behavior identified in this matrix is implemented and backed by Cargo tests.
The native compiler is intentionally marked partial until it reaches evaluator
parity for all required non-Java-FFI language features.
