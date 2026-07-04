# Klassic Rust Architecture

## Summary

Klassic is a Rust workspace with small crates for the language pipeline. The
root package builds the native `klassic` binary.
The native backend direction is tracked in
[`docs/native-backend-strategy.md`](native-backend-strategy.md): keep the direct
backend as the bootstrap path, while preserving a target/backend boundary for a
future optional LLVM backend.

```bash
cargo build
cargo test
cargo run -- -e "1 + 2"
```

## Pipeline

1. Source text
2. `klassic-span`: source files, spans, and diagnostics
3. `klassic-syntax`: lexer, parser, and AST
4. `klassic-rewrite`: placeholder desugaring and syntax normalization
5. `klassic-types`: type inference, records, typeclass constraints, and proof checks
6. `klassic-eval`: evaluator, modules, builtins, and REPL/session state
7. `klassic-native`: Linux x86_64 native compiler, x64 emitter, and ELF64 writer
8. Root binary: CLI argument handling and diagnostic presentation

## Crate Layout

### `klassic-span`
- Source file storage
- Byte spans
- Line/column mapping
- Human-readable diagnostics

### `klassic-syntax`
- Lexer
- Recursive-descent parser
- Untyped AST
- Functions, modules/imports, records, typeclass/instance declarations,
  theorem/trust/axiom declarations, collection literals, and type annotations

### `klassic-rewrite`
- Placeholder desugaring
- Syntax lowering / normalization

### `klassic-types`
- Types, schemes, substitutions, row-polymorphic records, constraints, and typed checks
- Immutable-binding generalization / instantiation
- Builtin and module signatures
- Nominal and structural record typing
- Contextual lambda checking for call arguments, including curried reducers
- Typeclass and higher-kinded constraints
- Theorem and trust-surface checks

### `klassic-eval`
- Parse -> rewrite -> typecheck -> evaluate wrapper
- Runtime values and evaluator
- Builtins, modules, records, typeclass dictionaries, and thread/file/dir helpers
- REPL/session state

### `klassic-native`
- Batch native compilation through `klassic build [--target linux-x86_64|x86_64-unknown-linux-gnu|macos-aarch64|aarch64-apple-darwin|windows-x86_64|x86_64-pc-windows-msvc|native] <file.kl> -o <output>`
- Reuses the Rust parser, rewrite pass, typechecker, and proof/trust analysis
- Emits target-native machine code directly; `LinuxX86_64`, `MacOsAArch64`, and `WindowsX86_64` are implemented
- Also emits ad-hoc-signed Mach-O arm64 executables for `aarch64-apple-darwin` (direct backend, growing subset)
- `WindowsX86_64` reuses the `LinuxX86_64` codegen backend (`DirectX86_64`) wholesale and only swaps the OS boundary (Win64 import-call shims) and container format (PE64)
- Carries an explicit `NativeTarget`; the implemented targets are `LinuxX86_64`, `MacOsAArch64`, and `WindowsX86_64`
- Keeps supported targets in a metadata registry containing the compact name,
  standard triple, architecture, direct-codegen backend, data layout,
  operating system, ABI, and executable format
- Exposes supported target names from the native target API so CLI help and
  target parsing stay aligned with the registry
- Resolves a per-compile native target context that pairs the selected target
  spec with its platform constants before entering codegen
- Keeps target-specific syscall numbers and OS constants such as fds, open
  modes, errno values, stat masks, clocks, mmap flags, and sendfile limits in a
  target-keyed platform constants registry; `TargetPlatform::syscall_number`
  panics for the Windows target as a deliberate tripwire (see below)
- Writes ELF64 executables (Linux x86_64), ad-hoc-signed Mach-O arm64 executables (Apple Silicon), and PE64 executables (Windows x86_64) directly without invoking `cc`, `as`, `ld`, Java, Scala, or the JVM
- Current native codegen covers the first vertical slice: integer and boolean
  expressions, string literal printing, `println` / `printlnError`, `assert`,
  curried `assertResult`, `if`, `while`, mutable integer/boolean locals,
  assignment, static integer-list `foreach` unrolling, top-level recursive
  integer functions, static and runtime integer-millisecond `sleep` via Linux `nanosleep`,
  zero-argument literal or lambda-value `stopwatch` via Linux `clock_gettime`, annotated
  boolean-returning / boolean-argument functions, queued `thread` bodies from
  literal or lambda-value jobs for the current native sample surface, simple unannotated
  integer/boolean return inference, stack-passed arguments beyond the first six
  integer/boolean native function parameters, call-site inlined unannotated
  pass-through and string-literal concatenation `def`s over runtime `String` /
  `List<String>` values even when only the return is annotated, annotated
  runtime `String` / `List<String>` parameters for scalar-returning recursive functions, including
  reentrant and self-calls staged before shared parameter buffers are updated,
  fixed-buffer annotated `String` / `List<String>` returns copied into call-site buffers, function
  value aliases, annotated supported record parameters and returns with staged
  record arguments and call-site return copies, including recursive
  runtime-record returns, static record fields, runtime `String` / `List<String>`,
  dynamic `Int` / `Boolean`, and nested runtime record fields, direct or method-style static-list
  `head` lookups including `tail` and `cons` chains, static `Map#get` / `.get`
  lookups with literal or folded static keys preserving those runtime return
  hints and record display paths, and runtime string/int/bool key
  lookups from static maps when the compatible values are strings, string lists,
  ints, booleans, supported static records, or non-string static lists, plus block, cleanup,
  and same-runtime-return conditional callees
  preserving those hints,
  immediate calls on conditional function values lowered to branch-local calls,
  pure conditional callable branches in immutable bindings or static aggregates
  lowered to synthesized branch-local-call lambdas for user callables and
  supported builtin function values with matching arity, with conditional
  builtin display metadata preserving selected-branch `<builtin:name>` output
  through printing, interpolation, string concatenation, and `toString`,
  including for returned callables and static aggregates in bound interpolation
  strings,
  top-level and inline lambda calls with the same annotated parameter matching,
  and top-level lambda bindings lowered as static functions or inlined at call
  sites when they capture mutable locals.
  Temporary stack pushes used while evaluating native call arguments are tracked
  alongside local slots, so nested argument expressions can allocate closure
  captures without overwriting saved argument values.
  Startup computes a stack floor from `getrlimit(RLIMIT_STACK)` (initial
  rsp minus the limit plus a 256 KiB margin) and every emitted function
  prologue probes rsp against it, so recursing past the stack reports
  `klassic: stack overflow` and exits 1 instead of dying on the guard
  page with a bare SIGSEGV. The probe is inert when the limit is
  unlimited (floor zero) and skipped entirely for programs that spawn
  threads, whose stacks live at unrelated addresses.
  `println` / `printlnError` stream simple
  string-concatenation and interpolation expressions directly until a heap string
  runtime is added. That streaming fast path is skipped when a fragment performs
  its own console output (a `println` reached directly or transitively through a
  called function): such concats / interpolations are materialized first so the
  nested output lands before the surrounding text, matching the evaluator instead
  of interleaving it. Interpolated strings can also be folded into static native
  values when all fragments resolve through immutable static bindings, including
  fragments with mutable block prefixes when their final values remain
  recoverable. A hole that assigns to a variable it also displays declines the
  fold (which would show the pre-mutation value) and runs through the runtime
  path so the displayed value reflects the mutation in order. Interpolation
  fragments that resolve to native runtime strings,
  or to dynamic native `Int` / `Boolean` values, produce fixed-buffer
  `RuntimeString` values. Because that fixed buffer and its offset cell are a
  single static slot per interpolation site, a hole that can re-enter the site
  (a call into a function that itself interpolates, including direct or mutual
  recursion) snapshots the in-progress prefix into a GC heap string, evaluates
  the hole, then restores the prefix and offset before appending the fragment;
  without that snapshot the inner build resets the offset to 0 and the outer
  resumed appending at the wrong position. Collection-literal holes keep their
  in-place short-circuit and never reach the snapshot path.
  Static string and integer list values can be bound with `val`; static string
  helpers, including `split` and `join`, are folded at compile time for native
  codegen, and static string concatenation can produce immutable static values
  from literals, static bindings, and static helper calls. Runtime string
  concatenation can also format dynamic native `Int` / `Boolean` operands into
  fixed-buffer runtime strings, and `toString` uses the same display path for
  dynamic native `Int` / `Boolean` values plus displayable static-native values
  that survive dynamic/effectful evaluation.
  Int `abs`, `int`, `floor`, and `ceil` are emitted directly. Static
  Double/Float literals and numeric helpers such as `double`, `sqrt`, `abs`,
  `floor`, and `ceil` are folded into printable native constants, with Float
  values kept at f32 precision for evaluator-matching display; helper arguments
  with mutable block prefixes can still be evaluated before recovering the final
  static numeric value. Static integer
  list literals, including simple constant arithmetic and bitwise elements, are
  stored in the native data section and support
  printing plus `size`, `isEmpty`, `contains`, `head`, `tail`, and static `cons` / `map` /
  `foldLeft`. Static non-integer lists live in a compile-time arena and support
  printing, `size`, `isEmpty`, `contains`, `head`, `tail`, `join`, and equality, plus static
  `foreach` unrolling, static `map` over static mappers, and static `foldLeft`
  over static numeric/string accumulator reducers. Static `cons` also covers
  generic static lists. Known Int-list `foreach` bindings are exposed as static
  facts inside the unrolled body, and Int-list `foldLeft` can produce static list
  accumulators. Immutable aliases to directly supported builtin functions, such
  as `val sub = substring`, are resolved back to their builtin call target during
  native codegen, and builtin function values keep evaluator-style
  `<builtin:name>` display when printed directly or inside static aggregates.
  Builtin values stored in static record fields or lists can also be called when
  the recovered arguments stay static. Mutable builtin aliases can be rebound to
  compatible builtin aliases on the straight-line static path.
  Static `if` expressions with recoverable boolean conditions
  can compile only the selected branch, preserving condition side effects before
  producing static native aggregate values, and simple mutable-loop effects are
  tracked for later static folds without folding the generated loop condition.
  Straight-line mutable static strings and generic static lists can be reassigned
  when the new value is also static; straight-line mutable function values can
  likewise be rebound to static lambda / `def` / builtin values. Dynamic `if`
  codegen compiles each branch against an isolated binding, virtual File/Dir,
  and queued-thread state, then merges only facts that are representably
  identical on both paths. Identical static aggregate returns, assignments, or
  virtual file contents can survive a runtime branch. Divergent string and
  runtime line-list branch results are materialized into shared fixed runtime
  buffers so the selected value can flow past the join, and divergent static
  string-list branches can join with each other or with runtime line-list
  branches through the same buffer. When neither branch directly yields a
  runtime line-list (both branches are static string-list literals), the
  join prefers the runtime-list buffer over the line-list buffer so
  downstream `map` / `foldLeft` callers can return non-string results
  through the runtime-list helper path. When the input source is itself a
  runtime line-list (for example, `FileInput#lines` joined with a static
  string list), `map` over the line list also accepts non-string scalar
  mapper bodies whose return type is provably `Int` or `Boolean`; the
  scalar-output path materializes the result into a fixed runtime list of
  back-to-back `i64` slots indexed at runtime.
  Divergent static list-like branches whose
  element types match (for example, two `Int` lists or two `String` lists of
  the same length) are also materialized through a runtime-list buffer so the
  joined value flows past the join. Divergent integer-list branches with
  different lengths share the same buffer with dynamic length tracking by
  padding the shorter branch's slots, so `[1, 2]` and `[3, 4, 5]` can join
  through one runtime list even though their lengths differ. The same buffer
  padding works for boolean-list and string-list branches whose lengths
  differ; padded scalar slots use a default value and padded string slots use
  an empty static string, both kept beyond the dynamic length the consumer
  observes. The branch shape detector and length predictor recognize `cons`
  and `tail` over static lists, so branches like `cons(99)([1, 2, 3])` versus
  `[4, 5]` and `tail([1, 2, 3])` versus `[40, 50, 60]` route through the same
  runtime-list buffer. The detector also accepts `map(...)(...)` calls and
  method-style `xs.map(f)` calls over static-list inputs, so
  `if(...) map([1, 2])(f) else map([3, 4, 5])(g)` and
  `if(...) [1, 2].map(f) else [10, 20, 30].map(g)` join through the runtime
  buffer using each branch's known input length. The same recognizer extends
  to `map(xs)(f)` and `xs.map(f)` over runtime-list inputs, so a runtime-list
  map result can be one branch of a dynamic if whose other branch is a static
  list literal. When one branch is a literal `null` and the other branch is
  list-like (static or runtime), the merge allocates the runtime-list buffer
  using the list branch's predicted length, sets the dynamic length to a
  sentinel of `-1` for the null branch, and answers `xs == null` /
  `null == xs` (and the `!=` variants) by checking the dynamic length for
  that sentinel; downstream operations (`size`, `head`, iteration) observe
  the actual length on the non-null path. Divergent static-set branches
  (`if(...) %(1, 2) else %(3, 4, 5)`) also merge through a runtime-list
  buffer that holds each branch's elements, with `size` / `Set#size` /
  `isEmpty` / `Set#isEmpty` / `contains` answering from the per-branch
  dynamic length and slot contents. `s.toList()` projects a static set's
  element vector into a fresh static list.
  Divergent static-map branches (any entry count, including
  `if(...) %["x": 1] else %["y": 2, "z": 3]`) also merge through a
  runtime-list buffer where keys and values alternate, with the buffer
  tagged so `Map#size` (and `size`) divide the dynamic length by 2 to
  return the entry count, and `Map#isEmpty` reuses the same length probe.
  The branch buffer padding uses a stride of 2 so the alternating
  key/value pattern survives even when one branch has fewer entries than
  the other. `Map#get` over a runtime map uses an unrolled linear search
  that compares each key slot (gated by the dynamic length so unused
  entries are skipped) and returns the corresponding value when matched,
  or zero/null/empty-string when not found, for static-string keys and
  scalar/string values. `Map#containsKey`, `Map#containsValue`, and
  `Map#get(...) == null` / `!= null` reuse the same unrolled search and
  cache the lookup needle in a fresh data slot so the comparison loop can
  reload the needle between iterations without losing it to register
  reuse. The two-argument method `m.getOrElse(k, d)` lowers to a
  temp-bound `{ val m' = m; val k' = k; if (m'.containsKey(k')) m'.get(k')
  else d }`, reusing the supported `containsKey` / `get` / `if` codegen
  while evaluating `m` and `k` exactly once. `m.keys()` / `m.values()`
  reuse the `__gc_smap_keys`/`__gc_smap_values` list builders for a runtime
  map and project a static map literal's compile-time entries into a fresh
  static list. `m.put(k, v)` builds a fresh static map from a static map's
  entries — updating an existing key in place or appending a new one — so
  the original map is untouched. `m.remove(k)`, `s.add(x)`, and `s.remove(x)`
  build a fresh static map/set the same way (a set add dedups), interning the
  filtered or extended entries, and `a.union(b)` / `a.intersect(b)` /
  `a.subtract(b)` combine two static sets — taking the receiver's element
  order as the base — into a fresh interned set. The kind tag also drives display: `println` and string
  interpolation emit `%(v1, v2, ...)` for runtime sets and
  `%[k: v, ...]` for runtime maps, matching the static-collection display
  rather than falling back to the bracketed list form.
  The per-element branch buffer also promotes static
  inner lists, static records, static maps, and static sets into runtime
  list / runtime record / kind-tagged runtime list buffers, so
  `[[1, 2], [3, 4]]` versus `[[5, 6]]`, `[#Pt(1, 2)]` versus `[#Pt(3, 4)]`,
  `[%["x": 1, "y": 2], %["z": 9]]` versus `[%["a": 100, "b": 200, "c": 300]]`,
  and `[%(1, 2), %(3, 4, 5)]` versus `[%(99)]` all join through the same
  runtime list of nested buffers. The
  per-position inner-length predictor pairs each outer-list position from
  both branches, takes the maximum inner length, and sizes the inner runtime
  list buffer accordingly, so `[[1, 2, 3], [4]]` and `[[5], [6, 7, 8]]` join
  even though their per-position inner lengths differ. When the outer
  branches have different lengths, the predictor falls back to the global
  maximum inner length so every padded outer slot can still hold either
  branch's longest inner list. The outer-list padding template also accepts
  static records, static int-lists, and static lists, so divergent
  record-list and nested-list branches with different outer lengths can use
  the same padding path. A separate global `deep_min_capacity` tracks the
  maximum capacity found anywhere in either branch's nested list, set, or
  map literals (counting map entries with the alternating key-value stride)
  and propagates uniformly to every nested level, so three-or-more-level
  nested list branches with mismatched deepest inner lengths (such as
  `[[[1, 2]]]` versus `[[[3, 4, 5]]]`), and lists of maps or sets with
  different inner sizes, all join through one padded buffer.
  Function values are merged by structural lambda equality or canonical builtin identity rather than
  raw label identity, so equivalent branch-local function values remain usable.
  When both dynamic branches return equivalent closures that capture branch-local
  mutable slots, their preserved stack depth must match and is carried across
  the join so later closure calls do not reuse the captured storage. Closure
  equality compares only captures that are actually referenced by the lambda
  body, which keeps returned records of equivalent closures mergeable even when
  their surrounding static environments contain unused branch-local names.
  Queued thread bodies use the same structural comparison and stack-depth rule,
  so equivalent branch-local thread captures can also survive a runtime `if`.
  Divergent aggregate / function mutation and divergent queued native threads
  remain compile-time errors. Divergent virtual file state is retained as an
  unknown path; later `FileInput#all` reads and existence/type checks use
  runtime syscalls instead of folding stale build-time facts.
  Logical `&&` and `||` keep normal short-circuit behavior
  and merge static facts conservatively across the skipped/executed RHS.
  Static `map`, `foldLeft`, fold-like three-stage curried calls, direct static
  typeclass methods, and List `bind` / `unit` calls support lambdas that can be
  folded into native constants or data sections. Mapper and reducer callables
  may also be placeholder-derived lambda aliases or top-level functions,
  including static functions that return lambda values, lambda bodies that call
  those returned static closures, and lambda fields that capture dictionary
  records in the typeclass dictionary-passing examples. Top-level lambda
  bindings and top-level `def` declarations are also kept as printable static
  function values. Static lambda values, including mutable aliases and rebinding
  cases, can also be called with runtime integer/boolean arguments by inlining
  the stored body and captured static environment. Non-identifier callees that
  evaluate to a static lambda or builtin function value preserve callee side
  effects before applying the returned callable, including side-effecting builtin
  values like `println`, `sleep`, `assert`, `thread`, and File/Dir helpers that
  update or inspect the native virtual file-system facts, plus string/list
  helpers such as `toUpperCase`, `split`, `join`, and `contains` when their
  arguments are runtime strings or runtime line lists. Supported curried helpers
  returned from effectful callee expressions, such as `assertResult`, `cons`,
  `contains`, `map`, imported `Set#contains`, and three-stage `foldLeft`, preserve
  those callee effects too. Collection and Map/Set helper builtin values such as
  `size`, `head`, `tail`, `isEmpty`, `contains`, `Map#get`, and
  `Set#contains` use the same static helper path when they are called through
  effectful value expressions. Runtime line-list values can also be compared
  against static string-list collection entries, and effectful query values that
  settle back to static values can still use static collection membership.
  Runtime record values copied from static map lookups can be compared
  structurally against static record entries through static list/set `contains`
  and map `containsValue`.
  Map literal `Map#get` / `.get` can return runtime native values from static
  or runtime keys after preserving map-entry and key effects, including
  variable-length runtime-list results selected from static or map-literal
  entries and nested record fields. Those variable-length labels keep helper
  iteration on the selected prefix for `contains`, `cons`, `foreach`, `map`,
  scalar/string/line-list/record `foldLeft`, runtime-list accumulator
  `foldLeft`, `join`, display, printing, and runtime-list equality.
  Static maps can lower `Map#get` / `.get` with runtime string/int/bool keys to
  native comparisons when the compatible values are uniformly string,
  string-list, int, boolean, supported static record, non-string static-list,
  `null`, or `()`, or when
  every compatible entry returns an equivalent static value, including the same callable value.
  A runtime key whose type has no compatible static keys returns static `null`.
  All-`null` compatible values also collapse to static `null`, because hits and
  misses are indistinguishable at the value level. Runtime misses among
  compatible keys report a native diagnostic when the selected value must be
  materialized, but direct `Map#get(...) == null` / `!= null` checks lower to
  key-match tests instead of materializing a dynamic tagged `null`.
  Immediate calls through runtime-key lookups of static callable maps, such as
  `Map#get(fns, key)(...)` and `fns.get(key)(...)`, dispatch to the selected
  lambda or builtin branch and merge the supported native return shapes.
  Runtime string/int/bool-key lookups over all-callable static maps can also be
  stored in immutable values, called later, and formatted through printing,
  interpolation, string concatenation, or `toString` with the same branch
  dispatch; equality involving these function values keeps the evaluator's
  always-false function comparison semantics. Recursive functions can capture
  bound callable dispatch values whose selector and candidates do not depend on
  the recursive function frame.
  Lambdas also remember the native stack slots for captured runtime bindings;
  when a block, inline lambda, or call-site inlined function returns such a
  lambda, the captured slots are kept alive so block/function-local mutable
  closure state survives across repeated calls. Static
  records/lists/maps/sets are checked recursively for returned closures, so
  multiple closures stored in a returned record can share the same captured
  mutable slot. Queued native `thread` bodies carry the same runtime capture
  metadata, allowing block-local mutable state to survive until queued thread
  bodies are emitted and run. `thread` itself can queue zero-argument lambda
  values as well as literal lambdas. Functions or static lambdas whose bodies queue
  threads directly or through immutable aliases are compiled on the caller's
  effectful path so queued bodies are attached to the current native execution
  stream rather than to the later function-emission pass or a static fold.
  Recursive functions that would otherwise require unsupported call-site
  inlining are still allowed to fold when called with static arguments, covering
  pure helpers over static lists, including `cons`-built list returns and static
  callable arguments, without entering the emitted recursive ABI path.
  Non-recursive top-level `def` declarations that close over top-level bindings
  are call-site inlined; recursive `def` declarations can still capture immutable
  static top-level values, builtin aliases, static lambda values, and immutable
  runtime string / line-list / selected-length runtime-list / runtime-record
  bindings by rebinding them inside the emitted function frame. Capturing a
  top-level `mutable` binding from a recursive (out-of-line) function is refused
  with a clean diagnostic rather than compiled: such a global lives in the
  top-level frame's slot, which the callee frame cannot address, so rebinding it
  would snapshot a stale value and discard writes. The non-recursive case is
  inlined and reaches the real slot, so it keeps working. Direct calls and value aliases for
  user-defined functions shadow same-named native builtins.
  A closure created inside a `foreach`/`while` body captures a loop-controlled
  or loop-body-local binding by frame-relative stack slot, not by value: a
  compile-time-unrolled foreach reuses that slot on every iteration once the
  iteration's scope is popped, and a `while` loop (or a runtime-length
  foreach) compiles its body once and shares the single slot across every
  runtime iteration. That is invisible as long as the closure is only called
  within the iteration that created it, but assigning it -- directly, or
  nested in a list/record/map/set -- into a binding that outlives every
  currently-open loop lets it later read whatever the slot happens to hold
  instead of the value it captured, so such an assignment is refused with a
  clean diagnostic rather than compiled. The check only counts a capture the
  closure body actually reads as a free variable, so a closure that merely
  happens to be compiled alongside an unrelated loop-scoped binding (e.g. it
  only reads a pre-loop `val`) still escapes cleanly, and a closure called
  within the same iteration (directly or via a helper) never triggers it
  since the slot is still live at that point.
  Immutable aliases to curried helpers such as `assertResult`, `cons`, `map`,
  and `foldLeft` resolve through the same native special-call paths as direct
  helper calls.
  Direct inline lambda calls are compiled at the call site when their arguments
  are runtime integer/boolean values, and impure lambda bodies are kept on the
  runtime path instead of being folded into static constants. Static record
  lambda methods also preserve effectful receivers and arguments when their
  final values remain static. Static lambda values returned from functions can
  be bound and called when captures and arguments are statically recoverable;
  unannotated functions that return runtime-capturing lambdas are inlined at the
  call site so their captured local slots stay alive after the function returns.
  When an enclosing lambda parameter is bound to a static constant during such
  inline lowering, an inner returned closure captures it by that static value
  rather than a frame-relative runtime slot: the parameter is immutable, and a
  runtime capture would both fail to survive the closure escaping its frame and
  shadow the correct value, so a parameter shadowing an outer `val` of the same
  name no longer makes the closure read the outer binding. Mutable captures keep
  their live runtime slot.
  Static `if` values, static binary folds, and
  static call folds use the same purity gate before native folding, and
  dynamic-control assignments invalidate static facts for runtime integer/boolean
  locals. Call-body folding is budgeted per top-level attempt: a fuel
  tank (1024 body evaluations, refilled at depth zero) bounds total
  fold time even when sibling strategies re-evaluate the same subtree,
  and a 128-level nesting cap bounds the compiler's own stack; a fold
  that exhausts either budget compiles to regular runtime code, so a
  deep recursive call like `down(200000)` neither overflows the
  compiler's stack nor hangs it. Numeric Float/Double binary expressions and string concatenation can
  still preserve mutable block-prefix effects when their operands ultimately
  yield static values.
  Static equality and `assertResult` over aggregates preserve effectful
  expected/actual expressions while comparing recovered static values, including
  compact Int-list values against generic static lists of Ints and
  side-effecting mixed numeric comparisons whose final values are recoverable.
  User-visible equality treats function and builtin-function values like the
  evaluator does: they compare false even when their native static
  representation is identical. Dynamic branch merging keeps a separate
  structural comparison for closures and builtin function values. Failing
  native `ToDo`, `assert`, `assertResult`, empty-list `head`, negative `sleep`,
  negative string-helper index/count paths, FileOutput open/write syscall
  failures, runtime `Dir#copy` source/target/copy failures, and Dir
  mkdir/delete/move syscall failures write evaluator-style
  source-located diagnostics to stderr before exiting non-zero. Failing
  `assertResult` messages reuse the dynamic print path for conditional builtin
  callable displays. Cleanup expressions preserve
  their body result while still emitting cleanup effects.
  Recoverably false `while` conditions emit their condition effects and skip the
  body, so unreachable native-unsupported constructs do not block compilation.
  Dynamic `while` loops that cannot be simulated to completion also invalidate
  static facts for locals assigned in their condition or body before later
  native folds run. Runtime-list locals assigned in those loops are copied into
  mutable selected-length storage before the loop begins, allowing assignments
  to update the visible list length within the materialized capacity.
  Static-list `foreach` (over `StaticIntList` or `StaticList`) also pre-grows
  any mutable runtime-list bindings assigned in its body by the iterable's
  length, so loops that cons each iteration's value onto a mutable list, such
  as `mutable acc: List<Int> = []; foreach(x in xs) { acc = cons(x)(acc) }`,
  fit within the materialized capacity even when the body's `if` arms differ
  in resulting length. `while` loops also pre-grow such mutable bindings by
  the predicted iteration count when the condition is `counter < N` or
  `counter <= N` over a static counter, falling back to a generous default
  cap when the bound cannot be predicted; the runtime-list assignment path
  then truncates oversized cons sources to the buffer capacity using a
  reverse-direction copy so in-place prepends preserve the prior contents.
  When the materialized list starts empty the predictor scans the body for
  `name = cons(head)(name)` and infers the slot kind (`Int`, `Boolean`, or a
  runtime string scratch) from the head expression, so an empty
  `mutable strs: List<String> = []` grows correctly when the body conses
  string values. Runtime int-to-string concatenation also resets its scratch
  buffer offset before each call so loop bodies see the fresh per-iteration
  digit rendering rather than appending to the prior iteration's trailing
  bytes.
  Static-list `map` and `foldLeft` can unroll lambdas with mutable prefix
  effects when their final result expression is still statically recoverable;
  method-style `xs.map(f)` and `xs.foldLeft(initial, reducer)` use the same path.
  When the mapper or reducer lambda's body references a dynamic captured
  local (its result is not statically recoverable for at least one element),
  static-list `map` and `foldLeft` lower each element into a runtime list
  buffer and call the lambda per element through the runtime-list helper
  path, so `map([10, 20, 30])((x) => x + n)` with a dynamic `n`,
  `map(["alpha", "beta"])((s) => s + suffix)` with a dynamic `suffix`, and
  `foldLeft([1, 2, 3])(0)((acc, x) => acc + x * n)` with a dynamic `n` work
  in native builds. The dynamic-capture detector is transitive across
  user-defined function calls, so `def addN(x) = x + n; map(xs)(addN)` and
  `foldLeft(xs)(0)((acc, x) => addN(x) + acc)` are both routed through the
  runtime path when `n` is dynamic.
  Static string/Map/Set helper calls may still fold their final helper result
  after emitting impure argument blocks, when those resulting argument values
  are statically recoverable.
  Static `join`, FileInput/FileOutput helpers, and Dir helpers use the same
  side-effect-preserving argument recovery for static paths/content/lists.
  Builtin module aliases, selective imports, and aliased helper values resolve
  to the same native helper paths, so `import Map as M`, `import Map.{size}`, and
  `val readAll = FI#readAll` work in native builds.
  Static `cons` construction and static list/map/set/record literals use that
  argument recovery rule too. When the head argument cannot be folded to a
  static value but the tail is a static list-like, native `cons` promotes the
  tail to a runtime list, prepends the runtime head, and returns a
  dynamic-length runtime list, so calls like `cons(n)([1, 2, 3])` with a
  dynamic `n` lower without folding away head-side effects.
  Static nominal and structural records,
  static map literals, and static set literals with static contents support
  construction, printing, nesting, static map/set helper calls, and equality
  through `assertResult`; records also support field selection, fixed-buffer
  runtime `String` / `List<String>`, dynamic `Int` / `Boolean`, and nested runtime
  record fields, compatible record equality and runtime string display for those fields,
  dynamic `if` merging for compatible runtime record branch results, mutable
  runtime record assignments from runtime or supported static initializers,
  annotated record function parameters/returns over the same field storage,
  runtime line-list `foldLeft` record accumulators, and
  static lambda method fields.
  Direct `head` over list literals can return runtime native values while
  preserving every list element's evaluation effects.
  Direct `tail` over list literals can return runtime line-list values from
  runtime string elements, or runtime-list values for non-string tails, on the
  same evaluated-elements path.
  Literal `contains` / `containsKey` / `containsValue` selectors can compare
  runtime native values directly without materializing a first-class runtime
  collection.
  Literal list/map/set `size` and list/map/set `isEmpty` selectors preserve
  effects and return collection cardinality or emptiness on the same path;
  set literal `size` counts only distinct runtime values.
  List literal `foreach` can unroll over runtime native values after evaluating
  all elements before the body.
  List literal `map` can unroll over runtime native values into runtime
  line-list results when every mapper result is string-compatible, or
  runtime-list results for non-string mapper outputs.
  List literal `join` can join runtime string values into a runtime string on
  the same evaluated-elements path.
  List literal display / `toString` can render supported runtime native values
  into a runtime string.
  List literal `foldLeft` can reduce those values into native scalar, string,
  line-list, runtime-list, or record accumulators.
  Map literal `Map#get` / `.get` can select runtime native values from static or
  runtime keys on the same evaluated-entry path, including runtime-list results
  whose selected length is tracked at runtime.
  Static file input/output helpers for static paths are supported
  with Linux syscalls and compile-time virtual file tracking; `FileOutput#write`
  / `FileOutput#append` can also write fixed-buffer runtime string content.
  Static-path `FileInput#open` callback bodies and callable callback values bind
  the stream path before normal native compilation, allowing them to return
  supported runtime values as well as folded static values.
  Paths whose contents become unknown through runtime writes or dynamic branches
  fall back to runtime `FileInput#all`, `FileInput#lines` / `readLines`,
  `FileOutput#exists`, `Dir#exists`, `Dir#isFile`, `Dir#isDirectory`,
  `Dir#list`, and `Dir#listFull` syscalls.
  Runtime string values can also
  be copied into NUL-terminated syscall path buffers for `FileInput#all` and
  direct file-input printing. `FileInput#open` callbacks with runtime paths bind
  the stream parameter as a runtime string, so callback bodies and callable
  callback values can return it or pass it through supported runtime string and
  file helpers such as `readAll`, `readLines`, `length`, and `cleanup`.
  Mutable runtime string and line-list bindings copy assignments into fixed
  buffers, allowing loop-carried string accumulators, line-list cursors, and
  closures that observe later assignments.
  Direct printing or immutable printable bindings of `FileInput#lines` / `readLines`
  are also supported, with `size`, `isEmpty`, `head`, `tail`,
  `cons`, inline or aliased-lambda and builtin-function-value `map` producing
  string line lists,
  String/Int/Bool/Null/Unit/List<String>-accumulator direct or method-style `foldLeft` with inline or aliased reducers, `join`,
  `split` / `join` with static or runtime string delimiters on runtime strings,
  runtime `foreach`, and
  equality / `assertResult` support
  against static string lists or other runtime line lists, plus
  `FileOutput#writeLines` write-back for runtime line lists,
  `FileOutput#write` / `append` / `writeLines` / `exists`, and
  `FileOutput#delete`, plus `Dir#mkdir` / `mkdirs` / `delete` / `copy` /
  `move` and `Dir#exists` / `isFile` / `isDirectory` / `list` / `listFull`.
  Runtime directory listings are represented as runtime line lists and sorted to
  match static/evaluator directory listing order.
  Direct
  `println(FileInput#all(path))` / `println(FileInput#readAll(path))` streams
  runtime file content without requiring the file to exist at native build time.
  Immutable runtime `FileInput#all(path)` / `readAll(path)` bindings can be
  printed or concatenated through a fixed native string buffer, compared with
  `==` / `!=` or `assertResult`, and queried with `isEmptyString` / `length`;
  method-style `toString`, `substring` / `at` with static or runtime integer
  indexes, ASCII-whitespace `trim` / `trimLeft` / `trimRight`, `repeat` with
  static or runtime integer counts,
  ASCII `toLowerCase` / `toUpperCase`,
  simple `matches` with static or runtime patterns, first-occurrence `replace`
  with static or runtime literal operands, all-occurrence `replaceAll` with
  static or runtime pattern and replacement strings, UTF-8 `reverse`,
  `startsWith`, `endsWith`, method-style `contains`, `indexOf`, and
  `lastIndexOf` are also supported. Oversized results fail with
  source-located runtime diagnostics. `FileInput#open`
  callback folding preserves mutable callback effects when final values remain
  statically recoverable. `Dir#current()` emits runtime `getcwd` and returns a
  runtime string so generated executables observe their execution cwd.
  `Dir#home()` reads runtime `HOME`, while `Dir#temp()` reads runtime `TMPDIR`
  with `/tmp` as its Linux fallback.
  `CommandLine#args()` reads the generated executable's argv at runtime,
  excludes argv[0], and exposes the result as a runtime line list for direct,
  unqualified, aliased-helper, and function-local native calls.
  `Process#exit(code)` evaluates its code argument and emits the Linux process
  exit syscall, giving generated native CLI tools explicit status codes. Static
  strings also route `substring` / `at` through the runtime slice emitter when
  the index expressions are mutable or otherwise dynamic integers, and static
  string `split` plus static string-list `join` accept runtime string
  delimiters through the same runtime string buffer path. Static
  first-occurrence `replace` can also use runtime string pattern and replacement
  operands, and static `repeat` accepts runtime integer counts.
  `StandardInput#all()` / `stdin()` read stdin into a fixed-buffer runtime
  string, and `StandardInput#lines()` / `stdinLines()` expose stdin through the
  runtime line-list representation shared with file and argv helpers. Static
  `Environment#vars()` / `env()` expose the generated executable's environment
  as `KEY=VALUE` runtime line-list entries for direct, aliased-helper, and
  generated-function native calls. `Environment#get(name)` / `getEnv(name)` and
  `Environment#exists(name)` / `hasEnv(name)` scan that same saved envp table for
  direct variable lookup and existence checks with static or runtime string keys.
  Static
  `Dir` helpers cover existence/type checks, mkdir/mkdirs, list/listFull,
  delete, copy, and move on static paths. Static `null` is available for
  immutable bindings, printing, equality, and `Map#get` misses. `()` is
  available for immutable bindings, printing, static string concatenation,
  equality, and `assertResult`. Native
  `assertResult` covers integers, booleans, static strings, static integer
  lists, static records, static maps, static sets, static nulls, and unit.
  Ordinary `==` / `!=` covers runtime integers/booleans and static aggregate
  values. `ToDo()` emits the evaluator-compatible native runtime failure text.
  Monomorphic `enum` declarations, variant constructors, and postfix
  `match` are lowered (after type checking, before codegen) onto the
  existing `__gc_*` heap primitives: an enum value is a
  `__gc_record(1 + fields)` whose slot 0 holds the boxed variant tag and
  whose remaining slots hold boxed integer / boolean payloads (booleans as
  0/1), GC heap-string payloads, or directly-stored nested-enum pointers,
  so the collector traces every slot as a pointer. String payloads merge
  through `compile_if`'s heap-string branch path (both arms are coerced to
  heap strings into a rooted slot) and compare against string literals via
  heap-string equality coercion.
  `match` becomes a chain of `if` tests with short-circuited tag checks
  guarding payload reads, supporting nested constructor patterns,
  integer/string literal and variable/wildcard patterns, and guards;
  literal/variable `match` over non-enum scrutinees lowers the same way,
  and a fully non-matching scrutinee aborts through `__match_fail()`.
  Enum **values** also display like the evaluator: `println` / `toString`
  / string interpolation of an enum format it as `Variant(field, ...)`
  (strings unquoted, nullary variants bare, nested enums recursing) by
  emitting a shape-driven formatting routine — monomorphic shapes are
  carried from the lowering pass and generic shapes from the
  `ConcreteEnumShape` side-table — rather than printing the raw heap
  pointer.
  Enums are real nominal types in `klassic-types` now: a declaration
  registers an `EnumSchema` (type parameters + per-variant field types)
  and declares each constructor as a polymorphic function into
  `Type::Enum(name, args)` — quantified positions are fresh inference
  variables, not `Type::Generic` (which unification treats as
  dynamic-like and would let payload constraints evaporate). `match`
  pins the scrutinee to the pattern's enum at fresh type arguments and
  types payload bindings at the instantiated field types, so
  `takeInt(Full("oops"))` against `Box<Int>` is a call-site type error
  and `case Full(v) => v + 1` knows `v : Int`. Annotations reach the
  checker as `Named` / applied `Named` and normalize to `Type::Enum`
  inside `resolve`. Every type walker (free-variable collection,
  instantiation, generic substitution, var renumbering, occurs check)
  descends through enum type arguments — missing any one of them
  silently desynchronizes quantification from instantiation. Native
  codegen's shape compatibility check remains as a backstop.
  Generic enums with scalar payloads compile by per-instantiation
  specialization at codegen time: a generic `enum` declaration is
  registered (variant tags + each field classified as a fixed repr or a
  type parameter), a construction projects each argument's `NativeValue`
  to a field repr and records the instance's concrete shape on the bound
  slot, and a `match` reads payloads through that shape — reusing the same
  `en_build_construct` / `en_build_match` lowering as monomorphic enums,
  so the heap layout is byte-identical. String and bool payloads are
  carried the same way: the constructor argument's `NativeValue` resolves
  the type parameter to a string / bool repr, the recorded shape rides
  through `val` bindings (and into function parameters) so the matcher
  reads the payload with the right boxing. A value constructed only from a
  nullary variant (`None`) cannot fix its type parameter, so matching it
  with a non-integer result reports a clean type error rather than
  miscompiling. Nested generic enums (`Some(Some(7))`) also compile: the
  per-instance shape is recursive, so each `EnumField` payload records the
  shape of the enum it points to, the outer construction captures the
  inner value's shape, and a nested constructor pattern descends into it
  with the inner instantiation's reprs (verified at two and three levels).
  Because the shape rides on the value rather than the declaration, one
  program may instantiate the same generic enum at several types (and use
  several generic enums) without interference — each inlined call site
  resolves its own reprs. Generic enums whose payload is an applied
  generic (`List<a>` / a record)
  remain unsupported and keep their compile-time diagnostics.
  Functions whose parameter or return annotations name a lowered
  monomorphic enum use a per-frame by-pointer calling convention: the
  enum's `__gc_record` pointer travels as a qword in the System V
  argument registers (or the caller stack-push convention past six)
  alongside scalars, the caller pins each evaluated heap argument in
  the GC root table while later arguments evaluate (unpinning just
  before the call), and the callee prologue spills pointer arguments
  to frame slots that it registers on the GC shadow stack — after all
  argument registers are spilled, because the shadow push clobbers
  rcx/r8/r9 — and pops again before returning. That makes recursive
  functions over monomorphic enum arguments, and enum-returning
  recursion, compile and run.
  Annotated `String` parameters and returns ride the same by-pointer
  convention as GC heap strings (`[i64 byte_len][bytes]`), retiring
  the fixed 64KB scratch-buffer convention at function boundaries:
  every recursion frame owns its own string, and strings larger than
  64KB cross calls. Static and fixed-buffer arguments are lifted onto
  the heap at the call site; consumers inside bodies either handle
  heap strings directly (`length` counts UTF-8 characters in place,
  `isEmptyString` reads the length prefix, `assertResult` and `==`
  compare through the heap-string equality, and the slicing emitters
  behind `substring` / `at` read a heap input in place from a rooted
  slot — only the slice *result* still lands in a fixed buffer, so a
  result over 64KB is the remaining cap) or materialize into a fresh
  fixed buffer transitionally (`String#parseInt`, where an input past
  64KB cannot be a number anyway). The
  caller unpins heap-string arguments exactly like enum pointers —
  the pin/unpin pair must stay matched per argument or the root
  table fills after a few thousand calls — and the GC tables (root
  table, shadow stack, mark worklist) are sized so deep recursion
  with per-frame heap values runs thousands of frames before the
  clean overflow abort. The tables themselves come from one anonymous
  mmap at startup (mmap memory is zero-filled, their required initial
  state); only a base-pointer qword per table lives in .data, so the
  ~824KB of table zeros never lands in the emitted binary — a trivial
  executable is back to a few tens of KB.
  Annotations naming a *fully-applied* generic enum (`Option<Int>`,
  `Option<String>`, `Option<Option<Int>>`) ride the same ABI: the
  annotation text alone fixes every type parameter, so the parameter's
  concrete shape is derived at predeclaration time and bound to the
  spilled slot in the prologue — no call-site shape tracking, hence
  recursion works. A return annotation of that form publishes the
  shape at each call site, so matching directly on a call's result
  works too, and a generic `match` whose scrutinee is any expression
  that published a shape (a fresh construction or such a call) now
  binds it to a scoped temp instead of demanding an identifier. The
  caller checks a tracked argument shape against the parameter's
  annotation-derived shape where both are resolved and rejects
  mismatches (`Some("x")` into `Option<Int>`) at compile time.
  Generic-enum constructions also evaluate their argument temps in a
  dedicated scope now: the slots used to outlive the construct, and
  an enclosing expression with operands spilled on the machine stack
  (a binary op's lhs) popped a temp instead of its operand once the
  construct appeared as a by-pointer call argument. Self-referential
  generic enums (`enum MyList<a> { case Cons(h: a, t: MyList<a>);
  case Nil }`) register too: an applied-generic field annotation
  classifies as a heap-pointer field whose nested shape is not tracked
  through the field — a deep pattern into it is diagnosed — because
  recursive traversals re-attach the concrete shape through the
  function parameter's annotation on every frame. The annotation
  scanner in `klassic-syntax` tracks angle-bracket depth alongside
  parentheses, so multi-parameter applied annotations
  (`Result<Int, String>`, `Map<String, Int>`) parse — a comma inside
  `<...>` previously terminated the annotation.
  A pattern variable binding an enum-typed payload whose nested shape
  is tracked (`case Some(inner) => inner match { ... }`) wraps its
  field read in a synthesized `__enum_shape_hint(value, id)` call;
  codegen compiles the value and publishes the indexed shape as
  pending, so the binding's slot records the payload's shape and a
  later `match` on the bound name resolves. Enum String payloads (GC
  heap strings) materialize into the fixed runtime-string buffer when
  passed to a String-annotated parameter or returned through a String
  return annotation, instead of rejecting the call. Recursion over
  generic parameters *without* a concrete annotation (true
  per-call-site monomorphization) remains diagnosed.
  Imports of the plain-Klassic `std.*` modules are inlined before type
  checking: the imported module's declarations are spliced between the
  bundled prelude and the user program (native name resolution is
  order-sensitive, and the modules call prelude helpers), the import node
  is dropped, and lazy codegen only emits the functions actually called.
  Because every module's declarations share one flat translation unit,
  each inlined module's free `def`s are name-mangled to
  `__mod_<path>_<name>` and references are rewritten scope-aware (local
  bindings shadow, so they are never mangled), so same-named but
  differently-typed free functions across modules — user or `std.*` —
  no longer collide (#449). Selective and bulk imports resolve to the
  mangled name; aliased imports (`import std.x as M`) collapse their
  `M.func` access to the target module's mangled name. A selective
  `import a.{x}` of a name the module does not export is rejected before
  splicing with the same `module `a` has no member `x`` diagnostic the
  evaluator gives — the export set mirrors the evaluator's root value
  bindings (free defs, top-level `val`s, enum constructors), so the unknown
  member is no longer silently dropped into an accepted build. The ADT-backed
  modules (`std.option` / `std.result`)
  now inline as well, on top of the generic-enum specialization above:
  their constructors (`some` / `ok`), consumers (`getOrElse` / `unwrapOr`
  / `isSome` / `isErr`) and method-style extensions compile to native.
  Helpers that *return* a freshly constructed generic enum through a
  `match` (`map` / `flatMap` / `orElse`) work too:
  a generic-enum shape produced inside a branch is captured per branch and
  the `if` lowering publishes the merged shape of its two branches, so the
  joined value carries it to its binding. The merge prefers a resolved
  field repr over a defaulted one (a defaulted field is only ever read in a
  tag-guarded dead arm, so trusting the resolved branch is sound), letting
  a string-payload `map` read back as a string rather than the
  `None` arm's defaulted scalar. Import discovery is transitive: a module
  imported only from inside another inlined module (`std.result` imports
  `std.option`) is found by a worklist over the parsed modules and spliced
  too, and the spliced modules are ordered so each module's imports precede
  it — a dependency's bare `val` (such as `std.option`'s `none`) must be
  declared before the dependent references it. A chained enum extension
  method that converts to a different enum (`Result.toOption` yields an
  `Option`) is dispatched against its own return enum rather than the
  receiver's: the extension function's body is read for the enum it
  produces, resolving constructor-alias helpers (`some` / `none` / `ok` /
  `err`) through a one-pass `def` / `val` -> enum map, so a following
  `getOrElse` routes to the right type.

  A portable C backend (roadmap PR 9 / PR 10) lives behind
  `--backend c`: `klassic --backend c build program.kl -o program.c`
  emits one C99 translation unit for a growing subset — `Int` /
  `Double` / `Bool` / `String` values, arithmetic / comparison /
  logical operators, string concatenation / equality / `length` /
  `substring` / `at` / `toString`, `val` / `mutable` / assignment,
  `if` / `while`, `println`, and annotated top-level `def`s including
  recursion. Strings are `KStr` values served by the `klassic_rt_*`
  C-ABI shims in `klassic-runtime` (built as `libklassic_runtime.a`
  and bundled with releases); the shims are written against Rust
  `str` exactly like the evaluator's builtins, so semantics match by
  construction, and the root-stack ABI is already reserved for the
  future collector. An output name not ending in `.c` makes the CLI
  find the system C compiler and link the runtime automatically — the
  macOS native-binary path (no Mach-O direct emission, per the
  roadmap). Anything outside the subset is a source-located
  diagnostic, never wrong code, and the direct ELF path is untouched.
  The emitter is `crates/klassic-native/src/cbackend.rs`. On hosts
  with no direct native backend (macOS today), a target-less
  `klassic build` routes through this C path automatically — the
  v0.2.0 behavior of silently cross-emitting a Linux ELF the host
  could not execute was a bug; an explicit `--target` still selects a
  cross build.

  A direct AArch64 backend for `aarch64-apple-darwin` lives in
  `crates/klassic-native/src/aarch64.rs` (A64 instruction encoding and
  codegen) and `crates/klassic-native/src/macho.rs` (the Mach-O arm64
  executable writer). Like the ELF path it needs no external
  toolchain: the generated code talks to the kernel via `svc #0x80`
  (Darwin syscall numbers in `x16`), and the writer embeds the ad-hoc
  code signature — a SHA-256 CodeDirectory computed in-process — that
  the Apple Silicon kernel demands before executing any arm64
  binary. Code is
  position-independent (`adrp`+`add` data addressing) because the
  kernel slides `MH_PIE` images. One Darwin reality check learned on
  first contact: the kernel rejects fully static `LC_UNIXTHREAD`
  arm64 mains with EBADEXEC, so the image is dyld-linked
  (`MH_DYLDLINK` + `LC_LOAD_DYLINKER` + `LC_MAIN`, plus the empty
  `LC_SYMTAB`/`LC_DYSYMTAB` pair dyld dereferences) even though the
  generated code imports nothing. The backend starts from the same
  kind of small vertical slice the x86_64 backend grew from. The
  current subset: Int/Bool/String expressions (arithmetic with an
  evaluator-matching division-by-zero abort, comparisons,
  short-circuit logic, heap string concatenation off an x19/x20
  bump allocator the kernel feeds via mmap), `val`/`mutable` locals
  addressed off a frame pointer, `if`/`while`, annotated `def`s as
  real AAPCS64 functions with per-frame recursion, the string
  builtins (length/substring/at/toString/isEmptyString with
  evaluator clamping semantics), monomorphic enums and `match`
  (riding the shared `desugar_enums` lowering — the emitter just
  implements its `__gc_*` primitive surface), cons lists (curried
  `cons`, head/tail/isEmpty/size, list-typed recursion the x86_64
  backend still rejects), records (nominal and structural, nested
  field access), and evaluator-format `println` for all of the
  above. Unsupported constructs fail with source-located
  diagnostics. `--target
  aarch64-apple-darwin` selects it from any host (cross builds work).
  A target-less `build` compiles for the detected host — Apple
  Silicon defaults to this backend and falls back to the portable C
  backend for programs outside the direct subset; Linux x86_64 keeps
  the full-featured direct ELF backend with no fallback.

  M11 (issue #538) adds `Cond::Cc` (carry clear), the AArch64
  condition Darwin's `svc #0x80` sets on syscall success — the mirror
  image of Linux's negative-rax convention. `Dir#exists` /
  `FileOutput#exists` are its first user: `access(path, F_OK)`
  followed by `cset x0, cc` turns the syscall's carry flag directly
  into the `Bool` result, no branch needed. `Cond::Cs` (carry set /
  failure) and an abort-on-failure helper are still deferred to
  whichever later milestone first needs to bail out of a failed
  syscall (M14 file I/O, M15 dir ops), to avoid dead-code churn.

  M12 (issue #538) desugars `#{...}` string interpolation: `Expr::
  StringInterpolation`'s parts fold left to right through the
  existing `emit_str_concat`, converting each hole's value to `Str`
  first via `emit_int_to_str`/`emit_bool_to_str` (a hole that's
  already a `Str` needs no conversion). Aarch64 strings are exact-size
  heap objects, so unlike the x86_64 backend's fixed-buffer
  interpolation path there's no capacity to track — every
  intermediate concat result is a fresh, correctly-sized allocation.
  A nested interpolation or an enum/record/list hole is deferred with
  a source-located diagnostic rather than guessed at.

  M13 (issue #538, first slice) adds `toUpperCase`/`toLowerCase`
  (ASCII-only case shift, matching the x86_64 backend's accepted
  precedent -- no full Unicode case tables), UTF-8-aware `reverse`
  (scans backward to find each character's start byte, top bits != 2
  once shifted right 6, then copies that character's bytes forward
  into the output at the current write cursor -- characters are
  discovered in reverse order, so writing them in discovery order
  reverses the string), `startsWith`/`endsWith` (length-gated byte
  comparison), and ASCII-only `trim`/`trimLeft`/`trimRight`. All of
  these allocate a fresh, exact-size heap string via `emit_alloc`
  rather than writing into a fixed-capacity scratch buffer, so there's
  no capacity limit to enforce. Two new `Assembler` primitives back
  this: `ldrb_reg_offset`/`strb_reg_offset` (register-offset byte
  load/store, the same size-field flip from `ldr_reg_offset` that
  `ldrb_post_increment` already applies to `ldr_post_increment`) and
  `is_ascii_whitespace_into` (tests a byte against the same
  space/tab/LF/CR/VT/FF set as the x86_64 backend's
  `emit_jump_if_ascii_whitespace`). One bug caught by macOS CI's own
  execution test: an early version of `trim` computed the result
  slice's start pointer into a scratch register *before* calling
  `emit_alloc`, whose heap-grow path only preserves `x0`-`x5` across
  the mmap it performs -- once a program trimmed enough strings to
  fill the bump allocator's 64 MiB segment and a grow actually fired,
  that pointer was silently clobbered (`SIGSEGV`). Fixed by saving the
  value across the call via push/pop and deferring the pointer
  computation until after `emit_alloc` returns; a regression test
  sized to force an actual heap-grow (100,000 `trim` calls on a
  1000-byte string) guards against a repeat.

  M13 slice 2 (issue #538) adds `join`: two passes over the cons-list
  (walk once to total element byte lengths and count them, so the
  separator's contribution is known before allocating; walk again,
  copying each element into a single exact-size allocation with the
  separator interspersed between elements). This is the first M13
  routine to walk a cons-list rather than operate on a single string.
  Remaining M13 slice: `replaceAll`/`split`, which need pattern
  matching plus a result whose length isn't known until the match
  count is, and are left for a follow-up PR once a value-spilling
  scheme is designed -- both routines need roughly 7 fixed values
  (byte-base pointers and lengths for input/pattern/replacement, plus
  a running match count and total result length) live simultaneously
  across the copy loop, more than the register file holds without a
  deliberate stack-spill discipline, and forcing it under time
  pressure risks a repeat of (or worse than) the M13-slice-1 bug below.

  M14 (issue #538) adds file I/O: `FileOutput#write`/`#append`
  (opens with `O_WRONLY|O_CREAT|` `O_TRUNC` or `O_APPEND`, writes the
  content bytes, closes), `FileInput#all` (opens `O_RDONLY`, reads up
  to a 1 MiB cap into a scratch heap buffer, closes, then allocates a
  second exact-size string object and copies just the bytes actually
  read), and `FileOutput#delete` (unlinks, tolerating `ENOENT` as
  success like the evaluator's `std::io::ErrorKind::NotFound`
  leniency). Introduces `emit_abort_if_syscall_failed`, the
  abort-on-failure counterpart to M11's `cset_syscall_succeeded` that
  M11 deferred until a caller needed it. Darwin syscall numbers/flags
  differ from Linux's (`open`=5, `read`=3, `close`=6, `unlink`=10;
  `O_WRONLY`=1, `O_APPEND`=0x8, `O_CREAT`=0x200, `O_TRUNC`=0x400) --
  reusing Linux's values would silently corrupt file-open semantics.
  Every value that must survive an `emit_alloc` call across these
  routines is pushed/popped explicitly rather than kept resident in a
  register, since `emit_alloc` unconditionally clobbers `x6` (even on
  its fast, no-heap-grow path) and only preserves `x0`-`x5` across its
  slower heap-grow path -- the exact lesson M13 slice 1's `trim` bug
  taught. Remaining plan after M14: M15 (directory ops), M16
  (environment/time), plus `replaceAll`/`split` above.

  M15 (issue #538) adds directory ops: `Dir#mkdir`/`#mkdirs`
  (`mkdir`=136, mode `0o755`), `Dir#delete` (`rmdir`=137),
  `Dir#isDirectory` (`fstatat64`=470 with `AT_FDCWD`=-2, testing
  `stat64.st_mode`'s file-type nibble against `S_IFDIR` -- Darwin
  places `st_mode` as a 2-byte halfword at offset 4, not 8 bytes at
  offset 24 like Linux), and `Dir#move` (`rename`=128). `Dir#mkdirs`
  cannot mutate the path's `/` bytes at runtime to walk prefixes the
  way the x86_64 backend does, because aarch64's rodata lives in the
  Mach-O `__TEXT` segment (read-only, unlike ELF's writable `.data`);
  since the path is already constrained to a compile-time literal,
  the backend instead computes every `/`-separated prefix in Rust and
  emits one `mkdir` (tolerating `EEXIST`, errno 17) per prefix. An
  empty path emits no prefixes at all (a no-op), matching the
  evaluator's `fs::create_dir_all("")` (which succeeds) rather than
  `mkdir("")`'s own `ENOENT` failure.
  `Dir#isDirectory` reads its 144-byte stat buffer from a fresh
  bump-heap allocation -- safe today because this backend has no
  collector yet to mistake the raw stat bytes for a pointer -- and
  reports `false` rather than aborting on a failed stat, matching the
  evaluator's `Path::is_dir()`. Introduces `ldrh_imm` (halfword load)
  to the assembler.

  M16 (issue #538) adds environment/time: `Environment#exists` and
  `Time#nowMillis`, plus a program-prologue change both depend on.
  dyld calls the Mach-O `LC_MAIN` entry as a plain AAPCS64 call --
  `x0`=argc, `x1`=argv, `x2`=envp, `x3`=apple -- so `emit_macho_program`
  now copies `x0`-`x2` into three newly reserved callee-saved
  registers (`x21`-`x23`) before anything else can clobber them,
  exactly the way `x19`/`x20` already carry the bump allocator's state
  across the whole program without ever being spilled.
  `Environment#exists` walks the NUL-terminated `envp` array pointed
  to by `x23` (each entry a `"KEY=VALUE"` C string) comparing a
  compile-time literal key's bytes against each entry's prefix,
  requiring the byte right after the match to be `'='` so `"FOO"`
  can't false-match an entry for `"FOOBAR"`. `Time#nowMillis` calls
  `gettimeofday` (syscall 116) into a 16-byte scratch buffer and
  computes `tv_sec*1000 + tv_usec/1000`; `syscalls.master` marks
  `gettimeofday` `NO_SYSCALL_STUB` (normally served from the commpage
  on real hardware for speed), so this is the one M16 syscall whose
  raw two-argument `svc #0x80` form wasn't verified against a written
  spec before landing -- the macOS CI execution test is what actually
  proves it works. Reuses the existing `mul_reg`/`sdiv_reg` for the
  millisecond conversion. `tv_usec` is Darwin's real 4-byte
  `__int32_t` field (followed by 4 bytes of padding to round the
  struct to 16 bytes) -- introduces `ldr_imm32` (32-bit load,
  zero-extending) to read it, rather than the existing 64-bit
  `ldr_imm`, so the result never depends on those padding bytes
  happening to be zero (an independent review flagged the original
  64-bit read as a correct-today-but-fragile assumption before this
  landed).

  `x86_64-pc-windows-msvc` (`--target x86_64-pc-windows-msvc` /
  `windows-x86_64`) is the one target that does *not* get its own
  codegen module: it reuses the entire `DirectX86_64` (Linux) code
  generator as-is, since the generated x86_64 machine code is already
  Win64-ABI-compatible; only the OS boundary and the container format
  differ. `NativeCodeGenerator` carries an `is_windows` flag that gates
  every syscall-emission site (`write`, `mmap`, `exit`, `sleep`,
  `stopwatch`, `Time#nowMillis`) to call one of a small set of Win64
  import-call shims (`__win_write`, `__win_mmap`, `__win_exit`,
  `__win_sleep`, `__win_time_now_millis`, `__win_qpc`, `__win_qpf`,
  plus a `__win_init` startup routine that caches `GetStdHandle`
  results) instead of a bare Linux `syscall` instruction, and skips
  the two pieces of Linux-initial-stack-layout
  setup (`argc`/`argv`/`envp`, the `getrlimit`-based stack-overflow
  probe) that have no Windows equivalent at this raw an entry point.
  `TargetPlatform::syscall_number` panics unconditionally for a
  Windows target as a compile-time tripwire, so any not-yet-gated
  syscall path fails loudly instead of silently emitting a Linux
  syscall number into a Windows binary. Each shim forces 16-byte `rsp`
  alignment via `and rsp, -16` before its own `call [rip+iat]`
  regardless of how it was entered — the reused Linux codegen was
  never written to maintain the Win64 "aligned before every call"
  invariant, so a shim reached from deep inside a closure/GC-allocation
  call chain can see `rsp` at either parity, and assuming a fixed
  incoming alignment reliably crashed inside `KERNELBASE.dll` on first
  contact. The PE64 writer is `crates/klassic-native/src/pe.rs`: a
  minimal three-section (`.text`/`.rdata`/`.data`) unsigned image
  importing every `kernel32.dll` function named in `ImportSymbol::ALL`
  (`GetStdHandle`, `WriteFile`, `ExitProcess`, `VirtualAlloc`,
  `GetSystemTimeAsFileTime`, `Sleep`, `QueryPerformanceCounter`,
  `QueryPerformanceFrequency` as of this writing), with a fixed
  `ImageBase` and no `.reloc` section (mirrors the "no external
  toolchain" property of the ELF and Mach-O writers). The ILT/IAT/
  hint-name table layout is derived entirely from `ImportSymbol::ALL`
  (order and count), and `ImportSymbol::iat_index` derives each
  symbol's IAT slot from its position in `ALL` rather than a
  hand-maintained match arm, so adding a new imported function is a
  three-line change (a variant, an `ALL` entry, a `name()` arm) that
  never touches `pe.rs`. Verified end-to-end under WSL2 binfmt
  interop (`./program.exe` launches the real Windows loader from
  Linux) against hello-world, recursive `def`s, string/enum/record/list
  operations, closures, and a GC-churn stress program, all matching
  the evaluator's output byte-for-byte.

  Every native-codegen path that could otherwise reach the
  `syscall_number` tripwire on Windows now has its own Win64 shim, so
  the tripwire is unreachable in practice; the `unsupported_on_target`
  diagnostic helper that used to raise a `` `Feature` is not yet
  supported when targeting x86_64-pc-windows-msvc `` error for
  not-yet-implemented builtins was removed once the last gate was
  replaced (it is referenced only in a comment now). `sleep`,
  `stopwatch`, and `Time#nowMillis` were the first to gain real
  codegen: `compile_sleep` stages the millisecond count into `rdi` and
  calls the `emit_win_sleep_runtime` shim, which moves it into `ecx`
  and calls `Sleep` directly (no seconds/nanos split needed, unlike
  Linux's `nanosleep`); `compile_time_now_millis` calls
  `emit_win_time_now_millis_runtime`, which wraps
  `GetSystemTimeAsFileTime` and converts its 100ns-tick FILETIME output
  to unix epoch millis via the fixed `116_444_736_000_000_000`
  1601/1970 epoch offset; and `compile_stopwatch` calls
  `emit_win_qpc_runtime` (wrapping `QueryPerformanceCounter`) before
  and after the timed body, then `emit_win_elapsed_millis_qpc` calls
  `emit_win_qpf_runtime` (wrapping `QueryPerformanceFrequency`) and
  computes `elapsed_ticks * 1000 / frequency`. All three shims follow
  the same save-every-register/self-aligning-`rsp` pattern as the
  original four Win64 shims (`__win_write`/`__win_mmap`/`__win_exit`/
  `__win_init`).

  The rest of the `Dir#` family (`#mkdir`/`#mkdirs`/`#delete`/`#move`/
  `#copy`/`#current`/`#exists`/`#isDirectory`/`#isFile`/`#temp`/
  `#list`/`#listFull`/`#home`) is backed by Win64 shims built on the
  same save-registers/align-stack/epilogue scaffold as `__win_write`:
  `__win_mkdir`/`__win_rmdir`/`__win_move`/`__win_copy_file` wrap
  `CreateDirectoryA`/`RemoveDirectoryA`/`MoveFileExA`/`CopyFileA`
  (the last replacing the Linux open/`sendfile`-loop/close sequence
  with one call) and share a `GetLastError()`-translating BOOL-result
  normalizer that maps `ERROR_FILE_NOT_FOUND`/`ERROR_PATH_NOT_FOUND`
  to `-errno_no_entry()` and `ERROR_ALREADY_EXISTS`/`ERROR_FILE_EXISTS`
  to `-errno_exists()` (both real Windows sentinels, `-2`/`-17`,
  rather than the placeholder `0` every other
  `WINDOWS_X86_64_PLATFORM_CONSTANTS` field carries) so the reused
  Linux `emit_runtime_error_if_rax_negative[_except_errno]` call sites
  need no changes; `__win_getcwd` wraps `GetCurrentDirectoryA` and
  absorbs its "length excludes NUL" ABI delta with a `+1` so
  `Dir#current`'s existing `dec_reg(Rax)` stays untouched;
  `__win_get_temp_path` wraps `GetTempPathA` and strips the one
  trailing `\` it always appends, for parity with the no-trailing-
  separator `Dir#temp`/`"/tmp"` convention; and `__win_get_file_attributes`
  wraps `GetFileAttributesA`, explicitly detecting the
  `INVALID_FILE_ATTRIBUTES` (`0xFFFFFFFF`) sentinel via a 64-bit
  register compare rather than a sign-extending immediate compare
  (the zero-extended DWORD would otherwise read as a large *positive*
  value and silently defeat every `cmp rax,0;jge ok`-style caller),
  backing `Dir#exists`/`#isDirectory`/`#isFile`'s shared
  `emit_runtime_path_exists_label`/`emit_runtime_path_type_check_label`
  helpers. `Dir#list`/`#listFull` use a `FindFirstFileA`/
  `FindNextFileA`/`FindClose` loop that mirrors the Linux `getdents64`
  output contract (newline buffer, dot-entry skip, `listFull` prefix,
  shared sort pass), with the same 65536-byte listing cap as Linux.
  `StandardInput#all`/`#lines` read through a `ReadFile` shim against a
  stdin handle cached at startup. Two related, non-codegen fixes ship
  alongside these shims: `emit_dir_mkdirs_label`'s runtime path-prefix
  scanner gained a parallel `\`-handling block (Windows paths may use
  either separator) that restores the original separator byte rather
  than hardcoding `/`; and `compile_dir_exists`/`_is_directory`/
  `_is_file` force the runtime (non-folded) path check whenever
  `self.is_windows`, since their existing compile-time constant fold
  for a statically-known path argument queries the *compiling host's*
  filesystem — correct for a same-target build, silently wrong for a
  Windows cross-build.

  `Environment#vars`/`#get`/`#exists` and `CommandLine#args` never
  reached a raw syscall in the first place — they walk the
  `environment_base`/`command_line_argc`/`command_line_argv1_base`
  slots that `emit_store_command_line_state` used to leave zeroed on
  Windows, so an ungated build would have silently compiled a binary
  that always observed an empty environment/argument list rather than
  failing to build; they were gated for that "wrong answer, not a
  crash" reason rather than the syscall-tripwire reason.
  `emit_store_command_line_state`'s Windows branch now synthesizes
  that layout at startup instead: `GetEnvironmentStringsA`'s flat
  block is walked once into a synthesized `char*` pointer array
  (`environment_base`, never freed — its entries point directly into
  the still-live `GetEnvironmentStringsA` block, mirroring the GC
  heap's own "never free" policy), and `GetCommandLineA`'s single ANSI
  string is hand-tokenized by a two-state (in-quotes / not) FSA into a
  NUL-separated buffer plus a matching pointer array
  (`command_line_argc`/`command_line_argv1_base`). The tokenizer
  deliberately does not implement `CommandLineToArgvW`'s
  `""`-unescaping or backslash-escaping rules — backslash is always
  literal — so a quoted Windows path ending in a backslash before its
  closing quote does not tokenize identically to the real CRT. Both
  slots keep the exact shape every existing
  `emit_command_line_args`/`emit_environment_*`/
  `emit_find_environment_*` consumer already assumed, so
  `CommandLine#args`, `Environment#vars`/`#get`/`#exists`, and (via a
  `USERPROFILE`-instead-of-`HOME` key swap) `Dir#home` needed no
  further changes to those consumers. Single-key `Environment#get`/
  `#exists` lookups additionally go through a `GetEnvironmentVariableA`
  shim so the OS resolves the name — Windows stores variables with
  OS-native casing (`Path=`, not `PATH=`) and resolves them
  case-insensitively, which a byte-for-byte envp-array walk does not;
  `Environment#vars` keeps the block walk since it only enumerates.

  `FileOutput#write`/`#append`/`#writeLines`/`#exists`/`#delete` and
  `FileInput#all`/`#readAll`/`#lines`/`#readLines` (`std.file`) branch
  their `Open`/`Read`/`Write`/`Close`/`Unlink`/`Access` syscall sites
  to Win64 shims: `win_create_file` (wraps `CreateFileA`, selecting
  `GENERIC_READ`+`OPEN_EXISTING` / `GENERIC_WRITE`+`CREATE_ALWAYS` /
  `FILE_APPEND_DATA`+`OPEN_ALWAYS` from a small selector carried in
  the target's `open_read_flags`/`open_write_flags` constants),
  `win_read` and `win_write_handle` (twins of `win_write` wrapping
  `ReadFile`/`WriteFile` against a raw file `HANDLE` rather than
  `win_write`'s cached-stdout/stderr selector), `win_close_handle`
  (wraps `CloseHandle`), `win_delete_file` (wraps `DeleteFileA`,
  translating a `GetLastError()` failure via
  `emit_win_translate_last_error_to_errno` so
  `ERROR_FILE_NOT_FOUND`/`ERROR_PATH_NOT_FOUND` map to the same
  `errno_no_entry` sentinel the Linux `Unlink` path already tolerates),
  and `win_get_file_attributes` (wraps `GetFileAttributesA`; its
  `INVALID_FILE_ATTRIBUTES` sentinel zero-extends to a *positive*
  64-bit value, so the shim compares against the exact 64-bit constant
  rather than a sign-extended `-1`). Two `compile_*` entry points
  (`FileOutput#exists`, `FileInput#all`/`#lines`) also fold a
  statically-known-constant path's existence/content from the
  *compiling host's* filesystem at compile time as an optimization;
  that fold is forced off whenever `self.is_windows`, since a Windows
  build's path only makes sense to check against the eventual Windows
  target's filesystem at runtime, not the Linux/macOS build machine's.
  `emit_print_expr_fragment`'s `println(FileInput#all(...))`/
  `println(FileInput#lines(...))`/
  `println(FileInput#open(path, s => s.readLines()))` print-fusion
  fast paths stream a file straight to the target fd without going
  through `compile_file_input_all`/`compile_file_input_lines`; each
  fast path's own `!self.is_windows` guard still skips it on Windows
  (the fast paths themselves still call the raw-syscall-only
  `emit_file_stream_to_fd_path_label`), falling through to the
  ordinary (Windows-safe) call dispatch instead — so those `println`
  calls still work on Windows, just through the general path rather
  than the fused fast path.

  All of the shims above use only the kernel32 "A" (ANSI) functions,
  reusing the existing UTF-8 path-staging code as-is, so non-ASCII
  paths and environment values/keys are unsupported (best-effort /
  undefined) on this target — the one documented Windows limitation.
  `thread { ... }` needs no gate of its own: a queued thread body is
  compiled into the tail of `main` via the same `compile_expr`
  dispatch as any other call site (no real OS thread, no syscall of
  its own). `Process#run`/`#nrun` (process spawning) has no
  native-backend implementation on any target yet, so there is
  nothing Windows-specific to gate there.

  The native runtime owns a dedicated GC heap that is separate from the
  static `.data` buffers used by the rest of the codegen. At program
  startup, the prologue invokes `mmap(NULL, 1 MiB,
  PROT_READ|PROT_WRITE, MAP_ANON|MAP_PRIVATE, -1, 0)` and stores the
  returned base/top/end pointers in dedicated globals. Three emitted
  subroutines back the heap:
  - `gc_alloc(size, type_tag)` aligns the request up to a 16-byte
    boundary, walks the singly-linked free list using a first-fit
    policy, and falls back to a bump pointer when no free block fits.
    Each block carries a 16-byte header — word 0 is the total block
    size with the GC mark bit reused as the top bit, word 1 is the
    type tag (0 marks a free block). A free-list hit zeroes the
    reused block's entire payload before returning it: first-fit can
    hand out a block larger than the request without splitting it,
    and because the mark trace walks header-size qwords, stale bytes
    from the block's previous life would otherwise be chased as heap
    pointers (the bump path skips the memset — fresh mmap memory is
    already zero). Callers must not assume `rdi` still holds the
    requested size after the call; `gc_alloc` rewrites it to the
    aligned total, which previously made `__gc_record`'s local
    payload memset overrun into the next block's header and corrupt
    the heap after the first collection.
  - `gc_collect()` performs stop-the-world mark-and-sweep. The mark
    phase walks compile-time-registered roots; the sweep phase walks
    every active segment linearly using each header's size, clears
    mark bits on survivors, and threads dead blocks onto a freshly
    rebuilt free list. If `gc_alloc` cannot satisfy a request even
    after a collection plus a heap growth it writes
    `klassic gc: out of memory` to stderr and exits with status 1.
  - `gc_grow_heap(requested_total)` is invoked by `gc_alloc` whenever
    even a post-collection retry cannot satisfy the bump path. It
    `mmap`s a fresh segment of at least 1 MiB (page-aligned, larger
    when the request itself exceeds 1 MiB), appends `{base, top, end}`
    to a fixed 64-entry segments table, repoints the active
    `gc_heap_*` globals at the new region, and freezes the previous
    segment's bump pointer so the next sweep covers it. Past 64
    segments the runtime exits with `klassic gc: heap segment limit
    reached`. Tracing handles type tag 2 (`pointer record`) and tag 3
    (`pointer array`) identically; both walk the entire payload as a
    packed array of heap pointers.

  The allocator and collector are tagged into the language through
  a `NativeValue::HeapPointer` variant. `__gc_alloc(size)` and
  `__gc_record(num_fields)` return that variant rather than a plain
  `Int`, so a `val a = __gc_alloc(...)` binding tells the codegen
  the slot it just emitted holds a tracked GC pointer. The
  `allocate_slot` helper then zero-initializes the slot and pushes
  its rbp-relative address onto a precise shadow stack — an 8192-
  entry data buffer with its own top-of-stack global. Each scope
  remembers how many entries it pushed so `pop_scope` can drop
  exactly that many entries on its way out, and
  `pop_scope_preserving_allocations` transfers the count up to the
  parent so entries belonging to slots whose stack memory survives
  are never lost. The GC mark phase walks the shadow stack as a
  second pass after the static root table, dereferencing each entry
  to read the slot's current heap pointer and feeding it into the
  shared `gc_mark_visit` worklist. Identifier loads, val/mutable
  bindings, function-argument binding sites, assertions, binary
  comparisons, and printing now all treat `HeapPointer` like `Int`
  so `val a = __gc_alloc(...); println(a > 0)` compiles cleanly.

  The debug builtins originally exposed the raw heap surface end to
  end: allocation, tagged records/arrays, heap-string construction
  and every string/list/map operation, pin/unpin, and introspection
  (pointer/segment/collection counts). That surface has since been
  removed from the user-facing language (the typechecker no longer
  declares them, so a Klassic program cannot reference them; a large
  Rust-side test and codegen suite for the removed primitives was
  deleted alongside it). Eleven primitives remain as
  compiler-internal-only dispatch tags, reachable exclusively through
  the desugar passes that synthesize them (never from parsed user
  source, since nothing declares them to the typechecker):
  `__gc_alloc`, `__gc_record`, `__gc_write` (enum construction),
  `__gc_read` / `__gc_read_ptr` / `__gc_read_string` /
  `__gc_read_double` (enum field access), `__gc_string` /
  `__gc_string_concat` (normalizing a Display/interpolation result
  into a traced heap string), and `__gc_int_to_string` /
  `__gc_double_to_string` (interpolation number formatting). Ordinary
  `String`, `List`, and `Map` values -- built through the runtime
  scratch-buffer representations described elsewhere in this
  document, not the tagged-heap-object primitives above -- remain
  the only heap-backed values a Klassic program can construct
  directly; the GC manages all of them without any explicit API.
- Monomorphic enums support a `Double` payload field
  (`EnumFieldRepr::DoubleField`): a new `NativeValue::RuntimeDouble`
  carries raw IEEE-754 bits in Rax (mirroring `Int`), boxed into an
  enum's `__gc_record` slot exactly like a scalar and unboxed via a new
  `__gc_read_double` builtin; minimal SSE2 codegen (`movq`
  xmm&lt;-&gt;gpr, `addsd`/`subsd`/`mulsd`/`divsd`, `ucomisd`,
  `cvttsd2si`/`cvtsi2sd`) backs `+ - * / == != < <= > >=`, with the four
  ordering comparisons made NaN-safe by swapping operands into the
  existing CF-based `Above`/`AboveOrEqual` conditions and `==`/`!=` using
  a new `Condition::Parity` guard. Formatting has a whole-number fast
  path (`<digits>.0`) and an exact-binary-fraction path (`<digits>.
  <digits>`, e.g. `2.5`), shared by direct `println` and the string-
  interpolation materialize path via
  `emit_append_whole_runtime_double_to_runtime_buffer_offset_label` (see
  the later `emit_load_runtime_double_ascii` entry below for how the
  fraction path recovers exact digits); a value outside both paths (NaN,
  Infinity, or a fractional value whose exact decimal expansion is too
  long, e.g. `0.1`) traps with a clean runtime error rather than risk
  wrong or mismatched-length digits, since there is no general
  shortest-round-trip formatter here. A `Double` literal's `Expr::Double`
  arm now also
  materializes its bits into Rax (previously purely a compile-time
  `StaticDouble` tag with no codegen) so an `if`/`match` branch merging a
  literal Double arm with a genuinely runtime one unifies soundly to
  `RuntimeDouble`. Generic enums keep rejecting `Double` fields
  (`classify_generic_field`) since per-instantiation shape tracking for
  them isn't wired up yet.
  `.map`/`.foldLeft` over a genuinely runtime (non-constant-folded) list
  compile their mapper/reducer body once per element via a Rust-side loop
  (`compile_compiled_literal_values_map`/`_fold_left`) whose
  `push_scope`/`pop_scope` pair reuses the same frame-relative stack slot
  for the element parameter on every iteration -- the same slot-reuse
  hazard the loop-escape check above guards for `foreach`/`while` bodies,
  but that checker never inspected this codegen-synthesized loop. A
  closure returned from the mapper/reducer that captures the element
  parameter and is folded into the mapped list or the accumulator (both of
  which outlive the iteration) escaped into a stale/reused slot, a silent
  miscompile rather than a diagnostic. Both loops now run the same
  `native_value_escapes_loop_scope` check against the per-element scope's
  base offset on the compiled per-iteration result and refuse with a clean
  diagnostic when it escapes, leaving lambdas that return plain values or
  closures over pre-loop bindings unaffected.
- Runtime `Double` formatting now also handles exact binary fractions
  (`2.5`, `3.25`, `0.125`, ...), not just whole numbers. The whole-number
  check and both formatting sites (`emit_print_runtime_double`,
  `emit_append_whole_runtime_double_to_runtime_buffer_offset_label`) were
  refactored to share one `emit_load_runtime_double_ascii` routine that
  leaves `Rsi`/`Rcx` pointing at the formatted ASCII bytes. When the
  whole-number round trip fails, it recovers the IEEE-754 mantissa/
  exponent directly from the raw bits (all GPR bit ops, no floating-point
  re-multiplication), strips the mantissa's trailing zero bits to find
  the odd `M` and exponent `k` such that `value == M / 2^k`, and computes
  the exact numerator `M * 5^k` with an overflow-checked `imul` loop
  (`NoOverflow`-gated, same idiom as `emit_integer_overflow_check`) bounded
  at `k <= 27` (`5^28` alone exceeds `2^64`, so no larger `k` can avoid
  overflow regardless of `M`). A second, independent bound rejects any
  result whose significant-digit count exceeds 15: a double's exact
  terminating expansion can run longer than what the type actually
  distinguishes (e.g. `1 / 2^27` has a 19-significant-digit exact
  expansion but prints as a 16-digit `to_string()`), and per the
  IEEE-754 `DBL_DIG == 15` guarantee, any <=15-significant-digit decimal
  round-trips through a double injectively, so a `D <= 15` exact
  expansion is guaranteed to equal Rust's shortest form (verified against
  `to_string()` for thousands of generated cases; empirically the first
  mismatch appears at `D == 17`). Values outside either bound (`0.1`,
  `0.3`, `123.456`, NaN, Infinity) keep tripping the existing clean
  runtime-error trap rather than ever emitting a wrong or mismatched-
  length digit string.

### `klassic-runtime`
- Shared runtime crate scaffold for behavior that should move out of `klassic-eval`
  as the implementation is split further.

### `klassic-macro-peg`
- Standalone macro PEG parser, AST, evaluator, and evaluation-strategy support

### Root Binary `klassic`
- Command-line parsing
- File execution
- REPL
- Exit-code policy and error presentation

## Design Notes

- The direct evaluator is the current execution engine.
- The native compiler is an additional batch build engine. Unsupported language
  constructs fail at compile time with span-aware diagnostics rather than falling
  back to the evaluator.
- The ELF writer computes the data segment offset from the actual text length and
  page-aligns it, so larger generated programs do not overwrite or truncate text.
- Diagnostics are source-span aware across parse, type, and runtime errors.
- Every `.kl` module the evaluator loads (the prelude, each imported `std.*`
  module, each locally imported user file) is evaluated as its own
  `SourceFile`, and every `FunctionValue`/`ThreadFunctionSnapshot` built while
  one is active is tagged with an `Arc` clone (`CURRENT_SOURCE` in
  `klassic-eval`). A runtime error that crosses a function-call boundary is
  stamped with the innermost def's own source the first time it does, so
  `Diagnostic::render_with_fallback` renders it against the module it
  actually came from rather than whatever file the top-level caller happens
  to be evaluating (issue #450).
- A `Diagnostic` also carries an optional `call_name`: the literal name the
  user wrote to reach the call currently in flight (e.g. `last` for
  `last([])`), tracked via a `CURRENT_CALL_NAME` thread-local pushed/restored
  by a drop guard around each of `eval_call`'s four `apply_callable` call
  sites in `klassic-eval`. `invoke_user_function` stamps a still-untagged
  diagnostic with it the same way it stamps `source`, so a runtime error
  raised deep inside a def's body (e.g. `head` failing inside `std.list`'s
  `last`) renders as `last: head expects a non-empty list` instead of naming
  only the innermost builtin — a bare builtin call with no wrapping user
  `def` gets no prefix at all, and a chain of wrapper calls attributes to
  the innermost def, not every frame it unwound through (issue #450, second
  half). Known gap: the five `apply_callable` call sites reached from
  *inside* a builtin's own implementation (a user callback passed to
  `map`/`foldLeft`/`filter`/`thread`/`stopwatch`) have no textual name
  available at that specific call point, so an error inside such a callback
  inherits the enclosing builtin's own name (e.g. `map: ...`) rather than
  naming the callback itself — an improvement over no name at all, but not
  fully precise; left as a documented limitation rather than chased further.
- The workspace keeps crate boundaries explicit so future optimizer or runtime
  work can stay isolated.

## Engineering Work

1. Keep Rust tests aligned with newly promoted `.kl` examples.
2. Move shared runtime code from `klassic-eval` into `klassic-runtime` when it
   reduces coupling.
3. Keep CLI and REPL behavior covered by integration tests.
