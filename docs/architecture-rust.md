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
- Batch native compilation through `klassic build [--target linux-x86_64|x86_64-unknown-linux-gnu|native] <file.kl> -o <output>`
- Reuses the Rust parser, rewrite pass, typechecker, and proof/trust analysis
- Emits Linux x86_64 machine code directly
- Carries an explicit `NativeTarget`; the only implemented target is currently
  `LinuxX86_64`
- Keeps supported targets in a metadata registry containing the compact name,
  standard triple, architecture, direct-codegen backend, data layout,
  operating system, ABI, and executable format
- Exposes supported target names from the native target API so CLI help and
  target parsing stay aligned with the registry
- Resolves a per-compile native target context that pairs the selected target
  spec with its platform constants before entering codegen
- Keeps target-specific syscall numbers and OS constants such as fds, open
  modes, errno values, stat masks, clocks, mmap flags, and sendfile limits in a
  target-keyed platform constants registry
- Writes ELF64 executables directly without invoking `cc`, `as`, `ld`, Java, Scala,
  or the JVM
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
  runtime is added. Interpolated strings can also be folded into static native
  values when all fragments resolve through immutable static bindings, including
  fragments with mutable block prefixes when their final values remain
  recoverable. Interpolation fragments that resolve to native runtime strings,
  or to dynamic native `Int` / `Boolean` values, produce fixed-buffer
  `RuntimeString` values.
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
  dynamic length and slot contents.
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
  reuse. The kind tag also drives display: `println` and string
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
  bindings by rebinding them inside the emitted function frame. Direct calls and value aliases for
  user-defined functions shadow same-named native builtins.
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
  Selective and bulk imports resolve directly; aliased imports
  (`import std.x as M`) additionally have their `M.func` access rewritten
  to a bare `func`. The ADT-backed modules (`std.option` / `std.result`)
  now inline as well, on top of the generic-enum specialization above:
  their constructors (`some` / `ok`), consumers (`getOrElse` / `unwrapOr`
  / `isSome` / `isErr`) and method-style extensions compile to native.
  Helpers that *return* a freshly constructed generic enum through a
  `match` (`mapOption` / `flatMapOption` / `orElse` / `mapResult`) work too:
  a generic-enum shape produced inside a branch is captured per branch and
  the `if` lowering publishes the merged shape of its two branches, so the
  joined value carries it to its binding. The merge prefers a resolved
  field repr over a defaulted one (a defaulted field is only ever read in a
  tag-guarded dead arm, so trusting the resolved branch is sound), letting
  a string-payload `mapOption` read back as a string rather than the
  `None` arm's defaulted scalar.

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
  toolchain: the writer lays the image out as a static `LC_UNIXTHREAD`
  executable whose code talks to the kernel via `svc #0x80` (Darwin
  syscall numbers in `x16`), and embeds the ad-hoc code signature —
  a SHA-256 CodeDirectory computed in-process — that the Apple
  Silicon kernel demands before executing any arm64 binary. Code is
  position-independent (`adrp`+`add` data addressing) because the
  kernel slides `MH_PIE` images. The backend starts from the same
  kind of small vertical slice the x86_64 backend grew from —
  currently top-level `println` of literals — and unsupported
  constructs fail with source-located diagnostics. `--target
  aarch64-apple-darwin` selects it from any host (cross builds work);
  a target-less `build` on macOS keeps routing through the C backend
  until the direct backend reaches useful parity.

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

  The debug builtins drive the GC end-to-end:
  `__gc_alloc(size)` (type tag 1, raw bytes, with native runtime
  guards against negative and overflowing sizes); `__gc_record(num_fields)`
  (type tag 2, packed heap pointers, fixed shape); `__gc_array(num_slots)`
  (type tag 3, packed heap pointers, indexed); `__gc_string(text)`
  (heap-allocated length-prefixed string built from a static literal or runtime
  `String`);
  `__gc_string_concat(a, b)` (joins two heap strings into a new one,
  spilling both inputs into shadow-stack-tracked slots so the
  allocation in the middle cannot reclaim them, and checking total
  byte length before payload-size arithmetic); `__gc_string_println(g)`
  (writes the heap string's bytes followed by `\n` to stdout); native `+`
  lowers through the same heap concatenation path when a `HeapString`
  participates, lifting static or runtime string fragments as needed, and
  native `==` / `!=` plus `assertResult` root the left operand across
  right-hand evaluation before reusing the byte-content equality scan;
  top-level or method-style `toString` copies heap bytes back into a
  fixed-buffer runtime `String` for ordinary string helpers; runtime string
  interpolation can append `HeapString` fragments into its fixed buffer;
  `__gc_string_len(s)` (validates and returns the byte length stored at
  offset 0);
  `__gc_string_alloc(n)` (reserves an `n`-byte zero-filled string for
  byte-by-byte construction); `__gc_string_get_byte(s, idx)` /
  `__gc_string_set_byte(s, idx, byte)` (single-byte access after
  validating the stored string length);
  `__gc_string_eq(a, b)` and heap string equality operators
  (stored-length validation followed by length-then-`repe cmpsb`
  byte equality);
  `__gc_string_substring(s, start, end)` (allocates a fresh heap
  string holding bytes `[start, end)` with three bounds checks
  and a stored-length sanity check before the destination allocation
  runs);
  `__gc_string_repeat(s, n)` (concatenates `s` with itself `n`
  times in a single allocation; negative `n` jumps to
  `gc_bounds_error`, the stored source length is validated, and
  total length is checked before allocation size arithmetic);
  `__gc_string_replace(s, from, to)` (counts non-overlapping
  matches, validates all stored string lengths, and checks the
  replacement result length before allocation);
  `__gc_string_split(s, sep_byte)` (counts separator bytes,
  checks the resulting list length before allocation, zeroes the
  destination pointer-list slots, then fills each slot with a fresh
  heap string segment);
  `__gc_string_index_of(s, byte)` (returns the first index of
  the low-byte of `byte` in `s`, or `-1` if absent — no
  allocation, just a movzx/cmp loop, after validating the stored
  string length);
  `__gc_string_index_of_from(s, byte, start)` (same byte search
  starting at `start`, returning `-1` if the cursor is at or past
  the end);
  `__gc_string_last_index_of(s, byte)` (reverse byte search,
  returning the highest matching byte index or `-1`);
  `__gc_string_to_int(s)` (permissive base-10 parser with
  optional leading `-` that stops at the first non-digit and
  returns 0 on empty / no-digit inputs, after validating the stored
  string length);
  `__gc_int_to_string(n)` (renders a signed integer to a fresh
  heap string via a digit-counting pass followed by a render-
  backwards pass into the exact-sized destination);
  `__gc_string_starts_with(s, prefix)` and
  `__gc_string_ends_with(s, suffix)` (length precheck plus
  `repe cmpsb` against the leading or trailing bytes, after
  validating both stored string lengths);
  `__gc_string_contains(haystack, needle)` (naive O(n*m) search
  via an outer i-loop and inner `repe cmpsb`, with all loop
  scratch slots allocated up front so no early conditional
  return skips a `sub rsp` emission, and both stored string
  lengths validated before scanning);
  `__gc_pointer_count(addr)` (derives the slot count of a record or
  array from the GC header — note that the count reflects the
  16-byte-aligned actual allocation, not the user-requested fields,
  and impossible or unaligned header sizes are rejected before the
  payload calculation);
  `__gc_segment_count()` (returns how many heap segments are
  currently mmap'd);
  `__gc_list_ptr(n)` (a heap-backed pointer list using a new tag-4
  layout `[len, ptr_0, ...]` whose mark phase skips the leading
  length qword); `__gc_list_ptr_len(lst)` validates and returns the
  stored length;
  `__gc_list_ptr_set(lst, idx, ptr)` /
  `__gc_list_ptr_get(lst, idx)` /
  `__gc_list_ptr_get_string(lst, idx)` (length and indexed pointer access
  for the new list, including a string-specific read for slots known to
  hold heap strings, and validating stored length before indexed
  access); `__gc_list_ptr_push(lst, ptr)` (returns a
  fresh tag-4 list with `ptr` appended, spilling both inputs into
  shadow-stack slots so neither the source list nor an inline
  `__gc_alloc(...)` argument is reclaimed by the destination's
  own allocation, and rejecting corrupted lengths before deriving
  the allocation size); `__gc_list_ptr_concat(a, b)` (returns a
  fresh tag-4 pointer list after checking that the two stored
  lengths can be safely summed for the destination allocation);
  `__gc_list_ptr_pop(lst)` / `__gc_list_ptr_reverse(lst)` (return
  fresh tag-4 pointer lists after checking that the stored length
  can be represented as the destination allocation size);
  `__gc_list_ptr_join(parts, sep)` validates the stored list,
  separator, and part lengths before using them to drive the
  two-pass join loop, and checks total output length before
  allocation;
  `__gc_list_int(n)` (a heap-backed integer list of length `n`,
  zero-initialized via a runtime fill loop so a free-list reuse
  cannot surface stale bytes); `__gc_list_int_len(lst)` (validated
  stored length read); `__gc_list_int_set(lst, idx, value)`
  and `__gc_list_int_get(lst, idx)` (untrusted-index element
  access, mirroring `__gc_read`/`__gc_write` for the dedicated
  list layout after validating stored length);
  `__gc_list_int_push(lst, v)` (returns a fresh list one slot
  longer with `v` appended — both inputs spill
  into shadow-stack slots so a collection inside the destination
  allocation cannot reclaim the source mid-copy, and corrupted
  lengths are rejected before the derived allocation size is
  computed);
  `__gc_list_int_pop(lst)` (returns a fresh list with the
  trailing element removed; popping an empty list jumps to
  `gc_bounds_error`, and corrupted lengths are rejected before
  deriving the smaller allocation size);
  `__gc_list_int_reverse(lst)` (returns a fresh list with the
  same payload in reverse order after checking the stored length
  can be represented as a destination allocation);
  `__gc_list_concat(a, b)` (returns a fresh int list whose
  payload is `a` followed by `b`, both copied via two rep movsb
  passes after checking that the two stored lengths can be safely
  summed for the destination allocation);
  `__gc_list_int_println(lst)` (prints `[a, b, c]\n`
  by driving `print_i64` per element through two anonymous stack
  slots that are released on exit after validating the stored
  list length); `__gc_list_int_sum` / `__gc_list_int_min` /
  `__gc_list_int_max` validate the stored list length before
  iterating; `__gc_list_int_to_string` additionally validates
  the separator length and checks total output length before
  allocation; `__gc_collect()`;
  `__gc_pin(addr)` / `__gc_unpin(addr)` for explicit static-table
  registration alongside the automatic shadow-stack tracking; and
  `__gc_read(addr, offset)` / `__gc_read_ptr(addr, offset)` /
  `__gc_read_string(addr, offset)` / `__gc_write(addr, offset, value)` for raw
  qword access, including scalar reads, pointer- or heap-string-provenance
  reads, and `Int`, `HeapPointer`, or `HeapString` stores (which doubles as
  record-field and array-slot access since both share the same packed-pointer
  layout). String-keyed maps validate that their backing pointer-list length is
  non-negative, in range, and even before scanning interleaved key/value slots;
  key string lengths are checked before equality scans. They use
  `__gc_smap_get` for generic pointer values and `__gc_smap_get_string` when a
  present value is known to hold a heap string.
  Marking uses an iterative
  worklist keyed off the type-tag header field: the
  `gc_mark_visit` subroutine sets the mark bit and pushes the
  block onto a 4096-entry trace stack; the trace loop pops each
  block and, when its tag is at or above "pointer record", walks
  the payload qword by qword recursively visiting every non-null
  pointer field.

  Thirty-five integration tests cover the lifecycle: reclamation when
  nothing is rooted, explicit-pin survival across a heap stress
  loop, recursive marking through a pointer record's two child
  blocks, automatic stack-slot retention so a `val a = __gc_alloc(...)`
  binding survives a heap-stress collection without any explicit
  `__gc_pin`, a basic `__gc_alloc` / `__gc_collect` smoke check,
  an eight-slot 1.2 MiB workload whose live blocks force the
  runtime to grow the heap beyond the initial segment and still
  read back the original sentinels after a follow-up collection,
  a `__gc_array` test that proves tag-3 payloads are walked
  identically to records, a `__gc_string` concat test that
  builds heap-allocated strings, joins them with a fresh heap
  allocation, drops the originals from explicit reach, runs two
  collections under heap pressure, and prints the survivors via
  `__gc_string_println` to confirm both copies and tracing work
  correctly, a `__gc_list_int` round-trip test that populates
  a heap-backed integer list, forces a heap-pressure collection
  while only the slot pins it, then reads back individual
  elements and the full `__gc_list_int_println` formatting, a
  heap-string introspection test exercising `__gc_string_len`,
  `__gc_string_eq` for both equal and unequal inputs, and a
  dynamically-allocated string built byte-by-byte through
  `__gc_string_alloc` + `__gc_string_set_byte` that survives an
  intermediate heap-pressure collection, and a final introspection
  test for `__gc_pointer_count` on a record and an array plus
  `__gc_segment_count` increasing past one once the heap grows,
  a `__gc_list_ptr` test that builds four sentinel children,
  stores them through the indexed setter, drops the direct
  references, pins only the list, and forces a heap-pressure
  collection — every child must still be reachable through the
  list's slots, proving the new tag-4 trace branch correctly
  skips the leading length and walks the rest as pointers,
  two bounds-check tests (one for a negative index, one for
  index >= length) that confirm the shared `gc_bounds_error`
  subroutine prints `klassic gc: index out of bounds` and exits
  with status 1 instead of writing past the payload, and a
  five-test `__gc_string_substring` matrix covering the in-
  bounds happy path (mid / full / empty windows), survival
  across an intervening heap-pressure collection, and the three
  failure modes (negative start, end past length, start > end)
  that all funnel into the same diagnostic and exit code, and a
  pair of `__gc_list_int_push` tests that build a five-element
  list incrementally and confirm it survives interleaved
  heap-pressure collections that drop every previous version,
  a pair of `__gc_list_ptr_push` tests covering the same
  incremental-grow scenario for tag-4 pointer lists — the second
  test appends inline `__gc_alloc(...)` results never bound to a
  `val`, so the only reason the children survive a heap-pressure
  collection is the list's mark-phase trace through its slots,
  plus a seven-test sweep of higher-level operations:
  `__gc_string_repeat` (basic, empty-source fast path,
  negative-count rejection, and size-overflow rejection),
  `__gc_string_index_of` (start / mid / end / missing / empty),
  `__gc_string_to_int` (positive, zero, negative, empty,
  partial-parse, invalid-leading, large), `__gc_list_int_pop`
  (round-trip down to `[]` plus an empty-list bounds error), and
  `__gc_list_concat` (both non-empty plus three empty-side
  combinations), and a four-test sweep covering
  `__gc_int_to_string` (zero, single digit, multi digit,
  negative, large), combined `__gc_string_starts_with` /
  `__gc_string_ends_with` (matching / non-matching / empty /
  longer-than-string / equal-length), `__gc_string_contains`
  (start / mid / end / missing / empty needle / longer needle /
  empty haystack — this test exposed a stack-tracker bug where
  an early conditional jump skipped a `sub rsp` emission and
  corrupted successive callers, fixed by allocating loop scratch
  slots before any control-flow split), and
  `__gc_list_int_reverse` (5 element, single, empty).
  The next phase of integration is wiring the existing
  structural string / list / record builtins onto the heap so
  any source program participates in GC without going through
  the explicit `__gc_*` interface.

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
- The workspace keeps crate boundaries explicit so future optimizer or runtime
  work can stay isolated.

## Engineering Work

1. Keep Rust tests aligned with newly promoted `.kl` examples.
2. Move shared runtime code from `klassic-eval` into `klassic-runtime` when it
   reduces coupling.
3. Keep CLI and REPL behavior covered by integration tests.
