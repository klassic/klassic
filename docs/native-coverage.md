# Native Compiler Coverage

This document enumerates the language constructs the native compiler currently
lowers to direct ELF for Linux x86_64 (selectable as `linux-x86_64`,
`x86_64-unknown-linux-gnu`), PE64 for Windows x86_64 (selectable as
`windows-x86_64`, `x86_64-pc-windows-msvc`), and direct Mach-O arm64 for macOS
(selectable as `macos-aarch64`, `aarch64-apple-darwin`). On matching hosts, a
target-less bare `klassic build` defaults to the host platform (i.e., `native`);
Windows x86_64 hosts now use the direct PE64 backend. Anything not listed
here fails at build time rather than falling back to the evaluator.

## Core Surface

The first vertical slice covers integer and boolean expressions, string
literal printing, `println`, `printlnError`, `assert`, curried `assertResult`,
`if`, `while`, mutable integer/boolean locals, assignment, and static
integer-list `foreach` unrolling.

Time and concurrency primitives:

- Static and runtime integer-millisecond `sleep` via Linux `nanosleep`.
- Zero-argument literal or lambda-value `stopwatch` via Linux `clock_gettime`.
- Queued `thread` bodies from literal or lambda-value jobs for the current
  native sample surface.

Function shapes natively supported include top-level recursive integer
functions and annotated boolean-returning / boolean-argument functions.
Obvious unannotated integer/boolean function and top-level lambda return
values are inferred for native codegen.

## Function Calls

Native function calls support stack-passed arguments beyond the first six
integer/boolean parameters. The native codegen tracks its own temporary stack
slots so nested argument evaluation can allocate local captures safely.

Top-level lambda bindings are lowered as static functions or inlined at call
sites when they capture mutable native locals. Direct inline lambda calls can
receive runtime integer/boolean arguments without folding away impure lambda
bodies.

Call-site inlined unannotated `def`s whose return is inferred from the call
site or annotated as `String` / `List<String>` also bind actual runtime
`String` and `List<String>` arguments directly, so small pass-through and
string-literal concatenation helpers do not require parameter annotations.

Recursive scalar-returning native functions can also accept annotated
`String` and `List<String>` parameters. Call sites stage runtime strings or
static / runtime line lists into fresh buffers before copying them into fixed
function-parameter buffers immediately before the scalar call ABI is invoked,
so reentrant and self-recursive calls preserve left-to-right argument
evaluation. Annotated `String` and `List<String>` function returns use fixed
return buffers that are copied into call-site buffers so adjacent calls do
not overwrite each other.

Annotated record parameters and returns whose fields lower to runtime
`String`, `List<String>`, `Int`, `Boolean`, or nested supported records use
the same fixed field-storage model. Call sites stage record arguments before
copying them into function-parameter storage and copy record returns into
call-site storage, so recursive functions can return supported runtime
records without call-site inlining.

Recursive functions that would otherwise need unsupported call-site inlining
are still folded when all call arguments are static, so pure helpers over
static lists can compile, including helpers that build static lists with
`cons` and helpers that take static callable arguments, without entering the
emitted recursive ABI path.

Recursive functions can also capture immutable top-level runtime strings,
line lists, selected-length runtime lists, and runtime records by rebinding
their existing fixed storage inside the emitted function frame.

## Function Values, Aliases, And Conditional Callables

Function value aliases, static record fields, runtime `String` /
`List<String>`, dynamic `Int` / `Boolean`, and nested runtime record fields,
direct or method-style `head` lookups from static lists including `tail` and
`cons` chains, and static `Map#get` / `.get` lookups with literal or folded
static keys for such `def`s preserve the runtime return metadata, so aliased
calls can still participate in string concatenation, line-list helpers, and
compatible record display.

Runtime string / int / bool-key lookups over static callable maps can be
bound to immutable values, called later, and formatted with the selected
callable display through printing, interpolation, string concatenation, or
`toString`. Equality involving those function values follows the evaluator's
always-false function comparison semantics. Recursive functions can capture
such bound callable dispatch values when the selector and candidates do not
depend on the recursive function frame.

Block, cleanup, and same-runtime-return conditional callees preserve the
same metadata for immediate calls. Immediate calls on conditional function
values lower to branch-local calls, so runtime string and line-list return
values can merge through the existing dynamic `if` result buffers, including
`List<String>` joins where one branch is a static string list and the other
branch is either another static string list or a runtime line list.

Pure conditional callable branches used in immutable bindings or static
aggregate elements snapshot the condition once, then store a synthesized
callable that performs the same branch-local call when invoked, including
supported builtin function values with matching arity. Conditional builtin
callables keep evaluator-style selected-branch `<builtin:name>` display,
including when returned from functions and observed through interpolation,
string concatenation, or `toString`, including bound interpolation /
concatenation strings and aggregate `toString` for lists, records, maps, and
sets that contain them.

Static lambda values returned from functions can be bound and called again
when their captured values and call arguments are statically recoverable.
Inline and top-level lambda calls use the same annotated `String` /
`List<String>` parameter matching. Static record lambda methods follow the
same rule, so native method calls keep mutable side effects on the runtime
path, including effectful receiver and argument expressions when their final
values remain statically recoverable.

## Static Folding Versus Mutable Effects

Static `if` folding is limited to pure conditions and selected branches,
keeping mutable branch effects in generated code. Dynamic `if` expressions
whose branches yield different native strings or runtime line lists copy the
selected branch result into a fixed runtime buffer, so the merged value
remains printable, comparable, and usable in later string / list helpers.

Assignments to runtime integer / boolean locals inside dynamic control flow
clear stale static facts for those locals. Dynamic `while` loops that cannot
be fully simulated also invalidate static facts for locals assigned in the
loop condition or body before later expressions are folded, and runtime-list
locals assigned in those loops are materialized into mutable selected-length
storage before the loop runs.

Static binary folds use the same purity check before replacing numeric,
equality, or string-concatenation expressions. Numeric Float / Double binary
expressions can also preserve mutable block-prefix effects when both sides
recover static numeric values.

String concatenation can preserve mutable block-prefix effects when a side
ultimately yields a static string value. Native `+` concatenation can build
fixed-buffer runtime strings when a static / runtime string is combined with
dynamic native `Int` or `Boolean` values. Native `toString` formats dynamic
`Int` and `Boolean` values into fixed-buffer runtime strings, and falls back
to evaluator-style display for static-native values that survive dynamic /
effectful evaluation, such as `null` and `()`.

Logical `&&` and `||` preserve normal short-circuiting in generated native
code, including static fact tracking, so skipped RHS effects are not used by
later native folds.

Static call folding and `assertResult` folding require pure arguments, so
argument blocks with mutable effects are evaluated by generated code.
Immutable aliases to curried helpers such as `assertResult`, `cons`, `map`,
and `foldLeft` lower to the same native paths as direct calls. Static
equality and `assertResult` over aggregate values preserve side-effecting
expected / actual expressions while comparing recovered values.

Static-list `map` and `foldLeft` can unroll lambdas with mutable prefix
effects when the final lambda result remains statically recoverable;
method-style `xs.map(f)` and `xs.foldLeft(initial, reducer)` use the same
native path. Static string / Map / Set helper calls that can still determine
static argument values after evaluating impure argument blocks preserve those
generated side effects before folding the helper result.

Static `join`, FileInput / FileOutput path / content / list arguments, and
Dir path arguments follow the same rule when the resulting arguments remain
statically recoverable. Static `cons` head and tail arguments use that rule
too, so list construction does not fold away mutable block effects. Static
list, map, set, record literal, and record constructor arguments use the
same recovery path.

## Strings

`println` and `printlnError` can stream simple string-concatenation
expressions and string interpolation fragments without a heap string runtime
yet. Interpolated strings whose fragments depend on immutable static values
can be folded for native `val` bindings and `assertResult`. Fragments
containing runtime native strings or dynamic native `Int` / `Boolean`
fragments produce fixed-buffer `RuntimeString` values. Mutable block
prefixes inside fragments are preserved when their final values remain
statically recoverable.

Static string and integer list values can be bound with `val`. Static string
helper calls such as `substring`, `split`, `join`, `trim`, `replace`,
`startsWith`, `indexOf`, `length`, `repeat`, and method-style
`"text".contains("x")` are folded during native compilation. Static
`substring` / `at` use the native runtime slice path when their indexes come
from mutable or otherwise dynamic integer values, and static string `split`
plus static string-list `join` accept runtime string delimiters. Static
first-occurrence `replace` accepts runtime string pattern and replacement
operands, all-occurrence `replaceAll` accepts runtime pattern and replacement
strings, and static `repeat` accepts runtime integer counts.

Static string helper functions can be bound through immutable aliases such as
`val sub = substring` and called through that alias in native builds.

Runtime `FileInput#all` / `FileInput#readAll` bindings support the same
fixed-buffer native string path for printing, concatenation, equality,
`assertResult`, method-style `toString`, `substring` / `at` with static or
runtime integer indexes, ASCII-whitespace trimming, `length`, `repeat` with
static or runtime integer counts, and string search predicates, plus ASCII
`toLowerCase` / `toUpperCase`, simple `matches` with static or runtime
patterns, first-occurrence `replace` with static or runtime literal operands,
all-occurrence `replaceAll` with static or runtime pattern and replacement
strings, and UTF-8 `reverse`. `__gc_string(runtimeString)` can also copy these
fixed-buffer runtime strings onto the GC heap as `HeapString` values for
heap-backed string composition. Once an operand is a `HeapString`, native `+`
lifts static or runtime string fragments as needed and concatenates through
the GC heap, and native `==` / `!=` plus
`assertResult` compare heap strings by byte content while keeping left-hand
temporaries rooted across right-hand evaluation. `toString(heapString)` copies
heap bytes back into a fixed-buffer runtime `String` for ordinary string
helpers; method-style `heapString.toString()` uses the same bridge. Runtime
string interpolation can append `HeapString` fragments.
High-level collection literals reject GC heap pointer values for now; use the
explicit `__gc_list_ptr_*` helpers for heap-pointer collections until Phase B
migrates ordinary lists onto the GC heap.
GC helper calls that consume heap addresses require values produced by GC
allocation or pointer-returning helpers; plain `Int` values are rejected at
build time even though the temporary debug surface is still source-typed as
integers. Raw `__gc_write` still accepts `Int`, `HeapPointer`, or `HeapString`
values as qwords, while its address operand must come from a GC pointer source.
Use `__gc_read_ptr(addr, offset)` rather than `__gc_read(addr, offset)` when a
raw field is meant to flow back into address-taking GC helpers; `__gc_read`
continues to model scalar qword reads as `Int`. Use
`__gc_read_string(addr, offset)` when the field is known to hold a heap string
and should re-enter the `HeapString` surface for `println`, `+`, `toString`, and
`assertResult`. Pointer lists have the same split surface: `__gc_list_ptr_get`
preserves generic pointer provenance, while `__gc_list_ptr_get_string` returns a
known heap-string slot as `HeapString`. String-keyed maps mirror that convention
with `__gc_smap_get` for generic pointer values and `__gc_smap_get_string` for
present values known to be heap strings.

Static string concatenation can be used in immutable bindings and static
record fields when at least one operand is a static string, including
static helper calls such as `size`, `head`, and method-style `parts.size()`.
Runtime string concatenation also accepts dynamic native `Int` / `Boolean`
operands by formatting them into the fixed string buffer.

Mutable runtime string and runtime line-list bindings copy assignments into
fixed buffers, so loops can update string accumulators and line-list cursors.
Closures that capture those fixed buffers stay on the native inline-call
path instead of being mistaken for statically foldable lambdas. Effectful
callee expressions that return string / list helper builtin values, such as
`toUpperCase`, `split`, `join`, or `contains`, dispatch through the runtime
string and runtime line-list native helper paths.

## Numeric Helpers

Integer `abs`, `int`, `floor`, and `ceil` calls are emitted for Int
arguments. Static Double / Float literals plus static numeric helpers such
as `double`, `sqrt`, `abs`, `floor`, and `ceil` are folded into printable
native constants, with Float values preserving f32 rounding and display.
Helper arguments with mutable block prefixes are evaluated before recovering
the final static numeric value.

## Static Lists, Maps, And Sets

Static integer list literals, including simple constant arithmetic elements,
are emitted into native data sections and can be printed or passed to
`size`, `isEmpty`, `head`, `tail`, and static `cons` / `map` / `foldLeft`.
Static non-integer lists are represented in compile-time arenas and support
printing, `foreach` unrolling, `size`, `isEmpty`, `contains`, `head`,
`tail`, `join`, static `map` for static mappers, static `foldLeft` for
static accumulator reducers such as string concatenation, generic static
`cons`, ordinary `==` / `!=`, and `assertResult`.

Known Int-list `foreach` bindings are also available to static folds inside
the loop body, so native code can build static lists and records from the
iteration value. Int-list `foldLeft` can build static list accumulators,
such as reverse via `e #cons acc`.

Static `if` expressions whose conditions fold to booleans can produce static
strings, lists, records, maps, sets, null, or unit values. Simple mutable
loops are tracked for later static folds when the generated loop code
remains dynamic.

Static `map`, `foldLeft`, fold-like three-stage curried calls, direct static
typeclass methods, and List `bind` / `unit` calls support integer and
static numeric / string lambdas that can be folded into native constants or
data sections, including lambdas that call returned static closure values.

Static structural and nominal records, static map literals, and static set
literals can be bound, printed, nested, queried with static map / set
helpers, and compared with `assertResult` when their contents are static
native values.

## Records

Record literals and constructors can carry fixed-buffer runtime `String` /
`List<String>`, dynamic `Int` / `Boolean`, and nested runtime record fields
for field selection, printing, and equality against compatible records.
They can be formatted through `toString`, interpolation, and string
concatenation. Dynamic `if` expressions can merge compatible runtime record
branch results through shared runtime field storage.

Mutable runtime record bindings reuse the same field storage for compatible
record assignments, including supported static-record initializers.
Annotated record function parameters and returns use compatible field
storage across normal and recursive native calls. Runtime line-list
`foldLeft` can use the same storage for record accumulators, including
empty `List<String>` fields.

## Enums And Match

Monomorphic enums lower to GC records with short-circuit tag dispatch:
each variant is a heap cell whose first slot is the boxed tag and whose
remaining slots box the payload. Supported payloads are integer (`Int` /
`Long` / `Short` / `Byte`), `Boolean`, `String`, and nested
monomorphic-enum fields, with distinct variant names. Generic enums
(`Option<a>`, `Result<a, b>`, self-referential shapes) compile per
instantiation, with payload shapes tracked through `val` bindings,
control-flow joins, and fully-applied annotated function boundaries — so
recursive functions over `Option<Int>`- or `Tree`-style annotations
lower natively. Enum values format through `toString`, interpolation,
and string concatenation, and compare with `==` / `!=` through a
structural deep-equal that matches the evaluator (rather than the heap
identity a bare pointer comparison would give).

`match` lowers to a tag-test if-chain over constructor patterns,
including nested constructor patterns, integer / string literal
patterns, variable bindings, wildcards, and arm guards. The checker
diagnoses match exhaustiveness and unreachable arms ahead of codegen.

Remaining gaps fail at build time with a source-located diagnostic
rather than miscompiling: aggregate payload fields such as
`List<SomeEnum>`.

## Extension Methods

`extension` declarations lower to ordinary native functions, and a
`receiver.method(...)` call on a value of the extended type dispatches
to the matching one. This covers extensions on built-in types such as
`Int` and `String`, generic extensions like `extension <a>(this:
List<a>)`, receiver use inside the body, and method chaining.
Standard-library extension methods (for example `.reversed()` from
`std.list` or `.capitalized()` from `std.string`) compile the same way
once their module is imported; the evaluator auto-loads the standard
library, so it additionally accepts those calls without an explicit
import.

## Type Classes

`typeclass` declarations, their `instance` implementations, and
constrained generic functions (`def f<'a>(x: 'a): R where Show<'a> =
...`) lower natively. Both direct instance-method calls (`show(42)`) and
dispatch through one or more constraints — including transitive
constraints, where one constrained function calls another — resolve to
the concrete instance at the call site and compile to ordinary native
calls. Instances over concrete types such as `Int` and `String` and
over applied types such as `List<Int>` are supported. A constraint with
no matching instance is rejected ahead of codegen with a `missing
instance` diagnostic, in native builds exactly as in the evaluator.

## Proof Surface

`axiom` and `theorem` declarations are a static, type-check-time
surface — Klassic does not run a proof checker on theorem bodies, and
the declarations are erased before codegen, so a native build compiles
and runs the ordinary runtime portion of a program that uses them. The
`--warn-trust` and `--deny-trust` gates apply to native builds exactly
as they do to the evaluator: `--warn-trust` reports every theorem whose
proof transitively depends on a trusted (`axiom` or `trust`-marked)
declaration, and `--deny-trust` rejects the build when any reachable
theorem does. Calling a zero-argument axiom in proof position is a
shared type-check limitation, diagnosed identically by both paths.

## Cleanup Clauses

A `cleanup { ... }` clause runs after its associated expression on the
normal completion path. Native builds do not unwind cleanup clauses when
that expression aborts at runtime (for example on division by zero): the
evaluator runs the clause before propagating the error, while a native
build reports the error without running it. A program that aborts
therefore differs only in the cleanup side effect, not in the final
exit; restoring abort-time unwinding is a known gap.

## Runtime List Literals And Selectors

Direct `head` over list literals can return runtime native values, including
records, after preserving every element's evaluation effects. Direct `tail`
over list literals can return runtime line-list values from runtime string
elements, or runtime-list values for non-string tails, on the same
evaluated-elements path.

List / set literal `contains` and map literal `containsKey` /
`containsValue` can compare runtime native values directly while preserving
literal evaluation effects.

Literal list / map / set `size` and list / map / set `isEmpty` selectors
answer after preserving effects, without requiring a first-class runtime
collection. Set literal `size` counts only distinct runtime values.

List literal `foreach` unrolls runtime native values after evaluating every
element before the loop body. List literal `map` unrolls runtime native
values into runtime line-list results when every mapper result is
string-compatible, or runtime-list results for non-string mapper outputs.
List literal `join` joins runtime string elements into a runtime string
without materializing a first-class runtime list.

List / map / set literal display / `toString` renders evaluated runtime
native values into runtime strings for supported element, key, and value
types, and the same path is available through printing, interpolation, and
string concatenation. List / map / set literal equality and `assertResult`
compare evaluated runtime native elements, keys, and values against
compatible literal expectations or static collection bindings.

Immutable list literal bindings retain evaluated runtime native elements as
runtime list values, with printing, `toString`, interpolation, string
concatenation, equality, `assertResult`, `head`, `tail`, `size`, `isEmpty`,
`contains`, `cons`, `foreach`, `map`, `foldLeft`, `join`, and
`FileOutput#writeLines` support. Runtime-list `foldLeft` can use compatible
`List<String>` accumulators, including reducers that build a list with
`cons`.

When a runtime-list label carries a selected runtime length, the native
helpers preserve that selected prefix through `contains`, `cons`, `foreach`,
`map`, scalar / string / line-list / record `foldLeft`, display, printing,
`toString`, `size`, `isEmpty`, `head`, `tail`, `join`, runtime-list
equality, and runtime-list accumulator `foldLeft`. Skipped variable-length
iterations preserve the previous accumulator storage.

Runtime records can carry those runtime list values through field access,
display, and equality against compatible static records. Dynamic `if`
branches can merge fixed-capacity runtime-list results, including compatible
static-list branches and record fields that carry runtime-list values, and
preserve selected runtime lengths whenever a runtime-list branch
participates in the merge.

Annotated `List<String>` function parameters and returns can consume
compatible runtime-list values by copying the selected string-element prefix
into fixed line-list buffers. Mutable bindings can rebind runtime-list
values in straight-line native code and in dynamic `while` loops when
assigned lists fit the materialized runtime-list capacity; each assignment
updates the selected list length.

List literal `foldLeft` reduces runtime native values into native scalar,
string, line-list, runtime-list, or record accumulators on the same
evaluated-elements path.

## Static Maps With Runtime Keys

Static maps can return supported static records or non-string static lists
from runtime string / int / bool keys by copying the selected entry into
runtime record or runtime-list storage. Static-map lookups whose selected
list length is not known until runtime track that length separately, so
`print`, `toString`, `size`, `isEmpty`, `head`, `tail`, `contains`, `cons`,
`foreach`, `map`, scalar / string / line-list / record `foldLeft`, `join`,
and equality only observe the selected prefix. Nested record fields can use
the same variable-length runtime-list storage.

Map literal `Map#get` / `.get` selects runtime native values, including
variable-length runtime-list values, from static or runtime keys while
preserving every entry's evaluation effects. Direct `Map#get(...) == null`
and `Map#get(...) != null` checks over static maps or map literals lower to
key-match tests, so runtime misses and null-valued hits can be tested
without materializing a dynamic tagged `null`.

Runtime record results can be passed back through static list / set
`contains` and map `containsValue` helpers for structural record membership.
Static string-key maps, static string-valued maps, string sets, and scalar
list / set / map entries answer `containsKey` / `containsValue` /
`contains` queries from runtime strings, ints, and booleans.

The two-argument `m.getOrElse(k, d)` lowers to a temp-bound
`containsKey` / `get` / `if`, evaluating `m` and `k` once and falling back
to `d` on a miss. `m.keys()` / `m.values()` return the key or value list of
a map — a runtime map reuses the key/value list builders, while a static map
literal projects its compile-time entries. `s.toList()` projects a static
set's elements into a list.

Builtin module aliases, selective imports, and aliased helper values, such
as `import Map as M`, `import Map.{size}`, and `val readAll = FI#readAll`,
resolve to the same native helper implementations. Static record fields may
contain lambda methods that are called with the receiver for native static
evaluation.

## File I/O And Directories

Static file input / output helpers for static paths are supported through
Linux file syscalls plus compile-time virtual file tracking.
`FileOutput#write` / `FileOutput#append` can write fixed-buffer runtime
string content. Static-path `FileInput#open` callback bodies and callable
callback values bind the stream path before normal native compilation, so
they may return supported runtime values as well as folded static values.

Paths whose contents become unknown through runtime writes or dynamic
branches fall back to runtime `FileInput#all`, `FileInput#lines` /
`readLines`, `FileOutput#exists`, `Dir#exists`, `Dir#isFile`,
`Dir#isDirectory`, `Dir#list`, and `Dir#listFull` syscalls.

Runtime string values can be used as paths for `FileInput#all`, direct
file-input printing, and `FileInput#open` callback bodies or callable
callback values whose stream parameter flows through supported runtime
string and file helpers, including `readAll` / `readLines`, `length`,
`cleanup`, or returning the path itself.

Direct printing or immutable printable bindings of `FileInput#lines` /
`readLines` are supported. Those runtime line lists support `size`,
`isEmpty`, `head`, `tail`, `cons`, `contains`, `map` with inline or aliased
lambdas and builtin function values, String / Int / Bool / Null / Unit /
List<String>-accumulator direct or method-style `foldLeft` with inline or
aliased reducers, `split` / `join` with static or runtime string delimiters
on runtime strings, runtime `foreach`, `toString`, string concatenation, and
equality / `assertResult` checks against static string lists or other
runtime line lists. `FileOutput#writeLines` can write them back out, plus
`FileOutput#write` / `append` / `writeLines` / `exists` / `delete`,
`Dir#mkdir` / `mkdirs` / `delete` / `copy` / `move`, and `Dir#exists` /
`isFile` / `isDirectory` / `list` / `listFull`. Paths are copied into
NUL-terminated syscall path buffers at runtime, with runtime directory
listings exposed through the same sorted runtime line-list representation.

Direct `Dir#current()` emits runtime `getcwd`, so generated native
executables observe their execution cwd rather than the cwd used during
native build. `Dir#home()` reads the generated executable's `HOME`, and
`Dir#temp()` reads runtime `TMPDIR` with `/tmp` as its Linux fallback.

## Process, Environment, And Standard Streams

`CommandLine#args()` returns the generated executable's process arguments
excluding argv[0] as the same runtime line-list representation, including
direct, unqualified, aliased-helper, and function-local native calls.

`Process#exit(code)` emits a native process exit after evaluating the code
argument, so generated native CLI tools can return explicit status codes.

`StandardInput#all()` / `stdin()` read stdin into a fixed-buffer runtime
string, and `StandardInput#lines()` / `stdinLines()` expose stdin as the
same runtime line-list representation used by native file and argv helpers.
Both direct calls and immutable helper aliases are supported.

`Environment#vars()` / `env()` return the generated executable's environment
as `KEY=VALUE` runtime line-list entries through direct calls or immutable
helper aliases. `Environment#get(name)` / `getEnv(name)` read a single
variable value using static or runtime string keys, while
`Environment#exists(name)` / `hasEnv(name)` checks for one without failing
when it is absent.

## Streaming Reads And Runtime String Bindings

`println(FileInput#all(path))` / `println(FileInput#readAll(path))` streams
runtime file content without requiring the file to exist during native
build. Immutable runtime `FileInput#all(path)` / `readAll(path)` bindings
can be printed or concatenated through a fixed native string buffer,
compared with `==` / `!=` or `assertResult`, and queried with
`isEmptyString`, `length`, method-style `toString`, `substring` / `at` with
static or runtime integer indexes, ASCII-whitespace trimming, `repeat` with
static or runtime integer counts, ASCII case conversion, simple `matches`
with static or runtime patterns, first-occurrence `replace` with static or
runtime literal operands, all-occurrence `replaceAll` with static or runtime
pattern and replacement strings, UTF-8 `reverse`, `startsWith`, `endsWith`,
method-style `contains`, `indexOf`, and `lastIndexOf`. Oversized results
are reported as source-located runtime errors.

`FileInput#open` callback folding preserves mutable callback effects when
the callback's final value remains statically recoverable.

## Null, Unit, And Diagnostics

Static `null` is supported for immutable bindings, printing, ordinary
equality, and `Map#get` misses. `()` is supported for immutable bindings,
printing, static string concatenation, ordinary equality, and
`assertResult`. Static strings / lists / records / maps / sets / null /
unit support ordinary `==` / `!=` when both sides are statically known.

`ToDo()` emits the same `not implemented yet` runtime failure message from
a native executable.

Native runtime failures for `assert`, `assertResult`, `head([])`, negative
`sleep`, and negative string helper indexes / counts include source-location
prefixes on stderr. Failing `assertResult` messages keep conditional builtin
callable displays in step with normal printing. Native FileOutput syscall
failures report source-located stderr diagnostics instead of continuing
silently, as do Dir copy / mkdir / delete / move failures.

Unsupported constructs fail at build time instead of falling back to the
evaluator.

## Target Matrix

Native target support is tracked as tiers. Tier 0 is the only target
that must build a runnable executable today; tiers 1 and 2 are tracked
here so the work item is visible even before the target abstraction
lands. See `docs/roadmap-targets-stdlib.md` for the wider rationale.

| Target triple                  | Backend                | Artifact      | Tier | Status      |
| ------------------------------ | ---------------------- | ------------- | ---- | ----------- |
| `x86_64-unknown-linux-gnu`     | direct ELF writer      | executable    | 0    | supported   |
| `x86_64-unknown-linux-musl`    | direct ELF writer      | executable    | 1    | planned (Tier 0 alias once exposed) |
| `aarch64-unknown-linux-gnu`    | direct ELF writer      | executable    | 1    | planned     |
| `wasm32-wasi`                  | wasm module emitter    | wasm-module   | 2    | planned     |
| `x86_64-apple-darwin`          | portable C backend     | executable    | 2    | planned     |
| `aarch64-apple-darwin`         | direct Mach-O writer   | executable    | 1    | supported (small subset, growing) |
| `x86_64-pc-windows-msvc`       | direct PE64 writer (reuses DirectX86_64 codegen) | executable | 0 | supported (core language: arithmetic, if/while, recursive def, strings, enums/match, records, lists, closures, GC) |

Rules:

- Tier 0 must keep working at every commit. No PR can land if the
  `x86_64-unknown-linux-gnu` direct ELF path stops producing a runnable
  executable.
- Tier 1 targets stay on the direct backend family so that Klassic's
  "no external `cc` / `as` / `ld`" property is preserved. arm64 macOS
  (`aarch64-apple-darwin`) is tier 1 and uses a direct Mach-O arm64 backend
  (svc #0x80), not the portable C backend.
- Tier 2 targets reach the matrix through the portable runtime call
  path / C backend. x86_64 macOS still reaches the matrix through the
  portable C backend. `x86_64-pc-windows-msvc` is the one deliberate
  exception below tier 2: it reuses the entire `DirectX86_64` (Linux)
  code generator rather than going through the C backend or getting a
  dedicated codegen module like aarch64 -- only the OS boundary
  (syscalls -> Win64 `kernel32.dll` import-call shims) and the
  container format (PE64, see `crates/klassic-native/src/pe.rs`)
  differ. `TargetPlatform::syscall_number` still panics unconditionally
  for the Windows target as a deliberate invariant tripwire, but every
  native-codegen path that could reach it is gated first unless it has
  its own Win64 shim. `sleep`, `stopwatch`, and `Time#nowMillis`
  (`std.time`) are no longer gated (W1-c): `sleep` wraps `Sleep` and
  `Time#nowMillis` wraps `GetSystemTimeAsFileTime` (converting its
  100ns-tick FILETIME output to unix epoch millis with the well-known
  `116_444_736_000_000_000` 1601/1970 epoch offset), while `stopwatch`
  snapshots `QueryPerformanceCounter` before and after the timed body
  and divides the tick delta by `QueryPerformanceFrequency` to get
  elapsed milliseconds -- see `emit_win_sleep_runtime`,
  `emit_win_time_now_millis_runtime`, `emit_win_qpc_runtime`,
  `emit_win_qpf_runtime`, and `emit_win_elapsed_millis_qpc` in
  `crates/klassic-native/src/lib.rs`. The remaining gated paths --
  `StandardInput#all`/`StandardInput#lines` (`stdin()`/`stdinLines()`),
  the `FileOutput#`/`FileInput#` family including `std.file` and the
  `println(FileInput#all(...))`/`println(FileInput#lines(...))`
  print-fusion fast paths, the `Dir#` family including `std.dir`,
  `Environment#vars`/`#get`/`#exists` (`std.env`, and the `env()`/
  `getEnv()`/`hasEnv()` prelude aliases), and `CommandLine#args`
  (`args()`, `std.cli`, `std.process`) -- all fail to build for
  `x86_64-pc-windows-msvc` with a
  `` `Feature` is not yet supported when targeting x86_64-pc-windows-msvc ``
  diagnostic instead of reaching the panic. `Environment#*` and
  `CommandLine#args` are gated even though they don't reach a syscall
  at all, because their backing argv/envp slots are left zeroed on
  Windows (see `emit_store_command_line_state`) and would otherwise
  silently compile a binary that always observes empty arguments/
  environment rather than failing to build. `thread { ... }` needs no
  gate of its own -- a queued thread body is spliced into the tail of
  `main` and compiled through the same per-builtin gates as any other
  call site, so an OS-touching thread body is still caught, and a
  thread body that stays inside the core language still builds.
  Process spawning (`Process#run`/`nrun`) has no native-backend
  implementation on any target yet, so there is nothing Windows-specific
  to gate there. See `tests/cli_smoke.rs`'s
  `build_target_windows_x86_64_gates_*` tests for the still-gated
  coverage and `build_target_windows_x86_64_supports_*` /
  `build_target_windows_x86_64_sleep_and_stopwatch_run` /
  `build_target_windows_x86_64_time_now_millis_runs` /
  `build_target_windows_x86_64_format_iso_of_now_millis_runs` for the
  sleep/stopwatch/time coverage.
- An unsupported target must produce a `TargetSpec`-style diagnostic,
  not a panic.

When a target moves from `planned` to `supported`, add a row entry and
a one-paragraph note in the relevant Native Compiler Development
Pattern section describing the new emitter / runtime path.
