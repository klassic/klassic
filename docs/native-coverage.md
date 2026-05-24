# Native Compiler Coverage

This document enumerates the language constructs the native compiler currently
lowers to direct ELF for Linux x86_64, selectable as `linux-x86_64`,
`x86_64-unknown-linux-gnu`, or `native` on a matching host. Anything not listed
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
continues to model scalar qword reads as `Int`.

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
