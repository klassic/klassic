# Klassic Implementation Notes

## Build And Execution

- Klassic is built as a native Rust workspace.
- The root package produces the `klassic` binary.
- The standard workflow is:

```bash
cargo build
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
cargo build --release
cargo run -- -e "1 + 2"
```

## Runtime Model

Klassic currently uses a direct evaluator. The evaluator runs after parsing,
rewriting, and typechecking, and it owns:

- runtime values
- lexical environments
- closures
- records
- modules and imports
- builtin functions
- typeclass dictionaries
- proof trust checks
- REPL/session state

This keeps the implementation straightforward while preserving user-visible
language behavior.

Klassic also has an initial native compiler path:

```bash
cargo run -- build --target linux-x86_64 program.kl -o program
cargo run -- build --target x86_64-unknown-linux-gnu program.kl -o program
cargo run -- build --target native program.kl -o program
```

That path now carries an explicit native target. `linux-x86_64` is the compact
Klassic name, `x86_64-unknown-linux-gnu` is accepted as the standard target
triple alias, and `native` resolves to the host target when the host is
implemented. The only implemented concrete target is Linux x86_64, which emits
ELF64 executables directly from Rust without invoking an external assembler,
linker, Java, Scala, sbt, or the JVM. The native compiler records each supported
target in a metadata registry with its compact name, standard triple,
architecture, operating system, ABI, and executable format, then keeps Linux
syscall numbers and OS ABI constants behind a target-platform boundary. Future
target work can change fds, open modes, errno values, stat masks, clocks, mmap
flags, and transfer limits without
editing expression codegen directly. It reuses the parser, rewrite pass,
typechecker, and
proof/trust checks, then emits
handwritten x64 for the subset it currently supports. That subset now
includes annotated boolean arguments and returns for native functions, simple
unannotated integer/boolean return inference, immutable static string/list
bindings, stack-passed arguments beyond the first six integer/boolean native
function parameters, call-site inlined unannotated pass-through and
string-literal concatenation `def`s over runtime `String` / `List<String>`
values even when only the return is annotated, temporary stack tracking for
saved operands and arguments,
streamed `println` / `printlnError` string interpolation, folded
interpolated strings when fragments depend on immutable static values, including
fragments with mutable block prefixes when their final values remain recoverable,
fixed-buffer `RuntimeString` interpolation when fragments include native runtime
strings or dynamic native `Int` / `Boolean` values,
compile-time folded static string helpers including `split` / `join`, static
string concatenation for immutable values, static record fields, and runtime
`String` / `List<String>`, dynamic `Int` / `Boolean`, and nested runtime record fields,
runtime string concatenation that formats dynamic native `Int` / `Boolean` and
runtime record operands,
annotated supported record parameters and returns using staged runtime field
storage across normal and recursive native calls,
static helper evaluation for calls such as `size`, `head`, `tail`, `join`, `Map#get`,
and method-style `parts.size()`, runtime-key `Map#get` selection from static maps
when the key is a runtime string/int/bool and the selected values are strings,
string lists, integers, booleans, or supported static records, immediate runtime-key calls through static
map callable values such as `Map#get(fns, key)(...)` and `fns.get(key)(...)`,
static-millisecond `sleep` through Linux
and runtime integer-millisecond `sleep` through Linux `nanosleep`,
zero-argument literal or lambda-value `stopwatch` through Linux `clock_gettime`,
queued `thread` bodies from literal or lambda-value jobs for the current native sample surface,
Int `abs` / `int` / `floor` / `ceil`, static Double/Float literal printing and
static numeric helpers such as `double`,
`sqrt`, `abs`, `floor`, and `ceil`, with Float literals kept at f32 precision
for evaluator-matching display and equality, including helper arguments whose
mutable block prefixes are emitted before recovering a static numeric result,
static integer-list `foreach` unrolling,
static integer list literals backed by native data labels including bitwise
integer expressions, list printing, and `size` / `isEmpty` / `head` / `tail` /
`contains` / static `cons` / static `map` / static
`foldLeft` for those static lists. Int-list `foreach` bindings are also
registered as static facts inside the unrolled body, allowing static lists,
records, maps, and sets to be built from the iteration value.
Static `if` expressions whose conditions fold to booleans can produce static
native values such as strings, lists, and records.
Simple mutable-loop effects are tracked for later static folds while generated
loop conditions remain dynamic.
Immutable aliases to directly supported builtin functions, for example
`val sub = substring`, are preserved as builtin references in the native
compiler so calls through the alias lower to the same static helper path.
Builtin function values also keep evaluator-style `<builtin:name>` display when
printed directly or inside static lists/records. Builtin values stored in static
record fields or lists can be called when their recovered arguments remain
static. Mutable builtin aliases can be rebound to compatible builtin aliases on
the straight-line static path.
Straight-line mutable static values such as strings and generic static lists can
be reassigned when the new value is also statically known; mutable function
values can likewise be rebound to static lambda, `def`, or builtin values along
the straight-line static path. The compiler rejects those aggregate/function
assignments inside dynamic `if` / `while` control flow until a heap-backed
runtime representation exists.
Static `map` folds single-argument integer lambdas over known static lists into
fresh data labels; static `foldLeft` folds two-argument integer reducers into a
native integer constant, and the generic static-list path also folds static
numeric/string reducers. Mapper/reducer callables may be direct lambdas,
placeholder-derived lambda aliases such as `val add = _ + _`, or top-level
functions such as `def inc(x) = x + 1`; lambda values returned by static
functions can also be bound, called, or passed onward as mappers when their
captures and arguments remain statically recoverable, and static lambda bodies
can call those returned closure values. Top-level lambda bindings and `def`
declarations remain printable function values in native executables, and direct
inline lambda calls can also receive runtime integer/boolean arguments by
compiling the lambda body at the call site; impure lambda bodies are not
statically folded, so native side effects such as mutable integer updates still
execute at runtime. Static
record lambda methods use the same recovery rule for effectful receivers and
arguments when their final values remain static. Static `if` conditions whose
final boolean values are recoverable can compile only the selected branch while
preserving condition side effects, so selected static aggregate branches no
longer require both branch labels to match. Dynamic-control assignments to
runtime integer/boolean locals still invalidate static facts for those variables.
`while` conditions whose final value is recoverably `false` emit condition
effects and skip compiling the dead body, matching evaluator behavior for
unreachable unsupported native constructs. Dynamic `while` loops that cannot be
fully simulated also invalidate static facts for locals assigned in the loop
condition or body before later folds run, and runtime-list locals assigned in
those loops are materialized into mutable selected-length storage.
Static binary folds for numeric, equality, and string-concatenation expressions
use the same guard. Numeric Float/Double binary expressions and string
concatenation can still preserve mutable block-prefix effects when operands
ultimately yield static values; non-static string concatenation formats dynamic
native `Int` / `Boolean` operands into fixed runtime strings. Native `toString`
uses the same fixed-buffer path for dynamic `Int` / `Boolean` values and falls
back to evaluator-style display for static-native values that survive
dynamic/effectful evaluation. Logical
`&&` and `||` keep runtime short-circuiting and do not let skipped RHS static
mutations leak into later folds. Static call
folding, inline-call static argument capture,
and `assertResult` folding also require pure arguments, so side-effecting
argument blocks stay on the generated runtime path. Immutable aliases to
curried helpers such as `assertResult`, `cons`, `map`, and `foldLeft` resolve
through the same native special-call paths as direct helper calls.
Static lambda values, including mutable aliases that are rebound on the
straight-line path, can be called with runtime integer/boolean arguments by
inlining the captured body instead of requiring a pure compile-time fold.
Non-identifier callee expressions that evaluate to a static lambda or builtin
function value now keep their callee-side effects before the returned callable is
applied, including side-effecting builtin function values such as `println`,
`sleep`, `assert`, queued `thread`, and File/Dir helper values that mutate or
query the native compiler's virtual file-system state. The same callee-effect
preservation applies when the returned builtin value is used as the first stage
of a supported curried helper such as `assertResult`, `cons`, `contains`, `map`,
or imported `Set#contains`, and for three-stage `foldLeft`.
Collection and Map/Set helper builtin values such as `size`, `head`, `tail`,
`isEmpty`, `contains`, `Map#get`, and `Set#contains` also reuse the same static helper path
when they are called through an effectful value-producing callee.
String and line-list helper builtin values such as `toUpperCase`, `length`,
`split`, and `join` similarly reuse the runtime helper paths, so effectful
callee expressions can feed runtime strings or runtime line lists into them
without falling back to static argument recovery.
Returned lambdas also carry native slots for captured runtime locals. If a
block, inline lambda, or call-site inlined function returns a lambda that
captures one of its stack slots, native codegen preserves that allocation so
block/function-local mutable closure state is shared across later calls.
Fixed-buffer runtime string and line-list captures are treated as runtime
captures even when their binding is stored as a constant data label, so closures
that observe later assignments are inlined instead of being statically folded.
Native variable slots also carry a binding identity, letting lambda rebinding
reuse a later value of the same mutable binding without confusing it with an
inner shadowing declaration of the same name.
Unannotated functions whose return value is a runtime-capturing lambda are
therefore compiled by inlining at the call site instead of forcing an integer
return ABI. Returned static aggregates are inspected recursively too, which
allows a record of closures to share the same block-local mutable slot.
Annotated `String` and `List<String>` parameters on scalar-returning native
functions use function-local fixed runtime buffers. Call sites copy static or
runtime strings plus static or runtime line lists into those buffers, then pass
only scalar `Int` / `Bool` parameters through the ordinary register/stack ABI;
this lets simple self-recursive scanners such as `countA(s: String, i: Int): Int` and
`countLines(lines: List<String>, i: Int): Int` compile without call-site
inlining. Native codegen first copies each `String` or `List<String>` argument
into a staged runtime buffer, finishes evaluating the remaining arguments, then
copies staged values into the shared parameter buffers immediately before the
scalar call ABI. This keeps reentrant calls and self-recursive rewrites from
overwriting the callee's shared parameter buffer before later arguments have
observed the old value.
Top-level lambda declarations and inline lambda calls use the same annotated
`String` / `List<String>` parameter matching, but bind the actual call-site value
directly because their bodies are emitted at the call site.
Mutable native `String` and `List<String>` bindings are represented as fixed
runtime buffers. Initial values and later assignments copy static or runtime
string/line-list content into those buffers, so dynamic loops can maintain
string accumulators and consume runtime line-list cursors without changing the
binding's native value shape.
Annotated `String` / `List<String>` returns use function-owned return buffers;
direct call sites immediately copy those buffers into call-site-local buffers so
neighboring calls can be composed without clobbering each other.
Function value aliases, static record fields, direct or method-style `head`
lookups from static lists including `tail` and `cons` chains, and static
`Map#get` / `.get` lookups with literal or folded static keys created from those
`def`s retain the annotated runtime return metadata, allowing aliased calls to
be recognized by string concatenation, dynamic string-branch merging, and
runtime line-list helpers.
Block, cleanup, and same-runtime-return conditional callees carry the same
return hint for immediate calls that feed those contexts.
Immediate calls on conditional function values are lowered from
`(if (cond) f else g)(args...)` to branch-local calls, preserving argument
evaluation on the selected path while reusing dynamic `if` result buffers for
runtime string and line-list returns.
Pure conditional callable branches used in immutable bindings or static
aggregate elements evaluate and save the condition once, then produce a
synthesized static lambda whose body performs the branch-local call. This keeps
`val f = if (cond) a else b; f(x)` and `[if (cond) a else b].head()(x)` usable
for runtime-returning functions and supported builtin function values with
matching arity without adding a general heap function value representation yet.
For conditional builtin branches, the synthesized callable also keeps display
metadata so printing, interpolation, string concatenation, and `toString` emit
the selected branch's `<builtin:name>`, even after the callable is returned from
a function or nested inside a static aggregate. Bound interpolation strings and
string concatenations that include such aggregates, including map keys and set
elements, use the runtime string buffer rather than freezing a `<function>`
placeholder at native build time.
Queued native `thread` bodies use the same capture metadata, so a thread queued
inside a block can still mutate and observe that block's captured mutable locals
when the queued body is emitted later. `thread` itself can queue zero-argument
lambda values as well as literal lambdas. `def` and static lambda bodies that queue
threads through direct calls or immutable aliases are compiled by call-site
inlining / effectful lambda execution so their queued work is emitted with the
caller current execution flow instead of being stranded during later function
emission or folded away as a pure static result.
Top-level `def` bodies that close over top-level bindings are also call-site
inlined for non-recursive functions. Recursive functions can capture immutable
static top-level values, including builtin aliases and static lambda values, and
immutable runtime string, line-list, selected-length runtime-list, and runtime-record
bindings by rebinding those captures inside the emitted function body. Direct calls and value
aliases for user-defined functions now shadow same-named native builtins on the
native path.
Static equality and `assertResult` over aggregate values preserve
side-effecting expected/actual expressions while comparing recovered values,
including equality between compact static Int-list values and generic static
lists whose elements are all Ints. The same effect-preserving recovery covers
mixed numeric equality such as side-effecting `Int` versus `Double`/`Float`
comparisons. User-visible equality follows the evaluator: function and
builtin-function values compare false even when they are the same native static
function value. Native branch merging still uses separate structural equality
for function values so equivalent closures can survive control-flow joins
without leaking that behavior into `==`.
Cleanup expressions preserve their body result while still emitting cleanup
effects, including runtime integer/boolean body values that must survive cleanup
register clobbering.
Dynamic `if` branches are compiled with isolated native compiler state and then
merged only when mutable/static variables and virtual File/Dir facts have the
same representable value on both paths. This keeps identical static aggregate
returns, assignments, and virtual file contents usable after a runtime branch
while also allowing divergent native string and runtime line-list branch results
to flow through shared fixed runtime buffers. Divergent static string-list
branches can also merge with each other or with runtime line-list branches by
materializing both sides into the same fixed line-list buffer. Fixed-shape
runtime-list branches can also merge by copying compatible runtime-list or
static-list values into shared per-element storage, including supported record
fields that carry runtime-list values. Structurally equivalent branch-local
lambda values plus canonical builtin function values can merge. If
equivalent returned closures
capture branch-local mutable slots, both branches must preserve the same stack
depth; that preserved depth is carried past the join so later closure calls keep
using the captured storage. Lambda equality compares only captures referenced by
the body, so returned records/lists/maps of closures do not become unequal merely
because their unused static environments differ. Equivalent queued thread bodies
use the same structural comparison and preserved-stack join rule, allowing
branch-local thread captures when both dynamic branches queue the same work.
Divergent
aggregate/function state, divergent file state needed by a later static read,
and divergent native thread queues are still rejected.
For static-list `map` and `foldLeft`, native codegen can now unroll lambdas
whose prefix expressions mutate runtime locals, then recover the final static
lambda result when the last expression remains statically known. Method-style
`xs.map(f)` and `xs.foldLeft(initial, reducer)` lower into the same native path.
Some static string/Map/Set helper calls can still fold their final helper result
after emitting impure argument blocks, as long as the resulting argument values
are recoverable statically.
Static `join`, FileInput/FileOutput path/content/list arguments, and Dir path
arguments use the same side-effect-preserving argument recovery path. Static
`cons` head and tail arguments use that path as well, so list construction does
not fold away mutable block effects. Static list, map, set, record literal, and
record constructor arguments use the same recovery path.
Builtin module aliases and selective imports now resolve in the native compiler,
so `import Map as M`, `M#size(...)`, `import Set as S`, imported helper names,
and helper value aliases such as `val readAll = FI#readAll` share the same
native helper implementations as their canonical module names.
Fold-like three-stage
curried calls over static lists, direct
static typeclass methods, and List `bind` / `unit` calls are also folded when
their lambdas and inputs are static.
Static record lambda fields keep their captured static bindings, which lets
dictionary-passing typeclass examples such as
`Show_List_dict(Show_Int_dict).show([1, 2, 3])` fold through captured dictionary
records, curried `map`, and `join`. Static non-integer lists are represented in a
compile-time arena and support printing, `size`, `isEmpty`, `head`, `tail`,
`contains`, `join`, static `foreach` unrolling, static `map` over static mappers, static
`foldLeft` over static accumulator reducers such as string concatenation, and
`assertResult`. Static `cons` supports generic static lists as well as compact
static integer lists, and Int-list `foldLeft` can now fall back to the generic
static reducer path for list-building reducers such as `e #cons acc`. Static nominal and
structural records whose fields are static native values now support native
construction, field selection, printing, static lambda method fields, and
`assertResult` equality. Record literals and constructors can also carry
fixed-buffer runtime `String` / `List<String>`, dynamic `Int` / `Boolean`, and
nested runtime record fields for field selection, printing, and equality against compatible
records, plus runtime string display through `toString`, interpolation, and
concatenation. Dynamic `if` expressions can copy compatible runtime record branch
results into shared runtime field storage, and mutable runtime record bindings
reuse that field storage for compatible assignments from runtime or supported
static-record initializers. Annotated record function parameters and returns
reuse compatible field storage for normal and recursive native calls. Runtime
line-list `foldLeft` can update record accumulators through the same storage,
including record fields initialized with an empty `List<String>`. Static map
and set literals are also represented as compile-time arenas; maps preserve
entry order, and sets de-duplicate values in the same first-occurrence order as
the evaluator. Static map/set helpers cover `Map#size`, `Map#isEmpty`,
`Map#containsKey`, `Map#containsValue`, `Map#get`, `Set#size`, `Set#isEmpty`,
and `Set#contains` when both the collection and query value are static. Static
string-key maps, string-valued maps, string sets, and scalar list/set/map
entries also support runtime string, int, and boolean membership queries for
`contains`, `Map#containsKey`, `Map#containsValue`, and `Set#contains`.
Static string-list entries can be matched against runtime line-list query values
through the same membership helpers.
Runtime records copied out of static map lookups can also be matched
structurally against static record entries through static list/set `contains`
and map `containsValue`.
Effectful query expressions that compile back to static values after preserving
their effects are also compared through the static collection membership path.
Map literal `Map#get` / `.get` can return runtime native values, including
supported runtime records and runtime-list values, from static or runtime keys
after evaluating every map entry and the key in source order. Runtime-list
outputs can keep a runtime-selected length, so display, `size`, `isEmpty`,
`head`, `tail`, `contains`, `cons`, `foreach`, `map`,
scalar/string/line-list/record `foldLeft`, `join`, and equality ignore unused
capacity slots.
Direct `head` over list literals can likewise return runtime native values
after evaluating every list element in source order.
Direct `tail` over list literals can return runtime line-list values from
runtime string elements, or runtime-list values for non-string tails, after
evaluating every list element in source order.
List/set literal `contains` and map literal `containsKey` / `containsValue` can
compare runtime native values directly after evaluating the literal and query
in source order.
Literal list/map/set `size` and list/map/set `isEmpty` selectors likewise
preserve literal effects and return cardinality or emptiness without
materializing runtime values; set literal `size` counts only distinct runtime
values.
List literal `foreach` evaluates every element first, then unrolls the loop body
over the compiled native values.
List literal `map` uses the same evaluated native values to produce runtime
line-list results when every mapper result is string-compatible, or runtime-list
results for non-string mapper outputs.
List literal `join` uses the same evaluated native values to join runtime
string elements into a runtime string.
List/map/set literal display / `toString` uses the same evaluated native values
to render supported element, key, and value displays into runtime strings, and
the same formatter feeds printing, interpolation, and string concatenation.
List/map/set literal equality and `assertResult` reuse the evaluated literal
slots to compare runtime native elements, keys, and values against compatible
literal expectations or static collection bindings.
Immutable runtime list literal bindings store the same evaluated slots behind a
runtime-list label after copying runtime string, line-list, and record elements
into list-owned buffers. The native path can then print or render those runtime
lists, compare them with compatible static or runtime lists, and feed them
through `head`, `tail`, `size`, `isEmpty`, `contains`, `cons`, `foreach`,
`map`, `foldLeft`, `join`, and `FileOutput#writeLines`.
Runtime-list `foldLeft` can also update compatible `List<String>` accumulators,
including reducers that produce a fresh list with `cons`; those reducer results
are copied back through the same fixed line-list accumulator buffer.
When a runtime-list label carries a selected runtime length, native helpers
preserve that prefix and do not expose unused capacity slots. Runtime-list
accumulators for `foldLeft` also work over variable-length sources by copying
the previous accumulator storage through skipped iterations.
Runtime records can also carry runtime-list fields through field access,
display, and equality against compatible static record fields.
Dynamic `if` joins can merge fixed-capacity runtime-list values and record
fields that carry them by copying each branch into shared runtime-list element
storage, preserving selected runtime lengths whenever a runtime-list branch
participates in the merge.
Annotated `List<String>` function parameters and returns can consume compatible
runtime-list values by copying the selected string-element prefix into fixed
line-list buffers, which also lets supported record returns carry runtime-list
fields through the annotated record ABI.
Mutable bindings can rebind runtime-list values in straight-line native code,
including `cons` and `tail` chains. Dynamic `while` bodies materialize assigned
runtime-list locals into mutable storage with a selected-length slot, so each
assignment can update the visible list length within the materialized capacity.
List literal `foldLeft` uses the same evaluated native values to reduce into
supported scalar, string, line-list, runtime-list, or record accumulators.
Static maps also support runtime string, int, and boolean `Map#get` / `.get`
keys when the compatible entries return all strings, all string lists, all ints,
all booleans, all supported static records, all non-string static lists, all
`null`, all `()`, or equivalent static values, including the same callable
value. Non-string list values may have different static lengths; the selected
length is copied into runtime-list storage, including nested record fields.
Runtime misses fail with a source-located native diagnostic when the selected
value must be materialized because the native path still has no dynamic tagged
`null` value, but direct `Map#get(...) == null` / `!= null` checks lower to
key-match tests over static maps and map literals. A runtime key whose type has
no compatible static keys returns static `null`; all-`null` compatible values
also return static `null` for both hits and misses. If the
compatible entries are all callable values, immediate calls through that lookup
dispatch to the selected lambda or builtin branch and merge the same supported
native return shapes; runtime string/int/bool-key lookups can also be bound to
immutable values, called later, and formatted through printing, interpolation,
string concatenation, or `toString` with the selected callable display through
the same dispatch path; equality involving those function values follows the
evaluator's always-false function comparison semantics. Recursive functions may
rebind frame-independent callable dispatch captures inside their emitted
function body, so top-level string/int/bool-key selected callables can be used
from self-recursive functions. Static
`null` is supported for immutable bindings, printing, equality, and `Map#get`
misses; `()` is supported for immutable bindings, printing, static string
concatenation, equality, and `assertResult`. `ToDo()` emits a native runtime failure with the evaluator's
`not implemented yet` message with the source location prefix. Native
`assert(false)` and failing `assertResult` checks now write source-located
runtime diagnostics to stderr before exiting non-zero, with conditional builtin
callable displays rendered through the same dynamic print path as normal
output; `head([])` follows the same executable-runtime diagnostic path instead
of failing the native build.
Negative `sleep` millisecond values also emit evaluator-style runtime
diagnostics from the generated executable.
Negative string helper indexes/counts for `substring`, `at`, and `repeat` also
emit evaluator-style runtime diagnostics from the generated executable.
FileOutput open/write syscall failures likewise write source-located diagnostics
instead of letting a generated executable continue silently. Dir mkdir/delete/move
syscall failures do the same, while `Dir#mkdirs` still tolerates already-existing
directories and `FileOutput#delete` still tolerates a missing file like the
evaluator. Runtime string values can be used as syscall paths for
`FileInput#all`, direct file-input printing, and `FileInput#open` callback
bodies or callable callback values whose stream parameter flows through
supported runtime string and file helpers, including `readAll` / `readLines`,
`length`, `cleanup`, or returning the path itself. Fixed-buffer runtime strings
can be copied onto the GC heap with `__gc_string(runtimeString)`, providing an
explicit bridge from runtime file/stdin data into `HeapString` operations.
Heap string operands also participate in native `+`, using the same
shadow-stack-rooted concatenation path as `__gc_string_concat` and lifting
static or runtime string fragments when needed.
Native `==` / `!=`, `assertResult`, and `__gc_string_eq` over heap strings
root the left operand while the right-hand side is evaluated, then compare
length-prefixed byte payloads through the same scan path.
`toString(heapString)` copies heap bytes back into a fixed-buffer
`RuntimeString`, and `heapString.toString()` uses the same bridge, letting
ordinary `String` helpers consume explicitly heap-backed data. Runtime string
interpolation appends `HeapString` fragments into the same fixed-buffer
representation.
High-level collection literals reject `HeapPointer` / `HeapString` values for
now instead of preserving them in unrooted fixed-buffer runtime-list metadata;
the explicit `__gc_list_ptr_*` helpers remain the native path for collections
of heap pointers.
Address-taking GC helper calls also reject plain `Int` native values at build
time; pointer-producing expressions keep their `HeapPointer` / `HeapString`
native value tags even though the temporary source-level debug API is still
typed through integers.
Raw `__gc_write` accepts `Int`, `HeapPointer`, and `HeapString` values as qwords,
so pointer records and arrays can store heap strings through the same field path
used for generic heap objects.
Raw `__gc_read` remains an integer qword load; `__gc_read_ptr` emits the same
load but returns `NativeValue::HeapPointer`, so pointer-record fields can flow
back into strict address-taking GC helpers without reopening arbitrary plain
`Int` addresses.
`__gc_read_string` is the string-specific companion that returns
`NativeValue::HeapString`, letting a raw field re-enter natural heap-string
printing, `+`, `toString`, and `assertResult` paths.
`__gc_list_ptr_get_string` provides the same string-specific projection for
tag-4 pointer lists whose slots are known to contain heap strings.
`__gc_smap_get_string` applies that convention to string-keyed maps, preserving
`NativeValue::HeapString` for present values whose payloads are heap strings.
Direct printing or immutable
printable bindings of `FileInput#lines` / `readLines` are also supported.
Runtime line-list values also support `size`, `isEmpty`, `head`, `tail`,
`cons`, `contains`, inline-lambda `map` or aliased-lambda /
builtin-function-value `map` that produces string line lists,
String/Int/Bool/Null/Unit/List<String>-accumulator direct or method-style `foldLeft` with inline or aliased reducers,
`split` / `join` with static or runtime string delimiters on
runtime strings. Runtime `split` preserves empty-input, leading, consecutive,
and trailing empty fields for non-empty delimiters and splits UTF-8 code point
byte groups for empty delimiters. Runtime line lists also support runtime
`foreach`, `toString`, string concatenation, and equality / `assertResult`
checks against static string lists or other runtime line lists, and
`FileOutput#writeLines` can write runtime line-list values back out,
`FileOutput#write` / `append` / `writeLines` / `exists` / `delete`,
`Dir#mkdir` / `mkdirs` / `delete` / `copy` / `move`, and `Dir#exists` / `isFile` /
`isDirectory` / `list` / `listFull`; runtime directory listings use the same
runtime line-list representation and are sorted to match static/evaluator
directory listing order, and native code copies those values into
NUL-terminated path buffers before invoking Linux syscalls. Direct
native printing of `FileInput#all(path)` and
`FileInput#readAll(path)` streams runtime file content, so those print paths no
longer require the file to be present during native build. Immutable native
bindings of runtime `FileInput#all(path)` / `FileInput#readAll(path)` can also
be printed or used in string concatenation through a fixed runtime string
buffer, compared for equality/inequality including `assertResult`, and queried
with `isEmptyString`, UTF-8 code-point `length`, UTF-8 code-point `substring`
and `at` with static or runtime integer indexes, method-style `toString`,
ASCII-whitespace `trim` / `trimLeft` / `trimRight`, `repeat` with static or
runtime integer counts, ASCII
`toLowerCase` / `toUpperCase`, first-occurrence `replace` with static or
runtime literal operands, all-occurrence `replaceAll` with static or runtime
pattern and replacement strings, simple `matches` with static or runtime
patterns, UTF-8 `reverse`, `startsWith`, `endsWith`, or
method-style `contains`, plus `indexOf` / `lastIndexOf` byte-offset searches;
oversized results fail at runtime with a source-located diagnostic instead of
being silently truncated. `Dir#copy` now uses runtime
`open`/`sendfile` for non-virtual source files instead of requiring the source
to be readable at native build time.
Static strings use the same runtime slice emitter for `substring` / `at` when
their index expressions are mutable or otherwise dynamic integers, so loops can
walk known strings without requiring every index to fold at build time.
Static string `split` and static string-list `join` likewise route through
runtime buffers when their delimiters are runtime strings.
Static first-occurrence `replace` uses the dynamic replacement emitter when its
pattern or replacement operand is a runtime string.
All-occurrence `replaceAll` can route static or runtime pattern and replacement
strings through a dynamic emitter that selects the supported `[0-9]`, empty, or
literal pattern path at runtime.
Static `repeat` uses the dynamic repeat emitter when its count is a runtime
integer.
`Dir#move` likewise treats non-virtual runtime moves as unknown File/Dir state
after emitting the rename syscall, preventing later native folds from using
stale build-time filesystem facts.
`CommandLine#args()` reads the generated executable's process arguments at
runtime, excludes argv[0], and exposes them as runtime line lists for direct,
unqualified, aliased-helper, and generated-function native calls.
`Process#exit(code)` evaluates its code argument before emitting the Linux
`exit` syscall, giving native CLI tools explicit success/failure statuses.
`StandardInput#all()` / `stdin()` read stdin into fixed-buffer runtime strings,
while `StandardInput#lines()` / `stdinLines()` expose stdin through the same
runtime line-list representation used by file input and argv helpers, including
immutable helper aliases.
`Environment#vars()` / `env()` walk the saved process envp table and expose
`KEY=VALUE` runtime line-list entries to generated native programs through
direct calls or immutable helper aliases.
`Environment#get(name)` / `getEnv(name)` and `Environment#exists(name)` /
`hasEnv(name)` scan that table for direct lookup and existence checks with
static or runtime string keys.
Native
`assertResult` compares integers and booleans at runtime, and compares static
strings/lists/records/maps/sets/null/unit through the known native data sections
and compile-time arenas. Ordinary
`==` / `!=` also works for static strings/lists/records/maps/sets/null/unit and
for runtime integer/boolean values. The ELF writer places the data segment
after the emitted text segment with page alignment instead of assuming a fixed
4 KiB text budget.
Static-path `FileInput` / `FileOutput` helpers use Linux file syscalls for
write/append/delete side effects and compile-time virtual file tracking for
native static reads. `FileOutput#write` / `FileOutput#append` can also write
fixed-buffer runtime string content and then mark that path's native virtual
file state as unknown. Static-path `FileInput#open` callback bodies and callable
callback values bind the stream path before normal native compilation, so they
may return supported runtime values as well as folded static values. Unknown
paths fall back to runtime `FileInput#all`, `FileInput#lines` / `readLines`,
`FileOutput#exists`, `Dir#exists`, `Dir#isFile`, `Dir#isDirectory`, `Dir#list`,
and `Dir#listFull` syscalls instead of reusing stale build-time facts. Runtime
string paths are accepted for the same read/existence/type-check operations and
for FileOutput write/append by copying the path bytes into a bounded
NUL-terminated runtime buffer; writeLines,
delete, mkdir/mkdirs, list/listFull, copy, and move use the same path-buffer path.
Runtime file-input bindings are
represented as fixed-buffer native string values when the file is not available
at build time. These values
support printing, string concatenation, equality/inequality, and
`isEmptyString` / `length` / method-style `toString` / `substring` / `at` with
static or runtime integer indexes / ASCII-whitespace trimming / `repeat` with
static or runtime integer counts / ASCII case conversion / first-occurrence
`replace` with static or runtime literal operands / all-occurrence `replaceAll`
with static or runtime pattern and replacement strings / simple `matches` with
static or runtime patterns / UTF-8 `reverse` /
`startsWith` / `endsWith` / method-style `contains` / `indexOf` /
`lastIndexOf`, while direct print streaming handles arbitrary-size
runtime output. `FileInput#open` callback
folding preserves mutable callback effects, including cleanup clauses, when the
callback's final value remains statically recoverable. `Dir#current()` emits
runtime `getcwd`, `Dir#home()` reads runtime `HOME`, and `Dir#temp()` reads
runtime `TMPDIR` with `/tmp` as its Linux fallback, all returning fixed-buffer
runtime strings. Static-path `Dir` helpers cover existence/type checks,
mkdir/mkdirs, list/listFull, delete, copy, and move with Linux syscalls plus the
same compile-time virtual filesystem tracking used by static reads.
The sample-program test harness also builds the required top-level `.kl`
programs with the native compiler on Linux x86_64 and compares native
stdout/stderr against the existing golden expectations whenever those fixtures
define one. Promoted future-feature programs and the checked-in typeclass
examples are also native-built and executed by the Rust integration tests.
Recursive functions that still require call-site inlining, such as recursive
functions with unsupported flexible native parameter or return representations,
are first given a chance to fold when all call arguments are static, so pure
recursive helpers over static lists can still compile, including helpers that
return static lists through curried `cons` and helpers that take static callable
arguments. Calls that cannot fold are rejected with a normal compile diagnostic
instead of recursively inlining until the compiler stack overflows.
Unsupported constructs fail with compile diagnostics; they do not silently fall
back to the evaluator.

## Type System

The Rust typechecker implements:

- immutable-binding generalization and instantiation
- type annotations
- numeric compatibility checks
- nominal record schemas
- structural record literals and structural record type annotations
- row-polymorphic field constraints
- contextual lambda checking at function-call sites
- strict propagation of resolved polymorphic result types into annotations
- constrained-polymorphic function declarations such as
  `def display<'a>(x: 'a): String where Show<'a> = show(x)`
- instance declarations with requirements such as
  `instance Show<List<'a>> where Show<'a>`
- higher-kinded typeclass examples over `List`
- theorem / axiom proposition checks and trust-level analysis

## Runtime Surface

The runtime supports:

- `println` and `printlnError`
- numeric helpers such as `sqrt`, `int`, `double`, `floor`, `ceil`, and `abs`
- string helpers such as `substring`, `at`, `matches`, `split`, `join`,
  trimming, replacement, case conversion, `contains`, `indexOf`, `repeat`, and `reverse`
- list, map, and set helpers
- assertions and `ToDo`
- timing helpers
- real Rust threads with synchronized mutable capture snapshots
- file and directory modules using portable Rust filesystem APIs

## REPL

The REPL supports:

- `:exit`
- `:history`
- multiline buffering on incomplete input
- persisted value bindings
- persisted record schemas
- persisted generalized binding schemes and structural record shapes

## Proof Trust

Trust diagnostics are deterministic:

- `--warn-trust` reports trusted proof dependencies.
- `--deny-trust` rejects proof graphs with trusted ancestors.
- Trusted dependencies are reported in source order when multiple candidates exist.

## Future Work

- Move shared runtime components from `klassic-eval` into `klassic-runtime` as
  boundaries become clearer.
- Extend the native compiler from its current integer/control-flow/function/list
  and static collection/record vertical slice to full evaluator parity.
- Expand the proof language beyond the current theorem/trust surface.
- Add optimizer or bytecode work only if it improves measurable runtime or
  debugging needs.
