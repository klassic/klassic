# Repository Guidelines

## Project Structure & Module Organization
- SBT-based Scala 3 project with some Java runtime helpers.
- Source: `src/main/scala/com/github/klassic/...`, Java runtime in `src/main/java/klassic/runtime`.
- Tests: `src/test/scala/...` using ScalaTest; fixtures in `src/test/resources`.
- Benchmarks: `src/jmh/scala/...` (JMH).
- Examples and scripts: `examples/`, `test-programs/`, and `.kl` files at repo root.

## Build, Test, and Development Commands
- Prerequisites: JDK 8+ (11+ recommended) and `sbt`.
- Compile: `sbt compile` — compiles main sources.
- Run REPL: `sbt console` — opens Scala REPL with useful imports preloaded.
- Run tests: `sbt test` (single suite: `sbt "testOnly com.github.klassic.TypeCheckerSpec"`).
- Assemble fat jar: `sbt assembly` — outputs `target/scala-3.3.6/klassic.jar`.
- Run interpreter: `java -jar target/scala-3.3.6/klassic.jar -e "println(1+2)"` or with a file.
- Benchmarks: `sbt "jmh:run -i 3 -wi 3 -f1 .*Basic.*"`.

## Coding Style & Naming Conventions
- Language: Scala 3.3.6 (plus minimal Java). Use 2-space indentation, standard Scala brace style.
- Naming: packages lowercase; types/objects `CamelCase`; methods/vals `lowerCamelCase`; constants `UPPER_SNAKE_CASE`.
- Organization: match package paths (e.g., `com.github.klassic` → `src/main/scala/com/github/klassic`).
- Prefer immutable `val` and pure functions; keep side effects explicit.
- Formatting: no enforced formatter in repo; keep diffs minimal and follow surrounding style.

## Testing Guidelines
- Framework: ScalaTest (`AnyFunSpec` with `describe`/`it` and `Diagrams`).
- Location: `src/test/scala/...`; name files `*Spec.scala` or `*Test.scala`.
- Add tests for behavior changes (parser, typer, runtime); include negative tests where relevant (`intercept[...]`).
- Run fast locally with `sbt test`; target a suite via `testOnly` while iterating.

## Commit & Pull Request Guidelines
- Commits: imperative mood, concise subject (<72 chars), scope prefix when helpful (e.g., `parser: handle underscores`).
- PRs: clear description, rationale, and before/after examples; link issues; include tests and doc updates.
- CI must be green (`sbt compile test assembly` as needed). Avoid large, mixed refactors.

## Security & Publishing Notes
- Do not commit secrets. Sonatype credentials are read from `~/.ivy2/.credentials` or `~/.m2/settings.xml` (see `build.sbt`).
- Verify no sensitive data lands in sources, tests, or example programs.

## Agent-Specific Instructions
- Keep changes surgical; preserve file layout and naming.
- Prefer `rg` for search; run `sbt test` before/after modifying core logic.
- Avoid repository-wide reformatting; match existing style in touched files.

