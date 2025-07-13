# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Klassic is a statically typed programming language with:
- Hindley-Milner type system with row polymorphism
- Object-functional programming paradigm  
- Built-in Java FFI (Foreign Function Interface)
- Higher-kinded types and typeclass support
- Written in Scala 3, using SBT as build tool

## Build Commands

```bash
# Compile the project
sbt compile

# Run all tests
sbt test

# Run tests excluding slow tests
sbt "testOnly -- -l 'slow'"

# Run specific test suite
sbt "testOnly com.github.klassic.ExpressionSpec"

# Run tests matching pattern
sbt "testOnly com.github.klassic.*TypeClass*"

# Build executable JAR
sbt assembly

# Run REPL
sbt run

# Run a Klassic program
java -jar klassic.jar -f program.kl

# Evaluate expression
java -jar klassic.jar -e "println(1 + 2)"

# Run benchmarks
sbt "jmh:run"                              # Run all benchmarks
sbt "jmh:run VmBasicOperationsBenchmark"   # Run specific benchmark
sbt "jmh:run -f 1 -i 1 -wi 1"              # Quick benchmark run
```

## Architecture

### Core Compiler Pipeline

The language implementation follows this pipeline:

1. **Parser** (`Parser.scala`) - Parser combinators that produce AST
2. **PlaceholderDesugerer** (`PlaceholderDesugerer.scala`) - Desugars placeholder syntax (`_`)
3. **SyntaxRewriter** (`SyntaxRewriter.scala`) - Rewrites syntax (e.g., `foreach` to `while`)
4. **Typer** (`Typer.scala`) - Type inference using extended Hindley-Milner
5. **TypeClassTransformer** (`TypeClassTransformer.scala`) - Transforms typeclasses to dictionary passing
6. **VmInterpreter** (`vm/VmInterpreter.scala`) - Stack-based VM interpreter

### Key Components

- **AST** (`Ast.scala`) - All AST node definitions including `TypeClassDeclaration`, `InstanceDeclaration`
- **Type System** (`Type.scala`) - Type definitions including kinds for higher-kinded types
- **Built-in Environment** (`BuiltinEnvironments.scala`) - Global functions and module definitions
- **Module System** - Modules in `moduleEnvironments`: List, Map, Set, FileInput, GPIO
- **VM** (`vm/` directory) - VM instructions, compiler, and interpreter

### Testing Structure

- Test framework: ScalaTest with FunSpec style
- Test files: `src/test/scala/com/github/klassic/*Spec.scala`
- Test programs: `test-programs/*.kl`
- Helper: `SpecHelper.scala` provides `E()` for evaluation testing
- JMH benchmarks: `src/jmh/scala/com/github/klassic/benchmarks/`

## Language Features

### Type System
- Hindley-Milner type inference with row polymorphism
- Higher-kinded types (e.g., `List<*>`, `Map<*, *>`)
- Typeclass support with `where` syntax
- Record types with structural typing

### Syntax Features
- Space-sensitive list/map/set literals: `[1 2 3]`, `%["a":1 "b":2]`, `%(1 2 3)`
- String interpolation: `"Hello #{name}"`
- Placeholder syntax for anonymous functions: `map([1 2 3])(_ + 1)`
- Cleanup expressions for resource management

### Pragmatic Features (New)
- **FileOutput Module**: `write`, `append`, `writeLines`, `exists`, `delete`
- **Dir Module**: `list`, `mkdir`, `mkdirs`, `exists`, `isFile`, `isDirectory`, `copy`, `move`, `current`, `home`, `temp`
- **Enhanced String Functions**: `split`, `join`, `trim`, `replace`, `toLowerCase`, `toUpperCase`, `startsWith`, `contains`, `indexOf`, `length`, `reverse`

### Recent Additions
- Pragmatic file I/O and directory operations
- Comprehensive string manipulation utilities
- Higher-kinded typeclasses with kind system
- `!=` operator support
- Enhanced typeclass VM support with dictionary passing
- JMH benchmarking infrastructure

## Development Workflow

### Running Tests
```bash
# Run all tests
sbt test

# Run specific test categories
sbt "testOnly com.github.klassic.VmInterpreterSpec"
sbt "testOnly com.github.klassic.TypeClass*"

# Run with specific test flags
sbt "testOnly -- -l 'slow'"  # Exclude slow tests
```

### Debugging Type Errors
Type errors include location information. The `Typer` provides detailed error messages with line/column positions.

### Adding New Features
1. Update `Ast.scala` for new syntax nodes
2. Extend `Parser.scala` with new grammar rules
3. Add type inference rules in `Typer.scala`
4. Implement VM instructions in `vm/` if needed
5. Write tests in appropriate `*Spec.scala` file

### Java Interop
- Method calls: `object->method(args)`
- Constructor: `new java.util.ArrayList`
- Automatic boxing/unboxing handled by runtime

## Important Notes

- VM compiler is incomplete - some VM tests are ignored with `pendingUntilFixed`
- REPL supports multi-line input with `:exit` and `:history` commands
- Test files use `E()` helper for evaluation assertions
- Runtime functions split between built-in (global) and module-specific