# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Klassic is a statically typed programming language with Hindley-Milner type system, row polymorphism based object system, and built-in Java FFI. The project is written in Scala 3 and uses SBT as its build tool.

## Build Commands

```bash
# Compile the project
sbt compile

# Run tests
sbt test

# Run tests excluding slow tests
sbt "testOnly -- -l 'slow'"

# Run specific test
sbt "testOnly com.github.klassic.ExpressionSpec"

# Build executable JAR
sbt assembly

# Run REPL
sbt run

# Run a Klassic program
java -jar klassic.jar -f program.kl

# Evaluate expression
java -jar klassic.jar -e "println(1 + 2)"
```

## Architecture

### Core Pipeline

The language implementation follows a standard compiler pipeline:

1. **Parser** (`Parser.scala`) - Parses source code into AST using parser combinators
2. **PlaceholderDesugerer** (`PlaceholderDesugerer.scala`) - Desugars placeholder syntax
3. **SyntaxRewriter** (`SyntaxRewriter.scala`) - Rewrites syntax (e.g., foreach to while)
4. **Typer** (`Typer.scala`) - Type inference using Hindley-Milner with row polymorphism
5. **Interpreter** (`Interpreter.scala`) - Tree-walking interpreter
6. **VmInterpreter** (`vm/VmInterpreter.scala`) - Optional VM-based interpreter

### Key Components

- **Type System**: Based on Hindley-Milner with extensions for row polymorphism and type constructors
- **Built-in Functions**: Defined in `Interpreter.scala` (BuiltinEnvironment)
- **Module System**: Supports modules like List, Map, Set, FileInput, GPIO
- **Java Interop**: Automatic boxing/unboxing and method resolution via reflection

### Testing

Tests use ScalaTest and are located in `src/test/scala`. Test programs are in `test-programs/` directory with `.kl` extension.

## Language Features

- First-class functions with type inference
- Mutable and immutable variable bindings
- List/Map/Set literals with space-sensitive syntax
- String interpolation
- Record types with row polymorphism
- Pattern matching via placeholders
- Java FFI for method calls and object creation

## Development Notes

- The VM compiler is incomplete and tests are currently ignored
- Type errors include location information for better diagnostics
- The REPL supports multi-line input and :history/:exit commands
- Runtime functions are split between built-in (global) and module-specific