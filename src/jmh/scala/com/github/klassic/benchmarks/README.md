# Klassic Benchmark Suite

This directory contains JMH (Java Microbenchmark Harness) benchmarks for measuring Klassic's performance.

## Benchmark Categories

### 1. VM Basic Operations (`VmBasicOperationsBenchmark`)
Tests fundamental VM operations:
- Arithmetic operations (add, sub, mul, div)
- Stack operations (push, pop)
- Variable operations (load, store)
- Control flow (jumps, comparisons)

### 2. Language Features (`LanguageFeaturesBenchmark`)
Tests language-specific features:
- Function calls (simple and nested)
- Closures with captured variables
- List, Map, and Set creation
- Record operations
- Java interop (method calls, object creation)

### 3. Algorithms (`AlgorithmBenchmark`)
Tests common algorithms:
- Fibonacci (iterative)
- Factorial
- Array summation
- Prime number checking
- Nested loops

### 4. End-to-End (`EndToEndBenchmark`)
Tests complete pipeline from source code to execution:
- Full program execution
- Parse-only performance
- Type-check-only performance

## Running Benchmarks

### Run all benchmarks
```bash
sbt "jmh:run"
```

### Run specific benchmark class
```bash
sbt "jmh:run VmBasicOperationsBenchmark"
```

### Run specific benchmark method
```bash
sbt "jmh:run VmBasicOperationsBenchmark.benchmarkAddition"
```

### Run with custom parameters
```bash
# Quick smoke test (1 fork, 1 iteration)
sbt "jmh:run -f 1 -i 1 -wi 1"

# More iterations for stable results
sbt "jmh:run -f 2 -i 10 -wi 5"

# Run with specific JVM options
sbt "jmh:run -jvmArgs '-Xms4G -Xmx4G'"
```

### Run with regex pattern
```bash
# Run all addition-related benchmarks
sbt "jmh:run .*Addition.*"

# Run all VM benchmarks
sbt "jmh:run .*Vm.*"
```

## Benchmark Parameters

Some benchmarks have configurable parameters:
- `AlgorithmBenchmark.fibN`: Fibonacci number to calculate (10, 15, 20)
- `AlgorithmBenchmark.arraySize`: Size of array for summation (10, 50, 100)

## Output

Benchmark results show:
- Average time per operation
- Standard deviation
- Throughput (ops/sec)

Results are displayed in the console and can be saved to JSON/CSV:
```bash
sbt "jmh:run -rf json -rff results.json"
sbt "jmh:run -rf csv -rff results.csv"
```

## Profiling

Run with profiling to identify hotspots:
```bash
# With stack profiler
sbt "jmh:run -prof stack"

# With GC profiler
sbt "jmh:run -prof gc"

# With JVM profiler
sbt "jmh:run -prof perfnorm"
```

## Tips for Accurate Results

1. Close other applications to reduce system noise
2. Run on a consistent CPU frequency (disable turbo boost if possible)
3. Use sufficient warm-up iterations
4. Run multiple forks to account for JVM variations
5. Check for GC pressure with `-prof gc`