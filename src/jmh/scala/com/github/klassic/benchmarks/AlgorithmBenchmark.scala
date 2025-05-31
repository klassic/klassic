package com.github.klassic.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import com.github.klassic._
import com.github.klassic.vm._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(value = 2, jvmArgs = Array("-Xms2G", "-Xmx2G"))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
class AlgorithmBenchmark {
  
  private var vm: VirtualMachine = _
  private var env: RuntimeEnvironment = _
  private var moduleEnv: ModuleEnvironment = _
  private var recordEnv: RecordEnvironment = _
  
  // Pre-compiled algorithm implementations
  private var fibonacciIterative: Vector[Instruction] = _
  private var fibonacciRecursive: Vector[Instruction] = _
  private var factorial: Vector[Instruction] = _
  private var sumArray: Vector[Instruction] = _
  private var bubbleSort: Vector[Instruction] = _
  private var isPrime: Vector[Instruction] = _
  
  @Param(Array("10", "15", "20"))
  var fibN: Int = _
  
  @Param(Array("10", "50", "100"))
  var arraySize: Int = _
  
  @Setup(Level.Trial)
  def setup(): Unit = {
    moduleEnv = new ModuleEnvironment()
    recordEnv = new RecordEnvironment()
    
    // Iterative Fibonacci
    fibonacciIterative = Vector(
      // Initialize (0-5)
      Push(BoxedInt(0)),      // 0
      Store("a"),             // 1
      Push(BoxedInt(1)),      // 2
      Store("b"),             // 3
      Push(BoxedInt(2)),      // 4
      Store("i"),             // 5
      // Check if n == 0 (6-11)
      Load("n"),              // 6
      Push(BoxedInt(0)),      // 7
      Equal,                  // 8
      JumpIfFalse(12),        // 9  - jump to index 12 if n != 0
      Push(BoxedInt(0)),      // 10
      Return,                 // 11
      // Check if n == 1 (12-17)
      Load("n"),              // 12
      Push(BoxedInt(1)),      // 13
      Equal,                  // 14
      JumpIfFalse(18),        // 15 - jump to index 18 if n != 1
      Push(BoxedInt(1)),      // 16
      Return,                 // 17
      // Loop start (18-34)
      Load("i"),              // 18
      Load("n"),              // 19
      LessOrEqual,            // 20
      JumpIfFalse(35),        // 21 - jump to index 35 (Load("b"))
      // temp = a + b
      Load("a"),              // 22
      Load("b"),              // 23
      Add,                    // 24
      Store("temp"),          // 25
      // a = b
      Load("b"),              // 26
      Store("a"),             // 27
      // b = temp
      Load("temp"),           // 28
      Store("b"),             // 29
      // i++
      Load("i"),              // 30
      Push(BoxedInt(1)),      // 31
      Add,                    // 32
      Store("i"),             // 33
      Jump(18),               // 34 - jump back to loop start
      // Return b
      Load("b"),              // 35
      Return                  // 36
    )
    
    // Factorial
    factorial = Vector(
      // Initialize result = 1
      Push(BoxedInt(1)),
      Store("result"),
      // Initialize i = 1
      Push(BoxedInt(1)),
      Store("i"),
      // Loop start (index 4)
      Load("i"),
      Load("n"),
      LessOrEqual,
      JumpIfFalse(17),  // Jump to Load("result") at index 17
      // result = result * i
      Load("result"),
      Load("i"),
      Mul,
      Store("result"),
      // i++
      Load("i"),
      Push(BoxedInt(1)),
      Add,
      Store("i"),
      Jump(4),
      // Return result
      Load("result"),
      Return
    )
    
    // Sum array elements
    sumArray = Vector(
      // Initialize sum = 0
      Push(BoxedInt(0)),
      Store("sum"),
      // Initialize i = 0
      Push(BoxedInt(0)),
      Store("i"),
      // Loop start (index 4)
      Load("i"),
      Load("size"),
      LessThan,
      JumpIfFalse(16),
      // sum += array[i] (simplified - just add i for now)
      Load("sum"),
      Load("i"),
      Add,
      Store("sum"),
      // i++
      Load("i"),
      Push(BoxedInt(1)),
      Add,
      Store("i"),
      Jump(4),
      // Return sum
      Load("sum"),
      Return
    )
    
    // Check if prime
    isPrime = Vector(
      // Check if n <= 1 (0-5)
      Load("n"),                 // 0
      Push(BoxedInt(1)),         // 1
      LessOrEqual,               // 2
      JumpIfFalse(6),            // 3 - if n > 1, jump to index 6
      Push(BoxedBoolean(false)), // 4
      Return,                    // 5
      // Check if n == 2 (6-11)
      Load("n"),                 // 6
      Push(BoxedInt(2)),         // 7
      Equal,                     // 8
      JumpIfFalse(12),           // 9 - if n != 2, jump to index 12
      Push(BoxedBoolean(true)),  // 10
      Return,                    // 11
      // Initialize i = 2 (12-13)
      Push(BoxedInt(2)),         // 12
      Store("i"),                // 13
      // Loop start (14-29)
      Load("i"),                 // 14
      Load("i"),                 // 15
      Mul,                       // 16 - i * i
      Load("n"),                 // 17
      LessOrEqual,               // 18 - i*i <= n
      JumpIfFalse(35),           // 19 - if i*i > n, jump to return true
      // Check if n % i == 0
      Load("n"),                 // 20
      Load("i"),                 // 21
      Div,                       // 22 - n / i
      Load("i"),                 // 23
      Mul,                       // 24 - (n/i) * i
      Load("n"),                 // 25
      Equal,                     // 26 - (n/i)*i == n
      JumpIfFalse(30),           // 27 - if not divisible, skip to i++
      Push(BoxedBoolean(false)), // 28
      Return,                    // 29
      // i++ and continue loop (30-34)
      Load("i"),                 // 30
      Push(BoxedInt(1)),         // 31
      Add,                       // 32
      Store("i"),                // 33
      Jump(14),                  // 34 - jump back to loop start
      // Return true (35-36)
      Push(BoxedBoolean(true)),  // 35
      Return                     // 36
    )
  }
  
  @Setup(Level.Invocation)
  def setupInvocation(): Unit = {
    vm = new VirtualMachine(moduleEnv, recordEnv)
    env = RuntimeEnvironment.pooled(None)
  }
  
  @Benchmark
  def benchmarkFibonacciIterative(bh: Blackhole): Unit = {
    val localEnv = RuntimeEnvironment.pooled(Some(env))
    localEnv.defineValue("n")(BoxedInt(fibN))
    val result = vm.run(fibonacciIterative, localEnv)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkFactorial(bh: Blackhole): Unit = {
    val localEnv = RuntimeEnvironment.pooled(Some(env))
    localEnv.defineValue("n")(BoxedInt(10))
    val result = vm.run(factorial, localEnv)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkSumArray(bh: Blackhole): Unit = {
    val localEnv = RuntimeEnvironment.pooled(Some(env))
    localEnv.defineValue("size")(BoxedInt(arraySize))
    val result = vm.run(sumArray, localEnv)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkIsPrime(bh: Blackhole): Unit = {
    val localEnv = RuntimeEnvironment.pooled(Some(env))
    localEnv.defineValue("n")(BoxedInt(97)) // A prime number
    val result = vm.run(isPrime, localEnv)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkNestedLoops(bh: Blackhole): Unit = {
    // Double nested loop: sum of i*j for i,j in 0..9
    val instructions = Vector(
      Push(BoxedInt(0)),
      Store("sum"),
      Push(BoxedInt(0)),
      Store("i"),
      // Outer loop start (index 4)
      Load("i"),
      Push(BoxedInt(10)),
      LessThan,
      JumpIfFalse(29),
      Push(BoxedInt(0)),
      Store("j"),
      // Inner loop start (index 10)
      Load("j"),
      Push(BoxedInt(10)),
      LessThan,
      JumpIfFalse(24),
      // sum += i * j
      Load("sum"),
      Load("i"),
      Load("j"),
      Mul,
      Add,
      Store("sum"),
      // j++
      Load("j"),
      Push(BoxedInt(1)),
      Add,
      Store("j"),
      Jump(10),
      // i++
      Load("i"),
      Push(BoxedInt(1)),
      Add,
      Store("i"),
      Jump(4),
      // Return sum
      Load("sum")
    )
    
    val result = vm.run(instructions, RuntimeEnvironment.pooled(Some(env)))
    bh.consume(result)
  }
}