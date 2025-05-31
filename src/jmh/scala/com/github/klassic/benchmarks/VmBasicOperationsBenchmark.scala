package com.github.klassic.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import scala.collection.mutable
import com.github.klassic._
import com.github.klassic.vm._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(value = 2, jvmArgs = Array("-Xms2G", "-Xmx2G"))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
class VmBasicOperationsBenchmark {
  
  private var vm: VirtualMachine = _
  private var emptyEnv: RuntimeEnvironment = _
  private var moduleEnv: ModuleEnvironment = _
  private var recordEnv: RecordEnvironment = _
  
  // Pre-compiled instruction sequences for benchmarks
  private var addInstructions: Vector[Instruction] = _
  private var subInstructions: Vector[Instruction] = _
  private var mulInstructions: Vector[Instruction] = _
  private var divInstructions: Vector[Instruction] = _
  private var pushPopInstructions: Vector[Instruction] = _
  private var loadStoreInstructions: Vector[Instruction] = _
  private var jumpInstructions: Vector[Instruction] = _
  private var comparisonInstructions: Vector[Instruction] = _
  
  @Setup(Level.Trial)
  def setup(): Unit = {
    moduleEnv = new ModuleEnvironment()
    recordEnv = new RecordEnvironment()
    emptyEnv = RuntimeEnvironment.pooled(None)
    
    // Initialize test environment with some variables
    emptyEnv.defineValue("x")(BoxedInt(10))
    emptyEnv.defineValue("y")(BoxedInt(20))
    
    // Pre-compile instruction sequences
    
    // Simple arithmetic: 1 + 2 + 3 + ... + 10
    addInstructions = Vector(
      Push(BoxedInt(1)),
      Push(BoxedInt(2)),
      Add,
      Push(BoxedInt(3)),
      Add,
      Push(BoxedInt(4)),
      Add,
      Push(BoxedInt(5)),
      Add,
      Push(BoxedInt(6)),
      Add,
      Push(BoxedInt(7)),
      Add,
      Push(BoxedInt(8)),
      Add,
      Push(BoxedInt(9)),
      Add,
      Push(BoxedInt(10)),
      Add
    )
    
    // Subtraction chain
    subInstructions = Vector(
      Push(BoxedInt(100)),
      Push(BoxedInt(10)),
      Sub,
      Push(BoxedInt(5)),
      Sub,
      Push(BoxedInt(3)),
      Sub,
      Push(BoxedInt(1)),
      Sub
    )
    
    // Multiplication chain
    mulInstructions = Vector(
      Push(BoxedInt(2)),
      Push(BoxedInt(3)),
      Mul,
      Push(BoxedInt(4)),
      Mul,
      Push(BoxedInt(5)),
      Mul
    )
    
    // Division chain
    divInstructions = Vector(
      Push(BoxedInt(1000)),
      Push(BoxedInt(10)),
      Div,
      Push(BoxedInt(5)),
      Div,
      Push(BoxedInt(2)),
      Div
    )
    
    // Push/Pop stress test
    pushPopInstructions = Vector.tabulate(100) { i =>
      if (i % 2 == 0) Push(BoxedInt(i))
      else Pop
    }
    
    // Load/Store operations
    loadStoreInstructions = Vector(
      Load("x"),
      Load("y"),
      Add,
      Store("result"),
      Load("result"),
      Push(BoxedInt(5)),
      Mul,
      Store("result2")
    )
    
    // Jump operations (simple loop)
    jumpInstructions = Vector(
      Push(BoxedInt(0)),     // 0: counter
      Store("i"),            // 1
      Load("i"),             // 2: loop start
      Push(BoxedInt(10)),    // 3
      LessThan,              // 4
      JumpIfFalse(11),       // 5: exit if i >= 10
      Load("i"),             // 6
      Push(BoxedInt(1)),     // 7
      Add,                   // 8
      Store("i"),            // 9
      Jump(2),               // 10: back to loop start
      Load("i")              // 11: result
    )
    
    // Comparison operations
    comparisonInstructions = Vector(
      Push(BoxedInt(10)),
      Push(BoxedInt(20)),
      LessThan,
      Push(BoxedInt(30)),
      Push(BoxedInt(30)),
      Equal,
      Push(BoxedInt(40)),
      Push(BoxedInt(35)),
      GreaterThan
    )
  }
  
  @Setup(Level.Invocation)
  def setupInvocation(): Unit = {
    vm = new VirtualMachine(moduleEnv, recordEnv)
  }
  
  @Benchmark
  def benchmarkAddition(bh: Blackhole): Unit = {
    val result = vm.run(addInstructions, emptyEnv)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkSubtraction(bh: Blackhole): Unit = {
    val result = vm.run(subInstructions, emptyEnv)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkMultiplication(bh: Blackhole): Unit = {
    val result = vm.run(mulInstructions, emptyEnv)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkDivision(bh: Blackhole): Unit = {
    val result = vm.run(divInstructions, emptyEnv)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkPushPop(bh: Blackhole): Unit = {
    val result = vm.run(pushPopInstructions, emptyEnv)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkLoadStore(bh: Blackhole): Unit = {
    val result = vm.run(loadStoreInstructions, RuntimeEnvironment.pooled(Some(emptyEnv)))
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkJumpLoop(bh: Blackhole): Unit = {
    val result = vm.run(jumpInstructions, RuntimeEnvironment.pooled(Some(emptyEnv)))
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkComparisons(bh: Blackhole): Unit = {
    val result = vm.run(comparisonInstructions, emptyEnv)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkSingleInstruction(bh: Blackhole): Unit = {
    val result = vm.run(Vector(Push(BoxedInt(42))), emptyEnv)
    bh.consume(result)
  }
}