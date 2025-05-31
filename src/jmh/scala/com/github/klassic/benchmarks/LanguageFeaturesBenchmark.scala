package com.github.klassic.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import com.github.klassic._
import com.github.klassic.vm._
import scala.collection.mutable

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(value = 2, jvmArgs = Array("-Xms2G", "-Xmx2G"))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
class LanguageFeaturesBenchmark {
  
  private var vm: VirtualMachine = _
  private var env: RuntimeEnvironment = _
  private var moduleEnv: ModuleEnvironment = _
  private var recordEnv: RecordEnvironment = _
  
  // Pre-compiled instruction sequences
  private var simpleFunctionCall: Vector[Instruction] = _
  private var nestedFunctionCalls: Vector[Instruction] = _
  private var closureCreation: Vector[Instruction] = _
  private var closureCapture: Vector[Instruction] = _
  private var listOperations: Vector[Instruction] = _
  private var mapOperations: Vector[Instruction] = _
  private var recordOperations: Vector[Instruction] = _
  
  @Setup(Level.Trial)
  def setup(): Unit = {
    moduleEnv = new ModuleEnvironment()
    recordEnv = new RecordEnvironment(mutable.Map(
      "Point" -> List(("x", Type.TDynamic), ("y", Type.TDynamic)),
      "Person" -> List(("name", Type.TString), ("age", Type.TInt))
    ))
    
    // Simple function call: (x, y) => x + y
    simpleFunctionCall = Vector(
      // Define function
      MakeClosure(List("x", "y"), 2, 6),
      Store("add"),
      // Function body starts at 2
      Load("x"),
      Load("y"),
      Add,
      Return,
      // Call function with arguments
      Load("add"),
      Push(BoxedInt(10)),
      Push(BoxedInt(20)),
      Call(2)
    )
    
    // Nested function calls
    nestedFunctionCalls = Vector(
      // Define inner function: x => x * 2
      MakeClosure(List("x"), 2, 5),
      Store("double"),
      Load("x"),
      Push(BoxedInt(2)),
      Mul,
      Return,
      // Define outer function: x => double(x) + 1
      MakeClosure(List("x"), 8, 13),
      Store("doublePlusOne"),
      Load("double"),
      Load("x"),
      Call(1),
      Push(BoxedInt(1)),
      Add,
      Return,
      // Call outer function
      Load("doublePlusOne"),
      Push(BoxedInt(5)),
      Call(1)
    )
    
    // Closure with captured variables
    closureCapture = Vector(
      // Set up captured variable
      Push(BoxedInt(100)),
      Store("captured"),
      // Create closure that captures 'captured'
      MakeClosure(List("x"), 4, 9),
      Store("addCaptured"),
      Load("x"),
      Load("captured"),
      Add,
      Return,
      // Call closure
      Load("addCaptured"),
      Push(BoxedInt(50)),
      Call(1),
      // Modify captured and call again
      Push(BoxedInt(200)),
      Store("captured"),
      Load("addCaptured"),
      Push(BoxedInt(50)),
      Call(1)
    )
    
    // List operations
    listOperations = Vector(
      // Create list [1, 2, 3, 4, 5]
      Push(BoxedInt(1)),
      Push(BoxedInt(2)),
      Push(BoxedInt(3)),
      Push(BoxedInt(4)),
      Push(BoxedInt(5)),
      MakeList(5),
      Store("list"),
      // Access elements (would need proper list access instructions)
      Load("list")
    )
    
    // Map operations
    mapOperations = Vector(
      // Create map {"a": 1, "b": 2, "c": 3}
      Push(ObjectValue("a")),
      Push(BoxedInt(1)),
      Push(ObjectValue("b")),
      Push(BoxedInt(2)),
      Push(ObjectValue("c")),
      Push(BoxedInt(3)),
      MakeMap(3),
      Store("map"),
      Load("map")
    )
    
    // Record operations
    recordOperations = Vector(
      // Create Point record
      Push(BoxedInt(10)),
      Push(BoxedInt(20)),
      NewRecord("Point", 2),
      Store("point"),
      // Access field
      Load("point"),
      GetField("x"),
      Load("point"),
      GetField("y"),
      Add
    )
  }
  
  @Setup(Level.Invocation)
  def setupInvocation(): Unit = {
    vm = new VirtualMachine(moduleEnv, recordEnv)
    env = RuntimeEnvironment.pooled(None)
  }
  
  @Benchmark
  def benchmarkSimpleFunctionCall(bh: Blackhole): Unit = {
    val result = vm.run(simpleFunctionCall, RuntimeEnvironment.pooled(Some(env)))
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkNestedFunctionCalls(bh: Blackhole): Unit = {
    val result = vm.run(nestedFunctionCalls, RuntimeEnvironment.pooled(Some(env)))
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkClosureCapture(bh: Blackhole): Unit = {
    val result = vm.run(closureCapture, RuntimeEnvironment.pooled(Some(env)))
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkListCreation(bh: Blackhole): Unit = {
    val result = vm.run(listOperations, RuntimeEnvironment.pooled(Some(env)))
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkMapCreation(bh: Blackhole): Unit = {
    val result = vm.run(mapOperations, RuntimeEnvironment.pooled(Some(env)))
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkRecordOperations(bh: Blackhole): Unit = {
    val result = vm.run(recordOperations, RuntimeEnvironment.pooled(Some(env)))
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkMethodCall(bh: Blackhole): Unit = {
    // Benchmark calling a method on a Java object
    val instructions = Vector(
      Push(ObjectValue("Hello, World!")),
      Push(BoxedInt(7)),
      CallMethod("charAt", 1)
    )
    val result = vm.run(instructions, env)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkNewObject(bh: Blackhole): Unit = {
    // Benchmark creating a new Java object
    val instructions = Vector(
      Push(ObjectValue("test string")),
      NewObject("java.lang.StringBuilder", 1),
      Push(ObjectValue(" appended")),
      CallMethod("append", 1),
      CallMethod("toString", 0)
    )
    val result = vm.run(instructions, env)
    bh.consume(result)
  }
}