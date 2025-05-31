package com.github.klassic.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import com.github.klassic._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(value = 2, jvmArgs = Array("-Xms2G", "-Xmx2G"))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
class EndToEndBenchmark {
  
  private var evaluator: Evaluator = _
  
  // Sample Klassic programs
  private val fibonacciProgram = """
    fun fib(n) {
      if(n < 2) {
        n
      } else {
        fib(n - 1) + fib(n - 2)
      }
    }
    fib(10)
  """
  
  private val factorialProgram = """
    fun factorial(n) {
      var result = 1
      var i = 1
      while(i <= n) {
        result = result * i
        i = i + 1
      }
      result
    }
    factorial(10)
  """
  
  private val listOperationsProgram = """
    val list = [1, 2, 3, 4, 5]
    var sum = 0
    foreach(x in list) {
      sum = sum + x
    }
    sum
  """
  
  private val stringOperationsProgram = """
    val str1 = "Hello"
    val str2 = "World"
    val result = str1 + " " + str2
    result.length()
  """
  
  private val objectCreationProgram = """
    record Point(x, y)
    val p1 = new Point(10, 20)
    val p2 = new Point(30, 40)
    p1.x + p1.y + p2.x + p2.y
  """
  
  private val higherOrderFunctionProgram = """
    fun map(list, f) {
      var result = []
      foreach(x in list) {
        result = result + [f(x)]
      }
      result
    }
    
    fun double(x) { x * 2 }
    
    map([1, 2, 3, 4, 5], double)
  """
  
  @Setup
  def setup(): Unit = {
    evaluator = new Evaluator()
  }
  
  @Benchmark
  def benchmarkFibonacciEndToEnd(bh: Blackhole): Unit = {
    val result = evaluator.evaluateString(fibonacciProgram)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkFactorialEndToEnd(bh: Blackhole): Unit = {
    val result = evaluator.evaluateString(factorialProgram)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkListOperationsEndToEnd(bh: Blackhole): Unit = {
    val result = evaluator.evaluateString(listOperationsProgram)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkStringOperationsEndToEnd(bh: Blackhole): Unit = {
    val result = evaluator.evaluateString(stringOperationsProgram)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkObjectCreationEndToEnd(bh: Blackhole): Unit = {
    val result = evaluator.evaluateString(objectCreationProgram)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkHigherOrderFunctionEndToEnd(bh: Blackhole): Unit = {
    val result = evaluator.evaluateString(higherOrderFunctionProgram)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkParseOnly(bh: Blackhole): Unit = {
    val session = new InteractiveSession
    val parser = new Parser
    val result = parser.process(fibonacciProgram, session)
    bh.consume(result)
  }
  
  @Benchmark
  def benchmarkTypeCheckOnly(bh: Blackhole): Unit = {
    val session = new InteractiveSession
    val parser = new Parser
    val placeholderDesugerer = new PlaceholderDesugerer
    val rewriter = new SyntaxRewriter
    val typer = new Typer
    
    val parsedProgram = parser.process(fibonacciProgram, session)
    val desugaredProgram = placeholderDesugerer.process(parsedProgram, session)
    val rewrittenProgram = rewriter.process(desugaredProgram, session)
    val typedProgram = typer.process(rewrittenProgram, session)
    bh.consume(typedProgram)
  }
}