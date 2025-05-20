package com.github.klassic.vm

import com.github.klassic._

class VmInterpreter extends Processor[TypedAst.Program, Value, InteractiveSession] {
  private val compiler = new VmCompiler
  private val vm = new VirtualMachine
  private val interpreter = new Interpreter

  override val name: String = "VmInterpreter"

  override def process(input: TypedAst.Program, session: InteractiveSession): Value = {
    val code = compiler.compile(input.block)
    val env = new RuntimeEnvironment(Some(interpreter.BuiltinEnvironment))
    vm.run(code, env)
  }
}
