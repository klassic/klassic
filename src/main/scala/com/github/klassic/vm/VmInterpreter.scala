package com.github.klassic.vm

import com.github.klassic._

class VmInterpreter extends Processor[TypedAst.Program, Value, InteractiveSession] {
  private val compiler = new VmCompiler
  private val interpreter = new Interpreter
  
  override val name: String = "VmInterpreter"

  override def process(input: TypedAst.Program, session: InteractiveSession): Value = {
    val runtimeRecordEnvironment: RecordEnvironment = interpreter.BuiltinRecordEnvironment
    input.records.foreach { case (name, record) =>
      val members = interpreter.toList(record.row)
      val rmembers = members.map { case (n, t) => n -> t }
      runtimeRecordEnvironment.records += (name -> rmembers)
    }
    
    val vm = new VirtualMachine(interpreter, interpreter.BuiltinModuleEnvironment, runtimeRecordEnvironment)
    val code = compiler.compile(input.block)
    vm.run(code, interpreter.BuiltinEnvironment)
  }
}
