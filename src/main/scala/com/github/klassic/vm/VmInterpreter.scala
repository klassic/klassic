package com.github.klassic.vm

import com.github.klassic._

class VmInterpreter extends Processor[TypedAst.Program, Value, InteractiveSession] {
  private val compiler = new VmCompiler
  
  override val name: String = "VmInterpreter"

  override def process(input: TypedAst.Program, session: InteractiveSession): Value = {
    val runtimeRecordEnvironment: RecordEnvironment = BuiltinEnvironments.BuiltinRecordEnvironment
    input.records.foreach { case (name, record) =>
      val members = BuiltinEnvironments.toList(record.row)
      val rmembers = members.map { case (n, t) => n -> t }
      runtimeRecordEnvironment.records += (name -> rmembers)
    }
    
    val vm = new VirtualMachine(BuiltinEnvironments.BuiltinModuleEnvironment, runtimeRecordEnvironment)
    val code = compiler.compile(input.block)
    vm.run(code, BuiltinEnvironments.BuiltinEnvironment)
  }
}
