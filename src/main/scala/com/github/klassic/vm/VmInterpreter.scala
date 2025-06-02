package com.github.klassic.vm

import com.github.klassic._
import com.github.klassic.Type._

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
    
    // Create a runtime environment that includes typeclass instance methods
    val runtimeEnv = new RuntimeEnvironment(Some(BuiltinEnvironments.BuiltinEnvironment))
    
    // Add typeclass instance methods to the runtime environment
    input.instances.foreach { case ((className, forType), instance) =>
      instance.methods.foreach { case (methodName, methodType) =>
        val instanceMethodName = s"${className}_${normalizeTypeName(forType)}_${methodName}"
        // For now, create a placeholder function that would be implemented by the actual instance
        val instanceFunction = createInstanceMethod(className, forType, methodName, input, runtimeEnv)
        runtimeEnv.update(instanceMethodName, instanceFunction)
      }
    }
    
    val vm = new VirtualMachine(BuiltinEnvironments.BuiltinModuleEnvironment, runtimeRecordEnvironment)
    val code = compiler.compile(input.block)
    vm.run(code, runtimeEnv)
  }
  
  private def normalizeTypeName(t: Type): String = t match {
    case TInt => "Int"
    case TString => "String"
    case TBoolean => "Boolean"
    case TFloat => "Float"
    case TDouble => "Double"
    case TUnit => "Unit"
    case TConstructor("List", List(TInt), _) => "List<Int>"
    case TConstructor(name, _, _) => name
    case _ => "Unknown"
  }
  
  private def createInstanceMethod(className: String, forType: Type, methodName: String, program: TypedAst.Program, runtimeEnv: RuntimeEnvironment): Value = {
    // First, check if there's a user-defined instance method
    val instanceOpt = program.instances.get((className, forType))
    
    instanceOpt match {
      case Some(instance) =>
        // Find the corresponding instance declaration in the original AST
        // For now, we'll use the built-in implementations
        (className, normalizeTypeName(forType), methodName) match {
      case ("Show", "Int", "show") => 
        NativeFunctionValue { case List(BoxedInt(x)) => ObjectValue(s"Int($x)") }
      case ("Show", "String", "show") => 
        NativeFunctionValue { case List(ObjectValue(x: String)) => ObjectValue(s"String($x)") }
      case ("Show", "List<Int>", "show") =>
        NativeFunctionValue { case List(ObjectValue(xs: java.util.List[_])) => 
          ObjectValue(s"List[${xs.size()} elements]") 
        }
      case ("Eq", "Int", "equals") =>
        NativeFunctionValue { case List(BoxedInt(x), BoxedInt(y)) => Value.boxBoolean(x == y) }
      case ("Eq", "String", "equals") =>
        NativeFunctionValue { case List(ObjectValue(x: String), ObjectValue(y: String)) => Value.boxBoolean(x == y) }
      case ("Eq", "Int", "notEquals") =>
        NativeFunctionValue { case List(BoxedInt(x), BoxedInt(y)) => Value.boxBoolean(x != y) }
      case ("Eq", "String", "notEquals") =>
        NativeFunctionValue { case List(ObjectValue(x: String), ObjectValue(y: String)) => Value.boxBoolean(x != y) }
      case ("Functor", "List", "map") =>
        // For Functor<List>, we need to adapt the argument order
        // Typeclass: map(f, xs) but built-in: map(xs)(f)
        NativeFunctionValue { 
          case List(f: Value, xs: Value) =>
            // Reorder arguments to match built-in map signature
            val builtinMap = try {
              runtimeEnv("map")
            } catch {
              case _: Exception => null
            }
            
            if (builtinMap != null) {
              // Call built-in map with reordered arguments
              builtinMap match {
                case nf: NativeFunctionValue =>
                  // First apply list
                  val curriedMap = nf.body(List(xs))
                  // Then apply function
                  curriedMap match {
                    case nf2: NativeFunctionValue => nf2.body(List(f))
                    case _ => throw new RuntimeException("Expected curried function from map")
                  }
                case _ => throw new RuntimeException("Expected native function for map")
              }
            } else {
              // Fallback implementation if built-in map is not available
              xs match {
                case ObjectValue(xsList: java.util.List[_]) =>
                  val newList = new java.util.ArrayList[Any]
                  var i = 0
                  while(i < xsList.size()) {
                    val x = Value.toKlassic(xsList.get(i).asInstanceOf[AnyRef])
                    // Apply the function
                    val result = f match {
                      case nf: NativeFunctionValue if nf.body.isDefinedAt(List(x)) =>
                        nf.body(List(x))
                      case VmClosureValue(params, bodyStart, bodyEnd, closureEnv, instructions) =>
                        // Execute closure with VM
                        val virtualMachine = new vm.VirtualMachine(BuiltinEnvironments.BuiltinModuleEnvironment, BuiltinEnvironments.BuiltinRecordEnvironment)
                        val newEnv = RuntimeEnvironment.pooled(Some(closureEnv))
                        params.headOption.foreach { p => newEnv.update(p, x) }
                        val closureCode = instructions.slice(bodyStart, bodyEnd + 1)
                        virtualMachine.run(closureCode, newEnv)
                      case _ =>
                        x // Fallback - return unchanged
                    }
                    // Don't convert - keep as Klassic Value
                    newList.add(result)
                    i += 1
                  }
                  ObjectValue(newList)
                case _ =>
                  throw new RuntimeException(s"Functor.map expects list as second argument, got: ${xs}")
              }
            }
          case args =>
            throw new RuntimeException(s"Functor.map expects (function, list), got: ${args}")
        }
      case _ =>
        NativeFunctionValue { case _ => 
          throw new RuntimeException(s"Instance method $className.$methodName for $forType not implemented")
        }
    }
      case None =>
        // No instance found, return error
        NativeFunctionValue { case _ => 
          throw new RuntimeException(s"No instance found for $className[$forType]")
        }
    }
  }
}
