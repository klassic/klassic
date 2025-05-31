package com.github.klassic

import java.io.BufferedReader
import java.nio.file.{Files, Path, Paths}
import java.util
import java.util.stream.Collectors

import scala.jdk.CollectionConverters._
import com.github.klassic._
import com.github.klassic.Type._
import com.github.klassic.TypedAst.{FunctionLiteral, TypedNode, ValueNode}
import com.pi4j.io.gpio.{GpioController, GpioFactory, GpioPinDigitalInput, GpioPinDigitalOutput, Pin, PinState, RaspiPin}
import com.pi4j.wiringpi.Gpio
import klassic.runtime.{AssertionError, NotImplementedError}

import scala.runtime.BoxedUnit

/**
 * @author Kota Mizushima
 */
class Interpreter extends Processor[TypedAst.Program, Value, InteractiveSession] {interpreter =>
  def reportError(message: String): Nothing = {
    throw InterpreterException(message)
  }

  // Helper method to execute VM closures with arguments
  private def executeVmClosure(closure: VmClosureValue, args: List[Value]): Value = {
    val VmClosureValue(params, bodyStart, bodyEnd, closureEnv, instructions) = closure
    val virtualMachine = new vm.VirtualMachine(interpreter, BuiltinModuleEnvironment, BuiltinRecordEnvironment)
    val newEnv = RuntimeEnvironment.pooled(Some(closureEnv))
    
    // Bind arguments to parameters
    params.zip(args).foreach { case (param, arg) =>
      newEnv.update(param, arg)
    }
    
    // Execute the closure's instructions range
    val closureCode = instructions.slice(bodyStart, bodyEnd + 1)
    virtualMachine.run(closureCode, newEnv)
  }

  def findMethod(self: AnyRef, name: String, params: Array[Value]): MethodSearchResult = {
    val selfClass = self.getClass
    val nameMatchedMethods = selfClass.getMethods.filter {
      _.getName == name
    }
    val maybeUnboxedMethod = nameMatchedMethods.find { m =>
      val parameterCountMatches = m.getParameterCount == params.length
      val parameterTypes = Value.classesOfValues(params)
      val parameterTypesMatches = (m.getParameterTypes zip parameterTypes).forall{ case (arg, param) =>
        arg.isAssignableFrom(param)
      }
      parameterCountMatches && parameterTypesMatches
    }.map{m =>
      m.setAccessible(true)
      UnboxedVersionMethodFound(m)
    }
    val maybeBoxedMethod = {
      nameMatchedMethods.find{m =>
        val parameterCountMatches = m.getParameterCount == params.length
        val boxedParameterTypes = Value.boxedClassesOfValues(params)
        val boxedParameterTypesMatches = (m.getParameterTypes zip boxedParameterTypes).forall{ case (arg, param) =>
          arg.isAssignableFrom(param)
        }
        parameterCountMatches && boxedParameterTypesMatches
      }
    }.map{m =>
      m.setAccessible(true)
      BoxedVersionMethodFound(m)
    }
    maybeUnboxedMethod.orElse(maybeBoxedMethod).getOrElse(NoMethodFound)
  }

  def findConstructor(target: Class[_], params: Array[Value]): ConstructorSearchResult = {
    val constructors = target.getConstructors
    val maybeUnboxedConstructor = constructors.find{c =>
      val parameterCountMatches = c.getParameterCount == params.length
      val unboxedParameterTypes = Value.classesOfValues(params)
      val parameterTypesMatches  = (c.getParameterTypes zip unboxedParameterTypes).forall{ case (arg, param) =>
        arg.isAssignableFrom(param)
      }
      parameterCountMatches && parameterTypesMatches
    }.map{c =>
      UnboxedVersionConstructorFound(c)
    }
    val maybeBoxedConstructor = {
      constructors.find{c =>
        val parameterCountMatches = c.getParameterCount == params.length
        val boxedParameterTypes = Value.boxedClassesOfValues(params)
        val parameterTypesMatches  = (c.getParameterTypes zip boxedParameterTypes).forall{ case (arg, param) =>
          arg.isAssignableFrom(param)
        }
        parameterCountMatches && parameterTypesMatches
      }
    }.map { c =>
      BoxedVersionConstructorFound(c)
    }
    maybeUnboxedConstructor.orElse(maybeBoxedConstructor).getOrElse(NoConstructorFound)
  }

  object BuiltinEnvironment extends RuntimeEnvironment(None) {
    define("substring"){ case List(ObjectValue(s:String), begin: BoxedInt, end: BoxedInt) =>
      ObjectValue(s.substring(begin.value, end.value))
    }

    define("at") { case List(ObjectValue(s:String), index: BoxedInt) =>
      ObjectValue(s.substring(index.value, index.value + 1))
    }

    define("matches") { case List(ObjectValue(s: String), ObjectValue(regex: String)) =>
      Value.boxBoolean(s.matches(regex))
    }

    define("sqrt") { case List(BoxedDouble(value)) =>
      BoxedDouble(math.sqrt(value))
    }

    define("int") { case List(BoxedDouble(value)) =>
      BoxedInt(value.toInt)
    }

    define("double") { case List(BoxedInt(value)) =>
      BoxedDouble(value.toDouble)
    }

    define("floor") { case List(BoxedDouble(value)) =>
      BoxedInt(value.toInt)
    }

    define("ceil") { case List(BoxedDouble(value)) =>
      BoxedInt(math.ceil(value).toInt)
    }

    define("abs") { case List(BoxedDouble(value)) =>
      BoxedDouble(math.abs(value))
    }

    define("thread") { case List(fun: CallableValue) =>
      fun match {
        case FunctionValue(value, _, environment) =>
          new Thread({() =>
            val env = RuntimeEnvironment.pooled(environment)
            interpreter.evaluate(TypedAst.FunctionCall(TDynamic, NoLocation, value, Nil), env)
            ()
          }).start()
          UnitValue
        case closure: VmClosureValue =>
          new Thread({() =>
            executeVmClosure(closure, List())
            ()
          }).start()
          UnitValue
        case _ =>
          throw new RuntimeException("unexpected callable value type")
      }
    }

    define("println") { case List(param) =>
      println(param)
      UnitValue
    }

    define("printlnError") { case List(param) =>
      Console.err.println(param)
      UnitValue
    }

    define("stopwatch") { case List(fun: CallableValue) =>
      fun match {
        case FunctionValue(value, _, environment) =>
          val env = RuntimeEnvironment.pooled(environment)
          val start = System.currentTimeMillis()
          interpreter.evaluate(TypedAst.FunctionCall(TDynamic, NoLocation, value, List()), env)
          val end = System.currentTimeMillis()
          BoxedInt((end - start).toInt)
        case closure: VmClosureValue =>
          val start = System.currentTimeMillis()
          executeVmClosure(closure, List())
          val end = System.currentTimeMillis()
          BoxedInt((end - start).toInt)
        case _ =>
          throw new RuntimeException("unexpected callable value type")
      }
    }
    define("sleep"){ case List(milliseconds: BoxedInt) =>
      Thread.sleep(milliseconds.value)
      UnitValue
    }

    define("map") { case List(ObjectValue(list: java.util.List[_])) =>
      NativeFunctionValue{
        case List(fun: FunctionValue) =>
          val newList = new java.util.ArrayList[Any]
          val env = RuntimeEnvironment.pooled(fun.environment)
          var i = 0
          while(i < list.size()) {
            val param: Value = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
            val result: Value = performFunctionInternal(fun.value, List(ValueNode(param)), env)
            newList.add(Value.fromKlassic(result))
            i += 1
          }
          ObjectValue(newList)
        case List(closure: VmClosureValue) =>
          val newList = new java.util.ArrayList[Any]
          var i = 0
          while(i < list.size()) {
            val param: Value = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
            val result: Value = executeVmClosure(closure, List(param))
            newList.add(Value.fromKlassic(result))
            i += 1
          }
          ObjectValue(newList)
      }
    }

    define("assert") { case List(BoxedBoolean(condition)) =>
        if(!condition) throw AssertionError("assertion failure") else UnitValue
    }

    define("assertResult") { case List(a: Value) =>
      NativeFunctionValue{
        case List(b: Value) =>
          if(a != b) throw AssertionError(s"expected: ${a}, actual: ${b}") else UnitValue
      }
    }

    define("head") { case List(ObjectValue(list: java.util.List[_])) =>
      Value.toKlassic(list.get(0).asInstanceOf[AnyRef])
    }
    define("tail") { case List(ObjectValue(list: java.util.List[_])) =>
      Value.toKlassic(list.subList(1, list.size()))
    }
    define("cons") { case List(value: Value) =>
      NativeFunctionValue{ case List(ObjectValue(list: java.util.List[_])) =>
        val newList = new java.util.ArrayList[Any](list.size() + 1)
        newList.add(Value.fromKlassic(value))
        newList.addAll(list)
        Value.toKlassic(newList)
      }
    }
    define("size") { case List(ObjectValue(list: java.util.List[_])) =>
      BoxedInt(list.size())
    }
    define("isEmpty") { case List(ObjectValue(list: java.util.List[_])) =>
      Value.boxBoolean(list.isEmpty)
    }
    define("ToDo") { case Nil =>
      throw NotImplementedError("not implemented yet")
    }
    define("url") { case List(ObjectValue(value: String)) =>
      ObjectValue(new java.net.URL(value))
    }
    define("uri") { case List(ObjectValue(value: String)) =>
      ObjectValue(new java.net.URL(value).toURI)
    }
    define("foldLeft") { case List(ObjectValue(list: java.util.List[_])) =>
      NativeFunctionValue{ case List(init: Value) =>
        NativeFunctionValue { case List(fun: CallableValue) =>
          fun match {
            case FunctionValue(value, _, environment) =>
              val env = RuntimeEnvironment.pooled(environment)
              var i = 0
              var result: Value = init
              while(i < list.size()) {
                val params: List[TypedNode] = List(ValueNode(result), ValueNode(Value.toKlassic(list.get(i).asInstanceOf[AnyRef])))
                result = performFunctionInternal(value, params, env)
                i += 1
              }
              result
            case closure: VmClosureValue =>
              var i = 0
              var result: Value = init
              while(i < list.size()) {
                val element = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
                result = executeVmClosure(closure, List(result, element))
                i += 1
              }
              result
            case _ =>
              throw new RuntimeException("unexpected callable value type")
          }
        }
      }
    }
    define("desktop") { case Nil =>
      ObjectValue(java.awt.Desktop.getDesktop())
    }
    defineValue("null")(
      ObjectValue(null)
    )
  }

  object BuiltinRecordEnvironment extends RecordEnvironment() {
    define("Point")(
      "x" -> TInt,
      "y" -> TInt
    )
    define("InStream")(
      "core" -> TDynamic
    )
    define("OutStream")(
      "core" -> TDynamic
    )
  }

  object BuiltinModuleEnvironment extends ModuleEnvironment() {
    private final val LIST         = "List"
    private final val MAP          = "Map"
    private final val SET          = "Set"
    private final val GPIO         = "GPIO"
    private final val FILE_INPUT   = "FileInput"

    enter(LIST) {
      define("head") { case List(ObjectValue(list: java.util.List[_])) =>
        Value.toKlassic(list.get(0).asInstanceOf[AnyRef])
      }
      define("tail") { case List(ObjectValue(list: java.util.List[_])) =>
        Value.toKlassic(list.subList(1, list.size()))
      }
      define("cons") { case List(value: Value) =>
        NativeFunctionValue { case List(ObjectValue(list: java.util.List[_])) =>
          val newList = new java.util.ArrayList[Any](list.size() + 1)
          newList.add(Value.fromKlassic(value))
          newList.addAll(list)
          Value.toKlassic(newList)
        }
      }
      define("remove") { case List(ObjectValue(self: java.util.List[_])) =>
        NativeFunctionValue{ case List(a: Value) =>
          val newList = new java.util.ArrayList[Any](self)
          newList.remove(Value.fromKlassic(a))
          ObjectValue(newList)
        }
      }
      define("size") { case List(ObjectValue(list: java.util.List[_])) =>
        BoxedInt(list.size())
      }
      define("isEmpty") { case List(ObjectValue(list: java.util.List[_])) =>
        Value.boxBoolean(list.isEmpty)
      }
      define("map") { case List(ObjectValue(list: java.util.List[_])) =>
        NativeFunctionValue{
          case List(fun: CallableValue) =>
            fun match {
              case FunctionValue(value, _, environment) =>
                val newList = new java.util.ArrayList[Any]
                val env = RuntimeEnvironment.pooled(environment)
                var i = 0
                while(i < list.size()) {
                  val param: Value = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
                  val result: Value = performFunctionInternal(value, List(ValueNode(param)), env)
                  newList.add(Value.fromKlassic(result))
                  i += 1
                }
                ObjectValue(newList)
              case closure: VmClosureValue =>
                val newList = new java.util.ArrayList[Any]
                var i = 0
                while(i < list.size()) {
                  val param: Value = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
                  val result: Value = executeVmClosure(closure, List(param))
                  newList.add(Value.fromKlassic(result))
                  i += 1
                }
                ObjectValue(newList)
              case _ =>
                throw new RuntimeException("unexpected callable value type")
            }
        }
      }
      define("foldLeft") { case List(ObjectValue(list: java.util.List[_])) =>
        NativeFunctionValue{ case List(init: Value) =>
          NativeFunctionValue { case List(fun: CallableValue) =>
            fun match {
              case FunctionValue(value, _, environment) =>
                val env = RuntimeEnvironment.pooled(environment)
                var i = 0
                var result: Value = init
                while(i < list.size()) {
                  val params: List[TypedNode] = List(ValueNode(result), ValueNode(Value.toKlassic(list.get(i).asInstanceOf[AnyRef])))
                  result = performFunctionInternal(value, params, env)
                  i += 1
                }
                result
              case closure: VmClosureValue =>
                val newList = new java.util.ArrayList[Any]
                var i = 0
                while(i < list.size()) {
                  val param: Value = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
                  val result: Value = executeVmClosure(closure, List(param))
                  newList.add(Value.fromKlassic(result))
                  i += 1
                }
                ObjectValue(newList)
              case _ =>
                throw new RuntimeException("unexpected callable value type")
            }
          }
        }
      }
    }
    enter(FILE_INPUT) {
      define("open") { case List(ObjectValue(path: String)) =>
        val reader = Files.newBufferedReader(Path.of(path))
        NativeFunctionValue { case List(fun: CallableValue) =>
          fun match {
            case FunctionValue(value, _, environment) =>
              val env = RuntimeEnvironment.pooled(environment)
              val reader = Files.newBufferedReader(Path.of(path))
              try {
                val inStream = RecordValue("InStream", List("core" -> ObjectValue(reader)))
                val params: List[TypedNode] = List(ValueNode(inStream))
                performFunctionInternal(value, params, env)
              } finally {
                reader.close()
              }
            case closure: VmClosureValue =>
              val reader = Files.newBufferedReader(Path.of(path))
              try {
                val inStream = RecordValue("InStream", List("core" -> ObjectValue(reader)))
                executeVmClosure(closure, List(inStream))
              } finally {
                reader.close()
              }
            case _ =>
              throw new RuntimeException("unexpected callable value type")
          }
        }
      }
      define("all") { case List(ObjectValue(path: String)) =>
        val result = Files.readString(Path.of(path))
        Value.toKlassic(result)
      }
      define("lines") { case List(ObjectValue(path: String)) =>
        val result = Files.lines(Path.of(path)).collect(Collectors.toList())
        Value.toKlassic(result)
      }
      define("readLines") { case List(inStream: RecordValue) =>
        val reader = Value.fromKlassic(inStream.members.find( { case (name, value) => name == "core"}).get._2).asInstanceOf[BufferedReader]
        val newList = new util.ArrayList[String]
        var line: String = null
        while({line = reader.readLine(); line } != null) {
          newList.add(line)
        }
        Value.toKlassic(newList)
      }
      define("readAll") { case List(inStream: RecordValue) =>
        val reader = Value.fromKlassic(inStream.members.find( { case (name, value) => name == "core"}).get._2).asInstanceOf[BufferedReader]
        val builder = new StringBuilder
        var ch: Int = -1
        while({ch = reader.read(); ch } != -1) {
          builder.append(ch.asInstanceOf[Char])
        }
        Value.toKlassic((builder.toString()))
      }
    }
    enter(GPIO) {
      define("pin") { case List(BoxedInt(pinNumber)) =>
        ObjectValue(pinNumber match {
          case 0 => RaspiPin.GPIO_00
          case 1 => RaspiPin.GPIO_01
          case 2 => RaspiPin.GPIO_02
          case 3 => RaspiPin.GPIO_03
          case 4 => RaspiPin.GPIO_04
          case 5 => RaspiPin.GPIO_05
          case 6 => RaspiPin.GPIO_06
          case 7 => RaspiPin.GPIO_07
          case 8 => RaspiPin.GPIO_08
          case 9 => RaspiPin.GPIO_09
          case 10 => RaspiPin.GPIO_10
          case 11 => RaspiPin.GPIO_11
          case 12 => RaspiPin.GPIO_12
          case 13 => RaspiPin.GPIO_13
          case 14 => RaspiPin.GPIO_14
          case 15 => RaspiPin.GPIO_15
          case 16 => RaspiPin.GPIO_16
          case 17 => RaspiPin.GPIO_17
          case 18 => RaspiPin.GPIO_18
          case 19 => RaspiPin.GPIO_19
          case 20 => RaspiPin.GPIO_20
          case 21 => RaspiPin.GPIO_21
          case 22 => RaspiPin.GPIO_22
          case 23 => RaspiPin.GPIO_23
          case 24 => RaspiPin.GPIO_24
          case 25 => RaspiPin.GPIO_25
          case 26 => RaspiPin.GPIO_26
          case 27 => RaspiPin.GPIO_27
          case 28 => RaspiPin.GPIO_28
          case 29 => RaspiPin.GPIO_29
          case 30 => RaspiPin.GPIO_30
          case 31 => RaspiPin.GPIO_31
        })
      }
      define("setup") { case List() =>
        Gpio.wiringPiSetupGpio()
        ObjectValue(GpioFactory.getInstance())
      }
      define("inputOf") { case List(ObjectValue(gpio:GpioController), ObjectValue(pin:Pin)) =>
        val input: GpioPinDigitalInput = gpio.provisionDigitalInputPin(pin.asInstanceOf[Pin])
        ObjectValue(input)
      }
      define("outputOf") { case List(ObjectValue(gpio: GpioController), ObjectValue(pin: Pin), BoxedBoolean(high)) =>
        val state = if (high) PinState.HIGH else PinState.LOW
        val output: GpioPinDigitalOutput = gpio.provisionDigitalOutputPin(pin, state)
        ObjectValue(output)
      }
      define("toggle") { case List(ObjectValue(output:GpioPinDigitalOutput)) =>
        output.toggle()
        UnitValue
      }
      define("toHigh") { case List(ObjectValue(output:GpioPinDigitalOutput)) =>
        output.high()
        UnitValue
      }
      define("toLow") { case List(ObjectValue(output:GpioPinDigitalOutput)) =>
        output.low()
        UnitValue
      }
      define("isHigh") { case List(ObjectValue(input:GpioPinDigitalInput)) =>
        Value.boxBoolean(input.isHigh)
      }
      define("isLow") { case List(ObjectValue(input:GpioPinDigitalInput)) =>
        Value.boxBoolean(input.isLow)
      }
    }
    enter(MAP) {
      define("add") { case List(ObjectValue(self: java.util.Map[_, _])) =>
        NativeFunctionValue{ case List(a: Value, b: Value) =>
          val newMap = new java.util.HashMap[Any, Any](self)
          newMap.put(Value.fromKlassic(a), Value.fromKlassic(b))
          ObjectValue(newMap)
        }
      }
      define("containsKey") { case List(ObjectValue(self: java.util.Map[_, _])) =>
        NativeFunctionValue{ case List(k: Value) =>
          Value.boxBoolean(self.containsKey(Value.fromKlassic(k)))
        }
      }
      define("containsValue") { case List(ObjectValue(self: java.util.Map[_, _])) =>
        NativeFunctionValue{ case List(v: Value) =>
          Value.boxBoolean(self.containsValue(Value.fromKlassic(v)))
        }
      }
      define("get") { case List(ObjectValue(self: java.util.Map[_, _])) =>
        NativeFunctionValue{ case List(k: Value) =>
          Value.toKlassic(self.get(Value.fromKlassic(k)).asInstanceOf[AnyRef])
        }
      }
      define("size") { case List(ObjectValue(self: java.util.Map[_, _])) =>
        BoxedInt(self.size())
      }
      define("isEmpty") { case List(ObjectValue(map: java.util.Map[_, _])) =>
        Value.boxBoolean(map.isEmpty)
      }
    }
    enter(SET) {
      define("add") { case List(ObjectValue(self: java.util.Set[_])) =>
        NativeFunctionValue{ case List(a: Value) =>
          val newSet = new java.util.HashSet[Any](self)
          newSet.add(Value.fromKlassic(a))
          ObjectValue(newSet)
        }
      }
      define("remove") { case List(ObjectValue(self: java.util.Set[_])) =>
        NativeFunctionValue{ case List(a: Value) =>
          val newSet = new java.util.HashSet[Any](self)
          newSet.remove(Value.fromKlassic(a))
          ObjectValue(newSet)
        }
      }
      define("contains") { case List(ObjectValue(self: java.util.Set[_])) =>
        NativeFunctionValue { case List(a: Value) =>
          Value.boxBoolean(self.contains(Value.fromKlassic(a)))
        }
      }
      define("size") { case List(ObjectValue(self: java.util.Set[_])) =>
        BoxedInt(self.size())
      }
      define("isEmpty") { case List(ObjectValue(self: java.util.Set[_])) =>
        Value.boxBoolean(self.isEmpty)
      }
    }
  }

  def toList(row: Type): List[(String, Type)] = row match {
    case tv@TVariable(_) => sys.error("cannot reach here")
    case TRowExtend(l, t, extension) => (l -> t) :: toList(extension)
    case TRowEmpty => Nil
    case otherwise => throw TyperPanic("Unexpected: " + otherwise)
  }

  final def interpret(program: TypedAst.Program, session: InteractiveSession): Value = {
    val runtimeRecordEnvironment: RecordEnvironment = BuiltinRecordEnvironment
    program.records.foreach { case (name, record) =>
      val members = toList(record.row)
      val rmembers = members.map { case (n, t) => n -> t }
      runtimeRecordEnvironment.records += (name -> rmembers)
    }
    interpreter.evaluate(program.block, env = BuiltinEnvironment, recordEnv = runtimeRecordEnvironment, moduleEnv = BuiltinModuleEnvironment)
  }

  private def evaluate(node: TypedNode): Value = {
    evaluate(node, BuiltinEnvironment)
  }

  private def performFunctionInternal(func: TypedNode, params: List[TypedNode], env: RuntimeEnvironment): Value = {
    performFunction(TypedAst.FunctionCall(TDynamic, NoLocation, func, params), env)
  }

  private def performFunction(node: TypedAst.FunctionCall, env: RuntimeEnvironment): Value = node match {
    case TypedAst.FunctionCall(type_, location, function, params) =>
      evaluate(function, env) match {
        case FunctionValue(TypedAst.FunctionLiteral(type_, location, fparams, optionalType, proc), cleanup, cenv) =>
          val local = RuntimeEnvironment.pooled(cenv)
          (fparams zip params).foreach{ case (fp, ap) =>
            local(fp.name) = evaluate(ap, env)
          }
          try {
            evaluate(proc, local)
          } finally {
            cleanup.foreach { expression =>
              evaluate(expression, local)
            }
          }
        case NativeFunctionValue(body) =>
          val actualParams = params.map{p => evaluate(p, env)}
          if(body.isDefinedAt(actualParams)) {
            body(actualParams)
          } else {
            reportError("parameters are not matched to the function's arguments")
          }
        case _ =>
          reportError("unknown error")
      }
  }

  private def evaluate(node: TypedNode, env: RuntimeEnvironment, recordEnv: RecordEnvironment = BuiltinRecordEnvironment, moduleEnv: ModuleEnvironment = BuiltinModuleEnvironment): Value = {
    def evalRecursive(node: TypedNode): Value = {
      node match{
        case TypedAst.Block(type_, location, expressions) =>
          val local = RuntimeEnvironment.pooled(Some(env))
          expressions.foldLeft(UnitValue:Value){(result, x) => evaluate(x, local)}
        case TypedAst.WhileExpression(type_, location, cond, body) =>
          while(evalRecursive(cond) == BoxedBoolean(true)) {
            evalRecursive(body)
          }
          UnitValue
        case TypedAst.IfExpression(type_, location, condition, pos, neg) =>
          evalRecursive(condition) match {
            case BoxedBoolean(true) => evalRecursive(pos)
            case BoxedBoolean(false) => evalRecursive(neg)
            case _ => reportError("type error")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.AND2, lhs, rhs) =>
          evalRecursive(lhs) match {
            case BoxedBoolean(true) => evalRecursive(rhs)
            case BoxedBoolean(false) => Value.boxBoolean(false)
            case _ => reportError("type error")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.BAR2, lhs, rhs) =>
          evalRecursive(lhs) match {
            case BoxedBoolean(false) => evalRecursive(rhs)
            case BoxedBoolean(true) => Value.boxBoolean(true)
            case _ => reportError("type error")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxBoolean(lval == rval)
            case (BoxedLong(lval), BoxedLong(rval)) => Value.boxBoolean(lval == rval)
            case (BoxedShort(lval), BoxedShort(rval)) => Value.boxBoolean(lval == rval)
            case (BoxedByte(lval), BoxedByte(rval)) => Value.boxBoolean(lval == rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => Value.boxBoolean(lval == rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => Value.boxBoolean(lval == rval)
            case (BoxedBoolean(lval), BoxedBoolean(rval)) => Value.boxBoolean(lval == rval)
            case (BoxedBoolean(lval), ObjectValue(rval:java.lang.Boolean)) => Value.boxBoolean(lval == rval.booleanValue())
            case (ObjectValue(lval:java.lang.Boolean), BoxedBoolean(rval)) => Value.boxBoolean(lval.booleanValue() == rval)
            case (ObjectValue(lval), ObjectValue(rval)) => Value.boxBoolean(lval == rval)
            case _ => reportError("comparation must be done between same types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.LESS_THAN, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxBoolean(lval < rval)
            case (BoxedLong(lval), BoxedLong(rval)) => Value.boxBoolean(lval < rval)
            case (BoxedShort(lval), BoxedShort(rval)) => Value.boxBoolean(lval < rval)
            case (BoxedByte(lval), BoxedByte(rval)) => Value.boxBoolean(lval < rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => Value.boxBoolean(lval < rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => Value.boxBoolean(lval < rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.GREATER_THAN, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxBoolean(lval > rval)
            case (BoxedLong(lval), BoxedLong(rval)) => Value.boxBoolean(lval > rval)
            case (BoxedShort(lval), BoxedShort(rval)) => Value.boxBoolean(lval > rval)
            case (BoxedByte(lval), BoxedByte(rval)) => Value.boxBoolean(lval > rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => Value.boxBoolean(lval > rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => Value.boxBoolean(lval > rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.LESS_OR_EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxBoolean(lval <= rval)
            case (BoxedLong(lval), BoxedLong(rval)) => Value.boxBoolean(lval <= rval)
            case (BoxedShort(lval), BoxedShort(rval)) => Value.boxBoolean(lval <= rval)
            case (BoxedByte(lval), BoxedByte(rval)) => Value.boxBoolean(lval <= rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => Value.boxBoolean(lval <= rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => Value.boxBoolean(lval <= rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.GREATER_EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxBoolean(lval >= rval)
            case (BoxedLong(lval), BoxedLong(rval)) => Value.boxBoolean(lval >= rval)
            case (BoxedShort(lval), BoxedShort(rval)) => Value.boxBoolean(lval >= rval)
            case (BoxedByte(lval), BoxedByte(rval)) => Value.boxBoolean(lval >= rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => Value.boxBoolean(lval >= rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => Value.boxBoolean(lval >= rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.ADD, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxInt(lval + rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval + rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval + rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval + rval).toByte)
            case (ObjectValue(lval:String), rval) => ObjectValue(lval + rval)
            case (lval, ObjectValue(rval:String)) => ObjectValue(lval.toString + rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedFloat((lval + rval))
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedDouble(lval + rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.SUBTRACT, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxInt(lval - rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval - rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval - rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval - rval).toByte)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedFloat((lval - rval))
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedDouble(lval - rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.MULTIPLY, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxInt(lval * rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval * rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval * rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval * rval).toByte)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedFloat((lval * rval))
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedDouble(lval * rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.DIVIDE, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxInt(lval / rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval / rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval / rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval / rval).toByte)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedFloat((lval / rval))
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedDouble(lval / rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.AND, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxInt(lval & rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval & rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval & rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval & rval).toByte)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.OR, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxInt(lval | rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval | rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval | rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval | rval).toByte)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.XOR, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => Value.boxInt(lval ^ rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval ^ rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval ^ rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval ^ rval).toByte)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case TypedAst.MinusOp(type_, location, operand) =>
          evalRecursive(operand) match {
            case BoxedInt(value) => Value.boxInt(-value)
            case BoxedLong(value) => BoxedLong(-value)
            case BoxedShort(value) => BoxedShort((-value).toShort)
            case BoxedByte(value) => BoxedByte((-value).toByte)
            case BoxedFloat(value) => BoxedFloat(-value)
            case BoxedDouble(value) => BoxedDouble(-value)
            case _ => reportError("- cannot be applied to non-integer value")
          }
        case TypedAst.PlusOp(type_, location, operand) =>
          evalRecursive(operand) match {
            case BoxedInt(value) => Value.boxInt(value)
            case BoxedLong(value) => BoxedLong(value)
            case BoxedShort(value) => BoxedShort(value)
            case BoxedByte(value) => BoxedByte(value)
            case BoxedFloat(value) => BoxedFloat(value)
            case BoxedDouble(value) => BoxedDouble(value)
            case _ => reportError("+ cannot be applied to non-integer value")
          }
        case TypedAst.IntNode(type_, location, value) =>
          Value.boxInt(value)
        case TypedAst.StringNode(type_, location, value) =>
          ObjectValue(value)
        case TypedAst.LongNode(type_, location, value) =>
          BoxedLong(value)
        case TypedAst.ShortNode(type_, location, value) =>
          BoxedShort(value)
        case TypedAst.ByteNode(type_, location, value) =>
          BoxedByte(value)
        case TypedAst.DoubleNode(type_, location, value) =>
          BoxedDouble(value)
        case TypedAst.FloatNode(type_, location, value) =>
          BoxedFloat(value)
        case TypedAst.BooleanNode(type_, location, value) =>
          Value.boxBoolean(value)
        case TypedAst.UnitNode(type_, location) =>
          UnitValue
        case TypedAst.ListLiteral(type_, location, elements) =>
          val params = elements.map{e => Value.fromKlassic(evalRecursive(e))}
          val newList = new java.util.ArrayList[Any]
          params.foreach{param =>
            newList.add(param)
          }
          ObjectValue(newList)
        case TypedAst.SetLiteral(type_, location, elements) =>
          val params = elements.map{e => Value.fromKlassic(evalRecursive(e))}
          val newSet = new java.util.HashSet[Any]
          params.foreach{param =>
            newSet.add(param)
          }
          ObjectValue(newSet)
        case TypedAst.MapLiteral(type_, location, elements) =>
          val params = elements.map{ case (k, v) =>
            (Value.fromKlassic(evalRecursive(k)), Value.fromKlassic(evalRecursive(v)))
          }
          val newMap = new java.util.HashMap[Any, Any]
          params.foreach{ case (k, v) =>
            newMap.put(k, v)
          }
          ObjectValue(newMap)
        case TypedAst.Id(type_, location, name) =>
          env(name)
        case TypedAst.Selector(type_, location, module, name) =>
          moduleEnv(module)(name)
        case TypedAst.LetDeclaration(type_, location, vr, optVariableType, value, body, immutable) =>
          env(vr) = evalRecursive(value)
          evalRecursive(body)
        case TypedAst.Assignment(type_, location, vr, value) =>
          env.set(vr, evalRecursive(value))
        case literal@TypedAst.FunctionLiteral(type_, location, _, _, _) =>
          FunctionValue(literal, None, Some(env))
        case TypedAst.LetFunctionDefinition(type_, location, name, body, cleanup, expression) =>
          env(name) = FunctionValue(body, cleanup, Some(env)): Value
          evalRecursive(expression)
        case TypedAst.MethodCall(type_, location, self, name, params) =>
          evalRecursive(self) match {
            case ObjectValue(value) =>
              val paramsArray = params.map{p => evalRecursive(p)}.toArray
              findMethod(value, name, paramsArray) match {
                case UnboxedVersionMethodFound(method) =>
                  val actualParams = paramsArray.map{Value.fromKlassic}
                  Value.toKlassic(method.invoke(value, actualParams:_*))
                case BoxedVersionMethodFound(method) =>
                  val actualParams = paramsArray.map{Value.fromKlassic}
                  Value.toKlassic(method.invoke(value, actualParams:_*))
                case NoMethodFound =>
                  throw new IllegalArgumentException(s"${self}.${name}(${params})")
              }
            case otherwise =>
              sys.error(s"cannot reach here: ${otherwise}")
          }
        case TypedAst.ObjectNew(type_, location, className, params) =>
          val paramsArray = params.map{evalRecursive}.toArray
          findConstructor(Class.forName(className), paramsArray) match {
            case UnboxedVersionConstructorFound(constructor) =>
              val actualParams = paramsArray.map{Value.fromKlassic}
              Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
            case BoxedVersionConstructorFound(constructor) =>
              val actualParams = paramsArray.map{Value.fromKlassic}
              Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
            case NoConstructorFound =>
              throw new IllegalArgumentException(s"new ${className}(${params}) is not found")
          }
        case TypedAst.RecordNew(type_, location, recordName, params) =>
          val paramsList = params.map{evalRecursive}
          recordEnv.records.get(recordName) match {
            case None => throw new IllegalArgumentException(s"record ${recordName} is not found")
            case Some(argsList) =>
              val members = (argsList zip paramsList).map{ case ((n, _), v) => n -> v }
              RecordValue(recordName, members)
          }
        case TypedAst.RecordSelect(type_, location, expression, memberName) =>
          evalRecursive(expression) match {
            case RecordValue(recordName, members) =>
              members.find{ case (mname, mtype) => memberName == mname} match {
                case None =>
                  throw new IllegalArgumentException(s"member ${memberName} is not found in record ${recordName}")
                case Some((_, value)) =>
                  value
              }
            case v =>
              throw new IllegalArgumentException(s"value ${v} is not record")
          }
        case call@TypedAst.FunctionCall(type_, location, function, params) =>
          performFunction(call, env)
        case TypedAst.Casting(type_, location, target, to) =>
          evalRecursive(target)
        case TypedAst.ValueNode(value) =>
          value
        case otherwise@TypedAst.ForeachExpression(type_, location, _, _, _) => sys.error(s"cannot reach here: ${otherwise}")
      }
    }
    evalRecursive(node)
  }

  override final val name: String = "Interpreter"

  override final def process(input: TypedAst.Program, session: InteractiveSession): Value = {
    interpret(input, session)
  }
}
