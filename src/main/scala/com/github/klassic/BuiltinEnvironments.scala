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

object BuiltinEnvironments {
  
  def reportError(message: String): Nothing = {
    throw InterpreterException(message)
  }

  def toList(row: Type): List[(String, Type)] = row match {
    case tv@TVariable(_, _) => sys.error("cannot reach here")
    case TRowExtend(l, t, extension) => (l -> t) :: toList(extension)
    case TRowEmpty => Nil
    case otherwise => throw TyperPanic("Unexpected: " + otherwise)
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
        case closure: VmClosureValue =>
          new Thread({() =>
            val VmClosureValue(params, bodyStart, bodyEnd, closureEnv, instructions) = closure
            val virtualMachine = new vm.VirtualMachine(BuiltinModuleEnvironment, BuiltinRecordEnvironment)
            val newEnv = RuntimeEnvironment.pooled(Some(closureEnv))
            val closureCode = instructions.slice(bodyStart, bodyEnd + 1)
            virtualMachine.run(closureCode, newEnv)
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
        case closure: VmClosureValue =>
          val start = System.currentTimeMillis()
          val VmClosureValue(params, bodyStart, bodyEnd, closureEnv, instructions) = closure
          val virtualMachine = new vm.VirtualMachine(BuiltinModuleEnvironment, BuiltinRecordEnvironment)
          val newEnv = RuntimeEnvironment.pooled(Some(closureEnv))
          val closureCode = instructions.slice(bodyStart, bodyEnd + 1)
          virtualMachine.run(closureCode, newEnv)
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
        case List(closure: VmClosureValue) =>
          val newList = new java.util.ArrayList[Any]
          var i = 0
          while(i < list.size()) {
            val param: Value = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
            val VmClosureValue(params, bodyStart, bodyEnd, closureEnv, instructions) = closure
            val virtualMachine = new vm.VirtualMachine(BuiltinModuleEnvironment, BuiltinRecordEnvironment)
            val newEnv = RuntimeEnvironment.pooled(Some(closureEnv))
            params.headOption.foreach { p => newEnv.update(p, param) }
            val closureCode = instructions.slice(bodyStart, bodyEnd + 1)
            val result = virtualMachine.run(closureCode, newEnv)
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
            case closure: VmClosureValue =>
              var i = 0
              var result: Value = init
              while(i < list.size()) {
                val element = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
                val VmClosureValue(params, bodyStart, bodyEnd, closureEnv, instructions) = closure
                val virtualMachine = new vm.VirtualMachine(BuiltinModuleEnvironment, BuiltinRecordEnvironment)
                val newEnv = RuntimeEnvironment.pooled(Some(closureEnv))
                if (params.size >= 2) {
                  newEnv.update(params(0), result)
                  newEnv.update(params(1), element)
                }
                val closureCode = instructions.slice(bodyStart, bodyEnd + 1)
                result = virtualMachine.run(closureCode, newEnv)
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
          case List(closure: VmClosureValue) =>
            val newList = new java.util.ArrayList[Any]
            var i = 0
            while(i < list.size()) {
              val param: Value = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
              val VmClosureValue(params, bodyStart, bodyEnd, closureEnv, instructions) = closure
              val virtualMachine = new vm.VirtualMachine(BuiltinModuleEnvironment, BuiltinRecordEnvironment)
              val newEnv = RuntimeEnvironment.pooled(Some(closureEnv))
              params.headOption.foreach { p => newEnv.update(p, param) }
              val closureCode = instructions.slice(bodyStart, bodyEnd + 1)
              val result = virtualMachine.run(closureCode, newEnv)
              newList.add(Value.fromKlassic(result))
              i += 1
            }
            ObjectValue(newList)
        }
      }
      define("foldLeft") { case List(ObjectValue(list: java.util.List[_])) =>
        NativeFunctionValue{ case List(init: Value) =>
          NativeFunctionValue { case List(fun: CallableValue) =>
            fun match {
              case closure: VmClosureValue =>
                var i = 0
                var result: Value = init
                while(i < list.size()) {
                  val element = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
                  val VmClosureValue(params, bodyStart, bodyEnd, closureEnv, instructions) = closure
                  val virtualMachine = new vm.VirtualMachine(BuiltinModuleEnvironment, BuiltinRecordEnvironment)
                  val newEnv = RuntimeEnvironment.pooled(Some(closureEnv))
                  if (params.size >= 2) {
                    newEnv.update(params(0), result)
                    newEnv.update(params(1), element)
                  }
                  val closureCode = instructions.slice(bodyStart, bodyEnd + 1)
                  result = virtualMachine.run(closureCode, newEnv)
                  i += 1
                }
                result
              case _ =>
                throw new RuntimeException("unexpected callable value type")
            }
          }
        }
      }
    }
    enter(FILE_INPUT) {
      define("open") { case List(ObjectValue(path: String)) =>
        NativeFunctionValue { case List(fun: CallableValue) =>
          fun match {
            case closure: VmClosureValue =>
              val reader = Files.newBufferedReader(Path.of(path))
              try {
                val inStream = RecordValue("InStream", List("core" -> ObjectValue(reader)))
                val VmClosureValue(params, bodyStart, bodyEnd, closureEnv, instructions) = closure
                val virtualMachine = new vm.VirtualMachine(BuiltinModuleEnvironment, BuiltinRecordEnvironment)
                val newEnv = RuntimeEnvironment.pooled(Some(closureEnv))
                params.headOption.foreach { p => newEnv.update(p, inStream) }
                val closureCode = instructions.slice(bodyStart, bodyEnd + 1)
                virtualMachine.run(closureCode, newEnv)
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
}