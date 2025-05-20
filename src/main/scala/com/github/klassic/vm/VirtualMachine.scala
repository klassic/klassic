package com.github.klassic.vm

import scala.collection.mutable
import com.github.klassic._

class VirtualMachine {
  private val stack = mutable.Stack[Value]()

  def run(code: Vector[Instruction], env: RuntimeEnvironment = new RuntimeEnvironment(None)): Value = {
    var pc = 0
    var running = true
    var result: Value = UnitValue
    while (pc < code.length && running) {
      code(pc) match {
        case Push(v) =>
          stack.push(v)
          pc += 1
        case Load(name) =>
          stack.push(env(name))
          pc += 1
        case Store(name) =>
          env.update(name, stack.pop())
          pc += 1
        case Add =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedInt(x + y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x + y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x + y).toShort)
            case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x + y).toByte)
            case (BoxedFloat(x), BoxedFloat(y)) => BoxedFloat(x + y)
            case (BoxedDouble(x), BoxedDouble(y)) => BoxedDouble(x + y)
            case _ => throw new RuntimeException("type error on add")
          }
          stack.push(r)
          pc += 1
        case Sub =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedInt(x - y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x - y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x - y).toShort)
            case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x - y).toByte)
            case (BoxedFloat(x), BoxedFloat(y)) => BoxedFloat(x - y)
            case (BoxedDouble(x), BoxedDouble(y)) => BoxedDouble(x - y)
            case _ => throw new RuntimeException("type error on sub")
          }
          stack.push(r)
          pc += 1
        case Mul =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedInt(x * y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x * y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x * y).toShort)
            case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x * y).toByte)
            case (BoxedFloat(x), BoxedFloat(y)) => BoxedFloat(x * y)
            case (BoxedDouble(x), BoxedDouble(y)) => BoxedDouble(x * y)
            case _ => throw new RuntimeException("type error on mul")
          }
          stack.push(r)
          pc += 1
        case Div =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedInt(x / y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x / y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x / y).toShort)
            case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x / y).toByte)
            case (BoxedFloat(x), BoxedFloat(y)) => BoxedFloat(x / y)
            case (BoxedDouble(x), BoxedDouble(y)) => BoxedDouble(x / y)
            case _ => throw new RuntimeException("type error on div")
          }
          stack.push(r)
          pc += 1
        case Neg =>
          val a = stack.pop()
          val r = a match {
            case BoxedInt(x) => BoxedInt(-x)
            case BoxedLong(x) => BoxedLong(-x)
            case BoxedShort(x) => BoxedShort((-x).toShort)
            case BoxedByte(x) => BoxedByte((-x).toByte)
            case BoxedFloat(x) => BoxedFloat(-x)
            case BoxedDouble(x) => BoxedDouble(-x)
            case _ => throw new RuntimeException("type error on neg")
          }
          stack.push(r)
          pc += 1
        case Jump(target) =>
          pc = target
        case JumpIfFalse(target) =>
          stack.pop() match {
            case BoxedBoolean(false) => pc = target
            case BoxedBoolean(true) => pc += 1
            case _ => throw new RuntimeException("condition must be boolean")
          }
        case Return =>
          result = if (stack.nonEmpty) stack.pop() else UnitValue
          running = false
        case PushFunction(lit) =>
          stack.push(FunctionValue(lit, None, Some(env)))
          pc += 1
        case Call(n) =>
          val args = Array.fill[Value](n)(UnitValue)
          var i = n - 1
          while(i >= 0){
            args(i) = stack.pop(); i -= 1
          }
          stack.pop() match {
            case FunctionValue(lit, _, cenv) =>
              val local = new RuntimeEnvironment(cenv)
              (lit.params.map(_.name) zip args).foreach{ case (n, v) => local.update(n, v) }
              val code = new VmCompiler().compileFunction(lit)
              val r = new VirtualMachine().run(code, local)
              stack.push(r)
            case NativeFunctionValue(body) =>
              val argList = args.toList
              if(body.isDefinedAt(argList)) stack.push(body(argList))
              else throw new RuntimeException("parameters are not matched")
            case other => throw new RuntimeException(s"${other} is not function")
          }
          pc += 1
        case CallMethod(name, n) =>
          val args = Array.fill[Value](n)(UnitValue)
          var i = n - 1
          while(i >= 0){ args(i) = stack.pop(); i -= 1 }
          stack.pop() match {
            case ObjectValue(obj) =>
              val search = findMethod(obj, name, args)
              search match {
                case UnboxedVersionMethodFound(m) =>
                  val actual = args.map(Value.fromKlassic)
                  stack.push(Value.toKlassic(m.invoke(obj, actual:_*)))
                case BoxedVersionMethodFound(m) =>
                  val actual = args.map(Value.fromKlassic)
                  stack.push(Value.toKlassic(m.invoke(obj, actual:_*)))
                case NoMethodFound =>
                  throw new RuntimeException(s"method ${name} not found")
              }
            case other => throw new RuntimeException(s"${other} has no methods")
          }
          pc += 1
        case NewObject(className, n) =>
          val args = Array.fill[Value](n)(UnitValue)
          var i = n - 1
          while(i >= 0){ args(i) = stack.pop(); i -= 1 }
          findConstructor(Class.forName(className), args) match {
            case UnboxedVersionConstructorFound(cons) =>
              val actual = args.map(Value.fromKlassic)
              stack.push(Value.toKlassic(cons.newInstance(actual:_*).asInstanceOf[AnyRef]))
            case BoxedVersionConstructorFound(cons) =>
              val actual = args.map(Value.fromKlassic)
              stack.push(Value.toKlassic(cons.newInstance(actual:_*).asInstanceOf[AnyRef]))
            case NoConstructorFound =>
              throw new RuntimeException(s"constructor for ${className} not found")
          }
          pc += 1
        case LessThan =>
          val b = stack.pop(); val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedBoolean(x < y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedBoolean(x < y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedBoolean(x < y)
            case (BoxedByte(x), BoxedByte(y)) => BoxedBoolean(x < y)
            case (BoxedFloat(x), BoxedFloat(y)) => BoxedBoolean(x < y)
            case (BoxedDouble(x), BoxedDouble(y)) => BoxedBoolean(x < y)
            case _ => throw new RuntimeException("type error on <")
          }
          stack.push(r); pc += 1
        case GreaterThan =>
          val b = stack.pop(); val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedBoolean(x > y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedBoolean(x > y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedBoolean(x > y)
            case (BoxedByte(x), BoxedByte(y)) => BoxedBoolean(x > y)
            case (BoxedFloat(x), BoxedFloat(y)) => BoxedBoolean(x > y)
            case (BoxedDouble(x), BoxedDouble(y)) => BoxedBoolean(x > y)
            case _ => throw new RuntimeException("type error on >")
          }
          stack.push(r); pc += 1
        case Equal =>
          val b = stack.pop(); val a = stack.pop()
          val r = BoxedBoolean(a == b)
          stack.push(r); pc += 1
      }
    }
    result
  }

  private def findMethod(self: AnyRef, name: String, params: Array[Value]): MethodSearchResult = {
    val selfClass = self.getClass
    val nameMatchedMethods = selfClass.getMethods.filter(_.getName == name)
    val maybeUnboxedMethod = nameMatchedMethods.find { m =>
      val parameterCountMatches = m.getParameterCount == params.length
      val parameterTypes = Value.classesOfValues(params)
      val parameterTypesMatches = (m.getParameterTypes zip parameterTypes).forall { case (arg, param) => arg.isAssignableFrom(param) }
      parameterCountMatches && parameterTypesMatches
    }.map { m => m.setAccessible(true); UnboxedVersionMethodFound(m) }
    val maybeBoxedMethod = nameMatchedMethods.find { m =>
      val parameterCountMatches = m.getParameterCount == params.length
      val boxedParameterTypes = Value.boxedClassesOfValues(params)
      val boxedParameterTypesMatches = (m.getParameterTypes zip boxedParameterTypes).forall { case (arg, param) => arg.isAssignableFrom(param) }
      parameterCountMatches && boxedParameterTypesMatches
    }.map { m => m.setAccessible(true); BoxedVersionMethodFound(m) }
    maybeUnboxedMethod.orElse(maybeBoxedMethod).getOrElse(NoMethodFound)
  }

  private def findConstructor(target: Class[_], params: Array[Value]): ConstructorSearchResult = {
    val constructors = target.getConstructors
    val maybeUnboxed = constructors.find { c =>
      val parameterCountMatches = c.getParameterCount == params.length
      val unboxedParameterTypes = Value.classesOfValues(params)
      val parameterTypesMatches = (c.getParameterTypes zip unboxedParameterTypes).forall { case (arg, param) => arg.isAssignableFrom(param) }
      parameterCountMatches && parameterTypesMatches
    }.map(UnboxedVersionConstructorFound)
    val maybeBoxed = constructors.find { c =>
      val parameterCountMatches = c.getParameterCount == params.length
      val boxedParameterTypes = Value.boxedClassesOfValues(params)
      val parameterTypesMatches = (c.getParameterTypes zip boxedParameterTypes).forall { case (arg, param) => arg.isAssignableFrom(param) }
      parameterCountMatches && parameterTypesMatches
    }.map(BoxedVersionConstructorFound)
    maybeUnboxed.orElse(maybeBoxed).getOrElse(NoConstructorFound)
  }
}
