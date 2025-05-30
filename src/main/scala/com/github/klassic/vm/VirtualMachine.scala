package com.github.klassic.vm

import scala.collection.mutable
import com.github.klassic._

class VirtualMachine(interpreter: Interpreter, moduleEnv: ModuleEnvironment, recordEnv: RecordEnvironment) {
  private val stack = mutable.Stack[Value]()
  private val callStack = mutable.Stack[(Int, RuntimeEnvironment)]()

  def run(code: Vector[Instruction], env: RuntimeEnvironment = new RuntimeEnvironment(None)): Value = {
    var pc = 0
    var currentEnv = env
    var running = true
    var result: Value = UnitValue
    
    while (pc < code.length && running) {
      code(pc) match {
        case Push(v) =>
          stack.push(v)
          pc += 1
          
        case Load(name) =>
          stack.push(currentEnv(name))
          pc += 1
          
        case LoadModule(module, name) =>
          stack.push(moduleEnv(module)(name))
          pc += 1
          
        case Store(name) =>
          currentEnv.update(name, stack.pop())
          pc += 1
          
        case Assign(name) =>
          currentEnv.set(name, stack.pop())
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
            case (ObjectValue(s: String), v) => ObjectValue(s + v)
            case (v, ObjectValue(s: String)) => ObjectValue(v.toString + s)
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
          
        case Equal =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedBoolean(x == y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedBoolean(x == y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedBoolean(x == y)
            case (BoxedByte(x), BoxedByte(y)) => BoxedBoolean(x == y)
            case (BoxedFloat(x), BoxedFloat(y)) => BoxedBoolean(x == y)
            case (BoxedDouble(x), BoxedDouble(y)) => BoxedBoolean(x == y)
            case (BoxedBoolean(x), BoxedBoolean(y)) => BoxedBoolean(x == y)
            case (ObjectValue(x), ObjectValue(y)) => BoxedBoolean(x == y)
            case _ => throw new RuntimeException("type error on equal")
          }
          stack.push(r)
          pc += 1
          
        case LessThan =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedBoolean(x < y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedBoolean(x < y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedBoolean(x < y)
            case (BoxedByte(x), BoxedByte(y)) => BoxedBoolean(x < y)
            case (BoxedFloat(x), BoxedFloat(y)) => BoxedBoolean(x < y)
            case (BoxedDouble(x), BoxedDouble(y)) => BoxedBoolean(x < y)
            case _ => throw new RuntimeException("type error on less than")
          }
          stack.push(r)
          pc += 1
          
        case LessOrEqual =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedBoolean(x <= y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedBoolean(x <= y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedBoolean(x <= y)
            case (BoxedByte(x), BoxedByte(y)) => BoxedBoolean(x <= y)
            case (BoxedFloat(x), BoxedFloat(y)) => BoxedBoolean(x <= y)
            case (BoxedDouble(x), BoxedDouble(y)) => BoxedBoolean(x <= y)
            case _ => throw new RuntimeException("type error on less or equal")
          }
          stack.push(r)
          pc += 1
          
        case GreaterThan =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedBoolean(x > y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedBoolean(x > y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedBoolean(x > y)
            case (BoxedByte(x), BoxedByte(y)) => BoxedBoolean(x > y)
            case (BoxedFloat(x), BoxedFloat(y)) => BoxedBoolean(x > y)
            case (BoxedDouble(x), BoxedDouble(y)) => BoxedBoolean(x > y)
            case _ => throw new RuntimeException("type error on greater than")
          }
          stack.push(r)
          pc += 1
          
        case GreaterOrEqual =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedBoolean(x >= y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedBoolean(x >= y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedBoolean(x >= y)
            case (BoxedByte(x), BoxedByte(y)) => BoxedBoolean(x >= y)
            case (BoxedFloat(x), BoxedFloat(y)) => BoxedBoolean(x >= y)
            case (BoxedDouble(x), BoxedDouble(y)) => BoxedBoolean(x >= y)
            case _ => throw new RuntimeException("type error on greater or equal")
          }
          stack.push(r)
          pc += 1
          
        case And =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedInt(x & y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x & y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x & y).toShort)
            case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x & y).toByte)
            case _ => throw new RuntimeException("type error on and")
          }
          stack.push(r)
          pc += 1
          
        case Or =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedInt(x | y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x | y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x | y).toShort)
            case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x | y).toByte)
            case _ => throw new RuntimeException("type error on or")
          }
          stack.push(r)
          pc += 1
          
        case Xor =>
          val b = stack.pop()
          val a = stack.pop()
          val r = (a, b) match {
            case (BoxedInt(x), BoxedInt(y)) => BoxedInt(x ^ y)
            case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x ^ y)
            case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x ^ y).toShort)
            case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x ^ y).toByte)
            case _ => throw new RuntimeException("type error on xor")
          }
          stack.push(r)
          pc += 1
          
        case Pop =>
          stack.pop()
          pc += 1
          
        case Dup =>
          stack.push(stack.top)
          pc += 1
          
        case Jump(target) =>
          pc = target
          
        case JumpIfFalse(target) =>
          stack.pop() match {
            case BoxedBoolean(false) => pc = target
            case BoxedBoolean(true) => pc += 1
            case _ => throw new RuntimeException("condition must be boolean")
          }
          
        case MakeList(size) =>
          val elements = (0 until size).map(_ => stack.pop()).reverse
          val list = new java.util.ArrayList[Any]()
          elements.foreach(e => list.add(Value.fromKlassic(e)))
          stack.push(ObjectValue(list))
          pc += 1
          
        case MakeSet(size) =>
          val elements = (0 until size).map(_ => stack.pop()).reverse
          val set = new java.util.HashSet[Any]()
          elements.foreach(e => set.add(Value.fromKlassic(e)))
          stack.push(ObjectValue(set))
          pc += 1
          
        case MakeMap(size) =>
          val kvPairs = (0 until size).flatMap { _ =>
            val v = stack.pop()
            val k = stack.pop()
            Some((k, v))
          }.reverse
          val map = new java.util.HashMap[Any, Any]()
          kvPairs.foreach { case (k, v) =>
            map.put(Value.fromKlassic(k), Value.fromKlassic(v))
          }
          stack.push(ObjectValue(map))
          pc += 1
          
        case MakeClosure(params, bodyStart, bodyEnd) =>
          stack.push(VmClosureValue(params, bodyStart, bodyEnd, currentEnv, code))
          pc += 1
          
        case Call(arity) =>
          val func = stack.pop()
          val args = (0 until arity).map(_ => stack.pop()).reverse
          func match {
            case VmClosureValue(params, bodyStart, bodyEnd, closureEnv, _) =>
              callStack.push((pc + 1, currentEnv))
              val newEnv = new RuntimeEnvironment(Some(closureEnv))
              params.zip(args).foreach { case (param, arg) =>
                newEnv.update(param, arg)
              }
              currentEnv = newEnv
              pc = bodyStart
            case NativeFunctionValue(body) =>
              if (body.isDefinedAt(args.toList)) {
                stack.push(body(args.toList))
                pc += 1
              } else {
                throw new RuntimeException("native function argument mismatch")
              }
            case _ =>
              throw new RuntimeException("not a function")
          }
          
        case CallMethod(name, arity) =>
          val args = (0 until arity).map(_ => stack.pop()).reverse.toArray
          val self = stack.pop()
          self match {
            case ObjectValue(obj) =>
              val result = interpreter.findMethod(obj, name, args) match {
                case UnboxedVersionMethodFound(method) =>
                  val actualParams = args.map(Value.fromKlassic)
                  Value.toKlassic(method.invoke(obj, actualParams: _*))
                case BoxedVersionMethodFound(method) =>
                  val actualParams = args.map(Value.fromKlassic)
                  Value.toKlassic(method.invoke(obj, actualParams: _*))
                case NoMethodFound =>
                  throw new RuntimeException(s"method $name not found")
              }
              stack.push(result)
              pc += 1
            case _ =>
              throw new RuntimeException("method call on non-object")
          }
          
        case NewObject(className, arity) =>
          val args = (0 until arity).map(_ => stack.pop()).reverse.toArray
          val result = interpreter.findConstructor(Class.forName(className), args) match {
            case UnboxedVersionConstructorFound(constructor) =>
              val actualParams = args.map(Value.fromKlassic)
              Value.toKlassic(constructor.newInstance(actualParams: _*).asInstanceOf[AnyRef])
            case BoxedVersionConstructorFound(constructor) =>
              val actualParams = args.map(Value.fromKlassic)
              Value.toKlassic(constructor.newInstance(actualParams: _*).asInstanceOf[AnyRef])
            case NoConstructorFound =>
              throw new RuntimeException(s"constructor for $className not found")
          }
          stack.push(result)
          pc += 1
          
        case NewRecord(recordName, arity) =>
          val args = (0 until arity).map(_ => stack.pop()).reverse.toList
          recordEnv.records.get(recordName) match {
            case Some(fieldList) =>
              val members = fieldList.zip(args).map { case ((fname, _), value) => fname -> value }
              stack.push(RecordValue(recordName, members))
              pc += 1
            case None =>
              throw new RuntimeException(s"record $recordName not found")
          }
          
        case GetField(fieldName) =>
          stack.pop() match {
            case RecordValue(_, members) =>
              members.find(_._1 == fieldName) match {
                case Some((_, value)) => stack.push(value)
                case None => throw new RuntimeException(s"field $fieldName not found")
              }
              pc += 1
            case _ =>
              throw new RuntimeException("field access on non-record")
          }
          
        case Return =>
          result = if (stack.nonEmpty) stack.pop() else UnitValue
          if (callStack.nonEmpty) {
            val (returnPc, prevEnv) = callStack.pop()
            pc = returnPc
            currentEnv = prevEnv
            stack.push(result)
          } else {
            running = false
          }
          
        case And2 | Or2 =>
          throw new RuntimeException("logical operators should be handled by compiler")
      }
    }
    result
  }
}
