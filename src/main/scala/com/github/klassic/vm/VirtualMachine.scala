package com.github.klassic.vm

import scala.collection.mutable
import com.github.klassic._
import scala.annotation.switch

class VirtualMachine(moduleEnv: ModuleEnvironment, recordEnv: RecordEnvironment) {
  // Pre-allocate stack for better performance
  private val INITIAL_STACK_SIZE = 1024
  private val stack = mutable.ArrayBuffer[Value]()
  stack.sizeHint(INITIAL_STACK_SIZE)
  // Pre-populate with null to avoid resize during execution
  for (i <- 0 until INITIAL_STACK_SIZE) stack += null
  
  private val callStack = mutable.ArrayBuffer[(Int, RuntimeEnvironment)]()
  private var stackPtr = 0

  private def push(value: Value): Unit = {
    if (stackPtr >= stack.length) {
      stack += value
    } else {
      stack(stackPtr) = value
    }
    stackPtr += 1
  }
  
  private def pop(): Value = {
    if (stackPtr <= 0) {
      throw new RuntimeException(s"Stack underflow: attempting to pop from empty stack (stackPtr=$stackPtr)")
    }
    stackPtr -= 1
    stack(stackPtr)
  }
  
  private def peek(): Value = stack(stackPtr - 1)
  
  // Arithmetic operations
  private def performAdd(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxInt(x + y)
    case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x + y)
    case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x + y).toShort)
    case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x + y).toByte)
    case (BoxedFloat(x), BoxedFloat(y)) => BoxedFloat(x + y)
    case (BoxedDouble(x), BoxedDouble(y)) => BoxedDouble(x + y)
    case (ObjectValue(s: String), v) => ObjectValue(s + v)
    case (v, ObjectValue(s: String)) => ObjectValue(v.toString + s)
    case _ => throw new RuntimeException("type error on add")
  }
  
  private def performSub(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxInt(x - y)
    case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x - y)
    case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x - y).toShort)
    case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x - y).toByte)
    case (BoxedFloat(x), BoxedFloat(y)) => BoxedFloat(x - y)
    case (BoxedDouble(x), BoxedDouble(y)) => BoxedDouble(x - y)
    case _ => throw new RuntimeException("type error on sub")
  }
  
  private def performMul(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxInt(x * y)
    case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x * y)
    case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x * y).toShort)
    case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x * y).toByte)
    case (BoxedFloat(x), BoxedFloat(y)) => BoxedFloat(x * y)
    case (BoxedDouble(x), BoxedDouble(y)) => BoxedDouble(x * y)
    case _ => throw new RuntimeException("type error on mul")
  }
  
  private def performDiv(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxInt(x / y)
    case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x / y)
    case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x / y).toShort)
    case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x / y).toByte)
    case (BoxedFloat(x), BoxedFloat(y)) => BoxedFloat(x / y)
    case (BoxedDouble(x), BoxedDouble(y)) => BoxedDouble(x / y)
    case _ => throw new RuntimeException("type error on div")
  }
  
  private def performNeg(a: Value): Value = a match {
    case BoxedInt(x) => Value.boxInt(-x)
    case BoxedLong(x) => BoxedLong(-x)
    case BoxedShort(x) => BoxedShort((-x).toShort)
    case BoxedByte(x) => BoxedByte((-x).toByte)
    case BoxedFloat(x) => BoxedFloat(-x)
    case BoxedDouble(x) => BoxedDouble(-x)
    case _ => throw new RuntimeException("type error on neg")
  }
  
  // Comparison operations
  private def performEqual(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxBoolean(x == y)
    case (BoxedLong(x), BoxedLong(y)) => Value.boxBoolean(x == y)
    case (BoxedShort(x), BoxedShort(y)) => Value.boxBoolean(x == y)
    case (BoxedByte(x), BoxedByte(y)) => Value.boxBoolean(x == y)
    case (BoxedFloat(x), BoxedFloat(y)) => Value.boxBoolean(x == y)
    case (BoxedDouble(x), BoxedDouble(y)) => Value.boxBoolean(x == y)
    case (BoxedBoolean(x), BoxedBoolean(y)) => Value.boxBoolean(x == y)
    case (ObjectValue(x), ObjectValue(y)) => Value.boxBoolean(x == y)
    case _ => throw new RuntimeException("type error on equal")
  }
  
  private def performNotEqual(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxBoolean(x != y)
    case (BoxedLong(x), BoxedLong(y)) => Value.boxBoolean(x != y)
    case (BoxedShort(x), BoxedShort(y)) => Value.boxBoolean(x != y)
    case (BoxedByte(x), BoxedByte(y)) => Value.boxBoolean(x != y)
    case (BoxedFloat(x), BoxedFloat(y)) => Value.boxBoolean(x != y)
    case (BoxedDouble(x), BoxedDouble(y)) => Value.boxBoolean(x != y)
    case (BoxedBoolean(x), BoxedBoolean(y)) => Value.boxBoolean(x != y)
    case (ObjectValue(x), ObjectValue(y)) => Value.boxBoolean(x != y)
    case _ => throw new RuntimeException("type error on not equal")
  }
  
  private def performLessThan(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxBoolean(x < y)
    case (BoxedLong(x), BoxedLong(y)) => Value.boxBoolean(x < y)
    case (BoxedShort(x), BoxedShort(y)) => Value.boxBoolean(x < y)
    case (BoxedByte(x), BoxedByte(y)) => Value.boxBoolean(x < y)
    case (BoxedFloat(x), BoxedFloat(y)) => Value.boxBoolean(x < y)
    case (BoxedDouble(x), BoxedDouble(y)) => Value.boxBoolean(x < y)
    case _ => throw new RuntimeException("type error on less than")
  }
  
  private def performLessOrEqual(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxBoolean(x <= y)
    case (BoxedLong(x), BoxedLong(y)) => Value.boxBoolean(x <= y)
    case (BoxedShort(x), BoxedShort(y)) => Value.boxBoolean(x <= y)
    case (BoxedByte(x), BoxedByte(y)) => Value.boxBoolean(x <= y)
    case (BoxedFloat(x), BoxedFloat(y)) => Value.boxBoolean(x <= y)
    case (BoxedDouble(x), BoxedDouble(y)) => Value.boxBoolean(x <= y)
    case _ => throw new RuntimeException("type error on less or equal")
  }
  
  private def performGreaterThan(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxBoolean(x > y)
    case (BoxedLong(x), BoxedLong(y)) => Value.boxBoolean(x > y)
    case (BoxedShort(x), BoxedShort(y)) => Value.boxBoolean(x > y)
    case (BoxedByte(x), BoxedByte(y)) => Value.boxBoolean(x > y)
    case (BoxedFloat(x), BoxedFloat(y)) => Value.boxBoolean(x > y)
    case (BoxedDouble(x), BoxedDouble(y)) => Value.boxBoolean(x > y)
    case _ => throw new RuntimeException("type error on greater than")
  }
  
  private def performGreaterOrEqual(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxBoolean(x >= y)
    case (BoxedLong(x), BoxedLong(y)) => Value.boxBoolean(x >= y)
    case (BoxedShort(x), BoxedShort(y)) => Value.boxBoolean(x >= y)
    case (BoxedByte(x), BoxedByte(y)) => Value.boxBoolean(x >= y)
    case (BoxedFloat(x), BoxedFloat(y)) => Value.boxBoolean(x >= y)
    case (BoxedDouble(x), BoxedDouble(y)) => Value.boxBoolean(x >= y)
    case _ => throw new RuntimeException("type error on greater or equal")
  }
  
  // Bitwise operations
  private def performAnd(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxInt(x & y)
    case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x & y)
    case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x & y).toShort)
    case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x & y).toByte)
    case _ => throw new RuntimeException("type error on and")
  }
  
  private def performOr(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxInt(x | y)
    case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x | y)
    case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x | y).toShort)
    case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x | y).toByte)
    case _ => throw new RuntimeException("type error on or")
  }
  
  private def performXor(a: Value, b: Value): Value = (a, b) match {
    case (BoxedInt(x), BoxedInt(y)) => Value.boxInt(x ^ y)
    case (BoxedLong(x), BoxedLong(y)) => BoxedLong(x ^ y)
    case (BoxedShort(x), BoxedShort(y)) => BoxedShort((x ^ y).toShort)
    case (BoxedByte(x), BoxedByte(y)) => BoxedByte((x ^ y).toByte)
    case _ => throw new RuntimeException("type error on xor")
  }
  
  // Method/Constructor resolution
  private def callMethod(obj: AnyRef, name: String, args: Array[Value]): Value = {
    BuiltinEnvironments.findMethod(obj, name, args) match {
      case UnboxedVersionMethodFound(m) =>
        val actualParams = args.map(Value.fromKlassic)
        Value.toKlassic(m.invoke(obj, actualParams: _*))
      case BoxedVersionMethodFound(m) =>
        val actualParams = args.map(Value.fromKlassic)
        Value.toKlassic(m.invoke(obj, actualParams: _*))
      case NoMethodFound =>
        throw new RuntimeException(s"method $name not found")
    }
  }
  
  private def newObject(className: String, args: Array[Value]): Value = {
    BuiltinEnvironments.findConstructor(Class.forName(className), args) match {
      case UnboxedVersionConstructorFound(constructor) =>
        val actualParams = args.map(Value.fromKlassic)
        Value.toKlassic(constructor.newInstance(actualParams: _*).asInstanceOf[AnyRef])
      case BoxedVersionConstructorFound(constructor) =>
        val actualParams = args.map(Value.fromKlassic)
        Value.toKlassic(constructor.newInstance(actualParams: _*).asInstanceOf[AnyRef])
      case NoConstructorFound =>
        throw new RuntimeException(s"constructor for $className not found")
    }
  }
  
  def run(code: Vector[Instruction], env: RuntimeEnvironment = RuntimeEnvironment.pooled(None)): Value = {
    var pc = 0
    var currentEnv = env
    var running = true
    var result: Value = UnitValue
    stackPtr = 0
    val codeLength = code.length
    
    while (pc < codeLength && running) {
      val instruction = code(pc)
      instruction match {
        case Push(v) =>
          push(v)
          pc += 1
          
        case Load(name) =>
          push(currentEnv(name))
          pc += 1
          
        case LoadModule(module, name) =>
          push(moduleEnv(module)(name))
          pc += 1
          
        case Store(name) =>
          currentEnv.update(name, pop())
          pc += 1
          
        case Assign(name) =>
          currentEnv.set(name, pop())
          pc += 1
          
        case Add =>
          // Optimize for common int + int case
          if (stackPtr >= 2) {
            val b = stack(stackPtr - 1)
            val a = stack(stackPtr - 2)
            (a, b) match {
              case (BoxedInt(x), BoxedInt(y)) =>
                stackPtr -= 2
                push(Value.boxInt(x + y))
              case _ =>
                stackPtr -= 2
                push(performAdd(a, b))
            }
          }
          pc += 1
          
        case Sub =>
          // Optimize for common int - int case
          if (stackPtr >= 2) {
            val b = stack(stackPtr - 1)
            val a = stack(stackPtr - 2)
            (a, b) match {
              case (BoxedInt(x), BoxedInt(y)) =>
                stackPtr -= 2
                push(Value.boxInt(x - y))
              case _ =>
                stackPtr -= 2
                push(performSub(a, b))
            }
          }
          pc += 1
          
        case Mul =>
          // Optimize for common int * int case
          if (stackPtr >= 2) {
            val b = stack(stackPtr - 1)
            val a = stack(stackPtr - 2)
            (a, b) match {
              case (BoxedInt(x), BoxedInt(y)) =>
                stackPtr -= 2
                push(Value.boxInt(x * y))
              case _ =>
                stackPtr -= 2
                push(performMul(a, b))
            }
          }
          pc += 1
          
        case Div =>
          // Optimize for common int / int case
          if (stackPtr >= 2) {
            val b = stack(stackPtr - 1)
            val a = stack(stackPtr - 2)
            (a, b) match {
              case (BoxedInt(x), BoxedInt(y)) =>
                stackPtr -= 2
                push(Value.boxInt(x / y))
              case _ =>
                stackPtr -= 2
                push(performDiv(a, b))
            }
          }
          pc += 1
          
        case Neg =>
          push(performNeg(pop()))
          pc += 1
          
        case Not =>
          pop() match {
            case BoxedBoolean(b) => push(BoxedBoolean(!b))
            case _ => throw new RuntimeException("type error on not")
          }
          pc += 1
          
        case Equal =>
          val b = pop()
          val a = pop()
          push(performEqual(a, b))
          pc += 1
          
        case NotEqual =>
          val b = pop()
          val a = pop()
          push(performNotEqual(a, b))
          pc += 1
          
        case LessThan =>
          val b = pop()
          val a = pop()
          push(performLessThan(a, b))
          pc += 1
          
        case LessOrEqual =>
          val b = pop()
          val a = pop()
          push(performLessOrEqual(a, b))
          pc += 1
          
        case GreaterThan =>
          val b = pop()
          val a = pop()
          push(performGreaterThan(a, b))
          pc += 1
          
        case GreaterOrEqual =>
          val b = pop()
          val a = pop()
          push(performGreaterOrEqual(a, b))
          pc += 1
          
        case And =>
          val b = pop()
          val a = pop()
          push(performAnd(a, b))
          pc += 1
          
        case Or =>
          val b = pop()
          val a = pop()
          push(performOr(a, b))
          pc += 1
          
        case Xor =>
          val b = pop()
          val a = pop()
          push(performXor(a, b))
          pc += 1
          
        case Pop =>
          pop()
          pc += 1
          
        case Dup =>
          push(peek())
          pc += 1
          
        case Jump(target) =>
          pc = target
          
        case JumpIfFalse(target) =>
          pop() match {
            case BoxedBoolean(false) => pc = target
            case BoxedBoolean(true) => pc += 1
            case _ => throw new RuntimeException("condition must be boolean")
          }
          
        case MakeList(size) =>
          val list = new java.util.ArrayList[Any](size)
          var i = stackPtr - size
          while (i < stackPtr) {
            list.add(Value.fromKlassic(stack(i)))
            i += 1
          }
          stackPtr -= size
          push(ObjectValue(list))
          pc += 1
          
        case MakeSet(size) =>
          val set = new java.util.HashSet[Any]()
          var i = stackPtr - size
          while (i < stackPtr) {
            set.add(Value.fromKlassic(stack(i)))
            i += 1
          }
          stackPtr -= size
          push(ObjectValue(set))
          pc += 1
          
        case MakeMap(size) =>
          val map = new java.util.HashMap[Any, Any]()
          var i = stackPtr - (size * 2)
          while (i < stackPtr) {
            val k = stack(i)
            val v = stack(i + 1)
            map.put(Value.fromKlassic(k), Value.fromKlassic(v))
            i += 2
          }
          stackPtr -= (size * 2)
          push(ObjectValue(map))
          pc += 1
          
        case MakeClosure(params, bodyStart, bodyEnd) =>
          push(VmClosureValue(params, bodyStart, bodyEnd, currentEnv, code))
          pc += 1
          
        case Call(arity) =>
          val func = pop()
          func match {
            case VmClosureValue(params, bodyStart, bodyEnd, closureEnv, _) =>
              callStack += ((pc + 1, currentEnv))
              val newEnv = RuntimeEnvironment.pooled(Some(closureEnv))
              var i = 0
              var argPtr = stackPtr - arity
              while (i < arity) {
                newEnv.update(params(i), stack(argPtr))
                i += 1
                argPtr += 1
              }
              stackPtr -= arity
              currentEnv = newEnv
              pc = bodyStart
            case NativeFunctionValue(body) =>
              val args = new Array[Value](arity)
              var i = arity - 1
              while (i >= 0) {
                args(i) = pop()
                i -= 1
              }
              if (body.isDefinedAt(args.toList)) {
                push(body(args.toList))
                pc += 1
              } else {
                throw new RuntimeException("native function argument mismatch")
              }
            case _ =>
              throw new RuntimeException("not a function")
          }
          
        case CallMethod(name, arity) =>
          val args = new Array[Value](arity)
          var i = arity - 1
          while (i >= 0) {
            args(i) = pop()
            i -= 1
          }
          val self = pop()
          self match {
            case ObjectValue(obj) =>
              push(callMethod(obj, name, args))
              pc += 1
            case _ =>
              throw new RuntimeException("method call on non-object")
          }
          
        case NewObject(className, arity) =>
          val args = new Array[Value](arity)
          var i = arity - 1
          while (i >= 0) {
            args(i) = pop()
            i -= 1
          }
          push(newObject(className, args))
          pc += 1
          
        case NewRecord(recordName, arity) =>
          recordEnv.records.get(recordName) match {
            case Some(fieldList) =>
              val members = new Array[(String, Value)](arity)
              var i = arity - 1
              while (i >= 0) {
                members(i) = (fieldList(i)._1, pop())
                i -= 1
              }
              push(RecordValue(recordName, members.toList))
              pc += 1
            case None =>
              throw new RuntimeException(s"record $recordName not found")
          }
          
        case GetField(fieldName) =>
          pop() match {
            case RecordValue(_, members) =>
              members.find(_._1 == fieldName) match {
                case Some((_, value)) => push(value)
                case None => throw new RuntimeException(s"field $fieldName not found")
              }
              pc += 1
            case _ =>
              throw new RuntimeException("field access on non-record")
          }
          
        case Return =>
          if (callStack.nonEmpty) {
            result = if (stackPtr > 0) pop() else UnitValue
            val (returnPc, prevEnv) = callStack.remove(callStack.length - 1)
            pc = returnPc
            currentEnv = prevEnv
            push(result)
          } else {
            // Top-level return - get result from stack and stop execution
            result = if (stackPtr > 0) pop() else UnitValue
            running = false
          }
          
        case And2 | Or2 =>
          throw new RuntimeException("logical operators should be handled by compiler")
      }
    }
    result
  }
}
