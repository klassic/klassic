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
      }
    }
    result
  }
}
