package com.github.klassic.vm

import scala.collection.mutable.ArrayBuffer
import com.github.klassic._
import com.github.klassic.TypedAst._
import com.github.klassic.Operator

class VmCompiler {
  def compile(block: Block): Vector[Instruction] = {
    val code = ArrayBuffer.empty[Instruction]
    compileNode(block, code)
    code += Return
    code.toVector
  }

  private def compileNode(node: TypedNode, code: ArrayBuffer[Instruction]): Unit = node match {
    case Block(_, _, exprs) =>
      exprs.foreach(compileNode(_, code))
    case IntNode(_, _, v) => code += Push(BoxedInt(v))
    case LongNode(_, _, v) => code += Push(BoxedLong(v))
    case ShortNode(_, _, v) => code += Push(BoxedShort(v))
    case ByteNode(_, _, v) => code += Push(BoxedByte(v))
    case DoubleNode(_, _, v) => code += Push(BoxedDouble(v))
    case FloatNode(_, _, v) => code += Push(BoxedFloat(v))
    case BooleanNode(_, _, v) => code += Push(BoxedBoolean(v))
    case StringNode(_, _, v) => code += Push(ObjectValue(v))
    case UnitNode(_, _) => code += Push(UnitValue)
    case Id(_, _, name) => code += Load(name)
    case Assignment(_, _, name, value) =>
      compileNode(value, code)
      code += Store(name)
      code += Push(UnitValue)
    case LetDeclaration(_, _, name, _, value, body, _) =>
      compileNode(value, code)
      code += Store(name)
      compileNode(body, code)
    case BinaryExpression(_, _, Operator.ADD, l, r) =>
      compileNode(l, code)
      compileNode(r, code)
      code += Add
    case BinaryExpression(_, _, Operator.SUBTRACT, l, r) =>
      compileNode(l, code); compileNode(r, code); code += Sub
    case BinaryExpression(_, _, Operator.MULTIPLY, l, r) =>
      compileNode(l, code); compileNode(r, code); code += Mul
    case BinaryExpression(_, _, Operator.DIVIDE, l, r) =>
      compileNode(l, code); compileNode(r, code); code += Div
    case BinaryExpression(_, _, Operator.LESS_THAN, l, r) =>
      compileNode(l, code); compileNode(r, code); code += LessThan
    case BinaryExpression(_, _, Operator.GREATER_THAN, l, r) =>
      compileNode(l, code); compileNode(r, code); code += GreaterThan
    case BinaryExpression(_, _, Operator.EQUAL, l, r) =>
      compileNode(l, code); compileNode(r, code); code += Equal
    case MinusOp(_, _, op) =>
      compileNode(op, code); code += Neg
    case PlusOp(_, _, op) =>
      compileNode(op, code)
    case IfExpression(_, _, cond, thenN, elseN) =>
      compileNode(cond, code)
      val jmpFalsePos = code.length
      code += JumpIfFalse(-1)
      compileNode(thenN, code)
      val jmpEndPos = code.length
      code += Jump(-1)
      val elsePos = code.length
      compileNode(elseN, code)
      val end = code.length
      code(jmpFalsePos) = JumpIfFalse(elsePos)
      code(jmpEndPos) = Jump(end)
    case WhileExpression(_, _, cond, body) =>
      val start = code.length
      compileNode(cond, code)
      val jmpOutPos = code.length
      code += JumpIfFalse(-1)
      compileNode(body, code)
      code += Jump(start)
      val end = code.length
      code(jmpOutPos) = JumpIfFalse(end)
    case ValueNode(v) => code += Push(v)
    case literal@FunctionLiteral(_, _, _, _, _) =>
      code += PushFunction(literal)
    case LetFunctionDefinition(_, _, name, body, _, expr) =>
      code += PushFunction(body)
      code += Store(name)
      compileNode(expr, code)
    case FunctionCall(_, _, func, params) =>
      compileNode(func, code)
      params.foreach(compileNode(_, code))
      code += Call(params.length)
    case MethodCall(_, _, self, name, params) =>
      compileNode(self, code)
      params.foreach(compileNode(_, code))
      code += CallMethod(name, params.length)
    case ObjectNew(_, _, className, params) =>
      params.foreach(compileNode(_, code))
      code += NewObject(className, params.length)
    case n =>
      throw new RuntimeException(s"unsupported node: $n")
  }

  def compileFunction(func: FunctionLiteral): Vector[Instruction] = {
    val code = ArrayBuffer.empty[Instruction]
    compileNode(func.proc, code)
    code += Return
    code.toVector
  }
}
