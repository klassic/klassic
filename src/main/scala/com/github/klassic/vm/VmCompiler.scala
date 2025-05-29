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
    case Selector(_, _, module, name) => code += LoadModule(module, name)
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
    case BinaryExpression(_, _, Operator.EQUAL, l, r) =>
      compileNode(l, code); compileNode(r, code); code += Equal
    case BinaryExpression(_, _, Operator.LESS_THAN, l, r) =>
      compileNode(l, code); compileNode(r, code); code += LessThan
    case BinaryExpression(_, _, Operator.LESS_OR_EQUAL, l, r) =>
      compileNode(l, code); compileNode(r, code); code += LessOrEqual
    case BinaryExpression(_, _, Operator.GREATER_THAN, l, r) =>
      compileNode(l, code); compileNode(r, code); code += GreaterThan
    case BinaryExpression(_, _, Operator.GREATER_EQUAL, l, r) =>
      compileNode(l, code); compileNode(r, code); code += GreaterOrEqual
    case BinaryExpression(_, _, Operator.AND, l, r) =>
      compileNode(l, code); compileNode(r, code); code += And
    case BinaryExpression(_, _, Operator.OR, l, r) =>
      compileNode(l, code); compileNode(r, code); code += Or
    case BinaryExpression(_, _, Operator.XOR, l, r) =>
      compileNode(l, code); compileNode(r, code); code += Xor
    case BinaryExpression(_, _, Operator.AND2, l, r) =>
      compileNode(l, code)
      code += Dup
      val jmpFalsePos = code.length
      code += JumpIfFalse(-1)
      code += Pop
      compileNode(r, code)
      val end = code.length
      code(jmpFalsePos) = JumpIfFalse(end)
    case BinaryExpression(_, _, Operator.BAR2, l, r) =>
      compileNode(l, code)
      code += Dup
      val jmpIfFalsePos = code.length
      code += JumpIfFalse(-1)
      val jmpToEndPos = code.length
      code += Jump(-1)
      val elseStart = code.length
      code += Pop
      compileNode(r, code)
      val end = code.length
      code(jmpIfFalsePos) = JumpIfFalse(elseStart)
      code(jmpToEndPos) = Jump(end)
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
      code += Pop
      code += Jump(start)
      val end = code.length
      code(jmpOutPos) = JumpIfFalse(end)
      code += Push(UnitValue)
    case FunctionCall(_, _, func, params) =>
      params.foreach(compileNode(_, code))
      compileNode(func, code)
      code += Call(params.length)
    case FunctionLiteral(_, _, params, _, body) =>
      val bodyStart = code.length + 2
      code += Jump(-1)
      val actualBodyStart = code.length
      compileNode(body, code)
      code += Return
      val bodyEnd = code.length
      code(bodyStart - 2) = Jump(bodyEnd)
      code += MakeClosure(params.map(_.name), actualBodyStart, bodyEnd)
    case LetFunctionDefinition(_, _, name, body, cleanup, expr) =>
      compileNode(body, code)
      code += Store(name)
      compileNode(expr, code)
    case ListLiteral(_, _, elements) =>
      elements.foreach(compileNode(_, code))
      code += MakeList(elements.length)
    case SetLiteral(_, _, elements) =>
      elements.foreach(compileNode(_, code))
      code += MakeSet(elements.length)
    case MapLiteral(_, _, elements) =>
      elements.foreach { case (k, v) =>
        compileNode(k, code)
        compileNode(v, code)
      }
      code += MakeMap(elements.length)
    case ObjectNew(_, _, className, params) =>
      params.foreach(compileNode(_, code))
      code += NewObject(className, params.length)
    case RecordNew(_, _, recordName, params) =>
      params.foreach(compileNode(_, code))
      code += NewRecord(recordName, params.length)
    case MethodCall(_, _, self, name, params) =>
      compileNode(self, code)
      params.foreach(compileNode(_, code))
      code += CallMethod(name, params.length)
    case RecordSelect(_, _, expr, field) =>
      compileNode(expr, code)
      code += GetField(field)
    case Casting(_, _, target, _) =>
      compileNode(target, code)
    case ValueNode(v) => code += Push(v)
    case ForeachExpression(_, _, _, _, _) =>
      throw new RuntimeException("ForeachExpression should have been desugared")
  }
}
