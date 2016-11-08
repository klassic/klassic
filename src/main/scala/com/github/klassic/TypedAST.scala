package com.github.klassic

/**
 * @author Kota Mizushima
 */

sealed abstract class TypedAST {
  val location: Location
  val description: TypeDescription
}

object TypedAST {
  sealed trait IntegerSuffix

  case object ByteSuffix extends IntegerSuffix

  case object ShortSuffix extends IntegerSuffix

  case object LongSuffix extends IntegerSuffix

  sealed trait FloatSuffix

  case object FloatSuffix extends FloatSuffix

  case class Program(val location: Location, imports: List[Import], block: Block)

  case class Import(val location: Location, simpleName: String, fqcn: String)

  case class Block(val description: TypeDescription, val location: Location, expressions: List[TypedAST]) extends TypedAST

  case class IfExpression(val description: TypeDescription, val location: Location, condition: TypedAST, thenExpression: TypedAST, elseExpression: TypedAST) extends TypedAST

  case class ForeachExpression(val description: TypeDescription, val location: Location, name: String, collection: TypedAST, body: TypedAST) extends TypedAST

  case class BinaryExpression(val description: TypeDescription, val location: Location, operator: Operator, lhs: TypedAST, rhs: TypedAST) extends TypedAST

  case class WhileExpression(val description: TypeDescription, val location: Location, condition: TypedAST, body: TypedAST) extends TypedAST

  case class MinusOp(val description: TypeDescription, val location: Location, operand: TypedAST) extends TypedAST

  case class PlusOp(val description: TypeDescription, val location: Location, operand: TypedAST) extends TypedAST

  case class StringNode(val description: TypeDescription, val location: Location, value: String) extends TypedAST

  case class IntNode(val description: TypeDescription, val location: Location, value: Int) extends TypedAST

  case class LongNode(val description: TypeDescription, val location: Location, value: Long) extends TypedAST

  case class ShortNode(val description: TypeDescription, val location: Location, value: Short) extends TypedAST

  case class ByteNode(val description: TypeDescription, val location: Location, value: Byte) extends TypedAST

  case class BooleanNode(val description: TypeDescription, val location: Location, value: Boolean) extends TypedAST

  case class DoubleNode(val description: TypeDescription, val location: Location, value: Double) extends TypedAST

  case class FloatNode(val description: TypeDescription, val location: Location, value: Float) extends TypedAST

  case class Identifier(val description: TypeDescription, val location: Location, name: String) extends TypedAST

  case class Assignment(val description: TypeDescription, val location: Location, variable: String, value: TypedAST) extends TypedAST

  case class ValDeclaration(val description: TypeDescription, val location: Location, variable: String, declaredType: Option[TypeDescription], value: TypedAST, immutable: Boolean) extends TypedAST

  case class FunctionLiteral(val description: TypeDescription, val location: Location, params: List[FormalParameter], proc: TypedAST) extends TypedAST

  case class FunctionDefinition(val description: TypeDescription, val location: Location, name: String, func: FunctionLiteral, cleanup: Option[TypedAST]) extends TypedAST

  case class FunctionCall(val description: TypeDescription, val location: Location, func: TypedAST, params: List[TypedAST]) extends TypedAST

  case class ListLiteral(val description: TypeDescription, val location: Location, elements: List[TypedAST]) extends TypedAST

  case class MapLiteral(val description: TypeDescription,  val location: Location, elements: List[(TypedAST, TypedAST)]) extends TypedAST

  case class NewObject(val description: TypeDescription, val location: Location, className: String, params: List[TypedAST]) extends TypedAST

  case class MethodCall(val description: TypeDescription, val location: Location, self: TypedAST, name: String, params: List[TypedAST]) extends TypedAST
}