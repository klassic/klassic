package com.github.klassic

/**
 * @author Kota Mizushima
 */

sealed abstract class AST {
  val location: Location
}

object AST {

  sealed trait IntegerSuffix

  case object ByteSuffix extends IntegerSuffix

  case object ShortSuffix extends IntegerSuffix

  case object LongSuffix extends IntegerSuffix

  sealed trait FloatSuffix

  case object FloatSuffix extends FloatSuffix

  case class Program(val location: Location, imports: List[Import], block: Block)

  case class Import(val location: Location, simpleName: String, fqcn: String)

  case class Block(val location: Location, expressions: List[AST]) extends AST

  case class IfExpression(val location: Location, condition: AST, thenExpression: AST, elseExpression: AST) extends AST

  case class ForeachExpression(val location: Location, name: String, collection: AST, body: AST) extends AST

  case class BinaryExpression(val location: Location, operator: Operator, lhs: AST, rhs: AST) extends AST

  case class WhileExpression(val location: Location, condition: AST, body: AST) extends AST

  case class MinusOp(val location: Location, operand: AST) extends AST

  case class PlusOp(val location: Location, operand: AST) extends AST

  case class StringNode(val location: Location, value: String) extends AST

  case class IntNode(val location: Location, value: Int) extends AST

  case class LongNode(val location: Location, value: Long) extends AST

  case class ShortNode(val location: Location, value: Short) extends AST

  case class ByteNode(val location: Location, value: Byte) extends AST

  case class BooleanNode(val location: Location, value: Boolean) extends AST

  case class DoubleNode(val location: Location, value: Double) extends AST

  case class FloatNode(val location: Location, value: Float) extends AST

  case class Identifier(val location: Location, name: String) extends AST

  case class Assignment(val location: Location, variable: String, value: AST) extends AST

  case class ValDeclaration(val location: Location, variable: String, description: Option[TypeDescription], value: AST, immutable: Boolean) extends AST

  case class FunctionLiteral(val location: Location, params: List[FormalParameter], proc: AST) extends AST

  case class FunctionDefinition(val location: Location, name: String, func: FunctionLiteral, cleanup: Option[AST]) extends AST

  case class FunctionCall(val location: Location, func: AST, params: List[AST]) extends AST

  case class ListLiteral(val location: Location, elements: List[AST]) extends AST

  case class MapLiteral(val location: Location, elements: List[(AST, AST)]) extends AST

  case class NewObject(val location: Location, className: String, params: List[AST]) extends AST

  case class MethodCall(val location: Location, self: AST, name: String, params: List[AST]) extends AST
}