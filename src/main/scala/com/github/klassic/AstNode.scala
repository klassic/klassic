package com.github.klassic

/**
 * @author Kota Mizushima
 */

sealed abstract class AstNode(val location: AstNode.Location)

object AstNode {
  sealed abstract class Location {
    def format: String
  }
  case class SourceLocation(line: Int, column: Int) extends Location {
    def format: String = s"<${line}, {colum}>:"
  }
  case object NoLocation extends Location {
    def format: String = s"<empty>:"
  }

  case class Block(expressions: List[AstNode]) extends AstNode(NoLocation)

  case class IfExpression(cond: AstNode, pos: AstNode, neg: AstNode) extends AstNode(NoLocation)

  case class ForeachExpression(name: String, collection: AstNode, body: AstNode) extends AstNode(NoLocation)

  case class BinaryExpression(operator: Operator, lhs: AstNode, rhs: AstNode) extends AstNode(NoLocation)

  case class WhileExpression(condition: AstNode, body: AstNode) extends AstNode(NoLocation)

  case class MinusOp(operand: AstNode) extends AstNode(NoLocation)

  case class PlusOp(operand: AstNode) extends AstNode(NoLocation)

  case class StringNode(value: String) extends AstNode(NoLocation)

  case class IntNode(value: Int) extends AstNode(NoLocation)

  case class LongNode(value: Long) extends AstNode(NoLocation)

  case class ShortNode(value: Short) extends AstNode(NoLocation)

  case class ByteNode(value: Byte) extends AstNode(NoLocation)

  case class BooleanNode(value: Boolean) extends AstNode(NoLocation)

  case class DoubleNode(value: Double) extends AstNode(NoLocation)

  case class FloatNode(value: Float) extends AstNode(NoLocation)

  case class Identifier(name: String) extends AstNode(NoLocation)

  case class Assignment(variable: String, value: AstNode) extends AstNode(NoLocation)

  case class ValDeclaration(variable: String, description: Option[TypeDescription], value: AstNode) extends AstNode(NoLocation)

  case class FunctionLiteral(params: List[FormalParameter], proc: AstNode) extends AstNode(NoLocation)

  case class FunctionDefinition(name: String, func: FunctionLiteral) extends AstNode(NoLocation)

  case class FunctionCall(func: AstNode, params: List[AstNode]) extends AstNode(NoLocation)

  case class ListLiteral(elements: List[AstNode]) extends AstNode(NoLocation)

  case class NewObject(className: String, params: List[AstNode]) extends AstNode(NoLocation)

  case class MethodCall(self: AstNode, name: String, params: List[AstNode]) extends AstNode(NoLocation)
}