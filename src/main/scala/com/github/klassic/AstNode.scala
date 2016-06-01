package com.github.klassic

/**
 * @author Kota Mizushima
 */

sealed abstract class AstNode(val location: Option[AstNode.Location])

object AstNode {
  case class Location(line: Int, column: Int)

  case class Block(expressions: List[AstNode]) extends AstNode(None)

  case class IfExpression(cond: AstNode, pos: AstNode, neg: AstNode) extends AstNode(None)

  case class ForeachExpression(name: String, collection: AstNode, body: AstNode) extends AstNode(None)

  case class BinaryExpression(operator: Operator, lhs: AstNode, rhs: AstNode) extends AstNode(None)

  case class WhileExpression(condition: AstNode, body: AstNode) extends AstNode(None)

  case class MinusOp(operand: AstNode) extends AstNode(None)

  case class PlusOp(operand: AstNode) extends AstNode(None)

  case class StringNode(value: String) extends AstNode(None)

  case class IntNode(value: Int) extends AstNode(None)

  case class LongNode(value: Long) extends AstNode(None)

  case class ShortNode(value: Short) extends AstNode(None)

  case class ByteNode(value: Byte) extends AstNode(None)

  case class BooleanNode(value: Boolean) extends AstNode(None)

  case class Identifier(name: String) extends AstNode(None)

  case class Assignment(variable: String, value: AstNode) extends AstNode(None)

  case class ValDeclaration(variable: String, value: AstNode) extends AstNode(None)

  case class FunctionLiteral(params: List[String], proc: AstNode) extends AstNode(None)

  case class FunctionDefinition(name: String, func: FunctionLiteral) extends AstNode(None)

  case class FunctionCall(func: AstNode, params: List[AstNode]) extends AstNode(None)

  case class ListLiteral(elements: List[AstNode]) extends AstNode(None)

  case class NewObject(className: String, params: List[AstNode]) extends AstNode(None)

  case class MethodCall(self: AstNode, name: String, params: List[AstNode]) extends AstNode(None)
}