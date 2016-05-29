package com.github.klassic

/**
 * @author Kota Mizushima
 */

sealed abstract class AstNode
case class Block(expressions: List[AstNode]) extends AstNode
case class IfExpression(cond: AstNode, pos: AstNode, neg: AstNode) extends AstNode
case class WhileExpression(condition: AstNode, body: AstNode) extends AstNode
case class BinaryExpression(operator: Operator, lhs: AstNode, rhs: AstNode) extends AstNode
case class MinusOp(operand: AstNode) extends AstNode
case class PlusOp(operand: AstNode) extends AstNode
case class StringNode(value: String) extends AstNode
case class IntNode(value: Int) extends AstNode
case class LongNode(value: Long) extends AstNode
case class ShortNode(value: Short) extends AstNode
case class ByteNode(value: Byte) extends AstNode
case class Identifier(name: String) extends AstNode
case class Assignment(variable: String, value: AstNode) extends AstNode
case class ValDeclaration(variable: String, value: AstNode) extends AstNode
case class FunctionLiteral(params:List[String], proc: AstNode) extends AstNode
case class FunctionDefinition(name: String, func: FunctionLiteral) extends AstNode
case class FunctionCall(func:AstNode, params:List[AstNode]) extends AstNode
case class ListLiteral(elements: List[AstNode]) extends AstNode
case class NewObject(className: String, params: List[AstNode]) extends AstNode
case class MethodCall(self:AstNode, name: Identifier, params:List[AstNode]) extends AstNode
