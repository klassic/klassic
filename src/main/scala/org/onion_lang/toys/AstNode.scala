package org.onion_lang
package toys

/**
 * @author Kota Mizushima
 */

sealed abstract class AstNode
case class Block(expressions: List[AstNode]) extends AstNode
case class IfExpr(cond: AstNode, pos: AstNode, neg: AstNode) extends AstNode
case class LessOp(left: AstNode, right: AstNode) extends AstNode
case class AddOp(left: AstNode, right: AstNode) extends AstNode
case class SubOp(left: AstNode, right: AstNode) extends AstNode
case class MulOp(left: AstNode, right: AstNode) extends AstNode
case class DivOp(left: AstNode, right: AstNode) extends AstNode
case class StringNode(value: String) extends AstNode
case class PrintLine(value: AstNode) extends AstNode
case class IntNode(value: Int) extends AstNode
case class Identifier(name: String) extends AstNode
case class Assignment(variable: String, value: AstNode) extends AstNode
case class ValDeclaration(variable: String, value: AstNode) extends AstNode
case class FunctionLiteral(params:List[String], proc: AstNode) extends AstNode
case class FunctionDefinition(name: String, func: FunctionLiteral) extends AstNode
case class FunctionCall(func:AstNode, params:List[AstNode]) extends AstNode
case class NewObject(className: String, params: List[AstNode]) extends AstNode
case class MethodCall(self:AstNode, name: Identifier, params:List[AstNode]) extends AstNode
