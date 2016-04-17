package org.onion_lang
package toys

/**
 * @author Kota Mizushima
 */

sealed trait AST
case class Block(expressions: List[AST]) extends AST
case class IfExpr(cond: AST, pos: AST, neg: AST) extends AST
case class LessOp(left: AST, right: AST) extends AST
case class AddOp(left: AST, right: AST) extends AST
case class SubOp(left: AST, right: AST) extends AST
case class MulOp(left: AST, right: AST) extends AST
case class DivOp(left: AST, right: AST) extends AST
case class StringVal(value: String) extends AST
case class PrintLine(value: AST) extends AST
case class IntVal(value: Int) extends AST
case class Ident(name: String) extends AST
case class Assignment(variable: String, value: AST) extends AST
case class ValDeclaration(variable: String, value: AST) extends AST
case class Func(params:List[String], proc: AST) extends AST
case class FuncDef(name: String, func: Func) extends AST
case class FuncCall(func:AST, params:List[AST]) extends AST
