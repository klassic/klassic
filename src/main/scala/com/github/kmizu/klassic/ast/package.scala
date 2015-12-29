package com.github.kmizu.klassic

package object ast {
  sealed abstract class AstNode {
    def location: Location
  }
  case class Location(line: Int, column: Int)

  sealed abstract class Type extends AstNode
  case class SimpleType(location: Location, name: String) extends AstNode
  case class TypeConstructor(location: Location, constructor: Type, args: List[Type]) extends AstNode
  case class LambdaTypeConstructor(location: Location, args: List[Type], body: Type) extends AstNode

  sealed abstract class Expression extends AstNode
  case class WhileExpression(location: Location, condition: Expression, body: Expression) extends Expression
  case class IfExpression(location: Location, condition: Expression, thenClause: Expression, elseClause: Expression) extends Expression
  case class BlockExpression(location: Location, expressions: List[Expression]) extends Expression
  case class MethodCall(location: Location, receiver: Expression, name: String, params: List[Expression]) extends Expression
  case class VariadicLiteral[T](location: Location, value: T) extends Expression
  case class TypedExpression(location: Location, body: Expression, annotation: Type) extends Expression
}
