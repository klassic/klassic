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
  case class VariadicLiteral[T](location: Location, value: T) extends Expression
  case class TypedExpression(location: Location, body: Expression, annotation: Type) extends Expression
}
