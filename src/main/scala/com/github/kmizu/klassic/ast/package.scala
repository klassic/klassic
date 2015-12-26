package com.github.kmizu.klassic

package object ast {
  sealed abstract class AstNode {
    def location: Location
  }
  case class Location(line: Int, column: Int)

  sealed abstract class Type extends AstNode

  sealed abstract class Expression extends AstNode
  case class TypedExpression(location: Location, body: Expression, annotation: Type) extends Expression
}
