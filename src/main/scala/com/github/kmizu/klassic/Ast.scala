package com.github.kmizu.klassic

/**
  * Created by Mizushima on 2016/02/02.
  */
object Ast {
  sealed abstract class Node
  case class Definition(name: Identifier, `type`: Option[TypeIdentifier], value: Expression)
  abstract sealed class Expression
  abstract sealed class Literal[T](value: T) extends Expression
  case class IntValue(value: Int) extends Literal[Int](value)
  case class DoubleValue(value: Double) extends Literal[Double](value)
  case class StringValue(value: String) extends Literal[String](value)
  case class BoolValue(value: Boolean) extends Literal[Boolean](value)
  case class Identifier(symbol: String) extends Expression

  case class Location(line: Int, column: Int)

  sealed abstract class Type extends Node
  case class TypeIdentifier(name: String) extends Type
  case class TypeConstructor(constructor: Type, args: List[Type]) extends Type
  case class LambdaTypeConstructor(args: List[Type], body: Type) extends Type

  case class WhileExpression(condition: Expression, body: Expression) extends Expression
  case class IfExpression(condition: Expression, thenClause: Expression, elseClause: Expression) extends Expression
  case class BlockExpression(expressions: List[Expression]) extends Expression
  case class MethodCall(receiver: Expression, name: String, params: List[Expression]) extends Expression
  case class TypedExpression(body: Expression, annotation: Type) extends Expression
}
