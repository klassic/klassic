package com.github.kmizu.klassic.parser
import fastparse.all._

object Parser {
  case class TypeIdentifier(symbol: String)
  case class Define(name: Identifier, `type`: Option[TypeIdentifier], value: Expression)
  abstract sealed class Expression
  abstract sealed class Literal[T](value: T) extends Expression
  case class IntValue(value: Int) extends Literal[Int](value)
  case class DoubleValue(value: Double) extends Literal[Double](value)
  case class StringValue(value: String) extends Literal[String](value)
  case class BoolValue(value: Boolean) extends Literal[Boolean](value)
  case class Identifier(symbol: String) extends Expression

  val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )
  val chars         = P(CharsWhile(!"\"\\".contains(_: Char)))
  val spacing       = P( CharsWhile(!"\r\n\t\f\b".contains(_:Char)).? )

  val expression: P[Expression] = P(literal)
  val literal: P[Expression] = P((integer | double | string | bool | identifier) ~ spacing)
  val integer: P[IntValue] = P(("-"|"+").? ~ CharIn('0'to'9').rep(1)).!.map{v => IntValue(v.toInt)}
  val double: P[DoubleValue] = P(("-"|"+").? ~ (CharIn('0'to'9').rep(1) ~ "." ~ CharIn('0'to'9').rep(1)).!.map{v => DoubleValue(v.toDouble)})
  val string: P[StringValue] = P("\"" ~ (chars | escape).rep(0).! ~ "\"").map(StringValue(_))
  val bool: P[BoolValue] = P(("true".!.map{_ => true} | "false".!.map{_ => false}).map{BoolValue(_)})
  val identifier: P[Identifier] = P{
    (identifierFirst ~ (identifierFirst | P(CharIn('0'to'9'))).rep(0)).!.map{Identifier(_)}
  }
  val identifierFirst: P[String] = P {
    CharIn('a' to 'z', 'A' to 'Z', List('_', '-', '+', '*', '/', '#', '$', '&', '!', '?', '%', '|', '=', '^', ':')).!
  }
}
