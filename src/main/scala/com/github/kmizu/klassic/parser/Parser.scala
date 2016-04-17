package com.github.kmizu.klassic.parser

import com.github.kmizu.klassic.Ast
import fastparse.all._

object Parser {


  val hexDigit      = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  val escape        = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))
  val chars         = P(CharsWhile(!"\"\\".contains(_: Char)))
  val spacing       = P(CharsWhile(!"\r\n\t\f\b".contains(_: Char)).?)

  def token(value: String) = value ~ spacing

  val klazz   = token("class")
  val traiz   = token("trait")
  val subtype = token("<:")
  val lbrace  = token("{")
  val rbrace  = token("}")

  val expression     : P[Ast.Expression]  = P(literal)
  val literal        : P[Ast.Expression]  = P((integer | double | string | bool | identifier) ~ spacing)
  val integer        : P[Ast.IntValue]    = P(("-" | "+").? ~ CharIn('0' to '9').rep(1)).!.map { v => Ast.IntValue(v.toInt) }
  val double         : P[Ast.DoubleValue] = P(("-" | "+").? ~ (CharIn('0' to '9').rep(1) ~ "." ~ CharIn('0' to '9').rep(1)).!.map { v => Ast.DoubleValue(v.toDouble) })
  val string         : P[Ast.StringValue] = P("\"" ~ (chars | escape).rep(0).! ~ "\"").map(Ast.StringValue(_))
  val bool           : P[Ast.BoolValue]   = P(("true".!.map { _ => true } | "false".!.map { _ => false }).map {
    Ast.BoolValue(_)
  })
  val identifier     : P[Ast.Identifier]  = P {
    (identifierFirst ~ (identifierFirst | P(CharIn('0' to '9'))).rep(0)).!.map {
      Ast.Identifier(_)
    }
  }
  val identifierFirst: P[String]          = P {
    CharIn('a' to 'z', 'A' to 'Z', List('_')).!
  }
}
