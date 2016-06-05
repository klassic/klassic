package com.github.klassic

/**
  * Created by Mizushima on 2016/05/20.
  */
sealed abstract class Operator(val descriptor: String)
object Operator {
  case object ADD extends Operator("+")
  case object SUBTRACT extends Operator("-")
  case object DIVIDE extends Operator("/")
  case object MULTIPLY extends Operator("*")
  case object EQUAL extends Operator("==")
  case object LESS_THAN extends Operator("<")
  case object LESS_OR_EQUAL extends Operator("<=")
  case object GREATER_THAN extends Operator(">")
  case object GREATER_EQUAL extends Operator(">=")
  case object AND2 extends Operator("&&")
  case object BAR2 extends Operator("||")
}
