package com.github.klassic

/**
  * Created by Mizushima on 2016/05/20.
  */
sealed abstract class Operator(val descriptor: String)
object Operator {
  case object Add extends Operator("+")
  case object Subtract extends Operator("-")
  case object Divide extends Operator("/")
  case object Multiply extends Operator("*")
  case object LessThan extends Operator("<")
  case object LessOrEqual extends Operator("<=")
  case object GreaterThan extends Operator(">")
  case object GreaterOrEqual extends Operator(">=")
}
