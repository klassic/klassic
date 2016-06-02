package com.github.klassic

sealed abstract class TypeDescription(val image: String)
object TypeDescription {
  case object IntType extends TypeDescription("Int")
  case object ShortType extends TypeDescription("Short")
  case object ByteType extends TypeDescription("Byte")
  case object LongType extends TypeDescription("Long")
  case object FloatType extends TypeDescription("Float")
  case object DoubleType extends TypeDescription("Double")
  case object BooleanType extends TypeDescription("Boolean")
  case object UnitType extends TypeDescription("Unit")
  case object DynamicType extends TypeDescription("*")
  case object UnknownType extends TypeDescription("?")
  case object ErrorType extends TypeDescription("!")
}
