package com.github.klassic

sealed abstract class TypeDescription(val image: String) {
  def ==>(returnType: TypeDescription): TypeDescription.FunctionType = {
    TypeDescription.FunctionType(List(this), returnType)
  }
  override def toString: String = image
}
object TypeDescription {

  case class TypeVariable(name: String) extends TypeDescription(name)

  case object IntType extends TypeDescription("Int")

  case object ShortType extends TypeDescription("Short")

  case object ByteType extends TypeDescription("Byte")

  case object LongType extends TypeDescription("Long")

  case object FloatType extends TypeDescription("Float")

  case object DoubleType extends TypeDescription("Double")

  case object BooleanType extends TypeDescription("Boolean")

  case object UnitType extends TypeDescription("Unit")

  case object DynamicType extends TypeDescription("*")

  case object ErrorType extends TypeDescription("!")

  case class FunctionType(paramTypes: List[TypeDescription], returnType: TypeDescription) extends TypeDescription(s"(${paramTypes.mkString(", ")}) => ${returnType}")

  case class TypeScheme(typeVariables: List[TypeVariable], description: TypeDescription)

  case class TypeConstructor(name: String, ts: List[TypeDescription]) extends TypeDescription(name + "<" + ts.mkString(", ") + ">")
}
