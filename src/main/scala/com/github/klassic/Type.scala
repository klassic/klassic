package com.github.klassic

sealed abstract class Type(val image: String) {
  def ==>(returnType: Type): Type.FunctionType = {
    Type.FunctionType(List(this), returnType)
  }
  override def toString: String = image
}
object Type {
  implicit class RichType(args: List[Type]) {
    def ==>(returnType: Type): Type.FunctionType = {
      Type.FunctionType(args, returnType)
    }
  }

  case class TypeVariable(name: String) extends Type(name)

  case object IntType extends Type("Int")

  case object ShortType extends Type("Short")

  case object ByteType extends Type("Byte")

  case object LongType extends Type("Long")

  case object FloatType extends Type("Float")

  case object DoubleType extends Type("Double")

  case object BooleanType extends Type("Boolean")

  case object UnitType extends Type("Unit")

  case object DynamicType extends Type("*")

  case object ErrorType extends Type("!")

  case class RecordType(name: String, paramTypes: List[Type]) extends Type(s"record ${name}<${paramTypes.mkString(", ")}>")

  case class FunctionType(paramTypes: List[Type], returnType: Type) extends Type(s"(${paramTypes.mkString(", ")}) => ${returnType}")

  case class TypeScheme(typeVariables: List[TypeVariable], description: Type)

  case class TypeConstructor(name: String, ts: List[Type]) extends Type(name + "<" + ts.mkString(", ") + ">")
}
