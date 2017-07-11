package com.github.klassic

sealed abstract class Type(val image: String) {
  def ==>(returnType: Type): Type.TFunction = {
    Type.TFunction(List(this), returnType)
  }
  override def toString: String = image
}
object Type {
  implicit class RichType(args: List[Type]) {
    def ==>(returnType: Type): Type.TFunction = {
      Type.TFunction(args, returnType)
    }
  }

  case class TVariable(name: String) extends Type(name)

  case object TInt extends Type("Int")

  case object TShort extends Type("Short")

  case object TByte extends Type("Byte")

  case object TLong extends Type("Long")

  case object TFloat extends Type("Float")

  case object TDouble extends Type("Double")

  case object TBoolean extends Type("Boolean")

  case object TUnit extends Type("Unit")

  case object TDynamic extends Type("*")

  case object TError extends Type("!")

  case class TRecordReference(name: String, paramTypes: List[Type]) extends Type(
    s"#${name}${if(paramTypes == Nil) "" else s"<${paramTypes.mkString(", ")}>"}"
  )

  sealed abstract class Row

  case object EmptyRow extends Row {
    override def toString(): String = "{}"
  }

  case class RowExtension(l: String, t: Type, e: Row) extends Row {
    override def toString(): String = s"${l}: ${t}; ${e}"
  }

  case class TRecord(ts: List[TVariable], row: Row) extends Type(s"Record{${row}")

  case class TFunction(paramTypes: List[Type], returnType: Type) extends Type(s"(${paramTypes.mkString(", ")}) => ${returnType}")

  case class TScheme(svariables: List[TVariable], stype: Type)

  case class TConstructor(name: String, ts: List[Type]) extends Type(name + "<" + ts.mkString(", ") + ">")
}
