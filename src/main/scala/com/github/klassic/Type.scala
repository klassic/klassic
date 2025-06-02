package com.github.klassic

// Kind system for higher-kinded types
sealed abstract class Kind
object Kind {
  case object Star extends Kind { override def toString = "*" }
  case class Arrow(from: Kind, to: Kind) extends Kind { override def toString = s"($from -> $to)" }
  
  // Helper to create arrow kinds
  def arrow(kinds: Kind*): Kind = kinds.reduceRight(Arrow(_, _))
}

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

  case class TVariable(name: String, kind: Kind = Kind.Star) extends Row(name)

  case object TInt extends Type("Int")

  case object TShort extends Type("Short")

  case object TByte extends Type("Byte")

  case object TLong extends Type("Long")

  case object TFloat extends Type("Float")

  case object TDouble extends Type("Double")

  case object TBoolean extends Type("Boolean")

  case object TUnit extends Type("Unit")

  case object TString extends Type("String")

  case object TDynamic extends Type("*")

  case object TError extends Type("!")

  case class TRecordReference(name: String, paramTypes: List[Type]) extends Type(
    s"#${name}${if(paramTypes == Nil) "" else s"<${paramTypes.mkString(", ")}>"}"
  )

  sealed abstract class Row(image: String) extends Type(image)

  case object TRowEmpty extends Row("")

  case class TRowExtend(l: String, t: Type, e: Type) extends Row(
    e match {
      case TVariable(_, _) => s"${l}: ${t}; ..."
      case _ => s"${l}: ${t}; ${e}"
    }
  )

  case class TRecord(ts: List[TVariable], row: Type) extends Type(s"record { ${row}}")

  case class TFunction(paramTypes: List[Type], returnType: Type) extends Type(s"(${paramTypes.mkString(", ")}) => ${returnType}")

  case class TScheme(svariables: List[TVariable], stype: Type, constraints: List[TConstraint] = Nil)

  case class TConstructor(name: String, ts: List[Type], kind: Kind = Kind.Star) extends Type(name + "<" + ts.mkString(", ") + ">")

  case class TConstraint(className: String, typeVar: TVariable) extends Type(s"${className}[${typeVar.name}]")

  case class TQualified(constraints: List[TConstraint], baseType: Type) extends Type(
    if (constraints.isEmpty) baseType.image
    else s"(${constraints.map(_.image).mkString(", ")}) => ${baseType.image}"
  )

  case class TTypeClass(name: String, typeParams: List[TVariable], methods: List[(String, TScheme)]) extends Type(s"class ${name}")

  case class TInstance(className: String, forType: Type, methods: Map[String, Type]) extends Type(s"instance ${className}[${forType}]")
}
