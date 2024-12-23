package com.github.klassic

import com.github.klassic.TypedAst.TypedNode

sealed abstract class Value
case class BoxedByte(value: Byte) extends Value {
  override def toString = value.toString
}
case class BoxedShort(value: Short) extends Value {
  override def toString = value.toString
}
case class BoxedInt(value: Int) extends Value {
  override def toString = value.toString
}
case class BoxedLong(value: Long) extends Value {
  override def toString = value.toString
}
case class BoxedBoolean(value: Boolean) extends Value {
  override def toString = value.toString
}
case class BoxedDouble(value: Double) extends Value {
  override def toString = value.toString
}
case class BoxedFloat(value: Float) extends Value {
  override def toString = value.toString
}
case class FunctionValue(value: TypedAst.FunctionLiteral, cleanup: Option[TypedNode], environment: Option[RuntimeEnvironment]) extends Value {
  override def toString: String = s"<function value>"
}
case class NativeFunctionValue(body: PartialFunction[List[Value], Value]) extends Value {
  override def toString: String = s"<native function>"
}
case object UnitValue extends Value {
  override def toString = "()"
}
case class ObjectValue(value: AnyRef) extends Value {
  override def toString: String = if(value eq null) "null" else value.toString
}
case class RecordValue(name: String, members: List[(String, Value)]) extends Value {
  override def toString: String =
    s"""| record ${name} {
        | ${members.map{ case (n, v) => s"\t${n} = ${v}"}.mkString("\n")}
        | }
    """.stripMargin
}
case class EnumValue(tag: String, items: List[Value]) extends Value {
  override def toString: String = {
    s"${tag}(${items.mkString(", ")})"
  }
}
object Value {

  def classOfValue(value: Value): java.lang.Class[_]= value match {
    case BoxedBoolean(v) => classOf[Boolean]
    case BoxedByte(v) => classOf[Byte]
    case BoxedShort(v) => classOf[Short]
    case BoxedInt(v) => classOf[Int]
    case BoxedLong(v) => classOf[Long]
    case BoxedFloat(v) => classOf[Float]
    case BoxedDouble(v) => classOf[Double]
    case ObjectValue(v) => v.getClass
    case otherwise => otherwise.getClass
  }

  def boxedClassOfValue(value: Value): java.lang.Class[_]= value match {
    case BoxedBoolean(v) => classOf[java.lang.Boolean]
    case BoxedByte(v) => classOf[java.lang.Byte]
    case BoxedShort(v) => classOf[java.lang.Short]
    case BoxedInt(v) => classOf[java.lang.Integer]
    case BoxedLong(v) => classOf[java.lang.Long]
    case BoxedFloat(v) => classOf[java.lang.Float]
    case BoxedDouble(v) => classOf[java.lang.Double]
    case ObjectValue(v) => v.getClass
    case otherwise => otherwise.getClass
  }

  def boxedClassesOfValues(values: Array[Value]): Array[java.lang.Class[_]] = values.map(boxedClassOfValue)

  def classesOfValues(values: Array[Value]):  Array[java.lang.Class[_]] = values.map(classOfValue)

  def fromKlassic(value: Value): AnyRef = value match {
    case BoxedBoolean(v) => java.lang.Boolean.valueOf(v)
    case BoxedByte(v) => java.lang.Byte.valueOf(v)
    case BoxedShort(v) => java.lang.Short.valueOf(v)
    case BoxedInt(v) => java.lang.Integer.valueOf(v)
    case BoxedLong(v) => java.lang.Long.valueOf(v)
    case BoxedFloat(v) => java.lang.Float.valueOf(v)
    case BoxedDouble(v) => java.lang.Double.valueOf(v)
    case ObjectValue(v) => v
    case UnitValue => UnitValue
    case otherwise => otherwise
  }

  def toKlassic(value: AnyRef): Value = value match {
    case v:java.lang.Boolean => BoxedBoolean(v.booleanValue())
    case v:java.lang.Byte => BoxedByte(v.byteValue())
    case v:java.lang.Short => BoxedShort(v.shortValue())
    case v:java.lang.Integer => BoxedInt(v.intValue())
    case v:java.lang.Long => BoxedLong(v.intValue())
    case v:java.lang.Float => BoxedFloat(v.floatValue())
    case v:java.lang.Double => BoxedDouble(v.doubleValue())
    case UnitValue => UnitValue
    case otherwise => ObjectValue(otherwise)
  }
}
