package com.github.klassic

/**
 * @author Kota Mizushima
 */
sealed abstract class Value
case class StringValue(value: String) extends Value {
  override def toString = value
}
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
case class FunctionValue(value: FunctionLiteral, environment: Option[Environment]) extends Value {
  override def toString = s"function value:${value} environment:${environment}"
}
case class NativeFunctionValue(body: PartialFunction[List[Value], Value]) extends Value {
  override def toString = s"native function"
}
case object UnitValue extends Value {
  override def toString = "()"
}
case class ObjectValue(value: AnyRef) extends Value {
  override def toString = value.toString
}
object Value {
  def fromKlassic(value: Value): AnyRef = value match {
    case StringValue(v) => v
    case BoxedBoolean(v) => new java.lang.Boolean(v)
    case BoxedInt(v) => new java.lang.Integer(v)
    case UnitValue => UnitValue
    case ObjectValue(v) => v
    case otherwise => otherwise
  }
  def toKlassic(value: AnyRef): Value = value match {
    case v:java.lang.String => StringValue(v)
    case v:java.lang.Boolean => BoxedBoolean(v.booleanValue())
    case v:java.lang.Integer => BoxedInt(v.intValue())
    case UnitValue => UnitValue
    case otherwise => ObjectValue(otherwise)
  }
}
