package org.onion_lang
package toys

/**
 * @author Kota Mizushima
 */
sealed abstract class Value
case class StringValue(value: String) extends Value {
  override def toString() = value
}
case class IntValue(value: Int) extends Value {
  override def toString() = value.toString
}
case class BooleanValue(value: Boolean) extends Value {
  override def toString() = value.toString
}
case class FunctionValue(value: FunctionLiteral, environment: Option[Environment]) extends Value {
  override def toString() = s"function value:${value} environment:${environment}"
}
case class NativeFunctionValue(body: PartialFunction[List[Value], Value]) extends Value {
  override def toString() = s"native function"
}
case object UnitValue extends Value {
  override def toString() = "unit"
}
case class ObjectValue(value: AnyRef) extends Value {
  override def toString() = s"object(${value})"
}
object Value {
  def fromToys(value: Value): AnyRef = value match {
    case StringValue(v) => v
    case BooleanValue(v) => new java.lang.Boolean(v)
    case IntValue(v) => new java.lang.Integer(v)
    case UnitValue => UnitValue
    case ObjectValue(v) => v
    case otherwise => otherwise
  }
  def toToys(value: AnyRef): Value = value match {
    case v:java.lang.String => StringValue(v)
    case v:java.lang.Boolean => BooleanValue(v.booleanValue())
    case v:java.lang.Integer => IntValue(v.intValue())
    case UnitValue => UnitValue
    case otherwise => ObjectValue(otherwise)
  }
}
