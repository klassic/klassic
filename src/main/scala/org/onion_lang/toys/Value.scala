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
case class NativeFunctionValue(body: List[Value] => Value) extends Value {
  override def toString() = s"native function"
}
case object UnitValue extends Value {
  override def toString() = "unit"
}
