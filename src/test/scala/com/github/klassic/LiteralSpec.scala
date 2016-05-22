package com.github.klassic

class LiteralSpec extends SpecHelper {
  val I = new Interpreter
  describe("integer literal") {
    val expectations = List[(String, Value)](
      "2"    -> IntValue(2),
      "+2"   -> IntValue(+2),
      "-2"   -> IntValue(-2),
      "1"    -> IntValue(1),
      "+1"   -> IntValue(+1),
      "-1"   -> IntValue(-1),
      "0"    -> IntValue(0),
      "+0"   -> IntValue(0),
      "-0"   -> IntValue(0)
    )
    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }
}
