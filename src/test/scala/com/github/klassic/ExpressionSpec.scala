package com.github.klassic

/**
  * Created by Mizushima on 2016/05/30.
  */
class ExpressionSpec extends SpecHelper {
  val I = new Interpreter

  describe("assignment") {
    val expectations: List[(String, Value)] = List(
      """
        |val a=1
        |a
      """.stripMargin -> BoxedInt(1),
      """
        |val a=1
        |a = a + 1
        |a
      """.stripMargin -> BoxedInt(2)
    )

    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }
  describe("while expression") {
    val expectations: List[(String, Value)] = List(
      """
        |val i = 1
        |while(i < 10) {
        |  i = i + 1
        |}
        |i
      """.stripMargin -> BoxedInt(10),
      """
        |val i = 10
        |while(i >= 0) {
        |  i = i - 1
        |}
        |i
      """.stripMargin -> BoxedInt(-1),
      s"""
        |val buf = new java.lang.StringBuffer
        |val i = 0
        |while(i <= 5) {
        |  buf.append("#{i}")
        |  i = i + 1
        |}
        |buf.toString()
      """.stripMargin -> ObjectValue("012345")

    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }
}
