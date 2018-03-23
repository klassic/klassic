package com.github.klassic

class FunctionSpec extends SpecHelper {
  describe("ceil") {
    val expectations: List[(String, Value)] = List(
      """
        |ceil(2.5)
      """.stripMargin -> BoxedInt(3),
      """
        |ceil(2.0)
      """.stripMargin -> BoxedInt(2),
      """
        |ceil(0.5)
      """.stripMargin -> BoxedInt(1),
      """
        |ceil(-0.5)
      """.stripMargin -> BoxedInt(0),
      """
        |ceil(-1.5)
      """.stripMargin -> BoxedInt(-1)
    )

    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
}
