package com.github.klassic

class CommentSpec extends SpecHelper {
  describe("line comments") {
    it("should be parsed correctly") {
      assertResult(
        E("""|//line comment
           |1
          """.stripMargin)
      )(BoxedInt(1))
      assertResult(
        E("""|1
            |//line comment
          """.stripMargin)
      )(BoxedInt(1))
      assertResult(
        E("""|1 +
            |//line comment
            |2
        """.stripMargin)
      )(BoxedInt(3))
      assertResult(
        E("""|1
            |//line comment
            |+ 2""".stripMargin)
      )(BoxedInt(2))
    }
  }

  describe("block comments") {
    it("should be parsed correctly") {
      assertResult(
        E("""|1 + /* block comment */
           |1
        """.stripMargin)
      )(BoxedInt(2))
      assertResult(
        E("""|1 /* + block comment */
           |1
        """.stripMargin)
      )(BoxedInt(1))
      assertResult(
        E("""|/*/**/*/
            |1
        """.stripMargin)
      )(BoxedInt(1))
      assertResult(
        E("""|1 /* nested
           |     /* comment */
           |   here */ + 2
        """.stripMargin)
      )(BoxedInt(3))
    }
  }
}
