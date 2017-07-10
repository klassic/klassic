package com.github.klassic

class CommentSpec extends SpecHelper {
  describe("line comment") {
    val expectations = List[(String, Value)](
      """|//line comment
         |1
      """.stripMargin -> BoxedInt(1),
      """|1
         |//line comment
      """.stripMargin -> BoxedInt(1),
      """|1 +
         |//line comment
         |2
      """.stripMargin -> BoxedInt(3),
      """|1
         |//line comment
         |+ 2""".stripMargin -> BoxedInt(2)
    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assert(expected == E(in))
      }
    }

    describe("block comment") {
      val expectations = List[(String, Value)](
        """|1 + /* block comment */
           |1
        """.stripMargin -> BoxedInt(2),
        """|1 /* + block comment */
           |1
        """.stripMargin -> BoxedInt(1),
        """|/*/**/*/
           |1
        """.stripMargin -> BoxedInt(1),
        """|1 /* nested
           |     /* comment */
           |   here */ + 2
        """.stripMargin -> BoxedInt(3)
      )
      expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
        it(s"expectations ${i}") {
          assert(expected == E(in))
        }
      }
    }
  }
}
