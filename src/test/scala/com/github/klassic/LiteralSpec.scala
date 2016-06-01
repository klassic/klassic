package com.github.klassic

import java.util.ArrayList

class LiteralSpec extends SpecHelper {
  val I = new Interpreter

  describe("integer literal") {
    val expectations = List[(String, Value)](
      "2" -> BoxedInt(2),
      "+2" -> BoxedInt(+2),
      "-2" -> BoxedInt(-2),
      "1" -> BoxedInt(1),
      "+1" -> BoxedInt(+1),
      "-1" -> BoxedInt(-1),
      "0" -> BoxedInt(0),
      "+0" -> BoxedInt(0),
      "-0" -> BoxedInt(0),
      s"${Int.MinValue}" -> BoxedInt(Int.MinValue),
      s"-${Int.MinValue}" -> BoxedInt(-Int.MinValue),
      s"${Int.MaxValue}" -> BoxedInt(Int.MaxValue),
      s"-${Int.MaxValue}" -> BoxedInt(-Int.MaxValue)
    )
    expectations.zipWithIndex.foreach { case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }

  describe("long literal") {
    val expectations = List[(String, Value)](
      "2L"    -> BoxedLong(2),
      "+2L"   -> BoxedLong(+2),
      "-2L"   -> BoxedLong(-2),
      "1L"    -> BoxedLong(1),
      "+1L"   -> BoxedLong(+1),
      "-1L"   -> BoxedLong(-1),
      "0L"    -> BoxedLong(0),
      "+0L"   -> BoxedLong(0),
      "-0L"   -> BoxedLong(0),
      s"${Long.MaxValue}L" -> BoxedLong(Long.MaxValue),
      s"${Long.MinValue + 1}L" -> BoxedLong(Long.MinValue + 1)
    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }

  describe("double literal") {
    val expectations = List[(String, Value)](
      "2.0" -> BoxedDouble(2.0),
      "2.5" -> BoxedDouble(2.5),
      "+0.0" -> BoxedDouble(+0.0),
      "-0.0" -> BoxedDouble(-0.0),
      "0.1" -> BoxedDouble(+0.1),
      "-0.1" -> BoxedDouble(-0.1)
    )

    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assert(expected == I.evaluateString(in))
      }
    }

    describe("float literal") {
      val expectations = List[(String, Value)](
        "2.0F" -> BoxedFloat(2.0F),
        "2.5F" -> BoxedFloat(2.5F),
        "+0.0F" -> BoxedFloat(+0.0F),
        "-0.0F" -> BoxedFloat(-0.0F),
        "0.1F" -> BoxedFloat(+0.1F),
        "-0.1F" -> BoxedFloat(-0.1F)
      )

      expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
        it(s"expectations ${i}") {
          assert(expected == I.evaluateString(in))
        }
      }
    }
  }

  describe("string literal with escape sequence") {
    val expectations = List[(String, Value)](
      """"\r\n"""" -> ObjectValue("\r\n"),
      """"\r"""" -> ObjectValue("\r"),
      """"\n"""" -> ObjectValue("\n"),
      """"\t"""" -> ObjectValue("\t"),
      """"\b"""" -> ObjectValue("\b"),
      """"\f"""" -> ObjectValue("\f"),
      """"\\"""" -> ObjectValue("\\")
    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }
  describe("list literal") {
    val expectations = List[(String, Value)](
      "[]" -> ObjectValue(listOf[Any]()),
      "[1]" -> ObjectValue(listOf(1)),
      """["a"]""" -> ObjectValue(listOf("a")),
      "[1, 2]" -> ObjectValue(listOf(1, 2)),
      """|[1
        | 2]
      """.stripMargin -> ObjectValue(listOf(1, 2)),
      """|[1,
        |
        | 2]
      """.stripMargin -> ObjectValue(listOf(1, 2)),
      """|[1
        |
        | 2]
      """.stripMargin -> ObjectValue(listOf(1, 2)),
      """|[1 +
        |
        | 2]
      """.stripMargin -> ObjectValue(listOf(3)),
      """|[1, 2
        | 3]
      """.stripMargin -> ObjectValue(listOf(1, 2, 3)),
      """|[1 2
         | 3 4]
      """.stripMargin -> ObjectValue(listOf(1, 2, 3, 4)),
      """| [[1 2]
         |  [3 4]]
      """.stripMargin -> ObjectValue(
        listOf(
          listOf(1, 2),
          listOf(3, 4)
        )
      )
    )
    expectations.zipWithIndex.foreach { case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }
}
