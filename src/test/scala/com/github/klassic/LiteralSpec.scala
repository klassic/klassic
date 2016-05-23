package com.github.klassic

import java.util.ArrayList

class LiteralSpec extends SpecHelper {
  def listOf[T](elements: T*): ArrayList[T] = {
    val newList = new ArrayList[T]
    elements.foreach{e =>
      newList.add(e)
    }
    newList
  }
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
      "-0"   -> IntValue(0),
      s"${Int.MinValue}" -> IntValue(Int.MinValue),
      s"-${Int.MinValue}" -> IntValue(-Int.MinValue),
      s"${Int.MaxValue}" -> IntValue(Int.MaxValue),
      s"-${Int.MaxValue}" -> IntValue(-Int.MaxValue)
    )
    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == I.evaluateString(in))
      }
    }

    describe("long literal") {
      val expectations = List[(String, Value)](
        "2L"    -> LongValue(2),
        "+2L"   -> LongValue(+2),
        "-2L"   -> LongValue(-2),
        "1L"    -> LongValue(1),
        "+1L"   -> LongValue(+1),
        "-1L"   -> LongValue(-1),
        "0L"    -> LongValue(0),
        "+0L"   -> LongValue(0),
        "-0L"   -> LongValue(0),
        s"${Long.MaxValue}L" -> LongValue(Long.MaxValue),
        s"${Long.MinValue + 1}L" -> LongValue(Long.MinValue + 1)
      )
      expectations.foreach{ case (in, expected) =>
        it(s"${in} evaluates to ${expected}") {
          assert(expected == I.evaluateString(in))
        }
      }
    }
  }

  describe("string literal with escape sequence") {
    val expectations = List[(String, Value)](
      """"\r\n"""" -> StringValue("\r\n"),
      """"\r"""" -> StringValue("\r"),
      """"\n"""" -> StringValue("\n"),
      """"\t"""" -> StringValue("\t"),
      """"\b"""" -> StringValue("\b"),
      """"\f"""" -> StringValue("\f"),
      """"\\"""" -> StringValue("\\")
    )
    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }
  describe("list literal") {
    val expectations = List[(String, Value)](
      "[]" -> ObjectValue(listOf[Any]()),
      "[1]" -> ObjectValue(listOf(IntValue(1))),
      """["a"]""" -> ObjectValue(listOf(StringValue("a"))),
      "[1, 2]" -> ObjectValue(listOf(IntValue(1), IntValue(2))),
      """|[1
        | 2]
      """.stripMargin -> ObjectValue(listOf(IntValue(1), IntValue(2))),
      """|[1,
        |
        | 2]
      """.stripMargin -> ObjectValue(listOf(IntValue(1), IntValue(2))),
      """|[1
        |
        | 2]
      """.stripMargin -> ObjectValue(listOf(IntValue(1), IntValue(2))),
      """|[1 +
        |
        | 2]
      """.stripMargin -> ObjectValue(listOf(IntValue(3))),
      """|[1, 2
        | 3]
      """.stripMargin -> ObjectValue(listOf(IntValue(1), IntValue(2), IntValue(3)))
    )
    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }
}
