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
      "2"    -> BoxedInt(2),
      "+2"   -> BoxedInt(+2),
      "-2"   -> BoxedInt(-2),
      "1"    -> BoxedInt(1),
      "+1"   -> BoxedInt(+1),
      "-1"   -> BoxedInt(-1),
      "0"    -> BoxedInt(0),
      "+0"   -> BoxedInt(0),
      "-0"   -> BoxedInt(0),
      s"${Int.MinValue}" -> BoxedInt(Int.MinValue),
      s"-${Int.MinValue}" -> BoxedInt(-Int.MinValue),
      s"${Int.MaxValue}" -> BoxedInt(Int.MaxValue),
      s"-${Int.MaxValue}" -> BoxedInt(-Int.MaxValue)
    )
    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == I.evaluateString(in))
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
      expectations.foreach{ case (in, expected) =>
        it(s"${in} evaluates to ${expected}") {
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
    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }
  describe("list literal") {
    val expectations = List[(String, Value)](
      "[]" -> ObjectValue(listOf[Any]()),
      "[1]" -> ObjectValue(listOf(BoxedInt(1))),
      """["a"]""" -> ObjectValue(listOf(ObjectValue("a"))),
      "[1, 2]" -> ObjectValue(listOf(BoxedInt(1), BoxedInt(2))),
      """|[1
        | 2]
      """.stripMargin -> ObjectValue(listOf(BoxedInt(1), BoxedInt(2))),
      """|[1,
        |
        | 2]
      """.stripMargin -> ObjectValue(listOf(BoxedInt(1), BoxedInt(2))),
      """|[1
        |
        | 2]
      """.stripMargin -> ObjectValue(listOf(BoxedInt(1), BoxedInt(2))),
      """|[1 +
        |
        | 2]
      """.stripMargin -> ObjectValue(listOf(BoxedInt(3))),
      """|[1, 2
        | 3]
      """.stripMargin -> ObjectValue(listOf(BoxedInt(1), BoxedInt(2), BoxedInt(3))),
      """|[1 2
         | 3 4]
      """.stripMargin -> ObjectValue(listOf(BoxedInt(1), BoxedInt(2), BoxedInt(3), BoxedInt(4)))
    )
    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }
}
