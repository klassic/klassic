package com.github.klassic

/**
  * Created by Mizushima on 2016/05/30.
  */
class ExpressionSpec extends SpecHelper {
  describe("assignment") {
    val expectations: List[(String, Value)] = List(
      """
        |mutable a=1
        |a
      """.stripMargin -> BoxedInt(1),
      """
        |mutable a=1
        |a = a + 1
        |a
      """.stripMargin -> BoxedInt(2),
      """
        |mutable a=1
        |a += 1
        |a
      """.stripMargin -> BoxedInt(2),
      """
        |mutable a=1
        |a -= 1
        |a
      """.stripMargin -> BoxedInt(0),
      """
        |mutable a=3
        |a *= 2
        |a
      """.stripMargin -> BoxedInt(6),
      """
        |mutable a=5
        |a /= 2
        |a
      """.stripMargin -> BoxedInt(2)

    )

    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }

  describe("while expression") {
    val expectations: List[(String, Value)] = List(
      """
        |mutable i = 1
        |while(i < 10) {
        |  i = i + 1
        |}
        |i
      """.stripMargin -> BoxedInt(10),
      """
        |mutable i = 10
        |while(i >= 0) {
        |  i = i - 1
        |}
        |i
      """.stripMargin -> BoxedInt(-1),
      s"""
        |val buf = new java.lang.StringBuffer
        |mutable i = 0
        |while(i <= 5) {
        |  buf->append("#{i}")
        |  i = i + 1
        |}
        |buf->toString()
      """.stripMargin -> ObjectValue("012345")

    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assert(expected == E(in))
      }
    }
  }

  describe("anonymous function") {
    val expectations: List[(String, Value)] = List(
      """
        |val add = (x, y) => x + y
        |add(3, 3)
      """.stripMargin -> BoxedInt(6)
    )

    expectations.zipWithIndex.foreach { case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assert(expected == E(in))
      }
    }
  }

  describe("logical expression") {
    val expectations: List[(String, Value)] = List(
      """
        |val i = 1
        |0 <= i && i <= 10
      """.stripMargin -> BoxedBoolean(true),
      """
        |val i = -1
        |0 <= i && i <= 10
      """.stripMargin -> BoxedBoolean(false),
      """
        |val i = -1
        |i < 0 || i > 10
      """.stripMargin -> BoxedBoolean(true),
      """
        |val i = 1
        |i < 0 || i > 10
      """.stripMargin -> BoxedBoolean(false)
    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assert(expected == E(in))
      }
    }
  }

  describe("foreach expression") {
    val expectations: List[(String, Value)] = List(
      """
         |val newList = new java.util.ArrayList
         |foreach(a in [1, 2, 3, 4, 5]) {
         |  newList->add((a :> Int) * 2)
         |}
         |newList
      """.stripMargin -> ObjectValue(listOf(2, 4, 6, 8, 10))
    )

    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assertResult(expected)(E(in))
      }
    }
  }

  describe("if expression") {
    val expectations: List[(String, Value)] = List(
      """
         |if(true) 1.0 else 2.0
      """.stripMargin -> BoxedDouble(1.0),
      """
         |if(false) 1.0 else 2.0
      """.stripMargin -> BoxedDouble(2.0)
    )

    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assertResult(expected)(E(in))
      }
    }
  }

  describe("function definition") {
    val expectations: List[(String, Value)] = List(
      """
         |def add(x, y) = x + y
         |add(2, 3)
      """.stripMargin -> BoxedInt(5),
      """
         |def fact(n) = if(n < 2) 1 else (n * fact(n - 1))
         |fact(4)
      """.stripMargin -> BoxedInt(24),
      """
         |def none() = 24 cleanup "none"
         |none()
      """.stripMargin -> BoxedInt(24),
      """
         |def hello() = {
         |  "Hello"
         |  0
         |} cleanup {
         |  "World"
         |}
         |hello()
      """.stripMargin -> BoxedInt(0)
    )

    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"expectations ${i}") {
        assertResult(expected)(E(in))
      }
    }
  }

}
