package com.github.klassic

class ListFunctionsSpec extends SpecHelper {
  describe("head") {
    val expectations: List[(String, Value)] = List(
      """
        |head([1])
      """.stripMargin -> BoxedInt(1),
      """
        |head([2 1])
      """.stripMargin -> BoxedInt(2),
      """
        |head([3 2 1])
      """.stripMargin -> BoxedInt(3)
    )

    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }

  describe("tail") {
    val expectations: List[(String, Value)] = List(
      """
        |tail([1])
      """.stripMargin -> ObjectValue(listOf()),
      """
        |tail([2 1])
      """.stripMargin -> ObjectValue(listOf(1)),
      s"""
        |tail([3 2 1])
      """.stripMargin -> ObjectValue(listOf(2, 1))
    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }

  describe("cons") {
    val expectations: List[(String, Value)] = List(
      """
         | cons(1)([])
      """.stripMargin -> ObjectValue(listOf(1)),
      """
         | cons(2)([1])
      """.stripMargin -> ObjectValue(listOf(2, 1)),
      """
        | cons(3)([2, 1])
      """.stripMargin -> ObjectValue(listOf(3, 2, 1)),
      """
        | 3 #cons (2 #cons (1 #cons []))
      """.stripMargin -> ObjectValue(listOf(3, 2, 1))
    )

    expectations.zipWithIndex.foreach { case ((in, expected), i) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }

  describe("size") {
    val expectations: List[(String, Value)] = List(
      """
        |size([])
      """.stripMargin -> BoxedInt(0),
      """
        |size([1])
      """.stripMargin -> BoxedInt(1),
      """
        |size([2 1])
      """.stripMargin -> BoxedInt(2),
      """
        |size([3 2 1])
      """.stripMargin -> BoxedInt(3)
    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }

  describe("isEmpty") {
    val expectations: List[(String, Value)] = List(
      """
        |isEmpty([])
      """.stripMargin -> BoxedBoolean(true),
      """
        |isEmpty([1])
      """.stripMargin -> BoxedBoolean(false),
      """
        |isEmpty([2 1])
      """.stripMargin -> BoxedBoolean(false),
      """
        |isEmpty([3 2 1])
      """.stripMargin -> BoxedBoolean(false)
    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }

  describe("map") {
    expect("for empty list")(
      """
        |map([])((x) => x + 1)
      """.stripMargin, ObjectValue(listOf())
    )
    expect("for a non empty list and a function that add arg to 1")(
      """
        |map([1 2 3])((x) => x + 1)
      """.stripMargin, ObjectValue(listOf(2, 3, 4))
    )
  }

  describe("map syntax") {
    expect("for empty list")(
      """
        |[] map x => x + 1
    """.stripMargin, ObjectValue(listOf())
    )
    expect("for a non empty list and block that add arg to 1")(
      """
        |[1 2 3] map x => x + 1
    """.stripMargin, ObjectValue(listOf(2, 3, 4))
    )
    expect("for a non empty list and non-param block that add arg to 1")(
      """
        |[1 2 3] map =>e + 1
""".stripMargin, ObjectValue(listOf(2, 3, 4))
    )
  }

  describe("reduce syntax") {
    expect("with an empty list and result:0 returns 0")(
      """
        |[] reduce 0, (r, e) => r + e
  """.stripMargin, BoxedInt(0)
    )
    expect("with a list and block that calculate e to r returns sum of elements")(
      """
        |[1 2 3] reduce 0, (r, e) => r + e
  """.stripMargin, BoxedInt(6)
    )
    expect("with a list and parameter-less block that calculate r + e returns sum of elements")(
      """
        |[1 2 3 4] reduce 0 => r + e
""".stripMargin, BoxedInt(10)
    )
  }
}
