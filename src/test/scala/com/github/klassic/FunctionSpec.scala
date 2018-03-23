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

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
  describe("int") {
    val expectations: List[(String, Value)] = List(
      """
        |int(2.5)
      """.stripMargin -> BoxedInt(2),
      """
        |int(2.0)
      """.stripMargin -> BoxedInt(2),
      """
        |int(0.5)
      """.stripMargin -> BoxedInt(0),
      """
        |int(-0.5)
      """.stripMargin -> BoxedInt(0),
      """
        |int(-1.5)
      """.stripMargin -> BoxedInt(-1)
    )

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
  describe("double") {
    val expectations: List[(String, Value)] = List(
      """
        |double(3)
      """.stripMargin -> BoxedDouble(3.0),
      """
        |double(2)
      """.stripMargin -> BoxedDouble(2.0),
      """
        |double(1)
      """.stripMargin -> BoxedDouble(1.0),
      """
        |double(0)
      """.stripMargin -> BoxedDouble(0.0),
      """
        |double(-1)
      """.stripMargin -> BoxedDouble(-1.0)
    )

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
  describe("floor") {
    val expectations: List[(String, Value)] = List(
      """
        |floor(2.5)
      """.stripMargin -> BoxedInt(2),
      """
        |floor(2.0)
      """.stripMargin -> BoxedInt(2),
      """
        |floor(0.5)
      """.stripMargin -> BoxedInt(0),
      """
        |floor(-0.5)
      """.stripMargin -> BoxedInt(0),
      """
        |floor(-1.5)
      """.stripMargin -> BoxedInt(-1)
    )

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
  describe("abs") {
    val expectations: List[(String, Value)] = List(
      """
        |abs(2.5)
      """.stripMargin -> BoxedDouble(2.5),
      """
        |abs(1.0)
      """.stripMargin -> BoxedDouble(1.0),
      """
        |abs(0.0)
      """.stripMargin -> BoxedDouble(0.0),
      """
        |abs(-0.5)
      """.stripMargin -> BoxedDouble(0.5),
      """
        |abs(-1.5)
      """.stripMargin -> BoxedDouble(1.5)
    )

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
  describe("substring") {
    val expectations: List[(String, Value)] = List(
      """
        |substring("FOO", 0, 1)
      """.stripMargin -> ObjectValue("F"),
      """
        |substring("FOO", 0, 2)
      """.stripMargin -> ObjectValue("FO"),
      """
        |substring("FOO", 0, 3)
      """.stripMargin -> ObjectValue("FOO"),
      """
        |substring("FOO", 1, 1)
      """.stripMargin -> ObjectValue(""),
      """
        |substring("FOO", 1, 2)
      """.stripMargin -> ObjectValue("O"),
      """
        |substring("FOO", 1, 3)
      """.stripMargin -> ObjectValue("OO")
    )

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
  describe("at") {
    val expectations: List[(String, Value)] = List(
      """
        |at("FOO", 0)
      """.stripMargin -> ObjectValue("F"),
      """
        |at("FOO", 1)
      """.stripMargin -> ObjectValue("O"),
      """
        |at("FOO", 2)
      """.stripMargin -> ObjectValue("O")
    )

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
  describe("matches") {
    val expectations: List[(String, Value)] = List(
      """
        |matches("FOO", ".*")
      """.stripMargin -> BoxedBoolean(true),
      """
        |matches("FOO", "FOO")
      """.stripMargin -> BoxedBoolean(true),
      """
        |matches("FOO", "FO")
      """.stripMargin -> BoxedBoolean(false),
      """
        |matches("FO", "FOO")
      """.stripMargin -> BoxedBoolean(false)
    )

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
  describe("sqrt") {
    val expectations: List[(String, Value)] = List(
      """
        |sqrt(4.0)
      """.stripMargin -> BoxedDouble(2.0),
      """
        |sqrt(9.0)
      """.stripMargin -> BoxedDouble(3.0),
      """
        |sqrt(2.0)
      """.stripMargin -> BoxedDouble(1.4142135623730951),
      """
        |sqrt(1.0)
      """.stripMargin -> BoxedDouble(1.0)
    )

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
}
