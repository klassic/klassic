package com.github.klassic

/**
  * Created by Mizushima on 2016/05/30.
  */
class TypeCheckerSpec extends SpecHelper {
  describe("assignment") {
    val expectations: List[(String, Value)] = List(
      """
        |val a=1
        |a
      """.stripMargin -> BoxedInt(1),
      """
        |mutable a=1
        |a = a + 1
        |a
      """.stripMargin -> BoxedInt(2),
      """
        |mutable s="FOO"
        |s=s+s
        |s
      """.stripMargin -> ObjectValue("FOOFOO")
    )

    expectations.zipWithIndex.foreach { case ((in, expected), i) =>
      it(s"expectation  ${i}") {
        assert(expected == E(in))
      }
    }
  }

  describe("valid function type") {
    val expectations: List[(String, Value)] = List(
      """
        |def add(x: Int, y: Int): Int = x + y
        |val f: (Int, Int) => Int = add
        |f(2, 3)
      """.stripMargin -> BoxedInt(5))

    expectations.zipWithIndex.foreach { case ((in, expected), i) =>
      it(s"expectation  ${i}") {
        assertResult(expected)(E(in))
      }
    }
  }

  describe("arithmetic operation between incompatible type cannot be done") {
    val inputs = List(
      """
        |val a = 1
        |val b = 2L
        |1 + 2L
      """.stripMargin,
      """
        |val a = 1
        |val b: Long = a
        |b
      """.stripMargin
    )
    inputs.zipWithIndex.foreach{ case (in, i) =>
      it(s"expectation  ${i}") {
        val e = intercept[TyperException] {
          E(in)
        }
      }
    }
  }

  describe("valid foreach expression") {
    val expectations: List[(String, Value)] = List(
      """
        |mutable a = 1
        |a = 2
        |foreach(b in [1, 2, 3]) {
        |  (b :> Int) + 3
        |}
      """.stripMargin -> UnitValue
    )

    expectations.zipWithIndex.foreach { case ((in, expected), i) =>
      it(s"expectation  ${i}") {
        assert(expected == E(in))
      }
    }
  }

  describe("invalid foreach expression") {
    val illTypedPrograms: List[String] = List(
      """
        |val a = 1
        |foreach(a in [1, 2, 3]) {
        |  b + 3
        |}
      """.stripMargin
    )
    illTypedPrograms.zipWithIndex.foreach { case (in, i) =>
      it(s"expectation  ${i}") {
        val e = intercept[TyperException] {
          E(in)
        }
      }
    }
  }

  describe("val channot change its value") {
    val illTypedPrograms: List[String] = List(
      """
        |val a = 1
        |a = 2
        |a
      """.stripMargin
    )
    illTypedPrograms.zipWithIndex.foreach { case (in, i) =>
      it(s"expectation  ${i}") {
        val e = intercept[TyperException] {
          E(in)
        }
      }
    }
  }

  describe("function type doesn't match ") {
    val illTypedPrograms: List[String] = List(
      """
        |def f(x, y) = x + y
        |f(10)
      """.stripMargin
    )
    illTypedPrograms.zipWithIndex.foreach { case (in, i) =>
      it(s"expectation  ${i}") {
        val e = intercept[TyperException] {
          E(in)
        }
      }
    }
  }

}

