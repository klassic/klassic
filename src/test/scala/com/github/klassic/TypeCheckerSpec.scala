package com.github.klassic

import com.github.klassic.TypeDescription._

/**
  * Created by Mizushima on 2016/05/30.
  */
class TypeCheckerSpec extends SpecHelper {
  val I = new TypeCheckerInterpreter

  describe("assignment") {
    val expectations: List[(String, (Value, TypeDescription))] = List(
      """
        |val a=1
        |a
      """.stripMargin -> (BoxedInt(1), IntType),
      """
        |val a=1
        |a = a + 1
        |a
      """.stripMargin -> (BoxedInt(2), IntType),
      """
        |val s="FOO"
        |s=s+s
        |s
      """.stripMargin -> (ObjectValue("FOOFOO"), DynamicType)
    )

    expectations.zipWithIndex.foreach { case ((in, expected), i) =>
      it(s"expectation  ${i}") {
        assert(expected == I.evaluateString(in))
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
    inputs.foreach{in =>
      val e = intercept[InterpreterException] {
        I.evaluateString(in)
      }
      println(e)
    }
  }

  describe("valid foreach expression") {
    val expectations: List[(String, (Value, TypeDescription))] = List(
      """
        |val a = 1
        |foreach(b in [1, 2, 3]) {
        |  b + 3
        |}
      """.stripMargin -> (UnitValue, UnitType)
    )

    expectations.zipWithIndex.foreach { case ((in, expected), i) =>
      it(s"expectation  ${i}") {
        assert(expected == I.evaluateString(in))
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
        val e = intercept[InterpreterException] {
          I.evaluateString(in)
        }
        println(e)
      }
    }
  }

}

