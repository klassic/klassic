package com.github.klassic

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class VmCompilerSpec extends AnyFunSpec with Matchers {
  val evaluator = new Evaluator

  describe("VM Compiler") {
    it("should compile and execute arithmetic operations") {
      val result = evaluator.evaluateStringWithVm("1 + 2 * 3")
      result shouldBe BoxedInt(7)
    }

    it("should compile and execute variable declarations") {
      val result = evaluator.evaluateStringWithVm(
        """
          |val x = 10
          |val y = 20
          |x + y
        """.stripMargin)
      result shouldBe BoxedInt(30)
    }

    it("should compile and execute if expressions") {
      val result = evaluator.evaluateStringWithVm(
        """
          |val x = 5
          |if (x > 3) 100 else 200
        """.stripMargin)
      result shouldBe BoxedInt(100)
    }

    it("should compile and execute while loops") {
      val result = evaluator.evaluateStringWithVm(
        """
          |mutable i = 0
          |mutable sum = 0
          |while (i < 5) {
          |  sum = sum + i
          |  i = i + 1
          |}
          |sum
        """.stripMargin)
      result shouldBe BoxedInt(10)
    }

    it("should compile and execute list literals") {
      val result = evaluator.evaluateStringWithVm("[1, 2, 3]")
      result match {
        case ObjectValue(list: java.util.List[_]) =>
          list.size() shouldBe 3
          list.get(0) shouldBe 1
          list.get(1) shouldBe 2
          list.get(2) shouldBe 3
        case _ => fail("Expected list")
      }
    }

    it("should compile and execute map literals") {
      val result = evaluator.evaluateStringWithVm("""%["a": 1, "b": 2]""")
      result match {
        case ObjectValue(map: java.util.Map[_, _]) =>
          map.size() shouldBe 2
          map.get("a") shouldBe 1
          map.get("b") shouldBe 2
        case _ => fail("Expected map")
      }
    }

    it("should compile and execute comparisons") {
      evaluator.evaluateStringWithVm("5 < 10") shouldBe BoxedBoolean(true)
      evaluator.evaluateStringWithVm("10 > 5") shouldBe BoxedBoolean(true)
      evaluator.evaluateStringWithVm("5 == 5") shouldBe BoxedBoolean(true)
      evaluator.evaluateStringWithVm("5 <= 5") shouldBe BoxedBoolean(true)
      evaluator.evaluateStringWithVm("5 >= 5") shouldBe BoxedBoolean(true)
    }

    it("should compile and execute logical operations") {
      evaluator.evaluateStringWithVm("true && false") shouldBe BoxedBoolean(false)
      evaluator.evaluateStringWithVm("true || false") shouldBe BoxedBoolean(true)
      evaluator.evaluateStringWithVm("5 & 3") shouldBe BoxedInt(1)
      evaluator.evaluateStringWithVm("5 | 3") shouldBe BoxedInt(7)
      evaluator.evaluateStringWithVm("5 ^ 3") shouldBe BoxedInt(6)
    }
  }
}