package com.github.klassic

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class VmNativeFunctionSpec extends AnyFunSpec with Matchers {
  val evaluator = new Evaluator

  describe("VM Native Functions") {
    it("should handle single argument native functions") {
      val output = new java.io.ByteArrayOutputStream()
      Console.withOut(output) {
        evaluator.evaluateStringWithVm("println(42)")
      }
      output.toString.trim shouldBe "42"
    }

    it("should handle native functions with multiple arguments") {
      val result = evaluator.evaluateStringWithVm(
        """
          |assertResult(42)(42)
        """.stripMargin)
      result shouldBe UnitValue
    }

    it("should handle module functions") {
      val result = evaluator.evaluateStringWithVm(
        """
          |val list = [1, 2, 3]
          |head(list)
        """.stripMargin)
      result shouldBe BoxedInt(1)
    }

    ignore("should handle functions returning functions") {
      val result = evaluator.evaluateStringWithVm(
        """
          |val list = [1, 2, 3]
          |val addOne = (x) => x + 1
          |map(list)(addOne)
        """.stripMargin)
      result match {
        case ObjectValue(list: java.util.List[_]) =>
          list.size() shouldBe 3
          list.get(0) shouldBe 2
          list.get(1) shouldBe 3
          list.get(2) shouldBe 4
        case _ => fail("Expected list")
      }
    }

    ignore("should handle curried native functions") {
      val result = evaluator.evaluateStringWithVm(
        """
          |val list = [1, 2, 3]
          |val mapper = map(list)
          |mapper((x) => x * 2)
        """.stripMargin)
      result match {
        case ObjectValue(list: java.util.List[_]) =>
          list.size() shouldBe 3
          list.get(0) shouldBe 2
          list.get(1) shouldBe 4
          list.get(2) shouldBe 6
        case _ => fail("Expected list")
      }
    }

    it("should preserve argument order in native functions") {
      val result = evaluator.evaluateStringWithVm(
        """
          |val s = "hello world"
          |substring(s, 0, 5)
        """.stripMargin)
      result shouldBe ObjectValue("hello")
    }
  }
}