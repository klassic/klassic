package com.github.klassic

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class DoubleEvaluationSpec extends AnyFunSpec with Matchers {
  val evaluator = new Evaluator

  describe("Native function calls") {
    it("should not evaluate arguments twice") {
      val result = evaluator.evaluateStringInFile(
        """
          |mutable counter = 0
          |val incrementAndReturn = () => {
          |  counter = counter + 1
          |  counter
          |}
          |
          |// This native function call should evaluate incrementAndReturn() only once
          |println(incrementAndReturn())
          |
          |// Counter should be 1, not 2
          |counter
        """.stripMargin, 
        "test.kl"
      )
      result shouldBe BoxedInt(1)
    }
  }
}