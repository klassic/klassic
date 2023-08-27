package com.github.klassic

import scala.runtime.BoxedUnit

/**
  * Created by Mizushima on 2016/05/30.
  */
class IfExpressionSpec extends SpecHelper {
  describe("simple if expression") {
    it("is evaluated correctly") {
      assertResult(
        E(
          """
            |if(true) 1.0 else 2.0
            """.stripMargin))(BoxedDouble(1.0))
      assertResult(
        E(
          """
            |if(false) 1.0 else 2.0
            """.stripMargin))(BoxedDouble(2.0))
    }
  }
  describe("no-else if expression") {
    it("is evaluated correctly") {
      assertResult(
        E(
          """
            |if(true) println("hello")
            |""".stripMargin))(UnitValue)
    }
  }
}
