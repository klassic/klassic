package com.github.klassic

/**
  * Created by Mizushima on 2016/05/30.
  */
class BinaryExpressionSpec extends SpecHelper {
  describe("bit and") {
    it("is evaluated correctly (1)") {
      assertResult(
        E(
          """
            |1 & 0
          """.stripMargin))(BoxedInt(0))
    }
    it("is evaluated correctly (2)") {
      assertResult(
        E(
          """
            |1 & 1
          """.stripMargin))(BoxedInt(1))
    }
  }
  describe("bit or") {
    it("is evaluated correctly (1)") {
      assertResult(
        E(
          """
            |1 | 0
          """.stripMargin))(BoxedInt(1))
    }
    it("is evaluated correctly (2)") {
      assertResult(
        E(
          """
            |0 | 0
          """.stripMargin))(BoxedInt(0))
    }
  }
  describe("bit xor") {
    it("is evaluated correctly (1)") {
      assertResult(
        E(
          """
            |1 ^ 0
          """.stripMargin))(BoxedInt(1))
    }
    it("is evaluated correctly (2)") {
      assertResult(
        E(
          """
            |1 ^ 1
          """.stripMargin))(BoxedInt(0))
    }
    it("is evaluated correctly (3)") {
      assertResult(
        E(
          """
            |0 ^ 0
          """.stripMargin))(BoxedInt(0))
    }
  }
}
