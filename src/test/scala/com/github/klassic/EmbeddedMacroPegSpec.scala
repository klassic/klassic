package com.github.klassic

/**
  * Created by Mizushima on 2016/05/30.
  */
class EmbeddedMacroPegSpec extends SpecHelper {
  describe("Macro PEG that is embedded") {
    it("is parsable") {
      assertResult(
        E(
          """
            |rule {
            |  S = "a";
            |}
            |1
          """.stripMargin))(BoxedInt(1))
    }
  }
}

