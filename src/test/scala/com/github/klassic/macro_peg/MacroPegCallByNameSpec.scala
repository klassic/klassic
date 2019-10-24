package com.github.klassic.macro_peg

import org.scalatest.{DiagrammedAssertions, FunSpec}
import EvaluationResult._
import Runner._

class MacroPegCallByNameSpec extends FunSpec with DiagrammedAssertions {
  describe("Macro PEG with call by name example") {
    it("palindrome") {
      val results = evalGrammar(
        """
          |S = P("") !.;
          |P(r) = "a" P("a" r) / "b" P("b" r) / r;
     """.stripMargin,
        Seq("abba", "abba","abbbba", "a"),
        EvaluationStrategy.CallByName
      )
      assertResult(Seq(Success(""), Success(""), Success(""), Failure))(results)
    }
  }
}

