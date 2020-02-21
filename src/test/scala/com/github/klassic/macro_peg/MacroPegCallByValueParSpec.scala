package com.github.klassic.macro_peg

import Runner._
import com.github.klassic.macro_peg.EvaluationResult._
import org.scalatest.diagrams.Diagrams
import org.scalatest.funspec.AnyFunSpec

class MacroPegCallByValueParSpec extends AnyFunSpec with Diagrams {
  describe("Macro PEG with call by value par example") {
    it("simple") {
      val results = evalGrammar(
        """
          |S = F("a"); F(A) = A A A;
     """.stripMargin,
        Seq("aaa"),
        EvaluationStrategy.CallByValuePar
      )
      assertResult(Seq(Success("")))(results)
    }

    it("xml") {
      val results = evalGrammar(
        """
          |S = "<" F([a-zA-Z_]+); F(N) = N ">" ("<" F([a-zA-Z_]+))* "</" N ">";
        """.stripMargin,
        Seq( "<a><b></b></a>"),
        EvaluationStrategy.CallByValuePar
      )
      assertResult(Seq(Success("")))(results)
    }
  }
}

