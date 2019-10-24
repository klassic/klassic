package com.github.klassic.macro_peg

sealed abstract class EvaluationStrategy
object EvaluationStrategy {
  case object CallByName extends EvaluationStrategy
  case object CallByValueSeq extends EvaluationStrategy
  case object CallByValuePar extends EvaluationStrategy
}
