package com.github.klassic.macro_peg

sealed trait EvaluationResult {
  def orElse(that: EvaluationResult): EvaluationResult
  def flatMap(fun: String => EvaluationResult): EvaluationResult
  def map(fun: String => String): EvaluationResult
  def get: String
  def getOrElse(default: String): String
  def isSuccess: Boolean
}
object EvaluationResult {
  case class Success(remained: String) extends EvaluationResult {
    def orElse(that: EvaluationResult): EvaluationResult = this
    def flatMap(fun: String => EvaluationResult): EvaluationResult = fun(remained)
    def map(fun: String => String): EvaluationResult = Success(fun(remained))
    def get: String = remained
    def getOrElse(default: String): String = remained
    override def isSuccess: Boolean = true
  }
  case object Failure extends EvaluationResult {
    def orElse(that: EvaluationResult): EvaluationResult= that
    def flatMap(fun: String => EvaluationResult): EvaluationResult = this
    def map(fun: String => String): EvaluationResult = this
    def get: String = throw new IllegalStateException("Failure")
    def getOrElse(default: String): String = default
    override def isSuccess: Boolean = false
  }
}
