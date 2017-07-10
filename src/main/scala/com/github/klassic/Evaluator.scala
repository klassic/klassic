package com.github.klassic

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}

import com.github.klassic.AST.Program

class Evaluator extends (String => Value) {
  val parser = new Parser
  val typer = new Typer
  val rewriter = new SyntaxRewriter
  val interpreter = new Interpreter
  override final def apply(program: String): Value = {
    evaluateString(program)
  }
  def evaluateFile(file: File): Value = using(new BufferedReader(new InputStreamReader(new FileInputStream(file)))){in =>
    val program = Iterator.continually(in.read()).takeWhile(_ != -1).map(_.toChar).mkString
    evaluateString(program, file.getName)
  }
  def evaluateString(program: String, fileName: String = "<no file>"): Value = {
    val parser = new Parser
    val parsedProgram = parser.process(program)
    val rewritedProgram = rewriter.process(parsedProgram)
    val typedProgram = typer.process(rewritedProgram)
    interpreter.process(typedProgram)
  }
}
