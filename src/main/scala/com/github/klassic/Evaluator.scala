package com.github.klassic

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}

import com.github.klassic.AST.Program
import com.github.scaruby.SFile

class Evaluator extends (String => Value) {
  val parser = new Parser
  val typer = new Typer
  val rewriter = new SyntaxRewriter
  val interpreter = new Interpreter
  override final def apply(program: String): Value = {
    evaluateString(program)
  }
  def evaluateFile(file: SFile): Value =
    for {
      in <- file.open()
    } {
      val program = Iterator.continually(in.read()).takeWhile(_ != -1).map(_.toChar).mkString
      evaluateString(program, file.name)
    }
  def evaluateString(program: String, fileName: String = "<no file>"): Value = {
    val parser = new Parser
    val parsedProgram = parser.process(program)
    val rewritedProgram = rewriter.process(parsedProgram)
    val typedProgram = typer.process(rewritedProgram)
    interpreter.process(typedProgram)
  }
}
