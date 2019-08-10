package com.github.klassic

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}

import com.github.klassic.AST.Program
import com.github.scaruby.SFile

class Evaluator extends (String => Value) {
  val parser = new Parser
  val typer = new Typer
  val placeholderDesugerer = new PlaceholderDesugerer
  val rewriter = new SyntaxRewriter
  val interpreter = new Interpreter
  override final def apply(program: String): Value = {
    evaluateString(program)
  }
  def evaluateFile(file: SFile): Value =
    for {
      in <- file.openReader()
    } {
      val program = in.readAll()
      evaluateString(program, file.name)
    }
  def evaluateString(program: String, fileName: String = "<no file>"): Value = {
    val parser = new Parser
    val parsedProgram = parser.process(program)
    val placeHolderIsDesugaredProgram = placeholderDesugerer.process(parsedProgram)
    val rewrittenProgram = rewriter.process(placeHolderIsDesugaredProgram)
    val typedProgram = typer.process(rewrittenProgram)
    interpreter.process(typedProgram)
  }
}
