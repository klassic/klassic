package com.github.klassic

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}

import com.github.klassic.Ast.Program
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
    val session = new InteractiveSession
    val parser = new Parser
    val parsedProgram = parser.process(program, session)
    val placeHolderIsDesugaredProgram = placeholderDesugerer.process(parsedProgram, session)
    val rewrittenProgram = rewriter.process(placeHolderIsDesugaredProgram, session)
    val typedProgram = typer.process(rewrittenProgram, session)
    interpreter.process(typedProgram, session)
  }
}
