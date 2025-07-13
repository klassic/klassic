package com.github.klassic

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}

import com.github.klassic.Ast.Program
import com.github.scaruby.SFile

class Evaluator extends (String => Value) {
  val parser = new Parser
  val typer = new Typer
  val placeholderDesugerer = new PlaceholderDesugerer
  val rewriter = new SyntaxRewriter
  val typeClassTransformer = new TypeClassTransformer
  val vmInterpreter = new vm.VmInterpreter
  override final def apply(program: String): Value = {
    evaluateString(program)
  }
  def evaluateFile(file: SFile): Value =
    for {
      in <- file.openReader()
    } {
      val program = in.readAll()
      evaluateStringInFile(program, file.name)
    }
  def evaluateString(program: String): Value = {
    evaluateStringInFile(program, "<no file>")
  }
  def evaluateStringInFile(program: String, fileName: String): Value = {
    val session = new InteractiveSession
    val parser = new Parser
    val parsedProgram = parser.process(program, session)
    val placeHolderIsDesugaredProgram = placeholderDesugerer.process(parsedProgram, session)
    val rewrittenProgram = rewriter.process(placeHolderIsDesugaredProgram, session)
    // val transformedProgram = typeClassTransformer.transform(rewrittenProgram) 
    val typedProgram = typer.process(rewrittenProgram, session)
    
    vmInterpreter.process(typedProgram, session)
  }

  def evaluateStringWithVm(program: String): Value = {
    val session = new InteractiveSession
    val parsedProgram = parser.process(program, session)
    val placeHolderIsDesugaredProgram = placeholderDesugerer.process(parsedProgram, session)
    val rewrittenProgram = rewriter.process(placeHolderIsDesugaredProgram, session)
    // val transformedProgram = typeClassTransformer.transform(rewrittenProgram) 
    val typedProgram = typer.process(rewrittenProgram, session)
    vmInterpreter.process(typedProgram, session)
  }
}
