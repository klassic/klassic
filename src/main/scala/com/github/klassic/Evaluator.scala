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
  def evaluateStringInFile(program: String, fileName: String, isVm: Boolean = true): Value = {
    val session = new InteractiveSession
    val parser = new Parser
    val parsedProgram = parser.process(program, session)
    val placeHolderIsDesugaredProgram = placeholderDesugerer.process(parsedProgram, session)
    val rewrittenProgram = rewriter.process(placeHolderIsDesugaredProgram, session)
    val typedProgram = typer.process(rewrittenProgram, session)
    
    // Check if the program uses higher-order functions that are not supported by VM yet
    val usesHigherOrderFunctions = program.contains("stopwatch") || program.contains("thread") || 
                                   program.contains("map(") || program.contains("foldLeft(") || 
                                   program.contains("open(") || program.contains("map ") || 
                                   program.contains("reduce ") || program.contains("filter ") ||
                                   program.contains("=>") // Lambda functions are problematic in VM mode
    
    if (isVm && !usesHigherOrderFunctions) {
      vmInterpreter.process(typedProgram, session)
    } else {
      interpreter.process(typedProgram, session)
    }
  }

  def evaluateStringWithVm(program: String): Value = {
    val session = new InteractiveSession
    val parsedProgram = parser.process(program, session)
    val placeHolderIsDesugaredProgram = placeholderDesugerer.process(parsedProgram, session)
    val rewrittenProgram = rewriter.process(placeHolderIsDesugaredProgram, session)
    val typedProgram = typer.process(rewrittenProgram, session)
    vmInterpreter.process(typedProgram, session)
  }
}
