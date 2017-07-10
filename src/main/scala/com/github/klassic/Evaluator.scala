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
    parser.parse(program) match {
      case parser.Success(program: Program, _) =>
        val transformedProgram = rewriter.transform(program)
        val typedProgram = typer.transform(transformedProgram)
        interpreter.interpret(typedProgram)
      case parser.Failure(m, n) => throw new InterpreterException(n.pos + ":" + m)
      case parser.Error(m, n) => throw new InterpreterException(n.pos + ":" + m)
    }
  }
}
