package com.github.klassic

class TypeCheckerInterpreter {
  val interpreter = new Interpreter
  def evaluateString(input: String): (Value, TypeDescription) = {
    val typer = new TypeChecker
    val node = interpreter.doParse(input)
    val description = typer.doType(node)
    val result = interpreter.evaluate(node)
    (result, description)
  }
}
