package com.github.klassic

class TypeCheckerInterpreter {
  val interpreter = new Interpreter
  def evaluateString(input: String): (Value, TypeDescription) = {
    val typer = new TypeChecker
    val node = interpreter.parse(input)
    val description = typer.typed(node)
    val result = interpreter.evaluate(node)
    (result, description)
  }
}
