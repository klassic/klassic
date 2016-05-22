package com.github.klassic

import scala.collection.Iterator.continually

/**
 * @author Kota Mizushima
 */
object Main {
  def main(args: Array[String]): Unit = {
    val (flag: String, node: AstNode) = parseCommandLine(args)
    val interpreter = new Interpreter
    val result = interpreter.evaluate(node)
    flag match {
      case "-e" => println(result)
      case _ =>
    }
  }

  def parseCommandLine(args: Array[String]): (String, Any) = {
    val parser = new Parser
    val (flag, ast) = args match {
      case Array("-e", line) => ("-e", parser.parse(line).get)
      case Array(fileName) =>
        openReader(fileName) { in =>
          val program = continually(in.read()).takeWhile(_ != -1).map(_.toChar).mkString
          parser.parse(program) match {
            case parser.Success(v, _) => ("-f", v)
            case parser.Failure(m, n) => sys.error(n.pos + ":" + m)
            case parser.Error(m, n) => sys.error(n.pos + ":" + m)
          }
        }
      case Array() =>
        sys.error("please specify program")
    }
    (flag, ast)
  }
}

