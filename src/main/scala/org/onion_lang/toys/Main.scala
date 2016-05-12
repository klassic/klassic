package org.onion_lang
package toys

import scala.collection.Iterator.continually

/**
 * @author Kota Mizushima
 */
object Main {
  object BuiltinEnvironment extends Environment(None) {
    define("substring"){ case List(s: StringValue, begin: IntValue, end: IntValue) =>
        StringValue(s.value.substring(begin.value, end.value))
    }
    define("at") { case List(s: StringValue, index: IntValue) =>
        StringValue(s.value.substring(index.value, index.value + 1))
    }
    define("matches") { case List(str: StringValue, regex: StringValue) =>
        BooleanValue(str.value.matches(regex.value))
    }
  }
  def main(args: Array[String]): Unit = {
    val (flag: String, ast: AST) = parseCommandLine(args)
    val interpreter = new Interpreter
    val result = interpreter.evaluate(BuiltinEnvironment, ast)
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

