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
    define("thread") { case List(fun: FunctionValue) =>
        new Thread {
          override def run(): Unit = {
            val interpreter = new Interpreter
            val env = new Environment(fun.environment)
            interpreter.evaluate(env, FunctionCall(fun.value, List()))
          }
        }.start()
        UnitValue
    }
    define("stopwatch") { case List(fun: FunctionValue) =>
      val interpreter = new Interpreter
      val env = new Environment(fun.environment)
      val start = System.currentTimeMillis()
      interpreter.evaluate(env, FunctionCall(fun.value, List()))
      val end = System.currentTimeMillis()
      IntValue((end - start).toInt)
    }
    define("sleep"){ case List(milliseconds: IntValue) =>
      Thread.sleep(milliseconds.value)
      UnitValue
    }
    define("new") { case (className: StringValue)::params =>
      val actualParams: Array[AnyRef] = params.map {param => Value.fromToys(param)}.toArray
      val actualClasses = actualParams.map{_.getClass}
      val constructor = Class.forName(className.value).getConstructor(actualClasses:_*)
      Value.toToys(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
    }
    define("invoke"){ case ObjectValue(self)::StringValue(name)::params =>
      val actualParams = params.map{Value.fromToys(_)}.toArray
      val actualClasses = actualParams.map{_.getClass}
      Value.toToys(self.getClass.getMethod(name, actualClasses:_*).invoke(self, actualParams))
    }
  }
  def main(args: Array[String]): Unit = {
    val (flag: String, ast: AstNode) = parseCommandLine(args)
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

