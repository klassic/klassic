package org.onion_lang
package toys

import scala.collection.Iterator.continually
import java.lang.reflect.{Constructor, Method}

/**
 * @author Kota Mizushima
 */
object Main {
  object BuiltinEnvironment extends Environment(None) {
    def findMethod(self: AnyRef, name: String, params: Array[AnyRef]): Option[Method] = {
      val selfClass = self.getClass
      val nameMatchedMethods = selfClass.getMethods.filter{_.getName == name}
      nameMatchedMethods.find{m =>
        m.getParameterCount == params.length &&
        (m.getParameterTypes zip params.map{_.getClass}).forall{ case (arg, param) =>
            arg.isAssignableFrom(param)
        }
      }
    }
    def findConstructor(target: Class[_], params: Array[AnyRef]): Option[Constructor[_]] = {
      val constructors = target.getConstructors
      constructors.find{c =>
        c.getParameterCount == params.length &&
        (c.getParameterTypes zip params.map{_.getClass}).forall{ case (arg, param) =>
          arg.isAssignableFrom(param)
        }
      }
    }
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
      findConstructor(Class.forName(className.value), actualParams) match {
        case Some(constructor) =>
          Value.toToys(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
        case None => throw new IllegalArgumentException(s"new(${className}, ${params}")
      }
    }
    define("invoke"){ case ObjectValue(self)::StringValue(name)::params =>
      val actualParams = params.map{Value.fromToys(_)}.toArray
      findMethod(self, name, actualParams) match {
        case Some(method) => Value.toToys(method.invoke(self, actualParams:_*))
        case None => throw new IllegalArgumentException(s"invoke(${self}, ${name}, ${params})")
      }
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

