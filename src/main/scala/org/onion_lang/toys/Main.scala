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




/*
class TrampolineInterpreter {
  def evaluate(env:Environment, ast:AST, C: Value => Value): Value = {
    def visit(ast:AST)(C: Value => Value): Value = {
      ast match{
        case Lines(exprs) =>{
            val local = new Environment(Some(env))
            exprs.foldLeft(UnitValue:Value){(result, x) => evaluate(local, x)}
        }
        case IfExpr(cond, pos, neg) =>
          visit(cond){
            case BooleanValue(true) => visit(pos)(C)
            case BooleanValue(false) => visit(neg)(C)
            case _ => typeError()
          }
        case LessOp(left, right) =>
          visit(left){lv =>
            visit(right){rv =>
              (lv, rv) match {
                case (IntValue(lval), IntValue(rval)) => BooleanValue(lval < rval)
                case _ => typeError()
              }
            }
          }
        case AddOp(left, right) =>
          visit(left){lv =>
            visit(right){rv =>
              (lv, rv) match {
                case (IntValue(lval), IntValue(rval)) => C(IntValue(lval + rval))
                case (StringValue(lval), rval) => C(StringValue(lval + rval))
                case (lval, StringValue(rval)) => C(StringValue(lval + rval))
                case _ => typeError()
              }
            }
          }
        case SubOp(left, right) =>
          visit(left){lv =>
             visit(right){rv =>
               (lv, rv) match {
                 case (IntValue(lval), IntValue(rval)) => C(IntValue(lval - rval))
                 case _ => typeError()
               }
             }
           }
        case MulOp(left, right) =>
          visit(left){lv =>
              visit(right){rv =>
                (lv, rv) match {
                  case (IntValue(lval), IntValue(rval)) => C(IntValue(lval * rval))
                  case _ => typeError()
                }
              }
          }
        case DivOp(left, right) =>
          visit(left){lv =>
                visit(right){rv =>
                  (lv, rv) match {
                    case (IntValue(lval), IntValue(rval)) => C(IntValue(lval / rval))
                    case _ => typeError()
                  }
                }
            }
        case IntVal(value) => C(IntValue(value))
        case StringVal(value) => C(StringValue(value))
        case PrintLine(value) =>
          visit(value){v =>
            println(v)
            C(v)
          }
        case Ident(name) =>
          C(env(name))
        case ValDeclaration(vr, value) =>
          visit(value){v =>
            env(vr) = v
            C(v)
          }
        case Assignment(vr, value) =>
          visit(value){v =>
            env.set(vr) = v
            C(v)
          }
        case func@Func(_, _) =>
          C(FunctionValue(func, Some(env)))
        case FuncDef(name, func) => env(name) =
          C(FunctionValue(func, Some(env)):Value)
        case FuncCall(func, params) =>
          visit(func) {
            case FunctionValue(Func(fparams, proc), cenv) =>
              val local = new Environment(cenv)
              var local_cont = C
              (fparams zip params).foreach{ case (fp, ap) =>
                visit(ap){v =>
                  local(fp) = v
                }
              }
              evaluate(local, proc)
            case _ => sys.error("Runtime Error!")
          }
      }
    }
    visit(ast)
  }
  def typeError(): Nothing = throw new RuntimeException("Type Error")
}

*/
