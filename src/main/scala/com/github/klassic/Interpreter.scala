package com.github.klassic

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}
import java.lang.reflect.{Constructor, Method}

/**
 * @author Kota Mizushima
 */
class Interpreter {evaluator =>
  def reportError(message: String): Nothing = {
    throw InterpreterException(message)
  }
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
    define("newObject") { case (className: StringValue)::params =>
      val actualParams: Array[AnyRef] = params.map {param => Value.fromKlassic(param)}.toArray
      findConstructor(Class.forName(className.value), actualParams) match {
        case Some(constructor) =>
          Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
        case None => throw new IllegalArgumentException(s"newObject(${className}, ${params}")
      }
    }
    define("thread") { case List(fun: FunctionValue) =>
      new Thread {
        override def run(): Unit = {
          val env = new Environment(fun.environment)
          evaluator.evaluate(env, FunctionCall(fun.value, Nil))
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
    define("invoke"){ case ObjectValue(self)::StringValue(name)::params =>
      val actualParams = params.map{Value.fromKlassic}.toArray
      findMethod(self, name, actualParams) match {
        case Some(method) => Value.toKlassic(method.invoke(self, actualParams:_*))
        case None => throw new IllegalArgumentException(s"invoke(${self}, ${name}, ${params})")
      }
    }
  }
  def evaluateFile(file: File): Value = using(new BufferedReader(new InputStreamReader(new FileInputStream(file)))){in =>
    val program = Iterator.continually(in.read()).takeWhile(_ != -1).map(_.toChar).mkString
    evaluateString(program)
  }
  def evaluateString(program: String): Value = {
    val parser = new Parser
    parser.parse(program) match {
      case parser.Success(node: AstNode, _) => evaluate(node)
      case parser.Failure(m, n) => sys.error(n.pos + ":" + m)
      case parser.Error(m, n) => sys.error(n.pos + ":" + m)
    }
  }
  def evaluate(node: AstNode): Value = evaluate(BuiltinEnvironment, node)
  def evaluate(env:Environment, node: AstNode): Value = {
    def evalRecursive(node: AstNode): Value = {
      node match{
        case Block(exprs) =>
          val local = new Environment(Some(env))
          exprs.foldLeft(UnitValue:Value){(result, x) => evaluate(local, x)}
        case IfExpr(cond, pos, neg) =>
          evalRecursive(cond) match {
            case BooleanValue(true) => evalRecursive(pos)
            case BooleanValue(false) => evalRecursive(neg)
            case _ => reportError("type error")
          }
        case BinaryExpression(Operator.LESS_THAN, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (IntValue(lval), IntValue(rval)) => BooleanValue(lval < rval)
            case (LongValue(lval), LongValue(rval)) => BooleanValue(lval < rval)
            case (ShortValue(lval), ShortValue(rval)) => BooleanValue(lval < rval)
            case (ByteValue(lval), ByteValue(rval)) => BooleanValue(lval < rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(Operator.GREATER_THAN, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (IntValue(lval), IntValue(rval)) => BooleanValue(lval > rval)
            case (LongValue(lval), LongValue(rval)) => BooleanValue(lval > rval)
            case (ShortValue(lval), ShortValue(rval)) => BooleanValue(lval > rval)
            case (ByteValue(lval), ByteValue(rval)) => BooleanValue(lval > rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(Operator.LESS_OR_EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (IntValue(lval), IntValue(rval)) => BooleanValue(lval <= rval)
            case (LongValue(lval), LongValue(rval)) => BooleanValue(lval <= rval)
            case (ShortValue(lval), ShortValue(rval)) => BooleanValue(lval <= rval)
            case (ByteValue(lval), ByteValue(rval)) => BooleanValue(lval <= rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(Operator.GREATER_EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (IntValue(lval), IntValue(rval)) => BooleanValue(lval >= rval)
            case (LongValue(lval), LongValue(rval)) => BooleanValue(lval >= rval)
            case (ShortValue(lval), ShortValue(rval)) => BooleanValue(lval >= rval)
            case (ByteValue(lval), ByteValue(rval)) => BooleanValue(lval >= rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(Operator.ADD, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (IntValue(lval), IntValue(rval)) => IntValue(lval + rval)
            case (LongValue(lval), LongValue(rval)) => LongValue(lval + rval)
            case (ShortValue(lval), ShortValue(rval)) => ShortValue((lval + rval).toShort)
            case (ByteValue(lval), ByteValue(rval)) => ByteValue((lval + rval).toByte)
            case (StringValue(lval), rval) => StringValue(lval + rval)
            case (lval, StringValue(rval)) => StringValue(lval + rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case BinaryExpression(Operator.SUBTRACT, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (IntValue(lval), IntValue(rval)) => IntValue(lval - rval)
            case (LongValue(lval), LongValue(rval)) => LongValue(lval - rval)
            case (ShortValue(lval), ShortValue(rval)) => ShortValue((lval - rval).toShort)
            case (ByteValue(lval), ByteValue(rval)) => ByteValue((lval - rval).toByte)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case BinaryExpression(Operator.MULTIPLY, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (IntValue(lval), IntValue(rval)) => IntValue(lval * rval)
            case (LongValue(lval), LongValue(rval)) => LongValue(lval * rval)
            case (ShortValue(lval), ShortValue(rval)) => ShortValue((lval * rval).toShort)
            case (ByteValue(lval), ByteValue(rval)) => ByteValue((lval * rval).toByte)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case BinaryExpression(Operator.DIVIDE, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (IntValue(lval), IntValue(rval)) => IntValue(lval / rval)
            case (LongValue(lval), LongValue(rval)) => LongValue(lval / rval)
            case (ShortValue(lval), ShortValue(rval)) => ShortValue((lval / rval).toShort)
            case (ByteValue(lval), ByteValue(rval)) => ByteValue((lval / rval).toByte)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case MinusOp(operand) =>
          evalRecursive(operand) match {
            case IntValue(value) => IntValue(-value)
            case LongValue(value) => LongValue(-value)
            case ShortValue(value) => ShortValue((-value).toShort)
            case ByteValue(value) => ByteValue((-value).toByte)
            case _ => reportError("- cannot be applied to non-integer value")
          }
        case PlusOp(operand) =>
          evalRecursive(operand) match {
            case IntValue(value) => IntValue(value)
            case LongValue(value) => LongValue(value)
            case ShortValue(value) => ShortValue(value)
            case ByteValue(value) => ByteValue(value)
            case _ => reportError("+ cannot be applied to non-integer value")
          }
        case IntNode(value) =>
          IntValue(value)
        case StringNode(value) =>
          StringValue(value)
        case LongNode(value) =>
          LongValue(value)
        case ShortNode(value) =>
          ShortValue(value)
        case ByteNode(value) =>
          ByteValue(value)
        case ListLiteral(elements) =>
          val params = elements.map{evalRecursive(_)}
          val newList = new java.util.ArrayList[Any]
          params.foreach{param =>
            newList.add(param)
          }
          ObjectValue(newList)
        case PrintLine(value) =>
          val v = evalRecursive(value)
          println(v)
          v
        case Identifier(name) => env(name)
        case ValDeclaration(vr, value) =>
          env(vr) = evalRecursive(value)
        case Assignment(vr, value) =>
          env.set(vr, evalRecursive(value))
        case func@FunctionLiteral(_, _) =>
          FunctionValue(func, Some(env))
        case FunctionDefinition(name, func) =>
          env(name) = FunctionValue(func, Some(env)): Value
        case MethodCall(self, name, params) =>
          evalRecursive(self) match {
            case ObjectValue(value) =>
              val actualParams = params.map{p => Value.fromKlassic(evalRecursive(p))}.toArray
              val method = findMethod(value, name.name, actualParams).get
              Value.toKlassic(method.invoke(value, actualParams:_*))
          }
        case NewObject(className, params) =>
          val actualParams: Array[AnyRef] = params.map {p => Value.fromKlassic(evalRecursive(p))}.toArray
          findConstructor(Class.forName(className), actualParams) match {
            case Some(constructor) =>
              Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
            case None => reportError(s"new(${className}, ${params} is illegal")
          }
        case FunctionCall(func, params) =>
          evalRecursive(func) match{
            case FunctionValue(FunctionLiteral(fparams, proc), cenv) =>
              val local = new Environment(cenv)
              (fparams zip params).foreach{ case (fp, ap) =>
                local(fp) = evalRecursive(ap)
              }
              evaluate(local, proc)
            case NativeFunctionValue(body) =>
              val actualParams = params.map{evalRecursive(_)}
              if(body.isDefinedAt(actualParams)) {
                body(params.map{p => evalRecursive(p)})
              } else {
                reportError("parameters are not matched to the function's arguments")
              }
            case _ =>
              reportError("unknown error")
          }
      }
    }
    evalRecursive(node)
  }
}
