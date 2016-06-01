package com.github.klassic

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}
import java.lang.reflect.{Constructor, Method}

import com.github.klassic.AstNode._

/**
 * @author Kota Mizushima
 */
class Interpreter {evaluator =>
  object SymbolGenerator {
    private[this] var counter: Int = 0
    def symbol(): String = {
      val name = "var" + counter
      counter += 1
      name
    }
  }
  import SymbolGenerator.symbol
  def reportError(message: String): Nothing = {
    throw InterpreterException(message)
  }

  def findMethod(self: AnyRef, name: String, params: Array[Value]): MethodSearchResult = {
    val selfClass = self.getClass
    val nameMatchedMethods = selfClass.getMethods.filter {
      _.getName == name
    }
    nameMatchedMethods.find { m =>
      val parameterCountMatches = m.getParameterCount == params.length
      val parameterTypes = Value.classesOfValues(params)
      val parameterTypesMatches = (m.getParameterTypes zip parameterTypes).forall{ case (arg, param) =>
        arg.isAssignableFrom(param)
      }
      parameterCountMatches && parameterTypesMatches
    }.map{m =>
      m.setAccessible(true)
      UnboxedVersionMethodFound(m)
    }.orElse({
      nameMatchedMethods.find{m =>
        val parameterCountMatches = m.getParameterCount == params.length
        val boxedParameterTypes = Value.boxedClassesOfValues(params)
        val boxedParameterTypesMatches = (m.getParameterTypes zip boxedParameterTypes).forall{ case (arg, param) =>
          arg.isAssignableFrom(param)
        }
        parameterCountMatches && boxedParameterTypesMatches
      }
    }.map{m =>
      m.setAccessible(true)
      BoxedVersionMethodFound(m)
    }).getOrElse(NoMethodFound)
  }

  def findConstructor(target: Class[_], params: Array[Value]): ConstructorSearchResult = {
    val constructors = target.getConstructors
    constructors.find{c =>
      val parameterCountMatches = c.getParameterCount == params.length
      val unboxedParameterTypes = Value.classesOfValues(params)
      val parameterTypesMatches  = (c.getParameterTypes zip unboxedParameterTypes).forall{ case (arg, param) =>
        arg.isAssignableFrom(param)
      }
      parameterCountMatches && parameterTypesMatches
    }.map{c =>
      UnboxedVersionConstructorFound(c)
    }.orElse({
      constructors.find{c =>
        val parameterCountMatches = c.getParameterCount == params.length
        val boxedParameterTypes = Value.boxedClassesOfValues(params)
        val parameterTypesMatches  = (c.getParameterTypes zip boxedParameterTypes).forall{ case (arg, param) =>
          arg.isAssignableFrom(param)
        }
        parameterCountMatches && parameterTypesMatches
      }
    }.map { c =>
      BoxedVersionConstructorFound(c)
    }).getOrElse(NoConstructorFound)
  }

  object BuiltinEnvironment extends Environment(None) {
    define("substring"){ case List(ObjectValue(s:String), begin: BoxedInt, end: BoxedInt) =>
      ObjectValue(s.substring(begin.value, end.value))
    }
    define("at") { case List(ObjectValue(s:String), index: BoxedInt) =>
      ObjectValue(s.substring(index.value, index.value + 1))
    }
    define("matches") { case List(ObjectValue(s: String), ObjectValue(regex: String)) =>
      BoxedBoolean(s.matches(regex))
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
    define("println") { case List(param) =>
      println(param)
      param
    }
    define("stopwatch") { case List(fun: FunctionValue) =>
      val interpreter = new Interpreter
      val env = new Environment(fun.environment)
      val start = System.currentTimeMillis()
      interpreter.evaluate(env, FunctionCall(fun.value, List()))
      val end = System.currentTimeMillis()
      BoxedInt((end - start).toInt)
    }
    define("sleep"){ case List(milliseconds: BoxedInt) =>
      Thread.sleep(milliseconds.value)
      UnitValue
    }
    define("invoke"){ case ObjectValue(self)::ObjectValue(name:String)::params =>
      val paramsArray = params.toArray
      findMethod(self, name, paramsArray) match {
        case UnboxedVersionMethodFound(method) =>
          val actualParams = paramsArray.map{Value.fromKlassic}
          Value.toKlassic(method.invoke(self, actualParams:_*))
        case BoxedVersionMethodFound(method) =>
          val actualParams = paramsArray.map{Value.fromKlassic}
          Value.toKlassic(method.invoke(self, actualParams:_*))
        case NoMethodFound =>
          throw new IllegalArgumentException(s"invoke(${self}, ${name}, ${params})")
      }
    }
    define("newObject") { case ObjectValue(className:java.lang.String)::params =>
      val paramsArray = params.toArray
      findConstructor(Class.forName(className), paramsArray) match {
        case UnboxedVersionConstructorFound(constructor) =>
          val actualParams = paramsArray.map{Value.fromKlassic}
          Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
        case BoxedVersionConstructorFound(constructor) =>
          val actualParams = paramsArray.map{Value.fromKlassic}
          Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
        case NoConstructorFound =>
          throw new IllegalArgumentException(s"newObject(${className}, ${params}")
      }
    }

  }
  def evaluateFile(file: File): Value = using(new BufferedReader(new InputStreamReader(new FileInputStream(file)))){in =>
    val program = Iterator.continually(in.read()).takeWhile(_ != -1).map(_.toChar).mkString
    evaluateString(program)
  }
  def evaluateString(program: String, fileName: String = "<no file>"): Value = {
    val parser = new Parser
    parser.parse(program) match {
      case parser.Success(node: AstNode, _) => evaluate(node)
      case parser.Failure(m, n) => throw new InterpreterException(n.pos + ":" + m)
      case parser.Error(m, n) => throw new InterpreterException(n.pos + ":" + m)
    }
  }
  def evaluate(node: AstNode): Value = evaluate(BuiltinEnvironment, node)
  def evaluate(env:Environment, node: AstNode): Value = {
    def rewrite(node: AstNode): AstNode = node match {
      case Block(expressions) => Block(expressions.map{rewrite})
      case IfExpression(cond: AstNode, pos: AstNode, neg: AstNode) =>
        IfExpression(rewrite(cond), rewrite(pos), rewrite(neg))
      case WhileExpression(condition, body: AstNode) =>
        WhileExpression(rewrite(condition), rewrite(body))
      case ForeachExpression(name, collection, body) =>
        val itVariable = symbol()
        Block(List(
          ValDeclaration(itVariable, MethodCall(rewrite(collection), "iterator", List())),
          WhileExpression(
            BinaryExpression(
              Operator.EQUAL,
              MethodCall(Identifier(itVariable), "hasNext", List()),
              BooleanNode(true)
            ),
            Block(List(
              ValDeclaration(name, MethodCall(Identifier(itVariable), "next", List())),
              body
            ))
          )
        ))
      case BinaryExpression(operator: Operator, lhs: AstNode, rhs: AstNode) =>
        BinaryExpression(operator, rewrite(lhs), rewrite(rhs))
      case MinusOp(operand) => MinusOp(rewrite(operand))
      case PlusOp(operand) => PlusOp(rewrite(operand))
      case n@StringNode(value) => n
      case n@IntNode(value) => n
      case n@LongNode(value)  => n
      case n@ShortNode(value) => n
      case n@ByteNode(value) => n
      case n@BooleanNode(value) => n
      case n@Identifier(name) => n
      case n@DoubleNode(name) => n
      case n@FloatNode(name) => n
      case Assignment(variable, value) => Assignment(variable, rewrite(value))
      case ValDeclaration(variable, value) => ValDeclaration(variable, rewrite(value))
      case FunctionLiteral(params, proc) => FunctionLiteral(params, rewrite(proc))
      case FunctionDefinition(name, func) => FunctionDefinition(name, rewrite(func).asInstanceOf[FunctionLiteral])
      case FunctionCall(func, params) => FunctionCall(rewrite(func), params.map{rewrite})
      case ListLiteral(elements) =>  ListLiteral(elements.map{rewrite})
      case NewObject(className, params) => NewObject(className, params.map{rewrite})
      case MethodCall(self, name, params) => MethodCall(rewrite(self), name, params.map{rewrite})
    }
    def evalRecursive(node: AstNode): Value = {
      node match{
        case Block(exprs) =>
          val local = new Environment(Some(env))
          exprs.foldLeft(UnitValue:Value){(result, x) => evaluate(local, x)}
        case WhileExpression(cond, body) =>
          while(evalRecursive(cond) == BoxedBoolean(true)) {
            evalRecursive(body)
          }
          UnitValue
        case IfExpression(cond, pos, neg) =>
          evalRecursive(cond) match {
            case BoxedBoolean(true) => evalRecursive(pos)
            case BoxedBoolean(false) => evalRecursive(neg)
            case _ => reportError("type error")
          }
        case BinaryExpression(Operator.EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval == rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedBoolean(lval == rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedBoolean(lval == rval)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedBoolean(lval == rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedBoolean(lval == rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedBoolean(lval == rval)
            case (BoxedBoolean(lval), BoxedBoolean(rval)) => BoxedBoolean(lval == rval)
            case (BoxedBoolean(lval), ObjectValue(rval:java.lang.Boolean)) => BoxedBoolean(lval == rval.booleanValue())
            case (ObjectValue(lval:java.lang.Boolean), BoxedBoolean(rval)) => BoxedBoolean(lval.booleanValue() == rval)
            case (ObjectValue(lval), ObjectValue(rval)) => BoxedBoolean(lval == rval)
            case _ => reportError("comparation must be done between same types")
          }
        case BinaryExpression(Operator.LESS_THAN, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval < rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedBoolean(lval < rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedBoolean(lval < rval)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedBoolean(lval < rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedBoolean(lval < rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedBoolean(lval < rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(Operator.GREATER_THAN, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval > rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedBoolean(lval > rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedBoolean(lval > rval)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedBoolean(lval > rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedBoolean(lval > rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedBoolean(lval > rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(Operator.LESS_OR_EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval <= rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedBoolean(lval <= rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedBoolean(lval <= rval)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedBoolean(lval <= rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedBoolean(lval <= rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedBoolean(lval <= rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(Operator.GREATER_EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval >= rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedBoolean(lval >= rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedBoolean(lval >= rval)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedBoolean(lval >= rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedBoolean(lval >= rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedBoolean(lval >= rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(Operator.ADD, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedInt(lval + rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval + rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval + rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval + rval).toByte)
            case (ObjectValue(lval:String), rval) => ObjectValue(lval + rval)
            case (lval, ObjectValue(rval:String)) => ObjectValue(lval + rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedFloat((lval + rval))
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedDouble(lval + rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case BinaryExpression(Operator.SUBTRACT, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedInt(lval - rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval - rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval - rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval - rval).toByte)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedFloat((lval - rval))
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedDouble(lval - rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case BinaryExpression(Operator.MULTIPLY, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedInt(lval * rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval * rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval * rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval * rval).toByte)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedFloat((lval * rval))
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedDouble(lval * rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case BinaryExpression(Operator.DIVIDE, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedInt(lval / rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval / rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval / rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval / rval).toByte)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedFloat((lval / rval))
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedDouble(lval / rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case MinusOp(operand) =>
          evalRecursive(operand) match {
            case BoxedInt(value) => BoxedInt(-value)
            case BoxedLong(value) => BoxedLong(-value)
            case BoxedShort(value) => BoxedShort((-value).toShort)
            case BoxedByte(value) => BoxedByte((-value).toByte)
            case BoxedFloat(value) => BoxedFloat(-value)
            case BoxedDouble(value) => BoxedDouble(-value)
            case _ => reportError("- cannot be applied to non-integer value")
          }
        case PlusOp(operand) =>
          evalRecursive(operand) match {
            case BoxedInt(value) => BoxedInt(value)
            case BoxedLong(value) => BoxedLong(value)
            case BoxedShort(value) => BoxedShort(value)
            case BoxedByte(value) => BoxedByte(value)
            case BoxedFloat(value) => BoxedFloat(value)
            case BoxedDouble(value) => BoxedDouble(value)
            case _ => reportError("+ cannot be applied to non-integer value")
          }
        case IntNode(value) =>
          BoxedInt(value)
        case StringNode(value) =>
          ObjectValue(value)
        case LongNode(value) =>
          BoxedLong(value)
        case ShortNode(value) =>
          BoxedShort(value)
        case ByteNode(value) =>
          BoxedByte(value)
        case DoubleNode(value) =>
          BoxedDouble(value)
        case FloatNode(value) =>
          BoxedFloat(value)
        case BooleanNode(value) =>
          BoxedBoolean(value)
        case ListLiteral(elements) =>
          val params = elements.map{e => Value.fromKlassic(evalRecursive(e))}
          val newList = new java.util.ArrayList[Any]
          params.foreach{param =>
            newList.add(param)
          }
          ObjectValue(newList)
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
              val paramsArray = params.map{p => evalRecursive(p)}.toArray
              findMethod(value, name, paramsArray) match {
                case UnboxedVersionMethodFound(method) =>
                  val actualParams = paramsArray.map{Value.fromKlassic}
                  Value.toKlassic(method.invoke(value, actualParams:_*))
                case BoxedVersionMethodFound(method) =>
                  val actualParams = paramsArray.map{Value.fromKlassic}
                  Value.toKlassic(method.invoke(value, actualParams:_*))
                case NoMethodFound =>
                  throw new IllegalArgumentException(s"${self}.${name}(${params})")
              }
            case otherwise =>
              sys.error(s"cannot reach here: ${otherwise}")
          }
        case NewObject(className, params) =>
          val paramsArray = params.map{evalRecursive}.toArray
          findConstructor(Class.forName(className), paramsArray) match {
            case UnboxedVersionConstructorFound(constructor) =>
              val actualParams = paramsArray.map{Value.fromKlassic}
              Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
            case BoxedVersionConstructorFound(constructor) =>
              val actualParams = paramsArray.map{Value.fromKlassic}
              Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
            case NoConstructorFound =>
              throw new IllegalArgumentException(s"newObject(${className}, ${params}")
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
        case e@ForeachExpression(_, _, _) => sys.error(s"cannot reach here: ${e}")
      }
    }
    evalRecursive(rewrite(node))
  }
}
