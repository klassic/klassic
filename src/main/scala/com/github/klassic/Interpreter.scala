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
          evaluator.evaluate(env, FunctionCall(NoLocation, fun.value, Nil))
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
      interpreter.evaluate(env, FunctionCall(NoLocation, fun.value, List()))
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

  def parse(program: String): AstNode = {
    val parser = new Parser
    parser.parse(program) match {
      case parser.Success(node: AstNode, _) => node
      case parser.Failure(m, n) => throw new InterpreterException(n.pos + ":" + m)
      case parser.Error(m, n) => throw new InterpreterException(n.pos + ":" + m)
    }
  }

  def evaluate(node: AstNode): Value = evaluate(BuiltinEnvironment, node)
  def evaluate(env:Environment, node: AstNode): Value = {
    def rewrite(node: AstNode): AstNode = node match {
      case Program(location, imports, block) => sys.error("cannot reach here")
      case Import(_, _, _) => sys.error("cannot reach here")
      case Block(location, expressions) => Block(location, expressions.map{rewrite})
      case IfExpression(location, cond, pos, neg) =>
        IfExpression(location, rewrite(cond), rewrite(pos), rewrite(neg))
      case WhileExpression(location, condition, body: AstNode) =>
        WhileExpression(location, rewrite(condition), rewrite(body))
      case e@ForeachExpression(location, name, collection, body) =>
        val itVariable = symbol()
        val location = e.location
        Block(location, List(
          ValDeclaration(location, itVariable, None, MethodCall(location, rewrite(collection), "iterator", List()), false),
          WhileExpression(
            location,
            BinaryExpression(
              location,
              Operator.EQUAL,
              MethodCall(location, Identifier(location, itVariable), "hasNext", List()),
              BooleanNode(location, true)
            ),
            Block(location, List(
              ValDeclaration(location, name, None, MethodCall(location, Identifier(location, itVariable), "next", List()), false),
              body
            ))
          )
        ))
      case BinaryExpression(location, operator, lhs, rhs) =>
        BinaryExpression(location, operator, rewrite(lhs), rewrite(rhs))
      case MinusOp(location, operand) => MinusOp(location, rewrite(operand))
      case PlusOp(location, operand) => PlusOp(location, rewrite(operand))
      case literal@StringNode(location, value) => literal
      case literal@IntNode(location, value) => literal
      case literal@LongNode(location, value)  => literal
      case literal@ShortNode(location, value) => literal
      case literal@ByteNode(location, value) => literal
      case literal@BooleanNode(location, value) => literal
      case literal@DoubleNode(location, value) => literal
      case literal@FloatNode(lcation, value) => literal
      case node@Identifier(_, name) => node
      case Assignment(location, variable, value) => Assignment(location, variable, rewrite(value))
      case ValDeclaration(location, variable, optionalType, value, immutable) => ValDeclaration(location, variable, optionalType, rewrite(value), immutable)
      case FunctionLiteral(location, params, proc) => FunctionLiteral(location, params, rewrite(proc))
      case FunctionDefinition(location, name, func) => FunctionDefinition(location, name, rewrite(func).asInstanceOf[FunctionLiteral])
      case FunctionCall(location, func, params) => FunctionCall(location, rewrite(func), params.map{rewrite})
      case ListLiteral(location, elements) =>  ListLiteral(location, elements.map{rewrite})
      case MapLiteral(location, elements) => MapLiteral(location, elements.map{ case (k, v) => (rewrite(k), rewrite(v))})
      case NewObject(location, className, params) => NewObject(location, className, params.map{rewrite})
      case MethodCall(location ,self, name, params) => MethodCall(location, rewrite(self), name, params.map{rewrite})
    }
    def evalRecursive(node: AstNode): Value = {
      node match{
        case Block(location, exprs) =>
          val local = new Environment(Some(env))
          exprs.foldLeft(UnitValue:Value){(result, x) => evaluate(local, x)}
        case WhileExpression(location, cond, body) =>
          while(evalRecursive(cond) == BoxedBoolean(true)) {
            evalRecursive(body)
          }
          UnitValue
        case IfExpression(location, condition, pos, neg) =>
          evalRecursive(condition) match {
            case BoxedBoolean(true) => evalRecursive(pos)
            case BoxedBoolean(false) => evalRecursive(neg)
            case _ => reportError("type error")
          }
        case BinaryExpression(location, Operator.AND2, lhs, rhs) =>
          evalRecursive(lhs) match {
            case BoxedBoolean(true) => evalRecursive(rhs)
            case BoxedBoolean(false) => BoxedBoolean(false)
            case _ => reportError("type error")
          }
        case BinaryExpression(location, Operator.BAR2, lhs, rhs) =>
          evalRecursive(lhs) match {
            case BoxedBoolean(false) => evalRecursive(rhs)
            case BoxedBoolean(true) => BoxedBoolean(true)
            case _ => reportError("type error")
          }
        case BinaryExpression(location, Operator.EQUAL, left, right) =>
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
        case BinaryExpression(location, Operator.LESS_THAN, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval < rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedBoolean(lval < rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedBoolean(lval < rval)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedBoolean(lval < rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedBoolean(lval < rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedBoolean(lval < rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(location, Operator.GREATER_THAN, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval > rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedBoolean(lval > rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedBoolean(lval > rval)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedBoolean(lval > rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedBoolean(lval > rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedBoolean(lval > rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(location, Operator.LESS_OR_EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval <= rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedBoolean(lval <= rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedBoolean(lval <= rval)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedBoolean(lval <= rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedBoolean(lval <= rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedBoolean(lval <= rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(location, Operator.GREATER_EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval >= rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedBoolean(lval >= rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedBoolean(lval >= rval)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedBoolean(lval >= rval)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedBoolean(lval >= rval)
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedBoolean(lval >= rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case BinaryExpression(location, Operator.ADD, left, right) =>
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
        case BinaryExpression(location, Operator.SUBTRACT, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedInt(lval - rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval - rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval - rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval - rval).toByte)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedFloat((lval - rval))
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedDouble(lval - rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case BinaryExpression(location, Operator.MULTIPLY, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedInt(lval * rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval * rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval * rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval * rval).toByte)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedFloat((lval * rval))
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedDouble(lval * rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case BinaryExpression(location, Operator.DIVIDE, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedInt(lval / rval)
            case (BoxedLong(lval), BoxedLong(rval)) => BoxedLong(lval / rval)
            case (BoxedShort(lval), BoxedShort(rval)) => BoxedShort((lval / rval).toShort)
            case (BoxedByte(lval), BoxedByte(rval)) => BoxedByte((lval / rval).toByte)
            case (BoxedFloat(lval), BoxedFloat(rval)) => BoxedFloat((lval / rval))
            case (BoxedDouble(lval), BoxedDouble(rval)) => BoxedDouble(lval / rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case MinusOp(location, operand) =>
          evalRecursive(operand) match {
            case BoxedInt(value) => BoxedInt(-value)
            case BoxedLong(value) => BoxedLong(-value)
            case BoxedShort(value) => BoxedShort((-value).toShort)
            case BoxedByte(value) => BoxedByte((-value).toByte)
            case BoxedFloat(value) => BoxedFloat(-value)
            case BoxedDouble(value) => BoxedDouble(-value)
            case _ => reportError("- cannot be applied to non-integer value")
          }
        case PlusOp(location, operand) =>
          evalRecursive(operand) match {
            case BoxedInt(value) => BoxedInt(value)
            case BoxedLong(value) => BoxedLong(value)
            case BoxedShort(value) => BoxedShort(value)
            case BoxedByte(value) => BoxedByte(value)
            case BoxedFloat(value) => BoxedFloat(value)
            case BoxedDouble(value) => BoxedDouble(value)
            case _ => reportError("+ cannot be applied to non-integer value")
          }
        case IntNode(location, value) =>
          BoxedInt(value)
        case StringNode(location, value) =>
          ObjectValue(value)
        case LongNode(location, value) =>
          BoxedLong(value)
        case ShortNode(location, value) =>
          BoxedShort(value)
        case ByteNode(location, value) =>
          BoxedByte(value)
        case DoubleNode(location, value) =>
          BoxedDouble(value)
        case FloatNode(location, value) =>
          BoxedFloat(value)
        case BooleanNode(location, value) =>
          BoxedBoolean(value)
        case ListLiteral(location, elements) =>
          val params = elements.map{e => Value.fromKlassic(evalRecursive(e))}
          val newList = new java.util.ArrayList[Any]
          params.foreach{param =>
            newList.add(param)
          }
          ObjectValue(newList)
        case MapLiteral(location, elements) =>
          val params = elements.map{ case (k, v) =>
            (Value.fromKlassic(evalRecursive(k)), Value.fromKlassic(evalRecursive(v)))
          }
          val newMap = new java.util.HashMap[Any, Any]
          params.foreach{ case (k, v) =>
             newMap.put(k, v)
          }
          ObjectValue(newMap)
        case Identifier(location, name) => env(name)
        case ValDeclaration(location, vr, optVariableType, value, immutable) =>
          env(vr) = evalRecursive(value)
        case Assignment(location, vr, value) =>
          env.set(vr, evalRecursive(value))
        case literal@FunctionLiteral(location, _, _) =>
          FunctionValue(literal, Some(env))
        case FunctionDefinition(location, name, func) =>
          env(name) = FunctionValue(func, Some(env)): Value
        case MethodCall(location, self, name, params) =>
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
        case NewObject(location, className, params) =>
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
        case FunctionCall(location, func, params) =>
          evalRecursive(func) match{
            case FunctionValue(FunctionLiteral(location, fparams, proc), cenv) =>
              val local = new Environment(cenv)
              (fparams zip params).foreach{ case (fp, ap) =>
                local(fp.name) = evalRecursive(ap)
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
        case otherwise@ForeachExpression(location, _, _, _) => sys.error(s"cannot reach here: ${otherwise}")
      }
    }
    evalRecursive(rewrite(node))
  }
}
