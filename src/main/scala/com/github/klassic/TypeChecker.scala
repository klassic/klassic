package com.github.klassic

import com.github.klassic.AST._
import com.github.klassic.TypeDescription._

import scala.collection.mutable
/**
  * Created by kota_mizushima on 2016/06/02.
  */
class TypeChecker {
  def isAssignableFrom(expectedType: TypeDescription, actualType: TypeDescription): Boolean = {
    if(expectedType == ErrorType || actualType == ErrorType) {
      false
    } else if(expectedType == DynamicType) {
      true
    } else if(actualType == DynamicType) {
      true
    } else {
      expectedType == actualType
    }
  }
  def doType(node: AST): TypeDescription = {
    typeCheck(node, TypeEnvironment(mutable.Map.empty, mutable.Set.empty, None))
  }
  def typeCheck(node: AST, environment : TypeEnvironment): TypeDescription = {
    node match {
      case Block(location, expressions) =>
        expressions match {
          case Nil => UnitType
          case x::xs =>
            val xType = typeCheck(x, environment)
            xs.foldLeft(xType){(b, e) => typeCheck(e, environment)}
        }
      case IntNode(_, _) => IntType
      case ShortNode(_, _) => ShortType
      case ByteNode(_, _) => ByteType
      case LongNode(_, _) => LongType
      case FloatNode(_, _) => FloatType
      case DoubleNode(_, _) => DoubleType
      case BooleanNode(_, _) => BooleanType
      case Assignment(location, variable, value) =>
        if(environment.immutableVariables.contains(variable)) {
          throw InterpreterException(s"variable '${variable}' cannot change")
        }
        val result = environment.lookup(variable) match {
          case None =>
            throw new InterpreterException(s"variable ${value} is not defined")
          case Some(variableType) =>
            val valueType = typeCheck(value, environment)
            if(!isAssignableFrom(variableType, valueType)) {
              throw new InterpreterException(s"expected type: ${variableType}, actual type: ${valueType}")
            }
            UnitType
        }
        result
      case IfExpression(location, cond, pos, neg) =>
        val condType = typeCheck(cond, environment)
        if(condType != BooleanType) {
          throw InterpreterException(s"condition type must be Boolean, actual: ${condType}")
        } else {
          val posType = typeCheck(pos, environment)
          val negType = typeCheck(neg, environment)
          if(!isAssignableFrom(posType, negType)) {
            throw new InterpreterException(s"type ${posType} and type ${negType} is incomparable")
          }
          if(posType == DynamicType)
            DynamicType
          else if(negType == DynamicType)
            DynamicType
          else
            posType
        }
      case ValDeclaration(location, variable, optVariableType, value, immutable) =>
        if(environment.variables.contains(variable)) {
          throw InterpreterException(s"${location.format} variable ${variable} is already defined")
        }
        val valueType = typeCheck(value, environment)
        optVariableType match {
          case Some(variableType) =>
            if(isAssignableFrom(variableType, valueType)) {
              environment.variables(variable) = variableType
            } else {
              throw new InterpreterException(s"${location.format} expected type: ${variableType}, but actual type: ${valueType}")
            }
          case None =>
            environment.variables(variable) = valueType
            if(immutable) {
              environment.immutableVariables += variable
            }
        }
        UnitType
      case ForeachExpression(location, variable, collection, body) =>
        val collectionType = typeCheck(collection, environment)
        if(!isAssignableFrom(collectionType, DynamicType)) {
          throw InterpreterException(s"${location.format} expression should be DynamicType")
        }
        if(environment.variables.contains(variable)) {
          throw InterpreterException(s"${location.format} variable ${variable} is already defined")
        }
        environment.variables(variable) = DynamicType
        typeCheck(body, environment)
        UnitType
      case WhileExpression(location, condition, body) =>
        val conditionType = typeCheck(condition, environment)
        if(conditionType != BooleanType) {
          throw InterpreterException(s"${location.format} condition type must be Boolean, actual: ${conditionType}")
        } else {
          typeCheck(body, environment)
          UnitType
        }
      case BinaryExpression(location, Operator.EQUAL, left, right) =>
        val lType = typeCheck(left, environment)
        val rType = typeCheck(right, environment)
        if(isAssignableFrom(lType, rType)) {
          BooleanType
        } else {
          throw InterpreterException(s"${location.format} expected type: ${lType}, actual type: ${rType}")
        }
      case BinaryExpression(location, Operator.LESS_THAN, left, right) =>
        (typeCheck(left, environment), typeCheck(right, environment)) match{
          case (IntType, IntType) => IntType
          case (LongType, LongType) => LongType
          case (ShortType, ShortType) => ShortType
          case (ByteType, ByteType) => ByteType
          case (FloatType, FloatType) => FloatType
          case (DoubleType, DoubleType) => DoubleType
          case (lType, DynamicType) => lType
          case (DynamicType, rtype) => rtype
          case _ => throw InterpreterException(s"${location.format} comparison operation must be done between the same numeric types")
        }
      case BinaryExpression(location, Operator.GREATER_THAN, left, right) =>
        (typeCheck(left, environment), typeCheck(right, environment)) match{
          case (IntType, IntType) => IntType
          case (LongType, LongType) => LongType
          case (ShortType, ShortType) => ShortType
          case (ByteType, ByteType) => ByteType
          case (FloatType, FloatType) => FloatType
          case (DoubleType, DoubleType) => DoubleType
          case (lType, DynamicType) => lType
          case (DynamicType, rtype) => rtype
          case _ => throw InterpreterException(s"${location.format} comparison operation must be done between the same numeric types")
        }
      case BinaryExpression(location, Operator.LESS_OR_EQUAL, left, right) =>
        (typeCheck(left, environment), typeCheck(right, environment)) match{
          case (IntType, IntType) => IntType
          case (LongType, LongType) => LongType
          case (ShortType, ShortType) => ShortType
          case (ByteType, ByteType) => ByteType
          case (FloatType, FloatType) => FloatType
          case (DoubleType, DoubleType) => DoubleType
          case (lType, DynamicType) => lType
          case (DynamicType, rtype) => rtype
          case _ => throw InterpreterException(s"${location.format} comparison operation must be done between the same numeric types")
        }
      case BinaryExpression(location, Operator.GREATER_EQUAL, left, right) =>
        (typeCheck(left, environment), typeCheck(right, environment)) match{
          case (IntType, IntType) => IntType
          case (LongType, LongType) => LongType
          case (ShortType, ShortType) => ShortType
          case (ByteType, ByteType) => ByteType
          case (FloatType, FloatType) => FloatType
          case (DoubleType, DoubleType) => DoubleType
          case (lType, DynamicType) => lType
          case (DynamicType, rtype) => rtype
          case _ => throw InterpreterException(s"${location.format} comparison operation must be done between the same numeric types")
        }
      case BinaryExpression(location, Operator.ADD, left, right) =>
        (typeCheck(left, environment), typeCheck(right, environment)) match{
          case (IntType, IntType) => IntType
          case (LongType, LongType) => LongType
          case (ShortType, ShortType) => ShortType
          case (ByteType, ByteType) => ByteType
          case (FloatType, FloatType) => FloatType
          case (DoubleType, DoubleType) => DoubleType
          case (lType, DynamicType) => lType
          case (DynamicType, rtype) => rtype
          case _ => throw InterpreterException(s"${location.format} arithmetic operation must be done between the same numeric types")
        }
      case BinaryExpression(location, Operator.SUBTRACT, left, right) =>
        (typeCheck(left, environment), typeCheck(right, environment)) match{
          case (IntType, IntType) => IntType
          case (LongType, LongType) => LongType
          case (ShortType, ShortType) => ShortType
          case (ByteType, ByteType) => ByteType
          case (FloatType, FloatType) => FloatType
          case (DoubleType, DoubleType) => DoubleType
          case (lType, DynamicType) => lType
          case (DynamicType, rtype) => rtype
          case _ => throw InterpreterException(s"${location.format} arithmetic operation must be done between the same numeric types")
        }
      case BinaryExpression(location, Operator.MULTIPLY, left, right) =>
        (typeCheck(left, environment), typeCheck(right, environment)) match{
          case (IntType, IntType) => IntType
          case (LongType, LongType) => LongType
          case (ShortType, ShortType) => ShortType
          case (ByteType, ByteType) => ByteType
          case (FloatType, FloatType) => FloatType
          case (DoubleType, DoubleType) => DoubleType
          case (lType, DynamicType) => lType
          case (DynamicType, rtype) => rtype
          case _ => throw InterpreterException(s"${location.format} arithmetic operation must be done between the same numeric types")
        }
      case BinaryExpression(location, Operator.DIVIDE, left, right) =>
        (typeCheck(left, environment), typeCheck(right, environment)) match{
          case (IntType, IntType) => IntType
          case (LongType, LongType) => LongType
          case (ShortType, ShortType) => ShortType
          case (ByteType, ByteType) => ByteType
          case (FloatType, FloatType) => FloatType
          case (DoubleType, DoubleType) => DoubleType
          case (lType, DynamicType) => lType
          case (DynamicType, rtype) => rtype
          case _ => throw InterpreterException(s"${location.format} arithmetic operation must be done between the same numeric types")
        }
      case MinusOp(location, operand) =>
        typeCheck(operand, environment) match {
          case ByteType => ByteType
          case IntType => IntType
          case ShortType => ShortType
          case LongType => LongType
          case FloatType => FloatType
          case DoubleType => DoubleType
          case DynamicType => DynamicType
          case otherwise => throw InterpreterException(s"${location.format} expected: Numeric type, actual: ${otherwise}")
        }
      case PlusOp(location, operand) =>
        typeCheck(operand, environment) match {
          case ByteType => ByteType
          case IntType => IntType
          case ShortType => ShortType
          case LongType => LongType
          case FloatType => FloatType
          case DoubleType => DoubleType
          case DynamicType => DynamicType
          case otherwise => throw InterpreterException(s"${location.format} expected: Numeric type, actual: ${otherwise}")
        }
      case StringNode(location, value) =>
        DynamicType
      case Identifier(location, name) =>
        environment.lookup(name) match {
          case None => throw InterpreterException(s"${location.format} variable '${name}' is not found")
          case Some(description) => description
        }
      case FunctionLiteral(location, params, proc) =>
        val paramsMap = mutable.Map(params.map{p => p.name -> p.description}:_*)
        val paramsSet = mutable.Set(params.map{_.name}:_*)
        val newEnvironment = TypeEnvironment(paramsMap, paramsSet, Some(environment))
        val paramTypes = params.map{_.description}
        val returnType = typeCheck(proc, newEnvironment)
        FunctionType(paramTypes, returnType)
      case FunctionDefinition(location, name, func, cleanup) =>
        if(environment.variables.contains(name)) {
          throw new InterruptedException(s"${location.format} function ${name} is already defined")
        }
        environment.variables(name) = typeCheck(func, environment)
        UnitType
      case FunctionCall(location, func, params) =>
        val funcType: FunctionType = typeCheck(func, environment) match {
          case f@FunctionType(_, _) => f
          case otherwise => throw InterpreterException(s"${location.format} expected: function type, actual type: ${otherwise}")
        }
        val actualParamTypes = params.map(p => typeCheck(p, environment))
        if(funcType.paramTypes.length != actualParamTypes.length) {
          throw InterpreterException(s"${location.format} function arity mismatch: expected length: ${funcType.paramTypes.length}, actual length: ${actualParamTypes.length}")
        }
        funcType.paramTypes.zip(actualParamTypes).foreach { case (expectedType, actualType) =>
            if(!isAssignableFrom(expectedType, actualType)){
              throw InterpreterException(s"${location.format} expected type: ${expectedType}, actual type:${actualType}")
            }
        }
        funcType.returnType
      case ListLiteral(location, elements) =>
        elements.foreach(e => typeCheck(e, environment))
        DynamicType
      case MapLiteral(location, elements) =>
        elements.foreach { case (k, v) =>
            typeCheck(k, environment)
            typeCheck(v, environment)
        }
        DynamicType
      case NewObject(location, className, params) =>
        params.foreach(p => typeCheck(p, environment))
        DynamicType
      case MethodCall(location, receiver, name, params) =>
        val receiverType = typeCheck(receiver, environment)
        if(receiverType != DynamicType) {
          throw InterpreterException(s"${location.format} expected: [*], actual: ${receiverType}")
        }
        params.foreach(p => typeCheck(p, environment))
        DynamicType
      case otherwise =>
        throw InterpreterPanic(otherwise.toString)
    }
  }
}
