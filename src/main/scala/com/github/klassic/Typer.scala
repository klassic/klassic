package com.github.klassic

import com.github.klassic.TypeDescription._

import scala.collection.mutable

/**
  * Created by kota_mizushima on 2016/06/02.
  */
class Typer {
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
  def doType(node: AST): TypedAST = {
    typeCheck(node, TypeEnvironment(mutable.Map.empty, mutable.Set.empty, None))
  }
  def typeCheck(node: AST, environment : TypeEnvironment): TypedAST = {
    node match {
      case AST.Block(location, expressions) =>
        expressions match {
          case Nil => TypedAST.Block(UnitType, location, Nil)
          case x::xs =>
            val typedX = typeCheck(x, environment)
            val reversedTypedElements = xs.foldLeft(typedX::Nil){(a, e) => typeCheck(e, environment)::a}
            TypedAST.Block(reversedTypedElements.head.description, location, reversedTypedElements.reverse)
        }
      case AST.IntNode(location, value) => TypedAST.IntNode(IntType, location, value)
      case AST.ShortNode(location, value) => TypedAST.ShortNode(ShortType, location, value)
      case AST.ByteNode(location, value) => TypedAST.ByteNode(ByteType, location, value)
      case AST.LongNode(location, value) => TypedAST.LongNode(LongType, location, value)
      case AST.FloatNode(location, value) => TypedAST.FloatNode(FloatType, location, value)
      case AST.DoubleNode(location, value) => TypedAST.DoubleNode(DoubleType, location, value)
      case AST.BooleanNode(location, value) => TypedAST.BooleanNode(BooleanType, location, value)
      case AST.Assignment(location, variable, value) =>
        if(environment.immutableVariables.contains(variable)) {
          throw InterpreterException(s"variable '${variable}' cannot change")
        }
        val result = environment.lookup(variable) match {
          case None =>
            throw new InterpreterException(s"variable ${value} is not defined")
          case Some(variableType) =>
            val typedValue = typeCheck(value, environment)
            if(!isAssignableFrom(variableType, typedValue.description)) {
              throw new InterpreterException(s"expected type: ${variableType}, actual type: ${typedValue.description}")
            }
            TypedAST.Assignment(variableType, location, variable, typedValue)
        }
        result
      case AST.IfExpression(location, cond, pos, neg) =>
        val typedCondition = typeCheck(cond, environment)
        if(typedCondition.description != BooleanType) {
          throw InterpreterException(s"condition type must be Boolean, actual: ${typedCondition.description}")
        } else {
          val posTyped = typeCheck(pos, environment)
          val negTyped = typeCheck(neg, environment)
          if(!isAssignableFrom(posTyped.description, negTyped.description)) {
            throw new InterpreterException(s"type ${posTyped.description} and type ${negTyped.description} is incomparable")
          }
          if(posTyped.description == DynamicType)
            TypedAST.IfExpression(DynamicType, location, typedCondition, posTyped, negTyped)
          else if(negTyped.description == DynamicType)
            TypedAST.IfExpression(DynamicType, location, typedCondition, posTyped, negTyped)
          else
            TypedAST.IfExpression(posTyped.description, location, typedCondition, posTyped, negTyped)
        }
      case AST.ValDeclaration(location, variable, optVariableType, value, immutable) =>
        if(environment.variables.contains(variable)) {
          throw InterpreterException(s"${location.format} variable ${variable} is already defined")
        }
        val typedValue = typeCheck(value, environment)
        val declaredType = optVariableType match {
          case Some(variableType) =>
            if(isAssignableFrom(variableType, typedValue.description)) {
              environment.variables(variable) = variableType
              variableType
            } else {
              throw new InterpreterException(s"${location.format} expected type: ${variableType}, but actual type: ${typedValue.description}")
            }
          case None =>
            environment.variables(variable) = typedValue.description
            if(immutable) {
              environment.immutableVariables += variable
            }
            typedValue.description
        }
        TypedAST.ValDeclaration(declaredType, location, variable, optVariableType, typedValue, immutable)
      case AST.ForeachExpression(location, variable, collection, body) =>
        val typedCollection = typeCheck(collection, environment)
        if(!isAssignableFrom(typedCollection.description, DynamicType)) {
          throw InterpreterException(s"${location.format} expression should be DynamicType")
        }
        if(environment.variables.contains(variable)) {
          throw InterpreterException(s"${location.format} variable ${variable} is already defined")
        }
        environment.variables(variable) = DynamicType
        val typedBody = typeCheck(body, environment)
        TypedAST.ForeachExpression(UnitType, location, variable, typedCollection, typedBody)
      case AST.WhileExpression(location, condition, body) =>
        val typedCondition = typeCheck(condition, environment)
        if(typedCondition.description != BooleanType) {
          throw InterpreterException(s"${location.format} condition type must be Boolean, actual: ${typedCondition.description}")
        } else {
          val typedBody = typeCheck(body, environment)
          TypedAST.WhileExpression(UnitType, location, typedCondition, typedBody)
        }
      case AST.BinaryExpression(location, Operator.EQUAL, left, right) =>
        val typedLhs = typeCheck(left, environment)
        val typedRhs = typeCheck(right, environment)
        if(isAssignableFrom(typedLhs.description, typedRhs.description)) {
          TypedAST.BinaryExpression(BooleanType, location, Operator.EQUAL, typedLhs, typedRhs)
        } else {
          throw InterpreterException(s"${location.format} expected type: ${typedLhs.description}, actual type: ${typedRhs.description}")
        }
      case AST.BinaryExpression(location, Operator.LESS_THAN, left, right) =>
        val typedLhs = typeCheck(left, environment)
        val typedRhs = typeCheck(right, environment)
        val resultType = (typedLhs.description, typedRhs.description) match{
          case (IntType, IntType) => BooleanType
          case (LongType, LongType) => BooleanType
          case (ShortType, ShortType) => BooleanType
          case (ByteType, ByteType) => BooleanType
          case (FloatType, FloatType) => BooleanType
          case (DoubleType, DoubleType) => BooleanType
          case (lType, DynamicType) => BooleanType
          case (DynamicType, rtype) => BooleanType
          case _ => throw InterpreterException(s"${location.format} comparison operation must be done between the same numeric types")
        }
        TypedAST.BinaryExpression(resultType, location, Operator.LESS_THAN, typedLhs, typedRhs)
      case AST.BinaryExpression(location, Operator.GREATER_THAN, left, right) =>
        val typedLhs = typeCheck(left, environment)
        val typedRhs = typeCheck(right, environment)
        val resultType = (typedLhs.description, typedRhs.description) match{
          case (IntType, IntType) => BooleanType
          case (LongType, LongType) => BooleanType
          case (ShortType, ShortType) => BooleanType
          case (ByteType, ByteType) => BooleanType
          case (FloatType, FloatType) => BooleanType
          case (DoubleType, DoubleType) => BooleanType
          case (lType, DynamicType) => BooleanType
          case (DynamicType, rtype) => BooleanType
          case _ => throw InterpreterException(s"${location.format} comparison operation must be done between the same numeric types")
        }
        TypedAST.BinaryExpression(resultType, location, Operator.GREATER_THAN, typedLhs, typedRhs)
      case AST.BinaryExpression(location, Operator.LESS_OR_EQUAL, left, right) =>
        val typedLhs = typeCheck(left, environment)
        val typedRhs = typeCheck(right, environment)
        val resultType = (typedLhs.description, typedRhs.description) match{
          case (IntType, IntType) => BooleanType
          case (LongType, LongType) => BooleanType
          case (ShortType, ShortType) => BooleanType
          case (ByteType, ByteType) => BooleanType
          case (FloatType, FloatType) => BooleanType
          case (DoubleType, DoubleType) => BooleanType
          case (lType, DynamicType) => BooleanType
          case (DynamicType, rtype) => BooleanType
          case _ => throw InterpreterException(s"${location.format} comparison operation must be done between the same numeric types")
        }
        TypedAST.BinaryExpression(resultType, location, Operator.LESS_OR_EQUAL, typedLhs, typedRhs)
      case AST.BinaryExpression(location, Operator.GREATER_EQUAL, left, right) =>
        val typedLhs = typeCheck(left, environment)
        val typedRhs = typeCheck(right, environment)
        val resultType = (typedLhs.description, typedRhs.description) match{
          case (IntType, IntType) => BooleanType
          case (LongType, LongType) => BooleanType
          case (ShortType, ShortType) => BooleanType
          case (ByteType, ByteType) => BooleanType
          case (FloatType, FloatType) => BooleanType
          case (DoubleType, DoubleType) => BooleanType
          case (lType, DynamicType) => BooleanType
          case (DynamicType, rtype) => BooleanType
          case _ => throw InterpreterException(s"${location.format} comparison operation must be done between the same numeric types")
        }
        TypedAST.BinaryExpression(resultType, location, Operator.GREATER_EQUAL, typedLhs, typedRhs)
      case AST.BinaryExpression(location, Operator.ADD, left, right) =>
        val typedLhs = typeCheck(left, environment)
        val typedRhs = typeCheck(right, environment)
        val resultType = (typedLhs.description, typedRhs.description) match{
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
        TypedAST.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs)
      case AST.BinaryExpression(location, Operator.SUBTRACT, left, right) =>
        val typedLhs = typeCheck(left, environment)
        val typedRhs = typeCheck(right, environment)
        val resultType = (typedLhs.description, typedRhs.description) match{
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
        TypedAST.BinaryExpression(resultType, location, Operator.SUBTRACT, typedLhs, typedRhs)
      case AST.BinaryExpression(location, Operator.MULTIPLY, left, right) =>
        val typedLhs = typeCheck(left, environment)
        val typedRhs = typeCheck(right, environment)
        val resultType = (typedLhs.description, typedRhs.description) match{
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
        TypedAST.BinaryExpression(resultType, location, Operator.MULTIPLY, typedLhs, typedRhs)
      case AST.BinaryExpression(location, Operator.DIVIDE, left, right) =>
        val typedLhs = typeCheck(left, environment)
        val typedRhs = typeCheck(right, environment)
        val resultType = (typedLhs.description, typedRhs.description) match{
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
        TypedAST.BinaryExpression(resultType, location, Operator.DIVIDE, typedLhs, typedRhs)
      case AST.MinusOp(location, operand) =>
        val typedOperand = typeCheck(operand, environment)
        val resultType = typedOperand.description match {
          case ByteType => ByteType
          case IntType => IntType
          case ShortType => ShortType
          case LongType => LongType
          case FloatType => FloatType
          case DoubleType => DoubleType
          case DynamicType => DynamicType
          case otherwise => throw InterpreterException(s"${location.format} expected: Numeric type, actual: ${otherwise}")
        }
        TypedAST.MinusOp(resultType, location, typedOperand)
      case AST.PlusOp(location, operand) =>
        val typedOperand = typeCheck(operand, environment)
        val resultType = typedOperand.description match {
          case ByteType => ByteType
          case IntType => IntType
          case ShortType => ShortType
          case LongType => LongType
          case FloatType => FloatType
          case DoubleType => DoubleType
          case DynamicType => DynamicType
          case otherwise => throw InterpreterException(s"${location.format} expected: Numeric type, actual: ${otherwise}")
        }
        TypedAST.PlusOp(resultType, location, typedOperand)
      case AST.StringNode(location, value) =>
        TypedAST.StringNode(DynamicType, location, value)
      case AST.Identifier(location, name) =>
        val resultType = environment.lookup(name) match {
          case None => throw InterpreterException(s"${location.format} variable '${name}' is not found")
          case Some(description) => description
        }
        TypedAST.Identifier(resultType, location, name)
      case AST.FunctionLiteral(location, params, proc) =>
        val paramsMap = mutable.Map(params.map{p => p.name -> p.description}:_*)
        val paramsSet = mutable.Set(params.map{_.name}:_*)
        val newEnvironment = TypeEnvironment(paramsMap, paramsSet, Some(environment))
        val paramTypes = params.map{_.description}
        val typedProc = typeCheck(proc, newEnvironment)
        TypedAST.FunctionLiteral(FunctionType(paramTypes, typedProc.description), location, params, typedProc)
      case AST.FunctionDefinition(location, name, body, cleanup) =>
        if(environment.variables.contains(name)) {
          throw new InterruptedException(s"${location.format} function ${name} is already defined")
        }
        val typedBody = typeCheck(body, environment)
        val typedCleanup = cleanup.map{c => typeCheck(c, environment)}
        TypedAST.FunctionDefinition(typedBody.description, location, name, typedBody.asInstanceOf[TypedAST.FunctionLiteral], typedCleanup)
      case AST.FunctionCall(location, target, params) =>
        val typedTarget = typeCheck(target, environment)
        val functionType: FunctionType = typedTarget.description match {
          case f@FunctionType(_, _) => f
          case otherwise => throw InterpreterException(s"${location.format} expected: function type, actual type: ${otherwise}")
        }
        val actualTypedParams = params.map(p => typeCheck(p, environment))
        val actualParamTypes = actualTypedParams.map(_.description)
        if(functionType.paramTypes.length != actualParamTypes.length) {
          throw InterpreterException(s"${location.format} function arity mismatch: expected length: ${functionType.paramTypes.length}, actual length: ${actualParamTypes.length}")
        }
        functionType.paramTypes.zip(actualParamTypes).foreach { case (expectedType, actualType) =>
            if(!isAssignableFrom(expectedType, actualType)){
              throw InterpreterException(s"${location.format} expected type: ${expectedType}, actual type:${actualType}")
            }
        }
        TypedAST.FunctionCall(functionType.returnType, location, typedTarget, actualTypedParams)
      case AST.ListLiteral(location, elements) =>
        val typedElements = elements.map(e => typeCheck(e, environment))
        TypedAST.ListLiteral(DynamicType, location, typedElements)
      case AST.MapLiteral(location, elements) =>
        val typedElements = elements.map{ case (k, v) =>
          (typeCheck(k, environment), typeCheck(v, environment))
        }
        TypedAST.MapLiteral(DynamicType, location, typedElements)
      case AST.NewObject(location, className, params) =>
        val typedParams = params.map(p => typeCheck(p, environment))
        TypedAST.NewObject(DynamicType, location, className, typedParams)
      case AST.MethodCall(location, receiver, name, params) =>
        val typedReceiver = typeCheck(receiver, environment)
        if(typedReceiver.description != DynamicType) {
          throw InterpreterException(s"${location.format} expected: [*], actual: ${typedReceiver.description}")
        }
        val typedParams = params.map(p => typeCheck(p, environment))
        TypedAST.MethodCall(DynamicType, location, typedReceiver, name, typedParams)
      case otherwise =>
        throw InterpreterPanic(otherwise.toString)
    }
  }
}
