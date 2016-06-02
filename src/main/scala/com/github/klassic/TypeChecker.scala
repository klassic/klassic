package com.github.klassic

import com.github.klassic.AstNode._
import com.github.klassic.TypeDescription._

import scala.collection.mutable
/**
  * Created by kota_mizushima on 2016/06/02.
  */
object TypeChecker {
  def expeted(expectedType: TypeDescription, actualType: TypeDescription): Boolean = {
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
  def typeCheck(node: AstNode, environment : TypeEnvironment): TypeDescription = {
    node match {
      case Block(expressions) =>
        var description: TypeDescription = UnitType
        var xs = expressions
        while(!xs.isEmpty) {
          val current = xs.head
          current match {
            case ValDeclaration(variable, optVariableType, value) =>
              if(environment.variables.contains(variable)) {
                throw new InterruptedException(s"variable ${variable} is already defined")
              }
              val valueType = typeCheck(value, environment)
              val declarationType = optVariableType.map { variableType =>
                if(expeted(variableType, valueType))
                  variableType
                else
                  ErrorType
              }.getOrElse {
                valueType
              }
              if(declarationType == ErrorType) {
                throw new InterpreterException(s"expected type: ${declarationType}, but actual type: ${valueType}")
              }
              environment.variables(variable) = declarationType
              description = declarationType
            case otherwise =>
              description = typeCheck(otherwise, environment)
          }
          xs = xs.tail
        }
        description
      case IntNode(_) => IntType
      case ShortNode(_) => ShortType
      case ByteNode(_) => ByteType
      case LongNode(_) => LongType
      case FloatNode(_) => FloatType
      case DoubleNode(_) => DoubleType
      case BooleanNode(_) => BooleanType
      case Assignment(variable, value) =>
        val result = environment.lookup(variable) match {
          case None =>
            throw new InterpreterException(s"variable ${value} is not defined")
          case Some(variableType) =>
            val valueType = typeCheck(value, environment)
            if(!expeted(variableType, valueType)) {
              throw new InterpreterException(s"expected type: ${variableType}, but actual type: ${valueType}")
            }
            UnitType
        }
        result
      case IfExpression(cond: AstNode, pos: AstNode, neg: AstNode) => ???
      case ValDeclaration(variable: String, description: Option[TypeDescription], value: AstNode) => ???
      case ForeachExpression(name: String, collection: AstNode, body: AstNode) => ???
      case BinaryExpression(operator: Operator, lhs: AstNode, rhs: AstNode) => ???
      case WhileExpression(condition: AstNode, body: AstNode) => ???
      case MinusOp(operand: AstNode) => ???
      case PlusOp(operand: AstNode) => ???
      case StringNode(value: String) =>
        DynamicType
      case Identifier(name: String) =>
        environment.lookup(name) match {
          case None => throw InterpreterException(s"variable ${name} is not found")
          case Some(description) => description
        }
      case FunctionLiteral(params: List[FormalParameter], proc: AstNode) => ???
        val paramsMap = mutable.Map(params.map{p => p.name -> p.description}:_*)
        val newEnvironment = TypeEnvironment(paramsMap, Some(environment))
        val paramTypes = params.map{_.description}
        val returnType = typeCheck(proc, newEnvironment)
        FunctionType(paramTypes, returnType)
      case FunctionDefinition(name: String, func: FunctionLiteral) => ???
        environment.variables(name) = typeCheck(func, environment)
        UnitType
      case FunctionCall(func: AstNode, params: List[AstNode]) =>
        val funcType: FunctionType = typeCheck(func, environment) match {
          case f@FunctionType(_, _) => f
          case otherwise => throw InterpreterException(s"expected: function type, actual type: ${otherwise}")
        }
        val actualParamTypes = params.map(p => typeCheck(p, environment))
        if(funcType.paramTypes.length != actualParamTypes.length) {
          throw InterpreterException(s"expected length: ${funcType.paramTypes.length}, actual length: ${actualParamTypes.length}")
        }
        funcType.paramTypes.zip(actualParamTypes).foreach { case (expectedType, actualType) =>
            expeted(expectedType, actualType)
        }
        funcType.returnType
      case ListLiteral(elements: List[AstNode]) =>
        elements.foreach(e => typeCheck(e, environment))
        DynamicType
      case NewObject(className: String, params: List[AstNode]) =>
        params.foreach(p => typeCheck(p, environment))
        DynamicType
      case MethodCall(self: AstNode, name: String, params: List[AstNode]) =>
        typeCheck(self, environment)
        params.foreach(p => typeCheck(p, environment))
        DynamicType
      case otherwise =>
        throw InterpreterPanic(otherwise.toString)
    }
  }
}
