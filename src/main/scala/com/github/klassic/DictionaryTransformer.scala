package com.github.klassic

import com.github.klassic.TypedAst._
import com.github.klassic.Type._

/**
 * Transforms type class method calls into dictionary-based calls
 */
class DictionaryTransformer {
  
  def transform(program: Program): Program = {
    val transformedBlock = transformBlock(program.block, program.typeClasses, program.instances)
    program.copy(block = transformedBlock)
  }
  
  private def transformBlock(block: Block, typeClasses: Map[String, TTypeClass], instances: Map[(String, Type), TInstance]): Block = {
    val transformedExprs = block.expressions.map(expr => transformNode(expr, typeClasses, instances))
    block.copy(expressions = transformedExprs)
  }
  
  private def transformNode(node: TypedNode, typeClasses: Map[String, TTypeClass], instances: Map[(String, Type), TInstance]): TypedNode = node match {
    case Block(type_, location, expressions) =>
      Block(type_, location, expressions.map(e => transformNode(e, typeClasses, instances)))
      
    case IfExpression(type_, location, condition, thenExpr, elseExpr) =>
      IfExpression(type_, location, 
        transformNode(condition, typeClasses, instances),
        transformNode(thenExpr, typeClasses, instances),
        transformNode(elseExpr, typeClasses, instances)
      )
      
    case FunctionCall(type_, location, func, params) =>
      // Check if the function is a type class method
      func match {
        case Id(_, _, name) if isTypeClassMethod(name, typeClasses) =>
          // Transform to instance method call
          transformTypeClassCall(type_, location, name, params, typeClasses, instances)
        case _ =>
          FunctionCall(type_, location, 
            transformNode(func, typeClasses, instances),
            params.map(p => transformNode(p, typeClasses, instances))
          )
      }
      
    case LetDeclaration(type_, location, variable, declaredType, value, body, immutable) =>
      LetDeclaration(type_, location, variable, declaredType,
        transformNode(value, typeClasses, instances),
        transformNode(body, typeClasses, instances),
        immutable
      )
      
    case LetFunctionDefinition(type_, location, name, body, cleanup, expression) =>
      LetFunctionDefinition(type_, location, name,
        transformFunctionLiteral(body, typeClasses, instances),
        cleanup.map(c => transformNode(c, typeClasses, instances)),
        transformNode(expression, typeClasses, instances)
      )
      
    case FunctionLiteral(type_, location, params, optionalType, proc) =>
      transformFunctionLiteral(FunctionLiteral(type_, location, params, optionalType, proc), typeClasses, instances)
      
    case other => other
  }
  
  private def transformFunctionLiteral(func: FunctionLiteral, typeClasses: Map[String, TTypeClass], instances: Map[(String, Type), TInstance]): FunctionLiteral = {
    func.copy(proc = transformNode(func.proc, typeClasses, instances))
  }
  
  private def isTypeClassMethod(name: String, typeClasses: Map[String, TTypeClass]): Boolean = {
    typeClasses.values.exists(tc => tc.methods.exists(_._1 == name))
  }
  
  private def transformTypeClassCall(
    type_ : Type,
    location: Location,
    methodName: String,
    params: List[TypedNode],
    typeClasses: Map[String, TTypeClass],
    instances: Map[(String, Type), TInstance]
  ): TypedNode = {
    // Find which type class this method belongs to
    val typeClass = typeClasses.values.find(tc => tc.methods.exists(_._1 == methodName)).get
    
    // Get the type of the first parameter (assuming single-parameter type classes for now)
    if (params.isEmpty) {
      throw new RuntimeException(s"Type class method $methodName called with no arguments")
    }
    
    val firstParamType = params.head.type_
    
    // Find the matching instance
    instances.find { case ((className, instType), _) =>
      className == typeClass.name && typesMatch(instType, firstParamType)
    } match {
      case Some(((_, _), instance)) =>
        // Generate a call to the instance method
        val instanceName = s"${typeClass.name}_${normalizeTypeName(firstParamType)}_instance"
        val instanceAccess = Id(TDynamic, location, instanceName)
        val methodAccess = RecordSelect(type_, location, instanceAccess, methodName)
        FunctionCall(type_, location, methodAccess, params.map(p => transformNode(p, typeClasses, instances)))
        
      case None =>
        throw new RuntimeException(s"No instance found for ${typeClass.name}[${firstParamType}]")
    }
  }
  
  private def typesMatch(instType: Type, paramType: Type): Boolean = {
    (instType, paramType) match {
      case (TInt, TInt) => true
      case (TString, TString) => true
      case (TBoolean, TBoolean) => true
      case (TConstructor(n1, args1), TConstructor(n2, args2)) if n1 == n2 =>
        args1.zip(args2).forall { case (t1, t2) => typesMatch(t1, t2) }
      case _ => false
    }
  }
  
  private def normalizeTypeName(type_ : Type): String = type_ match {
    case TInt => "Int"
    case TString => "String"
    case TBoolean => "Boolean"
    case TConstructor(name, _) => name
    case _ => "Unknown"
  }
}