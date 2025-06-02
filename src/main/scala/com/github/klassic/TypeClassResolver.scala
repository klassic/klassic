package com.github.klassic

import com.github.klassic.Type._
import com.github.klassic.TypedAst._

/**
 * Resolves type class constraints and transforms programs to use dictionary passing
 */
class TypeClassResolver {
  
  // Transform instance declarations into dictionary definitions
  def transformInstances(instances: List[Ast.InstanceDeclaration], env: TypeEnvironment): List[TypedNode] = {
    instances.map { inst =>
      val dictName = s"${inst.className}_${normalizeType(inst.forType)}_dict"
      val dictType = TRecord(Nil, buildRecordType(inst.methods))
      
      // Create a record with all the instance methods
      val methodBindings = inst.methods.map { method =>
        val methodName = method.name
        val methodValue = transformMethodToFunction(method)
        (methodName, methodValue)
      }
      
      // Create a record literal for the dictionary
      val dictValue = createRecordLiteral(dictType, inst.location, methodBindings)
      
      // Create a let binding for the dictionary
      LetDeclaration(
        TUnit,
        inst.location,
        dictName,
        dictType,
        dictValue,
        UnitNode(TUnit, inst.location),
        true
      )
    }
  }
  
  private def normalizeType(t: Type): String = t match {
    case TInt => "Int"
    case TString => "String"
    case TBoolean => "Boolean"
    case TFloat => "Float"
    case TDouble => "Double"
    case TConstructor(name, _, _) => name
    case _ => "Unknown"
  }
  
  private def buildRecordType(methods: List[Ast.MethodDefinition]): Type = {
    methods.foldRight[Type](TRowEmpty) { case (method, row) =>
      val methodType = inferMethodType(method)
      TRowExtend(method.name, methodType, row)
    }
  }
  
  private def inferMethodType(method: Ast.MethodDefinition): Type = {
    // For now, return TDynamic - in a real implementation, we'd infer the type
    TDynamic
  }
  
  private def transformMethodToFunction(method: Ast.MethodDefinition): TypedNode = {
    // Transform the method definition into a function value
    FunctionLiteral(
      TDynamic,
      method.location,
      method.body.params,
      method.body.optionalType,
      transformAstToTyped(method.body.body)
    )
  }
  
  private def transformAstToTyped(node: Ast.Node): TypedNode = {
    // Simple transformation - in a real implementation, this would do proper type checking
    node match {
      case Ast.IntNode(loc, value) => IntNode(TInt, loc, value)
      case Ast.StringNode(loc, value) => StringNode(TString, loc, value)
      case Ast.BooleanNode(loc, value) => BooleanNode(TBoolean, loc, value)
      case Ast.Id(loc, name) => Id(TDynamic, loc, name)
      case Ast.BinaryExpression(loc, op, lhs, rhs) =>
        BinaryExpression(TDynamic, loc, op, transformAstToTyped(lhs), transformAstToTyped(rhs))
      case Ast.FunctionCall(loc, func, params) =>
        FunctionCall(TDynamic, loc, transformAstToTyped(func), params.map(transformAstToTyped))
      case Ast.Block(loc, exprs) =>
        Block(TDynamic, loc, exprs.map(transformAstToTyped))
      case _ => UnitNode(TUnit, NoLocation)
    }
  }
  
  private def createRecordLiteral(recordType: Type, location: Location, fields: List[(String, TypedNode)]): TypedNode = {
    // Create a record literal - for now, we'll use a simplified representation
    // In a real implementation, this would create proper record construction
    fields.headOption match {
      case Some((_, value)) => value
      case None => UnitNode(TUnit, location)
    }
  }
}