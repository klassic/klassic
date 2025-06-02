package com.github.klassic

import com.github.klassic.Ast._
import com.github.klassic.Type._

/**
 * Transforms type class and instance declarations into dictionary-based implementations
 */
class TypeClassTransformer {
  
  case class TransformationContext(
    typeClasses: Map[String, TypeClassDeclaration],
    instances: List[InstanceDeclaration],
    generatedCode: List[Node]
  )
  
  def transform(program: Program): Program = {
    val context = TransformationContext(
      program.typeClasses.map(tc => tc.name -> tc).toMap,
      program.instances,
      List.empty
    )
    
    // Generate dictionary definitions for each instance
    val dictDefinitions = program.instances.flatMap { inst =>
      generateDictionaryDefinition(inst, context)
    }
    
    // Add dictionary definitions to the program's block
    val newBlock = program.block.copy(
      expressions = dictDefinitions ++ program.block.expressions
    )
    
    // Return the program without type class/instance declarations
    // but with generated dictionary code
    program.copy(
      typeClasses = Nil,
      instances = Nil,
      block = newBlock
    )
  }
  
  private def generateDictionaryDefinition(
    instance: InstanceDeclaration,
    context: TransformationContext
  ): List[Node] = {
    val dictName = generateDictionaryName(instance.className, instance.forType)
    val typeClass = context.typeClasses.get(instance.className)
    
    typeClass match {
      case Some(tc) =>
        // Create a record with all the type class methods
        val recordFields = instance.methods.map { method =>
          // Convert method definition to a lambda
          val lambda = Lambda(
            method.location,
            method.body.params,
            method.body.optionalType,
            method.body.body
          )
          (method.name, lambda)
        }
        
        // Create the dictionary as a val declaration
        val dictRecord = createRecordLiteral(instance.location, recordFields)
        val dictDecl = ValDeclaration(
          instance.location,
          dictName,
          None, // Let type inference handle it
          dictRecord,
          true // immutable
        )
        
        List(dictDecl)
        
      case None =>
        // Type class not found - this should be caught during type checking
        List.empty
    }
  }
  
  private def generateDictionaryName(className: String, forType: Type): String = {
    s"${className}_${normalizeType(forType)}_dict"
  }
  
  private def normalizeType(t: Type): String = t match {
    case TInt => "Int"
    case TString => "String"
    case TBoolean => "Boolean"
    case TFloat => "Float"
    case TDouble => "Double"
    case TUnit => "Unit"
    case TConstructor(name, _, _) => name
    case _ => "Unknown"
  }
  
  private def createRecordLiteral(location: Location, fields: List[(String, Node)]): Node = {
    // Create a record literal syntax
    // In Klassic, records are created with the syntax: record { field1: value1, field2: value2 }
    // For now, we'll simulate this with a map literal
    MapLiteral(
      location,
      fields.map { case (name, value) =>
        (StringNode(location, name), value)
      }
    )
  }
}