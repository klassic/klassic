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
    // Transform instance declarations into LetRec nodes
    // This makes type class methods available as top-level functions
    
    // For each instance, wrap all its methods and the rest of the program in nested LetRec
    val transformedBlock = program.instances.foldRight(program.block) { (inst, currentBlock) =>
      // For each method in the instance, create a LetRec
      inst.methods.foldRight(currentBlock) { (method, innerBlock) =>
        val letRec = Ast.LetRec(
          method.location,
          method.name,
          method.body,
          method.cleanup,
          innerBlock
        )
        // Wrap in a new block
        Ast.Block(innerBlock.location, List(letRec))
      }
    }
    
    // Return program with the transformed block but keep instances for typing
    program.copy(
      block = transformedBlock
    )
  }
  
  private def generateMethodDefinitions(
    instance: InstanceDeclaration,
    context: TransformationContext
  ): List[Node] = {
    // For each method in the instance, generate a LetRec node
    // that will be added to the top level of the program
    instance.methods.map { method =>
      LetRec(
        method.location,
        method.name,
        method.body,
        method.cleanup,
        // Use UnitNode as body since this is at the top level
        UnitNode(method.location)
      )
    }
  }
  
}