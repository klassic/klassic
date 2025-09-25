package com.github.klassic

import com.github.klassic.Type._
import scala.collection.mutable

case class TypeEnvironment(
  variables: Map[String, TScheme], 
  immutableVariables: Set[String], 
  records: Map[String, TRecord], 
  modules: Map[String, Map[String, TScheme]], 
  typeClasses: Map[String, TTypeClass],
  instances: Map[(String, Type), TInstance],
  parent: Option[TypeEnvironment]
) {
  def lookup(name: String): Option[TScheme] = {
    val result1 = variables.get(name)
    val result2  = result1.orElse(parent.flatMap{p => p.lookup(name)})
    result2
  }
  def lookupModuleMember(moduleName: String, memberName: String): Option[TScheme] = {
    for {
      module <- modules.get(moduleName)
      member <- module.get(memberName)
    } yield member
  }
  def updateMutableVariable(name: String, scheme: TScheme): TypeEnvironment = {
    this.copy(variables = this.variables + (name -> scheme))
  }
  def updateRecord(recordName: String, recordBody: TRecord): TypeEnvironment = {
    this.copy(records = this.records + (recordName -> recordBody))
  }
  def updateModule(moduleName: String, members: Map[String, TScheme]): TypeEnvironment = {
    this.copy(modules = this.modules + (moduleName -> members))
  }
  def updateImmuableVariable(name: String, scheme: TScheme): TypeEnvironment = {
    this.copy(variables = this.variables + (name -> scheme), immutableVariables = this.immutableVariables + name)
  }
  
  def lookupTypeClass(name: String): Option[TTypeClass] = {
    val result = typeClasses.get(name)
    result.orElse(parent.flatMap(_.lookupTypeClass(name)))
  }
  
  def updateTypeClass(name: String, typeClass: TTypeClass): TypeEnvironment = {
    this.copy(typeClasses = this.typeClasses + (name -> typeClass))
  }
  
  def lookupInstance(className: String, forType: Type): Option[TInstance] = {
    val result = instances.get((className, forType))
    result.orElse(parent.flatMap(_.lookupInstance(className, forType)))
  }
  
  def updateInstance(className: String, forType: Type, instance: TInstance): TypeEnvironment = {
    this.copy(instances = this.instances + ((className, forType) -> instance))
  }
  
  def findMatchingInstance(className: String, targetType: Type): Option[TInstance] = {
    // First try exact match
    lookupInstance(className, targetType).orElse {
      // Then try to find a matching instance by unifying types
      val candidates = instances.filter(_._1._1 == className)
      candidates.find { case ((_, instanceType), _) =>
        // Simple type matching for now - can be improved with proper unification
        canMatch(instanceType, targetType)
      }.map(_._2).orElse(parent.flatMap(_.findMatchingInstance(className, targetType)))
    }
  }
  
  private def canMatch(instanceType: Type, targetType: Type): Boolean = {
    (instanceType, targetType) match {
      case (TVariable(_, _), _) => true // Type variables match anything
      case (TConstructor(n1, args1, _), TConstructor(n2, args2, _)) if n1 == n2 && args1.length == args2.length =>
        args1.zip(args2).forall { case (t1, t2) => canMatch(t1, t2) }
      case (TRecordReference(name1, _), TRecordReference(name2, _)) => name1 == name2
      case (TRecordReference(_, _), TRecord(_, _)) => true // Record reference can match structural record
      case (TRecord(_, _), TRecordReference(_, _)) => true // Structural record can match record reference
      case (TRecord(_, _), TRecord(_, _)) => true // Structural records can match each other (simplified for now)
      // Handle mismatch between instance declaration parsing (TConstructor) and record creation (TRecordReference)
      case (TConstructor(name1, args1, _), TRecordReference(name2, args2)) if name1 == name2 => 
        args1.length == args2.length && args1.zip(args2).forall { case (t1, t2) => canMatch(t1, t2) }
      case (TRecordReference(name1, args1), TConstructor(name2, args2, _)) if name1 == name2 => 
        args1.length == args2.length && args1.zip(args2).forall { case (t1, t2) => canMatch(t1, t2) }
      case (t1, t2) => t1 == t2
    }
  }
}
object TypeEnvironment {
  def apply(variables: Map[String, TScheme]): TypeEnvironment = {
    TypeEnvironment(variables, Set.empty[String], Map.empty, Map.empty, Map.empty, Map.empty, None)
  }
}
