package com.github.klassic

import com.github.klassic.Type._
import scala.collection.mutable

case class TypeEnvironment(variables: Map[String, TypeScheme], immutableVariables: Set[String], records: Map[String, (List[TypeVariable], RecordConstructor)], modules: Map[String, Map[String, TypeScheme]], parent: Option[TypeEnvironment]) {
  def lookup(name: String): Option[TypeScheme] = {
    val result1 = variables.get(name)
    val result2  = result1.orElse(parent.flatMap{p => p.lookup(name)})
    result2
  }
  def lookupModuleMember(moduleName: String, memberName: String): Option[TypeScheme] = {
    for {
      module <- modules.get(moduleName)
      member <- module.get(memberName)
    } yield member
  }
  def updateMutableVariable(name: String, scheme: TypeScheme): TypeEnvironment = {
    this.copy(variables = this.variables + (name -> scheme))
  }
  def updateRecord(recordName: String, recordBody: (List[TypeVariable], RecordConstructor)): TypeEnvironment = {
    this.copy(records = this.records + (recordName -> recordBody))
  }
  def updateModule(moduleName: String, members: Map[String, TypeScheme]): TypeEnvironment = {
    this.copy(modules = this.modules + (moduleName -> members))
  }
  def updateImmuableVariable(name: String, scheme: TypeScheme): TypeEnvironment = {
    this.copy(variables = this.variables + (name -> scheme), immutableVariables = this.immutableVariables + name)
  }
}
object TypeEnvironment {
  def apply(variables: Map[String, TypeScheme]): TypeEnvironment = {
    TypeEnvironment(variables, Set.empty[String], Map.empty, Map.empty, None)
  }
}
