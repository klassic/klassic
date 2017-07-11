package com.github.klassic

import com.github.klassic.Type._
import scala.collection.mutable

case class TypeEnvironment(variables: Map[String, TScheme], immutableVariables: Set[String], records: Map[String, TRecord], modules: Map[String, Map[String, TScheme]], parent: Option[TypeEnvironment]) {
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
}
object TypeEnvironment {
  def apply(variables: Map[String, TScheme]): TypeEnvironment = {
    TypeEnvironment(variables, Set.empty[String], Map.empty, Map.empty, None)
  }
}
