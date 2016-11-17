package com.github.klassic

import com.github.klassic.TypeDescription._
import scala.collection.mutable

case class TypeEnvironment(variables: Map[String, TypeScheme], immutableVariables: Set[String], parent: Option[TypeEnvironment]) {
  def lookup(name: String): Option[TypeScheme] = {
    val result1 = variables.get(name)
    val result2  = result1.orElse(parent.flatMap{p => p.lookup(name)})
    result2
  }
  def updateMutableVariable(name: String, scheme: TypeScheme): TypeEnvironment = {
    this.copy(variables = this.variables + (name -> scheme))
  }
  def updateImmuableVariable(name: String, scheme: TypeScheme): TypeEnvironment = {
    this.copy(variables = this.variables + (name -> scheme), immutableVariables = this.immutableVariables + name)
  }
}
object TypeEnvironment {
  def apply(variables: Map[String, TypeScheme]): TypeEnvironment = {
    TypeEnvironment(variables, Set.empty[String], None)
  }
}
