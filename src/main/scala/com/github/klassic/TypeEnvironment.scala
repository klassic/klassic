package com.github.klassic
import scala.collection.mutable

case class TypeEnvironment(variables: mutable.Map[String, TypeDescription], immutableVariables: mutable.Set[String], parent: Option[TypeEnvironment]) {
  def lookup(name: String): Option[TypeDescription] = {
    val result1 = variables.get(name)
    val result2  = result1.orElse(parent.flatMap{p => p.lookup(name)})
    result2
  }
}
object TypeEnvironment {
  def apply(variables: mutable.Map[String, TypeDescription]): TypeEnvironment = {
    TypeEnvironment(variables, mutable.Set.empty[String], None)
  }
}
