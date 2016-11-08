package com.github.klassic

import com.github.klassic.TypeDescription._
import scala.collection.mutable

case class TypeEnvironment(variables: mutable.Map[String, TypeScheme], immutableVariables: mutable.Set[String], parent: Option[TypeEnvironment]) {
  def lookup(name: String): Option[TypeScheme] = {
    val result1 = variables.get(name)
    val result2  = result1.orElse(parent.flatMap{p => p.lookup(name)})
    result2
  }
}
object TypeEnvironment {
  def apply(variables: mutable.Map[String, TypeScheme]): TypeEnvironment = {
    TypeEnvironment(variables, mutable.Set.empty[String], None)
  }
}
