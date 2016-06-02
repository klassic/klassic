package com.github.klassic

case class TypeEnvironment(variables: Map[String, TypeDescription], parent: Option[TypeEnvironment]) {
  def lookup(name: String): Option[TypeDescription] = {
    val result1 = variables.get(name)
    val result2  = result1.orElse(parent.flatMap{p => p.lookup(name)})
    result2
  }
}
object TypeEnvironment {
  def apply(variables: Map[String, TypeDescription]): TypeEnvironment = {
    TypeEnvironment(variables, None)
  }
}
