package com.github.klassic

import scala.collection.mutable

/**
 * @author Kota Mizushima
 */
class Environment(val parent:Option[Environment]) {
  val variables = mutable.Map[String, Value]()
  def apply(key: String): Value = {
    variables.getOrElse(key, parent.map(_.apply(key)).getOrElse {
      throw new Exception("symbol'%s' not found".format(key))
    })
  }
  def set(key: String, value: Value): Value = {
    def iset(optEnv: Option[Environment]): Unit = optEnv match {
      case Some(env) => if(env.variables.contains(key)) env(key) = value else iset(env.parent)
      case None => ()
    }
    iset(Some(this))
    value
  }
  def define(name: String)(body: PartialFunction[List[Value], Value]): Value = {
    update(name, NativeFunctionValue(body))
  }
  def update(key: String, value: Value): Value = {
    variables(key) = value
    value
  }
  override def toString: String = s"Environment(${variables})"
}
