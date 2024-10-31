package com.github.klassic

import scala.collection.mutable

/**
 * @author Kota Mizushima
 */
class RuntimeEnvironment(val parent:Option[RuntimeEnvironment]) {
  val variables: mutable.Map[String,Value] = mutable.Map[String, Value]()
  def apply(key: String): Value = {
    variables.getOrElse(key, parent.map(_.apply(key)).getOrElse {
      throw new Exception("symbol'%s' not found".format(key))
    })
  }
  def set(key: String, value: Value): Value = {
    def iset(optEnv: Option[RuntimeEnvironment]): Unit = optEnv match {
      case Some(env) => if(env.variables.contains(key)) env(key) = value else iset(env.parent)
      case None => ()
    }
    iset(Some(this))
    value
  }
  def define(name: String)(body: PartialFunction[List[Value], Value]): Value = {
    update(name, NativeFunctionValue(body))
  }
  def defineValue(name: String)(value: Value): Value = {
    update(name, value)
  }
  def update(key: String, value: Value): Value = {
    variables(key) = value
    value
  }
  override def toString: String = s"Environment(${variables})"
}
