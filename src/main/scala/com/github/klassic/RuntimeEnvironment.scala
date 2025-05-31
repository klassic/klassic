package com.github.klassic

import scala.collection.mutable

/**
 * @author Kota Mizushima
 */
class RuntimeEnvironment(var parent:Option[RuntimeEnvironment]) {
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
  
  // Reset the environment for reuse
  def reset(newParent: Option[RuntimeEnvironment] = None): Unit = {
    variables.clear()
    parent = newParent
  }
}

// Environment pool to reduce allocations
object RuntimeEnvironment {
  private val pool = mutable.ArrayBuffer[RuntimeEnvironment]()
  private val MAX_POOL_SIZE = 100
  
  def pooled(parent: Option[RuntimeEnvironment]): RuntimeEnvironment = {
    if (pool.nonEmpty) {
      val env = pool.remove(pool.length - 1)
      env.reset(parent)
      env
    } else {
      new RuntimeEnvironment(parent)
    }
  }
  
  def release(env: RuntimeEnvironment): Unit = {
    if (pool.length < MAX_POOL_SIZE) {
      env.reset()
      pool += env
    }
  }
}
