package com.github.klassic

import scala.collection.mutable

class ModuleEnvironment(val modules: mutable.Map[String, mutable.Map[String, Value]] = mutable.Map.empty) {
  def apply(module: String)(name: String): Value = {
    (for {
      module <- modules.get(module)
      member <- module.get(name)
    } yield member).getOrElse {
      throw InterpreterException("module '%s' or member '%s' not found".format(module, name))
    }
  }
  def update(module: String, name: String, value: Value): Value = {
    modules.get(module) match {
      case Some(module) =>
        module(name) = value
        value
      case None =>
        val m = mutable.Map[String, Value]()
        m(name) = value
        modules(module) = m
    }
    value
  }
  def define(module: String)(name: String)(body: PartialFunction[List[Value], Value]): Value = {
    update(module, name, NativeFunctionValue(body))
  }
  override def toString: String = s"ModuleEnvironment(${modules})"
}
