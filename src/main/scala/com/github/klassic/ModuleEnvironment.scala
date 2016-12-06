package com.github.klassic

import scala.collection.mutable
import scala.util.DynamicVariable

class ModuleEnvironment(val modules: mutable.Map[String, mutable.Map[String, Value]] = mutable.Map.empty) {
  val currentModule: DynamicVariable[String] = new DynamicVariable[String](null)
  def apply(module: String)(name: String): Value = {
    (for {
      module <- modules.get(module)
      member <- module.get(name)
    } yield member).getOrElse {
      throw InterpreterException("module '%s' or member '%s' not found".format(module, name))
    }
  }
  def enter(newModuleName: String)(block: => Unit): Unit = {
    currentModule.withValue(newModuleName) {
      block
    }
  }
  def update(name: String, value: Value): Value = {
    modules.get(currentModule.value) match {
      case Some(module) =>
        module(name) = value
        value
      case None =>
        val m = mutable.Map[String, Value]()
        m(name) = value
        modules(currentModule.value) = m
    }
    value
  }
  def define(name: String)(body: PartialFunction[List[Value], Value]): Value = {
    update(name, NativeFunctionValue(body))
  }
  override def toString: String = s"ModuleEnvironment(${modules})"
}
