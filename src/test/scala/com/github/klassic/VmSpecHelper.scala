package com.github.klassic

import java.util.{ArrayList, HashMap}
import org.scalatest.diagrams.Diagrams
import org.scalatest.funspec.AnyFunSpec

trait VmSpecHelper extends AnyFunSpec with Diagrams {
  val E = new Evaluator
  def V(program: String): Value = E.evaluateStringWithVm(program)
  def listOf[T](elements: T*): ArrayList[T] = {
    val newList = new ArrayList[T]
    elements.foreach(newList.add)
    newList
  }
  def mapOf[K, V](kvs: (K, V)*): HashMap[K, V] = {
    val newMap = new HashMap[K, V]
    kvs.foreach{ case (k, v) => newMap.put(k, v) }
    newMap
  }
}
