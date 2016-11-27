package com.github.klassic

import org.scalatest.{DiagrammedAssertions, FunSpec}
import java.util.ArrayList
import java.util.HashMap

trait SpecHelper extends FunSpec with DiagrammedAssertions {
  val I = new Interpreter
  def listOf[T](elements: T*): ArrayList[T] = {
    val newList = new ArrayList[T]
    elements.foreach{e =>
      newList.add(e)
    }
    newList
  }
  def mapOf[K, V](kvs: (K, V)*): HashMap[K, V] = {
    val newMap = new HashMap[K, V]
    for((k, v) <- kvs) {
      newMap.put(k, v)
    }
    newMap
  }
  def expect[A, B](label: String)(expectation: (String, Value)): Unit = expectation match {
    case (expected, actual) =>
      it(label) {
        assertResult(I.evaluateString(expected))(actual)
      }
  }
}
