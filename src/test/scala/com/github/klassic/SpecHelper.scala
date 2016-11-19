package com.github.klassic

import org.scalatest.{DiagrammedAssertions, FunSpec}
import java.util.ArrayList
import java.util.HashMap

trait SpecHelper extends FunSpec with DiagrammedAssertions {
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
  def expect[A, B](expectation: (A, B)): Unit = expectation match {
    case (expected, actual) =>
      it(s"expectation ${expected}") {
        assertResult(expected)(actual)
      }
  }
}
