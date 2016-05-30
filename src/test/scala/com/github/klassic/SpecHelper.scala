package com.github.klassic

import org.scalatest.{DiagrammedAssertions, FunSpec}
import java.util.ArrayList

trait SpecHelper extends FunSpec with DiagrammedAssertions {
  def listOf[T](elements: T*): ArrayList[T] = {
    val newList = new ArrayList[T]
    elements.foreach{e =>
      newList.add(e)
    }
    newList
  }
}
