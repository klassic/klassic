package com.github.klassic

import org.scalatest.{FlatSpec, FunSuite}

/**
  * Created by Mizushima on 2016/05/30.
  */
class PlaceholderSpec extends TestSuiteHelper {
  test("binary expression has  one placeholder") {
    val result = E(
      """
        |val xs = [1 2 3]
        |map(xs)(_ + 1)
      """.stripMargin
    )
    assertResult(result)(ObjectValue(listOf(2, 3, 4)))
  }

  test("binary expression has two placeholder (1)") {
    val result = E(
      """
        |val add = _ + _
        |foldLeft([1 2 3])(0)(add)
      """.stripMargin
    )
    assertResult(result)(BoxedInt(6))
  }

  test("binary expression has two placeholder (2)") {
    val result = E(
      """
        |foldLeft([1 2 3])(0)(_ + _)
      """.stripMargin
    )
    assertResult(result)(BoxedInt(6))
  }

  test("unary expression - has one placeholder") {
    val result = E(
      """
        |map([1 2 3])(- _)
      """.stripMargin
    )
    assertResult(result)(ObjectValue(listOf(-1, -2, -3)))
  }
  test("unary expression + has one placeholder") {
    val result = E(
      """
        |map([1 2 3])(+ _)
      """.stripMargin
    )
    assertResult(result)(ObjectValue(listOf(1, 2, 3)))
  }
}
