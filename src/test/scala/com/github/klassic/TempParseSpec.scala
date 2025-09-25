package com.github.klassic

class TempParseSpec extends SpecHelper {
  ignore("parse lambda with operator #cons") {
    val result = E(
      """
        |((x, y) => y #cons x)
      """.stripMargin
    )
    assert(result != null)
  }
}

