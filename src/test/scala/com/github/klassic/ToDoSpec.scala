package com.github.klassic

import com.github.klassic.runtime.NotImplementedError

class ToDoSpec extends SpecHelper {
  val I = new Interpreter

  describe("ToDo() function") {
    it("throw NotImplementedException when it is evaluated") {
      intercept[NotImplementedError] {
        I.evaluateString(
          """
            |def fact(n) = if(n < 2) ToDo() else n * fact(n - 1)
            |fact(0)
          """.stripMargin
        )
      }
    }
  }
}
