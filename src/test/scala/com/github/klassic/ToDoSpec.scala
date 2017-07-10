package com.github.klassic

import klassic.runtime.NotImplementedError

class ToDoSpec extends SpecHelper {
  describe("ToDo() function") {
    it("throw NotImplementedException when it is evaluated") {
      intercept[NotImplementedError] {
        E(
          """
            |def fact(n) = if(n < 2) ToDo() else n * fact(n - 1)
            |fact(0)
          """.stripMargin
        )
      }
    }
  }
}
