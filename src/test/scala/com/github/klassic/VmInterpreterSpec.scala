package com.github.klassic

class VmInterpreterSpec extends VmSpecHelper {
  describe("function call") {
    it("evaluates function literal") {
      assertResult(BoxedInt(6))(
        V(
          """|
            |val add = (x, y) => x + y
            |add(3, 3)
            |""".stripMargin
        )
      )
    }
  }

  describe("method call") {
    it("invokes a Java method") {
      assertResult(ObjectValue("hello"))(
        V(
          """|
            |val sb = new java.lang.StringBuilder()
            |sb->append("he")
            |sb->append("llo")
            |sb->toString()
            |""".stripMargin
        )
      )
    }
  }

  describe("object creation") {
    it("creates new object") {
      assertResult(ObjectValue("abc"))(
        V("new java.lang.String(\"abc\")")
      )
    }
  }

  describe("closure") {
    it("captures outer variable") {
      assertResult(BoxedInt(3))(
        V(
          """|
            |mutable x = 1
            |val f = (y) => x + y
            |f(2)
            |""".stripMargin
        )
      )
    }
  }
}
