package com.github.klassic

class ModuleImportSpec extends SpecHelper {
  describe("module declaration header") {
    it("parses module header without semicolon and evaluates body") {
      val result = E(
        """
          |module foo.bar
          |1 + 1
        """.stripMargin
      )
      assertResult(BoxedInt(2))(result)
    }

    it("parses module header with semicolon and evaluates body") {
      val result = E(
        """
          |module foo.bar;
          |1 + 1
        """.stripMargin
      )
      assertResult(BoxedInt(2))(result)
    }
  }

  describe("import built-in modules and use members unqualified") {
    it("imports Map module and uses size on a map literal") {
      val result = E(
        """
          |import Map
          |size(%["A": 1, "B": 2])
        """.stripMargin
      )
      assertResult(BoxedInt(2))(result)
    }

    it("imports Set module and uses contains on a set literal (curried)") {
      val result = E(
        """
          |import Set
          |contains(%(1, 2, 3))(2)
        """.stripMargin
      )
      assertResult(BoxedBoolean(true))(result)
    }

    it("imports with alias and selective members") {
      val result = E(
        """
          |import Map as M
          |import Map.{size}
          |size(%["A": 1])
        """.stripMargin
      )
      assertResult(BoxedInt(1))(result)
      // selector via alias should also work
      val selector = E(
        """
          |import Map as M
          |M#size(%["A": 1])
        """.stripMargin
      )
      assertResult(BoxedInt(1))(selector)
    }
  }

  describe("import user-defined modules") {
    it("imports a user module by simple name and calls a function") {
      // Define and export the module
      E(
        """
          |module m
          |def twice(x) = x + x
        """.stripMargin
      )
      // Import and use it in a separate program
      val result = E(
        """
          |import m
          |twice(21)
        """.stripMargin
      )
      assertResult(BoxedInt(42))(result)
    }

    it("imports a user module by fully-qualified name and calls a function") {
      // Define and export the module under a namespace
      E(
        """
          |module user.util
          |def inc(x) = x + 1
        """.stripMargin
      )
      // Import using the FQCN; alias-free selector with FQCN is also supported
      val result = E(
        """
          |import user.util
          |inc(41)
        """.stripMargin
      )
      assertResult(BoxedInt(42))(result)

      val selector = E(
        """
          |user.util#inc(41)
        """.stripMargin
      )
      assertResult(BoxedInt(42))(selector)
    }
  }
}
