package com.github.klassic

class StringUtilsSpec extends SpecHelper {
  describe("Enhanced String utilities") {
    it("should split strings") {
      assertResult(
        E("""split("hello,world,test", ",")""")
      )(Value.toKlassic(java.util.Arrays.asList("hello", "world", "test")))
      
      assertResult(
        E("""split("a b c", " ")""")
      )(Value.toKlassic(java.util.Arrays.asList("a", "b", "c")))
    }

    it("should join strings") {
      assertResult(
        E("""join(["hello", "world"], ", ")""")
      )(ObjectValue("hello, world"))
      
      assertResult(
        E("""join(["a", "b", "c"], "-")""")
      )(ObjectValue("a-b-c"))
    }

    it("should trim strings") {
      assertResult(E("""trim("  hello  ")"""))(ObjectValue("hello"))
      assertResult(E("""trimLeft("  hello  ")"""))(ObjectValue("hello  "))
      assertResult(E("""trimRight("  hello  ")"""))(ObjectValue("  hello"))
    }

    it("should replace strings") {
      assertResult(
        E("""replace("hello world", "world", "klassic")""")
      )(ObjectValue("hello klassic"))
      
      assertResult(
        E("""replaceAll("123-456-789", "[0-9]", "X")""")
      )(ObjectValue("XXX-XXX-XXX"))
    }

    it("should change case") {
      assertResult(E("""toLowerCase("HELLO")"""))(ObjectValue("hello"))
      assertResult(E("""toUpperCase("hello")"""))(ObjectValue("HELLO"))
    }

    it("should check string properties") {
      assertResult(E("""startsWith("hello world", "hello")"""))(BoxedBoolean(true))
      assertResult(E("""endsWith("hello world", "world")"""))(BoxedBoolean(true))
      assertResult(E("""contains("hello world", "lo wo")"""))(BoxedBoolean(true))
      assertResult(E("""isEmptyString("")"""))(BoxedBoolean(true))
      assertResult(E("""isEmptyString("hello")"""))(BoxedBoolean(false))
    }

    it("should find string positions") {
      assertResult(E("""indexOf("hello world", "world")"""))(BoxedInt(6))
      assertResult(E("""indexOf("hello world", "xyz")"""))(BoxedInt(-1))
      assertResult(E("""lastIndexOf("hello hello", "hello")"""))(BoxedInt(6))
    }

    it("should get string length") {
      assertResult(E("""length("hello")"""))(BoxedInt(5))
      assertResult(E("""length("")"""))(BoxedInt(0))
    }

    it("should repeat and reverse strings") {
      assertResult(E("""repeat("ab", 3)"""))(ObjectValue("ababab"))
      assertResult(E("""reverse("hello")"""))(ObjectValue("olleh"))
    }

    // Format function requires variadic arguments which need special handling
    // Skipping for now as it requires more complex type system support

    it("should work with other features") {
      assertResult(
        E("""
          val text = "  Hello, World!  "
          val words = split(trim(text), ", ")
          val upper = map(words)((w) => toUpperCase(w))
          join(upper, " - ")
        """.stripMargin)
      )(ObjectValue("HELLO - WORLD!"))
    }
  }
}