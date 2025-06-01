package com.github.klassic

class TypeClassDetailedSpec extends SpecHelper {
  describe("Type class parsing details") {
    val parser = new Parser
    
    it("should handle simple type class without type params") {
      val program = """typeclass Show where {
}
1"""
      try {
        val ast = parser.parseAll(program)
        println(s"Parsed successfully: ${ast.typeClasses}")
        assert(ast.typeClasses.size == 1)
        assert(ast.typeClasses.head.name == "Show")
      } catch {
        case e: Exception =>
          println(s"Parse error: ${e.getMessage}")
          fail(e)
      }
    }
    
    it("should parse type class with angle brackets") {
      val program = """typeclass Show<'a> where {
}
1"""
      try {
        val ast = parser.parseAll(program)
        println(s"Parsed with type params: ${ast.typeClasses}")
        assert(ast.typeClasses.size == 1)
        assert(ast.typeClasses.head.name == "Show")
        assert(ast.typeClasses.head.typeParams.size == 1)
      } catch {
        case e: Exception =>
          println(s"Parse error at: ${e.getMessage}")
          // Try to understand what's happening
          val lines = program.split("\n")
          println(s"Line content: '${lines(0)}'")
          println(s"Character at position 14: '${if (program.length > 14) program.charAt(14) else "EOF"}'")
          fail(e)
      }
    }
  }
}