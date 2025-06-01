package com.github.klassic

class TypeClassParserSpec extends SpecHelper {
  describe("Type class parser") {
    val parser = new Parser
    
    it("should parse type class declaration without type params") {
      val program = """typeclass Show where {
        show: ('a) => String
      }
      1
      """
      val ast = parser.parseAll(program)
      assert(ast.typeClasses.size == 1)
      assert(ast.typeClasses.head.name == "Show")
      assert(ast.typeClasses.head.typeParams.size == 1) // default type param
      assert(ast.typeClasses.head.methods.size == 1)
      assert(ast.typeClasses.head.methods.head.name == "show")
    }
    
    it("should parse type class declaration with type params") {
      val program = """typeclass Show<'a> where {
        show: ('a) => String
      }
      1
      """
      try {
        val ast = parser.parseAll(program)
        assert(ast.typeClasses.size == 1)
        assert(ast.typeClasses.head.name == "Show")
        assert(ast.typeClasses.head.typeParams.size == 1)
        assert(ast.typeClasses.head.methods.size == 1)
        assert(ast.typeClasses.head.methods.head.name == "show")
      } catch {
        case e: Exception =>
          println(s"Parse failed: ${e.getMessage}")
          throw e
      }
    }
    
    it("should parse instance declaration") {
      val program = """typeclass Show<'a> where {
        show: ('a) => String
      }
      
      instance Show<Int> where {
        def show(x: Int): String = x
      }
      1
      """
      val ast = parser.parseAll(program)
      assert(ast.typeClasses.size == 1)
      assert(ast.instances.size == 1)
      assert(ast.instances.head.className == "Show")
      assert(ast.instances.head.methods.size == 1)
    }
  }
}