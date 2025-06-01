package com.github.klassic

class TypeClassSpec extends SpecHelper {
  describe("Type class support") {
    it("should parse type class declarations") {
      val program = """
        typeclass Show<'a> where {
          show: ('a) => String
        }
        1
      """
      val result = E(program)
      assert(result == BoxedInt(1))
    }

    it("should parse instance declarations") {
      val program = """
        typeclass Show<'a> where {
          show: ('a) => String
        }
        
        instance Show<Int> where {
          def show(x: Int): String = "Int: " + x
        }
        2
      """
      val result = E(program)
      assert(result == BoxedInt(2))
    }

    it("should support basic type class method calls") {
      val program = """
        typeclass Show<'a> where {
          show: ('a) => String
        }
        
        instance Show<Int> where {
          def show(x: Int): String = "Int(" + x + ")"
        }
        
        instance Show<String> where {
          def show(x: String): String = "String(" + x + ")"
        }
        
        // Direct method call - should resolve to correct instance
        show(42)
      """
      val result = E(program)
      assert(result == ObjectValue("Int(42)"))
    }
    
    it("should support type class with multiple methods") {
      val program = """
        typeclass Eq<'a> where {
          equals: ('a, 'a) => Boolean
          notEquals: ('a, 'a) => Boolean
        }
        
        instance Eq<Int> where {
          def equals(x: Int, y: Int): Boolean = x == y
          def notEquals(x: Int, y: Int): Boolean = x != y
        }
        
        instance Eq<String> where {
          def equals(x: String, y: String): Boolean = x == y
          def notEquals(x: String, y: String): Boolean = x != y
        }
        
        // Use type class methods directly
        equals(5, 5)
      """
      val result = E(program)
      assert(result == BoxedBoolean(true))
    }
  }
}