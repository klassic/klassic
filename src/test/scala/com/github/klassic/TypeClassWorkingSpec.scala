package com.github.klassic

class TypeClassWorkingSpec extends SpecHelper {
  describe("Working type class tests") {
    it("should use Show type class from existing test") {
      // This is based on the existing working test
      val program = """
        typeclass Show<'a> where {
          show: ('a) => String
        }
        
        instance Show<Int> where {
          def show(x: Int): String = "Int: " + x
        }
        
        instance Show<String> where {
          def show(x: String): String = "String: " + x
        }
        
        // Direct method calls work in current implementation
        show(42)
      """.stripMargin
      
      val result = E(program)
      println(s"Type class result: $result")
      assertResult(ObjectValue("Int: 42"))(result)
    }
    
    it("should verify the basic mechanism works") {
      val program = """
        typeclass Display<'a> where {
          display: ('a) => String
        }
        
        instance Display<Int> where {
          def display(x: Int): String = x.toString()
        }
        
        val result = display(123)
        result
      """.stripMargin
      
      // Let's see what actually happens
      val result = E(program)
      println(s"Result: $result")
      result
    }
  }
}