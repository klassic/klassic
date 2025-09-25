package com.github.klassic

class TypeClassSimpleSpec extends SpecHelper {
  describe("Simple type class usage") {
    it("should define and use Show type class") {
      val program = """
        // Define Show type class
        typeclass Show<'a> where {
          show: ('a) => String
        }
        
        // Instance for Int
        instance Show<Int> where {
          def show(x: Int): String = "number: " + x
        }
        
        // Use it
        val result = show(42)
        result
      """.stripMargin
      
      assertResult(ObjectValue("number: 42"))(E(program))
    }
    
    it("should handle multiple instances") {
      val program = """
        typeclass Show<'a> where {
          show: ('a) => String
        }
        
        instance Show<Int> where {
          def show(x: Int): String = "[" + x + "]"
        }
        
        instance Show<String> where {
          def show(x: String): String = "<<" + x + ">>"
        }
        
        val r1 = show(123)
        val r2 = show("hello")
        
        r1 + " and " + r2
      """.stripMargin
      
      assertResult(ObjectValue("[123] and <<hello>>"))(E(program))
    }
    
    it("should work with record types") {
      val program = """
        typeclass Display<'a> where {
          display: ('a) => String
        }
        
        record Point {
          x: Int
          y: Int
        }
        
        instance Display<Point> where {
          def display(p: Point): String = "(" + p.x + "," + p.y + ")"
        }
        
        val p = #Point(3, 4)
        display(p)
      """.stripMargin
      
      assertResult(ObjectValue("(3,4)"))(E(program))
    }
    
    it("should demonstrate type class dictionary passing") {
      val program = """
        // The current implementation transforms type classes into dictionaries
        typeclass Printable<'a> where {
          print: ('a) => String
        }
        
        instance Printable<Int> where {
          def print(x: Int): String = "Int=" + x
        }
        
        instance Printable<Boolean> where {
          def print(x: Boolean): String = if(x) "YES" else "NO"
        }
        
        // These calls use the dictionary-based implementation
        val s1 = print(42)
        val s2 = print(true)
        val s3 = print(false)
        
        s1 + ", " + s2 + ", " + s3
      """.stripMargin
      
      assertResult(ObjectValue("Int=42, YES, NO"))(E(program))
    }
  }
}