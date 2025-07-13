package com.github.klassic

class TypeClassUsageSpec extends SpecHelper {
  describe("Type class usage") {
    it("should use type class methods for different types") {
      val program = """
        // Define Show type class
        typeclass Show<'a> where {
          show: ('a) => String
        }
        
        // Instance for Int
        instance Show<Int> where {
          def show(x: Int): String = "Int: " + x
        }
        
        // Instance for String  
        instance Show<String> where {
          def show(x: String): String = "Str: " + x
        }
        
        // Use show method
        val result1 = show(42)
        val result2 = show("hello")
        
        assert(result1 == "Int: 42")
        assert(result2 == "Str: hello")
        
        "Type class methods work"
      """.stripMargin
      
      assertResult(ObjectValue("Type class methods work"))(E(program))
    }
    
    it("should support multiple type class instances") {
      val program = """
        // Define Eq type class
        typeclass Eq<'a> where {
          equals: ('a, 'a) => Boolean
        }
        
        // Instance for Int
        instance Eq<Int> where {
          def equals(x: Int, y: Int): Boolean = x == y
        }
        
        // Instance for String
        instance Eq<String> where {
          def equals(x: String, y: String): Boolean = x == y
        }
        
        // Custom type
        record Person {
          name: String
          age: Int
        }
        
        // Instance for Person
        instance Eq<Person> where {
          def equals(p1: Person, p2: Person): Boolean = 
            equals(p1.name, p2.name) && equals(p1.age, p2.age)
        }
        
        val p1 = #Person("Alice", 30)
        val p2 = #Person("Alice", 30)
        val p3 = #Person("Bob", 25)
        
        assert(equals(10, 10))
        assert(!equals(10, 20))
        assert(equals("foo", "foo"))
        assert(equals(p1, p2))
        assert(!equals(p1, p3))
        
        "Multiple instances work"
      """.stripMargin
      
      assertResult(ObjectValue("Multiple instances work"))(E(program))
    }
    
    it("should demonstrate practical type class usage") {
      val program = """
        // Serializable type class
        typeclass Serializable<'a> where {
          serialize: ('a) => String
          deserialize: (String) => 'a
        }
        
        // Instance for Int
        instance Serializable<Int> where {
          def serialize(x: Int): String = x.toString()
          def deserialize(s: String): Int = int(double(1)) // Simplified
        }
        
        // Instance for List<Int>
        instance Serializable<List<Int>> where {
          def serialize(xs: List<Int>): String = 
            "[" + join(map(xs)((x) => serialize(x)), ",") + "]"
          def deserialize(s: String): List<Int> = 
            [1, 2, 3] // Simplified for demo
        }
        
        val nums = [10, 20, 30]
        val serialized = serialize(nums)
        assert(serialized == "[10,20,30]")
        
        "Serialization works"
      """.stripMargin
      
      assertResult(ObjectValue("Serialization works"))(E(program))
    }
  }
}