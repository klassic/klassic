package com.github.klassic

class HigherKindedTypeClassSpec extends SpecHelper {
  describe("Higher-kinded type class support") {
    it("should parse type classes with kind annotations") {
      val program = """
        typeclass Functor<'f: * => *> where {
          map: (('a) => 'b, 'f<'a>) => 'f<'b>
        }
        
        typeclass Monad<'m: * => *> where {
          bind: ('m<'a>, ('a) => 'm<'b>) => 'm<'b>;
          unit: ('a) => 'm<'a>
        }
        
        1
      """
      val result = E(program)
      assert(result == BoxedInt(1))
    }

    it("should support instances for higher-kinded types") {
      val program = """
        typeclass Functor<'f: * => *> where {
          map: (('a) => 'b, 'f<'a>) => 'f<'b>
        }
        
        instance Functor<List> where {
          def map(f: ('a) => 'b, xs: List<'a>): List<'b> = {
            xs.map(f)
          }
        }
        
        2
      """
      val result = E(program)
      assert(result == BoxedInt(2))
    }

    it("should resolve higher-kinded type class instances") {
      val program = """
        typeclass Functor<'f: * => *> where {
          map: (('a) => 'b, 'f<'a>) => 'f<'b>
        }
        
        instance Functor<List> where {
          def map(f: ('a) => 'b, xs: List<'a>): List<'b> = {
            xs.map(f)
          }
        }
        
        val numbers = [1, 2, 3]
        map((x) => x * 2, numbers)
      """
      val result = E(program)
      // Check the actual result type and content
      result match {
        case ObjectValue(list: java.util.ArrayList[_]) =>
          assert(list.size() == 3)
          // The VM returns Java Integer objects, not BoxedInt
          assert(list.get(0) == Integer.valueOf(2))
          assert(list.get(1) == Integer.valueOf(4))
          assert(list.get(2) == Integer.valueOf(6))
        case _ =>
          fail(s"Expected ObjectValue with ArrayList, got: $result")
      }
    }

    it("should parse type constructors in constraints") {
      val program = """
        typeclass Functor<'f: * => *> where {
          map: (('a) => 'b, 'f<'a>) => 'f<'b>
        }
        
        3
      """
      val result = E(program)
      assert(result == BoxedInt(3))
    }
  }
}