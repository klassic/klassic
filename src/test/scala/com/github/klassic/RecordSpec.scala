package com.github.klassic

/**
  * Created by Mizushima on 2016/05/30.
  */
class RecordSpec extends SpecHelper {

  describe("new record") {
    val expectations: List[(String, Value)] = List(
      """
        |record Person {
        |  name: *
        |  age: Int
        |}
        |#Person("Hoge", 7)
      """.stripMargin -> RecordValue("Person", List("name" -> ObjectValue("Hoge"), "age" -> BoxedInt(7))),
      """
        |record Tuple<'a, 'b> {
        |  _1: 'a
        |  _2: 'b
        |}
        |#Tuple(1, 2)
      """.stripMargin -> RecordValue("Tuple", List("_1" -> BoxedInt(1), "_2" -> BoxedInt(2)))
    )

    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E.evaluateString(in))
      }
    }
  }

  describe("access record") {
    val expectations: List[(String, Value)] = List(
      """
        |record Person {
        |  name: *
        |  age: Int
        |}
        |val p = #Person("Hoge", 7)
        |p.name
      """.stripMargin -> ObjectValue("Hoge"),
      """
        |record Tuple<'a, 'b> {
        |  _1: 'a
        |  _2: 'b
        |}
        |val t = #Tuple(1, 2)
        |t._1
      """.stripMargin -> BoxedInt(1)
    )

    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }

    intercept[TyperException] {
      E {
        """
          | record Person {
          |   name: *
          |   age: Int
          | }
          | val p = #Person("Hoge", 1.0)
        """.stripMargin
      }
    }
    intercept[TyperException] {
      E {
        """
          | record Tuple<'a, 'b> {
          |   _1: 'a
          |   _2: 'b
          | }
          | val t = #Tuple(1, 2)
          | val k: Double = t._1
        """.stripMargin
      }
    }
  }
}
