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
        |new #Person("Hoge", 7)
      """.stripMargin -> RecordValue("Person", List("name" -> ObjectValue("Hoge"), "age" -> BoxedInt(7))),
      """
        |record Tuple<a', b'> {
        |  _1: a'
        |  _2: b'
        |}
        |new #Tuple(1, 2)
      """.stripMargin -> RecordValue("Tuple", List("_1" -> BoxedInt(1), "_2" -> BoxedInt(2)))
    )

    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == I.evaluateString(in))
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
        |val p = new #Person("Hoge", 7)
        |p.name
      """.stripMargin -> ObjectValue("Hoge"),
      """
        |record Tuple<a', b'> {
        |  _1: a'
        |  _2: b'
        |}
        |val t = new #Tuple(1, 2)
        |t._1
      """.stripMargin -> BoxedInt(1)
    )

    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == I.evaluateString(in))
      }
    }
  }
}
