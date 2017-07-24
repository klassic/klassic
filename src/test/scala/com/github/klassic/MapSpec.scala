package com.github.klassic

class MapSpec extends SpecHelper {
  describe("containsKey") {
    val expectations: List[(String, Value)] = List(
      """
        |%["name": "Kota Mizushima" "age": "33"] Map#containsKey "name"
      """.stripMargin -> BoxedBoolean(true),
      """
        |%["name": "Kota Mizushima" "age": "33"] Map#containsKey "age"
      """.stripMargin -> BoxedBoolean(true),
      """
        |%["name": "Kota Mizushima" "age": "33"] Map#containsKey "hoge"
      """.stripMargin -> BoxedBoolean(false)
    )

    expectations.foreach{ case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }

  describe("containsValue") {
    val expectations: List[(String, Value)] = List(
      """
        |%["name": "Kota Mizushima" "age": "33"] Map#containsValue "33"
      """.stripMargin -> BoxedBoolean(true),
      """
        |%["name": "Kota Mizushima" "age": "33"] Map#containsValue "Kota Mizushima"
      """.stripMargin -> BoxedBoolean(true),
      """
        |%["name": "Kota Mizushima" "age": "33"] Map#containsValue "hoge"
      """.stripMargin -> BoxedBoolean(false)
    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }

  describe("get") {
    val expectations: List[(String, Value)] = List(
      """
        |%["name": "Kota Mizushima" "age": "33"] Map#get "age"
      """.stripMargin -> ObjectValue("33"),
      """
        |%["name": "Kota Mizushima" "age": "33"] Map#get "name"
      """.stripMargin -> ObjectValue("Kota Mizushima"),
      """
        |%["name": "Kota Mizushima" "age": "33"] Map#get "hoge"
      """.stripMargin -> ObjectValue(null)
    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }

  describe("isEmpty") {
    expect("empty map should be isEmpty")(
      """
        |Map#isEmpty(%[])
      """.stripMargin -> BoxedBoolean(true)
    )
    expect("non empty map should not be isEmpty")(
      """
        |Map#isEmpty(%["x": 1 "y": 2])
      """.stripMargin -> BoxedBoolean(false)
    )
  }
}
