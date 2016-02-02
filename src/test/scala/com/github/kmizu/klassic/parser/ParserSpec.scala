package com.github.kmizu.klassic.parser

import fastparse.core.Parsed
import org.scalatest.FunSpec
import com.github.kmizu.klassic.Ast
import Parser._
import fastparse.all._

class ParserSpec extends FunSpec {
  describe("Parse integer") {
    it("0, 1, 2, 3") {
      assert(integer.parse("0").get.value == Ast.IntValue(0))
      assert(integer.parse("1").get.value == Ast.IntValue(1))
      assert(integer.parse("2").get.value == Ast.IntValue(2))
      assert(integer.parse("3").get.value == Ast.IntValue(3))
    }
    it("00, 01, 02, 03") {
      assert(integer.parse("00").get.value == Ast.IntValue(0))
      assert(integer.parse("01").get.value == Ast.IntValue(1))
      assert(integer.parse("02").get.value == Ast.IntValue(2))
      assert(integer.parse("03").get.value == Ast.IntValue(3))
    }
    it("-0, -1, -2, -3") {
      assert(integer.parse("-0").get.value == Ast.IntValue(0))
      assert(integer.parse("-1").get.value == Ast.IntValue(-1))
      assert(integer.parse("-2").get.value == Ast.IntValue(-2))
      assert(integer.parse("-3").get.value == Ast.IntValue(-3))
    }
    it("+0, +1, +2, +3") {
      assert(integer.parse("+0").get.value == Ast.IntValue(0))
      assert(integer.parse("+1").get.value == Ast.IntValue(1))
      assert(integer.parse("+2").get.value == Ast.IntValue(2))
      assert(integer.parse("+3").get.value == Ast.IntValue(3))
    }
  }
}