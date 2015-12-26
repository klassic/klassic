package com.github.kmizu.klassic

package object ast {
  sealed abstract class AstNode {
    def location: Location
  }
  case class Location(line: Int, column: Int)
}
