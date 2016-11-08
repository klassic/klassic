package com.github.klassic

sealed abstract class Location {
  def format: String
}
case class SourceLocation(line: Int, column: Int) extends Location {
  def format: String = s"${line}, ${column} :"
}
case object NoLocation extends Location {
  def format: String = s"? :"
}
