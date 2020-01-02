package com.github.klassic

abstract class Processor[-In, +Out, -Session] {
  def name: String
  def process(input: In, session: Session): Out
}
