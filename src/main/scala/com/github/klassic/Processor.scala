package com.github.klassic

abstract class Processor[-In, +Out] {
  def name: String
  def process(input: In): Out
}
