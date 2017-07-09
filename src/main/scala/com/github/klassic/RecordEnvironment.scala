package com.github.klassic

import scala.collection.mutable
import scala.util.DynamicVariable

class RecordEnvironment(val records: mutable.Map[String, List[(String, Type)]] = mutable.Map.empty) {
  def define(name: String)(members: (String, Type)*): Unit = {
    records += (name -> members.toList)
  }
  def get(recordName: String): Option[List[(String, Type)]] = {
    records.get(recordName)
  }
  override def toString: String = s"RecordEnvironment(${records})"
}
