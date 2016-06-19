package com.github.klassic

import java.io.File

import scala.collection.Iterator.continually

/**
 * @author Kota Mizushima
 */
object Main {
  def main(args: Array[String]): Unit = {
    val interpreter = new Interpreter
    parseCommandLine(args) match {
      case Some(("-e", line)) =>
        println(interpreter.evaluateString(line))
      case Some(("-f", fileName)) =>
        interpreter.evaluateFile(new File(fileName))
      case _ =>
        Console.err.println(
          """
            |Usage: java -jar klassic.jar (-f <fileName> | -e <expression>)
            |-f <fileName>   : read a program from <fileName> and execute it
            |-e <expression> : evaluate <expression>
          """.stripMargin)
    }
  }

  def parseCommandLine(args: Array[String]): Option[(String, String)] = {
    val parser = new Parser
    args match {
      case Array("-e", line) => Some("-e" -> line)
      case Array(fileName) => Some("-f"-> fileName)
      case otherwise  => None
    }
  }
}

