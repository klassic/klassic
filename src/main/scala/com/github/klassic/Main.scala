package com.github.klassic

import java.io.File

import scala.collection.Iterator.continually

/**
 * @author Kota Mizushima
 */
object Main {
  def main(args: Array[String]): Unit = {
    val evaluator = new Evaluator
    parseCommandLine(args) match {
      case Some(("-e", line)) =>
        println(evaluator.evaluateString(line))
      case Some(("-f", fileName)) =>
        evaluator.evaluateFile(new File(fileName))
      case _ =>
        Console.err.println(
          """
            |Usage: java -jar klassic.jar (-f <fileName> | -e <expression>)
            |<fileName>   : read a program from <fileName> and execute it
            |-e <expression> : evaluate <expression>
          """.stripMargin)
    }
  }

  def parseCommandLine(args: Array[String]): Option[(String, String)] = {
    val parser = new Parser
    args match {
      case Array(fileName) if fileName.endsWith("kl") => Some("-f"-> fileName)
      case Array("-e", line) => Some("-e" -> line)
      case otherwise  => None
    }
  }
}

