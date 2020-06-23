package com.github.klassic

import java.util.Arrays

/**
  * Created by Mizushima on 2016/05/30.
  */
class FileInputSpec extends SpecHelper {
  describe("FileInput") {
    it("open() and readAll() can read a file") {
      val path = classOf[FileInputSpec].getClassLoader.getResource("hello.txt").getPath
      assertResult(
        ObjectValue("Hello, World!")
      )(
        E(
          s"""
            |"${path}" FileInput#open {(stream) =>
            |  FileInput#readAll(stream)
            |}
          """.stripMargin)
      )
    }
    it("open() and readLines() can read a file and return it as a list") {
      val path = classOf[FileInputSpec].getClassLoader.getResource("hello.txt").getPath
      assertResult(
        ObjectValue(Arrays.asList("Hello, World!"))
      )(
        E(
          s"""
             |"${path}" FileInput#open {(stream) =>
             |  FileInput#readLines(stream)
             |}
          """.stripMargin)
      )
    }
    it("all() can read a file") {
      val path = classOf[FileInputSpec].getClassLoader.getResource("hello.txt").getPath
      assertResult(
        ObjectValue("Hello, World!")
      )(
        E(
          s"""
             |FileInput#all("${path}")
          """.stripMargin)
      )
    }
    it("lines() can read a file and return it as a list") {
      val path = classOf[FileInputSpec].getClassLoader.getResource("hello.txt").getPath
      assertResult(
        ObjectValue(Arrays.asList("Hello, World!"))
      )(
        E(
          s"""
             |FileInput#lines("${path}")
          """.stripMargin)
      )

    }
  }
}
