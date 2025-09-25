package com.github.klassic

import java.nio.file.{Files, Paths}

class FileOutputSpec extends SpecHelper {
  describe("FileOutput module") {
    it("should write content to a file") {
      val testFile = "test-file-write.txt"
      try {
        E(s"""FileOutput#write("$testFile", "Hello, World!")""")
        assert(Files.exists(Paths.get(testFile)))
        assert(Files.readString(Paths.get(testFile)) == "Hello, World!")
      } finally {
        Files.deleteIfExists(Paths.get(testFile))
      }
    }

    it("should append content to a file") {
      val testFile = "test-file-append.txt"
      try {
        E(s"""
          FileOutput#write("$testFile", "Line 1")
          FileOutput#append("$testFile", "\nLine 2")
        """.stripMargin)
        assert(Files.readString(Paths.get(testFile)) == "Line 1\nLine 2")
      } finally {
        Files.deleteIfExists(Paths.get(testFile))
      }
    }

    it("should check file existence") {
      val testFile = "test-file-exists.txt"
      assertResult(Value.boxBoolean(false))(
        E(s"""FileOutput#exists("$testFile")""")
      )
      
      Files.writeString(Paths.get(testFile), "test")
      try {
        assertResult(Value.boxBoolean(true))(
          E(s"""FileOutput#exists("$testFile")""")
        )
      } finally {
        Files.deleteIfExists(Paths.get(testFile))
      }
    }

    it("should delete a file") {
      val testFile = "test-file-delete.txt"
      Files.writeString(Paths.get(testFile), "test")
      assert(Files.exists(Paths.get(testFile)))
      
      E(s"""FileOutput#delete("$testFile")""")
      assert(!Files.exists(Paths.get(testFile)))
    }

    it("should write multiple lines") {
      val testFile = "test-file-lines.txt"
      try {
        E(s"""
          val lines = ["Line 1", "Line 2", "Line 3"]
          FileOutput#writeLines("$testFile", lines)
        """.stripMargin)
        
        val content = Files.readString(Paths.get(testFile))
        assert(content.trim == "Line 1\nLine 2\nLine 3")
      } finally {
        Files.deleteIfExists(Paths.get(testFile))
      }
    }

    it("should work with FileInput module") {
      val testFile = "test-file-io.txt"
      try {
        E(s"""
          FileOutput#write("$testFile", "Test content")
          val content = FileInput#all("$testFile")
          assert(content == "Test content")
        """.stripMargin)
      } finally {
        Files.deleteIfExists(Paths.get(testFile))
      }
    }
  }
}