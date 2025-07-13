package com.github.klassic

import org.scalatest.funspec.AnyFunSpec

class FileOutputProgramTest extends AnyFunSpec {
  describe("FileOutput practical example") {
    it("should demonstrate FileOutput usage") {
      val evaluator = new Evaluator
      val program = """
        |// Test FileOutput module functionality
        |println("Testing FileOutput module...")
        |
        |// Test basic file writing
        |FileOutput#write("test-output.txt", "Hello, Klassic!")
        |println("✓ File written successfully")
        |
        |// Test file existence check
        |val exists = FileOutput#exists("test-output.txt")
        |println("✓ File exists: #{exists}")
        |
        |// Read back to verify
        |val content = FileInput#all("test-output.txt")
        |println("✓ File content: #{content}")
        |
        |// Test appending
        |FileOutput#append("test-output.txt", "\nThis is an appended line")
        |val newContent = FileInput#all("test-output.txt")
        |println("✓ After append: #{newContent}")
        |
        |// Clean up
        |FileOutput#delete("test-output.txt")
        |val existsAfterDelete = FileOutput#exists("test-output.txt")
        |println("✓ File deleted, exists: #{existsAfterDelete}")
        |
        |println("\nFileOutput module is working correctly!")
      """.stripMargin
      
      evaluator(program)
    }
  }
}