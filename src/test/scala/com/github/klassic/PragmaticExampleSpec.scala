package com.github.klassic

import java.nio.file.{Files, Paths}

class PragmaticExampleSpec extends SpecHelper {
  describe("Pragmatic Klassic Example") {
    it("should demonstrate file processing with new features") {
      val program = """
        // A pragmatic example: Process a CSV file
        
        // Create a test CSV file
        val csvContent = "Name,Age,City\nAlice,30,NYC\nBob,25,LA\nCarol,35,Chicago"
        FileOutput#write("test-data.csv", csvContent)
        
        // Read and process the CSV
        val lines = FileInput#lines("test-data.csv")
        val header = head(lines)
        val data = tail(lines)
        
        // Process each row
        val results = map(data)((row) => {
          val fields = split(row, ",")
          val name = head(fields)
          val age = head(tail(fields))
          val city = head(tail(tail(fields)))
          
          // Create formatted output
          toUpperCase(name) + " is " + age + " years old and lives in " + city
        })
        
        // Write results to a new file
        FileOutput#writeLines("test-output.txt", results)
        
        // Verify the output exists
        val outputExists = FileOutput#exists("test-output.txt")
        assert(outputExists)
        
        // Clean up
        FileOutput#delete("test-data.csv")
        FileOutput#delete("test-output.txt")
        
        "Processing completed"
      """.stripMargin
      
      assertResult(E(program))(ObjectValue("Processing completed"))
    }
    
    it("should demonstrate directory operations") {
      val program = """
        // Create a project structure
        val projectName = "my-project"
        Dir#mkdir(projectName)
        Dir#mkdir(projectName + "/src")
        Dir#mkdir(projectName + "/tests")
        
        // Create some files
        FileOutput#write(projectName + "/README.md", "# My Klassic Project")
        FileOutput#write(projectName + "/src/main.kl", "println(\"Hello from Klassic!\")")
        FileOutput#write(projectName + "/tests/test.kl", "assert(1 + 1 == 2)")
        
        // List project contents
        val files = Dir#list(projectName)
        assert(size(files) == 3)
        
        // Check file types
        val readmeIsFile = Dir#isFile(projectName + "/README.md")
        val srcIsDir = Dir#isDirectory(projectName + "/src")
        assert(readmeIsFile && srcIsDir)
        
        // Clean up
        FileOutput#delete(projectName + "/tests/test.kl")
        FileOutput#delete(projectName + "/src/main.kl")
        FileOutput#delete(projectName + "/README.md")
        Dir#delete(projectName + "/tests")
        Dir#delete(projectName + "/src")
        Dir#delete(projectName)
        
        "Project structure demo completed"
      """.stripMargin
      
      assertResult(E(program))(ObjectValue("Project structure demo completed"))
    }
    
    it("should demonstrate string processing") {
      val program = """
        // Parse configuration-like data
        val config = "database.host = localhost\ndatabase.port = 5432\ndatabase.name = myapp"
        val lines = split(config, "\n")
        
        val parsed = map(lines)((line) => {
          val parts = split(trim(line), " = ")
          val key = head(parts)
          val value = head(tail(parts))
          key + " -> " + value
        })
        
        // Create output
        val output = join(parsed, ", ")
        assert(contains(output, "database.host -> localhost"))
        assert(contains(output, "5432"))
        
        output
      """.stripMargin
      
      val result = E(program).asInstanceOf[ObjectValue].value.asInstanceOf[String]
      assert(result.contains("database.host -> localhost"))
      assert(result.contains("database.port -> 5432"))
    }
  }
}