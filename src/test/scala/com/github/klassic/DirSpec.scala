package com.github.klassic

import java.nio.file.{Files, Paths}

class DirSpec extends SpecHelper {
  describe("Dir module") {
    it("should get current, home, and temp directories") {
      E("Dir#current()")
      E("Dir#home()")
      E("Dir#temp()")
    }

    it("should create and check directories") {
      val testDir = "test-dir"
      try {
        assertResult(Value.boxBoolean(false))(
          E(s"""Dir#exists("$testDir")""")
        )
        
        E(s"""Dir#mkdir("$testDir")""")
        
        assertResult(Value.boxBoolean(true))(
          E(s"""Dir#exists("$testDir")""")
        )
        
        assertResult(Value.boxBoolean(true))(
          E(s"""Dir#isDirectory("$testDir")""")
        )
        
        assertResult(Value.boxBoolean(false))(
          E(s"""Dir#isFile("$testDir")""")
        )
      } finally {
        Files.deleteIfExists(Paths.get(testDir))
      }
    }

    it("should create nested directories") {
      val testDir = "test-parent/test-child/test-grandchild"
      try {
        E(s"""Dir#mkdirs("$testDir")""")
        
        assert(Files.exists(Paths.get("test-parent")))
        assert(Files.exists(Paths.get("test-parent/test-child")))
        assert(Files.exists(Paths.get(testDir)))
      } finally {
        Files.deleteIfExists(Paths.get(testDir))
        Files.deleteIfExists(Paths.get("test-parent/test-child"))
        Files.deleteIfExists(Paths.get("test-parent"))
      }
    }

    it("should list directory contents") {
      val testDir = "test-list-dir"
      try {
        Files.createDirectory(Paths.get(testDir))
        Files.writeString(Paths.get(s"$testDir/file1.txt"), "content1")
        Files.writeString(Paths.get(s"$testDir/file2.txt"), "content2")
        Files.createDirectory(Paths.get(s"$testDir/subdir"))
        
        val files = E(s"""Dir#list("$testDir")""").asInstanceOf[ObjectValue]
          .value.asInstanceOf[java.util.List[_]]
        
        assert(files.size() == 3)
        assert(files.contains("file1.txt"))
        assert(files.contains("file2.txt"))
        assert(files.contains("subdir"))
      } finally {
        Files.deleteIfExists(Paths.get(s"$testDir/file1.txt"))
        Files.deleteIfExists(Paths.get(s"$testDir/file2.txt"))
        Files.deleteIfExists(Paths.get(s"$testDir/subdir"))
        Files.deleteIfExists(Paths.get(testDir))
      }
    }

    it("should copy files") {
      val sourceFile = "test-source.txt"
      val targetFile = "test-target.txt"
      try {
        Files.writeString(Paths.get(sourceFile), "test content")
        
        E(s"""Dir#copy("$sourceFile", "$targetFile")""")
        
        assert(Files.exists(Paths.get(targetFile)))
        assert(Files.readString(Paths.get(targetFile)) == "test content")
      } finally {
        Files.deleteIfExists(Paths.get(sourceFile))
        Files.deleteIfExists(Paths.get(targetFile))
      }
    }

    it("should move files") {
      val sourceFile = "test-move-source.txt"
      val targetFile = "test-move-target.txt"
      try {
        Files.writeString(Paths.get(sourceFile), "move content")
        
        E(s"""Dir#move("$sourceFile", "$targetFile")""")
        
        assert(!Files.exists(Paths.get(sourceFile)))
        assert(Files.exists(Paths.get(targetFile)))
        assert(Files.readString(Paths.get(targetFile)) == "move content")
      } finally {
        Files.deleteIfExists(Paths.get(sourceFile))
        Files.deleteIfExists(Paths.get(targetFile))
      }
    }

    it("should work with FileOutput module") {
      val testDir = "test-io-dir"
      val testFile = s"$testDir/test.txt"
      try {
        E(s"""
          Dir#mkdir("$testDir")
          FileOutput#write("$testFile", "Hello from Dir!")
          val isFile = Dir#isFile("$testFile")
          val content = FileInput#all("$testFile")
          assert(isFile)
          assert(content == "Hello from Dir!")
        """.stripMargin)
      } finally {
        Files.deleteIfExists(Paths.get(testFile))
        Files.deleteIfExists(Paths.get(testDir))
      }
    }
  }
}