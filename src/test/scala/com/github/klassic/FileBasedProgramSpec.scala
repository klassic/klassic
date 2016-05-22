package com.github.klassic

import java.io.{File, FileFilter}

class FileBasedProgramSpec extends SpecHelper {
  val I = new Interpreter
  val directory = new File("test-programs")
  describe(s"run Klassic programs under ${directory}") {
    for(program <- directory.listFiles(new FileFilter {
      override def accept(file: File): Boolean = file.getName.endsWith(".kl")
    })) {
      it(s"program ${program} runs successfully") {
        I.evaluateFile(program)
      }
    }
  }
}
