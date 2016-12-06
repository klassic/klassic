package com.github.klassic.runtime

import com.github.klassic.annotation.ATypeScheme
import com.github.klassic.{BoxedBoolean, NativeFunctionValue, ObjectValue, Value}

object Prelude {
  @ATypeScheme(tvs = Array[String](), ty = "(*, *) => Boolean")
  val startsWith: Value = NativeFunctionValue{ case List(ObjectValue(self: String), ObjectValue(prefix: String)) =>
    BoxedBoolean(self.startsWith(prefix))
  }
}
