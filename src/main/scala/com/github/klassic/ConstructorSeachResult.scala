package com.github.klassic

import scala.language.existentials
import java.lang.reflect.Constructor

sealed abstract class ConstructorSearchResult
case class UnboxedVersionConstructorFound(constructor: Constructor[_]) extends ConstructorSearchResult
case class BoxedVersionConstructorFound(constructor: Constructor[_]) extends ConstructorSearchResult
case object NoConstructorFound extends ConstructorSearchResult
