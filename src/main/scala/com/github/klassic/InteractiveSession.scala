package com.github.klassic

class InteractiveSession {
  var typeEnvironment: Option[TypeEnvironment] = None
  var runtimeEnvironment: Option[RuntimeEnvironment] = None
  var recordEnvironment: Option[RecordEnvironment]  = None
  var moduleEnvironment: Option[ModuleEnvironment] = None
}
