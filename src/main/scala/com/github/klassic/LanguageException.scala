package com.github.klassic

abstract class LanguageException(message: String) extends Exception(message)
case class InterpreterException(message: String) extends LanguageException(message)
case class InterpreterPanic(message: String) extends LanguageException("[PANIC]:" + message)
