package com.github.klassic.vm

import com.github.klassic.Value

sealed trait Instruction
case class Push(value: Value) extends Instruction
case class Load(name: String) extends Instruction
case class Store(name: String) extends Instruction
case object Add extends Instruction
case object Sub extends Instruction
case object Mul extends Instruction
case object Div extends Instruction
case object Neg extends Instruction
case class Jump(target: Int) extends Instruction
case class JumpIfFalse(target: Int) extends Instruction
case object Return extends Instruction
