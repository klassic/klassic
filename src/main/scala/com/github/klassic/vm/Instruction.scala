package com.github.klassic.vm

import com.github.klassic.Value

sealed trait Instruction
case class Push(value: Value) extends Instruction
case class Load(name: String) extends Instruction
case class Store(name: String) extends Instruction
case class Assign(name: String) extends Instruction
case object Add extends Instruction
case object Sub extends Instruction
case object Mul extends Instruction
case object Div extends Instruction
case object Neg extends Instruction
case object Equal extends Instruction
case object LessThan extends Instruction
case object LessOrEqual extends Instruction
case object GreaterThan extends Instruction
case object GreaterOrEqual extends Instruction
case object And extends Instruction
case object Or extends Instruction
case object Xor extends Instruction
case object And2 extends Instruction
case object Or2 extends Instruction
case class Jump(target: Int) extends Instruction
case class JumpIfFalse(target: Int) extends Instruction
case class Call(arity: Int) extends Instruction
case class CallMethod(name: String, arity: Int) extends Instruction
case class NewObject(className: String, arity: Int) extends Instruction
case class NewRecord(recordName: String, arity: Int) extends Instruction
case class GetField(fieldName: String) extends Instruction
case class MakeList(size: Int) extends Instruction
case class MakeSet(size: Int) extends Instruction
case class MakeMap(size: Int) extends Instruction
case class MakeClosure(params: List[String], bodyStart: Int, bodyEnd: Int) extends Instruction
case class LoadModule(module: String, name: String) extends Instruction
case object Pop extends Instruction
case object Dup extends Instruction
case object Return extends Instruction
