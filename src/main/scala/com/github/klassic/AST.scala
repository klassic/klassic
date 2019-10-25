package com.github.klassic

import com.github.klassic.Type.TVariable

/**
 * @author Kota Mizushima
 */

sealed abstract class AST {
  val location: Location
}

object AST {

  sealed trait IntegerSuffix

  case object ByteSuffix extends IntegerSuffix

  case object ShortSuffix extends IntegerSuffix

  case object LongSuffix extends IntegerSuffix

  sealed trait FloatSuffix

  case object FloatSuffix extends FloatSuffix

  case class Program(location: Location, grammar: Option[macro_peg.Ast.Grammar], imports: List[Import], records: List[RecordDeclaration], block: Block)

  case class Import(location: Location, simpleName: String, fqcn: String)

  case class RecordDeclaration(location: Location, name: String, ts: List[TVariable], members: List[(String, Type)], methods: List[MethodDefinition])

  case class EnumDeclaration(location: Location, id: String, params: List[Type], constructors: List[DataConstructor]) extends AST

  case class Block(location: Location, expressions: List[AST]) extends AST

  case class IfExpression(location: Location, condition: AST, thenExpression: AST, elseExpression: AST) extends AST

  case class ForeachExpression(location: Location, name: String, collection: AST, body: AST) extends AST

  case class BinaryExpression(location: Location, operator: Operator, lhs: AST, rhs: AST) extends AST

  case class WhileExpression(location: Location, condition: AST, body: AST) extends AST

  case class MinusOp(location: Location, operand: AST) extends AST

  case class PlusOp(location: Location, operand: AST) extends AST

  case class StringNode(location: Location, value: String) extends AST

  case class IntNode(location: Location, value: Int) extends AST

  case class LongNode(location: Location, value: Long) extends AST

  case class ShortNode(location: Location, value: Short) extends AST

  case class ByteNode(location: Location, value: Byte) extends AST

  case class BooleanNode(location: Location, value: Boolean) extends AST

  case class DoubleNode(location: Location, value: Double) extends AST

  case class FloatNode(location: Location, value: Float) extends AST

  case class Id(location: Location, name: String) extends AST
  object Id {
    def apply(name: String): Id = Id(NoLocation, name)
  }
  case class Placeholder(location: Location) extends AST
  object Placeholder{
    def apply(): Placeholder= Placeholder(NoLocation)
  }

  case class Selector(location: Location, module: String, name: String) extends AST

  sealed abstract class Assignment extends AST {
    val location: Location
    val variable: String
    val value: AST
  }

  case class SimpleAssignment(location: Location, variable: String, value: AST) extends Assignment

  case class PlusAssignment(location: Location, variable: String, value: AST) extends Assignment

  case class MinusAssignment(location: Location, variable: String, value: AST) extends Assignment

  case class MultiplicationAssignment(location: Location, variable: String, value: AST) extends Assignment

  case class DivisionAssignment(location: Location, variable: String, value: AST) extends Assignment

  case class ValDeclaration(location: Location, variable: String, type_ : Option[Type], value: AST, immutable: Boolean) extends AST

  case class EnumIn(location: Location, variant: EnumDeclaration, body: AST) extends AST

  case class RecordSelect(location: Location, record: AST, label: String) extends AST

  case class RecordCall(location: Location, self: AST, name: String, params: List[AST]) extends AST

  case class Let(location: Location, variable: String, type_ : Option[Type], value: AST, body: AST, immutable: Boolean) extends AST
  object Let {
    def apply(variable: String, type_ :Option[Type], value: AST, body: AST): Let = {
      Let(NoLocation, variable, type_, value, body, true)
    }
  }

  case class Lambda(location: Location, params: List[FormalParameterOptional], optionalType: Option[Type], body: AST) extends AST
  object Lambda {
    def apply(params: List[String], body: AST): Lambda = {
      Lambda(NoLocation, params.map{ case name => FormalParameterOptional(name, None)}, None, body)
    }
  }

  case class FunctionDefinition(location: Location, name: String, body: Lambda, cleanup: Option[AST]) extends AST

  case class MethodDefinition(location: Location, name: String, body: Lambda, cleanup: Option[AST]) extends AST

  case class LetRec(location: Location, name: String, function: Lambda, cleanup: Option[AST], body: AST) extends AST
  object LetRec {
    def apply(name: String, function: Lambda, body: AST): LetRec = {
      LetRec(NoLocation, name, function, None, body)
    }
  }

  case class FunctionCall(location: Location, func: AST, params: List[AST]) extends AST

  case class ListLiteral(location: Location, elements: List[AST]) extends AST

  case class SetLiteral(location: Location, elements: List[AST]) extends AST

  case class MapLiteral(location: Location, elements: List[(AST, AST)]) extends AST

  case class ObjectNew(location: Location, className: String, params: List[AST]) extends AST

  case class RecordNew(location: Location, recordName: String, params: List[AST]) extends AST

  case class MethodCall(location: Location, self: AST, name: String, params: List[AST]) extends AST

  case class Casting(location: Location, target: AST, to: Type) extends AST
}