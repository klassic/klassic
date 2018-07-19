package com.github.klassic

import com.github.klassic.Type.{TDynamic, TRecord, TScheme, TVariable}

/**
 * @author Kota Mizushima
 */

sealed abstract class TypedAST {
  val location: Location
  val type_   : Type
}

object TypedAST {
  type RecordEnvironment = Map[String, TRecord]

  sealed trait IntegerSuffix

  case object ByteSuffix extends IntegerSuffix

  case object ShortSuffix extends IntegerSuffix

  case object LongSuffix extends IntegerSuffix

  sealed trait FloatSuffix

  case object FloatSuffix extends FloatSuffix

  case class Program(location: Location, imports: List[Import], block: Block, records: RecordEnvironment)

  case class Import(location: Location, simpleName: String, fqcn: String)

  case class Block(type_ : Type, location: Location, expressions: List[TypedAST]) extends TypedAST

  case class IfExpression(type_ : Type, location: Location, condition: TypedAST, thenExpression: TypedAST, elseExpression: TypedAST) extends TypedAST

  case class ForeachExpression(type_ : Type, location: Location, name: String, collection: TypedAST, body: TypedAST) extends TypedAST

  case class BinaryExpression(type_ : Type, location: Location, operator: Operator, lhs: TypedAST, rhs: TypedAST) extends TypedAST

  case class WhileExpression(type_ : Type, location: Location, condition: TypedAST, body: TypedAST) extends TypedAST

  case class MinusOp(type_ : Type, location: Location, operand: TypedAST) extends TypedAST

  case class PlusOp(type_ : Type, location: Location, operand: TypedAST) extends TypedAST

  case class StringNode(type_ : Type, location: Location, value: String) extends TypedAST

  case class IntNode(type_ : Type, location: Location, value: Int) extends TypedAST

  case class LongNode(type_ : Type, location: Location, value: Long) extends TypedAST

  case class ShortNode(type_ : Type, location: Location, value: Short) extends TypedAST

  case class ByteNode(type_ : Type, location: Location, value: Byte) extends TypedAST

  case class BooleanNode(type_ : Type, location: Location, value: Boolean) extends TypedAST

  case class DoubleNode(type_ : Type, location: Location, value: Double) extends TypedAST

  case class FloatNode(type_ : Type, location: Location, value: Float) extends TypedAST

  case class Id(type_ : Type, location: Location, name: String) extends TypedAST

  case class Selector(type_ : Type, location: Location, module: String, name: String) extends TypedAST

  case class Assignment(type_ : Type, location: Location, variable: String, value: TypedAST) extends TypedAST

  case class LetDeclaration(type_ : Type, location: Location, variable: String, declaredType: Type, value: TypedAST, body: TypedAST, immutable: Boolean) extends TypedAST

  case class FunctionLiteral(type_ : Type, location: Location, params: List[FormalParameterOptional], optionalType: Option[Type], proc: TypedAST) extends TypedAST {
    override def toString: String = s"""${params.mkString(", ")} => ${optionalType.getOrElse("?")}"""
  }

  case class LetFunctionDefinition(type_ : Type, location: Location, name: String, body: FunctionLiteral, cleanup: Option[TypedAST], expression: TypedAST) extends TypedAST

  case class FunctionCall(type_ : Type, location: Location, func: TypedAST, params: List[TypedAST]) extends TypedAST

  case class ListLiteral(type_ : Type, location: Location, elements: List[TypedAST]) extends TypedAST

  case class SetLiteral(type_ : Type, location: Location, elements: List[TypedAST]) extends TypedAST

  case class MapLiteral(type_ : Type, location: Location, elements: List[(TypedAST, TypedAST)]) extends TypedAST

  case class RecordSelect(type_ : Type, location: Location, expression: TypedAST, member: String) extends TypedAST

  case class ObjectNew(type_ : Type, location: Location, className: String, params: List[TypedAST]) extends TypedAST

  case class RecordNew(type_ : Type, location: Location, recordName: String, params: List[TypedAST]) extends TypedAST

  case class MethodCall(type_ : Type, location: Location, self: TypedAST, name: String, params: List[TypedAST]) extends TypedAST

  case class Casting(type_ : Type, location: Location, target: TypedAST, to: Type) extends TypedAST

  case class ValueNode(value: Value) extends TypedAST {
    override val type_   : Type     = TDynamic
    override val location: Location = NoLocation
  }
}