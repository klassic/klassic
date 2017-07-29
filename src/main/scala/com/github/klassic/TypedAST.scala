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

  case class Program(val location: Location, imports: List[Import], block: Block, records: RecordEnvironment)

  case class Import(val location: Location, simpleName: String, fqcn: String)

  case class Block(val type_ : Type, val location: Location, expressions: List[TypedAST]) extends TypedAST

  case class IfExpression(val type_ : Type, val location: Location, condition: TypedAST, thenExpression: TypedAST, elseExpression: TypedAST) extends TypedAST

  case class ForeachExpression(val type_ : Type, val location: Location, name: String, collection: TypedAST, body: TypedAST) extends TypedAST

  case class BinaryExpression(val type_ : Type, val location: Location, operator: Operator, lhs: TypedAST, rhs: TypedAST) extends TypedAST

  case class WhileExpression(val type_ : Type, val location: Location, condition: TypedAST, body: TypedAST) extends TypedAST

  case class MinusOp(val type_ : Type, val location: Location, operand: TypedAST) extends TypedAST

  case class PlusOp(val type_ : Type, val location: Location, operand: TypedAST) extends TypedAST

  case class StringNode(val type_ : Type, val location: Location, value: String) extends TypedAST

  case class IntNode(val type_ : Type, val location: Location, value: Int) extends TypedAST

  case class LongNode(val type_ : Type, val location: Location, value: Long) extends TypedAST

  case class ShortNode(val type_ : Type, val location: Location, value: Short) extends TypedAST

  case class ByteNode(val type_ : Type, val location: Location, value: Byte) extends TypedAST

  case class BooleanNode(val type_ : Type, val location: Location, value: Boolean) extends TypedAST

  case class DoubleNode(val type_ : Type, val location: Location, value: Double) extends TypedAST

  case class FloatNode(val type_ : Type, val location: Location, value: Float) extends TypedAST

  case class Id(val type_ : Type, val location: Location, name: String) extends TypedAST

  case class Selector(val type_ : Type, val location: Location, module: String, name: String) extends TypedAST

  case class Assignment(val type_ : Type, val location: Location, variable: String, value: TypedAST) extends TypedAST

  case class LetDeclaration(val type_ : Type, val location: Location, variable: String, declaredType: Type, value: TypedAST, body: TypedAST, immutable: Boolean) extends TypedAST

  case class FunctionLiteral(val type_ : Type, val location: Location, params: List[FormalParameterOptional], optionalType: Option[Type], proc: TypedAST) extends TypedAST {
    override def toString: String = s"""${params.mkString(", ")} => ${optionalType.getOrElse("?")}"""
  }

  case class LetFunctionDefinition(val type_ : Type, val location: Location, name: String, body: FunctionLiteral, cleanup: Option[TypedAST], expression: TypedAST) extends TypedAST

  case class FunctionCall(val type_ : Type, val location: Location, func: TypedAST, params: List[TypedAST]) extends TypedAST

  case class ListLiteral(val type_ : Type, val location: Location, elements: List[TypedAST]) extends TypedAST

  case class SetLiteral(val type_ : Type, val location: Location, elements: List[TypedAST]) extends TypedAST

  case class MapLiteral(val type_ : Type, val location: Location, elements: List[(TypedAST, TypedAST)]) extends TypedAST

  case class RecordSelect(val type_ : Type, val location: Location, expression: TypedAST, member: String) extends TypedAST

  case class ObjectNew(val type_ : Type, val location: Location, className: String, params: List[TypedAST]) extends TypedAST

  case class RecordNew(val type_ : Type, val location: Location, recordName: String, params: List[TypedAST]) extends TypedAST

  case class MethodCall(val type_ : Type, val location: Location, self: TypedAST, name: String, params: List[TypedAST]) extends TypedAST

  case class Casting(val type_ : Type, val location: Location, target: TypedAST, to: Type) extends TypedAST

  case class ValueNode(value: Value) extends TypedAST {
    override val type_   : Type     = TDynamic
    override val location: Location = NoLocation
  }
}