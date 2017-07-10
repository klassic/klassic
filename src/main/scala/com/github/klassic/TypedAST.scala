package com.github.klassic

import com.github.klassic.Type.{DynamicType, TypeScheme, TypeVariable}

/**
 * @author Kota Mizushima
 */

sealed abstract class TypedAST {
  val location: Location
  val description: Type
}

object TypedAST {
  type RecordEnvironment = Map[String, (List[TypeVariable], List[(String, TypeScheme)])]

  sealed trait IntegerSuffix

  case object ByteSuffix extends IntegerSuffix

  case object ShortSuffix extends IntegerSuffix

  case object LongSuffix extends IntegerSuffix

  sealed trait FloatSuffix

  case object FloatSuffix extends FloatSuffix

  case class Program(val location: Location, imports: List[Import], block: Block, records: RecordEnvironment)

  case class Import(val location: Location, simpleName: String, fqcn: String)

  case class Block(val description: Type, val location: Location, expressions: List[TypedAST]) extends TypedAST

  case class IfExpression(val description: Type, val location: Location, condition: TypedAST, thenExpression: TypedAST, elseExpression: TypedAST) extends TypedAST

  case class ForeachExpression(val description: Type, val location: Location, name: String, collection: TypedAST, body: TypedAST) extends TypedAST

  case class BinaryExpression(val description: Type, val location: Location, operator: Operator, lhs: TypedAST, rhs: TypedAST) extends TypedAST

  case class WhileExpression(val description: Type, val location: Location, condition: TypedAST, body: TypedAST) extends TypedAST

  case class MinusOp(val description: Type, val location: Location, operand: TypedAST) extends TypedAST

  case class PlusOp(val description: Type, val location: Location, operand: TypedAST) extends TypedAST

  case class StringNode(val description: Type, val location: Location, value: String) extends TypedAST

  case class IntNode(val description: Type, val location: Location, value: Int) extends TypedAST

  case class LongNode(val description: Type, val location: Location, value: Long) extends TypedAST

  case class ShortNode(val description: Type, val location: Location, value: Short) extends TypedAST

  case class ByteNode(val description: Type, val location: Location, value: Byte) extends TypedAST

  case class BooleanNode(val description: Type, val location: Location, value: Boolean) extends TypedAST

  case class DoubleNode(val description: Type, val location: Location, value: Double) extends TypedAST

  case class FloatNode(val description: Type, val location: Location, value: Float) extends TypedAST

  case class Id(val description: Type, val location: Location, name: String) extends TypedAST

  case class Selector(val description: Type, val location: Location, module: String, name: String) extends TypedAST

  case class Assignment(val description: Type, val location: Location, variable: String, value: TypedAST) extends TypedAST

  case class LetDeclaration(val description: Type, val location: Location, variable: String, declaredType: Type, value: TypedAST, body: TypedAST, immutable: Boolean) extends TypedAST

  case class FunctionLiteral(val description: Type, val location: Location, params: List[FormalParameterOptional], optionalType: Option[Type], proc: TypedAST) extends TypedAST {
    override def toString: String = s"""${params.mkString(", ")} => ${optionalType.getOrElse("?")}"""
  }

  case class LetFunctionDefinition(val description: Type, val location: Location, name: String, body: FunctionLiteral, cleanup: Option[TypedAST], expression: TypedAST) extends TypedAST

  case class FunctionCall(val description: Type, val location: Location, func: TypedAST, params: List[TypedAST]) extends TypedAST

  case class ListLiteral(val description: Type, val location: Location, elements: List[TypedAST]) extends TypedAST

  case class SetLiteral(val description: Type, val location: Location, elements: List[TypedAST]) extends TypedAST

  case class MapLiteral(val description: Type, val location: Location, elements: List[(TypedAST, TypedAST)]) extends TypedAST

  case class AccessRecord(val description: Type, val location: Location, expression: TypedAST, member: String) extends TypedAST

  case class NewObject(val description: Type, val location: Location, className: String, params: List[TypedAST]) extends TypedAST

  case class NewRecord(val description: Type, val location: Location, recordName: String, params: List[TypedAST]) extends TypedAST

  case class MethodCall(val description: Type, val location: Location, self: TypedAST, name: String, params: List[TypedAST]) extends TypedAST

  case class Casting(val description: Type, val location: Location, target: TypedAST, to: Type) extends TypedAST

  case class ValueNode(value: Value) extends TypedAST {
    override val description: Type  = DynamicType
    override val location: Location = NoLocation
  }
}