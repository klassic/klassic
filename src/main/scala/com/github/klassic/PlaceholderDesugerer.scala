package com.github.klassic

import com.github.klassic.AST.{FunctionCall, ListLiteral, MapLiteral, ObjectNew, _}
import com.github.klassic.Type.{TBoolean, TDynamic}

import scala.collection.mutable

/**
  * @author Kota Mizushima
  */
class PlaceholderDesugerer extends Processor[AST.Program, AST.Program] {
  private object PlaceholderManager {
    private[this] var counter: Int = 0
    private[this] def nameOf(index: Int): String = "$" + index

    def count: Int = counter
    def hasPlaceholder: Boolean = count > 0
    def placeholders: List[String] = (0 until counter).toList.map{i=> nameOf(i)}

    def generate(): String = {
      val newName = nameOf(counter)
      counter += 1
      newName
    }

    def reset(): Unit = {
      counter = 0
    }
  }
  private val manager = PlaceholderManager

  def doRewrite(node: AST): AST = node match {
    case Block(location, expressions) =>
      def rewriteBlock(es: List[AST]): List[AST] = es match {
        case (x@ValDeclaration(location, variable, type_, value, immutable)) :: xs =>
          x.copy(value = doRewrite(x.value)) :: rewriteBlock(xs)
        case (x@FunctionDefinition(loation, name, expression, cleanup)) :: xs =>
          x.copy(body = doRewrite(x.body).asInstanceOf[AST.Lambda], cleanup  = x.cleanup.map{doRewrite}) :: xs
        case x :: xs =>
          doRewrite(x) :: rewriteBlock(xs)
        case Nil =>
          Nil
      }
      Block(location, rewriteBlock(expressions))
    case IfExpression(location, cond, pos, neg) =>
      IfExpression(location, doRewrite(cond), doRewrite(pos), doRewrite(neg))
    case WhileExpression(location, condition, body: AST) =>
      WhileExpression(location, doRewrite(condition), doRewrite(body))
    case RecordSelect(location, expression, member) =>
      RecordSelect(location, doRewrite(expression), member)
    case RecordCall(location, self, name, params) =>
      RecordCall(location, doRewrite(self), name, params.map{doRewrite})
    case RecordNew(location, name, members) =>
      RecordNew(location, name, members.map(doRewrite))
    case e@ForeachExpression(location, name, collection, body) =>
      ForeachExpression(location, name, doRewrite(collection), doRewrite(body))
    case x@BinaryExpression(location, _, _, _) =>
      val xLhs = doRewrite(x.lhs)
      val xRhs = doRewrite(x.rhs)
      if(manager.hasPlaceholder) {
        val formalParameters = manager.placeholders.map{name => FormalParameterOptional(name, None)}
        val result = Lambda(location, formalParameters, None, x.copy(lhs = xLhs, rhs = xRhs))
        manager.reset()
        result
      } else {
        x.copy(lhs = xLhs, rhs = xRhs)
      }
    case x@MinusOp(location, operand) =>
      val xOperand = doRewrite(x.operand)
      if(manager.hasPlaceholder) {
        val formalParameters = manager.placeholders.map{name => FormalParameterOptional(name, None)}
        val result = Lambda(location, formalParameters, None, x.copy(operand = xOperand))
        manager.reset()
        result
      } else {
        x.copy(operand = xOperand)
      }
    case x@PlusOp(location, operand) =>
      val xOperand = doRewrite(x.operand)
      if(manager.hasPlaceholder) {
        val formalParameters = manager.placeholders.map{name => FormalParameterOptional(name, None)}
        val result = Lambda(location, formalParameters, None, x.copy(operand = xOperand))
        manager.reset()
        result
      } else {
        x.copy(operand = xOperand)
      }
    case literal@StringNode(location, value) =>
      literal
    case literal@IntNode(location, value) =>
      literal
    case literal@LongNode(location, value)  =>
      literal
    case literal@ShortNode(location, value) =>
      literal
    case literal@ByteNode(location, value) =>
      literal
    case literal@BooleanNode(location, value) =>
      literal
    case literal@DoubleNode(location, value) =>
      literal
    case literal@FloatNode(lcation, value) =>
      literal
    case node@Id(_, _) =>
      node
    case node@Selector(_, _, _) =>
      node
    case SimpleAssignment(location, variable, value) =>
      SimpleAssignment(location, variable, doRewrite(value))
    case PlusAssignment(location, variable, value) =>
      PlusAssignment(location, variable, doRewrite(value))
    case MinusAssignment(location, variable, value) =>
      MinusAssignment(location, variable, doRewrite(value))
    case MultiplicationAssignment(location, variable, value) =>
      MultiplicationAssignment(location, variable, doRewrite(value))
    case DivisionAssignment(location, variable, value) =>
      DivisionAssignment(location, variable, doRewrite(value))
    case ValDeclaration(location, variable, optionalType, value, immutable) =>
      ValDeclaration(location, variable, optionalType, doRewrite(value), immutable)
    case Lambda(location, params, optionalType, proc) =>
      Lambda(location, params, optionalType, doRewrite(proc))
    case FunctionCall(location, func, params) =>
      FunctionCall(location, doRewrite(func), params.map{doRewrite})
    case ListLiteral(location, elements) =>
      ListLiteral(location, elements.map{doRewrite})
    case SetLiteral(location, elements) =>
      SetLiteral(location, elements.map{doRewrite})
    case x@MapLiteral(location, elements) =>
      x.copy(location, elements = x.elements.map{ case (k, v) => (doRewrite(k), doRewrite(v)) })
    case x@ObjectNew(location, className, params) =>
      x.copy(params = x.params.map{doRewrite})
    case x@MethodCall(location ,self, name, params) =>
      x.copy(self = doRewrite(x.self), params = x.params.map{doRewrite})
    case x@Casting(_, _, _) =>
      x.copy(target = doRewrite(x.target))
    case x@EnumDeclaration(location, id, params, constructors) =>
      x
    case x@EnumIn(location, variant, body) =>
      x.copy(body = doRewrite(body))
    case x@Let(location, variable, type_, value, body, immutable) =>
      x.copy(value = doRewrite(x.value), body = doRewrite(x.body))
    case x@LetRec(location, name, function, cleanup, body) =>
      x.copy(function = x.function.copy(body = doRewrite(x.function.body)), cleanup = x.cleanup.map{doRewrite}, body = doRewrite(body))
    case Placeholder(location) =>
      Id(location, manager.generate())
    case otherwise =>
      throw RewriterPanic(otherwise.toString)
  }

  def transform(program: AST.Program): AST.Program = {
    program.copy(block = doRewrite(program.block).asInstanceOf[AST.Block])
  }

  override final val name: String = "PlaceholderDesugerer"

  override final def process(input: Program): Program = {
    transform(input)
  }
}
