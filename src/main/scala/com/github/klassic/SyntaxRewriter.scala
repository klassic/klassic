package com.github.klassic

import com.github.klassic.AST.{FunctionCall, ListLiteral, MapLiteral, NewObject, _}
/**
  * Created by kota_mizushima on 2016/11/17.
  */
class SyntaxRewriter {
  object SymbolGenerator {
    private[this] var counter: Int = 0
    def symbol(): String = {
      val name = "var" + counter
      counter += 1
      name
    }
  }
  import SymbolGenerator.symbol
  def doRewrite(node: AST): AST = node match {
    case Block(location, expressions) =>
      Block(location, expressions.map{doRewrite})
    case IfExpression(location, cond, pos, neg) =>
      IfExpression(location, doRewrite(cond), doRewrite(pos), doRewrite(neg))
    case WhileExpression(location, condition, body: AST) =>
      WhileExpression(location, doRewrite(condition), doRewrite(body))
    case e@ForeachExpression(location, name, collection, body) =>
      val itVariable = symbol()
      val location = e.location
      Block(location, List(
        ValDeclaration(location, itVariable, None, MethodCall(location, doRewrite(collection), "iterator", List()), false),
        WhileExpression(
          location,
          BinaryExpression(
            location,
            Operator.EQUAL,
            MethodCall(location, Identifier(location, itVariable), "hasNext", List()),
            BooleanNode(location, true)
          ),
          Block(location, List(
            ValDeclaration(location, name, None, MethodCall(location, Identifier(location, itVariable), "next", List()), false),
            body
          ))
        )
      ))
    case BinaryExpression(location, operator, lhs, rhs) =>
      BinaryExpression(location, operator, doRewrite(lhs), doRewrite(rhs))
    case MinusOp(location, operand) => MinusOp(location, doRewrite(operand))
    case PlusOp(location, operand) => PlusOp(location, doRewrite(operand))
    case literal@StringNode(location, value) => literal
    case literal@IntNode(location, value) => literal
    case literal@LongNode(location, value)  => literal
    case literal@ShortNode(location, value) => literal
    case literal@ByteNode(location, value) => literal
    case literal@BooleanNode(location, value) => literal
    case literal@DoubleNode(location, value) => literal
    case literal@FloatNode(lcation, value) => literal
    case node@Identifier(_, name) => node
    case SimpleAssignment(location, variable, value) => SimpleAssignment(location, variable, doRewrite(value))
    case PlusAssignment(location, variable, value) =>
      val generatedSymbol = symbol()
      val rewritedValue = doRewrite(value)
      Block(
        location,
        List(
          ValDeclaration(location, generatedSymbol, None, rewritedValue, true),
          SimpleAssignment(location, variable,
            BinaryExpression(location, Operator.ADD, Identifier(location, variable), Identifier(location, generatedSymbol) )
          )
        )
      )
    case MinusAssignment(location, variable, value) =>
      val generatedSymbol = symbol()
      val rewritedValue = doRewrite(value)
      Block(
        location,
        List(
          ValDeclaration(location, generatedSymbol, None, rewritedValue, true),
          SimpleAssignment(location, variable,
            BinaryExpression(location, Operator.SUBTRACT, Identifier(location, variable), Identifier(location, generatedSymbol) )
          )
        )
      )
    case MultiplicationAssignment(location, variable, value) =>
      val generatedSymbol = symbol()
      val rewritedValue = doRewrite(value)
      Block(
        location,
        List(
          ValDeclaration(location, generatedSymbol, None, rewritedValue, true),
          SimpleAssignment(location, variable,
            BinaryExpression(location, Operator.MULTIPLY, Identifier(location, variable), Identifier(location, generatedSymbol) )
          )
        )
      )
    case DivisionAssignment(location, variable, value) =>
      val generatedSymbol = symbol()
      val rewritedValue = doRewrite(value)
      Block(
        location,
        List(
          ValDeclaration(location, generatedSymbol, None, rewritedValue, true),
          SimpleAssignment(location, variable,
            BinaryExpression(location, Operator.DIVIDE, Identifier(location, variable), Identifier(location, generatedSymbol) )
          )
        )
      )
    case ValDeclaration(location, variable, optionalType, value, immutable) => ValDeclaration(location, variable, optionalType, doRewrite(value), immutable)
    case FunctionLiteral(location, params, optionalType, proc) => FunctionLiteral(location, params, optionalType, doRewrite(proc))
    case FunctionDefinition(location, name, func, cleanup) => FunctionDefinition(location, name, doRewrite(func).asInstanceOf[FunctionLiteral], cleanup.map(doRewrite))
    case FunctionCall(location, func, params) => FunctionCall(location, doRewrite(func), params.map{doRewrite})
    case ListLiteral(location, elements) =>  ListLiteral(location, elements.map{doRewrite})
    case MapLiteral(location, elements) => MapLiteral(location, elements.map{ case (k, v) => (doRewrite(k), doRewrite(v))})
    case NewObject(location, className, params) => NewObject(location, className, params.map{doRewrite})
    case MethodCall(location ,self, name, params) => MethodCall(location, doRewrite(self), name, params.map{doRewrite})
  }

}
