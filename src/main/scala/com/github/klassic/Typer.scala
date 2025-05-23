package com.github.klassic

import java.lang.reflect.TypeVariable

import com.github.klassic.TypedAst.TypedNode
import com.github.klassic.Ast.Node
import com.github.klassic.Ast.RecordDeclaration
import com.github.klassic.Type.{TConstructor, _}
import com.github.klassic._ // For package object functions like newEmptySubstitution, etc.

import scala.collection.mutable // Ensure mutable is imported if not already via package object

/**
  * @author Kota Mizushima
  */
class Typer extends Processor[Ast.Program, TypedAst.Program, InteractiveSession] {
  type ModuleEnvironment = Map[String, Environment]
  type RecordEnvironment = Map[String, TRecord]
  type Name = String
  type Label = String

  def listOf(tp: Type): TConstructor = {
    TConstructor("List", List(tp))
  }
  def mapOf(k: Type, v: Type): TConstructor = {
    TConstructor("Map", List(k, v))
  }
  def setOf(tp: Type): TConstructor = {
    TConstructor("Set", List(tp))
  }
  val BuiltinEnvironment: Environment = {
    Map(
      "url"          -> TScheme(Nil, TFunction(List(TString), TDynamic)),
      "uri"          -> TScheme(Nil, TFunction(List(TString), TDynamic)),
      "substring"    -> TScheme(Nil, TFunction(List(TString, TInt, TInt), TString)),
      "at"           -> TScheme(Nil, TFunction(List(TDynamic, TInt), TDynamic)),
      "matches"      -> TScheme(Nil, TFunction(List(TString, TString), TBoolean)),
      "thread"       -> TScheme(Nil, TFunction(List(TFunction(List.empty, TDynamic)), TDynamic)),
      "println"      -> TScheme(List(tv("x")), TFunction(List(tv("x")), TUnit)),
      "printlnError" -> TScheme(List(tv("x")), TFunction(List(tv("x")), TUnit)),
      "stopwatch"    -> TScheme(Nil, TFunction(List(TFunction(List.empty, TDynamic)), TInt)),
      "sleep"        -> TScheme(Nil, TInt ==> TUnit),
      "isEmpty"      -> TScheme(List(tv("a")), listOf(tv("a")) ==> TBoolean),
      "ToDo"         -> TScheme(List(tv("a")), TFunction(Nil, tv("a"))),
      "assert"       -> TScheme(List(tv("a")), TBoolean ==> TUnit),
      "assertResult" -> TScheme(List(tv("a")), tv("a") ==> (tv("a") ==> TUnit)),
      "map"          -> TScheme(List(tv("a"), tv("b")), listOf(tv("a")) ==> ((tv("a") ==> tv("b"))  ==> listOf(tv("b")))),
      "head"         -> TScheme(List(tv("a")), listOf(tv("a")) ==> tv("a")),
      "tail"         -> TScheme(List(tv("a")), listOf(tv("a")) ==> listOf(tv("a"))),
      "cons"         -> TScheme(List(tv("a")), tv("a") ==> (listOf(tv("a")) ==> listOf(tv("a")))),
      "double"       -> TScheme(Nil, TInt ==> TDouble),
      "int"          -> TScheme(Nil, TDouble ==> TInt),
      "floor"        -> TScheme(Nil, TDouble ==> TInt),
      "ceil"         -> TScheme(Nil, TDouble ==> TInt),
      "sqrt"         -> TScheme(Nil, TDouble ==> TDouble),
      "abs"          -> TScheme(Nil, TDouble ==> TDouble),
      "size"         -> TScheme(List(tv("a")), listOf(tv("a")) ==> TInt),
      "foldLeft"     -> TScheme(List(tv("a"), tv("b")), listOf(tv("a")) ==> (tv("b") ==> ((List(tv("b"), tv("a")) ==> tv("b")) ==> tv("b")))),
      "null"         -> TScheme(List(tv("a")), tv("a")),
      "desktop"      -> TScheme(Nil, Nil ==> TDynamic)
    )
  }

  val TOutStream: TRecord = TRecord(
    Nil,
    TRowExtend("core", TDynamic, TRowEmpty)
  )

  val TInStream : TRecord = TRecord(
    Nil,
    TRowExtend("core", TDynamic, TRowEmpty)
  )

  val BuiltinRecordEnvironment: Map[String, TRecord] = {
    Map(
      "Point" -> TRecord(
        Nil,
        TRowExtend("x", TInt, TRowExtend("y", TInt, TRowEmpty))
      ),
      "InStream" -> TInStream,
      "OutStream" -> TOutStream,
    )
  }

  val BuiltinModuleEnvironment: Map[String, Environment] = {
    Map(
      "List" -> Map(
        "cons" -> TScheme(List(tv("a")), TFunction(List(tv("a"), listOf(tv("a"))), listOf(tv("a")))),
        "map" -> TScheme(List(tv("a"), tv("b")), listOf(tv("a")) ==> ((tv("a") ==> tv("b"))  ==> listOf(tv("b")))),
        "head" -> TScheme(List(tv("a")), listOf(tv("a")) ==> tv("a")),
        "tail" -> TScheme(List(tv("a")), listOf(tv("a")) ==> listOf(tv("a"))),
        "size" -> TScheme(List(tv("a")), listOf(tv("a")) ==> TInt),
        "isEmpty" -> TScheme(List(tv("a")), listOf(tv("a")) ==> TBoolean)
      ),
      "Map" -> Map(
        "add" -> TScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> (List(tv("a"), tv("b")) ==> mapOf(tv("a"), tv("b")))),
        "containsKey" -> TScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> (tv("a") ==> TBoolean)),
        "containsValue" -> TScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> (tv("b") ==> TBoolean)),
        "get" -> TScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> (tv("a") ==> tv("b"))),
        "size" -> TScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> TInt),
        "isEmpty" -> TScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> TBoolean)
      ),
      "Set" -> Map(
        "add" -> TScheme(List(tv("a")), setOf(tv("a")) ==> (tv("a") ==> setOf(tv("a")))),
        "remove" -> TScheme(List(tv("a")), setOf(tv("a")) ==> (tv("a") ==> setOf(tv("a")))),
        "contains" -> TScheme(List(tv("a")), setOf(tv("a")) ==> (tv("a") ==> TBoolean)),
        "size" -> TScheme(List(tv("a")), setOf(tv("a")) ==> TInt),
        "isEmpty" -> TScheme(List(tv("a")), setOf(tv("a")) ==> TBoolean)
      ),
      "FileInput" -> Map(
        "open" -> TScheme(List(tv("a")), TString ==> ((TInStream ==> tv("a")) ==> tv("a"))),
        "readAll" -> TScheme(List(), TInStream ==> TString),
        "readLines" -> TScheme(List(), TInStream ==> TString),
        "all" -> TScheme(List(), TString ==> TString),
        "lines" -> TScheme(List(), TString ==> listOf(TString)),
      ),
      "GPIO" -> Map(
        "pin" -> TScheme(Nil, List(TInt) ==> TDynamic),
        "setup" -> TScheme(Nil, List() ==> TDynamic),
        "outputOf" -> TScheme(Nil, List(TDynamic, TDynamic, TBoolean) ==> TDynamic),
        "inputOf" -> TScheme(Nil, List(TDynamic, TDynamic) ==> TDynamic),
        "isHigh" -> TScheme(Nil, List(TDynamic) ==> TBoolean),
        "isLow" -> TScheme(Nil, List(TDynamic) ==> TBoolean),
        "toggle" -> TScheme(Nil, List(TDynamic) ==> TUnit),
        "toHigh" -> TScheme(Nil, List(TDynamic) ==> TUnit),
        "toLow" -> TScheme(Nil, List(TDynamic) ==> TUnit)
      )
    )
  }

  def newInstanceFrom(scheme: TScheme): Type = {
    val s = newEmptySubstitution() 
    scheme.svariables.foreach { tv => extendSubstitution(s, tv, newTypeVariable()) } 
    replaceInType(s, scheme.stype) 
  }
  private var n: Int = 0
  private var m: Int = 0
  def newTypeVariable(): Type = {
    n += 1; TVariable("'a" + n)
  }
  def newTypeVariable(name: String): TVariable = {
    m += 1; TVariable(name + m)
  }

  val EmptySubstitution: Substitution = newEmptySubstitution() 

  def lookup(x: String, environment: Environment): Option[TScheme] = environment.get(x) match {
    case Some(t) => Some(t)
    case None => None
  }

  def generalize(t: Type, environment: Environment): TScheme = {
    TScheme(typeVariables(t) diff typeVariables(environment), t)
  }

  def unify(t: Type, u: Type, s: Substitution): Unit = (replaceInType(s, t), replaceInType(s, u)) match {
    case (TVariable(a), TVariable(b)) if a == b =>
      () 
    case (tvA@TVariable(a), typeB) if !(typeVariables(typeB) contains tvA) =>
      extendSubstitution(s, tvA, typeB) 
    case (typeA, tvB@TVariable(b)) => 
      unify(tvB, typeA, s) 
    case (TInt, TInt) => ()
    case (TShort, TShort) => ()
    case (TByte, TByte) => ()
    case (TLong, TLong) => ()
    case (TFloat, TFloat) => ()
    case (TDouble, TDouble) => ()
    case (TBoolean, TBoolean) => ()
    case (TString, TString) => ()
    case (TString, TDynamic) => ()
    case (TDynamic, TString) => ()
    case (TUnit, TUnit) => ()
    case (TDynamic, TDynamic) => ()
    case (TRecord(ts1, row1), TRecord(ts2, row2)) =>
      (ts1 zip ts2).foreach { case (t1_elem, t2_elem) => unify(t1_elem, t2_elem, s) }
      unify(row1, row2, s)
    case (TRowEmpty, TRowEmpty) => ()
    case (TRowExtend(label1, type1, rowTail1), row2@TRowExtend(_, _, _)) =>
      val (type2, rowTail2) = rewriteRow(row2, label1, s) 
      toList(rowTail1)._2 match {
        case Some(tv) if s.contains(tv) => typeError(current.location, "recursive row type") 
        case _ =>
          unify(replaceInType(s, type1), replaceInType(s, type2), s) 
          unify(replaceInType(s, rowTail1), replaceInType(s, rowTail2), s) 
      }
    case (r1@TRecordReference(t1, t2), r2@TRecordReference(u1, u2)) if t1 == u1 =>
      if(t2.length != u2.length) {
        typeError(current.location, s"type constructor arity mismatch: ${r1} != ${r2}")
      }
      (t2 zip u2).foreach { case (t_elem, u_elem) =>
        unify(t_elem, u_elem, s)
      }
    case (TRecordReference(name, ts), record2@TRecord(us, _)) =>
      if(ts.length != us.length) {
        typeError(current.location, s"type constructor arity mismatch: ${ts.length} != ${us.length}")
      }
      val record1 = recordEnvironment(name) 
      unify(record1, record2, s)
    case (r1@TRecord(_, _), r2@TRecordReference(_, _)) =>
      unify(r2, r1, s)
    case (TFunction(t1_params, t2_ret), TFunction(u1_params, u2_ret)) if t1_params.size == u1_params.size =>
      (t1_params zip u1_params).foreach { case (param_t, param_u) => unify(param_t, param_u, s) }
      unify(t2_ret, u2_ret, s) 
    case (TConstructor(k1, ts_args1), TConstructor(k2, us_args2)) if k1 == k2 =>
      (ts_args1 zip us_args2).foreach { case (arg_t, arg_u) => unify(arg_t, arg_u, s) }
    case (typeA, typeB) => 
      typeError(current.location, s"cannot unify ${typeA} with ${typeB}")
  }

  def toRow(bindings: List[(String, Type)]): Row = bindings match {
    case (n, t) :: tl => TRowExtend(n, t, toRow(tl))
    case Nil => TRowEmpty
  }

  def toList(row: Type): (List[(String, Type)], Option[TVariable]) = row match {
    case tv@TVariable(_) => (Nil, Some(tv))
    case TRowEmpty => (Nil, None)
    case TRowExtend(l, t, r) =>
      val (ls, mv) = toList(r)
      ((l -> t) :: ls, mv)
    case otherwise => throw TyperPanic("Unexpected: " + otherwise)
  }

  def rewriteRow(row: Type, newLabel: Label, s: Substitution): (Type, Type) = row match {
    case TRowEmpty => typeError(current.location, s"label ${newLabel} cannot be inserted")
    case TRowExtend(label, labelType, rowTail) if newLabel == label =>
      (labelType, rowTail) 
    case TRowExtend(label, labelType, alpha@TVariable(_)) =>
      val beta = newTypeVariable("r")
      val gamma = newTypeVariable("a")
      extendSubstitution(s, alpha, TRowExtend(newLabel, gamma, beta)) 
      (gamma, TRowExtend(label, labelType, beta)) 
    case TRowExtend(label, labelType, rowTail) =>
      val (labelType_, rowTail_) = rewriteRow(rowTail, newLabel, s)
      (labelType_, TRowExtend(label, labelType, rowTail_)) 
    case row =>
      typeError(current.location, s"Unexpect type: ${row}")
  }

  def doTypeRecords(recordDeclarations :List[RecordDeclaration]): RecordEnvironment = {
    val headers = recordDeclarations.map{d => (d.name, d.ts) }.toMap
    var recordEnvironment: RecordEnvironment = Map.empty
    //var s: Substitution = newEmptySubstitution() // s is not used in this function in the original code beyond this point
    recordDeclarations.foreach{recordDeclaration =>
      val recordName = recordDeclaration.name
      val location = recordDeclaration.location
      val members: List[(String, Type)] = recordDeclaration.members.map{ case (n, t) =>
          t match {
            case TRecordReference(rname, rtypes) if !headers.contains(rname) =>
              typeError(location, s"record ${rname} is not found")
            case r@TRecordReference(rname, rtypes) if recordName == rname =>
              val ts = headers(recordName)
              if(ts.length != rtypes.length) {
                typeError(location, s"type variables length mismatch: required: ${ts.length} actual: ${rtypes.length}")
              }
              (n, t)
            case _ =>
              (n, t)
          }
      }
      val ts = headers(recordName)
      recordEnvironment += (recordName -> TRecord(ts, toRow(members)))
    }
    recordEnvironment
  }

  def typeOf(e: Ast.Node, environment: Environment = BuiltinEnvironment, records: RecordEnvironment = BuiltinRecordEnvironment, modules: ModuleEnvironment = BuiltinModuleEnvironment): Type = {
    val a = newTypeVariable()
    val r = new SyntaxRewriter
    val s = newEmptySubstitution() // Create mutable substitution
    doType(r.doRewrite(e), TypeEnvironment(environment, Set.empty, records, modules, None), a, s) // s is mutated
    replaceInType(s, a) // Read from mutated s
  }

  var current: Ast.Node = null
  var recordEnvironment: RecordEnvironment = null
  // Signature changed: s0 to s, returns TypedNode
  def doType(e: Ast.Node, env: TypeEnvironment, t: Type, s: Substitution): TypedNode = {
    current = e
    e match {
      case Ast.Block(location, expressions) =>
        expressions match {
          case Nil =>
            unify(TUnit, t, s)
            TypedAst.Block(TUnit, location, Nil)
          case x :: Nil =>
            val typedX = doType(x, env, t, s)
            TypedAst.Block(replaceInType(s, t), location, typedX :: Nil)
          case x :: xs =>
            val xTypeVar = newTypeVariable()
            val typedX = doType(x, env, xTypeVar, s)
            val typedXs = xs.zipWithIndex.map { case (expr, index) =>
              val currentExprTypeVar = if (index == xs.length - 1) t else newTypeVariable()
              doType(expr, env, currentExprTypeVar, s)
            }
            TypedAst.Block(replaceInType(s, t), location, typedX :: typedXs)
        }
      case Ast.IntNode(location, value) =>
        unify(t, TInt, s)
        TypedAst.IntNode(replaceInType(s, t), location, value)
      case Ast.ShortNode(location, value) =>
        unify(t, TShort, s)
        TypedAst.ShortNode(replaceInType(s, t), location, value)
      case Ast.ByteNode(location, value) =>
        unify(t, TByte, s)
        TypedAst.ByteNode(replaceInType(s, t), location, value)
      case Ast.LongNode(location, value) =>
        unify(t, TLong, s)
        TypedAst.LongNode(replaceInType(s, t), location, value)
      case Ast.FloatNode(location, value) =>
        unify(t, TFloat, s)
        TypedAst.FloatNode(replaceInType(s, t), location, value)
      case Ast.DoubleNode(location, value) =>
        unify(t, TDouble, s)
        TypedAst.DoubleNode(replaceInType(s, t), location, value)
      case Ast.BooleanNode(location, value) =>
        unify(t, TBoolean, s)
        TypedAst.BooleanNode(replaceInType(s, t), location, value)
      case Ast.UnitNode(location) =>
        unify(t, TUnit, s)
        TypedAst.UnitNode(replaceInType(s, t), location)
      case Ast.SimpleAssignment(location, variable, value) =>
        if(env.immutableVariables.contains(variable)) {
          typeError(location, s"variable '$variable' cannot change")
        }
        env.lookup(variable) match {
          case None =>
            typeError(location, s"variable $variable is not defined")
          case Some(variableTypeScheme) =>
            val valueExpectedType = newInstanceFrom(variableTypeScheme) // Or variableTypeScheme.stype if no newInstance logic needed here
            val typedValue = doType(value, env, valueExpectedType, s) // Pass valueExpectedType as 't' for the value
            unify(replaceInType(s,valueExpectedType), typedValue.type_, s) // Unify what 't' became with actual value type
            unify(t, TUnit, s) // Assignment evaluates to Unit
            TypedAst.Assignment(replaceInType(s,valueExpectedType), location, variable, typedValue)
        }
      case Ast.IfExpression(location, cond, pos, neg) =>
        val typedCondition = doType(cond, env, TBoolean, s)
        unify(typedCondition.type_, TBoolean, s) // Ensure condition is boolean
        val posTyped = doType(pos, env, t, s)
        val negTyped = doType(neg, env, t, s)
        unify(posTyped.type_, negTyped.type_, s) // Ensure branches have same type
        unify(t, posTyped.type_, s) // Unify overall type 't' with branch type
        TypedAst.IfExpression(replaceInType(s, t), location, typedCondition, posTyped, negTyped)
      case Ast.TernaryExpression(location, cond, pos, neg) =>
        val typedCondition = doType(cond, env, TBoolean, s)
        unify(typedCondition.type_, TBoolean, s)
        val posTyped = doType(pos, env, t, s)
        val negTyped = doType(neg, env, t, s)
        unify(posTyped.type_, negTyped.type_, s)
        unify(t, posTyped.type_, s)
        TypedAst.IfExpression(replaceInType(s, t), location, typedCondition, posTyped, negTyped) // Uses IfExpression AST
      case Ast.WhileExpression(location, condition, body) =>
        val conditionType = TBoolean
        val typedCondition = doType(condition, env, conditionType, s)
        unify(typedCondition.type_, conditionType, s)
        val bodyType = newTypeVariable() // Body can be of any type, but while loop is Unit
        val typedBody = doType(body, env, bodyType, s)
        unify(TUnit, t, s) // while evaluates to Unit
        TypedAst.WhileExpression(TUnit, location, typedCondition, typedBody)
      
      case Ast.BinaryExpression(location, operator @ (Operator.EQUAL | Operator.LESS_THAN | Operator.GREATER_THAN | Operator.LESS_OR_EQUAL | Operator.GREATER_EQUAL) , lhs, rhs) =>
        val a = newTypeVariable()
        val b = newTypeVariable()
        val typedLhs = doType(lhs, env, a, s)
        val typedRhs = doType(rhs, env, b, s)
        unify(replaceInType(s,a), replaceInType(s,b), s) // operands must be unifiable
        val unifiedOperandType = replaceInType(s,a)
        // Further checks based on operator if needed (e.g. numeric for <, >, <=, >=)
        // For now, just ensure they are unifiable and result is Boolean
        unify(TBoolean, t, s)
        TypedAst.BinaryExpression(replaceInType(s,t), location, operator, typedLhs, typedRhs)

      case Ast.BinaryExpression(location, operator @ (Operator.ADD | Operator.SUBTRACT | Operator.MULTIPLY | Operator.DIVIDE), lhs, rhs) =>
        val a = newTypeVariable()
        val b = newTypeVariable()
        val typedLhs = doType(lhs, env, a, s)
        val typedRhs = doType(rhs, env, b, s)
        val typeLhs = replaceInType(s,a)
        val typeRhs = replaceInType(s,b)

        val resultType = (typeLhs, typeRhs) match {
          case (TInt, TInt) => TInt
          case (TLong, TLong) => TLong
          case (TShort, TShort) => TShort
          case (TByte, TByte) => TByte
          case (TFloat, TFloat) => TFloat
          case (TDouble, TDouble) => TDouble
          case (TString, _) if operator == Operator.ADD => TString
          case (_, TString) if operator == Operator.ADD => TString
          case (TDynamic, _) => TDynamic
          case (_, TDynamic) => TDynamic
          case _ => // Default to Int for numeric ops if not string/dynamic, or error
            unify(typeLhs, TInt, s)
            unify(typeRhs, TInt, s)
            TInt
        }
        unify(resultType, t, s)
        TypedAst.BinaryExpression(replaceInType(s,t), location, operator, typedLhs, typedRhs)

      case Ast.BinaryExpression(location, operator @ (Operator.AND | Operator.OR | Operator.XOR), lhs, rhs) => // Bitwise ops
        val typedLhs = doType(lhs, env, TInt, s)
        val typedRhs = doType(rhs, env, TInt, s)
        unify(typedLhs.type_, TInt, s)
        unify(typedRhs.type_, TInt, s)
        unify(TInt, t, s)
        TypedAst.BinaryExpression(replaceInType(s,t), location, operator, typedLhs, typedRhs)
      
      case Ast.MinusOp(location, operand) =>
        val a = newTypeVariable()
        val typedOperand = doType(operand, env, a, s)
        val operandFinalType = replaceInType(s,a) 
        val resultType = operandFinalType match {
            case TInt | TLong | TShort | TByte | TFloat | TDouble => operandFinalType
            case TDynamic => TDynamic
            case _ => unify(operandFinalType, TInt, s); TInt
        }
        unify(resultType, t, s)
        TypedAst.MinusOp(replaceInType(s,t), location, typedOperand)
      case Ast.PlusOp(location, operand) => // Similar to MinusOp
        val a = newTypeVariable()
        val typedOperand = doType(operand, env, a, s)
        val operandFinalType = replaceInType(s,a)
        val resultType = operandFinalType match {
            case TInt | TLong | TShort | TByte | TFloat | TDouble | TString => operandFinalType
            case TDynamic => TDynamic
            case _ => unify(operandFinalType, TInt, s); TInt
        }
        unify(resultType, t, s)
        TypedAst.PlusOp(replaceInType(s,t), location, typedOperand)

      case Ast.BinaryExpression(location, operator @ (Operator.AND2 | Operator.BAR2), lhs, rhs) => // Logical And/Or
        val typedLhs = doType(lhs, env, TBoolean, s)
        val typedRhs = doType(rhs, env, TBoolean, s)
        unify(typedLhs.type_, TBoolean, s)
        unify(typedRhs.type_, TBoolean, s)
        unify(TBoolean, t, s)
        TypedAst.BinaryExpression(replaceInType(s,t), location, operator, typedLhs, typedRhs)

      case Ast.StringNode(location, value) =>
        unify(TString, t, s)
        TypedAst.StringNode(replaceInType(s,t), location, value)
      case Ast.Id(location, name) =>
        env.lookup(name) match {
          case None => typeError(location, s"variable '${name}' is not found")
          case Some(u_scheme) => 
            val freshInstanceType = newInstanceFrom(u_scheme)
            unify(freshInstanceType, t, s)
        }
        TypedAst.Id(replaceInType(s,t), location, name)
      case Ast.Selector(location, module, name) =>
        env.lookupModuleMember(module, name) match {
          case None => typeError(location, s"module '${module}' or member '${name}' is not found")
          case Some(u_scheme) => 
            val freshInstanceType = newInstanceFrom(u_scheme)
            unify(freshInstanceType, t, s)
        }
        TypedAst.Selector(replaceInType(s,t), location, module, name)
      case node@Ast.RecordSelect(_, _, _) =>
        doTypeRecordSelect(node, env, t, s)
      case Ast.Lambda(location, params, optionalBodyType, body) =>
        val bodyActualType = optionalBodyType.getOrElse(newTypeVariable())
        val paramTypes = params.map{p => p.optionalType.getOrElse(newTypeVariable())}
        val paramSchemes = (params zip paramTypes).map{ case (p, pt) => p.name -> TScheme(Nil, pt) }
        
        val env1 = paramSchemes.foldLeft(env) { case (currentEnv, (name, scheme)) => currentEnv.updateImmuableVariable(name, scheme)}
        val typedBody = doType(body, env1, bodyActualType, s)
        unify(typedBody.type_, bodyActualType, s) // Ensure body matches expected/inferred body type

        val finalFuncType = TFunction(paramTypes.map(pt => replaceInType(s,pt)), replaceInType(s,bodyActualType))
        unify(t, finalFuncType, s)
        TypedAst.FunctionLiteral(replaceInType(s,t), location, params, optionalBodyType, typedBody)

      case Ast.Let(location, variable, optionalType, value, body, immutable) =>
        if(env.variables.contains(variable)) {
          typeError(location, s"variable $variable is already defined")
        }
        val valueExpectedType = optionalType.map {
          case TRecordReference(name, _) => env.records.getOrElse(name, typeError(location, s"record named ${name} is not found"))
          case otherwise => otherwise
        }.getOrElse(newTypeVariable())
        
        val typedValue = doType(value, env, valueExpectedType, s)
        unify(typedValue.type_, valueExpectedType, s) // Ensure value's actual type matches expected/inferred

        val valueFinalType = replaceInType(s, valueExpectedType)
        val generalizedValueType = generalize(valueFinalType, env.variables) // Generalize after substitution
        
        val newEnv = if(immutable) {
          env.updateImmuableVariable(variable, generalizedValueType)
        } else {
          env.updateMutableVariable(variable, generalizedValueType)
        }
        val typedBody = doType(body, newEnv, t, s)
        TypedAst.LetDeclaration(typedBody.type_, location, variable, valueFinalType, typedValue, typedBody, immutable)

      case Ast.LetRec(location, variable, value, cleanup, body) =>
         if(env.variables.contains(variable)) {
          throw new InterruptedException(s"${location.format} function ${variable} is already defined")
        }
        val funcVarType = newTypeVariable() // Type variable for the function itself
        val funcBodyExpectedType = newTypeVariable() // Type variable for what the function's body should return

        // Type the function definition (value) in an environment where the function variable exists with funcVarType
        val typedFuncDef = doType(value, env.updateImmuableVariable(variable, TScheme(Nil, funcVarType)), funcBodyExpectedType, s)
        
        // Unify funcVarType (what was assumed for the function) with funcBodyExpectedType (what its body is expected to return)
        // This also implies funcVarType should be compatible with typedFuncDef.type_
        unify(funcVarType, typedFuncDef.type_, s) // typedFuncDef.type_ is TFunction(...)
        unify(funcVarType, funcBodyExpectedType, s) // This seems redundant if typedFuncDef.type_ is already a TFunction correctly typed.
                                                // More accurately, unify funcVarType with the actual function type from typedFuncDef

        val finalFuncVarType = replaceInType(s, funcVarType)
        val generalizedFuncType = generalize(finalFuncVarType, env.variables)
        val envForBody = env.updateImmuableVariable(variable, generalizedFuncType)
        
        val typedBody = doType(body, envForBody, t, s)

        val typedCleanupOpt = cleanup.map{c => 
            val cleanupType = newTypeVariable()
            doType(c, env, cleanupType, s) // Cleanup typed in original env, or envForBody? Original seems more likely.
        }
        TypedAst.LetFunctionDefinition(typedBody.type_, location, variable, typedFuncDef.asInstanceOf[TypedAst.FunctionLiteral], typedCleanupOpt, typedBody)

      case Ast.FunctionCall(location, targetFuncExpr, params) =>
        val paramExpectedTypes = params.map{_ => newTypeVariable()}
        // 't' is the expected return type of the function call
        val typedTargetFunc = doType(targetFuncExpr, env, TFunction(paramExpectedTypes, t), s) 
        
        val typedParams = (params zip paramExpectedTypes).map { case (paramExpr, expectedType) =>
          doType(paramExpr, env, expectedType, s)
        }
        // After typing params, their types (paramExpectedTypes now possibly constrained in s) are used in TFunction for typedTargetFunc
        // This seems okay as 's' is threaded.
        unify(typedTargetFunc.type_, TFunction(paramExpectedTypes.map(pt => replaceInType(s,pt)), replaceInType(s,t)), s)
        TypedAst.FunctionCall(replaceInType(s,t), location, typedTargetFunc, typedParams)

      case Ast.ListLiteral(location, elements) =>
        val elementTypeVar = newTypeVariable()
        val typedElements = elements.map(el => doType(el, env, elementTypeVar, s))
        // After typing all elements, elementTypeVar in 's' holds the unified element type
        unify(listOf(replaceInType(s,elementTypeVar)), t, s)
        TypedAst.ListLiteral(replaceInType(s,t), location, typedElements)

      case Ast.SetLiteral(location, elements) =>
        val elementTypeVar = newTypeVariable()
        val typedElements = elements.map(el => doType(el, env, elementTypeVar, s))
        unify(setOf(replaceInType(s,elementTypeVar)), t, s)
        TypedAst.SetLiteral(replaceInType(s,t), location, typedElements)

      case Ast.MapLiteral(location, elements) =>
        val keyTypeVar = newTypeVariable()
        val valueTypeVar = newTypeVariable()
        val typedElements = elements.map{ case (k, v) =>
          val typedK = doType(k, env, keyTypeVar, s)
          val typedV = doType(v, env, valueTypeVar, s)
          (typedK, typedV)
        }
        unify(mapOf(replaceInType(s,keyTypeVar), replaceInType(s,valueTypeVar)), t, s)
        TypedAst.MapLiteral(replaceInType(s,t), location, typedElements)

      case Ast.ObjectNew(location, className, params) =>
        val paramTypes = params.map{_ => newTypeVariable()}
        val typedParams = (params zip paramTypes).map { case (p, pt) => doType(p, env, pt, s) }
        unify(TDynamic, t, s) // Java objects are TDynamic
        TypedAst.ObjectNew(TDynamic, location, className, typedParams)

      case Ast.RecordNew(location, recordName, params) =>
        env.records.get(recordName) match {
          case Some(recordScheme) =>
            // Instantiate scheme for the record itself (its type variables like 'T in Record[T]')
            // This is distinct from parameters to the constructor.
            // For now, assume recordScheme.ts are type parameters of the record, not constructor params.
            // The actual members define constructor param types.
            val (memberNamesTypes, _) = toList(recordScheme.row) // Original member types from definition
            
            if(params.length != memberNamesTypes.length) {
               typeError(location, s"Record ${recordName} constructor arity mismatch: expected ${memberNamesTypes.length}, got ${params.length}")
            }

            // Create a fresh substitution for instantiating the record scheme's type variables
            val instanceSub = newEmptySubstitution()
            recordScheme.ts.foreach(tv => extendSubstitution(instanceSub, tv, newTypeVariable()))
            
            val instantiatedMemberTypes = memberNamesTypes.map{ case (_, mt) => replaceInType(instanceSub, mt)}

            val typedParams = (params zip instantiatedMemberTypes).map { case (paramExpr, expectedMemberType) =>
              doType(paramExpr, env, expectedMemberType, s) // Type current param against instantiated member type
            }
            
            // Unify actual param types (after typing) with their expected (instantiated) member types
             (typedParams zip instantiatedMemberTypes).foreach { case (tp, emt) =>
                unify(tp.type_, emt, s)
            }

            val finalRecordType = replaceInType(instanceSub, recordScheme) // This should be TRecord(...)
            unify(t, finalRecordType, s)
            TypedAst.RecordNew(replaceInType(s,t), location, recordName, typedParams)
          case None =>
            typeError(location, s"record '$recordName' is not found")
        }

      case Ast.Casting(location, target, toType) => // Renamed 'to' to 'toType'
        val targetTypeVar = newTypeVariable()
        val typedTarget = doType(target, env, targetTypeVar, s)
        // Casting doesn't strictly unify target with toType in Hindley-Milner; it asserts.
        // However, the overall expression type 't' must be 'toType'.
        unify(t, toType, s)
        // Can add runtime check or specific unification rules if needed, e.g. unify(typedTarget.type_, toType, s) if cast implies type check
        TypedAst.Casting(replaceInType(s,t), location, typedTarget, toType)

      case Ast.MethodCall(location, receiver, name, params) =>
        val receiverTypeVar = newTypeVariable()
        val typedReceiver = doType(receiver, env, receiverTypeVar, s)
        // For now, assume method calls resolve to TDynamic, and receiver can be TDynamic
        unify(replaceInType(s, receiverTypeVar), TDynamic, s) 
        
        val typedParams = params.map{ p => 
            val paramTypeVar = newTypeVariable()
            doType(p, env, paramTypeVar, s)
        }
        unify(TDynamic, t, s) // Method call result is TDynamic
        TypedAst.MethodCall(TDynamic, location, typedReceiver, name, typedParams)
      case otherwise =>
        throw TyperPanic(otherwise.toString)
    }
  }

  // Signature changed: s0 to s, returns TypedNode
  private def doTypeRecordSelect(node: Ast.RecordSelect, environment: TypeEnvironment, t: Type, s: Substitution): TypedNode = {
    // Helper 'insert' is complex due to its original reliance on returning substitutions.
    // With mutable 's', 'insert' would now mutate 's' directly.
    // For this refactoring, let's simplify its role or how it's used.
    // The primary goal of record select is:
    // 1. Type the record expression.
    // 2. Ensure it's a record type (or variable that gets unified with one).
    // 3. Find the member type.
    // 4. Unify 't' (expected type of select expression) with member type.

    val recordExpr = node.record
    val memberName = node.label
    val recordExprTypeVar = newTypeVariable()

    val typedRecordExpr = doType(recordExpr, environment, recordExprTypeVar, s)
    val actualRecordType = replaceInType(s, recordExprTypeVar)

    actualRecordType match {
      case recType @ TRecord(_, row) =>
        var foundMemberType: Option[Type] = None
        var currentType = row
        var searching = true
        while(searching) {
          replaceInType(s, currentType) match { // Resolve currentType through s before matching
            case TRowExtend(label, labelType, tail) if label == memberName =>
              foundMemberType = Some(labelType)
              searching = false
            case TRowExtend(_, _, tail) =>
              currentType = tail
            case TRowEmpty =>
              searching = false // Member not found
            case tv@TVariable(_) => // Row variable, implies member must exist
              val newMemberType = newTypeVariable("member_" + memberName)
              val newTailVar = newTypeVariable("tail")
              unify(tv, TRowExtend(memberName, newMemberType, newTailVar), s)
              foundMemberType = Some(newMemberType)
              searching = false
            case TError => searching = false // Error type, stop
            case _ => typeError(node.location, s"Expected row type but got ${currentType}") ; searching = false

          }
        }
        foundMemberType match {
          case Some(memberT) =>
            unify(t, memberT, s)
            TypedAst.RecordSelect(replaceInType(s,t), node.location, typedRecordExpr, memberName)
          case None =>
            typeError(node.location, s"Member '${memberName}' not found in record type ${replaceInType(s, actualRecordType)}")
        }
      case TRecordReference(refName, _) =>
         environment.records.get(refName) match {
           case Some(definedRecord) =>
             // Effectively, unify actualRecordType (the reference) with its definition, then proceed.
             // This is implicitly handled if newInstanceFrom was used correctly for Id.
             // For simplicity here, we re-evaluate based on definedRecord.
             // This might need a more robust way to link TRecordReference to TRecord for member lookup.
             // A proper solution would be to unify actualRecordType with a TRecord containing the specific field,
             // similar to how row polymorphism is handled.
             // For now, let's assume 'actualRecordType' will eventually be unified with a TRecord.
             // We create a placeholder TRecord that *must* contain the member and unify with actualRecordType.
             val memberTypeVar = newTypeVariable()
             val tailVar = newTypeVariable()
             val expectedRecordShape = TRecord(Nil, TRowExtend(memberName, memberTypeVar, tailVar))
             unify(actualRecordType, expectedRecordShape, s)
             unify(t, memberTypeVar, s)
             TypedAst.RecordSelect(replaceInType(s,t), node.location, typedRecordExpr, memberName)
           case None => typeError(node.location, s"Record definition ${refName} not found.")
         }
      case tv@TVariable(_) => // Record expression is a type variable
        val memberTypeVar = newTypeVariable()
        val tailVar = newTypeVariable()
        val expectedRecordShape = TRecord(Nil, TRowExtend(memberName, memberTypeVar, tailVar))
        unify(tv, expectedRecordShape, s) // Unify the variable with a record type that has the member
        unify(t, memberTypeVar, s) // Overall type 't' is the member's type
        TypedAst.RecordSelect(replaceInType(s,t), node.location, typedRecordExpr, memberName)
      case other =>
        typeError(node.location, s"Expression ${node.record} is not a record, but type ${other}")
    }
  }

  def typeError(location: Location, message: String): Nothing = {
    throw TyperException(s"${location.format} ${message}")
  }

  def transform(program: Ast.Program): TypedAst.Program = {
    val tv = newTypeVariable() // Expected type for the whole program block
    val s = newEmptySubstitution() // Create the mutable substitution
    
    val recordEnv = doTypeRecords(program.records) // This uses newEmptySubstitution internally for its own 's' if needed
    this.recordEnvironment = recordEnv // Store for access in unify for TRecordReference
    
    // doType now mutates s and returns TypedNode
    val typedProgramBlock = doType(program.block, TypeEnvironment(BuiltinEnvironment, Set.empty, BuiltinRecordEnvironment ++ recordEnv, BuiltinModuleEnvironment, None), tv, s)
    
    // The overall type of the program is replaceInType(s, tv)
    // typedProgramBlock should already have its .type_ field set correctly by doType
    TypedAst.Program(program.location, Nil, typedProgramBlock.asInstanceOf[TypedAst.Block], recordEnv)
  }

  override final val name: String = "Typer"

  override final def process(input: Ast.Program, session :InteractiveSession): TypedAst.Program = transform(input)
}
