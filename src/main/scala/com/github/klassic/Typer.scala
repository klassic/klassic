package com.github.klassic

import com.github.klassic.AST.RecordDeclaration
import com.github.klassic.Type.{TConstructor, _}

import scala.collection.mutable

/**
  * @author Kota Mizushima
  */
class Typer extends Processor[AST.Program, TypedAST.Program] {
  type Environment = Map[String, TScheme]
  type ModuleEnvironment = Map[String, Environment]
  type RecordEnvironment = Map[String, TRecord]
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
    val a = newTypeVariable()
    val b = newTypeVariable()
    Map(
      "url" -> TScheme(List(), TFunction(List(TDynamic), TDynamic)),
      "uri" -> TScheme(List(), TFunction(List(TDynamic), TDynamic)),
      "substring" -> TScheme(List(), TFunction(List(TDynamic, TInt, TInt), TDynamic)),
      "at" -> TScheme(List(), TFunction(List(TDynamic, TInt), TDynamic)),
      "matches" -> TScheme(List(), TFunction(List(TDynamic, TDynamic), TBoolean)),
      "thread" -> TScheme(List(), TFunction(List(TFunction(List.empty, TDynamic)), TDynamic)),
      "println" ->  TScheme(List(tv("x")), TFunction(List(tv("x")), TUnit)),
      "stopwatch" -> TScheme(List(), TFunction(List(TFunction(List.empty, TDynamic)), TInt)),
      "sleep" -> TScheme(List(), TInt ==> TUnit),
      "isEmpty" -> TScheme(List(tv("a")), listOf(tv("a")) ==> TBoolean),
      "ToDo" -> TScheme(List(tv("a")), TFunction(List(), tv("a"))),
      "assert" -> TScheme(List(tv("a")), TBoolean ==> TUnit),
      "assertResult" -> TScheme(List(tv("a")), tv("a") ==> (tv("a") ==> TUnit)),
      "map" -> TScheme(List(tv("a"), tv("b")), listOf(tv("a")) ==> ((tv("a") ==> tv("b"))  ==> listOf(tv("b")))),
      "head" -> TScheme(List(tv("a")), listOf(tv("a")) ==> tv("a")),
      "tail" -> TScheme(List(tv("a")), listOf(tv("a")) ==> listOf(tv("a"))),
      "cons" -> TScheme(List(tv("a")), tv("a") ==> (listOf(tv("a")) ==> listOf(tv("a")))),
      "size" -> TScheme(List(tv("a")), listOf(tv("a")) ==> TInt),
      "foldLeft" -> TScheme(List(tv("a"), tv("b")), listOf(tv("a")) ==> (tv("b") ==> ((List(tv("b"), tv("a")) ==> tv("b")) ==> tv("b")))),
      "null" -> TScheme(List(tv("a")), tv("a")),
      "desktop" -> TScheme(List(), Nil ==> TDynamic)
    )
  }

  val BuiltinRecordEnvironment: Map[String, TRecord] = {
    Map(
      "Point" -> TRecord(
        Nil,
        RowExtension("x", TInt, RowExtension("y", TInt, EmptyRow))
      )
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
      )
    )
  }

  def newInstanceFrom(scheme: TScheme): Type = {
    scheme.typeVariables.foldLeft(EmptySubstitution)((s, tv) => s.extend(tv, newTypeVariable())).apply(scheme.description)
  }
  private var n: Int = 0
  def newTypeVariable(): Type = {
    n += 1; TVariable("'a" + n)
  }
  val EmptySubstitution: Substitution = new Substitution(Map.empty)
  class Substitution(val map: Map[TVariable, Type]) extends Function1[Type, Type] {
    def lookup(x: TVariable): Type = {
      map.getOrElse(x, x)
    }

    def applyRow(r: Row): Row = r match {
      case RowExtension(l, t, e) => RowExtension(l, apply(t), applyRow(e))
      case EmptyRow => EmptyRow
    }

    def apply(t: Type): Type = t match {
      case tv@TVariable(a) =>
        val u = lookup(tv)
        if (t == u) t else apply(u)
      case TFunction(t1, t2) => TFunction(t1.map{ t => apply(t)}, apply(t2))
      case TRecordReference(name, ts) => TRecordReference(name, ts.map{ t => apply(t)})
      case TRecord(ts, row) => TRecord(ts, applyRow(row))
      case TInt => TInt
      case TShort => TShort
      case TByte => TByte
      case TLong => TLong
      case TFloat => TFloat
      case TDouble => TDouble
      case TBoolean => TBoolean
      case TUnit => TUnit
      case TDynamic => TDynamic
      case TError => TError
      case TConstructor(name, args) => TConstructor(name, args.map{ arg => apply(arg)})
    }

    def apply(env: Environment): Environment = {
      env.map { case (x, ts) =>
          x -> TScheme(typeVariables(ts), this.apply(ts.description))
      }
    }

    def extend(tv: TVariable, td: Type): Substitution = new Substitution(this.map + (tv -> td))

    def remove(tv: TVariable): Substitution = new Substitution(this.map - tv)

    def compose(that: Substitution): Substitution = {
      val s1 = this
      val s2 = that
      new Substitution(s2.map.mapValues{t => s1.apply(t)} ++ s1.map)
    }
  }

  def lookup(x: String, environment: Environment): Option[TScheme] = environment.get(x) match {
    case Some(t) => Some(t)
    case None => None
  }

  def generalize(t: Type, environment: Environment): TScheme = {
    TScheme(typeVariables(t) diff typeVariables(environment), t)
  }

  def typeVariables(r: Row): List[TVariable] = r match {
    case RowExtension(l, t, e) => typeVariables(t) union typeVariables(e)
    case EmptyRow => Nil
  }

  def typeVariables(t: Type): List[TVariable] = t match {
    case tv @ TVariable(a) =>
      List(tv)
    case TInt =>
      Nil
    case TShort =>
      Nil
    case TByte =>
      Nil
    case TLong =>
      Nil
    case TFloat =>
      Nil
    case TDouble =>
      Nil
    case TBoolean =>
      Nil
    case TUnit =>
      Nil
    case TDynamic =>
      Nil
    case TError =>
      Nil
    case TFunction(t1, t2) =>
      t1.flatMap{typeVariables} union typeVariables(t2)
    case TRecordReference(name, ts) =>
      List(ts.flatMap{ case t => typeVariables(t)}:_*)
    case TRecord(ts, row) =>
      ts union typeVariables(row)
    case TConstructor(k, ts) =>
      ts.foldLeft(List[TVariable]()){ (tvs, t) => tvs union typeVariables(t)}
  }

  def typeVariables(ts: TScheme): List[TVariable] = {
    typeVariables(ts.description) diff ts.typeVariables
  }

  def typeVariables(environment: Environment): List[TVariable] = {
    environment.foldLeft(List[TVariable]()) { (tvs, nt) => tvs union typeVariables(nt._2) }
  }

  def unify(t: Type, u: Type, s: Substitution): Substitution = (s(t), s(u)) match {
    case (TVariable(a), TVariable(b)) if a == b =>
      s
    case (TVariable(a), _) if !(typeVariables(u) contains a) =>
      s.extend(TVariable(a), u)
    case (_, TVariable(a)) =>
      unify(u, t, s)
    case (TInt, TInt) =>
      s
    case (TShort, TShort) =>
      s
    case (TByte, TByte) =>
      s
    case (TLong, TLong) =>
      s
    case (TFloat, TFloat) =>
      s
    case (TDouble, TDouble) =>
      s
    case (TBoolean, TBoolean) =>
      s
    case (TUnit, TUnit) =>
      s
    case (TDynamic, TDynamic) =>
      s
    case (r1@TRecordReference(t1, t2), r2@TRecordReference(u1, u2)) if t1 == u1 =>
      if(t2.length != u2.length) {
        typeError(current.location, s"type constructor arity mismatch: ${r1} != ${r2}")
      }
      (t2 zip u2).foldLeft(s) { case (s, (t, u)) =>
        unify(t, u, s)
      }
    case (TFunction(t1, t2), TFunction(u1, u2)) if t1.size == u1.size =>
      unify(t2, u2, (t1 zip u1).foldLeft(s){ case (s, (t, u)) => unify(t, u, s)})
    case (TConstructor(k1, ts), TConstructor(k2, us)) if k1 == k2 =>
      (ts zip us).foldLeft(s){ case (s, (t, u)) => unify(t, u, s)}
    case _ =>
      typeError(current.location, s"cannot unify ${s(t)} with ${s(u)}")
  }

  def toRow(bindings: List[(String, Type)]): Row = bindings match {
    case (n, t) :: tl => RowExtension(n, t, toRow(tl))
    case Nil => EmptyRow
  }

  def toList(row: Row): List[(String, Type)] = row match {
    case RowExtension(l, t, extension) => (l -> t) :: toList(extension)
    case EmptyRow => Nil
  }

  def processRecords(recordDeclarations :List[RecordDeclaration]): RecordEnvironment = {
    val headers = recordDeclarations.map{d => (d.name, d.ts) }.toMap
    var recordEnvironment: RecordEnvironment = Map.empty
    recordDeclarations.foreach{recordDeclaration =>
      val recordName = recordDeclaration.name
      val location = recordDeclaration.location
      val members: List[(String, Type)] = recordDeclaration.members.map{ case (n, t) =>
          t match {
            case TRecordReference(rname, rtypes) if recordName == rname =>
              val ts = headers(recordName)
              if(ts.length != rtypes.length) {
                typeError(location, s"type variables length mismatch: required: ${ts.length} actual: ${rtypes.length}")
              }
              (n, t)
            case TRecordReference(rname, rtypes) if !headers.contains(rname) =>
              typeError(location, s"record ${rname} is not found")
            case _ =>
              (n, t)
          }
      }
      val ts = headers(recordName)
      recordEnvironment += (recordName -> TRecord(ts, toRow(members)))
    }
    recordEnvironment
  }

  def typeOf(e: AST, environment: Environment = BuiltinEnvironment, records: RecordEnvironment = BuiltinRecordEnvironment, modules: ModuleEnvironment = BuiltinModuleEnvironment): Type = {
    val a = newTypeVariable()
    val r = new SyntaxRewriter
    val (typedE, s) = doType(r.doRewrite(e), TypeEnvironment(environment, Set.empty, records, modules, None), a, EmptySubstitution)
    s(a)
  }

  var current: AST = null
  def doType(e: AST, env: TypeEnvironment, t: Type, s0: Substitution): (TypedAST, Substitution) = {
    current = e
    e match {
      case AST.Block(location, expressions) =>
        expressions match {
          case Nil =>
            (TypedAST.Block(TUnit, location, Nil), s0)
          case x::Nil =>
            val (typedX, newSub) = doType(x, env, t, s0)
            (TypedAST.Block(newSub(t), location, typedX::Nil), newSub)
          case x::xs =>
            val t = newTypeVariable()
            val ts = xs.map{_ => newTypeVariable()}
            val (result, s1) = doType(x, env, t, s0)
            val (reversedTypedElements, s2) = (xs zip ts).foldLeft((result::Nil, s0)){ case ((a, s), (e, t)) =>
              val (e2, s2) = doType(e, env, t, s)
              (e2::a, s2)
            }
            (TypedAST.Block(s2(ts.last), location, reversedTypedElements.reverse), s2)
        }
      case AST.IntNode(location, value) =>
        val newSub = unify(t, TInt, s0)
        (TypedAST.IntNode(newSub(t), location, value), newSub)
      case AST.ShortNode(location, value) =>
        val newSub = unify(t, TShort, s0)
        (TypedAST.ShortNode(newSub(t), location, value), newSub)
      case AST.ByteNode(location, value) =>
        val newSub = unify(t, TByte, s0)
        (TypedAST.ByteNode(newSub(t), location, value), newSub)
      case AST.LongNode(location, value) =>
        val newSub = unify(t, TLong, s0)
        (TypedAST.LongNode(newSub(t), location, value), newSub)
      case AST.FloatNode(location, value) =>
        val newSub = unify(t, TFloat, s0)
        (TypedAST.FloatNode(newSub(t), location, value), newSub)
      case AST.DoubleNode(location, value) =>
        val newSub = unify(t, TDouble, s0)
        (TypedAST.DoubleNode(newSub(t), location, value), newSub)
      case AST.BooleanNode(location, value) =>
        val newSub = unify(t, TBoolean, s0)
        (TypedAST.BooleanNode(newSub(t), location, value), newSub)
      case AST.SimpleAssignment(location, variable, value) =>
        if(env.immutableVariables.contains(variable)) {
          typeError(location, s"variable '$variable' cannot change")
        }
        env.lookup(variable) match {
          case None =>
            typeError(location, s"variable $variable is not defined")
          case Some(variableType) =>
            val (typedValue, s1) = doType(value, env, t, s0)
            val s2 = unify(variableType.description, typedValue.description, s1)
            (TypedAST.Assignment(variableType.description, location, variable, typedValue), s2)
        }
      case AST.IfExpression(location, cond, pos, neg) =>
        val (typedCondition, newSub1) = doType(cond, env, TBoolean, s0)
        val (posTyped, newSub2) = doType(pos, env, t, newSub1)
        val (negTyped, newSub3) = doType(neg, env, t, newSub2)
        (TypedAST.IfExpression(newSub3(t), location, typedCondition, posTyped, negTyped), newSub3)
      case AST.WhileExpression(location, condition, body) =>
        val a = newTypeVariable()
        val b = newTypeVariable()
        val c = newTypeVariable()
        val (typedCondition, s1) = doType(condition, env, a, s0)
        if(typedCondition.description != TBoolean) {
          typeError(location, s"condition type must be Boolean, actual: ${typedCondition.description}")
        } else {
          val (typedBody, s2) = doType(body, env, b, s1)
          val s3 = unify(TUnit, t, s2)
          (TypedAST.WhileExpression(TUnit, location, typedCondition, typedBody), s3)
        }
      case AST.BinaryExpression(location, Operator.EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (TInt, TInt) =>
            (TBoolean, s2)
          case (TLong, TLong) =>
            (TBoolean, s2)
          case (TShort, TShort) =>
            (TBoolean, s2)
          case (TByte, TByte) =>
            (TBoolean, s2)
          case (TFloat, TFloat) =>
            (TBoolean, s2)
          case (TDouble, TDouble) =>
            (TBoolean, s2)
          case (TBoolean, TBoolean) =>
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (a@TConstructor(n1, ts1), b@TConstructor(n2, ts2)) if n2 == n2  && ts1.length == ts2.length =>
            val sx = (ts1 zip ts2).foldLeft(s0) { case (s, (t1, t2)) =>
                unify(t1, t2, s)
            }
            (sx(a), sx)
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.EQUAL, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.LESS_THAN, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (TInt, TInt) =>
            (TBoolean, s2)
          case (TLong, TLong) =>
            (TBoolean, s2)
          case (TShort, TShort) =>
            (TBoolean, s2)
          case (TByte, TByte) =>
            (TBoolean, s2)
          case (TFloat, TFloat) =>
            (TBoolean, s2)
          case (TDouble, TDouble) =>
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.LESS_THAN, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.GREATER_THAN, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (TInt, TInt) =>
            (TBoolean, s2)
          case (TLong, TLong) =>
            (TBoolean, s2)
          case (TShort, TShort) =>
            (TBoolean, s2)
          case (TByte, TByte) =>
            (TBoolean, s2)
          case (TFloat, TFloat) =>
            (TBoolean, s2)
          case (TDouble, TDouble) =>
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.GREATER_THAN, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.LESS_OR_EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (TInt, TInt) =>
            (TBoolean, s2)
          case (TLong, TLong) =>
            (TBoolean, s2)
          case (TShort, TShort) =>
            (TBoolean, s2)
          case (TByte, TByte) =>
            (TBoolean, s2)
          case (TFloat, TFloat) =>
            (TBoolean, s2)
          case (TDouble, TDouble) =>
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.LESS_OR_EQUAL, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.GREATER_EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (TInt, TInt) =>
            (TBoolean, s2)
          case (TLong, TLong) =>
            (TBoolean, s2)
          case (TShort, TShort) =>
            (TBoolean, s2)
          case (TByte, TByte) =>
            (TBoolean, s2)
          case (TFloat, TFloat) =>
            (TBoolean, s2)
          case (TDouble, TDouble) =>
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.GREATER_EQUAL, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.ADD, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (TInt, TInt) =>
            (TInt, s2)
          case (TLong, TLong) =>
            (TLong, s2)
          case (TShort, TShort) =>
            (TShort, s2)
          case (TByte, TByte) =>
            (TByte, s2)
          case (TFloat, TFloat) =>
            (TFloat, s2)
          case (TDouble, TDouble) =>
            (TDouble, s2)
          case (TDynamic, TDynamic) =>
            (TDynamic, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (y, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (x, unify(x, y, s2))
          case (TDynamic, other) =>
            (TDynamic, s2)
          case (other, TDynamic) =>
            (TDynamic, s2)
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TInt, s4)
        }
        val s4 = unify(resultType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.SUBTRACT, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (TInt, TInt) =>
            (TInt, s2)
          case (TLong, TLong) =>
            (TLong, s2)
          case (TShort, TShort) =>
            (TShort, s2)
          case (TByte, TByte) =>
            (TByte, s2)
          case (TFloat, TFloat) =>
            (TFloat, s2)
          case (TDouble, TDouble) =>
            (TDouble, s2)
          case (TDynamic, TDynamic) =>
            (TDynamic, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (y, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (x, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TInt, s4)
        }
        val s4 = unify(resultType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.SUBTRACT, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.MULTIPLY, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (TInt, TInt) =>
            (TInt, s2)
          case (TLong, TLong) =>
            (TLong, s2)
          case (TShort, TShort) =>
            (TShort, s2)
          case (TByte, TByte) =>
            (TByte, s2)
          case (TFloat, TFloat) =>
            (TFloat, s2)
          case (TDouble, TDouble) =>
            (TDouble, s2)
          case (TDynamic, TDynamic) =>
            (TDynamic, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (y, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (x, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TInt, s4)
        }
        val s4 = unify(resultType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.MULTIPLY, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.DIVIDE, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (TInt, TInt) =>
            (TInt, s2)
          case (TLong, TLong) =>
            (TLong, s2)
          case (TShort, TShort) =>
            (TShort, s2)
          case (TByte, TByte) =>
            (TByte, s2)
          case (TFloat, TFloat) =>
            (TFloat, s2)
          case (TDouble, TDouble) =>
            (TDouble, s2)
          case (TDynamic, TDynamic) =>
            (TDynamic, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (y, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (x, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TInt, s4)
        }
        val s4 = unify(resultType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.DIVIDE, typedLhs, typedRhs), s4)
      case AST.MinusOp(location, operand) =>
        val a = newTypeVariable()
        val (typedOperand, s1) = doType(operand, env, a, s0)
        val (resultType, s2) = s1(a) match {
          case TInt  =>
            (TInt, s1)
          case TLong =>
            (TLong, s1)
          case TShort =>
            (TShort, s1)
          case TByte =>
            (TByte, s1)
          case TFloat  =>
            (TFloat, s1)
          case TDouble =>
            (TDouble, s1)
          case TDynamic =>
            (TDynamic, s1)
          case operandType =>
            val s2 = unify(TInt, operandType, s1)
            (TInt, s2)
        }
        (TypedAST.MinusOp(resultType, location, typedOperand), s2)
      case AST.PlusOp(location, operand) =>
        val a = newTypeVariable()
        val (typedOperand, s1) = doType(operand, env, a, s0)
        val (resultType, s2) = s1(a) match {
          case TInt  =>
            (TInt, s1)
          case TLong =>
            (TLong, s1)
          case TShort =>
            (TShort, s1)
          case TByte =>
            (TByte, s1)
          case TFloat  =>
            (TFloat, s1)
          case TDouble =>
            (TDouble, s1)
          case TDynamic =>
            (TDynamic, s1)
          case operandType =>
            val s2 = unify(TInt, operandType, s1)
            (TInt, s2)
        }
        (TypedAST.PlusOp(resultType, location, typedOperand), s2)
      case AST.BinaryExpression(location, Operator.AND2, lhs, rhs) =>
        val (typedLhs, s1) = doType(lhs, env, TBoolean, s0)
        val (typedRhs, s2) = doType(rhs, env, TBoolean, s1)
        val s3 = unify(TBoolean, t, s2)
        (TypedAST.BinaryExpression(TBoolean, location, Operator.AND2, typedLhs, typedRhs), s3)
      case AST.BinaryExpression(location, Operator.BAR2, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, TBoolean, s0)
        val (typedRhs, s2) = doType(rhs, env, TBoolean, s1)
        val s3 = unify(TBoolean, t, s2)
        (TypedAST.BinaryExpression(TBoolean, location, Operator.BAR2, typedLhs, typedRhs), s3)
      case AST.StringNode(location, value) =>
        val s1 = unify(TDynamic, t, s0)
        (TypedAST.StringNode(TDynamic, location, value), s1)
      case AST.Id(location, name) =>
        val s1 = env.lookup(name) match {
          case None => typeError(location, s"variable '${name}' is not found")
          case Some(u) => unify(newInstanceFrom(u), t, s0)
        }
        val resultType = s1(t)
        (TypedAST.Id(resultType, location, name), s1)
      case AST.Selector(location, module, name) =>
        val s1 = env.lookupModuleMember(module, name) match {
          case None => typeError(location, s"module '${module}' or member '${name}' is not found")
          case Some(u) => unify(newInstanceFrom(u), t, s0)
        }
        val resultType = s1(t)
        (TypedAST.Selector(resultType, location, module, name), s1)
      case AST.RecordSelect(location, expression, memberName) =>
        val t0 = newTypeVariable()
        val (te, s1) = doType(expression, env, t0, s0)
        te.description match {
          case TRecordReference(recordName, paramTypes)=>
            env.records.get(recordName) match {
              case None =>
                typeError(location, s"record ${recordName} is not found")
              case Some(record) =>
                val members = toList(record.row)
                members.find{ case (mname, mtype) => memberName == mname} match {
                  case None =>
                    throw typeError(location, s"member ${memberName} is not found in record ${recordName}")
                  case Some((mname, mtype)) =>
                    val sx = unify(mtype, t, s1)
                    (TypedAST.AccessRecord(sx(t), location, te, mname), sx)
                }
            }
          case t =>
            typeError(location, s"${t} is not record type")
        }
      case AST.Lambda(location, params, optionalType, body) =>
        val b = optionalType.getOrElse(newTypeVariable())
        val ts = params.map{p => p.optionalType.getOrElse(newTypeVariable())}
        val as = (params zip ts).map{ case (p, t) => p.name -> TScheme(List(), t) }
        val s1 = unify(t, TFunction(ts, b), s0)
        val env1 = as.foldLeft(env) { case (env, (name, scheme)) => env.updateImmuableVariable(name, scheme)}
        val (typedBody, s2) = doType(body, env1, b, s1)
        (TypedAST.FunctionLiteral(s2(t), location, params, optionalType, typedBody), s2)
      case AST.Let(location, variable, optionalType, value, body, immutable) =>
        if(env.variables.contains(variable)) {
          typeError(location, s"variable $variable is already defined")
        }
        val a = optionalType.getOrElse(newTypeVariable())
        val (typedValue, s1) = doType(value, env, a, s0)
        val s2 = unify(s1(a), typedValue.description, s1)
        val gen = generalize(s2(a), env.variables)
        val declaredType = s2(a)
        val newEnv = if(immutable) {
          env.updateImmuableVariable(variable, generalize(declaredType, env.variables))
        } else {
          env.updateMutableVariable(variable, generalize(declaredType, env.variables))
        }
        val (typedBody, s3) = doType(body, newEnv, t, s2)
        (TypedAST.LetDeclaration(typedBody.description, location, variable, declaredType, typedValue, typedBody, immutable), s3)
      case AST.LetRec(location, variable, value, cleanup, body) =>
        if(env.variables.contains(variable)) {
          throw new InterruptedException(s"${location.format} function ${variable} is already defined")
        }
        val a = newTypeVariable()
        val b = newTypeVariable()
        val (typedE1, s1) = doType(value, env.updateImmuableVariable(variable, TScheme(List(), a)), b, s0)
        val s2 = unify(a, b, s1)
        val (typedE2, s3) = doType(body, env.updateImmuableVariable(variable, generalize(s2(a), s2(env.variables))), t, s2)
        val x = newTypeVariable()
        val (typedCleanup, s4) = cleanup.map{c => doType(c, env, x, s3)} match {
          case Some((c, s)) => (Some(c), s)
          case None => (None, s3)
        }
        (TypedAST.LetFunctionDefinition(typedE2.description, location, variable, typedE1.asInstanceOf[TypedAST.FunctionLiteral], typedCleanup, typedE2), s4)
      case AST.FunctionCall(location, e1, ps) =>
        val t2 = ps.map{_ => newTypeVariable()}
        val (typedTarget, s1) = doType(e1, env, TFunction(t2, t), s0)
        val (tparams, s2) = (ps zip t2).foldLeft((Nil:List[TypedAST], s1)){ case ((tparams, s), (e, t)) =>
          val (tparam, sx) = doType(e, env, t, s)
          (tparam::tparams, sx)
        }
        (TypedAST.FunctionCall(s2(t), location, typedTarget, tparams.reverse), s2)
      case AST.ListLiteral(location, elements) =>
        val a = newTypeVariable()
        val listOfA = listOf(a)
        val (tes, sx) = elements.foldLeft((Nil:List[TypedAST], s0)){ case ((tes, s), e) =>
          val (te, sx) = doType(e, env, a, s)
          (te::tes, sx)
        }
        val sy = unify(listOfA, t, sx)
        (TypedAST.ListLiteral(sy(t), location, tes.reverse), sy)
      case AST.SetLiteral(location, elements) =>
        val a = newTypeVariable()
        val setOfA = setOf(a)
        val (tes, sx) = elements.foldLeft((Nil:List[TypedAST], s0)){ case ((tes, s), e) =>
          val (te, sx) = doType(e, env, a, s)
          (te::tes, sx)
        }
        val sy = unify(setOfA, t, sx)
        (TypedAST.SetLiteral(sy(t), location, tes.reverse), sy)
      case AST.MapLiteral(location, elements) =>
        val kt = newTypeVariable()
        val vt = newTypeVariable()
        val mapOfKV = mapOf(kt, vt)
        val (tes, sx) = elements.foldLeft((Nil:List[(TypedAST, TypedAST)], s0)){ case ((tes, s), (k, v)) =>
          val (typedK, sx) = doType(k, env, kt, s)
          val (typedY, sy) = doType(v, env, vt, sx)
          ((typedK -> typedY)::tes, sy)
        }
        val sy = unify(mapOfKV, t, sx)
        (TypedAST.MapLiteral(sy(t), location, tes.reverse), sy)
      case AST.ObjectNew(location, className, params) =>
        val ts = params.map{_ => newTypeVariable()}
        val (tes, sx) = (params zip ts).foldLeft((Nil:List[TypedAST], s0)){ case ((tes, s), (e, t)) =>
          val (te, sx) = doType(e, env, t, s)
          (te::tes, sx)
        }
        val sy = unify(TDynamic, t, sx)
        (TypedAST.NewObject(TDynamic, location, className, tes.reverse), sy)
      case AST.RecordNew(location, recordName, params) =>
        val ts = params.map{_ => newTypeVariable()}
        val (tes1, sx) = (params zip ts).foldLeft((Nil:List[TypedAST], s0)){ case ((tes, s), (e, t)) =>
          val (te, sx) = doType(e, env, t, s)
          (te::tes, sx)
        }
        val tes2 = tes1.reverse
        env.records.get(recordName) match {
          case Some(record) =>
            val xts = record.ts
            val members = toList(record.row)
            val sy = xts.foldLeft(sx) { case (s, t) => s.remove(t)}
            if(members.length != ts.length) {
              typeError(location, s"length mismatch: required: ${members.length}, actual: ${ts.length}")
            }
            val memberTypes = members.map{ case (_, t) => t}
            val parameterTypes = tes2.map { case te => te.description }
            val sn = (memberTypes zip parameterTypes).foldLeft(sy) { case (s, (m, p)) => unify(m, p, s)}
            val recordType = TRecordReference(recordName, xts.map{ t => sn(t)})
            val so = unify(t, recordType, sn)
            (TypedAST.NewRecord(recordType, location, recordName, tes2), so)
          case None =>
            typeError(location, s"record '$recordName' is not found")
        }
      case AST.Casting(location, target, to) =>
        val a = newTypeVariable()
        val (typedTarget, s1) = doType(target, env, a, s0)
        val s2 = unify(t, to, s1)
        (TypedAST.Casting(to, location, typedTarget, to), s2)
      case AST.MethodCall(location, receiver, name, params) =>
        val a = newTypeVariable()
        val (typedReceiver, s1) = doType(receiver, env, a, s0)
        val s2 = unify(s0(a), TDynamic, s1)
        val ts = params.map{_ => newTypeVariable()}
        val (tes, sx) = (params zip ts).foldLeft((Nil:List[TypedAST], s2)){ case ((tes, s), (e, t)) =>
          val (te, sx) = doType(e, env, t, s)
          (te::tes, sx)
        }
        val sy = unify(t, TDynamic, sx)
        (TypedAST.MethodCall(TDynamic, location, typedReceiver, name, tes.reverse), sy)
      case otherwise =>
        throw TyperPanic(otherwise.toString)
    }
  }

  def typeError(location: Location, message: String): Nothing = {
    throw TyperException(s"${location.format} ${message}")
  }

  def transform(program: AST.Program): TypedAST.Program = {
    val tv = newTypeVariable()
    val recordEnvironment: RecordEnvironment = processRecords(program.records)
    val (typedExpression, _) = doType(program.block, TypeEnvironment(BuiltinEnvironment, Set.empty, BuiltinRecordEnvironment ++ recordEnvironment, BuiltinModuleEnvironment, None), tv, EmptySubstitution)
    TypedAST.Program(program.location, Nil, typedExpression.asInstanceOf[TypedAST.Block], recordEnvironment)
  }

  override final val name: String = "Typer"

  override final def process(input: AST.Program): TypedAST.Program = transform(input)
}
