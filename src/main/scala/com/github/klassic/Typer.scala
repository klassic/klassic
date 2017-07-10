package com.github.klassic

import com.github.klassic.AST.RecordDeclaration
import com.github.klassic.Type.{TypeConstructor, _}

import scala.collection.mutable

/**
  * @author Kota Mizushima
  */
class Typer {
  type Environment = Map[String, TypeScheme]
  type ModuleEnvironment = Map[String, Environment]
  type RecordEnvironment = Map[String, (List[TypeVariable], List[(String, TypeScheme)])]
  def listOf(tp: Type): TypeConstructor = {
    TypeConstructor("List", List(tp))
  }
  def mapOf(k: Type, v: Type): TypeConstructor = {
    TypeConstructor("Map", List(k, v))
  }
  def setOf(tp: Type): TypeConstructor = {
    TypeConstructor("Set", List(tp))
  }
  val BuiltinEnvironment: Environment = {
    val a = newTypeVariable()
    val b = newTypeVariable()
    Map(
      "url" -> TypeScheme(List(), FunctionType(List(DynamicType), DynamicType)),
      "uri" -> TypeScheme(List(), FunctionType(List(DynamicType), DynamicType)),
      "substring" -> TypeScheme(List(), FunctionType(List(DynamicType, IntType, IntType), DynamicType)),
      "at" -> TypeScheme(List(), FunctionType(List(DynamicType, IntType), DynamicType)),
      "matches" -> TypeScheme(List(), FunctionType(List(DynamicType, DynamicType), BooleanType)),
      "thread" -> TypeScheme(List(), FunctionType(List(FunctionType(List.empty, DynamicType)), DynamicType)),
      "println" ->  TypeScheme(List(tv("x")), FunctionType(List(tv("x")), UnitType)),
      "stopwatch" -> TypeScheme(List(), FunctionType(List(FunctionType(List.empty, DynamicType)), IntType)),
      "sleep" -> TypeScheme(List(), IntType ==> UnitType),
      "isEmpty" -> TypeScheme(List(tv("a")), listOf(tv("a")) ==> BooleanType),
      "ToDo" -> TypeScheme(List(tv("a")), FunctionType(List(), tv("a"))),
      "assert" -> TypeScheme(List(tv("a")), BooleanType ==> UnitType),
      "assertResult" -> TypeScheme(List(tv("a")), tv("a") ==> (tv("a") ==> UnitType)),
      "map" -> TypeScheme(List(tv("a"), tv("b")), listOf(tv("a")) ==> ((tv("a") ==> tv("b"))  ==> listOf(tv("b")))),
      "head" -> TypeScheme(List(tv("a")), listOf(tv("a")) ==> tv("a")),
      "tail" -> TypeScheme(List(tv("a")), listOf(tv("a")) ==> listOf(tv("a"))),
      "cons" -> TypeScheme(List(tv("a")), tv("a") ==> (listOf(tv("a")) ==> listOf(tv("a")))),
      "size" -> TypeScheme(List(tv("a")), listOf(tv("a")) ==> IntType),
      "foldLeft" -> TypeScheme(List(tv("a"), tv("b")), listOf(tv("a")) ==> (tv("b") ==> ((List(tv("b"), tv("a")) ==> tv("b")) ==> tv("b")))),
      "null" -> TypeScheme(List(tv("a")), tv("a")),
      "desktop" -> TypeScheme(List(), Nil ==> DynamicType)
    )
  }

  val BuiltinRecordEnvironment: Map[String, (List[TypeVariable], List[(String, TypeScheme)])] = {
    Map(
      "Point" -> (Nil, List(
        "x" -> TypeScheme(Nil, IntType),
        "y" -> TypeScheme(Nil, IntType)
      ))
    )
  }

  val BuiltinModuleEnvironment: Map[String, Environment] = {
    Map(
      "List" -> Map(
        "cons" -> TypeScheme(List(tv("a")), FunctionType(List(tv("a"), listOf(tv("a"))), listOf(tv("a")))),
        "map" -> TypeScheme(List(tv("a"), tv("b")), listOf(tv("a")) ==> ((tv("a") ==> tv("b"))  ==> listOf(tv("b")))),
        "head" -> TypeScheme(List(tv("a")), listOf(tv("a")) ==> tv("a")),
        "tail" -> TypeScheme(List(tv("a")), listOf(tv("a")) ==> listOf(tv("a"))),
        "size" -> TypeScheme(List(tv("a")), listOf(tv("a")) ==> IntType),
        "isEmpty" -> TypeScheme(List(tv("a")), listOf(tv("a")) ==> BooleanType)
      ),
      "Map" -> Map(
        "add" -> TypeScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> (List(tv("a"), tv("b")) ==> mapOf(tv("a"), tv("b")))),
        "containsKey" -> TypeScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> (tv("a") ==> BooleanType)),
        "containsValue" -> TypeScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> (tv("b") ==> BooleanType)),
        "get" -> TypeScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> (tv("a") ==> tv("b"))),
        "size" -> TypeScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> IntType),
        "isEmpty" -> TypeScheme(List(tv("a"), tv("b")), mapOf(tv("a"), tv("b")) ==> BooleanType)
      ),
      "Set" -> Map(
        "add" -> TypeScheme(List(tv("a")), setOf(tv("a")) ==> (tv("a") ==> setOf(tv("a")))),
        "remove" -> TypeScheme(List(tv("a")), setOf(tv("a")) ==> (tv("a") ==> setOf(tv("a")))),
        "contains" -> TypeScheme(List(tv("a")), setOf(tv("a")) ==> (tv("a") ==> BooleanType)),
        "size" -> TypeScheme(List(tv("a")), setOf(tv("a")) ==> IntType),
        "isEmpty" -> TypeScheme(List(tv("a")), setOf(tv("a")) ==> BooleanType)
      )
    )
  }

  def newInstanceFrom(scheme: TypeScheme): Type = {
    scheme.typeVariables.foldLeft(EmptySubstitution)((s, tv) => s.extend(tv, newTypeVariable())).apply(scheme.description)
  }
  private var n: Int = 0
  def newTypeVariable(): Type = {
    n += 1; TypeVariable("'a" + n)
  }
  val EmptySubstitution: Substitution = new Substitution(Map.empty)
  class Substitution(val map: Map[TypeVariable, Type]) extends Function1[Type, Type] {
    def lookup(x: TypeVariable): Type = {
      map.getOrElse(x, x)
    }

    def apply(t: Type): Type = t match {
      case tv@TypeVariable(a) =>
        val u = lookup(tv)
        if (t == u) t else apply(u)
      case FunctionType(t1, t2) => FunctionType(t1.map{t => apply(t)}, apply(t2))
      case RecordType(name, ts) => RecordType(name, ts.map{t => apply(t)})
      case IntType => IntType
      case ShortType => ShortType
      case ByteType => ByteType
      case LongType => LongType
      case FloatType => FloatType
      case DoubleType => DoubleType
      case BooleanType => BooleanType
      case UnitType => UnitType
      case DynamicType => DynamicType
      case ErrorType => ErrorType
      case TypeConstructor(name, args) => TypeConstructor(name, args.map{arg => apply(arg)})
    }

    def apply(env: Environment): Environment = {
      env.map { case (x, ts) =>
          x -> TypeScheme(typeVariables(ts), this.apply(ts.description))
      }
    }

    def extend(tv: TypeVariable, td: Type): Substitution = new Substitution(this.map + (tv -> td))

    def remove(tv: TypeVariable): Substitution = new Substitution(this.map - tv)
  }

  def lookup(x: String, environment: Environment): Option[TypeScheme] = environment.get(x) match {
    case Some(t) => Some(t)
    case None => None
  }

  def generalize(t: Type, environment: Environment): TypeScheme = {
    TypeScheme(typeVariables(t) diff typeVariables(environment), t)
  }

  def typeVariables(t: Type): List[TypeVariable] = t match {
    case tv @ TypeVariable(a) =>
      List(tv)
    case IntType =>
      Nil
    case ShortType =>
      Nil
    case ByteType =>
      Nil
    case LongType =>
      Nil
    case FloatType =>
      Nil
    case DoubleType =>
      Nil
    case BooleanType =>
      Nil
    case UnitType =>
      Nil
    case DynamicType =>
      Nil
    case ErrorType =>
      Nil
    case FunctionType(t1, t2) =>
      t1.flatMap{typeVariables} union typeVariables(t2)
    case RecordType(name, ts) =>
      List(ts.flatMap{ case t => typeVariables(t)}:_*)
    case TypeConstructor(k, ts) =>
      ts.foldLeft(List[TypeVariable]()){(tvs, t) => tvs union typeVariables(t)}
  }

  def typeVariables(ts: TypeScheme): List[TypeVariable] = {
    typeVariables(ts.description) diff ts.typeVariables
  }

  def typeVariables(environment: Environment): List[TypeVariable] = {
    environment.foldLeft(List[TypeVariable]()) { (tvs, nt) => tvs union typeVariables(nt._2) }
  }

  def unify(t: Type, u: Type, s: Substitution): Substitution = (s(t), s(u)) match {
    case (TypeVariable(a), TypeVariable(b)) if a == b =>
      s
    case (TypeVariable(a), _) if !(typeVariables(u) contains a) =>
      s.extend(TypeVariable(a), u)
    case (_, TypeVariable(a)) =>
      unify(u, t, s)
    case (IntType, IntType) =>
      s
    case (ShortType, ShortType) =>
      s
    case (ByteType, ByteType) =>
      s
    case (LongType, LongType) =>
      s
    case (FloatType, FloatType) =>
      s
    case (DoubleType, DoubleType) =>
      s
    case (BooleanType, BooleanType) =>
      s
    case (UnitType, UnitType) =>
      s
    case (DynamicType, DynamicType) =>
      s
    case (RecordType(t1, t2), RecordType(u1, u2)) if t1 == u1 =>
      if(t2.length != u2.length) {
        throw TyperException(s"${current.location.format} type constructor arity mismatch: ${u2.length} != ${t2.length}")
      }
      (t2 zip u2).foldLeft(s) { case (s, (t, u)) =>
        unify(t, u, s)
      }
    case (FunctionType(t1, t2), FunctionType(u1, u2)) if t1.size == u1.size =>
      unify(t2, u2, (t1 zip u1).foldLeft(s){ case (s, (t, u)) => unify(t, u, s)})
    case (TypeConstructor(k1, ts), TypeConstructor(k2, us)) if k1 == k2 =>
      (ts zip us).foldLeft(s){ case (s, (t, u)) => unify(t, u, s)}
    case _ =>
      throw TyperException(
        s"""
           | cannot unify ${s(t)} with ${s(u)}
           | location: ${current.location.format}
         """.stripMargin
      )
  }

  def processRecords(recordDeclarations :List[RecordDeclaration]): RecordEnvironment = {
    val headers = recordDeclarations.map{d => (d.name, d.ts) }.toMap
    var recordEnvironment: RecordEnvironment = Map.empty
    recordDeclarations.foreach{r =>
      val recordName = r.name
      val location = r.location
      val members: List[(String, TypeScheme)] = r.members.map{ case (n, t) =>
          t match {
            case RecordType(rname, rtypes) if recordName == rname =>
              val ts = headers(recordName)
              if(ts.length != rtypes.length) {
                throw TyperException(s"${location.format} type variables mismatch: required: ${ts.length} actual: ${rtypes.length}")
              }
              (n, TypeScheme(Nil, t))
            case RecordType(rname, rtypes) if !headers.contains(rname) =>
              throw TyperException(s"${location.format} record ${rname} is not found")
            case _ =>
              (n, TypeScheme(Nil, t))
          }
      }
      val ts = headers(recordName)
      recordEnvironment += (recordName -> (ts, members))
    }
    recordEnvironment
  }

  def typeOf(e: AST, environment: Environment = BuiltinEnvironment, records: RecordEnvironment = BuiltinRecordEnvironment, modules: ModuleEnvironment = BuiltinModuleEnvironment): Type = {
    val a = newTypeVariable()
    val r = new SyntaxRewriter
    val (typedE, s) = doType(r.doRewrite(e), TypeEnvironment(environment, Set.empty, records, modules, None), a, EmptySubstitution)
    s(a)
  }

  def transform(program: AST.Program): TypedAST.Program = {
    val tv = newTypeVariable()
    val recordEnvironment: RecordEnvironment = processRecords(program.records)
    val (typedExpression, _) = doType(program.block, TypeEnvironment(BuiltinEnvironment, Set.empty, BuiltinRecordEnvironment ++ recordEnvironment, BuiltinModuleEnvironment, None), tv, EmptySubstitution)
    TypedAST.Program(program.location, Nil, typedExpression.asInstanceOf[TypedAST.Block], recordEnvironment)
  }

  var current: AST = null
  def doType(e: AST, env: TypeEnvironment, t: Type, s0: Substitution): (TypedAST, Substitution) = {
    current = e
    e match {
      case AST.Block(location, expressions) =>
        expressions match {
          case Nil =>
            (TypedAST.Block(UnitType, location, Nil), s0)
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
        val newSub = unify(t, IntType, s0)
        (TypedAST.IntNode(newSub(t), location, value), newSub)
      case AST.ShortNode(location, value) =>
        val newSub = unify(t, ShortType, s0)
        (TypedAST.ShortNode(newSub(t), location, value), newSub)
      case AST.ByteNode(location, value) =>
        val newSub = unify(t, ByteType, s0)
        (TypedAST.ByteNode(newSub(t), location, value), newSub)
      case AST.LongNode(location, value) =>
        val newSub = unify(t, LongType, s0)
        (TypedAST.LongNode(newSub(t), location, value), newSub)
      case AST.FloatNode(location, value) =>
        val newSub = unify(t, FloatType, s0)
        (TypedAST.FloatNode(newSub(t), location, value), newSub)
      case AST.DoubleNode(location, value) =>
        val newSub = unify(t, DoubleType, s0)
        (TypedAST.DoubleNode(newSub(t), location, value), newSub)
      case AST.BooleanNode(location, value) =>
        val newSub = unify(t, BooleanType, s0)
        (TypedAST.BooleanNode(newSub(t), location, value), newSub)
      case AST.SimpleAssignment(location, variable, value) =>
        if(env.immutableVariables.contains(variable)) {
          throw TyperException(s"${location.format} variable '$variable' cannot change")
        }
        env.lookup(variable) match {
          case None =>
            throw TyperException(s"${location.format} variable $variable is not defined")
          case Some(variableType) =>
            val (typedValue, s1) = doType(value, env, t, s0)
            val s2 = unify(variableType.description, typedValue.description, s1)
            (TypedAST.Assignment(variableType.description, location, variable, typedValue), s2)
        }
      case AST.IfExpression(location, cond, pos, neg) =>
        val (typedCondition, newSub1) = doType(cond, env, BooleanType, s0)
        val (posTyped, newSub2) = doType(pos, env, t, newSub1)
        val (negTyped, newSub3) = doType(neg, env, t, newSub2)
        (TypedAST.IfExpression(newSub3(t), location, typedCondition, posTyped, negTyped), newSub3)
      case AST.WhileExpression(location, condition, body) =>
        val a = newTypeVariable()
        val b = newTypeVariable()
        val c = newTypeVariable()
        val (typedCondition, s1) = doType(condition, env, a, s0)
        if(typedCondition.description != BooleanType) {
          throw TyperException(s"${location.format} condition type must be Boolean, actual: ${typedCondition.description}")
        } else {
          val (typedBody, s2) = doType(body, env, b, s1)
          val s3 = unify(UnitType, t, s2)
          (TypedAST.WhileExpression(UnitType, location, typedCondition, typedBody), s3)
        }
      case AST.BinaryExpression(location, Operator.EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (IntType, IntType) =>
            (BooleanType, s2)
          case (LongType, LongType) =>
            (BooleanType, s2)
          case (ShortType, ShortType) =>
            (BooleanType, s2)
          case (ByteType, ByteType) =>
            (BooleanType, s2)
          case (FloatType, FloatType) =>
            (BooleanType, s2)
          case (DoubleType, DoubleType) =>
            (BooleanType, s2)
          case (BooleanType, BooleanType) =>
            (BooleanType, s2)
          case (DynamicType, DynamicType) =>
            (BooleanType, s2)
          case (x: TypeVariable, y) if !y.isInstanceOf[TypeVariable] =>
            (BooleanType, unify(x, y, s2))
          case (x, y: TypeVariable) if !x.isInstanceOf[TypeVariable] =>
            (BooleanType, unify(x, y, s2))
          case (a@TypeConstructor(n1, ts1), b@TypeConstructor(n2, ts2)) if n2 == n2  && ts1.length == ts2.length =>
            val sx = (ts1 zip ts2).foldLeft(s0) { case (s, (t1, t2)) =>
                unify(t1, t2, s)
            }
            (sx(a), sx)
          case (ltype, rtype) =>
            val s3 = unify(IntType, ltype, s2)
            val s4 = unify(IntType, rtype, s3)
            (BooleanType, s4)
        }
        val s4 = unify(BooleanType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.EQUAL, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.LESS_THAN, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (IntType, IntType) =>
            (BooleanType, s2)
          case (LongType, LongType) =>
            (BooleanType, s2)
          case (ShortType, ShortType) =>
            (BooleanType, s2)
          case (ByteType, ByteType) =>
            (BooleanType, s2)
          case (FloatType, FloatType) =>
            (BooleanType, s2)
          case (DoubleType, DoubleType) =>
            (BooleanType, s2)
          case (DynamicType, DynamicType) =>
            (BooleanType, s2)
          case (x: TypeVariable, y) if !y.isInstanceOf[TypeVariable] =>
            (BooleanType, unify(x, y, s2))
          case (x, y: TypeVariable) if !x.isInstanceOf[TypeVariable] =>
            (BooleanType, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(IntType, ltype, s2)
            val s4 = unify(IntType, rtype, s3)
            (BooleanType, s4)
        }
        val s4 = unify(BooleanType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.LESS_THAN, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.GREATER_THAN, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (IntType, IntType) =>
            (BooleanType, s2)
          case (LongType, LongType) =>
            (BooleanType, s2)
          case (ShortType, ShortType) =>
            (BooleanType, s2)
          case (ByteType, ByteType) =>
            (BooleanType, s2)
          case (FloatType, FloatType) =>
            (BooleanType, s2)
          case (DoubleType, DoubleType) =>
            (BooleanType, s2)
          case (DynamicType, DynamicType) =>
            (BooleanType, s2)
          case (x: TypeVariable, y) if !y.isInstanceOf[TypeVariable] =>
            (BooleanType, unify(x, y, s2))
          case (x, y: TypeVariable) if !x.isInstanceOf[TypeVariable] =>
            (BooleanType, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(IntType, ltype, s2)
            val s4 = unify(IntType, rtype, s3)
            (BooleanType, s4)
        }
        val s4 = unify(BooleanType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.GREATER_THAN, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.LESS_OR_EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (IntType, IntType) =>
            (BooleanType, s2)
          case (LongType, LongType) =>
            (BooleanType, s2)
          case (ShortType, ShortType) =>
            (BooleanType, s2)
          case (ByteType, ByteType) =>
            (BooleanType, s2)
          case (FloatType, FloatType) =>
            (BooleanType, s2)
          case (DoubleType, DoubleType) =>
            (BooleanType, s2)
          case (DynamicType, DynamicType) =>
            (BooleanType, s2)
          case (x: TypeVariable, y) if !y.isInstanceOf[TypeVariable] =>
            (BooleanType, unify(x, y, s2))
          case (x, y: TypeVariable) if !x.isInstanceOf[TypeVariable] =>
            (BooleanType, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(IntType, ltype, s2)
            val s4 = unify(IntType, rtype, s3)
            (BooleanType, s4)
        }
        val s4 = unify(BooleanType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.LESS_OR_EQUAL, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.GREATER_EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (IntType, IntType) =>
            (BooleanType, s2)
          case (LongType, LongType) =>
            (BooleanType, s2)
          case (ShortType, ShortType) =>
            (BooleanType, s2)
          case (ByteType, ByteType) =>
            (BooleanType, s2)
          case (FloatType, FloatType) =>
            (BooleanType, s2)
          case (DoubleType, DoubleType) =>
            (BooleanType, s2)
          case (DynamicType, DynamicType) =>
            (BooleanType, s2)
          case (x: TypeVariable, y) if !y.isInstanceOf[TypeVariable] =>
            (BooleanType, unify(x, y, s2))
          case (x, y: TypeVariable) if !x.isInstanceOf[TypeVariable] =>
            (BooleanType, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(IntType, ltype, s2)
            val s4 = unify(IntType, rtype, s3)
            (BooleanType, s4)
        }
        val s4 = unify(BooleanType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.GREATER_EQUAL, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.ADD, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (IntType, IntType) =>
            (IntType, s2)
          case (LongType, LongType) =>
            (LongType, s2)
          case (ShortType, ShortType) =>
            (ShortType, s2)
          case (ByteType, ByteType) =>
            (ByteType, s2)
          case (FloatType, FloatType) =>
            (FloatType, s2)
          case (DoubleType, DoubleType) =>
            (DoubleType, s2)
          case (DynamicType, DynamicType) =>
            (DynamicType, s2)
          case (x: TypeVariable, y) if !y.isInstanceOf[TypeVariable] =>
            (y, unify(x, y, s2))
          case (x, y: TypeVariable) if !x.isInstanceOf[TypeVariable] =>
            (x, unify(x, y, s2))
          case (DynamicType, other) =>
            (DynamicType, s2)
          case (other, DynamicType) =>
            (DynamicType, s2)
          case (ltype, rtype) =>
            val s3 = unify(IntType, ltype, s2)
            val s4 = unify(IntType, rtype, s3)
            (IntType, s4)
        }
        val s4 = unify(resultType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.SUBTRACT, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (IntType, IntType) =>
            (IntType, s2)
          case (LongType, LongType) =>
            (LongType, s2)
          case (ShortType, ShortType) =>
            (ShortType, s2)
          case (ByteType, ByteType) =>
            (ByteType, s2)
          case (FloatType, FloatType) =>
            (FloatType, s2)
          case (DoubleType, DoubleType) =>
            (DoubleType, s2)
          case (DynamicType, DynamicType) =>
            (DynamicType, s2)
          case (x: TypeVariable, y) if !y.isInstanceOf[TypeVariable] =>
            (y, unify(x, y, s2))
          case (x, y: TypeVariable) if !x.isInstanceOf[TypeVariable] =>
            (x, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(IntType, ltype, s2)
            val s4 = unify(IntType, rtype, s3)
            (IntType, s4)
        }
        val s4 = unify(resultType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.SUBTRACT, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.MULTIPLY, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (IntType, IntType) =>
            (IntType, s2)
          case (LongType, LongType) =>
            (LongType, s2)
          case (ShortType, ShortType) =>
            (ShortType, s2)
          case (ByteType, ByteType) =>
            (ByteType, s2)
          case (FloatType, FloatType) =>
            (FloatType, s2)
          case (DoubleType, DoubleType) =>
            (DoubleType, s2)
          case (DynamicType, DynamicType) =>
            (DynamicType, s2)
          case (x: TypeVariable, y) if !y.isInstanceOf[TypeVariable] =>
            (y, unify(x, y, s2))
          case (x, y: TypeVariable) if !x.isInstanceOf[TypeVariable] =>
            (x, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(IntType, ltype, s2)
            val s4 = unify(IntType, rtype, s3)
            (IntType, s4)
        }
        val s4 = unify(resultType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.MULTIPLY, typedLhs, typedRhs), s4)
      case AST.BinaryExpression(location, Operator.DIVIDE, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2(a), s2(b)) match {
          case (IntType, IntType) =>
            (IntType, s2)
          case (LongType, LongType) =>
            (LongType, s2)
          case (ShortType, ShortType) =>
            (ShortType, s2)
          case (ByteType, ByteType) =>
            (ByteType, s2)
          case (FloatType, FloatType) =>
            (FloatType, s2)
          case (DoubleType, DoubleType) =>
            (DoubleType, s2)
          case (DynamicType, DynamicType) =>
            (DynamicType, s2)
          case (x: TypeVariable, y) if !y.isInstanceOf[TypeVariable] =>
            (y, unify(x, y, s2))
          case (x, y: TypeVariable) if !x.isInstanceOf[TypeVariable] =>
            (x, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(IntType, ltype, s2)
            val s4 = unify(IntType, rtype, s3)
            (IntType, s4)
        }
        val s4 = unify(resultType, t, s3)
        (TypedAST.BinaryExpression(resultType, location, Operator.DIVIDE, typedLhs, typedRhs), s4)
      case AST.MinusOp(location, operand) =>
        val a = newTypeVariable()
        val (typedOperand, s1) = doType(operand, env, a, s0)
        val (resultType, s2) = s1(a) match {
          case IntType  =>
            (IntType, s1)
          case LongType =>
            (LongType, s1)
          case ShortType =>
            (ShortType, s1)
          case ByteType =>
            (ByteType, s1)
          case FloatType  =>
            (FloatType, s1)
          case DoubleType =>
            (DoubleType, s1)
          case DynamicType =>
            (DynamicType, s1)
          case operandType =>
            val s2 = unify(IntType, operandType, s1)
            (IntType, s2)
        }
        (TypedAST.MinusOp(resultType, location, typedOperand), s2)
      case AST.PlusOp(location, operand) =>
        val a = newTypeVariable()
        val (typedOperand, s1) = doType(operand, env, a, s0)
        val (resultType, s2) = s1(a) match {
          case IntType  =>
            (IntType, s1)
          case LongType =>
            (LongType, s1)
          case ShortType =>
            (ShortType, s1)
          case ByteType =>
            (ByteType, s1)
          case FloatType  =>
            (FloatType, s1)
          case DoubleType =>
            (DoubleType, s1)
          case DynamicType =>
            (DynamicType, s1)
          case operandType =>
            val s2 = unify(IntType, operandType, s1)
            (IntType, s2)
        }
        (TypedAST.PlusOp(resultType, location, typedOperand), s2)
      case AST.BinaryExpression(location, Operator.AND2, lhs, rhs) =>
        val (typedLhs, s1) = doType(lhs, env, BooleanType, s0)
        val (typedRhs, s2) = doType(rhs, env, BooleanType, s1)
        val s3 = unify(BooleanType, t, s2)
        (TypedAST.BinaryExpression(BooleanType, location, Operator.AND2, typedLhs, typedRhs), s3)
      case AST.BinaryExpression(location, Operator.BAR2, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, BooleanType, s0)
        val (typedRhs, s2) = doType(rhs, env, BooleanType, s1)
        val s3 = unify(BooleanType, t, s2)
        (TypedAST.BinaryExpression(BooleanType, location, Operator.BAR2, typedLhs, typedRhs), s3)
      case AST.StringNode(location, value) =>
        val s1 = unify(DynamicType, t, s0)
        (TypedAST.StringNode(DynamicType, location, value), s1)
      case AST.Id(location, name) =>
        val s1 = env.lookup(name) match {
          case None => throw TyperException(s"${location.format} variable '${name}' is not found")
          case Some(u) => unify(newInstanceFrom(u), t, s0)
        }
        val resultType = s1(t)
        (TypedAST.Id(resultType, location, name), s1)
      case AST.Selector(location, module, name) =>
        val s1 = env.lookupModuleMember(module, name) match {
          case None => throw TyperException(s"${location.format} module '${module}' or member '${name}' is not found")
          case Some(u) => unify(newInstanceFrom(u), t, s0)
        }
        val resultType = s1(t)
        (TypedAST.Selector(resultType, location, module, name), s1)
      case AST.AccessRecord(location, expression, memberName) =>
        val t0 = newTypeVariable()
        val (te, s1) = doType(expression, env, t0, s0)
        te.description match {
          case RecordType(recordName, paramTypes)=>
            env.records.get(recordName) match {
              case None =>
                throw TyperException(s"${location.format} record ${recordName} is not found")
              case Some(members) =>
                val (xts, xmembers) = members
                xmembers.find{ case (mname, mscheme) => mname == memberName} match {
                  case None =>
                    throw TyperException(s"${location.format} member ${memberName} is not found in record ${recordName}")
                  case Some((mname, mscheme)) =>
                    val sx = unify(mscheme.description, t, s1)
                    (TypedAST.AccessRecord(sx(t), location, te, mname), sx)
                }
            }
          case t =>
            throw TyperException(s"${location.format} ${t} is not record type")
        }
      case AST.Lambda(location, params, optionalType, body) =>
        val b = optionalType.getOrElse(newTypeVariable())
        val ts = params.map{p => p.optionalType.getOrElse(newTypeVariable())}
        val as = (params zip ts).map{ case (p, t) => p.name -> TypeScheme(List(), t) }
        val s1 = unify(t, FunctionType(ts, b), s0)
        val env1 = as.foldLeft(env) { case (env, (name, scheme)) => env.updateImmuableVariable(name, scheme)}
        val (typedBody, s2) = doType(body, env1, b, s1)
        (TypedAST.FunctionLiteral(s2(t), location, params, optionalType, typedBody), s2)
      case AST.Let(location, variable, optionalType, value, body, immutable) =>
        if(env.variables.contains(variable)) {
          throw TyperException(s"${location.format} variable $variable is already defined")
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
        val (typedE1, s1) = doType(value, env.updateImmuableVariable(variable, TypeScheme(List(), a)), b, s0)
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
        val (typedTarget, s1) = doType(e1, env, FunctionType(t2, t), s0)
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
      case AST.NewObject(location, className, params) =>
        val ts = params.map{_ => newTypeVariable()}
        val (tes, sx) = (params zip ts).foldLeft((Nil:List[TypedAST], s0)){ case ((tes, s), (e, t)) =>
          val (te, sx) = doType(e, env, t, s)
          (te::tes, sx)
        }
        val sy = unify(DynamicType, t, sx)
        (TypedAST.NewObject(DynamicType, location, className, tes.reverse), sy)
      case AST.NewRecord(location, recordName, params) =>
        val ts = params.map{_ => newTypeVariable()}
        val (tes1, sx) = (params zip ts).foldLeft((Nil:List[TypedAST], s0)){ case ((tes, s), (e, t)) =>
          val (te, sx) = doType(e, env, t, s)
          (te::tes, sx)
        }
        val tes2 = tes1.reverse
        env.records.get(recordName) match {
          case Some((xts, xmembers)) =>
            val sy = xts.foldLeft(sx) { case (s, t) => s.remove(t)}
            if(xmembers.length != ts.length) {
              throw TyperException(s"${location.format} length mismatch: required: ${xmembers.length}, actual: ${ts.length}")
            }
            val memberTypes = xmembers.map{ case (_, t) => t.description}
            val parameterTypes = tes2.map { case te => te.description }
            val sn = (memberTypes zip parameterTypes).foldLeft(sy) { case (s, (m, p)) => unify(m, p, s)}
            val recordType = RecordType(recordName, xts.map{t => sn(t)})
            val so = unify(t, recordType, sn)
            (TypedAST.NewRecord(recordType, location, recordName, tes2), so)
          case None =>
            throw TyperException(s"${location.format} record '$recordName' is not found")
        }
      case AST.Casting(location, target, to) =>
        val a = newTypeVariable()
        val (typedTarget, s1) = doType(target, env, a, s0)
        val s2 = unify(t, to, s1)
        (TypedAST.Casting(to, location, typedTarget, to), s2)
      case AST.MethodCall(location, receiver, name, params) =>
        val a = newTypeVariable()
        val (typedReceiver, s1) = doType(receiver, env, a, s0)
        val s2 = unify(s0(a), DynamicType, s1)
        val ts = params.map{_ => newTypeVariable()}
        val (tes, sx) = (params zip ts).foldLeft((Nil:List[TypedAST], s2)){ case ((tes, s), (e, t)) =>
          val (te, sx) = doType(e, env, t, s)
          (te::tes, sx)
        }
        val sy = unify(t, DynamicType, sx)
        (TypedAST.MethodCall(DynamicType, location, typedReceiver, name, tes.reverse), sy)
      case otherwise =>
        throw TyperPanic(otherwise.toString)
    }
  }
}
