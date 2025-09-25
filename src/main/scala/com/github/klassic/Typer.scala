package com.github.klassic

import java.lang.reflect.TypeVariable

import com.github.klassic.TypedAst.TypedNode
import com.github.klassic.Ast.Node
import com.github.klassic.Ast.RecordDeclaration
import com.github.klassic.Type.{TConstructor, _}
import com.github.klassic._

import scala.collection.mutable

/**
  * @author Kota Mizushima
  */
class Typer extends Processor[Ast.Program, TypedAst.Program, InteractiveSession] {
  type ModuleEnvironment = Map[String, Environment]
  type RecordEnvironment = Map[String, TRecord]
  type Name = String
  type Label = String
  
  // Track constraints that need to be resolved
  case class ConstraintSet(constraints: List[TConstraint]) {
    def add(constraint: TConstraint): ConstraintSet = ConstraintSet(constraint :: constraints)
    def addAll(cs: List[TConstraint]): ConstraintSet = ConstraintSet(cs ++ constraints)
    def merge(other: ConstraintSet): ConstraintSet = ConstraintSet(other.constraints ++ constraints)
  }
  object ConstraintSet {
    val empty: ConstraintSet = ConstraintSet(Nil)
  }

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
      "split"        -> TScheme(Nil, TFunction(List(TString, TString), listOf(TString))),
      "join"         -> TScheme(Nil, TFunction(List(listOf(TString), TString), TString)),
      "trim"         -> TScheme(Nil, TFunction(List(TString), TString)),
      "trimLeft"     -> TScheme(Nil, TFunction(List(TString), TString)),
      "trimRight"    -> TScheme(Nil, TFunction(List(TString), TString)),
      "replace"      -> TScheme(Nil, TFunction(List(TString, TString, TString), TString)),
      "replaceAll"   -> TScheme(Nil, TFunction(List(TString, TString, TString), TString)),
      "toLowerCase"  -> TScheme(Nil, TFunction(List(TString), TString)),
      "toUpperCase"  -> TScheme(Nil, TFunction(List(TString), TString)),
      "startsWith"   -> TScheme(Nil, TFunction(List(TString, TString), TBoolean)),
      "endsWith"     -> TScheme(Nil, TFunction(List(TString, TString), TBoolean)),
      "contains"     -> TScheme(Nil, TFunction(List(TString, TString), TBoolean)),
      "indexOf"      -> TScheme(Nil, TFunction(List(TString, TString), TInt)),
      "lastIndexOf"  -> TScheme(Nil, TFunction(List(TString, TString), TInt)),
      "length"       -> TScheme(Nil, TFunction(List(TString), TInt)),
      "isEmptyString"-> TScheme(Nil, TFunction(List(TString), TBoolean)),
      "repeat"       -> TScheme(Nil, TFunction(List(TString, TInt), TString)),
      "reverse"      -> TScheme(Nil, TFunction(List(TString), TString)),
      "format"       -> TScheme(Nil, TDynamic),
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
      ),
      "FileOutput" -> Map(
        "write" -> TScheme(Nil, List(TString, TString) ==> TUnit),
        "append" -> TScheme(Nil, List(TString, TString) ==> TUnit),
        "writeLines" -> TScheme(Nil, List(TString, listOf(TString)) ==> TUnit),
        "exists" -> TScheme(Nil, List(TString) ==> TBoolean),
        "delete" -> TScheme(Nil, List(TString) ==> TUnit)
      ),
      "Dir" -> Map(
        "list" -> TScheme(Nil, List(TString) ==> listOf(TString)),
        "listFull" -> TScheme(Nil, List(TString) ==> listOf(TString)),
        "mkdir" -> TScheme(Nil, List(TString) ==> TUnit),
        "mkdirs" -> TScheme(Nil, List(TString) ==> TUnit),
        "exists" -> TScheme(Nil, List(TString) ==> TBoolean),
        "isDirectory" -> TScheme(Nil, List(TString) ==> TBoolean),
        "isFile" -> TScheme(Nil, List(TString) ==> TBoolean),
        "delete" -> TScheme(Nil, List(TString) ==> TUnit),
        "copy" -> TScheme(Nil, List(TString, TString) ==> TUnit),
        "move" -> TScheme(Nil, List(TString, TString) ==> TUnit),
        "current" -> TScheme(Nil, List() ==> TString),
        "home" -> TScheme(Nil, List() ==> TString),
        "temp" -> TScheme(Nil, List() ==> TString)
      )
    )
  }

  def newInstanceFrom(scheme: TScheme): Type = {
    scheme.svariables.foldLeft(EmptySubstitution)((s, tv) => s.extend(tv, newTypeVariable())).replace(scheme.stype)
  }
  private var n: Int = 0
  private var m: Int = 0
  def newTypeVariable(): Type = {
    n += 1; TVariable("'a" + n)
  }
  def newTypeVariable(name: String): TVariable = {
    m += 1; TVariable(name + m)
  }
  def newTypeVariable(kind: Kind): Type = {
    n += 1; TVariable("'a" + n, kind)
  }

  // Extract the type constructor from a type
  // For example: List<Int> -> List, Option<String> -> Option
  def extractTypeConstructor(t: Type): Option[(String, List[Type])] = t match {
    case TConstructor(name, args, _) if name.startsWith("'") => 
      // This is a type variable applied to arguments (e.g., 'f<'a>)
      Some((name, args))
    case TConstructor(name, args, _) => 
      // This is a concrete type constructor (e.g., List<Int>)
      Some((name, args))
    case _ => None
  }

  // Find which argument position contains the typeclass parameter
  def findTypeClassParameterPosition(methodType: Type, typeClassParam: TVariable): Option[Int] = {
    def containsTypeParam(t: Type, param: TVariable): Boolean = t match {
      case TVariable(name, _) => name == param.name
      case TConstructor(name, args, _) => 
        name == param.name || args.exists(containsTypeParam(_, param))
      case TFunction(params, ret) => 
        params.exists(containsTypeParam(_, param)) || containsTypeParam(ret, param)
      case _ => false
    }
    
    methodType match {
      case TFunction(params, _) =>
        params.zipWithIndex.find { case (paramType, _) => 
          containsTypeParam(paramType, typeClassParam)
        }.map(_._2)
      case _ => None
    }
  }

  val EmptySubstitution: Substitution = Map.empty

  def lookup(x: String, environment: Environment): Option[TScheme] = environment.get(x) match {
    case Some(t) => Some(t)
    case None => None
  }

  def generalize(t: Type, environment: Environment): TScheme = {
    TScheme(typeVariables(t) diff typeVariables(environment), t)
  }

  def unify(t: Type, u: Type, s: Substitution): Substitution = {
    var current_s = s
    var worklist: List[(Type, Type)] = List((current_s.replace(t), current_s.replace(u)))

    while(worklist.nonEmpty) {
      worklist match {
        case (TInt, TInt) :: rest =>
          worklist = rest
        case (TShort, TShort) :: rest =>
          worklist = rest
        case (TByte, TByte) :: rest =>
          worklist = rest
        case (TLong, TLong) :: rest =>
          worklist = rest
        case (TFloat, TFloat) :: rest =>
          worklist = rest
        case (TDouble, TDouble) :: rest =>
          worklist = rest
        case (TBoolean, TBoolean) :: rest =>
          worklist = rest
        case (TString, TString) :: rest =>
          worklist = rest
        case (TString, TDynamic) :: rest =>
          worklist = rest
        case (TDynamic, TString) :: rest =>
          worklist = rest
        case (TUnit, TUnit) :: rest =>
          worklist = rest
        case (TDynamic, TDynamic) :: rest =>
          worklist = rest
        case (TVariable(a, _), TVariable(b, _)) :: rest if a == b =>
          worklist = rest
        case (TVariable(a, _), other) :: rest if !(typeVariables(other) contains a) =>
          current_s = current_s.extend(TVariable(a), other)
          worklist = rest.map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        case (other, TVariable(a, _)) :: rest =>
          worklist = ((TVariable(a), other) :: rest).map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        case (TRecord(ts1, row1), TRecord(ts2, row2)) :: rest =>
          val next_worklist = (ts1 zip ts2).map{ case (t1, t2) => (t1, t2)} ::: ((row1, row2) :: rest)
          worklist = next_worklist.map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        case (TRowEmpty, TRowEmpty) :: rest =>
          worklist = rest
        case (TRowExtend(label1, type1, rowTail1), row2@TRowExtend(_, _, _)) :: rest =>
          val (type2, rowTail2, theta1) = rewriteRow(row2, label1, current_s)
          toList(rowTail1)._2 match {
            case Some(tv) if theta1.contains(tv) => typeError(current.location, "recursive row type")
            case _ =>
              val theta2: Substitution = unify(theta1.replace(type1), theta1.replace(type2), theta1)
              val s2 = theta2 union theta1
              val theta3 = unify(s2.replace(rowTail1), s2.replace(rowTail2), s2)
              current_s = theta3 union s2
          }
          worklist = rest.map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        case (r1@TRecordReference(t1, t2), r2@TRecordReference(u1, u2)) :: rest if t1 == u1 =>
          if(t2.length != u2.length) {
            typeError(current.location, s"type constructor arity mismatch: ${r1} != ${r2}")
          }
          val next_worklist = (t2 zip u2).map{ case (t, u) => (t, u)} ::: rest
          worklist = next_worklist.map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        case (TRecordReference(name, ts), record2@TRecord(us, _)) :: rest =>
          if(ts.length != us.length) {
            typeError(current.location, s"type constructor arity mismatch: ${ts.length} != ${us.length}")
          }
          val record1 = recordEnvironment(name)
          worklist = ((record1, record2) :: rest).map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        case (r1@TRecord(_, _), r2@TRecordReference(_, _)) :: rest =>
          worklist = ((r2, r1) :: rest).map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        case (TFunction(t1, t2), TFunction(u1, u2)) :: rest if t1.size == u1.size =>
          val next_worklist = (t1 zip u1).map{ case (t1, u1) => (t1, u1)} ::: ((t2, u2) :: rest)
          worklist = next_worklist.map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        // Handle type constructor with type variable (e.g., 'f<Int> with List<Int>)
        case (TConstructor(k1, ts, _), TConstructor(k2, us, _)) :: rest if k1.startsWith("'") && ts.length == us.length =>
          // k1 is a type variable applied to arguments, unify it with the constructor k2
          val typeVar = TVariable(k1)
          current_s = current_s.extend(typeVar, TConstructor(k2, Nil))
          val next_worklist = (ts zip us).map{ case (t, u) => (t, u)} ::: rest
          worklist = next_worklist.map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        case (TConstructor(k1, ts, _), TConstructor(k2, us, _)) :: rest if k2.startsWith("'") && ts.length == us.length =>
          // k2 is a type variable applied to arguments, unify it with the constructor k1
          val typeVar = TVariable(k2)
          current_s = current_s.extend(typeVar, TConstructor(k1, Nil))
          val next_worklist = (ts zip us).map{ case (t, u) => (t, u)} ::: rest
          worklist = next_worklist.map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        case (TConstructor(k1, ts, _), TConstructor(k2, us, _)) :: rest if k1 == k2 =>
          val next_worklist = (ts zip us).map{ case (t, u) => (t, u)} ::: rest
          worklist = next_worklist.map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        // Handle unification between TConstructor (from instance declarations) and TRecordReference (from record creation)
        case (TConstructor(name1, args1, _), TRecordReference(name2, args2)) :: rest if name1 == name2 =>
          val next_worklist = (args1 zip args2).map{ case (t, u) => (t, u)} ::: rest
          worklist = next_worklist.map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        case (TRecordReference(name1, args1), TConstructor(name2, args2, _)) :: rest if name1 == name2 =>
          val next_worklist = (args1 zip args2).map{ case (t, u) => (t, u)} ::: rest
          worklist = next_worklist.map{ case (t, u) => (current_s.replace(t), current_s.replace(u))}
        case (t, u) :: _ =>
          typeError(current.location, s"cannot unify ${current_s.replace(t)} with ${current_s.replace(u)}")
        case Nil => // This case should not be reached, but to exhaustively match
      }
    }
    current_s
  }

  def toRow(bindings: List[(String, Type)]): Row = bindings match {
    case (n, t) :: tl => TRowExtend(n, t, toRow(tl))
    case Nil => TRowEmpty
  }

  def toList(row: Type): (List[(String, Type)], Option[TVariable]) = row match {
    case tv@TVariable(_, _) => (Nil, Some(tv))
    case TRowEmpty => (Nil, None)
    case TRowExtend(l, t, r) =>
      val (ls, mv) = toList(r)
      ((l -> t) :: ls, mv)
    case otherwise => throw TyperPanic("Unexpected: " + otherwise)
  }

  def rewriteRow(row: Type, newLabel: Label, s: Substitution): (Type, Type, Substitution) = row match {
    case TRowEmpty => typeError(current.location, s"label ${newLabel} cannot be inserted")
    case TRowExtend(label, labelType, rowTail) if newLabel == label =>
      (labelType, rowTail, s)
    case TRowExtend(label, labelType, alpha@TVariable(_, _)) =>
      val beta = newTypeVariable("r")
      val gamma = newTypeVariable("a")
      (gamma, TRowExtend(label, labelType, beta), s.extend(alpha, TRowExtend(newLabel, gamma, beta)))
    case TRowExtend(label, labelType, rowTail) =>
      val (labelType_, rowTail_, s_) = rewriteRow(rowTail, newLabel, s)
      (labelType_, TRowExtend(label, labelType, rowTail_), s_)
    case row =>
      typeError(current.location, s"Unexpect type: ${row}")
  }

  def doTypeRecords(recordDeclarations :List[RecordDeclaration]): RecordEnvironment = {
    val headers = recordDeclarations.map{d => (d.name, d.ts) }.toMap
    var recordEnvironment: RecordEnvironment = Map.empty
    var s: Substitution = EmptySubstitution
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
    val (typedE, s) = doType(r.doRewrite(e), TypeEnvironment(environment, Set.empty, records, modules, Map.empty, Map.empty, None), a, EmptySubstitution)
    s.replace(a)
  }

  var current: Ast.Node = null
  var recordEnvironment: RecordEnvironment = null
  def doType(e: Ast.Node, env: TypeEnvironment, t: Type, s0: Substitution): (TypedNode, Substitution) = {
    current = e
    e match {
      case Ast.Block(location, expressions) =>
        expressions match {
          case Nil =>
            (TypedAst.Block(TUnit, location, Nil), s0)
          case x::Nil =>
            val (typedX, newSub) = doType(x, env, t, s0)
            (TypedAst.Block(newSub.replace(t), location, typedX::Nil), newSub)
          case x::xs =>
            val t = newTypeVariable()
            val ts = xs.map{_ => newTypeVariable()}
            val (result, s1) = doType(x, env, t, s0)
            val (reversedTypedElements, s2) = (xs zip ts).foldLeft((result::Nil, s1)){ case ((a, s), (e, t)) =>
              val (e2, s2) = doType(e, env, t, s)
              (e2::a, s2)
            }
            (TypedAst.Block(s2.replace(ts.last), location, reversedTypedElements.reverse), s2)
        }
      case Ast.IntNode(location, value) =>
        val newSub = unify(t, TInt, s0)
        (TypedAst.IntNode(newSub.replace(t), location, value), newSub)
      case Ast.ShortNode(location, value) =>
        val newSub = unify(t, TShort, s0)
        (TypedAst.ShortNode(newSub.replace(t), location, value), newSub)
      case Ast.ByteNode(location, value) =>
        val newSub = unify(t, TByte, s0)
        (TypedAst.ByteNode(newSub.replace(t), location, value), newSub)
      case Ast.LongNode(location, value) =>
        val newSub = unify(t, TLong, s0)
        (TypedAst.LongNode(newSub.replace(t), location, value), newSub)
      case Ast.FloatNode(location, value) =>
        val newSub = unify(t, TFloat, s0)
        (TypedAst.FloatNode(newSub.replace(t), location, value), newSub)
      case Ast.DoubleNode(location, value) =>
        val newSub = unify(t, TDouble, s0)
        (TypedAst.DoubleNode(newSub.replace(t), location, value), newSub)
      case Ast.BooleanNode(location, value) =>
        val newSub = unify(t, TBoolean, s0)
        (TypedAst.BooleanNode(newSub.replace(t), location, value), newSub)
      case Ast.UnitNode(location) =>
        val newSub = unify(t, TUnit, s0)
        (TypedAst.UnitNode(newSub.replace(t), location), newSub)
      case Ast.SimpleAssignment(location, variable, value) =>
        if(env.immutableVariables.contains(variable)) {
          typeError(location, s"variable '$variable' cannot change")
        }
        env.lookup(variable) match {
          case None =>
            typeError(location, s"variable $variable is not defined")
          case Some(variableType) =>
            val (typedValue, s1) = doType(value, env, t, s0)
            val s2 = unify(variableType.stype, typedValue.type_, s1)
            (TypedAst.Assignment(variableType.stype, location, variable, typedValue), s2)
        }
      case Ast.IfExpression(location, cond, pos, neg) =>
        val (typedCondition, newSub1) = doType(cond, env, TBoolean, s0)
        val (posTyped, newSub2) = doType(pos, env, t, newSub1)
        val (negTyped, newSub3) = doType(neg, env, t, newSub2)
        (TypedAst.IfExpression(newSub3.replace(t), location, typedCondition, posTyped, negTyped), newSub3)
      case Ast.TernaryExpression(location, cond, pos, neg) =>
        val (typedCondition, newSub1) = doType(cond, env, TBoolean, s0)
        val (posTyped, newSub2) = doType(pos, env, t, newSub1)
        val (negTyped, newSub3) = doType(neg, env, t, newSub2)
        (TypedAst.IfExpression(newSub3.replace(t), location, typedCondition, posTyped, negTyped), newSub3)
      case Ast.WhileExpression(location, condition, body) =>
        val a = newTypeVariable()
        val b = newTypeVariable()
        val c = newTypeVariable()
        val (typedCondition, s1) = doType(condition, env, a, s0)
        if(typedCondition.type_ != TBoolean) {
          typeError(location, s"condition type must be Boolean, actual: ${typedCondition.type_}")
        } else {
          val (typedBody, s2) = doType(body, env, b, s1)
          val s3 = unify(TUnit, t, s2)
          (TypedAst.WhileExpression(TUnit, location, typedCondition, typedBody), s3)
        }
      case Ast.BinaryExpression(location, Operator.EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
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
          case (TString, TString) =>
            (TBoolean, s2)
          case (TString, TDynamic) =>
            (TBoolean, s2)
          case (TDynamic, TString) =>
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (a@TConstructor(n1, ts1, _), b@TConstructor(n2, ts2, _)) if n2 == n2  && ts1.length == ts2.length =>
            val sx = (ts1 zip ts2).foldLeft(s0) { case (s, (t1, t2)) =>
                unify(t1, t2, s)
            }
            (sx.replace(a), sx)
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAst.BinaryExpression(resultType, location, Operator.EQUAL, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.NOT_EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
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
          case (TString, TString) =>
            (TBoolean, s2)
          case (TString, TDynamic) =>
            (TBoolean, s2)
          case (TDynamic, TString) =>
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (a@TConstructor(n1, ts1, _), b@TConstructor(n2, ts2, _)) if n2 == n2  && ts1.length == ts2.length =>
            val sx = (ts1 zip ts2).foldLeft(s0) { case (s, (t1, t2)) =>
                unify(t1, t2, s)
            }
            (sx.replace(a), sx)
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAst.BinaryExpression(resultType, location, Operator.NOT_EQUAL, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.LESS_THAN, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
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
        (TypedAst.BinaryExpression(resultType, location, Operator.LESS_THAN, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.GREATER_THAN, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
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
        (TypedAst.BinaryExpression(resultType, location, Operator.GREATER_THAN, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.LESS_OR_EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
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
        (TypedAst.BinaryExpression(resultType, location, Operator.LESS_OR_EQUAL, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.GREATER_EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
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
        (TypedAst.BinaryExpression(resultType, location, Operator.GREATER_EQUAL, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.ADD, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
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
          case (TString, other) =>
            (TString, s2)
          case (other, TString) =>
            (TString, s2)
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
        (TypedAst.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.SUBTRACT, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
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
        (TypedAst.BinaryExpression(resultType, location, Operator.SUBTRACT, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.MULTIPLY, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
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
        val s4: Substitution = unify(resultType, t, s3)
        (TypedAst.BinaryExpression(resultType, location, Operator.MULTIPLY, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.DIVIDE, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
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
        val s4: Substitution = unify(resultType, t, s3)

        (TypedAst.BinaryExpression(resultType, location, Operator.DIVIDE, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.AND, lhs, rhs) => val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
          case (TInt, TInt) =>
            (TInt, s2)
          case (TLong, TLong) =>
            (TLong, s2)
          case (TShort, TShort) =>
            (TShort, s2)
          case (TByte, TByte) =>
            (TByte, s2)
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
        val s4: Substitution = unify(resultType, t, s3)

        (TypedAst.BinaryExpression(resultType, location, Operator.AND, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.OR, lhs, rhs) => val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
          case (TInt, TInt) =>
            (TInt, s2)
          case (TLong, TLong) =>
            (TLong, s2)
          case (TShort, TShort) =>
            (TShort, s2)
          case (TByte, TByte) =>
            (TByte, s2)
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

        (TypedAst.BinaryExpression(resultType, location, Operator.OR, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.XOR, lhs, rhs) => val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
          case (TInt, TInt) =>
            (TInt, s2)
          case (TLong, TLong) =>
            (TLong, s2)
          case (TShort, TShort) =>
            (TShort, s2)
          case (TByte, TByte) =>
            (TByte, s2)
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

        (TypedAst.BinaryExpression(resultType, location, Operator.XOR, typedLhs, typedRhs), s4)
      case Ast.MinusOp(location, operand) =>
        val a = newTypeVariable()
        val (typedOperand, s1) = doType(operand, env, a, s0)
        val (resultType, s) = s1.replace(a) match {
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
        (TypedAst.MinusOp(resultType, location, typedOperand), s)
      case Ast.PlusOp(location, operand) =>
        val a = newTypeVariable()
        val (typedOperand, s1) = doType(operand, env, a, s0)
        val (resultType, s) = s1.replace(a) match {
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
        (TypedAst.PlusOp(resultType, location, typedOperand), s)
      case Ast.NotOp(location, operand) =>
        val (typedOperand, s1) = doType(operand, env, TBoolean, s0)
        (TypedAst.NotOp(TBoolean, location, typedOperand), s1)
      case Ast.BinaryExpression(location, Operator.AND2, lhs, rhs) =>
        val (typedLhs, s1) = doType(lhs, env, TBoolean, s0)
        val (typedRhs, s2) = doType(rhs, env, TBoolean, s1)
        val s = unify(TBoolean, t, s2)
        (TypedAst.BinaryExpression(TBoolean, location, Operator.AND2, typedLhs, typedRhs), s)
      case Ast.BinaryExpression(location, Operator.BAR2, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, TBoolean, s0)
        val (typedRhs, s2) = doType(rhs, env, TBoolean, s1)
        val s = unify(TBoolean, t, s2)
        (TypedAst.BinaryExpression(TBoolean, location, Operator.BAR2, typedLhs, typedRhs), s)
      case Ast.StringNode(location, value) =>
        val s = unify(TString, t, s0)
        (TypedAst.StringNode(TString, location, value), s)
      case Ast.Id(location, name) =>
        env.lookup(name) match {
          case None =>
            // Check if it's a type class method
            val typeClassMethod = env.typeClasses.values.find { tc =>
              tc.methods.exists(_._1 == name)
            }
            
            typeClassMethod match {
              case Some(tc) =>
                // Found a type class method - return it as a placeholder
                // The actual resolution will happen in function calls
                val methodScheme = tc.methods.find(_._1 == name).get._2
                val instanceType = newInstanceFrom(methodScheme)
                val s = unify(instanceType, t, s0)
                (TypedAst.Id(s.replace(t), location, name), s)
                
              case None =>
                typeError(location, s"variable '${name}' is not found")
            }
          case Some(u) =>
            val s = unify(newInstanceFrom(u), t, s0)
            val resultType = s.replace(t)
            (TypedAst.Id(resultType, location, name), s)
        }
      case Ast.Selector(location, module, name) =>
        val s = env.lookupModuleMember(module, name) match {
          case Some(u) => unify(newInstanceFrom(u), t, s0)
          case None =>
            // Try runtime-discovered modules (user-defined from previous runs)
            BuiltinEnvironments.BuiltinModuleEnvironment.modules.get(module) match {
              case Some(runtimeMembers) =>
                runtimeMembers.get(name) match {
                  case Some(value) =>
                    val stype: Type = value match {
                      case VmClosureValue(params, _, _, _, _) =>
                        val as = params.map(_ => newTypeVariable())
                        TFunction(as, newTypeVariable())
                      case NativeFunctionValue(_) => TFunction(List(newTypeVariable()), newTypeVariable())
                      case _ => newTypeVariable()
                    }
                    unify(stype, t, s0)
                  case None => typeError(location, s"module '${module}' or member '${name}' is not found")
                }
              case None => typeError(location, s"module '${module}' or member '${name}' is not found")
            }
        }
        val resultType = s.replace(t)
        (TypedAst.Selector(resultType, location, module, name), s)
      case node@Ast.RecordSelect(_, _, _) =>
        doTypeRecordSelect(node, env, t, s0)
      case Ast.Lambda(location, params, optionalType, body) =>
        val b = optionalType.getOrElse(newTypeVariable())
        val ts = params.map{p => p.optionalType.getOrElse(newTypeVariable())}
        val as = (params zip ts).map{ case (p, t) => p.name -> TScheme(Nil, t) }
        val s1 = unify(t, TFunction(ts, b), s0)
        val env1 = as.foldLeft(env) { case (env, (name, scheme)) => env.updateImmuableVariable(name, scheme)}
        val (typedBody, s) = doType(body, env1, b, s1)
        (TypedAst.FunctionLiteral(s.replace(t), location, params, optionalType, typedBody), s)
      case Ast.Let(location, variable, optionalType, value, body, immutable) =>
        if(env.variables.contains(variable)) {
          typeError(location, s"variable $variable is already defined")
        }
        val a = optionalType.map {
          case TRecordReference(name, _) =>
            env.records.get(name) match {
              case Some(record) =>
                record
              case None =>
                typeError(location, s"record named ${name} is not found")
            }
          case otherwise =>
            otherwise
        }.getOrElse(newTypeVariable())
        val (typedValue, s1) = doType(value, env, a, s0)
        val s2 = unify(s1.replace(a), typedValue.type_, s1)
        val gen = generalize(s2.replace(a), env.variables)
        val declaredType = s2.replace(a)
        val newEnv = if(immutable) {
          env.updateImmuableVariable(variable, generalize(declaredType, env.variables))
        } else {
          env.updateMutableVariable(variable, generalize(declaredType, env.variables))
        }
        val (typedBody, s) = doType(body, newEnv, t, s2)
        (TypedAst.LetDeclaration(typedBody.type_, location, variable, declaredType, typedValue, typedBody, immutable), s)
      case Ast.LetRec(location, variable, value, cleanup, body) =>
        if(env.variables.contains(variable)) {
          throw new InterruptedException(s"${location.format} function ${variable} is already defined")
        }
        val a = newTypeVariable()
        val b = newTypeVariable()
        val (typedE1, s1) = doType(value, env.updateImmuableVariable(variable, TScheme(Nil, a)), b, s0)
        val s2 = unify(a, b, s1)
        val (typedE2, s3) = doType(body, env.updateImmuableVariable(variable, generalize(s2.replace(a), s2(env.variables))), t, s2)
        val x = newTypeVariable()
        val (typedCleanup, s) = cleanup.map{c => doType(c, env, x, s3)} match {
          case Some((c, s)) => (Some(c), s)
          case None => (None, s3)
        }
        (TypedAst.LetFunctionDefinition(typedE2.type_, location, variable, typedE1.asInstanceOf[TypedAst.FunctionLiteral], typedCleanup, typedE2), s)
      case Ast.FunctionCall(location, e1, ps) =>
        // Check if this is a type class method call
        e1 match {
          case Ast.Id(_, name) =>
            val typeClassOpt = env.typeClasses.values.find(tc => tc.methods.exists(_._1 == name))
            typeClassOpt match {
              case Some(tc) =>
                // This is a type class method call - need to resolve the instance
                if (ps.isEmpty) {
                  typeError(location, s"Type class method $name requires at least one argument")
                }
                
                // Type check the arguments first
                val argTypes = ps.map(_ => newTypeVariable())
                val (typedArgs, s1) = (ps zip argTypes).foldLeft((Nil:List[TypedNode], s0)) { 
                  case ((args, s), (e, t)) =>
                    val (arg, sx) = doType(e, env, t, s)
                    (arg :: args, sx)
                }
                val typedArgsRev = typedArgs.reverse
                
                // Find the method signature in the typeclass
                val methodScheme = tc.methods.find(_._1 == name).map(_._2).getOrElse {
                  typeError(location, s"Method $name not found in typeclass ${tc.name}")
                }
                
                // Determine which argument contains the typeclass parameter
                val tcParam = tc.typeParams.headOption.getOrElse {
                  typeError(location, s"Typeclass ${tc.name} has no type parameters")
                }
                
                val argPosition = findTypeClassParameterPosition(methodScheme.stype, tcParam).getOrElse(0)
                
                // Get the type constructor from the appropriate argument
                val instanceType = if (argPosition < argTypes.length) {
                  val argType = s1.replace(argTypes(argPosition))
                  // For higher-kinded typeclasses, we need just the constructor
                  // For regular typeclasses, we need the full type
                  if (tcParam.kind != Kind.Star) {
                    // Higher-kinded typeclass - extract just the constructor
                    extractTypeConstructor(argType) match {
                      case Some((name, _)) if name.startsWith("'") =>
                        // It's a type variable, we need to look at the actual type
                        argType match {
                          case TConstructor(n, _, _) => TConstructor(n, Nil)  // Just the constructor
                          case _ => argType
                        }
                      case Some((name, _)) =>
                        // It's a concrete type constructor
                        TConstructor(name, Nil)
                      case None =>
                        // Not a type constructor application, use the whole type
                        argType
                    }
                  } else {
                    // Regular typeclass - use the full type
                    argType
                  }
                } else {
                  s1.replace(argTypes.head)
                }
                
                // Find the matching instance
                env.findMatchingInstance(tc.name, instanceType) match {
                  case Some(instance) =>
                    // Create a reference to the instance method
                    val instanceMethod = instance.methods.get(name) match {
                      case Some(methodType) =>
                        // For now, we'll create a special marker that the runtime will resolve
                        // In a real implementation, this would be dictionary passing or method resolution
                        val methodId = TypedAst.Id(methodType, location, s"${tc.name}_${normalizeTypeName(instanceType)}_${name}")
                        
                        // Type the method call
                        val s2 = unify(methodType, TFunction(argTypes, t), s1)
                        (TypedAst.FunctionCall(s2.replace(t), location, methodId, typedArgsRev), s2)
                        
                      case None =>
                        typeError(location, s"Instance for ${tc.name}[${instanceType}] doesn't implement method $name")
                    }
                    instanceMethod
                    
                  case None =>
                    typeError(location, s"No instance for ${tc.name}[${instanceType}]")
                }
                
              case None =>
                // Regular function call
                val t2 = ps.map{_ => newTypeVariable()}
                val (typedTarget, s1) = doType(e1, env, TFunction(t2, t), s0)
                val (tparams, s) = (ps zip t2).foldLeft((Nil:List[TypedNode], s1)){ case ((tparams, s), (e, t)) =>
                  val (tparam, sx) = doType(e, env, t, s)
                  (tparam::tparams, sx)
                }
                (TypedAst.FunctionCall(s.replace(t), location, typedTarget, tparams.reverse), s)
            }
            
          case _ =>
            // Regular function call
            val t2 = ps.map{_ => newTypeVariable()}
            val (typedTarget, s1) = doType(e1, env, TFunction(t2, t), s0)
            val (tparams, s) = (ps zip t2).foldLeft((Nil:List[TypedNode], s1)){ case ((tparams, s), (e, t)) =>
              val (tparam, sx) = doType(e, env, t, s)
              (tparam::tparams, sx)
            }
            (TypedAst.FunctionCall(s.replace(t), location, typedTarget, tparams.reverse), s)
        }
      case Ast.ListLiteral(location, elements) =>
        val a = newTypeVariable()
        val listOfA = listOf(a)
        val (tes, sx) = elements.foldLeft((Nil:List[TypedNode], s0)){ case ((tes, s), e) =>
          val (te, sx) = doType(e, env, a, s)
          (te::tes, sx)
        }
        val s = unify(listOfA, t, sx)
        (TypedAst.ListLiteral(s.replace(t), location, tes.reverse), s)
      case Ast.SetLiteral(location, elements) =>
        val a = newTypeVariable()
        val setOfA = setOf(a)
        val (tes, sx) = elements.foldLeft((Nil:List[TypedNode], s0)){ case ((tes, s), e) =>
          val (te, sx) = doType(e, env, a, s)
          (te::tes, sx)
        }
        val s = unify(setOfA, t, sx)
        (TypedAst.SetLiteral(s.replace(t), location, tes.reverse), s)
      case Ast.MapLiteral(location, elements) =>
        val kt = newTypeVariable()
        val vt = newTypeVariable()
        val mapOfKV = mapOf(kt, vt)
        val (tes, s) = elements.foldLeft((Nil:List[(TypedNode, TypedNode)], s0)){ case ((tes, s), (k, v)) =>
          val (typedK, sx) = doType(k, env, kt, s)
          val (typedY, sy) = doType(v, env, vt, sx)
          ((typedK -> typedY)::tes, sy)
        }
        val sy = unify(mapOfKV, t, s)
        (TypedAst.MapLiteral(sy.replace(t), location, tes.reverse), sy)
      case Ast.ObjectNew(location, className, params) =>
        val ts = params.map{_ => newTypeVariable()}
        val (tes, sx) = (params zip ts).foldLeft((Nil:List[TypedNode], s0)){ case ((tes, s), (e, t)) =>
          val (te, sx) = doType(e, env, t, s)
          (te::tes, sx)
        }
        val s = unify(TDynamic, t, sx)
        (TypedAst.ObjectNew(TDynamic, location, className, tes.reverse), s)
      case Ast.RecordNew(location, recordName, params) =>
        val ts = params.map{_ => newTypeVariable()}
        val (tes1, sx) = (params zip ts).foldLeft((Nil:List[TypedNode], s0)){ case ((tes, s), (e, t)) =>
          val (te, sx) = doType(e, env, t, s)
          (te::tes, sx)
        }
        val tes2 = tes1.reverse
        env.records.get(recordName) match {
          case Some(record) =>
            val xts = record.ts
            val (members, rv) = toList(record.row)
            val sy = xts.foldLeft(sx) { case (s, t) => s.remove(t)}
            if(members.length != ts.length) {
              typeError(location, s"length mismatch: required: ${members.length}, actual: ${ts.length}")
            }
            val memberTypes = members.map{ case (_, t) => t}
            val parameterTypes = tes2.map { case te => te.type_ }
            val sn = (memberTypes zip parameterTypes).foldLeft(sy) { case (s, (m, p)) => unify(m, p, s)}
            val structuralRecordType: TRecord = env.records(recordName)
            // Use TRecordReference (nominal type) instead of TRecord (structural type)
            val nominalRecordType = TRecordReference(recordName, structuralRecordType.ts)
            val s = unify(t, nominalRecordType, sn)
            (TypedAst.RecordNew(nominalRecordType, location, recordName, tes2), s)
          case None =>
            typeError(location, s"record '$recordName' is not found")
        }
      case Ast.Casting(location, target, to) =>
        val a = newTypeVariable()
        val (typedTarget, s1) = doType(target, env, a, s0)
        val s = unify(t, to, s1)
        (TypedAst.Casting(to, location, typedTarget, to), s)
      case Ast.MethodCall(location, receiver, name, params) =>
        val a = newTypeVariable()
        val (typedReceiver, s1) = doType(receiver, env, a, s0)
        val s2 = unify(s0.replace(a), TDynamic, s1)
        val ts = params.map{_ => newTypeVariable()}
        val (tes, sx) = (params zip ts).foldLeft((Nil:List[TypedNode], s2)){ case ((tes, s), (e, t)) =>
          val (te, sx) = doType(e, env, t, s)
          (te::tes, sx)
        }
        val s = unify(t, TDynamic, sx)
        (TypedAst.MethodCall(TDynamic, location, typedReceiver, name, tes.reverse), s)
      case otherwise =>
        throw TyperPanic(otherwise.toString)
    }
  }

  private def doTypeRecordSelect(node: Ast.RecordSelect, environment: TypeEnvironment, t: Type, s0: Substitution): (TypedNode, Substitution) = {
    def insert(record: TRecord, l1: Label, l1Type: Type, rowVariable: TVariable, s: Substitution): (TRecord, Substitution) = {
      def go(row: Type): (TRowExtend, Substitution) = row match {
        case r@TRowExtend(l2, l2Type, rowTail) if l1 == l2 =>
          val (r, sx) = go(rowTail)
          val sy = unify(l1Type, l2Type, sx)
          (TRowExtend(l1, sy.replace(l2Type), r), sy)
        case TRowExtend(l2, l2Type, rowTail) =>
          val (r, sx) = go(rowTail)
          (TRowExtend(l2, l2Type, r), sx)
        case tv@TVariable(_, _) =>
          (TRowExtend(l1, l1Type, rowVariable), s)
        case t =>
          sys.error("cannot reach here in insert: " + t)
      }
      val (r, sx) = go(record.row)
      (TRecord(record.ts, r), sx)
    }
    val location = node.location
    val expression = node.record
    val memberName = node.label
    val t0 = newTypeVariable()
    val (te, s1) = doType(expression, environment, t0, s0)
    te.type_ match {
      case TRecordReference(recordName, paramTypes) =>
        environment.records.get(recordName) match {
          case None =>
            typeError(location, s"record ${recordName} is not found")
          case Some(record) =>
            toList(record.row) match {
              case (members, Some(_)) =>
                val a = newTypeVariable("a")
                val r = newTypeVariable("r")
                val (newRecord, s2) = insert(record, memberName, a, r, s1)
                val s3 = unify(record, newRecord, s2)
                val s4 = unify(t, a, s3)
                (TypedAst.RecordSelect(s4.replace(a), location, te, memberName), s4)
              case (members, None) =>
                members.find { case (mname, mtype) => memberName == mname } match {
                  case None =>
                    throw typeError(location, s"member ${memberName} is not found in record ${recordName}")
                  case Some((mname, mtype)) =>
                    val s = unify(mtype, t, s1)
                    (TypedAst.RecordSelect(s.replace(t), location, te, mname), s)
                }
            }
        }
      case tv@(TVariable(_, _)) =>
        val a = newTypeVariable("a")
        val r = newTypeVariable("r")
        val (record, s2) = insert(TRecord(Nil, r), memberName, a, r, s1)
        val s3 = unify(tv, record, s2)
        val s = unify(t, a, s3)
        (TypedAst.RecordSelect(s.replace(a), location, te, memberName), s)
      case record@TRecord(ts, row) =>
        toList(row) match {
          case (members, None) =>
            members.find { case (mname, mtype) => memberName == mname } match {
              case None =>
                throw typeError(location, s"member ${memberName} is not found in record ${record}")
              case Some((mname, mtype)) =>
                val s = unify(mtype, t, s1)
                (TypedAst.RecordSelect(s.replace(t), location, te, mname), s)
            }
          case (members, Some(_)) =>
            val a = newTypeVariable("a")
            val r = newTypeVariable("r")
            val (newRecord, s2) = insert(record, memberName, a, r, s1)
            val s3 = unify(record, newRecord, s2)
            val s = unify(t, a, s3)
            (TypedAst.RecordSelect(s.replace(a), location, te, memberName), s)
        }
      case _ =>
        typeError(location, s"${t} is not record type")
    }
  }

  def processTypeClasses(typeClasses: List[Ast.TypeClassDeclaration]): Map[String, TTypeClass] = {
    typeClasses.map { tc =>
      val methods = tc.methods.map { m =>
        (m.name, TScheme(tc.typeParams, m.typeSignature))
      }
      tc.name -> TTypeClass(tc.name, tc.typeParams, methods)
    }.toMap
  }
  
  def processInstances(instances: List[Ast.InstanceDeclaration], typeClasses: Map[String, TTypeClass]): Map[(String, Type), TInstance] = {
    instances.map { inst =>
      // Look up the type class to get expected method signatures
      val typeClass = typeClasses.get(inst.className) match {
        case Some(tc) => tc
        case None => 
          throw TyperException(s"${inst.location.format} Type class ${inst.className} not found")
      }
      
      val methodTypes = inst.methods.map { m =>
        // Get the expected type from the type class
        val expectedType = typeClass.methods.find(_._1 == m.name) match {
          case Some((_, TScheme(typeParams, methodType, _))) =>
            // Substitute the instance's type for the type class parameter
            val substitution: Substitution = if (typeParams.nonEmpty) {
              Map(typeParams.head -> inst.forType)
            } else {
              EmptySubstitution
            }
            substitution.replace(methodType)
          case None =>
            throw TyperException(s"${m.location.format} Method ${m.name} not found in type class ${inst.className}")
        }
        
        // Use the expected type from the type class
        m.name -> expectedType
      }.toMap
      
      (inst.className, inst.forType) -> TInstance(inst.className, inst.forType, methodTypes)
    }.toMap
  }

  // Instance resolution
  def resolveInstance(env: TypeEnvironment, constraint: TConstraint, substitution: Substitution): Option[(TInstance, Substitution)] = {
    val TConstraint(className, typeVar) = constraint
    val targetType = substitution.replace(typeVar)
    
    env.findMatchingInstance(className, targetType) match {
      case Some(instance) =>
        // Unify the instance type with the target type to get proper substitutions
        val freshVars = mutable.Map[TVariable, TVariable]()
        def freshen(t: Type): Type = t match {
          case tv@TVariable(name, _) =>
            freshVars.getOrElseUpdate(tv, newTypeVariable(name))
          case TConstructor(name, args, _) =>
            TConstructor(name, args.map(freshen))
          case TFunction(params, ret) =>
            TFunction(params.map(freshen), freshen(ret))
          case other => other
        }
        
        val freshInstanceType = freshen(instance.forType)
        try {
          val s = unify(freshInstanceType, targetType, substitution)
          Some((instance, s))
        } catch {
          case _: TyperException => None
        }
      case None => None
    }
  }
  
  // Resolve all constraints for a type
  def resolveConstraints(env: TypeEnvironment, constraints: List[TConstraint], substitution: Substitution, location: Location): (Map[TConstraint, TInstance], Substitution) = {
    constraints.foldLeft((Map.empty[TConstraint, TInstance], substitution)) { case ((resolved, s), constraint) =>
      resolveInstance(env, constraint, s) match {
        case Some((instance, s2)) =>
          (resolved + (constraint -> instance), s2)
        case None =>
          val targetType = s.replace(constraint.typeVar)
          typeError(location, s"No instance for ${constraint.className}[${targetType}]")
      }
    }
  }

  def normalizeTypeName(t: Type): String = t match {
    case TInt => "Int"
    case TString => "String"
    case TBoolean => "Boolean"
    case TFloat => "Float"
    case TDouble => "Double"
    case TUnit => "Unit"
    case TConstructor("List", args, _) if args.nonEmpty => 
      s"List<${args.map(normalizeTypeName).mkString(", ")}>"
    case TConstructor(name, Nil, _) => name
    case TConstructor(name, args, _) =>
      s"$name<${args.map(normalizeTypeName).mkString(", ")}>"
    case TRecordReference(name, _) => name
    case TRecord(_, _) => "Record"
    case TVariable(name, _) => name
    case _ => "Unknown"
  }

  def typeError(location: Location, message: String): Nothing = {
    throw TyperException(s"${location.format} ${message}")
  }

  def transform(program: Ast.Program): TypedAst.Program = {
    val tv = newTypeVariable()
    val s: Substitution = EmptySubstitution
    val recordEnvironment = doTypeRecords(program.records)
    this.recordEnvironment = recordEnvironment
    
    // Process type classes
    val typeClasses = processTypeClasses(program.typeClasses)
    
    // Process instances
    val instances = processInstances(program.instances, typeClasses)
    
    var env = TypeEnvironment(
      BuiltinEnvironment, 
      Set.empty, 
      BuiltinRecordEnvironment ++ recordEnvironment, 
      BuiltinModuleEnvironment, 
      typeClasses,
      instances,
      None
    )

    // Apply import semantics: bring imported module members into scope as unqualified names
    if (program.imports.nonEmpty) {
      var vars = env.variables
      var modulesMap = env.modules
      program.imports.foreach { imp =>
        val moduleName = imp.simpleName // last segment
        // collect module member schemes from type env or runtime discovery
        val memberSchemesOpt: Option[Map[String, TScheme]] = env.modules.get(moduleName) match {
          case some@Some(_) => some
          case None =>
            val runtimeMembersOpt = BuiltinEnvironments.BuiltinModuleEnvironment.modules.get(moduleName)
            runtimeMembersOpt.map { runtimeMembers =>
              runtimeMembers.map { case (name, value) =>
                val stype: Type = value match {
                  case VmClosureValue(params, _, _, _, _) =>
                    val as = params.map(_ => newTypeVariable())
                    TFunction(as, newTypeVariable())
                  case NativeFunctionValue(_) => TFunction(List(newTypeVariable()), newTypeVariable())
                  case _ => newTypeVariable()
                }
                name -> TScheme(Nil, stype)
              }.toMap
            }
        }

        memberSchemesOpt match {
          case Some(memberSchemes) =>
            // filter unqualified bindings by only/excludes
            val filtered: Map[String, TScheme] = {
              val base = imp.only match {
                case Some(onlyList) => memberSchemes.view.filterKeys(onlyList.toSet).toMap
                case None => memberSchemes
              }
              base.filterNot { case (n, _) => imp.excludes.contains(n) }
            }
            vars = vars ++ filtered
            // register alias module mapping for selector typing
            imp.alias.foreach { aliasName =>
              modulesMap = modulesMap + (aliasName -> memberSchemes)
            }
            // also register FQCN module mapping for selector typing if discovered at runtime
            if (!modulesMap.contains(imp.fqcn)) {
              modulesMap = modulesMap + (imp.fqcn -> memberSchemes)
            }
          case None => ()
        }
      }
      env = env.copy(variables = vars, modules = modulesMap)
    }
    
    val (typedExpression, _) = doType(program.block, env, tv, EmptySubstitution)
    TypedAst.Program(program.location, program.module, program.imports, typedExpression.asInstanceOf[TypedAst.Block], recordEnvironment, typeClasses, instances, program.instances)
  }

  override final val name: String = "Typer"

  override final def process(input: Ast.Program, session :InteractiveSession): TypedAst.Program = transform(input)
}
