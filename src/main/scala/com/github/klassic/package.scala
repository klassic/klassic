package com.github

import java.io.{BufferedReader, FileInputStream, InputStreamReader}

import com.github.klassic.Type.{Row, TBoolean, TByte, TConstructor, TDouble, TDynamic, TError, TFloat, TFunction, TInt, TLong, TRecord, TRecordReference, TRowEmpty, TRowExtend, TScheme, TShort, TString, TUnit, TVariable}

import scala.language.reflectiveCalls
import scala.collection.mutable

/**
 * @author Kota Mizushima
 */
package object klassic {
  def openReader[A](fileName: String)(f: BufferedReader => A): A = {
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(fileName), "UTF-8"))
    using(reader)(f)
  }
  def using[A <: AutoCloseable, B](resource: A)(f: A => B): B = try {
    f(resource)
  } finally {
    scala.util.control.Exception.allCatch(resource.close())
  }

  val p = new Parser
  val t = new Typer
  val e = new Evaluator

  def typeOf(input: String): Type = {
    t.typeOf(p.parseExpression(input))
  }

  def evaluate(input: String): Value = {
    e.evaluateString(input)
  }

  def tv(name: String): TVariable = {
    TVariable(name)
  }

  type Environment = Map[String, TScheme]

  type Substitution = mutable.Map[TVariable, Type]

  def newEmptySubstitution(): Substitution = mutable.Map.empty[TVariable, Type]

  def cloneSubstitution(s: Substitution): Substitution = s.clone()

  def lookupSubstitution(sub: Substitution, x: TVariable): Type = sub.getOrElse(x, x)

  def replaceInType(sub: Substitution, ty: Type): Type = ty match {
    case tv@TVariable(a) =>
      val u = lookupSubstitution(sub, tv)
      if (ty == u) ty else replaceInType(sub, u)
    case TFunction(t1, t2) =>
      TFunction(t1.map{ t => replaceInType(sub, t)}, replaceInType(sub, t2))
    case TRecordReference(name, ts) =>
      TRecordReference(name, ts.map{ t => replaceInType(sub, t)})
    case TRecord(ts, row) =>
      TRecord(ts, replaceInType(sub, row))
    case TRowEmpty =>
      ty
    case tr@TRowExtend(l, t, r) =>
      TRowExtend(l, replaceInType(sub, t), replaceInType(sub, r))
    case TInt =>
      TInt
    case TShort =>
      TShort
    case TByte =>
      TByte
    case TLong =>
      TLong
    case TFloat =>
      TFloat
    case TDouble =>
      TDouble
    case TBoolean =>
      TBoolean
    case TUnit =>
      TUnit
    case TString =>
      TString
    case TDynamic =>
      TDynamic
    case TError =>
      TError
    case TConstructor(name, args) => TConstructor(name, args.map{ arg => replaceInType(sub, arg)})
  }

  def applySubstitution(sub: Substitution, env: Environment): Environment = {
    env.map { case (x, ts) =>
      x -> TScheme(typeVariables(ts), replaceInType(sub, ts.stype))
    }
  }

  def extendSubstitution(sub: Substitution, tv: TVariable, td: Type): Unit = {
    sub(tv) = td
  }

  def removeSubstitution(sub: Substitution, tv: TVariable): Unit = {
    sub -= tv
  }

  def unionSubstitutions(target: Substitution, source: Substitution): Unit = {
    val sourceTransformed = source.map { case (k, v) => k -> replaceInType(target, v) }
    sourceTransformed.foreach { case (k, v) => target.getOrElseUpdate(k, v) }
  }

  def typeVariables(r: Row): List[TVariable] = r match {
    case TRowExtend(l, t, e) => typeVariables(t) concat typeVariables(e)
    case TRowEmpty => Nil
    case tv@TVariable(_) => List(tv)
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
    case TString =>
      Nil
    case TDynamic =>
      Nil
    case TError =>
      Nil
    case TFunction(t1, t2) =>
      t1.flatMap{typeVariables} concat typeVariables(t2)
    case TRecordReference(name, ts) =>
      List(ts.flatMap{ case t => typeVariables(t)}:_*)
    case TRowEmpty =>
      Nil
    case TRowExtend(l, t, r) =>
      typeVariables(r) concat typeVariables(t)
    case TRecord(ts, row) =>
      ts concat typeVariables(row)
    case TConstructor(k, ts) =>
      ts.foldLeft(List[TVariable]()){ (tvs, t) => tvs concat typeVariables(t)}
  }

  def typeVariables(ts: TScheme): List[TVariable] = {
    typeVariables(ts.stype) diff ts.svariables
  }

  def typeVariables(environment: Environment): List[TVariable] = {
    environment.foldLeft(List[TVariable]()) { (tvs, nt) => tvs concat typeVariables(nt._2) }
  }
}
