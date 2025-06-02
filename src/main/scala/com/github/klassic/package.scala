package com.github

import java.io.{BufferedReader, FileInputStream, InputStreamReader}

import com.github.klassic.Type.{Row, TBoolean, TByte, TConstructor, TConstraint, TDouble, TDynamic, TError, TFloat, TFunction, TInstance, TInt, TLong, TQualified, TRecord, TRecordReference, TRowEmpty, TRowExtend, TScheme, TShort, TString, TTypeClass, TUnit, TVariable}

import scala.language.reflectiveCalls

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

  type Substitution = Map[TVariable, Type]

  implicit class RichSubstitution(val self: Map[TVariable, Type]) extends AnyVal {
    def lookup(x: TVariable): Type = {
      self.getOrElse(x, x)
    }

    def replace(ty: Type): Type = ty match {
      case tv@TVariable(a, _) =>
        val u = lookup(tv)
        if (ty == u) ty else replace(u)
      case TFunction(t1, t2) =>
        TFunction(t1.map{ t => replace(t)}, replace(t2))
      case TRecordReference(name, ts) =>
        TRecordReference(name, ts.map{ t => replace(t)})
      case TRecord(ts, row) =>
        TRecord(ts, replace(row))
      case TRowEmpty =>
        ty
      case tr@TRowExtend(l, t, r) =>
        TRowExtend(l, replace(t), replace(r))
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
      case TConstructor(name, args, kind) => 
        // Check if the constructor name is a type variable that we need to substitute
        if (name.startsWith("'")) {
          self.get(TVariable(name)) match {
            case Some(TConstructor(newName, _, _)) =>
              // Replace the type variable with the constructor
              TConstructor(newName, args.map(replace), kind)
            case Some(other) =>
              // This shouldn't happen in well-typed programs
              TConstructor(name, args.map(replace), kind)
            case None =>
              // No substitution for this type variable
              TConstructor(name, args.map(replace), kind)
          }
        } else {
          TConstructor(name, args.map(replace), kind)
        }
      case TConstraint(className, typeVar) =>
        TConstraint(className, replace(typeVar).asInstanceOf[TVariable])
      case TQualified(constraints, baseType) =>
        TQualified(constraints.map(c => replace(c).asInstanceOf[TConstraint]), replace(baseType))
      case TTypeClass(name, typeParams, methods) =>
        TTypeClass(name, typeParams, methods)
      case TInstance(className, forType, methods) =>
        TInstance(className, replace(forType), methods.view.mapValues(replace).toMap)
    }

    def apply(env: Environment): Environment = {
      env.map { case (x, ts) =>
        x -> TScheme(typeVariables(ts), replace(ts.stype))
      }
    }

    def extend(tv: TVariable, td: Type): Substitution = {
      self + (tv -> td)
    }

    def remove(tv: TVariable): Substitution = self - tv

    def union(that: Substitution): Substitution = {
      val s1 = self
      val s2 = that
      s2.view.mapValues{t => s1.replace(t)}.toMap ++ s1
    }
  }

  def typeVariables(r: Row): List[TVariable] = r match {
    case TRowExtend(l, t, e) => typeVariables(t) concat typeVariables(e)
    case TRowEmpty => Nil
    case tv@TVariable(_, _) => List(tv)
  }

  def typeVariables(t: Type): List[TVariable] = t match {
    case tv @ TVariable(a, _) =>
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
    case TConstructor(k, ts, _) =>
      // If the constructor name is a type variable, include it
      val ctorVars = if (k.startsWith("'")) List(TVariable(k)) else Nil
      ctorVars ++ ts.flatMap(typeVariables)
    case TConstraint(_, tv) =>
      List(tv)
    case TQualified(constraints, baseType) =>
      constraints.flatMap(c => typeVariables(c)) ++ typeVariables(baseType)
    case TTypeClass(_, typeParams, _) =>
      typeParams
    case TInstance(_, forType, _) =>
      typeVariables(forType)
  }

  def typeVariables(ts: TScheme): List[TVariable] = {
    typeVariables(ts.stype) diff ts.svariables
  }

  def typeVariables(environment: Environment): List[TVariable] = {
    environment.foldLeft(List[TVariable]()) { (tvs, nt) => tvs concat typeVariables(nt._2) }
  }
}
