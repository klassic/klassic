package com.github

import java.io.{BufferedReader, FileInputStream, InputStreamReader}

import com.github.klassic.Type.{Row, TBoolean, TByte, TConstructor, TDouble, TDynamic, TError, TFloat, TFunction, TInt, TLong, TRecord, TRecordReference, TRowEmpty, TRowExtend, TScheme, TShort, TUnit, TVariable}

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

  type Substitution = Map[TVariable, Type]

  type Environment = Map[String, TScheme]

  implicit class RichSubstitution(val self: Substitution) extends AnyVal {
    def lookup(x: TVariable): Type = {
      self.getOrElse(x, x)
    }

    def applyRow(r: Row): Row = r match {
      case TRowExtend(l, t, e) => TRowExtend(l, apply(t), applyRow(e))
      case TRowEmpty => TRowEmpty
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
        x -> TScheme(typeVariables(ts), self.apply(ts.stype))
      }
    }

    def extend(tv: TVariable, td: Type): Substitution = self + (tv -> td)

    def remove(tv: TVariable): Substitution = self - tv

    def compose(that: Substitution): Substitution = {
      val s1 = self
      val s2 = that
      s2.mapValues{t => s1.apply(t)} ++ s1
    }
  }

  def typeVariables(r: Row): List[TVariable] = r match {
    case TRowExtend(l, t, e) => typeVariables(t) union typeVariables(e)
    case TRowEmpty => Nil
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
    typeVariables(ts.stype) diff ts.svariables
  }

  def typeVariables(environment: Environment): List[TVariable] = {
    environment.foldLeft(List[TVariable]()) { (tvs, nt) => tvs union typeVariables(nt._2) }
  }
}
