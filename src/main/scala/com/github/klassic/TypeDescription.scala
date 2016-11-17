package com.github.klassic

sealed abstract class TypeDescription(val image: String) {
  override def toString: String = image
}
object TypeDescription {
  case class TypeVariable(name: String) extends TypeDescription(name)
  case object IntType extends TypeDescription("Int")
  case object ShortType extends TypeDescription("Short")
  case object ByteType extends TypeDescription("Byte")
  case object LongType extends TypeDescription("Long")
  case object FloatType extends TypeDescription("Float")
  case object DoubleType extends TypeDescription("Double")
  case object BooleanType extends TypeDescription("Boolean")
  case object UnitType extends TypeDescription("Unit")
  case object DynamicType extends TypeDescription("*")
  case object UnknownType extends TypeDescription("?")
  case object ErrorType extends TypeDescription("!")
  case class ObjectType(name: String) extends TypeDescription(name)
  case class FunctionType(paramTypes: List[TypeDescription], returnType: TypeDescription) extends TypeDescription(s"(${paramTypes.mkString(", ")}) => ${returnType}")
  case class TypeScheme(typeVariables: List[TypeVariable], description: TypeDescription) {
    def newInstance: TypeDescription = {
      typeVariables.foldLeft(EmptySubstitution)((s, tv) => s.extend(tv, newTypeVariable())).apply(description)
    }
  }
  case class TypeConstructor(t: String, ks: List[TypeDescription]) extends TypeDescription(t + "[" + ks.mkString(", ") + "]")

  private var n: Int = 0
  def newTypeVariable(): TypeDescription = {
    n += 1; TypeVariable("'a" + n)
  }
  type Environment = Map[String, TypeScheme]
  val EmptySubstitution: Substitution = new Substitution(Map.empty)
  class Substitution(val map: Map[TypeVariable, TypeDescription]) extends Function1[TypeDescription, TypeDescription] {
    def lookup(x: TypeVariable): TypeDescription = {
      map.get(x).getOrElse(x)
    }

    def apply(t: TypeDescription): TypeDescription = t match {
      case tv@TypeVariable(a) =>
        val u = lookup(tv)
        if (t == u) t else apply(u)
      case FunctionType(t1, t2) => FunctionType(t1.map{t => apply(t)}, apply(t2))
      case IntType => IntType
      case ShortType => ShortType
      case ByteType => ByteType
      case LongType => LongType
      case FloatType => FloatType
      case DoubleType => DoubleType
      case BooleanType => BooleanType
      case UnitType => UnitType
      case DynamicType => DynamicType
    }

    def extend(tv: TypeVariable, td: TypeDescription): Substitution = new Substitution(this.map + (tv -> td))
  }

  def lookup(x: String, environment: Environment): Option[TypeScheme] = environment.get(x) match {
    case Some(t) => Some(t)
    case None => None
  }

  def generate(t: TypeDescription, environment: Environment): TypeScheme = {
    TypeScheme(typeVariables(t) diff typeVariables(environment), t)
  }

  def typeVariables(t: TypeDescription): List[TypeVariable] = t match {
    case tv @ TypeVariable(a) =>
      List(tv)
    case FunctionType(t1, t2) =>
      t1.flatMap{typeVariables} union typeVariables(t2)
    case TypeConstructor(k, ts) =>
      ts.foldLeft(List[TypeVariable]()){(tvs, t) => tvs union typeVariables(t)}
  }

  def typeVariables(ts: TypeScheme): List[TypeVariable] = {
    typeVariables(ts.description) diff ts.typeVariables
  }

  def typeVariables(environment: Environment): List[TypeVariable] = {
    environment.foldLeft(List[TypeVariable]()) { (tvs, nt) => tvs union typeVariables(nt._2) }
  }

  def mgu(t: TypeDescription, u: TypeDescription, s: Substitution): Substitution = (s(t), s(u)) match {
    case (TypeVariable(a), TypeVariable(b)) if a == b =>
      s
    case (TypeVariable(a), _) if !(typeVariables(u) contains a) =>
      s.extend(TypeVariable(a), u)
    case (_, TypeVariable(a)) =>
      mgu(u, t, s)
    case (FunctionType(t1, t2), FunctionType(u1, u2)) =>
      (t1 zip u1).foldLeft(s){ case (s, (t, u)) => mgu(t, u, s)}
    case (TypeConstructor(k1, ts), TypeConstructor(k2, us)) if k1 == k2 =>
      (ts zip us).foldLeft(s){(s, tu) => mgu(tu._1, tu._2, s)}
    case _ =>
      throw TyperException("cannot unify " + s(t) + " with " + s(u))
  }
}
