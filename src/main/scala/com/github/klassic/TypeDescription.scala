package com.github.klassic

sealed abstract class TypeDescription(val image: String)
object TypeDescription {
  private var n: Int = 0
  def newTypeVariable(): TypeDescription = {
    n += 1; TypeVariable("'a" + n)
  }
  val EmptySubstitution: Substitution = new Substitution {
    def lookup(t: TypeVariable): TypeDescription = t
  }
  abstract class Substitution extends Function1[TypeDescription, TypeDescription] {
    def lookup(x: TypeVariable): TypeDescription

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

    def extend(x: TypeVariable, t: TypeDescription): Substitution = new Substitution {
      def lookup(y: TypeVariable): TypeDescription = {
        if (x == y) t else Substitution.this.lookup(y)
      }
    }
  }
  case class TypeScheme(typeVariables: List[TypeVariable], description: TypeDescription) {
    def newInstance: TypeDescription = {
      typeVariables.foldLeft(EmptySubstitution)((s, tv) => s.extend(tv, newTypeVariable())).apply(description)
    }
  }
  type Environment = List[(String, TypeScheme)]
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
  case class FunctionType(paramTypes: List[TypeDescription], returnType: TypeDescription) extends
    TypeDescription(s"(${paramTypes.mkString(", ")}) => ${returnType}")
}
