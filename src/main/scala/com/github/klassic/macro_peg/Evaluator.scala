package com.github.klassic.macro_peg

import com.github.klassic.macro_peg.EvaluationResult.{Failure, Success}
import com.github.klassic.macro_peg.Ast.Position

case class Evaluator(grammar: Ast.Grammar, strategy: EvaluationStrategy = EvaluationStrategy.CallByName) {
  private def expand(node: Ast.Expression): Ast.Expression = node match {
    case Ast.CharClass(pos, positive, elems) =>
      Ast.CharSet(pos, positive, elems.foldLeft(Set[Char]()){
        case (set, Ast.CharRange(f, t)) => (f to t).foldLeft(set)((set, c) => set + c)
        case (set, Ast.OneChar(c)) => set + c
      })
    case Ast.Alternation(pos, e1, e2) => Ast.Alternation(pos, expand(e1), expand(e2))
    case Ast.Sequence(pos, e1, e2) => Ast.Sequence(pos, expand(e1), expand(e2))
    case Ast.Repeat0(pos, body) => Ast.Repeat0(pos, expand(body))
    case Ast.Repeat1(pos, body) => Ast.Repeat1(pos, expand(body))
    case Ast.Optional(pos, body) => Ast.Optional(pos, expand(body))
    case Ast.AndPredicate(pos, body) => Ast.AndPredicate(pos, expand(body))
    case Ast.NotPredicate(pos, body) => Ast.NotPredicate(pos, expand(body))
    case Ast.Call(pos, name, args) => Ast.Call(pos, name, args.map{expand})
    case e => e
  }
  private val FUNS: Map[Symbol, Ast.Expression] = {
    grammar.rules.map{r => r.name -> (if(r.args.isEmpty) expand(r.body) else Ast.Function(r.body.pos, r.args, expand(r.body)))}.toMap
  }

  private[this] def eval(input: String, exp: Ast.Expression): EvaluationResult = {
    def evaluateIn(input: String, exp: Ast.Expression, bindings: Map[Symbol, Ast.Expression]): EvaluationResult = exp match {
      case Ast.Debug(pos, body) =>
        println("DEBUG: " + extract(body, bindings))
        Success(input)
      case Ast.Alternation(pos, l, r) =>
        evaluateIn(input, l, bindings).orElse(evaluateIn(input, r, bindings))
      case Ast.Sequence(pos, l, r) =>
        for(in1 <- evaluateIn(input, l, bindings);
            in2 <- evaluateIn(in1, r, bindings)) yield in2
      case Ast.AndPredicate(pos, body) =>
        evaluateIn(input, body, bindings).map{_ => input}
      case Ast.NotPredicate(pos, body) =>
        evaluateIn(input, body, bindings) match {
          case Success(in) => Failure
          case Failure => Success(input)
        }
      case Ast.Call(pos, name, params) =>
        val fun = bindings(name).asInstanceOf[Ast.Function]
        val args = fun.args
        val body = fun.body
        if(args.length != params.length) throw EvaluationException(s"args length of $name should be equal to params length")
        val result: Either[Failure.type, (String, Map[Symbol, Ast.Expression])]= strategy match {
          case EvaluationStrategy.CallByName =>
            Right(input -> args.zip(params.map(p => extract(p, bindings))).toMap)
          case EvaluationStrategy.CallByValueSeq =>
            def loop(input: String, params: List[Ast.Expression], values: List[Ast.Expression]): Either[Failure.type, (String, List[Ast.Expression])] = params match {
              case p::ps =>
                evaluateIn(input, p, bindings) match {
                  case Success(rest) =>
                    val value = Ast.StringLiteral(Position(-1, -1), input.substring(0, input.length - rest.length))
                    loop(rest, ps, value::values)
                  case Failure =>
                    Left(Failure)
                }
              case Nil =>
                Right(input -> values.reverse)
            }
            loop(input, params, Nil) match {
              case Left(_) =>
                Left(Failure)
              case Right((input, values)) =>
                Right(input -> args.zip(values).toMap)
            }
          case EvaluationStrategy.CallByValuePar =>
            val results = params.map{p => evaluateIn(input, p, bindings)}
            if(results.forall{_.isSuccess}) {
              val values = results.map(_.asInstanceOf[Success].get).map{rest =>
                Ast.StringLiteral(Position(-1, -1), input.substring(0, input.length - rest.length))
              }
              Right(input -> args.zip(values).toMap)
            } else {
              Left(Failure)
            }
        }
        result match {
          case Left(_) =>
            Failure
          case Right((input, nparams)) =>
            println(s"call with: ${input} and ${nparams}")
            val r = evaluateIn(input, body, bindings ++ nparams)
            println(r)
            r
        }
      case Ast.Identifier(pos, name) =>
        val body = bindings(name)
        evaluateIn(input, body, bindings)
      case Ast.Optional(pos, body) =>
        evaluateIn(input, body, bindings).orElse(Success(input))
      case Ast.Repeat0(pos, body) =>
        var in = input
        var result: EvaluationResult = Failure
        while({result = evaluateIn(in, body, bindings); result != Failure}) {
          in = result.get
        }
        Success(in)
      case Ast.Repeat1(pos, body) =>
        var in = input
        var result = evaluateIn(in, body, bindings)
        result match {
          case Failure => Failure
          case Success(next) =>
            var in: String = next
            while({result = evaluateIn(in, body, bindings); result != Failure}) {
              in = result.get
            }
            Success(in)
        }
      case Ast.StringLiteral(pos, target) =>
        if (input.startsWith(target)) Success(input.substring(target.length)) else Failure
      case Ast.CharSet(_, positive, set) =>
        if(input == "" || (positive != set(input(0)))) Failure
        else Success(input.substring(1))
      case Ast.Wildcard(pos) =>
        if (input.length >= 1) Success(input.substring(1)) else Failure
      case Ast.CharClass(_, _, _) => sys.error("must be unreachable")
      case Ast.Function(_, _, _) => sys.error("must be unreachable")
    }
    evaluateIn(input, exp, FUNS)
  }

  private[this] def extract(exp: Ast.Expression, bindings: Map[Symbol, Ast.Expression]): Ast.Expression = exp match {
    case Ast.Debug(pos, body) =>
      Ast.Debug(pos, extract(body, bindings))
    case Ast.Alternation(pos, l, r) =>
      Ast.Alternation(pos, extract(l, bindings), extract(r, bindings))
    case Ast.Sequence(pos, l, r) =>
      Ast.Sequence(pos, extract(l, bindings), extract(r, bindings))
    case Ast.AndPredicate(pos, body) =>
      Ast.AndPredicate(pos, extract(body, bindings))
    case Ast.NotPredicate(pos, body) =>
      Ast.NotPredicate(pos, extract(body, bindings))
    case Ast.Call(pos, name, params) =>
      Ast.Call(pos, name, params.map(r => extract(r, bindings)))
    case Ast.Identifier(pos, name) =>
      bindings.getOrElse(name, Ast.Identifier(pos, name))
    case Ast.Function(pos, args, body) =>
      Ast.Function(pos, args, body)
    case Ast.Optional(pos, body) =>
      Ast.Optional(pos, extract(body, bindings))
    case Ast.Repeat0(pos, body) =>
      Ast.Repeat0(pos, extract(body, bindings))
    case Ast.Repeat1(pos, body) =>
      Ast.Repeat1(pos, extract(body, bindings))
    case Ast.StringLiteral(pos, target) =>
      Ast.StringLiteral(pos, target)
    case Ast.Wildcard(pos) =>
      Ast.Wildcard(pos)
    case ast@Ast.CharClass(_, _, _) => ast
    case ast@Ast.CharSet(_, _, _) => ast
  }

  def evaluate(input: String, start: Symbol): EvaluationResult = {
    val body = FUNS(start)
    eval(input, body)
  }
}
