package org.onion_lang.toys

/**
 * @author Kota Mizushima
 */
class Interpreter {
  def evaluate(env:Environment, ast:AST): Value = {
    def evalRecursive(ast:AST): Value = {
      ast match{
        case Block(exprs) =>
          val local = new Environment(Some(env))
          exprs.foldLeft(UnitValue:Value){(result, x) => evaluate(local, x)}
        case IfExpr(cond, pos, neg) =>
          evalRecursive(cond) match {
            case BooleanValue(true) => evalRecursive(pos)
            case BooleanValue(false) => evalRecursive(neg)
            case _ => sys.error("Runtime Error!")
          }
        case LessOp(left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (IntValue(lval), IntValue(rval)) => BooleanValue(lval < rval)
            case _ => sys.error("Runtime Error!")
          }
        case AddOp(left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (IntValue(lval), IntValue(rval)) => IntValue(lval + rval)
            case (StringValue(lval), rval) => StringValue(lval + rval)
            case (lval, StringValue(rval)) => StringValue(lval + rval)
            case _ => sys.error("Runtime Error!")
          }
        case SubOp(left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (IntValue(lval), IntValue(rval)) => IntValue(lval - rval)
            case _ => sys.error("Runtime Error!")
          }
        case MulOp(left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (IntValue(lval), IntValue(rval)) => IntValue(lval * rval)
            case _ => sys.error("Runtime Error!")
          }
        case DivOp(left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (IntValue(lval), IntValue(rval)) => IntValue(lval / rval)
            case _ => sys.error("Runtime Error!")
          }
        case IntVal(value) =>
          IntValue(value)
        case StringVal(value) =>
          StringValue(value)
        case PrintLine(value) =>
          val v = evalRecursive(value)
          println(v)
          v
        case Ident(name) => env(name)
        case ValDeclaration(vr, value) =>
          env(vr) = evalRecursive(value)
        case Assignment(vr, value) =>
          env.set(vr, evalRecursive(value))
        case func@Func(_, _) =>
          FunctionValue(func, Some(env))
        case FuncDef(name, func) =>
          env(name) = FunctionValue(func, Some(env)): Value
        case FuncCall(func, params) =>
          evalRecursive(func) match{
            case FunctionValue(Func(fparams, proc), cenv) =>
              val local = new Environment(cenv)
              (fparams zip params).foreach{ case (fp, ap) =>
                local(fp) = evalRecursive(ap)
              }
              evaluate(local, proc)
            case NativeFunctionValue(body) =>
              body(params.map{p => evalRecursive(p)})
            case _ => sys.error("Runtime Error!")
          }
      }
    }
    evalRecursive(ast)
  }
}
