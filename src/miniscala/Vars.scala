package miniscala

import miniscala.Ast._
import miniscala.nSet._

/**
  * Computation of free variables (or rather, identifiers).
  */
object Vars {

  def freeVars(e: Exp): nSet[Id] = e match {
    case _: Literal => nSet()
    case VarExp(x) => nSet(x)
    case BinOpExp(leftexp, _, rightexp) => freeVars(leftexp) ++ freeVars(rightexp)
    case UnOpExp(_, exp) => freeVars(exp)
    case IfThenElseExp(condexp, thenexp, elseexp) => union(union(freeVars(condexp), freeVars(thenexp)),freeVars(elseexp))
    case BlockExp(vals, defs, exp) =>
      var fv = freeVars(exp)
      for (d <- defs)
        fv = union(fv,freeVars(d))
      for (d <- defs)
        fv = difference(fv, declaredVars(d))
      for (d <- vals.reverse)
        fv = union(difference(fv,declaredVars(d)),freeVars(d))
      fv
    case TupleExp(exps) =>
      var fv = nSet[Id]()
      for (exp <- exps)
        fv = union(freeVars(exp),fv)
      fv
    case MatchExp(exp, cases) =>
      var fv = freeVars(exp)
      for (c <- cases)
        fv = difference(union(fv,(freeVars(c.exp))),c.pattern)
      fv
    case CallExp(funexp, args) =>
      ???
    case LambdaExp(params, body) => freeVars(body) -- params.map(p => p.x)
  }

  def freeVars(decl: Decl): nSet[Id] = decl match {
    case ValDecl(_, _, exp) => freeVars(exp)
    case DefDecl(_, params, _, body) => freeVars(body) -- params.map(p => p.x)
  }


  def declaredVars(decl: Decl): nSet[Id] = decl match {
    case ValDecl(x, _, _) => nSet(x)
    case DefDecl(x, _, _, _) => nSet(x)
  }
}
