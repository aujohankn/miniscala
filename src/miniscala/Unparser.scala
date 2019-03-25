package miniscala

import miniscala.Ast._

/**
  * Unparser for MiniScala.
  */
object Unparser {
  def unparse(e: AstNode): String = e match {
    case IntLit(c) => "" + c
    case BoolLit(c) => "" + c
    case FloatLit(c) => "" + c
    case StringLit(c) => c
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = unparse(leftexp)
      val rightval = unparse(rightexp)
      op match {
        case PlusBinOp() => "(" + leftval + " + " + rightval + ")"
        case MinusBinOp() => "(" + leftval + " - " + rightval + ")"
        case MultBinOp() => "(" + leftval + " * " + rightval + ")"
        case DivBinOp() => "(" + leftval + " / " + rightval + ")"
        case ModuloBinOp() => "(" + leftval + " % " + rightval + ")"
        case MaxBinOp() => "(" + leftval + " max " + rightval + ")"
        case EqualBinOp() => "(" + leftval + "==" + rightval + ")"
        case LessThanBinOp() => "(" + leftval + "<" + rightval + ")"
        case LessThanOrEqualBinOp() => "(" + leftval + "<=" + rightval + ")"
        case AndBinOp() => "(" + leftval + "&" + rightval + ")"
        case OrBinOp() => "(" + leftval + "|" + rightval + ")"
      }
    case UnOpExp(op, exp) =>
      val expval = unparse(exp)
      op match {
        case NegUnOp() => "(" +  "-" + expval  + ")"
        case NotUnOp() =>  "!" + "(" + expval + ")"
      }
    case BlockExp(vals, defs, exp) =>
      var Blockstring = ""
      for(v<-vals)
        Blockstring = Blockstring + unparse(v) + "; "
      for (d<-defs)
        Blockstring = Blockstring + unparse(d)
      "{" + Blockstring + unparse(exp) + "}"
    case ValDecl(x, None, exp) => "val " + x + " = " + unparse(exp)
    case ValDecl(x, Some(t), exp) => "val " + x + s": ${unparse(t)}" + "=" + unparse(exp)
    case IntType() => "Int"
    case BoolType() => "Boolean"
    case FloatType() => "Float"
    case StringType() => "String"
    case TupleType(types) => types.map(unparse).mkString("(", ";", ")")
    case VarExp(x) => x
    case IfThenElseExp(condexp,thenexp,elseexp) => "if " + "(" + condexp + ") " + thenexp + " else " + elseexp
    case TupleExp(exps) => exps.map(unparse).mkString("(" , "," , ")")
    case MatchExp(exp,cases) => exp + cases.map(unparse).mkString("(", ";", ")")
    case MatchCase(pattern, exp) => exp + "match { case " + pattern.mkString("(", ";", ")") + "}"
    case DefDecl(fun,params,optrestype,body) => "def" + fun + params + ":" + optrestype + "=" + unparse(body)
    case CallExp(fun,args) =>
      var argstring = ""
      for (a<-args)
        argstring = argstring + unparse(a) + ","
      fun + "(" + argstring + ")"
  }
}