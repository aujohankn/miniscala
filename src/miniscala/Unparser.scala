package miniscala

import miniscala.Ast._
import miniscala.Interpreter.eval

/**
  * Unparser for MiniScala.
  */
object Unparser {

  def unparse(e: Exp): String = e match {
    case IntLit(c) => "" + c
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
      }
    case UnOpExp(op, exp) =>
      val expval = unparse(exp)
      op match {
        case NegUnOp() => "(" +  "-" + expval  + ")"
      }
  }
}
