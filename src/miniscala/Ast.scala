package miniscala

import scala.util.parsing.input.{NoPosition, Position, Positional}

/**
  * Abstract syntax tree representation of MiniScala programs.
  */
object Ast {

  /**
    * An AST node contains information about its position in the source code.
    */
  sealed abstract class AstNode extends Positional

  /**
    * Expressions.
    */
  sealed abstract class Exp extends AstNode {
    def eval(): Int
  }

  case class BinOpExp(leftexp: Exp, op: BinOp, rightexp: Exp) extends Exp {
    def eval() = op.eval(leftexp.eval(),rightexp.eval())
  }

  case class UnOpExp(op: UnOp, exp: Exp) extends Exp {
    def eval() = op.eval(exp.eval())
  }

  case class IntLit(c: Int) extends Exp {
    def eval() = c
  }

  /**
    * Binary operators.
    */
  sealed abstract class BinOp extends AstNode {
    def eval(v1: Int, v2: Int): Int
  }

  case class PlusBinOp() extends BinOp {
    def eval(v1: Int, v2: Int) = v1 + v2
  }

  case class MinusBinOp() extends BinOp {
    def eval(v1: Int, v2: Int) = v1 - v2
  }

  case class MultBinOp() extends BinOp {
    def eval(v1: Int, v2: Int) = v1 * v2
  }

  case class DivBinOp() extends BinOp {
    def eval(v1: Int, v2: Int) = v1/v2
  }

  case class ModuloBinOp() extends BinOp {
    def eval(v1: Int, v2: Int) = v1 % v2
  }

  case class MaxBinOp() extends BinOp {
    def eval(v1: Int, v2: Int) =
      if (v1>v2) v1 else v2
  }

  /**
    * Unary operators.
    */
  sealed abstract class UnOp extends AstNode {
    def eval(v: Int): Int
  }

  case class NegUnOp() extends UnOp {
    def eval(v: Int) = -v
  }

  /**
    * Exception with a message and (optionally) a source code position.
    */
  class MiniScalaError(msg: String, pos: Position = NoPosition)
    extends RuntimeException(if (pos != NoPosition) s"$msg at line ${pos.line} column ${pos.column}" else msg)
}
