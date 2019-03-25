package miniscala

import miniscala.Ast._
import miniscala.Unparser.unparse

import scala.io.StdIn

/**
  * Interpreter for MiniScala.
  */
object Interpreter {

  sealed abstract class Val
  case class IntVal(v: Int) extends Val
  case class BoolVal(v: Boolean) extends Val
  case class FloatVal(v: Float) extends Val
  case class StringVal(v: String) extends Val
  case class TupleVal(vs: List[Val]) extends Val
  case class ClosureVal(params: List[FunParam], optrestype: Option[Type], body: Exp, defs: List[DefDecl], env: Env) extends Val

  type Env = Map[Id, Val]


  def eval(e: Exp, env: Env): Val = e match {
    case IntLit(c) => IntVal(c)
    case BoolLit(c) => BoolVal(c)
    case FloatLit(c) => FloatVal(c)
    case StringLit(c) => StringVal(c)
    case VarExp(x) =>
      env.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e))
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = eval(leftexp, env)
      val rightval = eval(rightexp, env)
      op match {
        case PlusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 + v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 + v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 + v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 + v2)
            case (StringVal(v1), StringVal(v2)) => StringVal(v1 + v2)
            case (StringVal(v1), IntVal(v2)) => StringVal(v1 + v2)
            case (StringVal(v1), FloatVal(v2)) => StringVal(v1 + v2)
            case (IntVal(v1), StringVal(v2)) => StringVal(v1 + v2)
            case (FloatVal(v1), StringVal(v2)) => StringVal(v1 + v2)
            case _ => throw new InterpreterError(s"Type mismatch at '+', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MinusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 - v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 - v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 - v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 - v2)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MultBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 * v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 * v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 * v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 * v2)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case DivBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 / v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 / v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 / v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 / v2)
            case _ => throw new InterpreterError(s"Type mismatch at '/', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case ModuloBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Modulo by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 % v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 % v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 % v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 % v2)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case EqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => BoolVal(v1 == v2)
            case (FloatVal(v1), FloatVal(v2)) => BoolVal(v1 == v2)
            case (IntVal(v1), FloatVal(v2)) => BoolVal(v1 == v2)
            case (FloatVal(v1), IntVal(v2)) => BoolVal(v1 == v2)
            case (StringVal(v1), StringVal(v2)) => BoolVal(v1 == v2)
            case (TupleVal(vs1), TupleVal(vs2)) => BoolVal(vs1 == vs2)
            case _ => throw new InterpreterError(s"Type mismatch at '==', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case LessThanBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => BoolVal(v1 < v2)
            case (FloatVal(v1), FloatVal(v2)) => BoolVal(v1 < v2)
            case (IntVal(v1), FloatVal(v2)) => BoolVal(v1 < v2)
            case (FloatVal(v1), IntVal(v2)) => BoolVal(v1 < v2)
            case _ => throw new InterpreterError(s"Type mismatch at '<', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case LessThanOrEqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => BoolVal(v1 <= v2)
            case (FloatVal(v1), FloatVal(v2)) => BoolVal(v1 <= v2)
            case (IntVal(v1), FloatVal(v2)) => BoolVal(v1 <= v2)
            case (FloatVal(v1), IntVal(v2)) => BoolVal(v1 <= v2)
            case _ => throw new InterpreterError(s"Type mismatch at '<=', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MaxBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) if v1 < v2 => IntVal(v2)
            case (IntVal(v1), IntVal(v2)) => IntVal(v1)
            case (FloatVal(v1), FloatVal(v2)) if v1 < v2 => FloatVal(v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1)
            case (IntVal(v1), FloatVal(v2)) if v1 < v2 => FloatVal(v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1)
            case (FloatVal(v1), IntVal(v2)) if v1 < v2 => FloatVal(v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1)
            case _ => throw new InterpreterError(s"Type mismatch at 'max', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case AndBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => BoolVal(v1 & v2)
            case _ => throw new InterpreterError(s"Type mismatch at '&', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case OrBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => BoolVal(v1 | v2)
            case _ => throw new InterpreterError(s"Type mismatch at '|', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
      }
    case UnOpExp(op, exp) =>
      val expval = eval(exp, env)
      op match {
        case NegUnOp() =>
          expval match {
            case IntVal(v) => IntVal(-v)
            case FloatVal(v) => FloatVal(-v)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected value ${valueToString(expval)}", op)
          }
        case NotUnOp() =>
          expval match {
            case BoolVal(v) => BoolVal(!v)
            case _ => throw new InterpreterError(s"Type mismatch at '!', unexpected value ${valueToString(expval)}", op)
          }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val cexp = eval(condexp,env)
      cexp match {
        case BoolVal(c) =>
          if (c) eval(thenexp,env) else eval(elseexp,env)
        case _ => throw new InterpreterError(s"Type mismatch at 'bool', unexpected value ${valueToString(cexp)}", e)
      }
    case BlockExp(vals, defs, exp) =>
      var env1 = env
      for (d <- vals) {
        val v = eval(d.exp, env1)
        checkValueType(v, d.opttype,e)
        env1 = env1 + (d.x -> v)
      }
      var env2 = env1
      for (d <- defs)
        env2 = env2 + (d.fun -> ClosureVal(d.params,d.optrestype,d.body,defs,env1))
      eval(exp, env2)
    case TupleExp(exps) =>
      var vals = List[Val]()
      for (exp <- exps)
        vals = eval(exp, env) :: vals
      TupleVal(vals.reverse)
    case MatchExp(exp, cases) =>
      val expval = eval(exp, env)
      expval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            if (vs.length == c.pattern.length) {
              val ms = c.pattern.zip(vs)
              return eval(c.exp,env++ms)
            }
          }
          throw new InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw new InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
    case CallExp(funexp, args) =>
      eval(funexp,env) match {
        case ClosureVal(params,op,body,defs,env) =>
          if (args.length != params.length) throw new InterpreterError("Wrong amount of arguments",funexp)
          var env1 = env
          for((p,a) <- params.zip(args)) {
            val z = eval(a,env)
            checkValueType(z, p.opttype, e)
            env1 = env1 + (p.x -> z)
          }
          var env2 = env1
          for (d <- defs) {
            env2 = env2 + (d.fun -> ClosureVal(params, op, body, defs, env1))
          }
          val res = eval(body,env2)
          checkValueType(res,op,e)
          res
        case _ => throw new InterpreterError("Expected closure but got ",funexp)
      }


    case LambdaExp(params, body) =>
      ClosureVal(params,None,body,Nil,env)
  }

  /**
    * Checks whether value `v` has type `ot` (if present), generates runtime type error otherwise.
    */
  def checkValueType(v: Val, ot: Option[Type], n: AstNode): Unit = ot match {
    case Some(t) =>
      (v, t) match {
        case (IntVal(_), IntType()) |
             (BoolVal(_), BoolType()) |
             (FloatVal(_), FloatType()) |
             (IntVal(_), FloatType()) |
             (StringVal(_), StringType()) => // do nothing
        case (TupleVal(vs), TupleType(ts)) if vs.length == ts.length =>
          for ((vi, ti) <- vs.zip(ts))
            checkValueType(vi, Some(ti), n)
        case (ClosureVal(cparams, optcrestype, _, _, _), FunType(paramtypes, restype)) if cparams.length == paramtypes.length =>
          for ((p, t) <- cparams.zip(paramtypes))
            checkTypesEqual(t, p.opttype, n)
          checkTypesEqual(restype, optcrestype, n)
        case _ =>
          throw new InterpreterError(s"Type mismatch: value ${valueToString(v)} does not match type ${unparse(t)}", n)
      }
    case None => // do nothing
  }

  /**
    * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
    */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw new InterpreterError(s"Type mismatch: type ${unparse(t1)} does not match expected type ${unparse(t2)}", n)
    case None => // do nothing
  }

  /**
    * Converts a value to its string representation (for error messages).
    */
  def valueToString(v: Val): String = v match {
    case IntVal(c) => c.toString
    case FloatVal(c) => c.toString
    case BoolVal(c) => c.toString
    case StringVal(c) => c
    case TupleVal(vs) => vs.map(v => valueToString(v)).mkString("(", ",", ")")
    case ClosureVal(params, _, exp, _, _) => // the resulting string ignores the result type annotation and the declaration environment
      s"<(${params.map(p => unparse(p)).mkString(",")}), ${unparse(exp)}>"
  }

  /**
    * Builds an initial environment, with a value for each free variable in the program.
    */
  def makeInitialEnv(program: Exp): Env = {
    var env = Map[Id, Val]()
    for (x <- Vars.freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      env = env + (x -> IntVal(StdIn.readInt()))
    }
    env
  }

  /**
    * Prints message if option -trace is used.
    */
  def trace(msg: String): Unit =
    if (Options.trace)
      println(msg)

  /**
    * Exception thrown in case of MiniScala runtime errors.
    */
  class InterpreterError(msg: String, node: AstNode) extends MiniScalaError(s"Runtime error: $msg", node.pos)
}
