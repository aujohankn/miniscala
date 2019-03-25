package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker._
import miniscala.parser.Parser.parse

object Test {
  def main(args: Array[String]): Unit = {
    testVal("{ def f(x) = x; f(2)}", IntVal(2))
    testTypeFail("{ def f(x) = x; f(2)}")
    testFail("f(100)")
    test("{ def f(x: Int): Int = x; f(2) }", IntVal(2), IntType())
    testFail("{ def f(x: Int): Int = x; f(2, 3) }")
    test("{ val x = 2; { def p(): Int = x * 2;{ def q():Int = p(); { val x = 5; { def p(): Int = x + 1; q() } } } } }", IntVal(4),IntType())
    testTypeFail("{ def f(x: Int => Int): Int = x(3); f((x: Int) => true) }")
    testVal("((x) => x(2)) ((x) => x - 3) ",IntVal(-1))
  }

  def test(prg: String, rval: Val, rtype: Type) = {
    testVal(prg, rval)
    testType(prg, rtype)
  }

  def testFail(prg: String) = {
    testValFail(prg)
    testTypeFail(prg)
  }

  def testVal(prg: String, value: Val, env: Env = Map[Id, Val]()) = {
    assert(eval(parse(prg), env) == value)
  }

  def testType(prg: String, out: Type, tenv: TypeEnv = Map[Id, Type]()) = {
    assert(typeCheck(parse(prg), tenv) == out)
  }

  def testValFail(prg: String) = {
    try {
      eval(parse(prg), Map[Id, Val]())
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

  def testTypeFail(prg: String) = {
    try {
      typeCheck(parse(prg), Map[Id, Type]())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}