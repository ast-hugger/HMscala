package com.github.vassilibykov.hmscala

import org.scalatest.FunSuite

abstract class InferenceTest extends FunSuite {
  def typeOf(expr: Expression): Type

  test("int literal style should be TInt") {
    assertResult(typeOf(IntLiteral(42))) (TInt)
  }

  test("bool literal style should be TBool") {
    assertResult(typeOf(BoolLiteral(true))) (TBool)
    assertResult(typeOf(BoolLiteral(false))) (TBool)
  }

  test("abstraction type should be function") {
    assertResult(typeOf(Abstraction("x", Variable("x")))) (TFunction(TVariable("a"), TVariable("a")))
  }

  test("application type should be returned type") {
    assertResult(typeOf(Application(Abstraction("x", Variable("x")), IntLiteral(42)))) (TInt)
  }
}
