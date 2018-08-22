package com.github.vassilibykov.hmscala

class AlgorithmWTest extends InferenceTest {
  override def typeOf(expr: Expression): Type = Environment.empty.inferTypeW(expr)._1
}
