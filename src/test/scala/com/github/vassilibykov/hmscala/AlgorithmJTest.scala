package com.github.vassilibykov.hmscala

class AlgorithmJTest extends InferenceTest {
  override def typeOf(expr: Expression): Type = Environment.empty.inferTypeJ(expr, new TypeUnifier)
}
