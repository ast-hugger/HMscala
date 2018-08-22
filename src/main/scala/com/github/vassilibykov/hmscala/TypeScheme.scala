package com.github.vassilibykov.hmscala

class TypeScheme(vars: Set[String], base: Type) extends Substitutable[TypeScheme] {

  def freeVariables: Set[String] = base.freeVariables -- vars

  override def toString: String = if (vars.isEmpty) base.toString else s"\u2200$vars.$base"

  override def applySubstitution(subst: Substitution): TypeScheme = new TypeScheme(vars, subst.without(vars)(base))

  def instantiate(): Type = Substitution.toInstantiate(vars)(base)
}

object TypeScheme {
  def apply(t: Type) = new TypeScheme(Set.empty, t)
}
