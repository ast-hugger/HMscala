package com.github.vassilibykov.hmscala

/**
  * A map of type variable names to types they are replaced with. This class
  * could be a type alias for a simple map. However, because of its apply()
  * method we can conveniently use it as if it were a function we can apply to
  * things in which variables need to be substituted.
  */
class Substitution private (private val replacements: Map[String,Type]) {
  def lookup(name: String): Option[Type] = replacements.get(name)
  def without(names: Iterable[String]) = new Substitution(replacements -- names)
  def + (other: Substitution): Substitution = new Substitution(replacements ++ other.replacements)
  def apply[T](x: Substitutable[T]): T = x.applySubstitution(this)
}

object Substitution {
  def apply(): Substitution = new Substitution(Map())

  def apply(name: String, t: Type): Substitution = t match {
    case _: TVariable => Substitution()
    case _ if t.freeVariables contains name => throw new TypeError(s"variable $name is free in type $t")
    case _ => new Substitution(Map(name -> t))
  }

  def toInstantiate(vars: Iterable[String]): Substitution = new Substitution(vars.map(v => v -> TVariable(v)).toMap)
}

trait Substitutable[T] {
  def applySubstitution(subst: Substitution): T
}
