package com.github.vassilibykov.hmscala

sealed abstract class Type extends Substitutable[Type] {
  def freeVariables: Set[String] = Set()

  override def applySubstitution(subst: Substitution): Type = this

  def mostGeneralUnifier(other: Type): Either[String, Substitution] = other match {
    case tv: TVariable => Right(Substitution(tv.name, this))
    case _ => Left(s"types $this and $other do not unify")
  }
}

object TInt extends Type {
  override def toString: String = "int"

  override def mostGeneralUnifier(other: Type): Either[String, Substitution] =
    if (other == this) Right(Substitution())
    else super.mostGeneralUnifier(other)
}

object TBool extends Type {
  override def toString: String = "bool"

  override def mostGeneralUnifier(other: Type): Either[String, Substitution] =
    if (other == this) Right(Substitution())
    else super.mostGeneralUnifier(other)
}

case class TVariable(name: String) extends Type {
  override def freeVariables: Set[String] = Set(name)

  override def toString: String = name

  override def applySubstitution(subst: Substitution): Type = subst.lookup(name) getOrElse this

  override def mostGeneralUnifier(other: Type): Either[String, Substitution] = Right(Substitution(name, other))
}

case class TFunction(from: Type, to: Type) extends Type {
  override def freeVariables: Set[String] = from.freeVariables union to.freeVariables

  override def toString: String = from match {
    case _: TFunction => s"($from) -> $to"
    case _ => s"$from -> $to"
  }

  override def applySubstitution(subst: Substitution): Type =
    TFunction(subst(from), subst(to))

  override def mostGeneralUnifier(other: Type): Either[String, Substitution] = other match {
    case fun: TFunction =>
      for {
        uFrom <- from.mostGeneralUnifier(fun.from)
        uTo <- uFrom(to).mostGeneralUnifier(uFrom(fun.to))
      } yield uFrom + uTo
    case _ => super.mostGeneralUnifier(other)
  }
}
