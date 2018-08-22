package com.github.vassilibykov.hmscala

abstract sealed class Expression {
  /** Allows creating an Application instance using the natural syntax. */
  def apply(arg: Expression) = Application(this, arg)
}

case class IntLiteral(value: Int) extends Expression {
  override def toString: String = value.toString
}

case class BoolLiteral(value: Boolean) extends Expression {
  override def toString: String = value.toString
}

case class Variable(name: String) extends Expression {
  override def toString: String = name
}

case class Application(fun: Expression, arg: Expression) extends Expression {
  override def toString: String = fun match {
    case _: Abstraction => s"($fun) $arg"
    case _ => s"$fun $arg"
  }
}

case class Abstraction(varName: String, body: Expression) extends Expression {
  override def toString: String = s"\u03BB$varName. $body"
}