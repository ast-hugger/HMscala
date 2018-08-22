package com.github.vassilibykov.hmscala

import scala.util.{Failure, Success, Try}

object HM {
  private val t = BoolLiteral(true)
  private val f = BoolLiteral(false)
  private def int(value: Int) = IntLiteral(value)
  private def fun(gen: Variable => Expression) = Abstraction("x", gen(Variable("x")))
  // application is handled by Expression.apply()
  private val id = fun(x => x)

  private def inferTypeW(expr: Expression): Either[String, Type] = {
    Try(Environment.empty.inferTypeW(expr)) match {
      case Success((t, ts)) => Right(t)
      case Failure(ex) => Left(ex.getMessage)
    }
  }

  private def inferTypeJ(expr: Expression): Either[String, Type] = {
    Try(Environment.empty.inferTypeJ(expr, new TypeUnifier)) match {
      case Success(t) => Right(t)
      case Failure(ex) => Left(ex.getMessage)
    }
  }

  private def printExprAndType(infer: Expression => Either[String, Type])(expression: Expression) {
    infer(expression) match {
      case Right(t) => println(s"$expression: $t")
      case Left(message) => println(s"$expression: error: $message")
    }
  }

  def main(args: Array[String]): Unit = {
    val display: Expression => Unit = printExprAndType(inferTypeJ)
    display(int(3))
    display(f)
    display(fun(x => int(42)))
    display(id)
    display(id(t))
    display(id(int(42)))
    display(id(id)) // J succeeds inferring this and W does not
    display(id(id(f)))
  }
}