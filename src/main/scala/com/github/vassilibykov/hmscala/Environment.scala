package com.github.vassilibykov.hmscala

class Environment(private val bindings: Map[String, TypeScheme]) extends Substitutable[Environment] {

  def freeVariables: Set[String] = bindings.values.foldLeft(Set.empty: Set[String]) ((s, ts) => s ++ ts.freeVariables)

  def without(name: String): Environment = new Environment(bindings - name)

  def withName(name: String, scheme: TypeScheme): Environment = new Environment(bindings + (name -> scheme))

  override def applySubstitution(subst: Substitution) = new Environment(bindings mapValues (it => subst(it)))

  def + (other: Environment): Environment = new Environment(bindings ++ other.bindings)

  def generalize(t: Type): TypeScheme = new TypeScheme(t.freeVariables -- freeVariables, t)

  def inferTypeW(expr: Expression): (Type, Substitution) = expr match {
    case _: IntLiteral => (TInt, Substitution())
    case _: BoolLiteral => (TBool, Substitution())
    case Variable(name) => bindings.get(name)
      .map(it => (it.instantiate(), Substitution()))
      .getOrElse(throw new TypeError(s"Unbound variable $name"))
    case Application(fun, arg) => {
      val a = generateTypeVar()
      val (funType, funSubst) = inferTypeW(fun)
      val (argType, argSubst) = funSubst(this).inferTypeW(arg)
      val mgu = argSubst(funType).mostGeneralUnifier(TFunction(argType, a)) match {
        case Right(v) => v
        case Left(message) => throw new TypeError(message)
      }
      (mgu(a), mgu + argSubst + funSubst)
    }
    case Abstraction(varName, body) => {
      val a = generateTypeVar()
      val env = (this without varName) + Environment(varName, a)
      val (bodyType, bodySubst) = env.inferTypeW(body)
      (TFunction(bodySubst(a), bodyType), bodySubst)
    }
  }

  def inferTypeJ(expr: Expression, unifier: TypeUnifier): Type = expr match {
    case _: IntLiteral => TInt
    case _: BoolLiteral => TBool
    case v: Variable =>
      (bindings get v.name).map(_.instantiate()) getOrElse (throw new TypeError(s"Unbound variable ${v.name}"))
    case app: Application => {
      val funType = inferTypeJ(app.fun, unifier)
      val argType = inferTypeJ(app.arg, unifier)
      val a = generateTypeVar()
      unifier.unify(funType, TFunction(argType, a))
      unifier.find(a)
    }
    case abs: Abstraction => {
      val a = generateTypeVar()
      val rType = this.withName(abs.varName, TypeScheme(a)).inferTypeJ(abs.body, unifier)
      TFunction(unifier.find(a), unifier.find(rType))
    }
  }

  private var serial = -1
  private val Letters = "abcdefghijklmnopqrstuvwxyz"

  def generateTypeVar(): TVariable = {
    serial += 1
    TVariable(Letters(serial).toString)
  }
}

object Environment {
  def empty = new Environment(Map.empty)
  def apply(varName: String, ts: TypeScheme): Environment = new Environment(Map(varName -> ts))
  def apply(varName: String, t: Type): Environment = apply(varName, TypeScheme(t))
}


class TypeError(message: String = "?") extends Exception(message)