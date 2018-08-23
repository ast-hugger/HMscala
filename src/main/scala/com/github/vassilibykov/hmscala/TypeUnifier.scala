package com.github.vassilibykov.hmscala

import scala.collection.mutable

class TypeUnifier {
  private val nodes = mutable.Map[Type, Node]()

  def find(t: Type): Type = ensureNode(t).topmost.value

  def unify(a: Type, b: Type): Unit = {
    (find(a), find(b)) match {
      case (_, _) if a == b =>
      case (af: TFunction, bf: TFunction) =>
        unify(af.from, bf.from)
        unify(af.to, bf.to)
      case (av: TVariable, bt) => unifyVar(av, bt)
      case (at, bv: TVariable) => unifyVar(bv, at)
      case _ => throw new TypeError(s"types $a and $b do not match")
    }
  }

  private def unifyVar(v: TVariable, t: Type): Unit = {
    nodes(v).parent = nodes get t
  }

  private def ensureNode(t: Type): Node = nodes.getOrElse(t, {
    val node = new Node(t)
    nodes(t) = node
    node
  })

  class Node(val value: Type) {
    var parent: Option[Node] = None
    def topmost: Node = parent.map(_.topmost) getOrElse this
    override def toString: String = s"Node($value, $parent)"
  }
}
