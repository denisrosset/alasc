package com.faacets.perm

import Implicits._

class ExplicitTransversal(explicitMap: scala.collection.immutable.TreeMap[Int, Permutation]) extends Transversal {
  override def toString: String = (for ((key, value) <- explicitMap) yield key + " => " + value).mkString("","\n","")
  override def size = explicitMap.size
  override def apply(el: Int): Permutation = explicitMap(el)
  override def contains(el: Int): Boolean = explicitMap.contains(el)
  override def iterable = explicitMap.keys
}

object ExplicitTransversal {
  def fromGenerators(el: Domain, G: Iterable[Permutation]): Transversal = { // should be extended to other transversal types
    val m = scala.collection.mutable.HashMap.empty[Int, Permutation]
    def visit(a: Domain, p: Permutation) {
      if (!m.isDefinedAt(a)) {
        m(a) = p.inverse
        for ((g, i) <- G.view.zipWithIndex)
          visit(a**g, g.inverse*p)
      }
    }
    visit(el, new IdentityPermutation(degree))
    new ExplicitTransversal(scala.collection.immutable.TreeMap.empty[Int, Permutation] ++ m)
  }
}
