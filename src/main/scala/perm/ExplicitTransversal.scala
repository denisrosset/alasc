package com.faacets.perm

import Implicits._

class ExplicitTransversal[T <: Permutation[T]](explicitMap: scala.collection.immutable.TreeMap[Int, T]) extends Transversal[T] {
  override def toString: String = (for ((key, value) <- explicitMap) yield key + " => " + value).mkString("","\n","")
  override def size = explicitMap.size
  override def apply(el: Int): T = explicitMap(el)
  override def contains(el: Int): Boolean = explicitMap.contains(el)
  override def iterable = explicitMap.keys
}

object ExplicitTransversal {
  def fromGenerators[P <: Permutation[P]](el: Domain, G: Iterable[P]): ExplicitTransversal[P] = { // should be extended to other transversal types
    val m = scala.collection.mutable.HashMap.empty[Int, P]
val degree = G.head.domainSize
    def visit(a: Domain, p: P) {
      if (!m.isDefinedAt(a)) {
        m(a) = p.inverse
        for ((g, i) <- G.view.zipWithIndex)
          visit(a**g, g.inverse*p)
      }
    }
    visit(el, G.head.identity)
    new ExplicitTransversal[P](scala.collection.immutable.TreeMap.empty[Int, P] ++ m)
  }
  def trivial[P <: Permutation[P]](alpha: Domain, identity: P): ExplicitTransversal[P] = {
    new ExplicitTransversal[P](scala.collection.immutable.TreeMap(List((alpha, identity)):_*))
  }
}
