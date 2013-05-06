package com.faacets
package perm
package bsgs

import scala.collection.immutable.TreeMap

case class ExplicitTransversal[E <: PermElement[E]](beta: Dom, map: TreeMap[Dom, (E, E)]) extends Transversal[ExplicitTransversal[E], E] {
  // implementation of PartialFunction
  def apply(b: Dom) = map.apply(b)

  def mapValues[F <: PermElement[F]](f: E => F): ExplicitTransversal[F] = new ExplicitTransversal(beta, TreeMap.empty[Dom, (F, F)] ++ map.mapValues( Function.tupled( (u, uinv) => (f(u), f(uinv)) )))
  // implementation of Iterable
  def isDefinedAt(b: Dom) = map.isDefinedAt(b)
  def iterator = map.iterator
  override def size = map.size // for speed reasons

  // implementation of AbstractTransversal
  def addingGenerator(s: E) = {
    val sinv = s.inverse
    var newMap = map
    def visit(b: Dom, upair: (E, E)) {
      // we received an element b in the orbit, and a pair of permutations
      val (u, uinv) = upair
      assert(u.image(beta) == b)
      assert(uinv.image(b) == beta)
      // such that beta^u = b and uinv = u^-1
      // looking at the image c of b by our new generator s
      // c = b^s
      val c = s.image(b)
      // we ask if there is a permutation v in the transversal such that
      // beta^v = c, as we have (beta^u)^s = c, we write v = u*s
      if (!newMap.isDefinedAt(c)) {
        val v = u*s
        val vinv = sinv * uinv
        assert(v.image(beta) == c)
        assert(vinv.image(c) == beta)
        newMap = newMap + ((c, (v, vinv)))
        visit(c, (v, vinv))
      }
    }
    for ((a, upair) <- map)
      visit(a, upair)
    val n = ExplicitTransversal(beta, newMap)
    n.assertValid
    n
  }
}

case class ExplicitTransversalFactory[E <: PermElement[E]]() extends TransversalFactory[ExplicitTransversal[E], E] {
  def empty(beta: Dom, id: E) = ExplicitTransversal(beta, TreeMap((beta, (id, id))))

}
