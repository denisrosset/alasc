package com.faacets.perm

package bsgs {
  import scala.collection.immutable.TreeMap
  import scala.collection.mutable.HashMap

  /** Transversal implementation storing the elements explicitly. */
  case class ExplicitTransversal[P <: Permutation[P]](beta: Domain, map: TreeMap[Int, P]) extends Transversal[P, ExplicitTransversal[P]] {
    def size = map.size
    def apply(el: Int): P = map(el)
    def contains(el: Int): Boolean = map.contains(el)
    def orbitIterator = map.keysIterator
    def elementsIterator = map.valuesIterator
    def +(s: P) = {
      val sinv = s.inverse
      var newMap = map
      def visit(b: Domain, ub: P) {
        val c = s.image(b)
        if (!newMap.contains(c)) {
          val uc = sinv * ub
          newMap = newMap + ((c, uc))
          visit(c, uc)
        }
      }
      for ((a, ua) <- map)
        visit(a, ua)
      new ExplicitTransversal(beta, newMap)
    }
  }

  object ExplicitTransversal {
    def apply[P <: Permutation[P]](beta: Domain, id: P) =
      new ExplicitTransversal(beta, TreeMap((beta, id)))
  }
}
