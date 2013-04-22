package com.faacets.perm
package bsgs

trait ExplicitTransversalMixin extends AbstractTransversalMixin {
  type TransversalMixin = ExplicitTransversalMixin
  type Transversal = ExplicitTransversal

  import scala.collection.immutable.TreeMap

  def makeEmptyTransversal(beta: Domain) = ExplicitTransversal(beta, TreeMap((beta, underlyingGroup.identity)))

  case class ExplicitTransversal(beta: Domain, map: TreeMap[Domain, UnderlyingElement]) extends AbstractTransversal {
    // implementation of PartialFunction
    def apply(b: Domain) = map.apply(b)

    // implementation of Iterable
    def isDefinedAt(b: Domain) = map.isDefinedAt(b)
    def iterator = map.iterator
    override def size = map.size // for speed reasons

    // implementation of AbstractTransversal
    def addingGenerator(s: UnderlyingElement) = {
      val sinv = s.inverse
      var newMap = map
      def visit(b: Domain, ub: UnderlyingElement) {
        val c = s.image(b)
        if (!newMap.isDefinedAt(c)) {
          val uc = sinv * ub
          newMap = newMap + ((c, uc))
          visit(c, uc)
        }
      }
      for ((a, ua) <- map)
        visit(a, ua)
      ExplicitTransversal(beta, newMap)
    }
  }
}
