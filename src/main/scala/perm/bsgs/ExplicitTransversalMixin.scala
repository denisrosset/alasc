package com.faacets.perm
package bsgs

trait ExplicitTransversalMixin extends BSGSTypes {
  type Transversal = ExplicitTransversal

  import scala.collection.immutable.TreeMap

  def makeEmptyTransversal(beta: Domain) = ExplicitTransversal(beta, TreeMap((beta, (underlyingGroup.identity, underlyingGroup.identity))))

  case class ExplicitTransversal(beta: Domain, map: TreeMap[Domain, (UnderlyingElement, UnderlyingElement)]) extends AbstractTransversal {
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
      def visit(b: Domain, upair: (UnderlyingElement, UnderlyingElement)) {
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
}
