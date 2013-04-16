package com.faacets.perm

package bsgs {
  trait AbstractTransversalMixin {
    type TransversalMixin <: AbstractTransversalMixin
    type Transversal <: AbstractTransversal

    type UnderlyingGroup <: PermutationGroup
    val g: UnderlyingGroup
    type UnderlyingElement = g.Element

    def makeEmpty(beta: Domain): Transversal

    trait AbstractTransversal extends PartialFunction[Domain, UnderlyingElement] with Iterable[(Domain, UnderlyingElement)] {
      def beta: Domain /** Element for which the transversal is defined. */
      def keysIterator: Iterator[Domain] = iterator.map(_._1)
      def valuesIterator: Iterator[UnderlyingElement] = iterator.map(_._2)

      /** Returns a random element of the transversal. */
      def random(implicit gen: scala.util.Random = scala.util.Random): (Domain, UnderlyingElement) = {
        val num = gen.nextInt(size)
        iterator.drop(num).next()
      }
      def addingGenerator(s: UnderlyingElement): Transversal  /** Returns a new transversal extended with s added to its generators. */
        /** Checks the sanity of the transversal. */
      def assertValid {
        for ((b, u) <- iterator)
          assert (u.image(beta) == b)
      }
    }
  }
}
