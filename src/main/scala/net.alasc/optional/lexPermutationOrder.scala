package net.alasc.optional

import spire.algebra.Order
import spire.syntax.groupAction._
import net.alasc.algebra.{FiniteGroup, FaithfulPermutationAction}
import net.alasc.syntax.permutationAction._

object lexPermutationOrder {
  /** Lexicographic order on permutations. */
  class LexPermutationOrder[P](implicit algebra: FiniteGroup[P], action: FaithfulPermutationAction[P]) extends Order[P] {
    override def eqv(x: P, y: P) = algebra.eqv(x, y)

    def compare(x: P, y: P): Int = {
      val n = x.supportMax.getOrElse(-1).max(y.supportMax.getOrElse(-1)) + 1
      var i = 0
      while (i < n) {
        val c = ((i <|+| x) - (i <|+| y)).signum
        if (c != 0)
          return c
        i += 1
      }
      0
    }
  }

  implicit def LexPermutationOrder[P: FiniteGroup: FaithfulPermutationAction]: Order[P] = new LexPermutationOrder[P]
}
