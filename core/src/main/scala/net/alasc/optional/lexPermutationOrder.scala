package net.alasc.optional

import spire.algebra.{Eq, Group, Order}
import spire.syntax.action._
import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.syntax.permutationAction._

object lexPermutationOrder {
  
  /** Lexicographic order on permutations. */
  class LexPermutationOrder[P: FaithfulPermutationAction: Group] extends Order[P] {
    override def eqv(x: P, y: P) = Eq[P].eqv(x, y)

    def compare(x: P, y: P): Int = {
      val n = x.largestMovedPoint.getOrElseFast(-1).max(y.largestMovedPoint.getOrElseFast(-1)) + 1
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

  implicit def LexPermutationOrder[P: Group: FaithfulPermutationAction]: Order[P] = new LexPermutationOrder[P]

}
