package net.alasc

import spire.algebra.Group
import spire.syntax.group._

import net.alasc.algebra.PermutationAction

package object finite {

  /** Commutator gInv hInv g h, see definition Sec 2.3.3 p.27, D. Holt, Handbook of Comp. Group Theory, 2005 */
  def commutator[G:Group](g: G, h: G): G = g.inverse |+| h.inverse |+| g |+| h

  type FaithfulPermutationActionBuilder[A] = FaithfulActionBuilder[A, Int, PermutationAction[A]]

  object FaithfulPermutationActionBuilder {
    def apply[A](implicit ev: FaithfulPermutationActionBuilder[A]): FaithfulPermutationActionBuilder[A] = ev
  }

}
