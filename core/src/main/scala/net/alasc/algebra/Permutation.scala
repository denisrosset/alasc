package net.alasc.algebra

import spire.algebra.{Eq, Group}
import spire.math.SafeLong

/** Type class for Permutation-like objects.
  *
  * Combines [[Eq]], [[Group]] and [[PermutationAction]] in a single typeclass.
  */
trait Permutation[P] extends Eq[P] with Group[P] with PermutationAction[P] {

  /** Returns the order of this permutation. */
  def order(p: P): SafeLong = permutationOrder(p)

}
