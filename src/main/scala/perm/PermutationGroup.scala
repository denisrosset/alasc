package com.faacets.perm

/** Trait defining a permutation group, whose elements are of type P. The underlying
  * type has to extend the Permutation trait.
  */
trait PermutationGroup[P <: Permutation[P]] extends FiniteGroup[P] {
  val degree: Int /** Degree of the permutation group, i.e. size of the domain. */
}
