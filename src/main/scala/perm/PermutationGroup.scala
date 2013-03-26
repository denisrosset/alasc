package com.faacets.perm

/** Trait defining a permutation group, whose elements are of type P. The underlying
  * type has to extend the Permutation trait.
  */
trait PermutationGroup[P <: Permutation[P]] extends Iterable[P] {
  val degree: Int /** Degree of the permutation group, i.e. size of the domain. */
  def verify: Boolean /** Checks the group construction for consistency. */
  def generatingSet: Iterable[P] /** Returns an iterator on a set of generators for the group. */
  def order: Int /** Order of the group, i.e. number of permutations in the group. */
  def contains(perm: P): Boolean /** Checks if the group contains permutation P. */
  def iterator: Iterator[P] /** An iterator through all group elements. */
  def randomElement: P /** Produces a random element. */
}
