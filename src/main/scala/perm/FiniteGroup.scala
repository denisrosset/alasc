package com.faacets.perm

/** Trait defining a group, whose elements are of type E. */

trait FiniteGroup[E <: GroupElement[E]] extends Iterable[E] {
  def assertValid /** Checks the group construction for consistency. */
  def identity: E /** Returns the identity element. */
  def generators: Iterable[E] /** Returns an iterator on a set of generators for the group. */
  def order: BigInt /** Order of the group, i.e. number of elements in the group. */
  def contains(el: E): Boolean /** Checks if the group contains element el. */
  def iterator: Iterator[E] /** An iterator through all group elements. */
  def randomElement: E /** Produces a random element. */
}
