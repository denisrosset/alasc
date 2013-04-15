package com.faacets.perm

/** Trait defining a finite group. */
trait FiniteGroup extends AbstractGroup {
  type Group <: FiniteGroup
  type Element <: FiniteGroupElement
  def order: BigInt /** Order of the group, i.e. number of elements in the group. */
  def elements: Iterable[Element] /** An iterator through all group elements. */
  def generators: Iterable[Element] /** Returns an iterable on a set of generators for the group. */
  def contains(e: Element): Boolean /** Checks if the group contains element e. */
  trait FiniteGroupElement extends AbstractElement {
    self: Element =>
    /** Period of this group element, i.e. the smallest positive integer m such that this**m = identity. */
    def period: Int = {
      def innerLoop(e: Element, i: Int): Int = {
        if (e.isIdentity)
          i
        else
          innerLoop(this*e, i + 1)
      }
      innerLoop(this, 1)
    }
  }
}
