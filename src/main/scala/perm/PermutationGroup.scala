package com.faacets.perm

/** Trait defining a permutation group. */
trait PermutationGroup extends FiniteGroup {
  type Group <: PermutationGroup
  type Element <: PermutationElement
  def degree: Int /** Degree of the permutation group, i.e. size of the domain. */
  def domain = (0 until degree)
  /** Trait defining a generic permutation group element. The class implementing this
    * trait has to define a lexicograpic ordering on the permutations by implementing Ordered.
    */
  trait PermutationElement extends FiniteGroupElement with PermutationElementHelpers {
    self: Element =>
    def explicit: Permutation = Permutation.fromUniqueArray(images)
    def domainSize: Int = group.degree
    def domain = (0 until domainSize)
    def image(el: Domain): Domain /** Image of a domain element. */
    def images: Array[Domain] /** Images of the permutation acting on 0...domainSize-1 */

    /** Compares two permutations by lexicographic order. */
    def compare(that: Element) = {
      require(domainSize == that.domainSize)
      val firstNotEqual = (0 until domainSize).find(i => image(i) != that.image(i))
      firstNotEqual match {
        case None => 0
        case Some(i) if image(i) <= that.image(i) => -1
        case _ => 1
      }
    }
  }

  trait PermutationElementHelpers {
    self: PermutationElement =>
    def cycle(start: Domain) = explicit.cycle(start)
    def cycles(includeTrivialCycles: Boolean = false): Iterable[(Domain, Int)] =
      explicit.cycles(includeTrivialCycles)
    def inSupport(k: Domain) = image(k) != k
    def support = domain.filter(inSupport(_))
  }
}
