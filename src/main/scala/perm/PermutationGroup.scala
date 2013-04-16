package com.faacets.perm

/** Trait defining a permutation group. */
trait PermutationGroup extends FiniteGroup {
  type Group <: PermutationGroup
  type Element <: Permutation
  def degree: Int /** Degree of the permutation group, i.e. size of the domain. */

  /** Trait defining a generic permutation group element. The class implementing this
    * trait has to define a lexicograpic ordering on the permutations by implementing Ordered.
    */
  trait Permutation extends FiniteGroupElement with Ordered[Element] {
    self: Element =>
    def toExplicit: SymmetricGroup#Element =
      SymmetricGroup(degree).make(images)
    def domainSize: Int = group.degree
    def image(el: Domain): Domain /** Image of a domain element. */
    def images: Vector[Domain] /** Images of the permutation acting on 0...domainSize-1 */
    def compare(that: Element): Int /** Compares two permutation by lexicographic order. */
    def hasInSupport(el: Domain): Boolean = (image(el) != el) /** Checks if this permutation moves element el of the domain. */
    def support: Iterable[Domain] = (0 until domainSize).filter(hasInSupport(_)) /** Return all points moved by this permutation. */
    def cycle[P](start: Domain): Iterable[Domain] = {
      def walk(el: Domain): List[Domain] = if (el == start) List.empty[Domain] else el :: walk(image(el))
      start :: walk(image(start))
    }
    def cycles[P](includeTrivialCycles: Boolean = false): Iterable[(Domain, Int)] = {
      var checked = scala.collection.mutable.BitSet(domainSize)
      var i = domainSize - 1
      var cycleList = List.empty[(Domain, Int)]
      while (i >= 0) {
        if(!checked(i)) {
          var minEl = i
          var j = i
          var cycleLength = 0
          do {
            checked(j) = true
            if (minEl > j)  minEl = j
            j = image(j)
            cycleLength += 1
          } while (j != i)

          if (cycleLength > 1 || includeTrivialCycles)
            cycleList = (minEl, cycleLength) :: cycleList
        }
        i -= 1
      }
      import scala.math.Ordering.Implicits._
      cycleList.sortWith(_<_)
    }
  }
}
