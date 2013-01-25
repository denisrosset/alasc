package com.faacets.perm

trait Permutation[P] extends GroupElement[P] with Ordered[P] {
  def domainSize: Int
  def image(el: Domain): Domain
  def images: Vector[Domain]
  def compare(that: P): Int
  def toExplicit: ExplicitPermutation = new ExplicitPermutation(images)

  def hasInSupport(el: Domain): Boolean = (image(el) != el)
  def support: Iterable[Domain] = (0 until domainSize).filter(hasInSupport(_))
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
