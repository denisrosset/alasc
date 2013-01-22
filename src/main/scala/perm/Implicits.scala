package com.faacets.perm

package object Implicits {
  type Domain = Int
  type Base = Seq[Domain]

  implicit def empowerMyDomain(alpha: Domain) = new EmpoweredDomain(alpha)

  implicit def permutationOrdering[P <: Permutation[P]]: Ordering[P] = {
    import scala.math.Ordering.Implicits._
    Ordering.fromLessThan(_.images < _.images)
  }

  def hasInSupport[P](p: Permutation[P], el: Domain): Boolean = (p.image(el) != el)
  def support[P](p: Permutation[P]): Iterable[Domain] = (0 until p.domainSize).filter(hasInSupport(p, _))
  def cycle[P](p: Permutation[P], start: Domain): Iterable[Domain] = {
    def walk(el: Domain): List[Domain] = if (el == start) List.empty[Domain] else el :: walk(p.image(el))
      start :: walk(p.image(start))
  }
  def cycles[P](p: Permutation[P], includeTrivialCycles: Boolean = false): Iterable[(Domain, Int)] = {
    var checked = scala.collection.mutable.BitSet(p.domainSize)
    var i = p.domainSize - 1
    var cycleList = List.empty[(Domain, Int)]
    while (i >= 0) {
      if(!checked(i)) {
        var minEl = i
        var j = i
        var cycleLength = 0
        do {
          checked(j) = true
          if (minEl > j)  minEl = j
          j = p.image(j)
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
