package com.faacets.perm

import Implicits._

trait RichPermutation[P <: RichPermutation[P]] extends Permutation[P] {
  def images: Vector[Domain] = Vector(((0 until domainSize).map((el: Domain) => image(el))):_*)

  override def compare(that: P): Int = {
    import scala.math.Ordering.Implicits._
    Ordering[Vector[Int]].compare(images, that.images)
  }
  def verify: Boolean = {
    val notInside = scala.collection.mutable.BitSet((0 to domainSize): _*)
    (0 until domainSize).map(image(_)).foreach(i => {
      if (!notInside(i)) return false // already inside, so duplicate element
      notInside -= i
    })
    return notInside.isEmpty
  }

  def isIdentity: Boolean = !(0 until domainSize).exists( hasInSupport(_) )
  def identity: P = this*inverse

  def hasInSupport(el: Domain): Boolean = (image(el) != el)
  def support: Iterable[Domain] = (0 until domainSize).filter(hasInSupport(_))
  def cycle(start: Domain): Iterable[Domain] = {
    def walk(el: Domain): List[Domain] = if (el == start) List.empty[Domain] else el :: walk(image(el))
      start :: walk(image(start))
  }
  def cycles(includeTrivialCycles: Boolean = false): Iterable[(Domain, Int)] = {
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

  def toExplicit: ExplicitPermutation = new ExplicitPermutation(images)
}
