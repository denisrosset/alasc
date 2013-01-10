package com.faacets.perm

import Implicits._

trait Permutation extends Ordered[Permutation] {
  def compare(that: Permutation): Int = {
    val thisImages = images
    val thatImages = that.images
    import scala.math.Ordering.Implicits._
    Ordering[Vector[Int]].compare(thisImages, thatImages)
  }

  def image(el: Domain): Domain

  def cycle(start: Domain): Iterable[Domain] = {
    def walk(el: Domain): List[Domain] = if (el == start) List.empty[Domain] else el :: walk(image(el))
      start :: walk(image(start))
  }
  override def equals(other: Any): Boolean = other match {
    case that: Permutation => !(0 until domainSize).exists(i => this.image(i) != that.image(i))
    case _ => false
  }
  // Standard scala methods
  override def toString = {
    if(Permutation.printCycles) {
      def cycleStr(i: (Domain, Int)): String = cycle(i._1).mkString("(", ", ", ")")
      val cyclesStr = cycles(false).map(cycleStr(_)).mkString("","","")
      "Permutation(" + domainSize + ")" + cyclesStr
    } else
      images.mkString("Permutation(",", ",")")
  }

  override def hashCode = images.hashCode
  def apply(cycle: Domain*): Permutation = {
    val P: Array[Int] = Array.tabulate[Domain](domainSize)((i:Domain) => i)
    val list = cycle.toList
    var el = list
    while (!el.tail.isEmpty) {
      P(el.head) = el.tail.head
      el = el.tail
    }
    P(el.head) = list.head
    this*new ExplicitPermutation(Vector(P:_*))
  }
  def images: Vector[Domain] = Vector(((0 until domainSize).map((el: Domain) => image(el))):_*)
  def domainSize: Int
  def inverse: Permutation = {
    val a = Array.fill[Domain](domainSize)(0)
    for (i <- 0 until domainSize) a(image(i)) = i
    new ExplicitPermutation(Vector(a:_*))
  }
  def *(other: Permutation): Permutation = {
    val a = images
    val b = other.images ++ (other.domainSize until domainSize)
    new ExplicitPermutation(Vector(a.map(b(_)):_*) ++ b.slice(a.length, b.length))
  }
  def support: Iterable[Domain] = (0 until domainSize).filter(hasInSupport(_))
  def hasInSupport(el: Domain): Boolean = (image(el) != el)
  def isIdentity: Boolean = !(0 until domainSize).exists( hasInSupport(_) )
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

  def verify: Boolean = {
    val notInside = scala.collection.mutable.BitSet((0 to domainSize): _*)
    (0 until domainSize).map(image(_)).foreach(i => {
      if (!notInside(i)) return false // already inside, so duplicate element
      notInside -= i
    })
    return notInside.isEmpty
  }
  def resizedTo(n: Int): Option[Permutation] = {
    if (n == domainSize) return Some(this)
    if (n > domainSize) return Some(new ExplicitPermutation(images ++ (domainSize until n)))
    if (n < domainSize && (n until domainSize).exists(hasInSupport(_)))
      return None
    return Some(new ExplicitPermutation(images.take(n)))
  }
}

object Permutation {
  def apply(args: Domain*): Permutation = {
    if(args.length == 1 && args(0) > 0)
      return new IdentityPermutation(args(0))
    new ExplicitPermutation(Vector(args:_*))
  }
  var printCycles = true
}
