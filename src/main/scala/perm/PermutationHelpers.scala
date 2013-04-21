package com.faacets.perm

trait PermutationHelpers  {
  self: Permutation =>
  
  def withSwap(i: Domain, j: Domain): Permutation = {
    require(isDefinedAt(i) && isDefinedAt(j))
    val a = arr.clone
    val k = a(i)
    a(i) = a(j)
    a(j) = k
    Permutation(a)
  }
  def cycle[P](start: Domain): List[Domain] = {
    def walk(el: Domain): List[Domain] =
      if (el == start)
        Nil
      else
        el :: walk(image(el))
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
  def **(n: Int): Permutation = n match { // TODO: implement algorithm POWER of HCGT
    case 1 => this
    case _ => this**(n-1)
  }
  def lehmerCode: Array[Int] = {
    val code = new Array[Int](size)
    val checked = Array.fill[Int](size)(1)
    for ((i, p) <- iterator) {
      checked(p) = 0
      code(i) = checked.take(p).sum
    }
    code
  }
  def inSupport(k: Domain) = image(k) != k /** Checks if this permutation moves element el of the domain. */
  def support = domain.filter(inSupport(_)) /** Return all points moved by this permutation. */
}

trait PermutationHelpersObject {
  def fromLehmerCode(code: Seq[Int]) = {
    val openSpots = scala.collection.mutable.ArrayBuffer.tabulate[Int](code.size)(x => x)
    val imgs = new Array[Domain](code.size)
    for (i <- 0 until code.size)
      imgs(i) = openSpots.remove(code(i))
    Permutation.fromUniqueArray(imgs)
  }
}
