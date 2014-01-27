package net.alasc

trait GenPermuting extends Any with GenFinite with Ordered[GenPermuting] {
  /** Size of the current permutation. */
  def size: Int
  /** Image of a domain element. */
  def image(k: Dom): Dom
  /** Tests if this permutation is the identity. */
  def isIdentity: Boolean
  /** Returns the inverse of this permutation. */
  def inverse: GenPermuting
  /** Hashes this perm using a MurmurHash3 ordered hash with seed Perm.hashSeed. */
  def hash: Int
  /** Returns the image of this permutation as a sequence of Dom elements. */
  def imagesSeq: IndexedSeq[Dom]
  /** Return the cycle of this permutation starting at a given domain element. */
  def cycle[P](start: Dom): Seq[Dom]
  /** Returns a representation of this permutation as a product of cycles. */
  def cycles: Seq[Seq[Dom]]
  /** Returns an explicit representation of this permutation. */
  def toPerm: Perm
  /** Returns a Text representation of the cycles of this permutation using symbols. */
  def cyclesToTextUsingSymbols(symbols: Seq[String]): String
  /** Returns a Text representation of the cycles of this permutation.
    * 
    * @note Domain elements are represented one-based.
    */
  def cyclesToText: String
  /** Return an Iterable over the elements of the domain of this permutation. */
  def domain: Iterable[Dom]
  /** Returns the support of this permutation. */
  def support: Iterable[Dom]
}

trait Permuting[P <: Permuting[P]] extends Any with Finite[P] with GenPermuting {
  self: P =>
  /** Tests for equality with another Permuting. */
  def ===(that: Perm): Boolean
  /** Returns the inverse of this permutation. */
  def inverse: P
  /** Returns the product of this permutation with another permutation. */
  def *(that: P): P
}

trait GenPermutingLike extends Any with GenPermuting {
  def compare(that: GenPermuting): Int = {
    import Dom.ZeroBased._
    for (i <- 0 until size)
      java.lang.Math.signum(image(i).compare(that.image(i))) match {
        case -1 =>
          return -1
        case 1 =>
          return 1
      }
    return 0
  }

  def imagesSeq = Dom.domain(size).map(k => image(k)).toIndexedSeq
  def cycle[P](start: Dom) = {
    def walk(el: Dom): List[Dom] =
      if (el == start)
        Nil
      else
        el :: walk(image(el))
    start :: walk(image(start))
  }
  def cycles = {
    import Dom.ZeroBased._
    var checked = scala.collection.mutable.BitSet(size)
    var i = Dom.last(size)
    var cycleList = List.empty[List[Dom]]
    while (i >= Dom.first) {
      if(!checked(i._0)) {
        def visit(e: Dom, stopWhen: Dom): List[Dom] = {
          checked(e._0) = true
          if (image(e) == stopWhen)
            e :: Nil
          else
            e :: visit(image(e), stopWhen)
        }
        val cycle = visit(i, i)
        if (cycle.length > 1) {
          val mn = cycle.minBy(_._0)
          val (bef, aft) = cycle.span(_ != mn)
          cycleList = (aft ++ bef) :: cycleList
        }
      }
      i = i - 1
    }
    cycleList.sortBy(_.head._0)
  }
  def cyclesToTextUsingSymbols(symbols: Seq[String]) = {
    import Dom.ZeroBased._
    cycles.map(_.map( d => symbols(d) ).mkString("(",",",")")).mkString("")
  }
  def cyclesToText =
    cycles.map(_.map(_._1).mkString("(",",",")")).mkString("")
  def domain = Dom.domain(size)
  def support = domain.filter( k => image(k) !== k )
}

trait PermutingLike[P <: Permuting[P]] extends Any with Permuting[P] {
  self: P =>
}
