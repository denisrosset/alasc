package net.alasc
package math

import scala.annotation.tailrec

import net.alasc.syntax.permutationAction._

/** Lexicographically ordered sequence of permutations acting on `n` elements. */
final case class Perms(n: Int) extends coll.big.IndexedSeqImpl[Perm] with coll.Searchable[BigInt, Perm] with coll.Iterable[Perm] with coll.Traversable[Perm] with coll.Container[Perm] with coll.Mappable[Perms, Perm] {
  def contains(perm: Perm): Boolean = perm.supportMax.getOrElse(-1) < n
  @tailrec def fact(k: Int, acc: BigInt = 1): BigInt =
    if (k < 2) acc else fact(k - 1, acc * k)
  def length = fact(n)

  /** Returns the lexicographic rank of the given permutation.
    * 
    * Inspired by sympy.combinatorics.permutations.
    */
  def find(perm: Perm): Option[BigInt] =
    if (perm.supportMax.getOrElse(-1) >= n) None else {
      var rank = BigInt(0)
      val rho: Array[Int] = perm.images(n).toArray
      var k = n - 1
      var psize: BigInt = fact(k)
      var j = 0
      while (j < n - 1) {
        rank += rho(j) * psize
        var i = j + 1
        while (i < n) {
          if (rho(i) > rho(j))
            rho(i) -= 1
          i += 1
        }
        psize /= k
        k -= 1
        j += 1
      }
      Some(rank)
    }

  def foreach[U](f: Perm => U) = iterator.foreach(f)

  /** Iterates through all permutations, sorted lexicographically.
    * 
    * Inspired by sympy.combinatorics.permutations.
    */
  def iterator: Iterator[Perm] = new Iterator[Perm] {
    val n = Perms.this.n

    var nextImages = Array.tabulate(n)(identity)

    def hasNext = nextImages ne null

    def next = {
      if (nextImages eq null) Iterator.empty.next
      val nextRes = Perm.fromImages(nextImages)
      def swap(k: Int, l: Int): Unit = {
        val oldK = nextImages(k)
        nextImages(k) = nextImages(l)
        nextImages(l) = oldK
      }
      var i = n - 2
      while (i >= 0 && nextImages(i + 1) < nextImages(i))
        i -= 1
      if (i == -1) {
        nextImages = null
        nextRes
      } else {
        var j = n - 1
        while (nextImages(j) < nextImages(i))
          j -= 1
        swap(i, j)
        i += 1
        j = n - 1
        while (i < j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      nextRes
    }
  }

  /** Returns the k-th permutation ranked lexicographically.
    * 
    * Inspired by sympy.combinatorics.permutations.
    */
  def apply(rank: BigInt): Perm = {
    val images = Array.fill(n)(0)
    var psize = BigInt(1)
    var i = 0
    var remRank = rank
    while (i < n) {
      val newPsize = psize * (i + 1)
      val d = (remRank % newPsize) / psize
      remRank -= d * psize
      images(n - i - 1) = d.toInt
      var j = n - i
      while (j < n) {
        if (images(j) > d - 1)
          images(j) += 1
        j += 1
      }
      psize = newPsize
      i += 1
    }
    assert(remRank == 0)
    Perm.fromImages(images)
  }
}
