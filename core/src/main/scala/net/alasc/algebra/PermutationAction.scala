package net.alasc.algebra

import scala.annotation.tailrec

import spire.algebra._
import spire.math.SafeLong
import spire.std.int._

import metal.syntax._

import net.alasc.perms.Perm
import net.alasc.util._

/** Permutation action: a group action on non-negative integers.
  *
  * The domain on which the permutation acts starts at 0.
  *
  * The standard action, whose speed should be optimized, is the right action.
  *
  */
trait PermutationAction[G] extends Action[Int, G] { self =>

  /** Tests if `g` moves any point. */
  def movesAnyPoint(g: G): Boolean

  /** Tests if the point `i` is in the support of `g`. */
  def movesPoint(g: G, i: Int): Boolean = actr(i, g) != i

  /** Number of non-negative integers moved by the permutation. */
  def nMovedPoints(g: G): Int = {
    val n = movedPointsUpperBound(g).getOrElseFast(-1) + 1
    var nMoved: Int = 0
    var k: Int = 0
    while (k < n) {
      if (movesPoint(g, k))
        nMoved += 1
      k += 1
    }
    nMoved
  }

  /** Returns a bit set of all non-negative integers k that are changed by the action of the permutation,
    * i.e. `S = { k | k <|+| g != k }`.
    */
  def movedPoints(g: G): Set[Int] = {
    val n = movedPointsUpperBound(g).getOrElseFast(-1) + 1
    val bitset = metal.mutable.FixedBitSet.reservedSize(largestMovedPoint(g).getOrElse(-1) + 1)
    var k: Int = 0
    while (k < n) {
      if (movesPoint(g, k))
        bitset += k
      k += 1
    }
    bitset.toScala
  }

  /** Returns the maximal element in the support of ` g`, or NNNone if the support is empty. */
  def largestMovedPoint(g: G): NNOption = {
    var k: Int = movedPointsUpperBound(g).getOrElseFast(-1)
    while (k > -1) {
      if (movesPoint(g, k)) return NNSome(k)
      k -= 1
    }
    NNNone
  }

  /** Returns the minimal element in the support of `g`, or NNNone if the support is empty. */
  def smallestMovedPoint(g: G): NNOption = {
    var k: Int = 0
    val n: Int = movedPointsUpperBound(g).getOrElseFast(-1) + 1
    while (k < n) {
      if (movesPoint(g, k)) return NNSome(k)
      k += 1
    }
    NNNone
  }

  /** Returns an arbitrary element in the support of `g` or NNNone if support empty. */
  def findMovedPoint(g: G): NNOption = largestMovedPoint(g)

  /** Returns a fast-to-compute upper bound on the maximal element in the support of `g`. */
  def movedPointsUpperBound(g: G): NNOption

  /** Returns the sign of the permutation `g`. */
  def signPerm(g: G): Int = {
    val rest = metal.mutable.FixedBitSet.fromIterable(movedPoints(g))
    var sign = 1
    while (rest.nonEmpty) {
      val h = rest.min
      var iter = h
      var n = 0
      while (n == 0 || iter != h) {
        rest -= iter
        n += 1
        iter = actr(iter, g)
      }
      if (n % 2 == 0)
        sign = -sign
    }
    sign
  }

  /** Returns the cycle structure of the permutation `g`. */
  def cycleStructure(g: G): Map[Int, Int] = {
    val rest = metal.mutable.FixedBitSet.fromIterable(movedPoints(g))
    val cs = metal.mutable.HashMap.empty[Int, Int]
    while (rest.nonEmpty) {
      val h = rest.min
      var iter = h
      var n = 0
      while (n == 0 || iter != h) {
        rest -= iter
        n += 1
        iter = actr(iter, g)
      }
      cs(n) = cs.getOrElse(n, 0) + 1
    }
    cs.toScala
  }

  /** Returns the order of the permutation representation of this element. */
  def permutationOrder(g: G): SafeLong = {
    val cs = cycleStructure(g)
    cs.keys.foldLeft(SafeLong(1)) { case (acc, len) => spire.math.lcm(acc, SafeLong(len)) }
  }

  /** Returns the orbit of the domain element `i` under the action of `g`. */
  def orbit(g: G, i: Int): Set[Int] = {
    val mut = metal.mutable.ResizableBitSet(i)
    @tailrec def rec(k: Int): Unit =
      if (k != i) {
        mut += k
        rec(actr(k, g))
      }
    rec(actr(i, g))
    mut.toScala
  }

  /** Returns the images of `g` on the domain (0 until n).
    *
    * Requires that `n > supportMax(g)`.*/
  def images(g: G, n: Int): IndexedSeq[Int] = new IndexedSeq[Int] {
    require(largestMovedPoint(g).getOrElseFast(-1) < n)
    def length = n
    def apply(idx: Int) = actr(idx, g)
  }

  /** Returns a permutation with the same images as the action of `g`. */
  def toPerm(g: G): Perm =
    Perm.fromSupportAndImageFun(movedPoints(g), k => actr(k, g))

  /** Return the smallest element of the domain moved by the given generators, or [[NNNone]]. */
  def smallestMovedPoint(generators: Iterable[G]): NNOption = {
    var mn = Int.MaxValue
    var moved = false
    val it = generators.iterator
    while (it.hasNext) {
      val g = it.next()
      smallestMovedPoint(g) match {
        case NNOption(i) =>
          mn = spire.math.min(mn, i)
          moved = true
        case _ =>
      }
    }
    if (moved) NNOption(mn) else NNNone
  }

  /** Return the largest element of the domain moved by the given generators, or [[NNNone]]. */
  def largestMovedPoint(generators: Iterable[G]): NNOption = {
    var mx = -1
    val it = generators.iterator
    while (it.hasNext) {
      val g = it.next()
      mx = spire.math.max(largestMovedPoint(g).getOrElseFast(-1), mx)
    }
    if (mx >= 0) NNOption(mx) else NNNone
  }

}

object PermutationAction {

  def apply[G](implicit G: PermutationAction[G]): PermutationAction[G] = G

}
